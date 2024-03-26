//! # Interpreter Module
//!
//! The Interpreter module is responsible for the execution, and runtime management of a
//! SAP program. Through the [`eval_program()`] function, the AST is traversed, and the
//! actual runtime logic of each node is applied. For example if a [`Binary`] node is
//! encountered, it is converted into it's actual value (i.e. `1+2` becomes `3`).
//!
//! ## Values
//!
//! [`Value`]s a repsented as enum. The same module also contains all the logic for
//! manipulating values, such as comparing, adding, subtracting and so on...
//!
//! ## Environment
//!
//! At runtime, values which aren't directly stored in the AST (i.e variables) are stored
//! in an [`Environment`]. An environment is made up of 2 things: A hashmap binding the
//! variable names to the data they store, and a reference to the outer/parent
//! environment.
//!
//! ## Execution
//!
//! As previosuly mentioned, a SAP program is executed by traversing it's AST, and
//! applying the runtime logic of each node. This can be evaluating numerical expression,
//! retrieving/storing variables, repetition or displaying to the console.
//!
//! Before understanding traversal interrupts, it is important to note that the call stack
//! is reflective of where in the tree the code has traversed, as each AST node has it's
//! own dedicated function.
//!
//! There are two things which can interrupt the traversal of the tree:
//! - Runtime errors
//! - Return statements
//!
//! When a runtime error is encountered, traversal stops completely and
//! the error is returned to the caller of the [`eval_program()`] function.
//!
//! When a return statement in encountered, the code backtracks up the tree until reaching
//! a [`FunctionCall`] node, or the root ([`Program`]) node. If the root node is reached,
//! this indicates the return statement was used outside of a function, which is treated
//! as an error.
//!
//! If a [`FunctionCall`] node is reached, this indicates the return statement was used
//! inside a function, which was invoked by this function call. The return value is
//! unwrapped into a normal value, and can be used as normal in the place of the function
//! call.
//!
//! For example, take the expression `set x = 1+add(1, 3)`, the function `add(1, 3)` might
//! produce a `Return(Value(4))` which is then unwrapped into a `Value(4)`. The resulting
//! statement now looks like `set x = 1+4`.
use std::cell::RefCell;
use std::ops::ControlFlow;
use std::ops::ControlFlow::{Break, Continue};
use std::rc::Rc;

use ast::expression::{Binary, Expression, FunctionCall, Identifier, Selection, Unary};
use ast::literal::Literal;
use ast::statement::{
    Display, FunctionDeclaration, RepeatForever, RepeatNTimes, RepeatUntil, Return, Set,
    Statement,
};
use ast::Program;
use lexer::token::TokenKind;
use shared::error::{Error, ErrorKind};
use shared::{err, stdoutln};

use crate::runtime::{EnvRef, Environment};
use crate::value::{Function, Value};

// Attempt to obtain the current version of the interpreter module.
pub const VERSION: Option<&str> = std::option_env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod test;

pub mod runtime;
pub mod value;

/// Represents an interruption in traversal of the AST.
///
/// The reasons traversal may be interrupted are:
/// 1. An error in encountered, in which case backtrack all the way up the call stack and
///    return the error to the caller of the [`eval_program()`] function.
/// 2. Or, a [`Return`] statement in encountered.
///
/// In the case of a [`Return`] statement, the code backtracks up the call stack to the
/// point where the function containing the [`Return`] statement was called, and hands
/// over the return value to the caller. If the return statement was used outside of a
/// function, and the code reaches the end of the call stack, it is treated as an error.
pub enum TraversalInterrupt {
    ReturnValue(Rc<Value>),
    Error(Error),
}

/// Type alias representing what the evaluation of a node produces.
///
/// It may either produce a [`Value`], indicating that the evaluation was a success and
/// traversal may continue as normal, and it may produce a [`TraversalInterrupt`], which
/// should be immediately returned from the current function, unless specified otherwise.
type EvaluationOutcome = ControlFlow<TraversalInterrupt, Rc<Value>>;

/// Shorthand macro for creating an [`Error`] wrapped in an [`EvaluationOutcome`].
///
/// # Arguments
///
/// Either:
/// * `kind` - An [`ErrorKind`].
/// * `err_msg` - The error message (Same syntax as the [`format!`] macro).
///
/// Or
/// * `err` - An already constructed [`Error`].
macro_rules! traversal_error {
    ($kind:expr, $($arg:tt)*) => {
        Break(TraversalInterrupt::Error(Error::new(&format!($($arg)*), $kind)))
    };
    ($err:ident) => {
        Break(TraversalInterrupt::Error($err))
    }
}

/// Shorthand function for creating a new empty [`EnvRef`]
pub fn create_env() -> EnvRef {
    return Rc::new(RefCell::new(Environment::new()));
}

/// Probably the most important function in the whole program, the main entry point of the
/// SAP interpreter. It performs the actual execution of a program, returning a final
/// result.
///
/// # Arguments
///
/// * `env` - An [`EnvRef`] representing the starting global environment. Data may be
///   inserted ahead of time.
/// * `ast` - An AST root ([`Program`]) node representing the program that is to be
///   executed.
///
/// # Returns
///
/// Either an [`Ok`] containing a tuple
pub fn eval_program(env: &EnvRef, ast: Program) -> Result<(EnvRef, Rc<Value>), Error> {
    match eval_statements(env, &ast.statements) {
        Break(TraversalInterrupt::ReturnValue(_)) => {
            err!(ErrorKind::TypeError, "'return' used outside of function")
        }
        Break(TraversalInterrupt::Error(error)) => Err(error),
        Continue(value) => Ok((env.clone(), value)),
    }
}

/// Evaluates a list of [`Statement`]s, returning the result of the last statement.
fn eval_statements(env: &EnvRef, statements: &Vec<Statement>) -> EvaluationOutcome {
    let mut value = Rc::new(Value::Null);
    for statement in statements {
        value = eval_statement(env, &statement)?;
    }
    return Continue(value);
}

/// Evaluates a single [`Statement`], returning the result of the evaluation.
fn eval_statement(env: &EnvRef, statement: &Statement) -> EvaluationOutcome {
    match statement {
        Statement::Expression(expression) => eval_expression(env, expression),
        Statement::Return(ret) => eval_return_statement(env, ret),
        Statement::Set(set_stmt) => eval_set_statement(env, set_stmt),
        Statement::FunctionDeclaration(func) => eval_func_decl_statement(env, func),
        Statement::RepeatNTimes(repeat) => eval_repeat_n_times_statement(env, repeat),
        Statement::RepeatUntil(repeat) => eval_repeat_until_statement(env, repeat),
        Statement::RepeatForever(repeat) => eval_repeat_forever_statement(env, repeat),
        Statement::Display(display) => eval_display_statement(env, display),
    }
}

/// Evaluates a [`Set`] statement, storing the result of the expression in the
/// environment.
fn eval_set_statement(env: &EnvRef, let_stmt: &Set) -> EvaluationOutcome {
    let value = eval_expression(env, &let_stmt.expr)?;
    let name = let_stmt.ident.name.clone();
    env.borrow_mut().store(name, value);
    return Continue(Rc::new(Value::Null));
}

/// Evaluates a [`Return`] statement, interrupting the traversal and returning the value
/// to the caller.
fn eval_return_statement(env: &EnvRef, ret: &Return) -> EvaluationOutcome {
    let value = eval_expression(env, &ret.value)?;
    return Break(TraversalInterrupt::ReturnValue(value));
}

/// Evaluates a [`FunctionDeclaration`] statement, storing the function in the
/// environment.
fn eval_func_decl_statement(
    env: &EnvRef,
    func: &FunctionDeclaration,
) -> EvaluationOutcome {
    let parameters = func
        .parameters
        .iter()
        .map(|ident| ident.name.clone())
        .collect();

    let name = func.name.name.clone();
    let value = Rc::new(Value::Function(Function {
        parameters,
        body: func.body.statements.clone(),
        env: env.clone(),
    }));

    env.borrow_mut().store(name, value);
    return Continue(Rc::new(Value::Null));
}

/// Evaluates a [`Display`] statement, printing the result of the expressions to the
/// console (or whatever [`stdoutln!`] outputs to).
fn eval_display_statement(env: &EnvRef, display: &Display) -> EvaluationOutcome {
    // Evaluate each item in the display statement, convert it to a string and print it as
    // one string, separated by spaces.
    let mut values = Vec::new();
    for expression in &display.expressions {
        match eval_expression(env, expression)?.cast_to_string() {
            Value::String(s) => values.push(s),
            _ => unreachable!(),
        };
    }
    stdoutln!("{}", values.join(" "));
    return Continue(Rc::new(Value::Null));
}

/// Evaluates a [`RepeatForever`] statement, repeating the body of the statement forever.
fn eval_repeat_forever_statement(
    env: &EnvRef,
    repeat: &RepeatForever,
) -> EvaluationOutcome {
    loop {
        eval_statements(env, &repeat.body.statements)?;
    }
}

/// Evaluates a [`RepeatNTimes`] statement, repeating the body of the statement `n` times.
fn eval_repeat_n_times_statement(
    env: &EnvRef,
    repeat: &RepeatNTimes,
) -> EvaluationOutcome {
    // From the given `repeat` arugment, determine the number of times to repeat the body.
    let n_rc = eval_expression(env, &repeat.n)?;
    let n = match n_rc.as_ref() {
        Value::Integer(n) => {
            if *n < 0 {
                return traversal_error!(
                    ErrorKind::TypeError,
                    "repeat _ times expected non-negative integer, got '{}'",
                    n
                );
            }
            *n as usize
        }
        _ => {
            return traversal_error!(
                ErrorKind::TypeError,
                "repeat _ times expected non-negative integer, got type {}",
                n_rc.variant_name()
            )
        }
    };

    // Repeat the body `n` times
    for _ in 0..n {
        eval_statements(env, &repeat.body.statements)?;
    }

    // This statement does not produce a value, so return `null`.
    return Continue(Rc::new(Value::Null));
}

/// Evaluates a [`RepeatUntil`] statement, repeating the body of the statement until a
/// given condition is `true`.
fn eval_repeat_until_statement(env: &EnvRef, repeat: &RepeatUntil) -> EvaluationOutcome {
    loop {
        // Determine if the condition is `true`, if so, break the loop.
        let condition = eval_expression(env, &repeat.condition)?.cast_to_boolean();
        let condition_value = match condition {
            Ok(value) => value,
            Err(e) => return traversal_error!(e),
        };
        if let Value::Boolean(true) = condition_value {
            break;
        }
        // Otherwise, continue evaluating the body of the repeat statement.
        eval_statements(env, &repeat.body.statements)?;
    }

    // This statement does not produce a value, so return `null`.
    return Continue(Rc::new(Value::Null));
}

/// Evaluates an [`Expression`], returning the result of the evaluation.
fn eval_expression(env: &EnvRef, expression: &Expression) -> EvaluationOutcome {
    match expression {
        Expression::Literal(literal) => eval_literal(literal),
        Expression::Unary(unary) => eval_unary_expression(env, unary),
        Expression::Binary(binary) => eval_binary_expression(env, binary),
        Expression::Selection(selection) => eval_selection_expression(env, selection),
        Expression::Identifier(ident) => eval_identifier_expression(env, ident),
        Expression::FunctionCall(func_call) => eval_func_call_expression(env, func_call),
        // Array related expressions have been implemented in the parser, but not in the
        // interpreter (yet!).
        _ => traversal_error!(
            ErrorKind::NotImplemented,
            "expression type '{}' has not been implemented by the interpreter",
            expression
        ),
    }
}

/// Evaluates a [`Literal`], returning the result of the evaluation.
fn eval_literal(literal: &Literal) -> EvaluationOutcome {
    let result = match literal {
        Literal::Integer { value, .. } => Ok(Value::Integer(*value)),
        Literal::Float { value, .. } => Ok(Value::Float(*value)),
        Literal::Boolean { value, .. } => Ok(Value::Boolean(*value)),
        Literal::String { value, .. } => Ok(Value::String(value.to_owned())),
    };
    match result {
        Ok(value) => Continue(Rc::new(value)),
        Err(e) => traversal_error!(e),
    }
}

/// Evaluates a [`Unary`] expression, returning the result of the evaluation.
fn eval_unary_expression(env: &EnvRef, unary: &Unary) -> EvaluationOutcome {
    // Evaluate the left hand side of the unary expression.
    let left_rc = eval_expression(env, &unary.operand)?;
    let left = left_rc.as_ref();

    // Perform the operation on the left hand side.
    let result = match unary.operator {
        TokenKind::Not => left.not(),
        TokenKind::Minus => left.neg(),
        _ => {
            return traversal_error!(
                ErrorKind::TypeError,
                "invalid operation '{}' for type {}",
                unary.operator,
                left.variant_name(),
            )
        }
    };
    match result {
        Ok(value) => Continue(Rc::new(value)),
        Err(e) => traversal_error!(e),
    }
}

/// Evaluates a [`Binary`] expression, returning the result of the evaluation.
fn eval_binary_expression(env: &EnvRef, binary: &Binary) -> EvaluationOutcome {
    // Evaluate the left and right hand side of the binary expression.
    let left_rc = eval_expression(env, &binary.left)?;
    let right_rc = eval_expression(env, &binary.right)?;

    // Borrow the values inside the Rc<Value>.
    let left = left_rc.as_ref();
    let right = right_rc.as_ref();

    // Perform the operation on the left and right hand side.
    let result = match binary.operator {
        TokenKind::Plus => left.add(right),
        TokenKind::Minus => left.sub(right),
        TokenKind::Mult => left.mul(right),
        TokenKind::Div => left.div(right),
        TokenKind::Eq => left.eq(right),
        TokenKind::NotEq => left.ne(right),
        TokenKind::Lt => left.lt(right),
        TokenKind::LtEq => left.le(right),
        TokenKind::Gt => left.gt(right),
        TokenKind::GtEq => left.ge(right),
        TokenKind::And => left.and(right),
        TokenKind::Or => left.or(right),
        TokenKind::Mod => left.rem(right),
        _ => {
            return traversal_error!(
                ErrorKind::TypeError,
                "invalid operation '{}' between {} and {}",
                binary.operator,
                left.variant_name(),
                right.variant_name(),
            )
        }
    };
    match result {
        Ok(value) => Continue(Rc::new(value)),
        Err(e) => traversal_error!(e),
    }
}

/// Evaluates a [`Selection`] expression, returning the result of the evaluation.
fn eval_selection_expression(env: &EnvRef, selection: &Selection) -> EvaluationOutcome {
    // Evaluate the condition of the selection statement.
    let result = eval_expression(env, &selection.condition)?.cast_to_boolean();
    let condition_value = match result {
        Ok(value) => value,
        Err(e) => return traversal_error!(e),
    };

    // If the condition is `true`, evaluate the body of the selection statement.
    // Otherwise, evaluate the body of the `else` statement, if it exists (otherwise,
    // return `null`).
    match condition_value {
        Value::Boolean(b) => {
            if b {
                eval_statements(env, &selection.conditional.statements)
            } else if let Some(else_conditional) = &selection.else_conditional {
                eval_statements(env, &else_conditional.statements)
            } else {
                Continue(Rc::new(Value::Null))
            }
        }
        // The `cast_to_boolean` function guarantees that the result is a boolean, so this
        // should never be reached.
        _ => unreachable!(),
    }
}

/// Evaluates an [`Identifier`] expression, returning the result of the evaluation.
fn eval_identifier_expression(env: &EnvRef, ident: &Identifier) -> EvaluationOutcome {
    return lookup_variable_name(env, &ident.name);
}

/// Evaluates a [`FunctionCall`] expression, returning the result of the evaluation.
fn eval_func_call_expression(
    env: &EnvRef,
    func_call: &FunctionCall,
) -> EvaluationOutcome {
    // Evaluate callee
    let callee = eval_expression(env, &func_call.callee)?;

    // Evaluate each argument and store the result in the arguments vector.
    let mut arguments = Vec::new();
    for expression in &func_call.arguments {
        let value = eval_expression(env, expression)?;
        arguments.push(value);
    }

    return apply_function(&callee, &arguments);
}

/// Applies a function to a list of arguments, returning the result of the evaluation.
///
/// To "apply" a function means to create a new environment for the function, execute
/// the body of the function, and return the result of the body.
fn apply_function(callee: &Rc<Value>, arguments: &Vec<Rc<Value>>) -> EvaluationOutcome {
    match &**callee {
        Value::Function(function) => {
            // Create a new environment for the function.
            let mut env = Environment::new_enclosed_environment(&function.env);

            // Transfer the arguments into the new environment.
            for (param_name, arg_value) in
                function.parameters.iter().zip(arguments.iter())
            {
                env.store(param_name.clone(), arg_value.clone())
            }

            // Execute the body of the function and handle the result
            match eval_statements(&Rc::new(RefCell::new(env)), &function.body) {
                Break(TraversalInterrupt::ReturnValue(value)) => Continue(value),
                Break(TraversalInterrupt::Error(err)) => traversal_error!(err),
                _ => Continue(Rc::new(Value::Null)),
            }
        }
        _ => traversal_error!(
            ErrorKind::TypeError,
            "'{}' is not callable",
            callee.variant_name(),
        ),
    }
}

/// Attempts to lookup a variable name in the environment, returning the result of the
/// evaluation.
fn lookup_variable_name(env: &EnvRef, name: &str) -> EvaluationOutcome {
    if let Some(value) = env.borrow().lookup(name) {
        Continue(value)
    } else {
        traversal_error!(ErrorKind::NameError, "variable '{}' does not exist", name)
    }
}
