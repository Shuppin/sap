use std::cell::RefCell;
use std::ops::ControlFlow;
use std::ops::ControlFlow::{Break, Continue};
use std::rc::Rc;

use ast::expression::{
    Binary, Expression, FunctionCall, FunctionDeclaration, Identifier, Selection, Unary,
};
use ast::literal::Literal;
use ast::statement::{Let, Return, Statement};
use ast::Program;
use lexer::token::TokenKind;
use shared::err;
use shared::error::{Error, ErrorKind};

use crate::runtime::{EnvRef, Environment};
use crate::value::{Function, Value};

// Attempt to obtain the current version of the CLI package
pub const VERSION: Option<&str> = std::option_env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod test;

pub mod runtime;
pub mod value;

/// Represents a break in traversal of the AST.
///
/// The reasons we would want to stop traversal are:
/// 1. We encountered an error.
/// 2. The AST contains a return statement, and we need to stop traversal of the current
///    branch and return the value back to the start of that branch.
pub enum TraversalBreak {
    ReturnValue(Rc<Value>),
    Error(Error),
}

type EvalOutcome = ControlFlow<TraversalBreak, Rc<Value>>;

macro_rules! traversal_error {
    ($kind:expr, $($arg:tt)*) => {
        Break(TraversalBreak::Error(Error::new(&format!($($arg)*), $kind)))
    };
    ($err:ident) => {
        Break(TraversalBreak::Error($err))
    }
}

pub fn create_env() -> EnvRef {
    return Rc::new(RefCell::new(Environment::new()));
}

pub fn eval_program(env: &EnvRef, ast: Program) -> Result<(EnvRef, Rc<Value>), Error> {
    match eval_statements(env, &ast.statements) {
        Break(TraversalBreak::ReturnValue(_)) => {
            err!(ErrorKind::TypeError, "'return' used outside of function")
        }
        Break(TraversalBreak::Error(error)) => Err(error),
        Continue(value) => Ok((env.clone(), value)),
    }
}

fn eval_statements(env: &EnvRef, statements: &Vec<Statement>) -> EvalOutcome {
    let mut value = Rc::new(Value::Null);
    for statement in statements {
        value = eval_statement(env, &statement)?;
    }
    return Continue(value);
}

fn eval_statement(env: &EnvRef, statement: &Statement) -> EvalOutcome {
    match statement {
        Statement::Expression(expression) => eval_expression(env, expression),
        Statement::Return(ret) => eval_return_statement(env, ret),
        Statement::Let(let_stmt) => eval_let_statement(env, let_stmt),
    }
}

fn eval_let_statement(env: &EnvRef, let_stmt: &Let) -> EvalOutcome {
    let value = eval_expression(env, &let_stmt.expr)?;
    let name = let_stmt.ident.name.clone();
    env.borrow_mut().store(name, value);
    return Continue(Rc::new(Value::Null));
}

fn eval_return_statement(env: &EnvRef, ret: &Return) -> EvalOutcome {
    let value = eval_expression(env, &ret.value)?;
    return Break(TraversalBreak::ReturnValue(value));
}

fn eval_expression(env: &EnvRef, expression: &Expression) -> EvalOutcome {
    match expression {
        Expression::Literal(literal) => eval_literal(literal),
        Expression::Unary(unary) => eval_unary_expression(env, unary),
        Expression::Binary(binary) => eval_binary_expression(env, binary),
        Expression::Selection(selection) => eval_selection_expression(env, selection),
        Expression::Identifier(ident) => eval_identifier_expression(env, ident),
        Expression::FunctionDeclaration(func) => eval_func_decl_expression(env, func),
        Expression::FunctionCall(func_call) => eval_func_call_expression(env, func_call),
        _ => todo!(),
    }
}

fn eval_literal(literal: &Literal) -> EvalOutcome {
    let result = match literal {
        Literal::Integer { value, .. } => Ok(Value::Integer(*value)),
        Literal::Float { value, .. } => Ok(Value::Float(*value)),
        Literal::Boolean { value, .. } => Ok(Value::Boolean(*value)),
        _ => todo!(),
    };
    match result {
        Ok(value) => Continue(Rc::new(value)),
        Err(e) => traversal_error!(e),
    }
}

fn eval_unary_expression(env: &EnvRef, unary: &Unary) -> EvalOutcome {
    let left_rc = eval_expression(env, &unary.operand)?;
    let left = left_rc.as_ref();
    let result = match unary.operator {
        TokenKind::Not => left.not(),
        TokenKind::Minus => left.neg(),
        _ => todo!(),
    };
    match result {
        Ok(value) => Continue(Rc::new(value)),
        Err(e) => traversal_error!(e),
    }
}

fn eval_binary_expression(env: &EnvRef, binary: &Binary) -> EvalOutcome {
    let left_rc = eval_expression(env, &binary.left)?;
    let right_rc = eval_expression(env, &binary.right)?;

    // Borrow the values inside the Rc<Value>
    let left = left_rc.as_ref();
    let right = right_rc.as_ref();

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
        _ => todo!(),
    };
    match result {
        Ok(value) => Continue(Rc::new(value)),
        Err(e) => traversal_error!(e),
    }
}

fn eval_selection_expression(env: &EnvRef, selection: &Selection) -> EvalOutcome {
    let result = eval_expression(env, &selection.condition)?.to_boolean();
    let condition_value = match result {
        Ok(value) => value,
        Err(e) => return Break(TraversalBreak::Error(e)),
    };
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
        _ => unreachable!(),
    }
}

fn eval_identifier_expression(env: &EnvRef, ident: &Identifier) -> EvalOutcome {
    return lookup_variable_name(env, &ident.name);
}

fn eval_func_decl_expression(env: &EnvRef, func: &FunctionDeclaration) -> EvalOutcome {
    let parameters = func
        .parameters
        .iter()
        .map(|ident| ident.name.clone())
        .collect();

    return Continue(Rc::new(Value::Function(Function {
        parameters,
        body: func.body.statements.clone(),
        env: env.clone(),
    })));
}

fn eval_func_call_expression(env: &EnvRef, func_call: &FunctionCall) -> EvalOutcome {
    // Evaluate callee
    let callee = eval_expression(env, &func_call.callee)?;

    // Evaluate arguments
    let mut arguments = Vec::new();

    for expression in &func_call.arguments {
        let value = eval_expression(env, expression)?;
        arguments.push(value);
    }

    return apply_function(&callee, &arguments);
}

fn apply_function(callee: &Rc<Value>, arguments: &Vec<Rc<Value>>) -> EvalOutcome {
    match &**callee {
        Value::Function(function) => {
            let mut env = Environment::new_enclosed_environment(&function.env);

            for (param_name, arg_value) in function.parameters.iter().zip(arguments.iter()) {
                env.store(param_name.clone(), arg_value.clone())
            }

            // Execute the body of the function and handle the result
            match eval_statements(&Rc::new(RefCell::new(env)), &function.body) {
                Break(TraversalBreak::ReturnValue(value)) => Continue(value),
                Break(TraversalBreak::Error(err)) => traversal_error!(err),
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

fn lookup_variable_name(env: &EnvRef, name: &str) -> EvalOutcome {
    if let Some(value) = env.borrow().lookup(name) {
        Continue(value)
    } else {
        traversal_error!(ErrorKind::NameError, "variable '{}' does not exist", name)
    }
}
