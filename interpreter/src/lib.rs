use core::error::{Error, ErrorKind};
use core::{err, Function, Value};
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
use runtime::CallStack;

mod runtime;

#[cfg(test)]
mod test;

pub struct Interpreter {
    callstack: CallStack,
}

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

impl Interpreter {
    pub fn new() -> Self {
        Self {
            callstack: CallStack::new(),
        }
    }

    pub fn eval_program(&mut self, ast: Program) -> Result<Rc<Value>, Error> {
        match self.eval_statements(&ast.statements) {
            Continue(value) => Ok(value),
            Break(TraversalBreak::ReturnValue(_)) => {
                err!(ErrorKind::TypeError, "'return' used outside of function")
            }
            Break(TraversalBreak::Error(error)) => Err(error),
        }
    }

    fn eval_statements(&mut self, statements: &Vec<Statement>) -> EvalOutcome {
        let mut value = Rc::new(Value::Null);
        for statement in statements {
            value = self.eval_statement(&statement)?;
        }
        return Continue(value);
    }

    fn eval_selection_block(&mut self, statements: &Vec<Statement>) -> EvalOutcome {
        // A selection block behaves just the same as a list of statements
        self.eval_statements(statements)
    }

    fn eval_statement(&mut self, statement: &Statement) -> EvalOutcome {
        match statement {
            Statement::Expression(expression) => self.eval_expression(expression),
            Statement::Return(ret) => self.eval_return_statement(ret),
            Statement::Let(let_stmt) => self.eval_let_statement(let_stmt),
        }
    }

    fn eval_let_statement(&mut self, let_stmt: &Let) -> EvalOutcome {
        let value = self.eval_expression(&let_stmt.expr)?;
        self.callstack
            .current_record
            .set(&let_stmt.ident.name, value);
        Continue(Rc::new(Value::Null))
    }

    fn eval_return_statement(&mut self, ret: &Return) -> EvalOutcome {
        let value = self.eval_expression(&ret.value)?;
        return Break(TraversalBreak::ReturnValue(value));
    }

    fn eval_expression(&mut self, expression: &Expression) -> EvalOutcome {
        match expression {
            Expression::Literal(literal) => self.eval_literal(literal),
            Expression::Unary(unary) => self.eval_unary_expression(unary),
            Expression::Binary(binary) => self.eval_binary_expression(binary),
            Expression::Selection(selection) => self.eval_selection_expression(selection),
            Expression::Identifier(ident) => self.eval_identifier_expression(ident),
            Expression::FunctionDeclaration(func) => self.eval_func_decl_expression(func),
            Expression::FunctionCall(func_call) => self.eval_func_call_expression(func_call),
            _ => todo!(),
        }
    }

    fn eval_func_call_expression(&mut self, func_call: &FunctionCall) -> EvalOutcome {
        let callee_rc = self.eval_expression(&func_call.callee)?;
        let callee = callee_rc.as_ref();

        match callee {
            Value::Function(func) => {
                // Compare arguments to parameters
                if func_call.arguments.len() != func.parameters.len() {
                    return Break(TraversalBreak::Error(Error::new(
                        format!(
                            "Expected {} argument(s), got {}",
                            func.parameters.len(),
                            func_call.arguments.len()
                        )
                        .as_str(),
                        ErrorKind::TypeError,
                    )));
                }

                // Evaluate arguments
                let mut arguments: Vec<(&str, Rc<Value>)> = Vec::new();

                // Zip the argument and parameter vectors together
                for (expression, name) in func_call.arguments.iter().zip(func.parameters.iter()) {
                    let value = self.eval_expression(expression)?;
                    arguments.push((name, value));
                }

                // Enter new scope
                self.callstack.push_new_record();
                for (name, value) in arguments {
                    self.callstack.current_record.set(name, value)
                }
                // Execute the body of the function and handle the result
                let return_value = match self.eval_statements(&func.body) {
                    // If a return value is encountered, use it as the result
                    Break(TraversalBreak::ReturnValue(value)) => value,

                    // Propagate errors upwards by immediately returning them
                    Break(TraversalBreak::Error(err)) => return Break(TraversalBreak::Error(err)),

                    // If execution concludes without a ReturnValue, default to Null
                    _ => Rc::new(Value::Null),
                };
                // Discard scope
                self.callstack.discard_current_record();

                // Send return value back to caller
                match *return_value {
                    Value::Function(_) => Break(TraversalBreak::Error(Error::new(
                        "Function returns type 'Function', which is not yet supported",
                        ErrorKind::TypeError,
                    ))),
                    _ => Continue(return_value),
                }
            }
            _ => Break(TraversalBreak::Error(Error::new(
                format!("'{}' is not callable", callee.variant_name()).as_str(),
                ErrorKind::TypeError,
            ))),
        }
    }

    fn eval_func_decl_expression(&self, func: &FunctionDeclaration) -> EvalOutcome {
        let parameters = func
            .parameters
            .iter()
            .map(|ident| ident.name.clone())
            .collect();

        Continue(Rc::new(Value::Function(Function {
            parameters,
            body: func.body.statements.clone(),
        })))
    }

    fn eval_identifier_expression(
        &self,
        ident: &Identifier,
    ) -> ControlFlow<TraversalBreak, Rc<Value>> {
        self.lookup_variable_name(&ident.name)
    }

    fn eval_selection_expression(&mut self, selection: &Selection) -> EvalOutcome {
        let result = self.eval_expression(&selection.condition)?.to_boolean();
        let condition_value = match result {
            Ok(value) => value,
            Err(e) => return Break(TraversalBreak::Error(e)),
        };
        match condition_value {
            Value::Boolean(b) => {
                if b {
                    self.eval_selection_block(&selection.conditional.statements)
                } else if let Some(else_conditional) = &selection.else_conditional {
                    self.eval_selection_block(&else_conditional.statements)
                } else {
                    Continue(Rc::new(Value::Null))
                }
            }
            _ => unreachable!(),
        }
    }

    fn eval_unary_expression(&mut self, unary: &Unary) -> EvalOutcome {
        let left_rc = self.eval_expression(&unary.operand)?;
        let left = left_rc.as_ref();
        let result = match unary.operator {
            TokenKind::Not => left.not(),
            TokenKind::Minus => left.neg(),
            _ => todo!(),
        };
        match result {
            Ok(value) => Continue(Rc::new(value)),
            Err(e) => Break(TraversalBreak::Error(e)),
        }
    }

    fn eval_binary_expression(&mut self, binary: &Binary) -> EvalOutcome {
        let left_rc = self.eval_expression(&binary.left)?;
        let right_rc = self.eval_expression(&binary.right)?;

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
            Err(e) => Break(TraversalBreak::Error(e)),
        }
    }

    fn eval_literal(&self, literal: &Literal) -> EvalOutcome {
        let result = match literal {
            Literal::Integer { value, .. } => Ok(Value::Integer(*value)),
            Literal::Float { value, .. } => Ok(Value::Float(*value)),
            Literal::Boolean { value, .. } => Ok(Value::Boolean(*value)),
            _ => todo!(),
        };
        match result {
            Ok(value) => Continue(Rc::new(value)),
            Err(e) => Break(TraversalBreak::Error(e)),
        }
    }

    fn lookup_variable_name(&self, name: &str) -> EvalOutcome {
        if let Some(value) = self.callstack.lookup(name) {
            Continue(value)
        } else {
            Break(TraversalBreak::Error(Error::new(
                format!("variable '{}' does not exist", name).as_str(),
                ErrorKind::NameError,
            )))
        }
    }
}
