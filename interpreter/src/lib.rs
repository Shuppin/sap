use core::error::{Error, ErrorKind};
use core::{err, Value};
use std::ops::ControlFlow;

use ast::expression::{Binary, Expression, Selection, Unary};
use ast::literal::Literal;
use ast::statement::{Return, Statement};
use ast::Program;
use lexer::token::TokenKind;

#[cfg(test)]
mod test;

/// Represents a break in traversal of the AST.
///
/// The reasons we would want to stop traversal are:
/// 1. We encountered an error.
/// 2. The AST contains a return statement, and we need to stop traversal of the current
///    branch and return the value back to the start of that branch.
pub enum TraversalBreak {
    ReturnValue(Value),
    Error(Error),
}

type EvalOutcome = ControlFlow<TraversalBreak, Value>;

fn map_result_to_eval_outcome(result: Result<Value, Error>) -> EvalOutcome {
    match result {
        Ok(value) => ControlFlow::Continue(value),
        Err(error) => ControlFlow::Break(TraversalBreak::Error(error)),
    }
}

pub fn eval(ast: Program) -> Result<Value, Error> {
    match eval_statements(&ast.statements) {
        ControlFlow::Continue(value) => Ok(value),
        ControlFlow::Break(TraversalBreak::ReturnValue(_)) => {
            err!(ErrorKind::TypeError, "'return' used outside of function")
        }
        ControlFlow::Break(TraversalBreak::Error(error)) => Err(error),
    }
}

fn eval_statements(statements: &Vec<Statement>) -> EvalOutcome {
    let mut value = Value::Null;
    for statement in statements {
        value = eval_statement(&statement)?;
    }
    return ControlFlow::Continue(value);
}

fn eval_fn_block(statements: &Vec<Statement>) -> EvalOutcome {
    // A fn block will unwrap return values and just return the inner value
    match eval_statements(statements) {
        ControlFlow::Continue(value) => ControlFlow::Continue(value),
        ControlFlow::Break(TraversalBreak::ReturnValue(value)) => ControlFlow::Continue(value),
        ControlFlow::Break(TraversalBreak::Error(error)) => {
            ControlFlow::Break(TraversalBreak::Error(error))
        }
    }
}

fn eval_selection_block(statements: &Vec<Statement>) -> EvalOutcome {
    // A selection block behaves just the same as a list of statements
    eval_statements(statements)
}

fn eval_statement(statement: &Statement) -> EvalOutcome {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        Statement::Return(ret) => eval_return_statement(ret),
        _ => todo!(),
    }
}

fn eval_return_statement(ret: &Return) -> EvalOutcome {
    let value = eval_expression(&ret.value)?;
    return ControlFlow::Break(TraversalBreak::ReturnValue(value));
}

fn eval_expression(expression: &Expression) -> EvalOutcome {
    match expression {
        Expression::Literal(literal) => eval_literal(literal),
        Expression::Unary(unary) => eval_unary_expression(unary),
        Expression::Binary(binary) => eval_binary_expression(binary),
        Expression::Selection(selection) => eval_selection_expression(selection),
        _ => todo!(),
    }
}

fn eval_selection_expression(selection: &Selection) -> EvalOutcome {
    let result = eval_expression(&selection.condition)?.to_boolean();
    let condition = map_result_to_eval_outcome(result)?;
    match condition {
        Value::Boolean(b) => {
            if b {
                eval_selection_block(&selection.conditional.statements)
            } else if let Some(else_conditional) = &selection.else_conditional {
                eval_selection_block(&else_conditional.statements)
            } else {
                ControlFlow::Continue(Value::Null)
            }
        }
        _ => unreachable!(),
    }
}

fn eval_unary_expression(unary: &Unary) -> EvalOutcome {
    let left = eval_expression(&unary.operand)?;
    let result = match unary.operator {
        TokenKind::Not => !left,
        TokenKind::Minus => -left,
        _ => todo!(),
    };
    map_result_to_eval_outcome(result)
}

fn eval_binary_expression(binary: &Binary) -> EvalOutcome {
    let left = eval_expression(&binary.left)?;
    let right = eval_expression(&binary.right)?;
    let result = match binary.operator {
        TokenKind::Plus => left + right,
        TokenKind::Minus => left - right,
        TokenKind::Mult => left * right,
        TokenKind::Div => left / right,
        TokenKind::Eq => left.eq(&right),
        TokenKind::NotEq => left.ne(&right),
        TokenKind::Lt => left.lt(&right),
        TokenKind::LtEq => left.le(&right),
        TokenKind::Gt => left.gt(&right),
        TokenKind::GtEq => left.ge(&right),
        TokenKind::And => left.and(&right),
        TokenKind::Or => left.or(&right),
        _ => todo!(),
    };
    map_result_to_eval_outcome(result)
}

fn eval_literal(literal: &Literal) -> EvalOutcome {
    let result = match literal {
        Literal::Integer { value, .. } => Ok(Value::Integer(*value)),
        Literal::Float { value, .. } => Ok(Value::Float(*value)),
        Literal::Boolean { value, .. } => Ok(Value::Boolean(*value)),
        _ => todo!(),
    };
    map_result_to_eval_outcome(result)
}
