use core::error::Error;
use core::Value;

use ast::expression::{Binary, Expression, Unary};
use ast::literal::Literal;
use ast::statement::Statement;
use ast::Program;
use lexer::token::TokenKind;

#[cfg(test)]
mod test;

pub fn eval(ast: Program) -> Result<Value, Error> {
    let mut value = Value::Null;
    for statement in ast.statements {
        value = eval_statement(&statement)?;
    }
    return Ok(value);
}

fn eval_statement(statement: &Statement) -> Result<Value, Error> {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        _ => todo!(),
    }
}

fn eval_expression(expression: &Expression) -> Result<Value, Error> {
    match expression {
        Expression::Literal(literal) => eval_literal(literal),
        Expression::Unary(unary) => eval_unary_expression(unary),
        Expression::Binary(binary) => eval_binary_expression(binary),
        _ => todo!(),
    }
}

fn eval_unary_expression(unary: &Unary) -> Result<Value, Error> {
    let left = eval_expression(&unary.operand)?;
    match unary.operator {
        TokenKind::Not => !left,
        TokenKind::Minus => -left,
        _ => todo!(),
    }
}

fn eval_binary_expression(binary: &Binary) -> Result<Value, Error> {
    let left = eval_expression(&binary.left)?;
    let right = eval_expression(&binary.right)?;
    match binary.operator {
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
    }
}

fn eval_literal(literal: &Literal) -> Result<Value, Error> {
    match literal {
        Literal::Integer { value, .. } => Ok(Value::Integer(*value)),
        Literal::Float { value, .. } => Ok(Value::Float(*value)),
        Literal::Boolean { value, .. } => Ok(Value::Boolean(*value)),
        _ => todo!(),
    }
}
