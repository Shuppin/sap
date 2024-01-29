use core::Value;

use ast::expression::{Expression, Unary};
use ast::literal::Literal;
use ast::statement::Statement;
use ast::Program;
use lexer::token::TokenKind;

#[cfg(test)]
mod test;

pub fn eval(ast: Program) -> Value {
    let mut value = Value::Null;
    for statement in ast.statements {
        value = eval_statement(&statement);
    }
    return value;
}

fn eval_statement(statement: &Statement) -> Value {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        _ => todo!(),
    }
}

fn eval_expression(expression: &Expression) -> Value {
    match expression {
        Expression::Literal(literal) => eval_literal(literal),
        Expression::Unary(unary) => eval_unary_expression(unary),
        _ => todo!(),
    }
}

fn eval_unary_expression(unary: &Unary) -> Value {
    let left = eval_expression(&unary.operand);
    match unary.operator {
        TokenKind::Not => eval_not_operation(left),
        TokenKind::Minus => eval_minus_prefix_operation(left),
        _ => todo!(),
    }
}

fn eval_not_operation(value: Value) -> Value {
    match value {
        Value::Integer(n) => Value::Boolean(n == 0),
        Value::Float(n) => Value::Boolean(n == 0.0),
        Value::Boolean(b) => Value::Boolean(!b),
        Value::Null => Value::Boolean(true),
    }
}

fn eval_minus_prefix_operation(value: Value) -> Value {
    match value {
        Value::Integer(n) => Value::Integer(-n),
        Value::Float(n) => Value::Float(-n),
        Value::Boolean(b) => Value::Integer(if b { -1 } else { 0 }),
        Value::Null => todo!(),
    }
}

fn eval_literal(literal: &Literal) -> Value {
    match literal {
        Literal::Integer { value, .. } => Value::Integer(*value),
        Literal::Boolean { value, .. } => Value::Boolean(*value),
        _ => todo!(),
    }
}
