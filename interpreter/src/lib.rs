use core::Value;

use ast::expression::Expression;
use ast::literal::Literal;
use ast::statement::Statement;
use ast::Program;

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
        _ => todo!(),
    }
}

fn eval_literal(literal: &Literal) -> Value {
    match literal {
        Literal::Integer { value, .. } => Value::Integer(*value),
        _ => todo!()
    }
}
