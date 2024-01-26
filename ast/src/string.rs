use std::fmt::{Display, Formatter, Result};

use crate::expression::*;
use crate::literal::Literal;
use crate::statement::{Let, Return, Statement};
use crate::{Node, Program};

fn format_items<T: ToString>(items: &Vec<T>) -> String {
    return items
        .iter()
        .map(|item| item.to_string())
        .collect::<Vec<String>>()
        .join(", ");
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Node::Program(p) => write!(f, "{}", p),
            Node::Statement(s) => write!(f, "{}", s),
            Node::Expression(e) => write!(f, "{}", e),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_items(&self.statements))
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Let(Let { ident, expr, .. }) => {
                return write!(f, "let {} = {};", ident.name, expr);
            }
            Statement::Return(Return { value, .. }) => {
                write!(f, "return {};", value)
            }
            Statement::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Identifier(Identifier { name: id, .. }) => write!(f, "{}", id),
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Unary(Unary {
                operator, operand, ..
            }) => write!(f, "({}{})", operator, operand),
            Expression::Binary(Binary {
                operator,
                left,
                right,
                ..
            }) => write!(f, "({} {} {})", left, operator, right),
            Expression::FunctionCall(FunctionCall {
                callee, arguments, ..
            }) => write!(
                f,
                "{}({})",
                callee,
                arguments
                    .iter()
                    .map(|expr| expr.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Index(Index { object, index, .. }) => write!(f, "{}[{}]", object, index),
            Expression::Selection(_) => todo!(),
            Expression::FunctionDeclaration(_) => todo!(),
            Expression::Array(Array { elements, .. }) => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(|expr| expr.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Literal::Integer { value, .. } => write!(f, "{}", value),
            Literal::Boolean { value, .. } => write!(f, "{}", value),
            Literal::Float { value, .. } => write!(f, "{}", value),
            Literal::String { value, .. } => write!(f, "\"{}\"", value),
        }
    }
}
