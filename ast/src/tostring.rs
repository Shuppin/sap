use std::fmt::{Display, Formatter, Result};

use crate::expression::*;
use crate::literal::Literal;
use crate::statement::{Return, Set, Statement};
use crate::{Program, StatementList};

fn format_items<T: ToString>(items: &Vec<T>) -> String {
    return items
        .iter()
        .map(|item| item.to_string())
        .collect::<Vec<String>>()
        .join("; ");
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_items(&self.statements))
    }
}

impl Display for StatementList {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_items(&self.statements))
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Set(Set { ident, expr, .. }) => {
                return write!(f, "set {} = {}", ident.name, expr);
            }
            Statement::Return(Return { value, .. }) => {
                write!(f, "return {}", value)
            }
            Statement::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
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
            Expression::Selection(Selection {
                condition,
                conditional,
                else_conditional,
                ..
            }) => {
                let conditional_str = match conditional.statements.is_empty() {
                    true => "".to_string(),
                    false => format!(" {}", conditional),
                };

                let else_conditional_str = match else_conditional {
                    Some(else_conditional) => match else_conditional.statements.is_empty() {
                        true => " otherwise".to_string(),
                        false => format!(" otherwise {}", else_conditional),
                    },
                    None => "".to_string(),
                };

                write!(
                    f,
                    "if {} then{}{} end",
                    condition, conditional_str, else_conditional_str
                )
            }

            Expression::FunctionDeclaration(FunctionDeclaration {
                parameters, body, ..
            }) => {
                let body_block = match body.statements.is_empty() {
                    true => "{}".to_string(),
                    false => format!("{{ {} }}", body),
                };

                write!(
                    f,
                    "fn ({}) {}",
                    parameters
                        .iter()
                        .map(|expr| expr.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    body_block
                )
            }
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

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.name)
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
