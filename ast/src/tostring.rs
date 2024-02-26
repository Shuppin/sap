//! This module contains the `Display` trait implementations for the AST nodes.
//!
//! The `Display` trait is used to convert an AST node to a string representation. This
//! is useful for debugging and testing the code.
use std::fmt::{Display, Formatter, Result};

use lexer::token::TokenKind;

use crate::expression::*;
use crate::literal::Literal;
use crate::statement::{
    FunctionDeclaration, RepeatForever, RepeatNTimes, RepeatUntil, Return, Set, Statement,
};
use crate::{Program, StatementList};

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_semi_seperated(&self.statements))
    }
}

impl Display for StatementList {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_semi_seperated(&self.statements))
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Set(Set { ident, expr, .. }) => {
                write!(f, "{} {} = {}", TokenKind::Set, ident.name, expr)
            }
            Statement::Return(Return { value, .. }) => {
                write!(f, "{} {}", TokenKind::Return, value)
            }
            Statement::Expression(expr) => write!(f, "{}", expr),
            Statement::FunctionDeclaration(FunctionDeclaration {
                name,
                parameters,
                body,
                ..
            }) => {
                write!(
                    f,
                    "{} {name}({}){}{}",
                    TokenKind::DefineFunction,
                    format_comma_seperated(&parameters),
                    format_stmt_list(body),
                    TokenKind::End
                )
            }
            Statement::RepeatNTimes(RepeatNTimes { n, body, .. }) => {
                write!(
                    f,
                    "{} {n} {}{}{}",
                    TokenKind::Repeat,
                    TokenKind::Times,
                    format_stmt_list(body),
                    TokenKind::End
                )
            }
            Statement::RepeatUntil(RepeatUntil {
                condition, body, ..
            }) => {
                write!(
                    f,
                    "{} {} {condition}{}{}",
                    TokenKind::Repeat,
                    TokenKind::Until,
                    format_stmt_list(body),
                    TokenKind::End
                )
            }
            Statement::RepeatForever(RepeatForever { body, .. }) => {
                write!(
                    f,
                    "{} {}{}{}",
                    TokenKind::Repeat,
                    TokenKind::Forever,
                    format_stmt_list(body),
                    TokenKind::End
                )
            }
            Statement::Display(display) => {
                write!(
                    f,
                    "{} {}",
                    TokenKind::Display,
                    format_comma_seperated(&display.expressions)
                )
            }
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
            }) => match operator {
                TokenKind::Not => write!(f, "({} {})", operator, operand),
                _ => write!(f, "({}{})", operator, operand),
            },
            Expression::Binary(Binary {
                operator,
                left,
                right,
                ..
            }) => write!(f, "({} {} {})", left, operator, right),
            Expression::FunctionCall(FunctionCall {
                callee, arguments, ..
            }) => write!(f, "{}({})", callee, format_comma_seperated(arguments)),
            Expression::Index(Index { object, index, .. }) => write!(f, "{}[{}]", object, index),
            Expression::Selection(Selection {
                condition,
                conditional,
                else_conditional,
                ..
            }) => {
                let formatted_conditional = match conditional.statements.is_empty() {
                    true => "".to_string(),
                    false => format!(" {}", conditional),
                };

                let formatted_else_conditional = match else_conditional {
                    Some(else_conditional) => match else_conditional.statements.is_empty() {
                        true => format!(" {}", TokenKind::Otherwise),
                        false => format!(" {} {else_conditional}", TokenKind::Otherwise),
                    },
                    None => "".to_string(),
                };

                write!(
                    f,
                    "{} {condition} {}{formatted_conditional}{formatted_else_conditional} {}",
                    TokenKind::If,
                    TokenKind::Then,
                    TokenKind::End,
                )
            }
            Expression::Array(Array { elements, .. }) => {
                write!(f, "[{}]", format_comma_seperated(&elements))
            }
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

/// Formats a list of statements into a string representation.
fn format_stmt_list(list: &StatementList) -> String {
    // Some formatting to make the output more readable.
    // If the body is empty, insert a single space between in place of the body.
    // Otherwise, insert the body with a single space before and after it.
    match list.statements.is_empty() {
        true => " ".to_string(),
        false => format!(" {} ", list),
    }
}

/// Formats a list of items into a semi-colon separated string representation.
fn format_semi_seperated<T: ToString>(items: &Vec<T>) -> String {
    return items
        .iter()
        .map(|item| item.to_string())
        .collect::<Vec<String>>()
        .join("; ");
}

/// Formats a list of items into a comma separated string representation.
fn format_comma_seperated<T: ToString>(items: &Vec<T>) -> String {
    return items
        .iter()
        .map(|item| item.to_string())
        .collect::<Vec<String>>()
        .join(", ");
}
