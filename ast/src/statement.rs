use serde::Serialize;
use shared::span::{GetSpan, Span};

use crate::expression::{Expression, Identifier};

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(untagged)]
pub enum Statement {
    Set(Set),
    Return(Return),
    Expression(Expression),
}

impl GetSpan for Statement {
    fn span(&self) -> &Span {
        match self {
            Statement::Set(set_stmt) => &set_stmt.span,
            Statement::Return(ret_stmt) => &ret_stmt.span,
            Statement::Expression(expr) => expr.span(),
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Set {
    pub ident: Identifier,
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Return {
    pub value: Expression,
    pub span: Span,
}
