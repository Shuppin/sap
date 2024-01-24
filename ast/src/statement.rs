use presap_lexer::token::Span;
use serde::Serialize;

use crate::{
    expression::{Expression, Identifier},
    GetSpan,
};

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Statement {
    Let(Let),
    Return(Return),
    Expression(Expression),
}

impl GetSpan for Statement {
    fn span(&self) -> &Span {
        match self {
            Statement::Let(let_stmt) => &let_stmt.span,
            Statement::Return(ret_stmt) => &ret_stmt.span,
            Statement::Expression(expr) => expr.span(),
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub struct Let {
    pub ident: Identifier,
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub struct Return {
    pub value: Expression,
    pub span: Span,
}
