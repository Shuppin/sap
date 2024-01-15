use presap_lexer::token::Span;

use crate::expression::{Expression, Identifier};

#[derive(Debug)]
pub enum Statement {
    Let(Let),
    Return(Return),
    Expression(Expression),
}

#[derive(Debug)]
pub struct Let {
    pub ident: Identifier,
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub struct Return {
    pub value: Expression,
    pub span: Span,
}
