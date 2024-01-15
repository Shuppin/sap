use presap_lexer::token::Span;

use crate::ast_expressions::{Expression, IdentifierExpression};

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

pub struct LetStatement {
    pub name: IdentifierExpression,
    pub value: Expression,
    pub span: Span,
}

pub struct ReturnStatement {
    pub value: Expression,
    pub span: Span,
}
