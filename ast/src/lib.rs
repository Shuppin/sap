pub mod expression;
pub mod statement;

use presap_lexer::token::Span;

use crate::{expression::*, statement::Statement};

#[derive(Debug)]
pub enum Literal {
    Integer {
        value: i64,
        span: Span,
    },
    Float {
        value: f64,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    Boolean {
        value: bool,
        span: Span,
    },
    Array {
        elements: Vec<Expression>,
        span: Span,
    },
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: vec![],
            span: Span { start: 0, end: 0 },
        }
    }
}

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}
