pub mod expression;
pub mod statement;
pub mod string;

use presap_lexer::token::Span;
use serde::Serialize;

use crate::{expression::*, statement::Statement};

pub trait GetSpan {
    fn span(&self) -> &Span;
}

#[derive(Debug, Serialize)]
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

impl GetSpan for Literal {
    fn span(&self) -> &Span {
        match self {
            Self::Integer { span, .. } => span,
            Self::Float { span, .. } => span,
            Self::String { span, .. } => span,
            Self::Boolean { span, .. } => span,
            Self::Array { span, .. } => span,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Serialize)]
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
