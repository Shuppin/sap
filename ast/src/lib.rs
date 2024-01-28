pub mod expression;
pub mod literal;
pub mod statement;
pub mod string;

use presap_lexer::token::Span;
use serde::Serialize;
use statement::Statement;

pub trait GetSpan {
    fn span(&self) -> &Span;
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
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

#[derive(Debug, Serialize)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

impl Block {
    pub fn new() -> Self {
        Self {
            statements: vec![],
            span: Span { start: 0, end: 0 },
        }
    }
}
