use presap_lexer::token::Span;

use crate::{ast_expressions::*, ast_statements::Statement};

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

pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}
