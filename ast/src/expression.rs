use presap_lexer::token::Span;

use crate::{Block, Literal};

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Unary(Unary),
    Binary(Binary),
    Selection(Selection),
    FunctionDeclaration(FunctionDeclaration),
    FunctionCall(FunctionCall),
    Index(Index),
    Literal(Literal),
    None,
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Unary {
    pub operator: String,
    pub operand: Box<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Binary {
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Selection {
    pub condition: Box<Expression>,
    pub conditional: Block,
    pub else_conditional: Option<Block>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: Identifier,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Index {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
}
