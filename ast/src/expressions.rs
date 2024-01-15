use presap_lexer::token::Span;

use crate::{Block, Literal};

pub enum Expression {
    Identifier(IdentifierExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Selection(SelectionExpression),
    FunctionDeclaration(FunctionDeclarationExpression),
    FunctionCall(FunctionCallExpression),
    Index(IndexExpression),
    Literal(Literal),
}

pub struct IdentifierExpression {
    pub name: String,
    pub span: Span,
}

pub struct UnaryExpression {
    pub operator: String,
    pub operand: Box<Expression>,
    pub span: Span,
}

pub struct BinaryExpression {
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

pub struct SelectionExpression {
    pub condition: Box<Expression>,
    pub conditional: Block,
    pub else_conditional: Option<Block>,
    pub span: Span,
}

pub struct FunctionDeclarationExpression {
    pub name: IdentifierExpression,
    pub parameters: Vec<IdentifierExpression>,
    pub body: Block,
    pub span: Span,
}

pub struct FunctionCallExpression {
    pub name: IdentifierExpression,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

pub struct IndexExpression {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
}
