use lexer::token::TokenKind;
use serde::Serialize;
use shared::span::{GetSpan, Span};

use crate::literal::Literal;
use crate::StatementList;

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(untagged)]
pub enum Expression {
    Identifier(Identifier),
    Unary(Unary),
    Binary(Binary),
    Selection(Selection),
    FunctionDeclaration(FunctionDeclaration),
    FunctionCall(FunctionCall),
    Array(Array),
    Index(Index),
    Literal(Literal),
}

impl GetSpan for Expression {
    fn span(&self) -> &Span {
        match self {
            Expression::Identifier(identifier) => &identifier.span,
            Expression::Unary(unary) => &unary.span,
            Expression::Binary(binary) => &binary.span,
            Expression::Selection(selection) => &selection.span,
            Expression::FunctionDeclaration(function_declaration) => &function_declaration.span,
            Expression::FunctionCall(function_call) => &function_call.span,
            Expression::Array(array) => &array.span,
            Expression::Index(index) => &index.span,
            Expression::Literal(literal) => literal.span(),
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Unary {
    pub operator: TokenKind,
    pub operand: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Binary {
    pub operator: TokenKind,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Selection {
    pub condition: Box<Expression>,
    pub conditional: StatementList,
    pub else_conditional: Option<StatementList>,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct FunctionDeclaration {
    pub parameters: Vec<Identifier>,
    pub body: StatementList,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct FunctionCall {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Array {
    pub elements: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Index {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
}
