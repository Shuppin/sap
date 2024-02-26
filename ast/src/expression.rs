//! This module contains the AST representation for expressions in the SAP language.
//!
//! An expression is a combination of one or more values, variables, operators, and
//! functions that evaluates to a single value. The AST nodes in this module represent the
//! different types of expressions that are supported in the SAP language.
//!
//! Expressions are used in the language to perform calculations, make decisions, and
//! manipulate data. They can be used as standalone statements, or as part of larger
//! expressions or statements.
//!
//! Note: The `Box` type is used to indicate that the data enclosed is stored on the heap,
//! rather than on the stack. This is necessary because otherwise the compiler needs to
//! know the size of the expression at compile time, which is not possible for recursive
//! data structures like the AST.
use lexer::token::TokenKind;
use serde::Serialize;
use shared::span::{GetSpan, Span};

use crate::literal::Literal;
use crate::StatementList;

/// Represents a single expression in the SAP language.
#[derive(Debug, Serialize, PartialEq, Clone)]
// Tell serde not to include this enum in the JSON output, since it only adds clutter and doesn't
// provide any useful information.
#[serde(untagged)]
pub enum Expression {
    Identifier(Identifier),
    Unary(Unary),
    Binary(Binary),
    Selection(Selection),
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
            Expression::FunctionCall(function_call) => &function_call.span,
            Expression::Array(array) => &array.span,
            Expression::Index(index) => &index.span,
            Expression::Literal(literal) => literal.span(),
        }
    }
}

/// Represents an identifier in the SAP language.
/// For example: `x`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

/// Represents a unary expression in the SAP language.
/// For example: `-x`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Unary {
    pub operator: TokenKind,
    pub operand: Box<Expression>,
    pub span: Span,
}

/// Represents a binary expression in the SAP language.
/// For example: `x + 5`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Binary {
    pub operator: TokenKind,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

/// Represents a selection expression in the SAP language.
/// For example:
/// ```sap
/// if x > 5 then
///     display "x is greater than 5"
/// otherwise
///     display "x is less than or equal to 5"
/// end
/// ```
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Selection {
    pub condition: Box<Expression>,
    pub conditional: StatementList,
    pub else_conditional: Option<StatementList>,
    pub span: Span,
}

/// Represents a function call expression in the SAP language.
/// For example: `add(5, 10)`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct FunctionCall {
    /// The function being called. This can be an identifier or an expression which
    /// evaluates to a function.
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

/// Represents an array expression in the SAP language.
/// For example: `[1, 2, 3, 4, 5]`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Array {
    pub elements: Vec<Expression>,
    pub span: Span,
}

/// Represents an index expression in the SAP language.
/// For example: `arr[0]`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Index {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
}
