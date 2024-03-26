//! This module contains the AST nodes for the SAP language statements.
//!
//! A statement is a single line of code that performs an action, such as setting a
//! variable, returning a value, or defining a function. The AST nodes in this module
//! represent the different types of statements that can be found in the SAP language.
//!
//! Standalone expressions are also considered statements in the SAP language, and are
//! represented by the `Expression` enum in the `expression` module. This allows
//! expressions to be used as statements in the language, such as in the case of function
//! calls.
use serde::Serialize;
use shared::span::{GetSpan, Span};

use crate::expression::{Expression, Identifier};
use crate::StatementList;

/// Represents a single statement in the SAP language.
#[derive(Debug, Serialize, PartialEq, Clone)]
// Tell serde not to include this enum in the JSON output, since it only adds clutter and
// doesn't provide any useful information.
#[serde(untagged)]
pub enum Statement {
    Set(Set),
    Return(Return),
    Expression(Expression),
    FunctionDeclaration(FunctionDeclaration),
    RepeatNTimes(RepeatNTimes),
    RepeatUntil(RepeatUntil),
    RepeatForever(RepeatForever),
    Display(Display),
}

impl GetSpan for Statement {
    fn span(&self) -> &Span {
        match self {
            Statement::Set(set_stmt) => &set_stmt.span,
            Statement::Return(ret_stmt) => &ret_stmt.span,
            Statement::Expression(expr) => expr.span(),
            Statement::FunctionDeclaration(function_declaration) => {
                &function_declaration.span
            }
            Statement::RepeatNTimes(repeat_n_times) => &repeat_n_times.span,
            Statement::RepeatUntil(repeat_until) => &repeat_until.span,
            Statement::RepeatForever(repeat_forever) => &repeat_forever.span,
            Statement::Display(display) => &display.span,
        }
    }
}

/// Represents a set statement in the SAP language.
/// For example: `set x = 5`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Set {
    pub ident: Identifier,
    pub expr: Expression,
    pub span: Span,
}

/// Represents a return statement in the SAP language.
/// For example: `return 5`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Return {
    pub value: Expression,
    pub span: Span,
}

/// Represents a function declaration in the SAP language.
/// For example:
/// ```sap
/// defineFunction add(x, y)
///     return x + y
/// end
/// ```
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: StatementList,
    pub span: Span,
}

/// Represents a repeat n times statement in the SAP language.
/// For example:
/// ```sap
/// repeat 5 times
///     display "Hello, world!"
/// end
/// ```
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct RepeatNTimes {
    pub n: Expression,
    pub body: StatementList,
    pub span: Span,
}

/// Represents a repeat until statement in the SAP language.
/// For example:
/// ```sap
/// repeat until x > 5
///     set x = x + 1
/// end
/// ```
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct RepeatUntil {
    pub condition: Expression,
    pub body: StatementList,
    pub span: Span,
}

/// Represents a repeat forever statement in the SAP language.
/// For example:
/// ```sap
/// repeat forever
///     display "Hello, world!"
/// end
/// ```
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct RepeatForever {
    pub body: StatementList,
    pub span: Span,
}

/// Represents a display statement in the SAP language.
/// For example: `display "Hello, world!"`
#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Display {
    pub expressions: Vec<Expression>,
    pub span: Span,
}
