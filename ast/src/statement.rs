use serde::Serialize;
use shared::span::{GetSpan, Span};

use crate::expression::{Expression, Identifier};
use crate::StatementList;

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(untagged)]
pub enum Statement {
    Set(Set),
    Return(Return),
    Expression(Expression),
    FunctionDeclaration(FunctionDeclaration),
    RepeatNTimes(RepeatNTimes),
    RepeatUntil(RepeatUntil),
    RepeatForever(RepeatForever),
}

impl GetSpan for Statement {
    fn span(&self) -> &Span {
        match self {
            Statement::Set(set_stmt) => &set_stmt.span,
            Statement::Return(ret_stmt) => &ret_stmt.span,
            Statement::Expression(expr) => expr.span(),
            Statement::FunctionDeclaration(function_declaration) => &function_declaration.span,
            Statement::RepeatNTimes(repeat_n_times) => &repeat_n_times.span,
            Statement::RepeatUntil(repeat_until) => &repeat_until.span,
            Statement::RepeatForever(repeat_forever) => &repeat_forever.span,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Set {
    pub ident: Identifier,
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct Return {
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: StatementList,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct RepeatNTimes {
    pub n: Expression,
    pub body: StatementList,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct RepeatUntil {
    pub condition: Expression,
    pub body: StatementList,
    pub span: Span,
}

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub struct RepeatForever {
    pub body: StatementList,
    pub span: Span,
}
