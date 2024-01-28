pub mod expression;
pub mod literal;
pub mod statement;
pub mod string;

use presap_lexer::token::Span;
use serde::Serialize;
use statement::Statement;

/// Converts an AST node to a JSON string representation.
///
/// # Arguments
///
/// * `node` - The AST node to convert.
///
/// # Returns
///
/// Returns a Result containing the JSON string representation of the AST node, or an
/// error if the conversion fails.
pub fn ast_to_json<T>(node: &T) -> Result<String, serde_json::error::Error>
where
    T: ?Sized + Serialize,
{
    serde_json::to_string_pretty(&node)
}

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
