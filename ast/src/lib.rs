pub mod expression;
pub mod literal;
pub mod statement;
pub mod tostring;

use serde::Serialize;
use shared::span::Span;
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

/// Represents a whole program source file in the SAP language. It is the root node for
/// all syntax trees generated by the parser.
#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}

impl Program {
    /// Creates a new instance of the Program struct.
    ///
    /// Note: `span.end` is set to 0
    pub fn new() -> Self {
        Self {
            statements: vec![],
            span: Span { start: 0, end: 0 },
        }
    }
}

/// Represents a group of statements encapsulated by a set of curly braces `{}`.
#[derive(Debug, Serialize, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

impl Block {
    /// Creates a new instance of the Block struct.
    ///
    /// Note: `span.end` is set to 0
    pub fn new() -> Self {
        Self {
            statements: vec![],
            span: Span { start: 0, end: 0 },
        }
    }
}
