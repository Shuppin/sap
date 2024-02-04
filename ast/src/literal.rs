use lexer::token::Span;
use serde::Serialize;

use crate::GetSpan;

#[derive(Debug, Serialize, PartialEq, Clone)]
#[serde(tag = "type")]
pub enum Literal {
    Integer { value: i64, span: Span },
    Float { value: f64, span: Span },
    String { value: String, span: Span },
    Boolean { value: bool, span: Span },
}

impl GetSpan for Literal {
    fn span(&self) -> &Span {
        match self {
            Self::Integer { span, .. } => span,
            Self::Float { span, .. } => span,
            Self::String { span, .. } => span,
            Self::Boolean { span, .. } => span,
        }
    }
}
