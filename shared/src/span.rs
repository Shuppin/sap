use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};

/// Constant to control serialization
const EXPANDED_SPAN_SERIALISATION: bool = false; // Change to false to skip serialization

/// Represents a span of characters in the source code.
///
/// A `Span` contains the starting and ending indices of a span of characters in the
/// source code. The `start` field represents the index of the first character in the
/// span, while the `end` field represents the index of the last character in the span.
#[derive(PartialEq, Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// Creates a new `Span` with the specified start and end positions.
    ///
    /// # Arguments
    ///
    /// * `start` - The starting position of the span.
    /// * `end` - The ending position of the span.
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}

impl Serialize for Span {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if EXPANDED_SPAN_SERIALISATION {
            let mut state = serializer.serialize_struct("Span", 2)?;
            state.serialize_field("start", &self.start)?;
            state.serialize_field("end", &self.end)?;
            state.end()
        } else {
            serializer.serialize_str(&format!("{}..{}", self.start, self.end))
        }
    }
}

/// A trait for enums whose variants all contain a `Span`.
pub trait GetSpan {
    fn span(&self) -> &Span;
}
