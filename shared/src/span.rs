//! Contains the `Span` struct, which represents a span of characters in the source code.
use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};

/// Constant to control whether to use shorthand serialisation for spans.
///
/// If `true`, spans will be serialised into: `"span": "157..160"`, instead of
/// `"span": {"start": 157, "end": 160}`.
const SHORTHAND_SPAN_SERIALISATION: bool = true;

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

/// Implements the `Serialize` trait for the `Span` struct.
///
/// This allows the `Span` struct to be serialized into a JSON object.
impl Serialize for Span {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if SHORTHAND_SPAN_SERIALISATION {
            serializer.serialize_str(&format!("{}..{}", self.start, self.end))
        } else {
            let mut state = serializer.serialize_struct("Span", 2)?;
            state.serialize_field("start", &self.start)?;
            state.serialize_field("end", &self.end)?;
            state.end()
        }
    }
}

/// Trait for getting the span of a node. For enums, this is useful for getting the span
/// of the current variant, without having to match on each variant.
pub trait GetSpan {
    fn span(&self) -> &Span;
}
