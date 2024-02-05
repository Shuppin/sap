use serde::Serialize;

/// Represents a span of characters in the source code.
///
/// A `Span` contains the starting and ending indices of a span of characters in the
/// source code. The `start` field represents the index of the first character in the
/// span, while the `end` field represents the index of the last character in the span.
#[derive(PartialEq, Debug, Clone, Serialize)]
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

/// A trait for enums whose variants all contain a `Span`.
pub trait GetSpan {
    fn span(&self) -> &Span;
}
