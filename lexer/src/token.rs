use std::fmt::Display;

use serde::Serialize;

/// Represents a token in the lexer.
///
/// Tokens are the smallest units of a programming language. They represent
/// individual elements such as keywords, identifiers, literals, and symbols.
/// Tokens are used by the lexer to break down the source code into meaningful
/// components that can be processed by the parser.
///
/// Tokens are useful because they provide a structured representation of the
/// source code, allowing for easier analysis, interpretation, and transformation
/// of the code. They serve as the foundation for building compilers, interpreters,
/// and other language processing tools.
#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Implements the `Display` trait for the `Token` struct.
/// This allows the `Token` struct to be formatted as a string.
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token::{:?} ('{}', <{}:{}>)",
            self.kind, self.kind, self.span.start, self.span.end
        )
    }
}

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

/// Represents a specific variant of a token.
///
/// A `TokenKind` can be one of several variants, such as `Identifier`, `IntegerLiteral`,
/// etc. Each variant contains additional data specific to that kind of token.
#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum TokenKind {
    // Special
    Illegal(String),
    Eof,

    // Value holders
    Identifier { name: String },
    Int(String),
    Float(String),
    String(String),

    // Arithmetic operators
    Assign, // =
    Plus,   // +
    Minus,  // -
    Mult,   // *
    Div,    // /
    Mod,    // %

    // Comparison operators
    Lt,    // <
    LtEq,  // <=
    Gt,    // >
    GtEq,  // >=
    Eq,    // ==
    NotEq, // !=

    // Boolean operators
    Not, // !
    And, // &&
    Or,  // ||

    // Delimiters
    Comma,     // ,
    Semicolon, // ;
    Colon,     // :

    // Brackets
    LParen,   // (
    RParen,   // )
    LCurly,   // {
    RCurly,   // }
    LBracket, // [
    RBracket, // ]

    // Keywords
    Fn,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl TokenKind {
    /// Looks up an identifier and returns the corresponding token kind.
    ///
    /// This function serves as a mapping, providing a single point of truth defining all
    /// the keywords in the language. The lexer module uses this function to generate
    /// tokens for keywords.
    ///
    /// # Arguments
    ///
    /// * `ident` - The identifier to look up.
    ///
    /// # Returns
    ///
    /// The corresponding token kind for the identifier.
    pub fn lookup_ident(ident: &str) -> TokenKind {
        match ident {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "return" => TokenKind::Return,
            _ => TokenKind::Identifier {
                name: ident.to_string(),
            },
        }
    }
}

/// Implements the `Display` trait for `TokenKind`.
/// This allows `TokenKind` to be formatted as a string when using the `write!` macro.
impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Match the `TokenKind` variant and assign the corresponding string literal.
        let string_literal = match self {
            Self::Identifier { name } => name,
            Self::Int(num) => return write!(f, "{}", num),
            Self::Float(num) => return write!(f, "{}", num),
            Self::String(string) => string,
            Self::Illegal(string) => string,
            Self::Assign => "=",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Mult => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Lt => "<",
            Self::LtEq => "<=",
            Self::Gt => ">",
            Self::GtEq => ">=",
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::Not => "!",
            Self::And => "&&",
            Self::Or => "||",
            Self::Comma => ",",
            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LCurly => "{",
            Self::RCurly => "}",
            Self::LBracket => "[",
            Self::RBracket => "]",
            Self::Fn => "fn",
            Self::Let => "let",
            Self::If => "if",
            Self::Else => "else",
            Self::True => "true",
            Self::False => "false",
            Self::Return => "return",
            Self::Eof => "<EOF>",
        }
        .to_string();

        // Write the string literal to the formatter.
        write!(f, "{}", string_literal)
    }
}
