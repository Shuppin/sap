use serde::Serialize;
use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token::{:?} ('{}', <{}:{}>)",
            self.kind, self.kind, self.span.start, self.span.end
        )
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum TokenKind {
    // Special
    Illegal,
    Eof,

    // Value holders
    Identifier { name: String },
    Int(i64),
    Float(f64),
    String(String),

    // Arithmetic operators
    Assign, // =
    Plus,   // +
    Minus,  // -
    Mult,   // *
    Div,    // /
    Not,    // !
    Mod,    // %

    // Comparison operators
    Lt,    // <
    LtEq,  // <=
    Gt,    // >
    GtEq,  // >=
    Eq,    // ==
    NotEq, // !=

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

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string_literal = match self {
            Self::Identifier { name } => name,
            Self::Int(num) => return write!(f, "{}", num),
            Self::Float(num) => return write!(f, "{}", num),
            Self::String(string) => string,
            Self::Assign => "=",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Mult => "*",
            Self::Div => "/",
            Self::Not => "!",
            Self::Mod => "%",
            Self::Lt => "<",
            Self::LtEq => "<=",
            Self::Gt => ">",
            Self::GtEq => ">=",
            Self::Eq => "==",
            Self::NotEq => "!=",
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
            Self::Illegal | Self::Eof => "",
        }
        .to_string();
        write!(f, "{}", string_literal)
    }
}
