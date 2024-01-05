pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub enum TokenKind {
    // Special
    Illegal,
    Eof,

    // Value holders
    Identifier { name: String },
    Int(i64),
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
    Less,   // <
    LessEq, // <=
    More,   // >
    MoreEq, // >=
    Eq,     // ==
    NotEq,  // !=

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
            _ => TokenKind::Identifier { name: ident.to_string() },
        }
    }
}
