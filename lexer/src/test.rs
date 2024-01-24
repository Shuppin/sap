use crate::{token::Token, Lexer, Span, TokenKind};

/// Ignore these absoultely cursed macros, they're an internal shorthand
/// used to reduce the boilerplate needed for writing tests.
macro_rules! token {
    ($tok:ident, $start:expr, $end:expr) => {
        Token {
            span: Span {
                start: $start,
                end: $end,
            },
            kind: TokenKind::$tok,
        }
    };
}

macro_rules! ident {
    ($value:expr, $start:expr, $end:expr) => {
        Token {
            span: Span {
                start: $start,
                end: $end,
            },
            kind: TokenKind::Identifier { name: $value },
        }
    };
}

macro_rules! int {
    ($value:expr, $start:expr, $end:expr) => {
        Token {
            span: Span {
                start: $start,
                end: $end,
            },
            kind: TokenKind::Int($value),
        }
    };
}

macro_rules! float {
    ($value:expr, $start:expr, $end:expr) => {
        Token {
            span: Span {
                start: $start,
                end: $end,
            },
            kind: TokenKind::Float($value),
        }
    };
}

macro_rules! string {
    ($value:expr, $start:expr, $end:expr) => {
        Token {
            span: Span {
                start: $start,
                end: $end,
            },
            kind: TokenKind::String($value),
        }
    };
}

fn lexer_common(input: &str, expected_tokens: Vec<Token>) {
    let mut lexer = Lexer::new(input);
    let mut tokens_iter = expected_tokens.into_iter();
    loop {
        let token = lexer.next_token();
        assert_eq!(
            tokens_iter
                .next()
                .expect("lexer produced more tokens than expected"),
            token
        );
        if token.kind == TokenKind::Eof {
            break;
        }
    }
}

#[test]
fn lexer_simple() {
    lexer_common(
        "=+(){},:;",
        vec![
            token!(Assign, 0, 1),
            token!(Plus, 1, 2),
            token!(LParen, 2, 3),
            token!(RParen, 3, 4),
            token!(LCurly, 4, 5),
            token!(RCurly, 5, 6),
            token!(Comma, 6, 7),
            token!(Colon, 7, 8),
            token!(Semicolon, 8, 9),
            token!(Eof, 9, 10),
        ],
    );
}

#[test]
fn lexer_numbers() {
    lexer_common(
        "111 22 3.14 45",
        vec![
            int!(111, 0, 3),
            int!(22, 4, 6),
            float!(3.14, 7, 11),
            int!(45, 12, 14),
            token!(Eof, 14, 15),
        ],
    )
}

#[test]
fn lexer_let() {
    lexer_common(
        "let x=5;",
        vec![
            token!(Let, 0, 3),
            ident!("x".to_string(), 4, 5),
            token!(Assign, 5, 6),
            int!(5, 6, 7),
            token!(Semicolon, 7, 8),
            token!(Eof, 8, 9),
        ],
    );
}

#[test]
fn lexer_string() {
    lexer_common(
        r#" "hello, world!" "This is 'weird'" "#,
        vec![
            string!("hello, world!".to_string(), 1, 16),
            string!("This is 'weird'".to_string(), 17, 34),
            token!(Eof, 35, 36),
        ],
    )
}

#[test]
fn lexer_array() {
    lexer_common(
        "[3, 4, 5, 6]",
        vec![
            token!(LBracket, 0, 1),
            int!(3, 1, 2),
            token!(Comma, 2, 3),
            int!(4, 4, 5),
            token!(Comma, 5, 6),
            int!(5, 7, 8),
            token!(Comma, 8, 9),
            int!(6, 10, 11),
            token!(RBracket, 11, 12),
            token!(Eof, 12, 13),
        ],
    );
}

#[test]
fn lexer_bool() {
    lexer_common(
        "let y=true",
        vec![
            token!(Let, 0, 3),
            ident!("y".to_string(), 4, 5),
            token!(Assign, 5, 6),
            token!(True, 6, 10),
            token!(Eof, 10, 11),
        ],
    )
}

#[test]
fn lexer_function() {
    lexer_common(
        "let add = fn(a,b) { a+b };",
        vec![
            token!(Let, 0, 3),
            ident!("add".to_string(), 4, 7),
            token!(Assign, 8, 9),
            token!(Fn, 10, 12),
            token!(LParen, 12, 13),
            ident!("a".to_string(), 13, 14),
            token!(Comma, 14, 15),
            ident!("b".to_string(), 15, 16),
            token!(RParen, 16, 17),
            token!(LCurly, 18, 19),
            ident!("a".to_string(), 20, 21),
            token!(Plus, 21, 22),
            ident!("b".to_string(), 22, 23),
            token!(RCurly, 24, 25),
            token!(Semicolon, 25, 26),
            token!(Eof, 26, 27),
        ],
    )
}

#[test]
fn lexer_multiline() {
    let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);";

    lexer_common(
        input,
        vec![
            token!(Let, 0, 3),
            ident!("five".to_string(), 4, 8),
            token!(Assign, 9, 10),
            int!(5, 11, 12),
            token!(Semicolon, 12, 13),
            token!(Let, 14, 17),
            ident!("ten".to_string(), 18, 21),
            token!(Assign, 22, 23),
            int!(10, 24, 26),
            token!(Semicolon, 26, 27),
            token!(Let, 28, 31),
            ident!("add".to_string(), 32, 35),
            token!(Assign, 36, 37),
            token!(Fn, 38, 40),
            token!(LParen, 40, 41),
            ident!("x".to_string(), 41, 42),
            token!(Comma, 42, 43),
            ident!("y".to_string(), 44, 45),
            token!(RParen, 45, 46),
            token!(LCurly, 47, 48),
            ident!("x".to_string(), 51, 52),
            token!(Plus, 53, 54),
            ident!("y".to_string(), 55, 56),
            token!(Semicolon, 56, 57),
            token!(RCurly, 58, 59),
            token!(Semicolon, 59, 60),
            token!(Let, 62, 65),
            ident!("result".to_string(), 66, 72),
            token!(Assign, 73, 74),
            ident!("add".to_string(), 75, 78),
            token!(LParen, 78, 79),
            ident!("five".to_string(), 79, 83),
            token!(Comma, 83, 84),
            ident!("ten".to_string(), 85, 88),
            token!(RParen, 88, 89),
            token!(Semicolon, 89, 90),
            token!(Eof, 90, 91),
        ],
    );
}

#[test]
fn lexer_conditional() {
    let input = "let five = 5;
let ten = 10;

if (5 < 10) {
    return true;
} else {
    return false;
}";
    lexer_common(
        input,
        vec![
            token!(Let, 0, 3),
            ident!("five".to_string(), 4, 8),
            token!(Assign, 9, 10),
            int!(5, 11, 12),
            token!(Semicolon, 12, 13),
            token!(Let, 14, 17),
            ident!("ten".to_string(), 18, 21),
            token!(Assign, 22, 23),
            int!(10, 24, 26),
            token!(Semicolon, 26, 27),
            token!(If, 29, 31),
            token!(LParen, 32, 33),
            int!(5, 33, 34),
            token!(Lt, 35, 36),
            int!(10, 37, 39),
            token!(RParen, 39, 40),
            token!(LCurly, 41, 42),
            token!(Return, 47, 53),
            token!(True, 54, 58),
            token!(Semicolon, 58, 59),
            token!(RCurly, 60, 61),
            token!(Else, 62, 66),
            token!(LCurly, 67, 68),
            token!(Return, 73, 79),
            token!(False, 80, 85),
            token!(Semicolon, 85, 86),
            token!(RCurly, 87, 88),
            token!(Eof, 88, 89),
        ],
    );
}