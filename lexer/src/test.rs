use crate::token::Token;
use crate::{Lexer, Span, TokenKind};

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
            kind: TokenKind::Int($value.to_string()),
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
            kind: TokenKind::Float($value.to_string()),
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
            token!(Eof, 9, 9),
        ],
    );
}

#[test]
fn lexer_operators() {
    lexer_common(
        "= + - * / % < <= > >= == != not and or",
        vec![
            token!(Assign, 0, 1),
            token!(Plus, 2, 3),
            token!(Minus, 4, 5),
            token!(Mult, 6, 7),
            token!(Div, 8, 9),
            token!(Mod, 10, 11),
            token!(Lt, 12, 13),
            token!(LtEq, 14, 16),
            token!(Gt, 17, 18),
            token!(GtEq, 19, 21),
            token!(Eq, 22, 24),
            token!(NotEq, 25, 27),
            token!(Not, 28, 31),
            token!(And, 32, 35),
            token!(Or, 36, 38),
            token!(Eof, 38, 38),
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
            token!(Eof, 14, 14),
        ],
    )
}

#[test]
fn lexer_set() {
    lexer_common(
        "set x=5;",
        vec![
            token!(Set, 0, 3),
            ident!("x".to_string(), 4, 5),
            token!(Assign, 5, 6),
            int!(5, 6, 7),
            token!(Semicolon, 7, 8),
            token!(Eof, 8, 8),
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
            token!(Eof, 35, 35),
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
            token!(Eof, 12, 12),
        ],
    );
}

#[test]
fn lexer_bool() {
    lexer_common(
        "set y=true",
        vec![
            token!(Set, 0, 3),
            ident!("y".to_string(), 4, 5),
            token!(Assign, 5, 6),
            token!(True, 6, 10),
            token!(Eof, 10, 10),
        ],
    )
}

#[test]
fn lexer_function() {
    lexer_common(
        "defineFunction add(a,b) a+b end",
        vec![
            token!(DefineFunction, 0, 14),
            ident!("add".to_string(), 15, 18),
            token!(LParen, 18, 19),
            ident!("a".to_string(), 19, 20),
            token!(Comma, 20, 21),
            ident!("b".to_string(), 21, 22),
            token!(RParen, 22, 23),
            ident!("a".to_string(), 24, 25),
            token!(Plus, 25, 26),
            ident!("b".to_string(), 26, 27),
            token!(End, 28, 31),
            token!(Eof, 31, 31),
        ],
    )
}

#[test]
fn lexer_multiline() {
    let input = "set five = 5;
set ten = 10;
defineFunction add(x, y)
  x + y;
end

set result = add(five, ten);";

    lexer_common(
        input,
        vec![
            token!(Set, 0, 3),
            ident!("five".to_string(), 4, 8),
            token!(Assign, 9, 10),
            int!(5, 11, 12),
            token!(Semicolon, 12, 13),
            token!(NewLine, 13, 14),
            token!(Set, 14, 17),
            ident!("ten".to_string(), 18, 21),
            token!(Assign, 22, 23),
            int!(10, 24, 26),
            token!(Semicolon, 26, 27),
            token!(NewLine, 27, 28),
            token!(DefineFunction, 28, 42),
            ident!("add".to_string(), 43, 46),
            token!(LParen, 46, 47),
            ident!("x".to_string(), 47, 48),
            token!(Comma, 48, 49),
            ident!("y".to_string(), 50, 51),
            token!(RParen, 51, 52),
            token!(NewLine, 54, 55),
            ident!("x".to_string(), 55, 56),
            token!(Plus, 57, 58),
            ident!("y".to_string(), 59, 60),
            token!(Semicolon, 60, 61),
            token!(NewLine, 61, 62),
            token!(End, 62, 65),
            token!(NewLine, 66, 67),
            token!(Set, 67, 70),
            ident!("result".to_string(), 71, 77),
            token!(Assign, 78, 79),
            ident!("add".to_string(), 80, 83),
            token!(LParen, 83, 84),
            ident!("five".to_string(), 84, 88),
            token!(Comma, 88, 89),
            ident!("ten".to_string(), 90, 93),
            token!(RParen, 93, 94),
            token!(Semicolon, 94, 95),
            token!(Eof, 95, 95),
        ],
    );
}

#[test]
fn lexer_conditional() {
    let input = "set five = 5;
set ten = 10;

if (5 < 10) then
    return true;
otherwise
    return false;
end";
    lexer_common(
        input,
        vec![
            token!(Set, 0, 3),
            ident!("five".to_string(), 4, 8),
            token!(Assign, 9, 10),
            int!(5, 11, 12),
            token!(Semicolon, 12, 13),
            token!(NewLine, 13, 14),
            token!(Set, 14, 17),
            ident!("ten".to_string(), 18, 21),
            token!(Assign, 22, 23),
            int!(10, 24, 26),
            token!(Semicolon, 26, 27),
            token!(NewLine, 28, 29),
            token!(If, 29, 31),
            token!(LParen, 32, 33),
            int!(5, 33, 34),
            token!(Lt, 35, 36),
            int!(10, 37, 39),
            token!(RParen, 39, 40),
            token!(Then, 41, 45),
            token!(NewLine, 49, 50),
            token!(Return, 50, 56),
            token!(True, 57, 61),
            token!(Semicolon, 61, 62),
            token!(NewLine, 62, 63),
            token!(Otherwise, 63, 72),
            token!(NewLine, 76, 77),
            token!(Return, 77, 83),
            token!(False, 84, 89),
            token!(Semicolon, 89, 90),
            token!(NewLine, 90, 91),
            token!(End, 91, 94),
            token!(Eof, 94, 94),
        ],
    );
}

#[test]
fn lexer_keywords() {
    let input =
        "defineFunction set if otherwise true false return then end repeat times until forever display";
    lexer_common(
        input,
        vec![
            token!(DefineFunction, 0, 14),
            token!(Set, 15, 18),
            token!(If, 19, 21),
            token!(Otherwise, 22, 31),
            token!(True, 32, 36),
            token!(False, 37, 42),
            token!(Return, 43, 49),
            token!(Then, 50, 54),
            token!(End, 55, 58),
            token!(Repeat, 59, 65),
            token!(Times, 66, 71),
            token!(Until, 72, 77),
            token!(Forever, 78, 85),
            token!(Display, 86, 93),
            token!(Eof, 93, 93),
        ],
    )
}
