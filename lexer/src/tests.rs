use crate::{token::Token, Lexer, Span, TokenKind};

/// Ignore this absoultely cursed macro, it's an internal shorthand
/// used to reduce the boilerplate needed for writing tests.
macro_rules! e {
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

fn test_lexer_common(input: &str, expected_tokens: Vec<Token>) {
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
fn test_lexer_simple() {
    test_lexer_common(
        "=+(){},:;",
        vec![
            e!(Assign, 0, 1),
            e!(Plus, 1, 2),
            e!(LParen, 2, 3),
            e!(RParen, 3, 4),
            e!(LCurly, 4, 5),
            e!(RCurly, 5, 6),
            e!(Comma, 6, 7),
            e!(Colon, 7, 8),
            e!(Semicolon, 8, 9),
            e!(Eof, 9, 10),
        ],
    );
}
