use std::str::Chars;

use token::{Span, Token, TokenKind};

#[cfg(test)]
mod tests;

pub mod token;

pub struct Lexer<'lexer> {
    input: Chars<'lexer>,
    chr: char,
    position: usize,
}

impl<'lexer> Lexer<'lexer> {
    pub fn new(input: &'lexer str) -> Self {
        let mut lexer = Lexer {
            input: input.chars(),
            chr: char::from(0),
            position: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        match self.input.next() {
            Some(chr) => {
                self.chr = chr;
            }
            None => self.chr = '\0',
        }
        self.position += 1;
    }

    fn peek_char(&mut self) -> char {
        match self.input.clone().next() {
            Some(chr) => chr,
            None => '\0',
        }
    }
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comment();

        let start_position = self.position;

        let token_kind = match self.chr {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Mult,
            '/' => TokenKind::Div,
            '%' => TokenKind::Mod,

            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            ':' => TokenKind::Colon,

            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LCurly,
            '}' => TokenKind::RCurly,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,

            '\0' => TokenKind::Eof,
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::Eq
                } else {
                    TokenKind::Assign
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::NotEq
                } else {
                    TokenKind::Not
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            '"' => {
                let string = self.read_string();
                return Token {
                    span: Span {
                        start: start_position - 1,
                        end: self.position - 1,
                    },
                    kind: TokenKind::String(string),
                };
            }
            _ => {
                if is_letter(self.chr) {
                    let ident = self.read_identifier();
                    return Token {
                        span: Span {
                            start: start_position - 1,
                            end: self.position - 1,
                        },
                        kind: TokenKind::lookup_ident(&ident),
                    };
                } else if is_digit(self.chr) {
                    return self.read_number();
                } else {
                    TokenKind::Illegal
                }
            }
        };
        self.read_char();

        return Token {
            span: Span {
                start: start_position - 1,
                end: self.position - 1,
            },
            kind: token_kind,
        };
    }

    fn read_string(&mut self) -> String {
        let mut string = String::new();
        // Read opening '"'
        self.read_char();

        // Read string contents
        while !(self.chr == '"' || self.chr == '\0') {
            string.push(self.chr);
            self.read_char();
        }

        // Read closing '"'
        self.read_char();

        string
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while is_letter(self.chr) {
            ident.push(self.chr);
            self.read_char();
        }
        ident
    }

    fn _read_int(&mut self, number: &mut String) {
        while is_digit(self.chr) {
            number.push(self.chr);
            self.read_char();
        }
    }

    fn read_number(&mut self) -> Token {
        let mut number = String::new();
        self._read_int(&mut number);
        if self.chr == '.' {
            number.push(self.chr);
            self.read_char();
            self._read_int(&mut number);
            return Token {
                span: Span {
                    start: self.position - number.len() - 1,
                    end: self.position - 1,
                },
                kind: TokenKind::Float(number.parse().unwrap()),
            };
        }
        return Token {
            span: Span {
                start: self.position - number.len() - 1,
                end: self.position - 1,
            },
            kind: TokenKind::Int(number.parse().unwrap()),
        };
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.chr, ' ' | '\t' | '\n' | '\r') {
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        if self.chr == '/' && self.peek_char() == '/' {
            self.read_char();
            self.read_char();
            loop {
                self.read_char();
                if self.chr == '\n' {
                    self.read_char();
                    break;
                }
                if self.chr == '\0' {
                    break;
                }
            }
        }
    }
}

fn is_letter(chr: char) -> bool {
    chr.is_ascii_alphabetic() || chr == '_'
}

fn is_digit(chr: char) -> bool {
    chr.is_ascii_digit()
}
