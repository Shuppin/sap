use std::str::Chars;

use shared::span::Span;
use token::{Token, TokenKind};

// Attempt to obtain the current version of the CLI package
pub const VERSION: Option<&str> = std::option_env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod test;

pub mod token;

/// Lexer for tokenising input strings.
///
/// The `Lexer` provides methods for tokenising input strings into individual tokens.
/// It supports various token types such as identifiers, numbers, strings, and operators.
/// The `Lexer` uses a cursor-based approach to iterate over the input string and extract
/// tokens.
pub struct Lexer<'lexer> {
    /// Represents the input for the lexer.
    ///
    /// The `input` field is of type `Chars<'lexer>`, which is an iterator over the
    /// characters of a string. Using `Chars` instead of just a raw string allows for
    /// iteration over the string one character at a time. Notably, it supports
    /// unicode characters and characters of unusual length.
    input: Chars<'lexer>,
    chr: char,
    position: usize,
}

impl<'lexer> Lexer<'lexer> {
    /// Creates a new Lexer instance.
    ///
    /// # Arguments
    ///
    /// * `input` - The input string to be tokenised.
    pub fn new(input: &'lexer str) -> Self {
        let mut lexer = Lexer {
            input: input.chars(),
            chr: char::from(0),
            position: 0,
        };
        lexer.read_char();
        // We set position to 0 here because `read_char()` increments position to 1,
        // but we want to start the index at 0 for consistency.
        lexer.position = 0;
        lexer
    }

    /// Reads the next character from the input stream and updates the lexer's internal
    /// state.
    fn read_char(&mut self) {
        match self.input.next() {
            Some(chr) => {
                self.chr = chr;
                self.position += 1
            }
            None => {
                // '\0' indicates the end of the file.
                // If we are already at the end of the file, there is no need to update the
                // character or increment the position.
                if self.chr != '\0' {
                    self.chr = '\0';
                    self.position += 1
                }
            }
        }
    }

    /// Returns the next character in the input stream without consuming it.
    ///
    /// # Returns
    ///
    /// The next character in the input stream, or `'\0'` if the end of the stream has
    /// been reached.
    fn peek_char(&mut self) -> char {
        // Clones the iterator to peek ahead without advancing it.
        match self.input.clone().next() {
            Some(chr) => chr,
            None => '\0',
        }
    }

    /// Advances the lexer to the next token in the input stream and returns the token.
    ///
    /// This function is essentially a large switch statement, containing branches
    /// corresponding to every token type. This function skips any whitespace and
    /// comments before identifying the next token. The token is represented by a
    /// `Token` struct, which contains information about its kind (e.g., identifier,
    /// operator, literal) and its span in the input stream.
    ///
    /// # Returns
    ///
    /// The next token in the sequence, and will continue to return an Eof token once the
    /// end is reached.
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comment();

        let start_position = self.position;

        // Determine what type of token we are dealing with.
        let token_kind = match self.chr {
            // Single character symbols
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

            // Potentially double character symbols
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
            '&' => {
                if self.peek_char() == '&' {
                    self.read_char();
                    TokenKind::And
                } else {
                    TokenKind::Illegal(self.chr.to_string())
                }
            }
            '|' => {
                if self.peek_char() == '|' {
                    self.read_char();
                    TokenKind::Or
                } else {
                    TokenKind::Illegal(self.chr.to_string())
                }
            }

            // String literals
            '"' => {
                let string = self.read_string();
                return Token {
                    span: Span {
                        start: start_position,
                        end: self.position,
                    },
                    kind: TokenKind::String(string),
                };
            }

            // Else, we a dealing with a keyword, identifier, number of an illegal character.
            _ => {
                if is_letter(self.chr) {
                    let ident = self.read_identifier();
                    return Token {
                        span: Span {
                            start: start_position,
                            end: self.position,
                        },
                        kind: TokenKind::lookup_ident(&ident),
                    };
                } else if is_digit(self.chr) {
                    return self.read_number();
                } else {
                    TokenKind::Illegal(self.chr.to_string())
                }
            }
        };

        // Since every branch advances the character by at least one, we move the function out
        // here to simplify the syntax.
        self.read_char();

        return Token {
            span: Span {
                start: start_position,
                end: self.position,
            },
            kind: token_kind,
        };
    }

    /// Reads a string literal (including quotes) from the current position in the input.
    ///
    /// # Returns
    ///
    /// A `String` containing the contents of the string literal.
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

    /// Reads an identifier starting from the current character position.
    ///
    /// # Returns
    ///
    /// A `String` representing the identifier extracted from the input.
    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while is_letter(self.chr) {
            ident.push(self.chr);
            self.read_char();
        }
        ident
    }

    /// Reads and appends digits to a given string from the current position in the input.
    ///
    /// # Arguments
    ///
    /// * `number` - A mutable reference to a `String` where the digits are appended.
    fn _read_int(&mut self, number: &mut String) {
        while is_digit(self.chr) {
            number.push(self.chr);
            self.read_char();
        }
    }

    /// Reads a number from the current position in the input and constructs a `Token`.
    ///
    /// This function reads a sequence of digits as an integer. If a decimal point is
    /// encountered, it continues to read the fractional part, constructing a
    /// floating-point number.
    ///
    /// # Returns
    ///
    /// A `Token` representing either an integer or a floating-point number, depending on
    /// the input.
    fn read_number(&mut self) -> Token {
        let mut number = String::new();
        self._read_int(&mut number);
        if self.chr == '.' {
            number.push(self.chr);
            self.read_char();
            self._read_int(&mut number);
            return Token {
                span: Span {
                    start: self.position - number.len(),
                    end: self.position,
                },
                kind: TokenKind::Float(number),
            };
        }
        return Token {
            span: Span {
                start: self.position - number.len(),
                end: self.position,
            },
            kind: TokenKind::Int(number),
        };
    }

    /// Skips over any whitespace characters in the current input.
    fn skip_whitespace(&mut self) {
        while matches!(self.chr, ' ' | '\t' | '\n' | '\r') {
            self.read_char();
        }
    }

    /// Skips over a single-line comment (`//`) in the current input.
    ///
    /// It reads characters until it reaches the end of the line or the end of the input.
    ///
    /// Assumes that the current character (`self.chr`) is the first slash.
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

/// Serves as a source of truth for the definition of a 'letter'.
fn is_letter(chr: char) -> bool {
    chr.is_ascii_alphabetic() || chr == '_'
}

/// Serves as a source of truth for the definition of a 'digit'.
fn is_digit(chr: char) -> bool {
    chr.is_ascii_digit()
}
