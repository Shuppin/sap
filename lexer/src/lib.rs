//! The `lexer` module provides a lexer for tokenising input strings.
//!
//! The `Lexer` provides methods for tokenising input strings into individual tokens.
//! It supports various token types such as identifiers, numbers, strings, and operators.
//! The `Lexer` uses a cursor-based approach to iterate over the input string and extract
//! tokens.
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
        let start_position = self.position;

        // Skip over any whitespace, comments, and newlines.
        match self.skip_garbage() {
            Ok(encountered_newline) => {
                // If we encountered a newline character (`\n`), we return a NewLine token.
                if encountered_newline {
                    return Token {
                        span: Span::new(self.position - 1, self.position),
                        kind: TokenKind::NewLine,
                    };
                }
            }
            // The only type of error that can be returned is an unterminated multi-line
            // comment, so we can safely unwrap the error and return the corresponding
            // token.
            Err(_) => {
                return Token {
                    span: Span::new(start_position, self.position),
                    kind: TokenKind::UnterminatedComment,
                };
            }
        }

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
            '\n' => TokenKind::NewLine,

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
                    TokenKind::Illegal(self.chr.to_string())
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

            // String literals
            '"' => {
                match self.read_string() {
                    Ok(string) => {
                        return Token {
                            span: Span {
                                start: start_position,
                                end: self.position,
                            },
                            kind: TokenKind::String(string),
                        }
                    }
                    Err(_) => {
                        return Token {
                            span: Span {
                                start: start_position,
                                end: self.position,
                            },
                            kind: TokenKind::UnterminatedString,
                        }
                    }
                };
            }

            // Else, we a dealing with a keyword, identifier, number of an illegal character.
            _ => {
                if is_valid_ident_start(self.chr) {
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
    fn read_string(&mut self) -> Result<String, ()> {
        let mut string = String::new();
        // Read opening '"'
        self.read_char();

        // Read string contents
        while !(self.chr == '"') {
            if self.chr == '\0' {
                return Err(());
            }
            string.push(self.chr);
            self.read_char();
        }

        // Read closing '"'
        self.read_char();

        Ok(string)
    }

    /// Reads an identifier starting from the current character position.
    ///
    /// # Returns
    ///
    /// A `String` representing the identifier extracted from the input.
    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();

        while is_valid_ident_continue(self.chr) {
            ident.push(self.chr);
            self.read_char();
        }
        ident
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

        // If we encounter a decimal point, we continue to read the fractional part.
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
        } else {
            return Token {
                span: Span {
                    start: self.position - number.len(),
                    end: self.position,
                },
                kind: TokenKind::Int(number),
            };
        }
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

    /// Skips over a single-line comment (`//`) in the current input.
    ///
    /// It reads characters until it reaches the end of the line or the end of the input.
    ///
    /// Assumes that the current character (`self.chr`) is the first slash.
    fn skip_comment(&mut self) {
        if self.chr == '/' && self.peek_char() == '/' {
            // Read the '//'
            self.read_char();
            self.read_char();

            // Read the comment till the end of the line, or the end of the input.
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

    /// Skips over a multi-line comment (`/* ... */`) in the current input.
    ///
    /// Assumes that the current character (`self.chr`) is the first slash.
    ///
    /// # Returns
    ///
    /// An `Ok(())` if the multi-line comment was successfully skipped, or an
    /// `Err(())` error if the comment was not terminated.
    fn skip_multi_comment(&mut self) -> Result<(), ()> {
        // Consume the opening '/*'
        if self.chr == '/' && self.peek_char() == '*' {
            self.read_char();
            self.read_char();
        } else {
            return Ok(());
        }
        // Consume the comment
        while !(self.chr == '*' && self.peek_char() == '/') {
            self.read_char();
            if self.chr == '\0' {
                return Err(());
            };
        }
        // Consume the closing '*/'
        self.read_char();
        self.read_char();

        Ok(())
    }

    /// Skips over any whitespace characters, comments, and newlines in the current input.
    ///
    /// # Returns
    ///
    /// A `Result` containing a `bool` indicating whether a newline character was
    /// encountered. If an error occurs, it returns an `Err(())`.
    fn skip_garbage(&mut self) -> Result<bool, ()> {
        // We store whether we encountered a newline because the lexer does
        // count newlines, however it only needs to know if it encountered one,
        // not how many it encountered.
        //
        // Note: The parser does depend on this functionality, so don't remove it. :)
        let mut encountered_newline = false;
        while matches!(self.chr, ' ' | '\t' | '\n' | '\r' | '/') {
            match self.chr {
                // Skip whitespace
                ' ' | '\t' => self.skip_whitespace(),
                // Skip newlines
                '\n' | '\r' => {
                    encountered_newline = true;
                    self.read_char();
                }
                // Skip comments
                '/' => match self.peek_char() {
                    '/' => self.skip_comment(),
                    '*' => self.skip_multi_comment()?,
                    _ => break,
                },
                // The while statement above ensures that there can be no other pattern, but we need
                // to handle it in this match statement to satisfy the compiler.
                _ => unreachable!(),
            }
        }
        return Ok(encountered_newline);
    }

    /// Skips over any whitespace characters in the current input.
    fn skip_whitespace(&mut self) {
        while matches!(self.chr, ' ' | '\t') {
            self.read_char();
        }
    }
}

/// Serves as a source of truth for the definition of what an identifier can start with.
fn is_valid_ident_start(chr: char) -> bool {
    chr.is_ascii_alphabetic() || chr == '_'
}

/// Serves as a source of truth for the definition of what an identifier can continue
/// with.
fn is_valid_ident_continue(chr: char) -> bool {
    chr.is_ascii_alphanumeric() || chr == '_' || is_digit(chr)
}

/// Serves as a source of truth for the definition of a 'digit'.
fn is_digit(chr: char) -> bool {
    chr.is_ascii_digit()
}
