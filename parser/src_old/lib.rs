use precedence::{get_token_precendence, Precedence};
use presap_ast::{
    expression::{Expression, Identifier, Unary, Binary},
    statement::{Let, Return, Statement},
    Literal, Program,
};
use presap_lexer::{
    token::{Span, Token, TokenKind},
    Lexer,
};

mod precedence;

#[cfg(test)]
mod test;

#[cfg(test)]
mod test_to_string;

/// Temporary type which is used to represent an error produced by the parser.
pub type ParseError = String;

/// The Parser struct represents a parser for the input source code.
///
/// It is responsible for tokenising and parsing the source code to produce an Abstract Syntax Tree (AST).
pub struct Parser<'lexer> {
    lexer: Lexer<'lexer>,
    cur_token: Token,
    peek_token: Token,
}

impl<'lexer> Parser<'lexer> {
    /// Creates a new Parser instance with the given lexer.
    ///
    /// # Arguments
    ///
    /// * `lexer` - The lexer to be used for tokenising the input.
    ///
    /// # Returns
    ///
    /// A new Parser instance.
    pub fn new(mut lexer: Lexer<'lexer>) -> Self {
        // Initialise the current and peek tokens by calling `next_token` twice.
        let cur = lexer.next_token();
        let peek = lexer.next_token();

        Parser {
            lexer: lexer,
            cur_token: cur,
            peek_token: peek,
        }
    }

    /// Advances to the next token by replacing the peek token with the next token from the lexer,
    /// and updates the current token with the peek token.
    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    /// Checks if the current token matches the given token kind.
    ///
    /// # Arguments
    ///
    /// * `token` - The token kind to compare against.
    ///
    /// # Returns
    ///
    /// `true` if the current token matches the given token kind, otherwise `false`.
    #[inline]
    fn cur_token_is(&mut self, token: &TokenKind) -> bool {
        self.cur_token.kind == *token
    }

    /// Checks if the peek token matches the given token kind.
    ///
    /// # Arguments
    ///
    /// * `token` - The token kind to compare against.
    ///
    /// # Returns
    ///
    /// `true` if the peek token matches the given token kind, otherwise `false`.
    #[inline]
    fn peek_token_is(&mut self, token: &TokenKind) -> bool {
        self.peek_token.kind == *token
    }

    /// Throws an error if the peek token does not match the expected token,
    /// otherwise moves to the next token.
    ///
    /// # Arguments
    ///
    /// * `expected_token` - The expected token kind.
    ///
    /// # Returns
    ///
    /// `Ok(())` if the peek token matches the expected token, otherwise `Err` with an error message.
    fn expect_token(&mut self, expected_token: TokenKind) -> Result<(), String> {
        if self.peek_token_is(&expected_token) {
            self.next_token();
            Ok(())
        } else {
            Err(format!(
                "expected {:?}, got {:?}",
                expected_token, self.peek_token
            ))
        }
    }

    /// Parses the entire program and returns the resulting AST.
    ///
    /// # Returns
    ///
    /// - `Ok(program)` if the parsing is successful, where `program` is the parsed program.
    /// - `Err(errors)` if there are parsing errors, where `errors` is a vector of parse errors.
    pub fn parse_program(&mut self) -> Result<Program, Vec<ParseError>> {
        let mut program = Program::new();
        let mut errors: Vec<ParseError> = Vec::new();

        // Parse statements until the end of the input.
        while !self.cur_token_is(&TokenKind::Eof) {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(e) => errors.push(e),
            };
            self.next_token()
        }

        // Set the end span of the program to the end span of the current token.
        program.span.end = self.cur_token.span.end;

        if errors.is_empty() {
            Ok(program)
        } else {
            Err(errors)
        }
    }

    /// Parses a statement and returns the resulting AST.
    ///
    /// # Returns
    ///
    /// - `Ok(statement)` if the parsing is successful, where `statement` is the parsed statement.
    /// - `Err(error)` if there is a parsing error, where `error` is the parse error.
    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
            // _ => return Err(format!("unexpected token {:?}", self.cur_token)),
        }
    }

    /// Parses a let statement and returns the resulting AST.
    ///
    /// # Returns
    ///
    /// - `Ok(statement)` if the parsing is successful, where `statement` is the parsed let statement.
    /// - `Err(error)` if there is a parsing error, where `error` is the parse error.
    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        // The structure of a let statement consists of:
        // 'Let' <identifier> `Assign` <expression> `Semicolon`

        let start = self.cur_token.span.start;

        // Advance over the `Let` token
        self.next_token();

        // Parse identifier
        let ident = self.parse_identifier()?;

        // Advance over the 'Assign' token
        self.expect_token(TokenKind::Assign)?;

        // Parse expression
        // TODO: For now we are just skipping expression parsing
        while !self.cur_token_is(&TokenKind::Semicolon) {
            if self.cur_token_is(&TokenKind::Eof) {
                panic!("Unexpected EOF");
            }
            self.next_token();
        }

        let end = self.cur_token.span.end;

        Ok(Statement::Let(Let {
            ident,
            expr: Expression::None,
            span: Span { start, end },
        }))
    }

    /// Parses a return statement and returns the resulting AST.
    ///
    /// # Returns
    ///
    /// - `Ok(statement)` if the parsing is successful, where `statement` is the parsed return statement.
    /// - `Err(error)` if there is a parsing error, where `error` is the parse error.
    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        // The structure of a return statement consists of:
        // `Return` <expression> `Semicolon`

        let start = self.cur_token.span.start;

        // Advance over the `Return` token
        self.next_token();

        // TODO: For now we are just skipping expression parsing
        while !self.cur_token_is(&TokenKind::Semicolon) {
            if self.cur_token_is(&TokenKind::Eof) {
                panic!("Unexpected EOF");
            }
            self.next_token();
        }

        let end = self.cur_token.span.end;

        Ok(Statement::Return(Return {
            value: Expression::None,
            span: Span { start, end },
        }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        // Since semicolons are optional, we only consume it if it's there.
        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let start = self.cur_token.span.start;

        let mut left_expression = self.parse_prefix_expression()?;

        while self.peek_token.kind != TokenKind::Semicolon
            && precedence < get_token_precendence(&self.peek_token.kind)
        {
            match self.parse_infix_expression(&left_expression, start) {
                Some(binary_expression) => {
                    left_expression = binary_expression?;
                }
                None => return Ok(left_expression),
            }
        }

        Ok(left_expression)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        match &self.cur_token.kind {
            TokenKind::Identifier { name } => Ok(Expression::Identifier(Identifier {
                name: name.to_string(),
                span: self.cur_token.span.clone(),
            })),
            TokenKind::Int(i) => Ok(Expression::Literal(Literal::Integer {
                value: *i,
                span: self.cur_token.span.clone(),
            })),
            TokenKind::Not | TokenKind::Minus => {
                let start = self.cur_token.span.start;
                let operator = self.cur_token.kind.clone();
                // Consume operator token
                self.next_token();
                let expr = self.parse_expression(Precedence::Prefix)?;
                let end = self.cur_token.span.end;
                return Ok(Expression::Unary(Unary {
                    operator,
                    operand: Box::new(expr),
                    span: Span { start, end },
                }));
            }
            _ => {
                return Err(format!(
                    "no prefix parse function for '{}' found",
                    self.cur_token.kind
                ))
            }
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: &Expression,
        span_start: usize
    ) -> Option<Result<Expression, ParseError>> {
        match &self.cur_token.kind {
            // plus, minus, div, mult, eq, noteq, lt, gt
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mult
            | TokenKind::Div
            | TokenKind::Eq
            | TokenKind::NotEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::Gt
            | TokenKind::GtEq => {
                self.next_token();
                let operator = self.cur_token.clone();
                let precedence = get_token_precendence(&self.cur_token.kind);
                self.next_token();
                let right = self.parse_expression(precedence)?;
                return Some(Ok(Expression::Binary(Binary {
                    operator: operator,
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                    span: Span { start: span_start, end: () },
                })))
            }
            _ => todo!(),
        }
    }

    /// Parses an identifier token and returns an identifier struct.
    ///
    /// # Returns
    ///
    /// - `Ok(identifier)` - where `identifier` is the parsed identifier.
    /// - `Err(error)` - if the current token is not an identifier.
    fn parse_identifier(&self) -> Result<Identifier, ParseError> {
        let name = match &self.cur_token.kind {
            TokenKind::Identifier { name } => name.to_string(),
            _ => return Err(format!("expected Identifier, got {:?}", self.peek_token)),
        };

        Ok(Identifier {
            name,
            span: self.cur_token.span.clone(),
        })
    }
}

pub fn parse(input: &str) -> Result<Program, Vec<ParseError>> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    parser.parse_program()
}