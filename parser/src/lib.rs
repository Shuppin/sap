//! # Parser Module
//!
//! The parser module contains the implementation of the SAP language parser. The parser
//! is responsible for taking a sequence of tokens and converting them into an abstract
//! syntax tree (AST). The AST is then used by the interpreter to execute the program.
//!
//! The parser is implemented as a recursive descent parser, which is a top-down parser
//! that starts from the root of the syntax tree and works its way down to the leaves.
//!
//! It is also responsible for reporting syntax errors in the input program. When
//! a syntax error is encountered, the parser returns an error containing a message
//! describing the error.
use std::num::IntErrorKind;

use ast::expression::{
    Array, Binary, Expression, FunctionCall, Identifier, Index, Selection, Unary,
};
use ast::literal::Literal;
use ast::statement::{
    Display, FunctionDeclaration, RepeatForever, RepeatUntil, Return, Set, Statement,
};
use ast::{Program, StatementList};
use lexer::token::{Token, TokenKind};
use lexer::Lexer;
use shared::error::{Error, ErrorKind};
use shared::span::{GetSpan, Span};

// Attempt to obtain the current version of the parser module.
pub const VERSION: Option<&str> = std::option_env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod test;

/// The `Parser` struct is responsible for parsing a sequence of tokens into an abstract
/// syntax tree (AST). The parser is implemented as a recursive descent parser, which is
/// a top-down parser that starts from the root of the syntax tree and works its way down
/// to the leaves.
///
/// The `Parser` struct contains a reference to a `Lexer` instance, which is used to
/// obtain the tokens that make up the input program. The parser is also responsible for
/// reporting syntax errors in the input program. When a syntax error is encountered, the
/// parser returns an error containing a message describing the error.
pub struct Parser<'lexer> {
    lexer: Lexer<'lexer>,
    cur_token: Token,
}

/// This macro is an expansion for creating a new `Error` instance with a syntax error
/// kind. It's intended for use within the parser to simplify the creation of syntax
/// errors.
///
/// Arguments are identical to the `format!` macro.
macro_rules! parse_err {
    ($($arg:tt)*) => {{
        Err(shared::error::Error::new(&format!($($arg)*), ErrorKind::SyntaxError))
    }}
}

/// This macro is an expansion for parsing binary expressions. It's intended for use
/// within the parser to simplify the repetitive pattern of parsing binary operations.
///
/// # Arguments
///
/// * `self` - The parser instance, typically the `self` keyword in a method.
/// * `downstream_parse_fn` - A function that parses the individual expressions on both
///   sides of the binary operation. This function is called repeatedly to handle the left
///   and right-hand sides of the binary operation.
/// * `pattern` - A pattern matching the current token to the relevant operator(s).
macro_rules! parse_binary_expr {
    ($self:ident, $downstream_parse_fn:ident, $pattern:pat) => {
        let start = $self.cur_token.span.start;
        // Parse the left hand side
        let mut node = $self.$downstream_parse_fn()?;

        // While the current token matches the expected operator, repeatedly parse the right hand
        // side and construct a new binary expression.
        while matches!(&$self.cur_token.kind, $pattern) {
            let operator = $self.cur_token.kind.clone();
            // Move onto next token
            $self.next_token();
            // Parse the right hand side
            let right = $self.$downstream_parse_fn()?;
            let span = Span::new(start, $self.cur_token.span.end);
            // Construct a new binary expression, placing the old `node` inside the new one.
            node = Expression::Binary(Binary {
                operator,
                left: Box::new(node),
                right: Box::new(right),
                span,
            });
        }

        let result: Result<Expression, Error> = Ok(node);
        return result;
    };
}

impl<'lexer> Parser<'lexer> {
    pub fn new(mut lexer: Lexer<'lexer>) -> Self {
        // Load the first token from the lexer to be used as the current token.
        let cur = lexer.next_token();
        Parser {
            lexer,
            cur_token: cur,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    /// Returns true if the current token is equal to the expected token.
    #[inline]
    fn cur_token_is(&self, token_kind: &TokenKind) -> bool {
        self.cur_token.kind == *token_kind
    }

    /// Consumes the current token if it matches the expected token, otherwise returns
    /// a syntax error.
    ///
    /// # Arguments
    ///
    /// * `expected_kind` - The expected token kind.
    ///
    /// # Returns
    ///
    /// An `Ok` containing `()` if the current token matches the expected token, otherwise
    /// an `Err` containing a syntax error.
    fn eat(&mut self, expected_kind: &TokenKind) -> Result<(), Error> {
        if self.cur_token_is(expected_kind) {
            self.next_token();
            Ok(())
        } else {
            // This is essentially the default syntax error, any error which doesn't have a specific
            // handler be caught by this.
            parse_err!("expected {}, got {}", expected_kind, self.cur_token.kind)
        }
    }

    /// The main entry point for the parser. This method is responsible for parsing and
    /// lexing the entire input program, and returning the resulting AST or a syntax
    /// error.
    ///
    /// # Returns
    ///
    /// An `Ok` containing the AST (starting with the root `Program` node) if the input
    /// program is successfully parsed, otherwise an `Err` containing a syntax error.
    pub fn parse_program(&mut self) -> Result<Program, Error> {
        let mut program = Program::new();

        if self.cur_token_is(&TokenKind::Eof) {
            return Ok(program);
        } else {
            program.statements = self.parse_statement_list()?.statements;
            program.span.end = self.cur_token.span.end;
            self.eat(&TokenKind::Eof)?;
            return Ok(program);
        }
    }

    /// Parses a list of statements without consuming the termination token
    /// (`End` | `Eof` | `Otherwise`)
    ///
    /// Note: Does not handle empty statements lists, the caller should handle this case.
    fn parse_statement_list(&mut self) -> Result<StatementList, Error> {
        let mut list = StatementList::new();

        // Optionally consume leading NewLines
        while matches!(self.cur_token.kind, TokenKind::NewLine) {
            self.next_token();
        }

        loop {
            // Parse a statement and add it to the list
            list.statements.push(self.parse_statement()?);

            // If the next token is 'End' or 'Eof' or 'Otherwise', this represents the end of the
            // statement list, so we break out of the loop.
            if matches!(
                self.cur_token.kind,
                TokenKind::End | TokenKind::Eof | TokenKind::Otherwise
            ) {
                break; // No separator needed before 'End' or 'Eof'
            }

            // Each statement must be separated by a `NewLine` or `Semicolon`, which is enforced
            // here. The exception is when the next token is 'End' or 'Eof' or 'Otherwise', which is
            // handled by the previous if statement.
            if !matches!(
                self.cur_token.kind,
                TokenKind::Semicolon | TokenKind::NewLine
            ) {
                return parse_err!("expected ';' or newline between statements");
            }
            // Multiple separators are allowed, so we consume all of them.
            while matches!(
                self.cur_token.kind,
                TokenKind::Semicolon | TokenKind::NewLine
            ) {
                self.next_token();
            }

            // If we encounter 'End' or 'Eof' after consuming separators, break out of the loop.
            if matches!(
                self.cur_token.kind,
                TokenKind::End | TokenKind::Eof | TokenKind::Otherwise
            ) {
                break;
            }
        }

        Ok(list)
    }

    /// Parses a single statement.
    ///
    /// The EBNF for a statement is:
    /// ```ebnf
    ///     statement = set_stmt
    ///                 | return_stmt
    ///                 | expression
    ///                 | fn_decl_stmt
    ///                 | repeat_stmt
    ///                 | display_stmt;
    /// ```
    fn parse_statement(&mut self) -> Result<Statement, Error> {
        match self.cur_token.kind {
            TokenKind::Set => self.parse_set_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::DefineFunction => self.parse_fn_decl_stmt(),
            TokenKind::Repeat => self.parse_repeat_stmt(),
            TokenKind::Display => self.parse_display_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    /// Parses a `Set` (i.e. `set x = 4`) statement.
    ///
    /// The EBNF for a `Set` statement is:
    /// ```ebnf
    ///     set_stmt = "set" , ident , "=" , expression;
    /// ```
    fn parse_set_stmt(&mut self) -> Result<Statement, Error> {
        let start = self.cur_token.span.start;
        self.eat(&TokenKind::Set)?;
        let ident = self.parse_identifier()?;
        self.eat(&TokenKind::Assign)?;
        let expr = self.parse_expression()?;
        let span = Span::new(start, self.cur_token.span.end);

        return Ok(Statement::Set(Set { ident, expr, span }));
    }

    /// Parses a `Return` statement.
    ///
    /// The EBNF for a `Return` statement is:
    /// ```ebnf
    ///    return_stmt = "return" , expression;
    /// ```
    fn parse_return_stmt(&mut self) -> Result<Statement, Error> {
        let start = self.cur_token.span.start;
        self.eat(&TokenKind::Return)?;
        let return_value = self.parse_expression()?;
        let span = Span::new(start, self.cur_token.span.end);
        return Ok(Statement::Return(Return {
            value: return_value,
            span,
        }));
    }

    /// Parses a function declaration statement.
    ///
    /// The EBNF for a function declaration statement is:
    /// ```ebnf
    ///     fn_decl_stmt = "defineFunction" , "(" , [params] , ")" , [statements]
    ///                    , "end";
    ///     params = ident , {"," , ident};
    /// ```
    fn parse_fn_decl_stmt(&mut self) -> Result<Statement, Error> {
        let start = self.cur_token.span.start;

        self.eat(&TokenKind::DefineFunction)?;

        // Parse the function name
        let name = self.parse_identifier()?;

        // Parse the function parameters.
        self.eat(&TokenKind::LParen)?;
        let parameters = self.parse_fn_decl_parameters()?;
        self.eat(&TokenKind::RParen)?;

        // Parse the function body.
        // If the function body is empty, return an empty statement list. Otherwise, parse the
        // function body.
        let body = if self.cur_token_is(&TokenKind::End) {
            self.create_empty_statement_list()
        } else {
            self.parse_statement_list()?
        };

        let end = self.cur_token.span.end;
        self.eat(&TokenKind::End)?;
        let span = Span::new(start, end);

        return Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            name,
            parameters,
            body,
            span,
        }));
    }

    /// Parses the parameters of a function declaration.
    ///
    /// The EBNF grammar for this function is:
    /// ```ebnf
    ///     params = [ident , {"," , ident}]
    /// ```
    fn parse_fn_decl_parameters(&mut self) -> Result<Vec<Identifier>, Error> {
        if self.cur_token_is(&TokenKind::RParen) {
            // Return an empty array if there are no parameters.
            return Ok(vec![]);
        } else {
            let mut parameters: Vec<Identifier> = Vec::new();
            // Parse the first parameter
            let param = self.parse_fn_decl_parameter()?;
            parameters.push(param);

            // Parse all subsequent parameters seperated by commas.
            while self.cur_token_is(&TokenKind::Comma) {
                self.eat(&TokenKind::Comma)?;
                let param = self.parse_fn_decl_parameter()?;
                parameters.push(param);
            }
            return Ok(parameters);
        }
    }

    /// This function parses a single function parameter, the same as parsing an
    /// identifier, except with a specialised error message.
    fn parse_fn_decl_parameter(&mut self) -> Result<Identifier, Error> {
        let prev_token_kind = self.cur_token.kind.clone();
        self.parse_identifier().map_err(|mut err| {
            err.message = format!("expected function parameter, got '{}'", prev_token_kind);
            return err;
        })
    }

    /// Parses a `Display` statement.
    ///
    /// The EBNF for a `Display` statement is:
    /// ```ebnf
    ///     display_stmt = "display" , expression , {"," , expression};
    /// ```
    fn parse_display_stmt(&mut self) -> Result<Statement, Error> {
        let start = self.cur_token.span.start;
        self.eat(&TokenKind::Display)?;
        let expressions = match self.cur_token.kind {
            TokenKind::NewLine | TokenKind::Semicolon => {
                return parse_err!("empty display statement");
            }
            _ => self.parse_expr_list()?,
        };
        let span = Span::new(start, self.cur_token.span.end);
        return Ok(Statement::Display(Display { expressions, span }));
    }

    /// Parses a `Repeat` statement.
    ///
    /// The EBNF for a `Repeat` statement is:
    /// ```ebnf
    ///     repeat_stmt = "repeat" , (repeat_n_times | repeat_until | repeat_forever);
    /// ```
    fn parse_repeat_stmt(&mut self) -> Result<Statement, Error> {
        let start = self.cur_token.span.start;

        self.eat(&TokenKind::Repeat)?;

        let statement = match &self.cur_token.kind {
            TokenKind::Until => self.parse_repeat_until(start)?,
            TokenKind::Forever => self.parse_repeat_forever(start)?,
            _ => self.parse_repeat_n_times(start)?,
        };

        return Ok(statement);
    }

    /// Parses a `RepeatUntil` statement.
    ///
    /// The EBNF for a `RepeatUntil` statement is:
    /// ```ebnf
    ///    repeat_until = "until" , expression , [statements] , "end";
    /// ```
    fn parse_repeat_until(&mut self, start: usize) -> Result<Statement, Error> {
        self.eat(&TokenKind::Until)?;

        let expression = self.parse_expression()?;

        let statements = match self.cur_token.kind {
            TokenKind::End => self.create_empty_statement_list(),
            _ => self.parse_statement_list()?,
        };

        let span = Span::new(start, self.cur_token.span.end);

        self.eat(&TokenKind::End)?;

        return Ok(Statement::RepeatUntil(RepeatUntil {
            condition: expression,
            body: statements,
            span,
        }));
    }

    /// Parses a `RepeatForever` statement.
    ///
    /// The EBNF for a `RepeatForever` statement is:
    /// ```ebnf
    ///   repeat_forever = "forever" , [statements] , "end";
    /// ```
    fn parse_repeat_forever(&mut self, start: usize) -> Result<Statement, Error> {
        self.eat(&TokenKind::Forever)?;

        let statements = match self.cur_token.kind {
            TokenKind::End => self.create_empty_statement_list(),
            _ => self.parse_statement_list()?,
        };

        let span = Span::new(start, self.cur_token.span.end);

        self.eat(&TokenKind::End)?;

        return Ok(Statement::RepeatForever(RepeatForever {
            body: statements,
            span,
        }));
    }

    /// Parses a `RepeatNTimes` statement.
    ///
    /// The EBNF for a `RepeatNTimes` statement is:
    /// ```ebnf
    ///    repeat_n_times = expression , "times" , [statements] , "end";
    /// ```
    fn parse_repeat_n_times(&mut self, start: usize) -> Result<Statement, Error> {
        let n = self.parse_expression()?;

        self.eat(&TokenKind::Times)?;

        let statements = match self.cur_token.kind {
            TokenKind::End => self.create_empty_statement_list(),
            _ => self.parse_statement_list()?,
        };

        let span = Span::new(start, self.cur_token.span.end);

        self.eat(&TokenKind::End)?;

        return Ok(Statement::RepeatNTimes(ast::statement::RepeatNTimes {
            n,
            body: statements,
            span,
        }));
    }

    /// Parses an `Expression` statement.
    fn parse_expr_stmt(&mut self) -> Result<Statement, Error> {
        let expr = self.parse_expression()?;
        return Ok(Statement::Expression(expr));
    }

    /// Parses an expression (e.g. `1+1`).
    ///
    /// The EBNF for an expression is:
    /// ```ebnf
    ///     expression = or_expr;
    /// ```
    fn parse_expression(&mut self) -> Result<Expression, Error> {
        self.parse_or_expr()
    }

    /// Parses an or expression (e.g. `true or false`).
    ///
    /// The EBNF for an or expression is:
    /// ```ebnf
    ///     or_expr = and_expr {"or" , and_expr};
    /// ```
    fn parse_or_expr(&mut self) -> Result<Expression, Error> {
        // <or_expr> -> <and_expr> (`Or` <and_expr>)*
        parse_binary_expr!(self, parse_and_expr, TokenKind::Or);
    }

    /// Parses an and expression (e.g. `x > 1 and x < 10`)
    ///
    /// The EBNF for an and expression is:
    /// ```ebnf
    ///     and_expr = eq_expr {"and" , eq_expr};
    /// ```
    fn parse_and_expr(&mut self) -> Result<Expression, Error> {
        parse_binary_expr!(self, parse_eq_expr, TokenKind::And);
    }

    /// Parses an equality expression (e.g. `a == b != c`)
    ///
    /// The EBNF for an equality expression is:
    /// ```ebnf
    ///     eq_expr = comp_expr {("==" | "!=") , comp_expr};
    /// ```
    fn parse_eq_expr(&mut self) -> Result<Expression, Error> {
        parse_binary_expr!(self, parse_comp_expr, TokenKind::Eq | TokenKind::NotEq);
    }

    /// Parses a comparison expression (e.g. `x > 4`)
    ///
    /// The EBNF for a comparison expression is:
    /// ```ebnf
    ///     comp_expr = sum_expr {("<" | "<=" | ">" | ">=") , sum_expr};
    /// ```
    fn parse_comp_expr(&mut self) -> Result<Expression, Error> {
        parse_binary_expr!(
            self,
            parse_sum_expr,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq
        );
    }

    /// Parses a sum expression (e.g. `1+2-3`)
    ///
    /// The EBNF for a sum expression is:
    /// ```ebnf
    ///     sum_expr = product_expr {("+" | "-") , product_expr}
    /// ```
    fn parse_sum_expr(&mut self) -> Result<Expression, Error> {
        parse_binary_expr!(self, parse_product_expr, TokenKind::Plus | TokenKind::Minus);
    }

    /// Parses a product expression (e.g. `1 * 2 / 3 % 4`)
    ///
    /// The EBNF for a product expression is:
    /// ```ebnf
    ///     product_expr = postfix_expr {("*" | "/" | "%") , postfix_expr};
    /// ```
    fn parse_product_expr(&mut self) -> Result<Expression, Error> {
        // <product_expr> -> <postfix_expr> ((`Mult` | `Div` | `Mod`) <postfix_expr>)*
        parse_binary_expr!(
            self,
            parse_postfix_expr,
            TokenKind::Mult | TokenKind::Div | TokenKind::Mod
        );
    }

    /// Parses a post-fix expression (e.g. `x[10]` or `print(x)`)
    ///
    /// The EBNF for a post-fix expression is:
    /// ```ebnf
    ///     postfix_expr = prefix_expr {fn_call | array_index};
    ///         array_index = "[" , expression , "]"
    ///         fn_call = "(" , [args] , ")";
    ///         args = expression , {"," , expression};
    /// ```
    fn parse_postfix_expr(&mut self) -> Result<Expression, Error> {
        let start = self.cur_token.span.start;
        let mut node = self.parse_prefix_expr()?;

        loop {
            match &self.cur_token.kind {
                // Parse array index
                TokenKind::LBracket => {
                    self.eat(&TokenKind::LBracket)?;
                    let index_expr = self.parse_expression()?;
                    self.eat(&TokenKind::RBracket)?;
                    let span = Span::new(start, self.cur_token.span.end);
                    node = Expression::Index(Index {
                        object: Box::new(node),
                        index: Box::new(index_expr),
                        span,
                    })
                }
                // Parse function call
                TokenKind::LParen => {
                    self.eat(&TokenKind::LParen)?;
                    let arguments = self
                        .parse_expr_list_maybe_empty(&TokenKind::RParen)
                        .map_err(|mut err| {
                            err.message =
                                format!("expected function parameter, got {}", self.cur_token.kind);
                            err
                        })?;
                    self.eat(&TokenKind::RParen)?;

                    let span = Span::new(start, self.cur_token.span.end);
                    node = Expression::FunctionCall(FunctionCall {
                        callee: Box::new(node),
                        arguments,
                        span,
                    })
                }
                _ => break,
            };
        }

        return Ok(node);
    }

    /// Parse a prefix expression (e.g. `not true` or `---1`)
    ///
    /// The EBNF for a prefix expression is:
    /// ```ebnf
    ///     prefix_expr = {"not" | "-"} , ident_expr;
    /// ```
    fn parse_prefix_expr(&mut self) -> Result<Expression, Error> {
        // Add each prefix operation into an array. For example, if the expression was `not -1`,
        // the array would be `[TokenKind::Not, TokenKind::Minus]`.
        let mut prefix_operations = Vec::new();
        while matches!(&self.cur_token.kind, TokenKind::Not | TokenKind::Minus) {
            prefix_operations.push(self.cur_token.clone());
            self.next_token();
        }

        // Parse the expression.
        let mut node: Expression = self.parse_ident_expr()?;

        // Now, add each operation to the syntax tree IN REVERSE, since the tree starts from the
        // root node (the expression), and then the prefix operation closest to the expression is
        // added to the tree first.
        for operation in prefix_operations.into_iter().rev() {
            let operator = operation.kind.clone();
            let span = Span::new(operation.span.start, node.span().end);
            node = Expression::Unary(Unary {
                operator,
                operand: Box::new(node),
                span,
            })
        }

        return Ok(node);
    }

    /// Parse an identifier expression (e.g. `my_var`).
    ///
    /// The EBNF for an identfier expression is:
    /// ```ebnf
    ///     ident_expr = ident | group_expr;
    /// ```
    fn parse_ident_expr(&mut self) -> Result<Expression, Error> {
        match &self.cur_token.kind.clone() {
            TokenKind::Identifier { .. } => Ok(Expression::Identifier(self.parse_identifier()?)),
            _ => self.parse_group_expr(),
        }
    }

    /// Parse a group expression (e.g. `(1+1)*2`).
    ///
    /// The EBNF for a group expression is:
    /// ```ebnf
    ///     group_expr = ("(" , expression , ")") | entity_expr;
    /// ```
    fn parse_group_expr(&mut self) -> Result<Expression, Error> {
        if self.cur_token_is(&TokenKind::LParen) {
            self.eat(&TokenKind::LParen)?;
            let expr = self.parse_expression()?;
            self.eat(&TokenKind::RParen)?;
            return Ok(expr);
        } else {
            return self.parse_entity_expr();
        }
    }

    /// Parse an entity expression. An 'entity' is a single building block of an
    /// expression, it cannot be sliced up into smaller expressions.
    ///
    /// The EBNF for an entity expression is:
    /// ```ebnf
    ///     entity_expr = selection_expr | array_expr | literal;
    /// ```
    fn parse_entity_expr(&mut self) -> Result<Expression, Error> {
        match &self.cur_token.kind {
            TokenKind::If => return self.parse_selection_expr(),
            TokenKind::LBracket => return self.parse_array_expr(),
            _ => {
                let literal = self.parse_literal()?;
                return Ok(Expression::Literal(literal));
            }
        }
    }

    /// Parse an array expression (e.g. `[1, 2, x, 16/4]`).
    ///
    /// The EBNF for an array expression is:
    /// ```ebnf
    ///     array_expr = "[" , [elements] , "]";
    ///         elements = expression , {"," , expression};
    /// ```
    fn parse_array_expr(&mut self) -> Result<Expression, Error> {
        let start = self.cur_token.span.start;
        self.eat(&TokenKind::LBracket)?;
        let elements = self.parse_expr_list_maybe_empty(&TokenKind::RBracket)?;
        self.eat(&TokenKind::RBracket)?;
        let span = Span::new(start, self.cur_token.span.end);

        return Ok(Expression::Array(Array { elements, span }));
    }

    /// Parse a selection expression (e.g. `if x > 4 then return x end`)
    ///
    /// The EBNF for an array expression is:
    /// ```ebnf
    ///     selection_expr = "if" , expression , "then" , [statements]
    ///                      , ["otherwise" [statements]] "end";
    /// ```
    fn parse_selection_expr(&mut self) -> Result<Expression, Error> {
        let start = self.cur_token.span.start;

        self.eat(&TokenKind::If)?;
        let condition_expr = self.parse_expression()?;
        self.eat(&TokenKind::Then)?;
        let conditional_statements = self.parse_selection_if_body()?;
        let else_conditional_block = self.parse_selection_else_body()?;

        let end = self.cur_token.span.end;
        self.eat(&TokenKind::End)?;
        let span = Span::new(start, end);

        return Ok(Expression::Selection(Selection {
            condition: Box::new(condition_expr),
            conditional: conditional_statements,
            else_conditional: else_conditional_block,
            span,
        }));
    }

    /// Parse the statements within the 'if' branch of a selection statement.
    fn parse_selection_if_body(&mut self) -> Result<StatementList, Error> {
        match matches!(self.cur_token.kind, TokenKind::End | TokenKind::Otherwise) {
            false => self.parse_statement_list(),
            true => Ok(self.create_empty_statement_list()),
        }
    }

    /// Parse the statements within the 'else' branch of a selection statement (if it
    /// exists).
    fn parse_selection_else_body(&mut self) -> Result<Option<StatementList>, Error> {
        if self.cur_token_is(&TokenKind::Otherwise) {
            self.eat(&TokenKind::Otherwise)?;
            let else_conditional_statements = match matches!(self.cur_token.kind, TokenKind::End) {
                false => self.parse_statement_list()?,
                true => self.create_empty_statement_list(),
            };
            Ok(Some(else_conditional_statements))
        } else {
            Ok(None)
        }
    }

    /// Parse a literal. A literal is a hard-coded `string`, `int`, `float` or `bool`.
    ///
    /// The EBNF grammar for a literal is:
    /// ```ebnf
    ///     literal = int | float | bool | string;
    ///         bool = "true" | "false";
    ///         float = int , "." , int;
    ///         int = digit , {digit};
    ///             digit = "0" | "1" | ... | "9";
    ///         string = '"', {char} , '"';
    ///             char = ? any character ? -'"';
    /// ```
    fn parse_literal(&mut self) -> Result<Literal, Error> {
        let span = self.cur_token.span.clone();
        match &self.cur_token.kind.clone() {
            TokenKind::Int(n) => {
                return self.parse_integer_literal(n);
            }
            TokenKind::Float(n) => {
                return self.parse_float_literal(n);
            }
            TokenKind::True => {
                self.next_token();
                return Ok(Literal::Boolean { value: true, span });
            }
            TokenKind::False => {
                self.next_token();
                return Ok(Literal::Boolean { value: false, span });
            }
            TokenKind::String(string) => {
                self.next_token();
                return Ok(Literal::String {
                    value: string.to_owned(),
                    span,
                });
            }
            _ => parse_err!("expected Literal, got {}", self.cur_token.kind),
        }
    }

    /// Converts a string into an integer literal.
    fn parse_integer_literal(&mut self, integer_to_parse: &String) -> Result<Literal, Error> {
        let span = self.cur_token.span.clone();
        self.next_token();
        match integer_to_parse.parse::<i64>() {
            Ok(n) => Ok(Literal::Integer { value: n, span }),
            Err(err) => match err.kind() {
                IntErrorKind::PosOverflow => Err(Error::new(
                    &format!(
                        "literal to large for type Integer, whose maximum value is `{}`",
                        i64::MAX
                    ),
                    ErrorKind::OverflowError,
                )),
                _ => parse_err!("failed to parse literal into Integer"),
            },
        }
    }

    /// Converts a string into a float literal.
    fn parse_float_literal(&mut self, float_to_parse: &String) -> Result<Literal, Error> {
        let span = self.cur_token.span.clone();
        self.next_token();
        match float_to_parse.parse::<f64>() {
            Ok(n) => Ok(Literal::Float { value: n, span }),
            Err(_) => {
                return parse_err!("failed to parse literal into Float");
            }
        }
    }

    /// Parse an identifier (e.g. `my_var`).
    ///
    /// The EBNF for this function is:
    /// ```ebnf
    ///     ident = letter , {letter | digit};
    ///         digit = "0" | "1" | ... | "9";
    ///         letter = ? any alphabetical character ? | "_";
    /// ```
    fn parse_identifier(&mut self) -> Result<Identifier, Error> {
        let span = self.cur_token.span.clone();
        let name = match &self.cur_token.kind {
            TokenKind::Identifier { name } => name.to_string(),
            _ => return parse_err!("expected Identifier, got {}", self.cur_token.kind),
        };
        // TokenKind has already been checked, so we can safely call next_token()
        self.next_token();

        Ok(Identifier { name, span })
    }

    /// Parses a potentially empty list of (comma seperated) expressions.
    ///
    /// The EBNF for this function is:
    /// ```ebnf
    ///     expressions = [expression , {"," , expression}];
    /// ```
    fn parse_expr_list_maybe_empty(
        &mut self,
        ending_token: &TokenKind,
    ) -> Result<Vec<Expression>, Error> {
        if self.cur_token_is(ending_token) {
            return Ok(vec![]);
        } else {
            return self.parse_expr_list();
        };
    }

    /// Parses a list of (comma seperated) expressions.
    ///
    /// The EBNF for this function is:
    /// ```ebnf
    ///     expressions = expression , {"," , expression};
    /// ```
    fn parse_expr_list(&mut self) -> Result<Vec<Expression>, Error> {
        let mut expressions: Vec<Expression> = Vec::new();
        expressions.push(self.parse_expression()?);
        while self.cur_token_is(&TokenKind::Comma) {
            self.eat(&TokenKind::Comma)?;
            expressions.push(self.parse_expression()?);
        }
        return Ok(expressions);
    }

    /// Helper function to create an empty statement list at the location fo the current
    /// token.
    fn create_empty_statement_list(&self) -> StatementList {
        StatementList {
            statements: vec![],
            span: Span::new(self.cur_token.span.start, self.cur_token.span.start),
        }
    }
}

/// This function takes in a string input, and produces either a valid syntax tree, or a
/// syntax error.
///
/// # Arguments
///
/// * `input` - The input SAP program as a string.
///
/// # Returns
///
/// * If there are no syntax errors, an `Ok(Program)`, which is the root node containing the
/// entire AST (Abstract Syntax Tree) which represents the program.
///
/// * If there are syntax errors, then an `Err(Error)` is returned, containing information
/// about what went wrong.
pub fn parse(input: &str) -> Result<Program, Error> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    return parser.parse_program();
}
