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

pub struct Parser<'lexer> {
    lexer: Lexer<'lexer>,
    cur_token: Token,
}

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
/// * `parse_fn` - A function that parses the individual expressions on both sides of the
///   binary operation. This function is called repeatedly to handle the left and
///   right-hand sides of the binary operation.
/// * `pattern` - A pattern matching the current token to the relevant operator(s).
macro_rules! parse_binary_expr {
    ($self:ident, $parse_fn:ident, $pattern:pat) => {
        let start = $self.cur_token.span.start;
        let mut node = $self.$parse_fn()?;

        while matches!(&$self.cur_token.kind, $pattern) {
            let operator = $self.cur_token.kind.clone();
            $self.next_token();
            let right = $self.$parse_fn()?;
            let span = Span::new(start, $self.cur_token.span.end);
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
        let cur = lexer.next_token();

        Parser {
            lexer,
            cur_token: cur,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    #[inline]
    fn cur_token_is(&mut self, token: &TokenKind) -> bool {
        self.cur_token.kind == *token
    }

    fn eat(&mut self, expected_kind: &TokenKind) -> Result<(), Error> {
        if self.cur_token_is(expected_kind) {
            self.next_token();
            Ok(())
        } else {
            parse_err!("expected {}, got {}", expected_kind, self.cur_token.kind)
        }
    }

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
    /// Note: Does not handle empty statements lists.
    fn parse_statement_list(&mut self) -> Result<StatementList, Error> {
        let mut list = StatementList::new();

        // Optionally consume leading NewLines
        while matches!(self.cur_token.kind, TokenKind::NewLine) {
            self.next_token();
        }

        loop {
            list.statements.push(self.parse_statement()?);

            // Check if the next token is the end of the block or file
            if matches!(
                self.cur_token.kind,
                TokenKind::End | TokenKind::Eof | TokenKind::Otherwise
            ) {
                break; // No separator needed before 'End' or 'Eof'
            }

            // Enforce separator after each statement, except before 'End' or 'Eof'
            if !matches!(
                self.cur_token.kind,
                TokenKind::Semicolon | TokenKind::NewLine
            ) {
                return parse_err!("expected ';' or newline between statements");
            }

            // Consume any number of NewLine or Semicolon separators
            while matches!(
                self.cur_token.kind,
                TokenKind::Semicolon | TokenKind::NewLine
            ) {
                self.next_token();
            }
            // If we encounter 'End' or 'Eof' after consuming separators, break out of the loop
            if matches!(
                self.cur_token.kind,
                TokenKind::End | TokenKind::Eof | TokenKind::Otherwise
            ) {
                break;
            }
        }

        Ok(list)
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        // <statement> -> <set_stmt> | <return_stmt> | <expression> | <fn_decl_stmt> |
        //                <repeat_stmt> | <display_stmt>
        match self.cur_token.kind {
            TokenKind::Set => self.parse_set_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::DefineFunction => self.parse_fn_decl_stmt(),
            TokenKind::Repeat => self.parse_repeat_stmt(),
            TokenKind::Display => self.parse_display_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_set_stmt(&mut self) -> Result<Statement, Error> {
        // <Set_stmt> -> `Set` `Ident` `Assign` <expression>
        let start = self.cur_token.span.start;
        self.eat(&TokenKind::Set)?;
        let ident = self.parse_identifier()?;
        self.eat(&TokenKind::Assign)?;
        let expr = self.parse_expression()?;
        let span = Span::new(start, self.cur_token.span.end);

        return Ok(Statement::Set(Set { ident, expr, span }));
    }

    fn parse_return_stmt(&mut self) -> Result<Statement, Error> {
        // <return_stmt> -> `Return` <expression>
        let start = self.cur_token.span.start;
        self.eat(&TokenKind::Return)?;
        let return_value = self.parse_expression()?;
        let span = Span::new(start, self.cur_token.span.end);
        return Ok(Statement::Return(Return {
            value: return_value,
            span,
        }));
    }

    fn parse_fn_decl_stmt(&mut self) -> Result<Statement, Error> {
        // <fn_decl_expr> -> <fn_decl_expr> -> `defineFunction` `Ident` `LParen` <fn_params>
        // `RParen` <statements>? `End`
        // <fn_params> -> `LParen` (`Ident` (`,` `Ident`)*)? `RParen`
        let start = self.cur_token.span.start;

        self.eat(&TokenKind::DefineFunction)?;

        let name = self.parse_identifier()?;

        self.eat(&TokenKind::LParen)?;
        let parameters = match self.cur_token_is(&TokenKind::RParen) {
            false => {
                // `Ident` (`,` `Ident`)*
                let mut identifiers: Vec<Identifier> = Vec::new();
                let prev_token_kind = self.cur_token.kind.clone();
                match self.parse_identifier() {
                    Ok(ident) => identifiers.push(ident),
                    Err(_) => {
                        return parse_err!("expected function parameter, got '{}'", prev_token_kind)
                    }
                }
                while self.cur_token_is(&TokenKind::Comma) {
                    self.eat(&TokenKind::Comma)?;
                    let prev_token_kind = self.cur_token.kind.clone();
                    match self.parse_identifier() {
                        Ok(ident) => identifiers.push(ident),
                        Err(_) => {
                            return parse_err!(
                                "expected function parameter, got '{}'",
                                prev_token_kind
                            )
                        }
                    }
                }
                identifiers
            }
            true => vec![],
        };
        self.eat(&TokenKind::RParen)?;

        // Here we do a switch case to check if the function body is empty
        let body = match self.cur_token_is(&TokenKind::End) {
            false => self.parse_statement_list()?,
            true => StatementList {
                statements: vec![],
                span: Span::new(self.cur_token.span.start, self.cur_token.span.start),
            },
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

    fn parse_display_stmt(&mut self) -> Result<Statement, Error> {
        // <display_stmt> -> `Display` <expr_list>
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

    fn parse_repeat_stmt(&mut self) -> Result<Statement, Error> {
        // <repeat_stmt> -> `Repeat` (<repeat_n_times> | <repeat_until> | <repeat_forever>)
        let start = self.cur_token.span.start;

        self.eat(&TokenKind::Repeat)?;

        let statement = match &self.cur_token.kind {
            TokenKind::Until => self.parse_repeat_until(start)?,
            TokenKind::Forever => self.parse_repeat_forever(start)?,
            _ => self.parse_repeat_n_times(start)?,
        };

        return Ok(statement);
    }

    fn parse_repeat_until(&mut self, start: usize) -> Result<Statement, Error> {
        // <repeat_until> -> `Until` <expression> <statements>? `End`

        self.eat(&TokenKind::Until)?;

        let expression = self.parse_expression()?;

        let statements = match self.cur_token.kind {
            TokenKind::End => StatementList {
                statements: vec![],
                span: Span::new(self.cur_token.span.start, self.cur_token.span.start),
            },
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

    fn parse_repeat_forever(&mut self, start: usize) -> Result<Statement, Error> {
        // <repeat_forever> -> `Forever` <statements>? `End`

        self.eat(&TokenKind::Forever)?;

        let statements = match self.cur_token.kind {
            TokenKind::End => StatementList {
                statements: vec![],
                span: Span::new(self.cur_token.span.start, self.cur_token.span.start),
            },
            _ => self.parse_statement_list()?,
        };

        let span = Span::new(start, self.cur_token.span.end);

        self.eat(&TokenKind::End)?;

        return Ok(Statement::RepeatForever(RepeatForever {
            body: statements,
            span,
        }));
    }

    fn parse_repeat_n_times(&mut self, start: usize) -> Result<Statement, Error> {
        // <repeat_n_times> -> <expression> `Times` <statements>? `End`

        let n = self.parse_expression()?;

        self.eat(&TokenKind::Times)?;

        let statements = match self.cur_token.kind {
            TokenKind::End => StatementList {
                statements: vec![],
                span: Span::new(self.cur_token.span.start, self.cur_token.span.start),
            },
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

    fn parse_expr_stmt(&mut self) -> Result<Statement, Error> {
        // <expr_stmt> -> <expression>
        let expr = self.parse_expression()?;
        return Ok(Statement::Expression(expr));
    }

    fn parse_expression(&mut self) -> Result<Expression, Error> {
        // <expression> -> <bool_expr>
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<Expression, Error> {
        // <or_expr> -> <and_expr> (`Or` <and_expr>)*
        parse_binary_expr!(self, parse_and_expr, TokenKind::Or);
    }

    fn parse_and_expr(&mut self) -> Result<Expression, Error> {
        // <and_expr> -> <eq_expr> (`And` <eq_expr>)*
        parse_binary_expr!(self, parse_eq_expr, TokenKind::And);
    }

    fn parse_eq_expr(&mut self) -> Result<Expression, Error> {
        // <eq_expr> -> <comp_expr> ((`Eq`|`NotEq`) <comp_expr>)*
        parse_binary_expr!(self, parse_comp_expr, TokenKind::Eq | TokenKind::NotEq);
    }

    fn parse_comp_expr(&mut self) -> Result<Expression, Error> {
        // <comp_expr> -> <sum_expr> ((`Less`|`LessEq`|`More`|`MoreEq`) <sum_expr>)*
        parse_binary_expr!(
            self,
            parse_sum_expr,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq
        );
    }

    fn parse_sum_expr(&mut self) -> Result<Expression, Error> {
        // <sum_expr> -> <product_expr> ((`Plus` | `Minus`) <product_expr>)
        parse_binary_expr!(self, parse_product_expr, TokenKind::Plus | TokenKind::Minus);
    }

    fn parse_product_expr(&mut self) -> Result<Expression, Error> {
        // <product_expr> -> <postfix_expr> ((`Mult` | `Div` | `Mod`) <postfix_expr>)*
        parse_binary_expr!(
            self,
            parse_postfix_expr,
            TokenKind::Mult | TokenKind::Div | TokenKind::Mod
        );
    }

    fn parse_postfix_expr(&mut self) -> Result<Expression, Error> {
        // <postfix_expr> -> <prefix_expr> (<fn_call> | <array_index>)*
        //
        // <array_index> -> `LBracket` <expression> `RBracket`
        //
        // <fn_call> -> `LParen` <expr_list>? `RParen`
        let start = self.cur_token.span.start;
        let mut node = self.parse_prefix_expr()?;

        loop {
            match &self.cur_token.kind {
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
                TokenKind::LParen => {
                    self.eat(&TokenKind::LParen)?;
                    let arguments = if self.cur_token_is(&TokenKind::RParen) {
                        vec![]
                    } else {
                        self.parse_expr_list().map_err(|mut err| {
                            err.message =
                                format!("expected function parameter, got {}", self.cur_token.kind);
                            err
                        })?
                    };
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

    fn parse_prefix_expr(&mut self) -> Result<Expression, Error> {
        // <prefix_expr> -> (`Not`|`Minus`)* <ident_expr>

        // !-hello;
        // ["!", "-"]

        // node = Ident(Hello)
        // node = Unary("-", Ident(Hello))
        // node = Unary("!", Unary("-", Ident(Hello)))
        let mut prefix_operations = Vec::new();
        while matches!(&self.cur_token.kind, TokenKind::Not | TokenKind::Minus) {
            prefix_operations.push(self.cur_token.clone());
            self.next_token();
        }

        let mut node: Expression = self.parse_ident_expr()?;

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

    fn parse_ident_expr(&mut self) -> Result<Expression, Error> {
        // <ident_expr> -> `Ident` | <group_expr>
        match &self.cur_token.kind.clone() {
            TokenKind::Identifier { .. } => Ok(Expression::Identifier(self.parse_identifier()?)),
            _ => self.parse_group_expr(),
        }
    }

    fn parse_group_expr(&mut self) -> Result<Expression, Error> {
        // <group_expr> -> (`LParen` <expression>? `RParen`) | <entity_expr>
        if self.cur_token_is(&TokenKind::LParen) {
            self.eat(&TokenKind::LParen)?;
            let expr = self.parse_expression()?;
            self.eat(&TokenKind::RParen)?;
            return Ok(expr);
        } else {
            return self.parse_entity_expr();
        }
    }

    fn parse_entity_expr(&mut self) -> Result<Expression, Error> {
        // <entity_expr> -> <selection_expr>
        //                | <array_expr>
        //                | <literal>
        match &self.cur_token.kind {
            TokenKind::If => self.parse_selection_expr(),
            TokenKind::LBracket => self.parse_array_expr(),
            _ => {
                let literal = self.parse_literal()?;
                return Ok(Expression::Literal(literal));
            }
        }
    }

    fn parse_array_expr(&mut self) -> Result<Expression, Error> {
        // <array_expr> -> `LBracket` <expr_list>? `LBracket`
        let start = self.cur_token.span.start;
        self.eat(&TokenKind::LBracket)?;
        let elements = match self.cur_token_is(&TokenKind::RBracket) {
            false => self.parse_expr_list()?,
            true => vec![],
        };
        self.eat(&TokenKind::RBracket)?;
        let span = Span::new(start, self.cur_token.span.end);

        return Ok(Expression::Array(Array { elements, span }));
    }

    fn parse_selection_expr(&mut self) -> Result<Expression, Error> {
        // <selection_expr> -> `If` <expr> `Then` <statements>? (`Otherwise` <statements>?)? `End`
        let start = self.cur_token.span.start;

        self.eat(&TokenKind::If)?;
        let condition_expr = self.parse_expression()?;
        self.eat(&TokenKind::Then)?;
        let conditional_statements =
            match matches!(self.cur_token.kind, TokenKind::End | TokenKind::Otherwise) {
                false => self.parse_statement_list()?,
                true => StatementList {
                    statements: vec![],
                    span: Span::new(self.cur_token.span.start, self.cur_token.span.start),
                },
            };

        let else_conditional_block = if self.cur_token_is(&TokenKind::Otherwise) {
            self.eat(&TokenKind::Otherwise)?;
            let else_conditional_statements = match matches!(self.cur_token.kind, TokenKind::End) {
                false => self.parse_statement_list()?,
                true => StatementList {
                    statements: vec![],
                    span: Span::new(self.cur_token.span.start, self.cur_token.span.start),
                },
            };
            Some(else_conditional_statements)
        } else {
            None
        };

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

    fn parse_literal(&mut self) -> Result<Literal, Error> {
        // <literal> -> `Int`
        //     | `Float`
        //     | `Bool`
        //     | `String`
        let span = self.cur_token.span.clone();
        match &self.cur_token.kind.clone() {
            TokenKind::Int(n) => {
                self.next_token();
                match n.parse::<i64>() {
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
            TokenKind::Float(n) => {
                self.next_token();
                match n.parse::<f64>() {
                    Ok(n) => Ok(Literal::Float { value: n, span }),
                    Err(_) => {
                        return parse_err!("failed to parse literal into Float");
                    }
                }
            }
            TokenKind::True => {
                self.next_token();
                Ok(Literal::Boolean { value: true, span })
            }
            TokenKind::False => {
                self.next_token();
                Ok(Literal::Boolean { value: false, span })
            }
            TokenKind::String(string) => {
                self.next_token();
                Ok(Literal::String {
                    value: string.to_owned(),
                    span,
                })
            }
            _ => parse_err!("expected Literal, got {}", self.cur_token.kind),
        }
    }

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

    fn parse_expr_list(&mut self) -> Result<Vec<Expression>, Error> {
        // <expr_list> -> <expression> (`,` <expression>)*
        let mut expressions: Vec<Expression> = Vec::new();
        expressions.push(self.parse_expression()?);
        while self.cur_token_is(&TokenKind::Comma) {
            self.eat(&TokenKind::Comma)?;
            expressions.push(self.parse_expression()?);
        }
        return Ok(expressions);
    }
}

pub fn parse(input: &str) -> Result<Program, Error> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    return parser.parse_program();
}
