use log::info;
use presap_ast::expression::{Array, Binary, Expression, FunctionCall, Identifier, Index, Unary};
use presap_ast::literal::Literal;
use presap_ast::statement::{Let, Return, Statement};
use presap_ast::{Block, GetSpan, Program};
use presap_lexer::token::{Span, Token, TokenKind};
use presap_lexer::Lexer;

#[cfg(test)]
mod test;

pub type ParseError = String;

pub struct Parser<'lexer> {
    lexer: Lexer<'lexer>,
    cur_token: Token,
    errors: Vec<ParseError>,
}

impl<'lexer> Parser<'lexer> {
    pub fn new(mut lexer: Lexer<'lexer>) -> Self {
        let cur = lexer.next_token();

        Parser {
            lexer,
            cur_token: cur,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    #[inline]
    fn cur_token_is(&mut self, token: &TokenKind) -> bool {
        self.cur_token.kind == *token
    }

    fn eat(&mut self, expected_kind: &TokenKind) -> Result<(), ParseError> {
        if self.cur_token_is(expected_kind) {
            self.next_token();
            Ok(())
        } else {
            Err(format!(
                "expected {}, got {}",
                expected_kind, self.cur_token.kind
            ))
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, Vec<ParseError>> {
        // <program> -> <statements>? `Eof`
        // <statements> -> <statement> (`Semi` <statement>)* `Semi`?
        let mut program = Program::new();

        while !self.cur_token_is(&TokenKind::Eof) {
            info!("Parsing statement begining with {}", self.cur_token);
            match self.parse_statement() {
                Ok(stmt) => {
                    info!("Parsed statement: \"{}\"", stmt);
                    program.statements.push(stmt);
                }
                Err(err) => {
                    info!("Parser error: {}", err);
                    self.errors.push(err);
                    self.skip_to_next_statement();
                }
            }

            if self.cur_token_is(&TokenKind::Semicolon) {
                self.next_token()
            };
        }

        program.span.end = self.cur_token.span.end;

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors.clone())
        }
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        // <program> -> <statements>? `Eof`
        // <statements> -> <statement> (`Semi` <statement>)* `Semi`?
        // <block> -> `LCurly` <statements> `RCurly`
        let mut block = Block::new();
        let mut errors: Vec<ParseError> = Vec::new();

        while !self.cur_token_is(&TokenKind::Eof) {
            info!("(Block) Parsing statement begining with {}", self.cur_token);
            match self.parse_statement() {
                Ok(stmt) => {
                    info!("(Block) Parsed statement: \"{}\"", stmt);
                    block.statements.push(stmt);
                }
                Err(err) => {
                    info!("(Block) Parser error: {}", err);
                    errors.push(err);
                    self.skip_to_next_statement();
                }
            }

            if self.cur_token_is(&TokenKind::Semicolon) {
                self.next_token()
            };
        }

        block.span.end = self.cur_token.span.end;

        if self.errors.is_empty() {
            Ok(block)
        } else {
            let last_error = errors.pop().unwrap();
            self.errors.append(&mut errors);
            Err(last_error)
        }
    }

    /// Advances the current token upto the next semicolon or EOF, without consuming it
    fn skip_to_next_statement(&mut self) {
        while !matches!(self.cur_token.kind, TokenKind::Eof | TokenKind::Semicolon) {
            info!("skip_to_next_statement(): {}", self.cur_token);
            self.next_token();
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        // <statement> -> <let_stmt> | <return_stmt> | <expression>
        match self.cur_token.kind {
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Statement, ParseError> {
        // <let_stmt> -> `Let` `Ident` `Assign` <expression>
        let start = self.cur_token.span.start;
        self.eat(&TokenKind::Let)?;
        let ident = self.parse_identifier()?;
        self.eat(&TokenKind::Assign)?;
        let expr = self.parse_expression()?;
        let span = Span::new(start, self.cur_token.span.end);

        return Ok(Statement::Let(Let { ident, expr, span }));
    }

    fn parse_return_stmt(&mut self) -> Result<Statement, ParseError> {
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

    fn parse_expr_stmt(&mut self) -> Result<Statement, ParseError> {
        // <expr_stmt> -> <expression>
        let expr = self.parse_expression()?;
        return Ok(Statement::Expression(expr));
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // <expression> -> <bool_expr>
        // self.parse_bool_expr()
        self.parse_eq_expr()
    }

    // fn parse_bool_expr(&mut self) -> Result<Expression, ParseError> {
    //     <bool_expr> -> <eq_expr> ((`And` | `Or`) <eq_expr>)*
    //     let start = self.cur_token.span.start;

    //     let mut node = self.parse_not_expr();

    //     while self.cur_token_is(&TokenKind::And) || self.cur_token_is(&TokenKind::Or) {
    //         // stuff
    //     }
    // }

    fn parse_eq_expr(&mut self) -> Result<Expression, ParseError> {
        // <eq_expr> -> <comp_expr> ((`Eq`|`NotEq`) <comp_expr>)*
        let start = self.cur_token.span.start;
        let mut node = self.parse_comp_expr()?;

        while matches!(&self.cur_token.kind, TokenKind::Eq | TokenKind::NotEq) {
            let operator = self.cur_token.kind.clone();
            // TokenKind has already been checked, so we can safely call next_token()
            self.next_token();
            let right = self.parse_comp_expr()?;
            let span = Span::new(start, self.cur_token.span.end);
            node = Expression::Binary(Binary {
                operator,
                left: Box::new(node),
                right: Box::new(right),
                span,
            });
        }

        return Ok(node);
    }

    fn parse_comp_expr(&mut self) -> Result<Expression, ParseError> {
        // <comp_expr> -> <sum_expr> ((`Less`|`LessEq`|`More`|`MoreEq`) <sum_expr>)*
        let start = self.cur_token.span.start;
        let mut node = self.parse_sum_expr()?;

        while matches!(
            &self.cur_token.kind,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq
        ) {
            let operator = self.cur_token.kind.clone();
            // TokenKind has already been checked, so we can safely call next_token()
            self.next_token();
            let right = self.parse_sum_expr()?;
            let span = Span::new(start, self.cur_token.span.end);
            node = Expression::Binary(Binary {
                operator,
                left: Box::new(node),
                right: Box::new(right),
                span,
            });
        }

        return Ok(node);
    }

    fn parse_sum_expr(&mut self) -> Result<Expression, ParseError> {
        // <sum_expr> -> <product_expr> ((`Plus` | `Minus`) <product_expr>)
        let start = self.cur_token.span.start;
        let mut node = self.parse_product_expr()?;

        while matches!(self.cur_token.kind, TokenKind::Plus | TokenKind::Minus) {
            let operator = self.cur_token.kind.clone();
            // TokenKind has already been checked, so we can safely call next_token()
            self.next_token();
            let right = self.parse_product_expr()?;
            let span = Span::new(start, self.cur_token.span.start);
            node = Expression::Binary(Binary {
                operator,
                left: Box::new(node),
                right: Box::new(right),
                span,
            });
        }

        return Ok(node);
    }

    fn parse_product_expr(&mut self) -> Result<Expression, ParseError> {
        // <product_expr> -> <postfix_expr> ((`Mult` | `Div` | `Mod`) <postfix_expr>)*
        let start = self.cur_token.span.start;
        let mut node = self.parse_postfix_expr()?;

        while matches!(
            self.cur_token.kind,
            TokenKind::Mult | TokenKind::Div | TokenKind::Mod
        ) {
            let operator = self.cur_token.kind.clone();
            // TokenKind has already been checked, so we can safely call next_token()
            self.next_token();
            let right = self.parse_postfix_expr()?;
            let span = Span::new(start, self.cur_token.span.end);
            node = Expression::Binary(Binary {
                operator,
                left: Box::new(node),
                right: Box::new(right),
                span,
            });
        }

        return Ok(node);
    }

    fn parse_postfix_expr(&mut self) -> Result<Expression, ParseError> {
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
                    let arguments = match self.cur_token_is(&TokenKind::RParen) {
                        false => self.parse_expr_list()?,
                        true => vec![],
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

    fn parse_expr_list(&mut self) -> Result<Vec<Expression>, ParseError> {
        // <expr_list> -> <expression> (`,` <expression>)*
        let mut expressions: Vec<Expression> = Vec::new();
        expressions.push(self.parse_expression()?);
        while self.cur_token_is(&TokenKind::Comma) {
            self.eat(&TokenKind::Comma)?;
            expressions.push(self.parse_expression()?);
        }
        return Ok(expressions);
    }

    fn parse_prefix_expr(&mut self) -> Result<Expression, ParseError> {
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

    fn parse_ident_expr(&mut self) -> Result<Expression, ParseError> {
        // <ident_expr> -> `Ident` | <group_expr>
        match &self.cur_token.kind.clone() {
            TokenKind::Identifier { .. } => Ok(Expression::Identifier(self.parse_identifier()?)),
            _ => self.parse_group_expr(),
        }
    }

    fn parse_group_expr(&mut self) -> Result<Expression, ParseError> {
        // <group_expr> -> (`LParen` <expression> `RParen`) | <entity_expr>
        if self.cur_token_is(&TokenKind::LParen) {
            self.eat(&TokenKind::LParen)?;
            let expr = self.parse_expression()?;
            self.eat(&TokenKind::RParen)?;
            return Ok(expr);
        } else {
            return self.parse_entity_expr();
        }
    }

    fn parse_entity_expr(&mut self) -> Result<Expression, ParseError> {
        // <entity_expr> -> <selection_expr>
        //                | <fn_decl_expr>
        //                | <array_expr>
        //                | <literal>
        match &self.cur_token.kind {
            // TokenKind::If => self.parse_selection_expr(),
            // TokenKind::Fn => self.parse_fn_decl_expr(),
            TokenKind::LBracket => self.parse_array_expr(),
            _ => {
                let literal = self.parse_literal()?;
                return Ok(Expression::Literal(literal));
            }
        }
    }

    fn parse_array_expr(&mut self) -> Result<Expression, ParseError> {
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

    fn parse_selection_expr(&mut self) -> Result<Expression, ParseError> {
        // <selection_expr> -> `If` <expr> <block> `Else` <block>
        // let start = self.cur_token.span.start;
        // self.eat(&TokenKind::If)?;
        // let condition_expr = self.parse_expression()?;
        // let conditional_block = self.parse_block()?;
        // self.eat(&TokenKind::Else)?;
        // let else_conditional_block = self.parse_block()?;
        // let span = Span::new(start, self.cur_token.span.end);
        // return Ok(Expression::Selection(Selection {
        //     condition: Box::new(condition_expr),
        //     conditional: conditional_block,
        //     else_conditional: else_conditional_block,
        //     span,
        // }));
        todo!()
    }

    fn parse_fn_decl_expr(&mut self) -> Result<Expression, ParseError> {
        todo!()
    }

    fn parse_literal(&mut self) -> Result<Literal, ParseError> {
        // <literal> -> `Int`
        //     | `Float`
        //     | `Bool`
        //     | `String`
        let span = self.cur_token.span.clone();
        match &self.cur_token.kind.clone() {
            TokenKind::Int(n) => {
                self.next_token();
                Ok(Literal::Integer { value: *n, span })
            }
            TokenKind::Float(n) => {
                self.next_token();
                Ok(Literal::Float { value: *n, span })
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
            // TODO: TokenKind::Bool ...
            _ => Err(format!("Expected Literal, got {}", self.cur_token.kind)),
        }
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let span = self.cur_token.span.clone();
        let name = match &self.cur_token.kind {
            TokenKind::Identifier { name } => name.to_string(),
            _ => return Err(format!("expected Identifier, got {}", self.cur_token.kind)),
        };
        // TokenKind has already been checked, so we can safely call next_token()
        self.next_token();

        Ok(Identifier { name, span })
    }
}

pub fn parse(input: &str) -> Result<Program, Vec<ParseError>> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    parser.parse_program()
}
