use presap_lexer::token::TokenKind;

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // == !=
    LessGreater, // < > <= >=
    Sum,         // + -
    Product,     // * / %
    Prefix,      // -x or !x
    Call,        // print(x)
    Index,       // nums[x]
}

pub fn get_token_precendence(token: &TokenKind) -> Precedence {
    match token {
        TokenKind::Eq | TokenKind::NotEq => Precedence::Equals,
        TokenKind::Lt | TokenKind::LtEq => Precedence::LessGreater,
        TokenKind::Gt | TokenKind::GtEq => Precedence::LessGreater,
        TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
        TokenKind::Mult | TokenKind::Div | TokenKind::Mod => Precedence::Product,
        TokenKind::LParen => Precedence::Call,
        TokenKind::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}
