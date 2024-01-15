use presap_lexer::{token::TokenKind, Lexer};

fn main() {
    println!("MR Interactive lexer");
    println!("Enter an expression to generate tokens, or an empty line to exit.");
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        if input.trim_end().is_empty() {
            break;
        }

        let mut lexer = Lexer::new(&input);
        loop {
            let token = lexer.next_token();
            println!("{}", token);
            if token.kind == TokenKind::Eof {
                break;
            }
        }
    }
    println!("Bye");
}
