//! This file serves as the entry point for the lexer module when compiled to a binary.
//! It prompts the user to enter an expression and generates tokens based on the input.
//! The tokens are then printed to the console until an empty line is entered to exit the
//! program.
use presap_lexer::token::TokenKind;
use presap_lexer::Lexer;

fn main() {
    println!("MR Interactive lexer");
    println!("Enter an expression to generate tokens, or an empty line to exit.");
    loop {
        // Obtain the input string from standard input.
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        // `trim_end()` removes trailing whitespace
        if input.trim_end().is_empty() {
            break;
        }

        let mut lexer = Lexer::new(&input);

        // Begin the Read-Lex-Print-Loop (RLPL)
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
