use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn appendTextToOutput(name: &str);
}

macro_rules! output {
    ($($arg:tt)*) => {
        // #[cfg(target_arg = "wasm32")]
        {
            appendTextToOutput(&format!($($arg)*))
        }

        // #[cfg(not(target_arg = "wasm32"))]
        // {
        //     print!($($arg)*)
        // }
    }
}

#[wasm_bindgen]
pub fn do_some_stuff() {
    output!("Hi, this is Ferris speaking.\n");
    output!("I'm currently talking to you from a WASM binary.\n");
}

#[wasm_bindgen]
pub fn interpret(display_env: bool, source: String) {
    let env = interpreter::create_env();

    // First, parse the input, then evaluate the parsed AST.
    match parser::parse(&source) {
        Ok(parsed_ast) => match interpreter::eval_program(&env, parsed_ast) {
            Ok(evaluation) => {
                if display_env {
                    // Display the environment.
                    output!("\n{}\n", evaluation.0.borrow());
                    // On a new line, display the evaluation result.
                    match *evaluation.1 {
                        interpreter::value::Value::Null => {}
                        _ => output!("\nEvaluated as: {}\n", evaluation.1),
                    }
                } else {
                    // Seperate logic for displaying values if we don't want to display the
                    // environment.
                    match *evaluation.1 {
                        interpreter::value::Value::Null => {}
                        _ => output!("{}\n", evaluation.1),
                    }
                }
            }
            Err(err) => output!("{:?}: {}\n", err.kind, err.message),
        },
        Err(err) => output!("{:?}: {}\n", err.kind, err.message),
    }
}

#[wasm_bindgen]
pub fn parse(source: String) {
    let result = parser::parse(&source);
    match result {
        Ok(parsed_ast) => {
            // Here, we serialise (convert to JSON) the AST before printing it. This improves the
            // readability of the AST, and makes it easier to follow.
            output!("\n=== Serialised AST start ===\n");
            output!(
                "{}",
                ast::ast_to_json(&parsed_ast).unwrap_or(parsed_ast.to_string())
            );
            output!("\n=== Serialised AST end ===\n");

            // Printing the AST directly like this converts it back into a form which is almost
            // identical to the original input, but with parentheses around expressions and uniform
            // whitespace.
            output!("\nParsed as:\n{}\n", parsed_ast);
        }
        Err(err) => output!("{:?}: {}\n", err.kind, err.message),
    }
}

#[wasm_bindgen]
pub fn lex(source: String) {
    let mut lexer = lexer::Lexer::new(&source);
    // Loop through the tokens and print them.
    loop {
        let token = lexer.next_token();
        output!("{}\n", token);
        // If the token is an end of file (EOF) token, then we have reached the end of the input.
        if token.kind == lexer::token::TokenKind::Eof {
            break;
        }
    }
}