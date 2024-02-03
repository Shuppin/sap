use std::io::Write;

use interpreter::Interpreter;
use parser::parse;

fn main() {
    println!("MR REPL");
    println!("Enter an expression to evaluate, or an empty line to exit.");

    let mut interpreter = Interpreter::new();

    loop {
        let mut input = String::new();
        print!("> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        println!();

        if input.trim_end().is_empty() {
            break;
        }

        match parse(&input) {
            Ok(parsed_ast) => {
                let evaluation = interpreter.eval_program(parsed_ast);
                match evaluation {
                    Ok(value) => println!("{}", value),
                    Err(err) => println!("{:?}: {}", err.kind, err.message),
                }
            }
            Err(err) => {
                println!("Parser generated {} error(s): {:#?}", err.len(), err);
            }
        }
    }
    println!("Bye");
}
