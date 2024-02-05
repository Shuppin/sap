use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

use interpreter::eval_program;
use interpreter::runtime::Environment;
use parser::parse;

fn main() {
    println!("MR REPL");
    println!("Enter an expression to evaluate, or an empty line to exit.");

    let env = Rc::new(RefCell::new(Environment::new()));

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
                let evaluation = eval_program(&env, parsed_ast);
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
