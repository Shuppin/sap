use std::io::Write;

use log::{error, info, LevelFilter};
use presap_parser::parse;
use serde_json;

fn main() {
    simple_logging::log_to_file("runtime.log", LevelFilter::Info).unwrap();
    // simple_logging::log_to_stderr(LevelFilter::Info);
    info!("Hello world");
    println!("MR Interactive parser");
    println!("Enter an expression to generate tokens, or an empty line to exit.");
    loop {
        let mut input = String::new();
        print!("> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        println!();

        if input.trim_end().is_empty() {
            break;
        }

        info!("Parsing input {:?}", input);

        match parse(&input) {
            Ok(parsed_ast) => {
                info!(
                    "=== Serialised AST START ===\n{}",
                    serde_json::to_string_pretty(&parsed_ast).unwrap()
                );
                info!("=== Serialised AST END ===");
                println!("\nParsed as:\n{}", parsed_ast);
            }
            Err(err) => {
                println!("Parser generated {} error(s): {:#?}", err.len(), err);
                error!("{:?}", err);
            }
        }
    }
    println!("Bye");
}
