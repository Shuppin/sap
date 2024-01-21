use log::{error, info, LevelFilter};
use serde_json;
use std::io::Write;

use presap_parser::parse;

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
                info!("{:#?}", parsed_ast);
                info!("\n\n------------------\n");
                info!("{}", serde_json::to_string_pretty(&parsed_ast).unwrap());
                info!("\n\n------------------\n");
                info!("{}", parsed_ast);
                println!("\nParsed as:\n{}", parsed_ast);
            }
            Err(err) => error!("{:?}", err),
        }
    }
    println!("Bye");
}
