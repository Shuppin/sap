use std::fs::File;
use std::io::{Read, Write};
use std::time::Instant;

use clap::{Arg, Command};
use lexer::Lexer;

// Attempt to obtain the current version of the CLI package
pub const VERSION: Option<&str> = std::option_env!("CARGO_PKG_VERSION");

fn main() -> Result<(), String> {
    let matches = cli().get_matches();

    let mut start: Option<Instant> = None;

    // Access the FILE argument
    if let Some(file_path) = matches.get_one::<String>("FILE") {
        // Some cursed logic to check if the "-t" argument was passed
        if *matches.get_one::<bool>("timer").unwrap_or(&false) {
            start = Some(Instant::now());
        }

        let mut file = File::open(file_path).map_err(|e| e.to_string())?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| e.to_string())?;

        match matches.get_one::<String>("output").map(|s| s.as_str()) {
            Some("ast") | Some("parse") => {
                let result = parser::parse(&contents);
                print_parse_result(result);
            }
            Some("tokens") | Some("lex") => {
                let mut lexer = lexer::Lexer::new(&contents);
                generate_and_print_tokens(&mut lexer);
            }
            Some("eval") | None => {
                let env = interpreter::create_env();
                match parser::parse(&contents) {
                    Ok(parsed_ast) => {
                        if let Err(err) = interpreter::eval_program(&env, parsed_ast) {
                            println!("{:?}: {}", err.kind, err.message)
                        }
                    }
                    Err(errors) => {
                        for err in errors {
                            println!("{:?}: {}", err.kind, err.message)
                        }
                    }
                };
            }
            Some(_) => return Err("Invalid output option provided".to_string()),
        };

        if let Some(start) = start {
            let time_elapsed = Instant::now() - start;
            println!("Execution finished in {}ms", time_elapsed.as_millis())
        }

        Ok(())
    } else {
        // Only set the Ctrl-C handler for the REPLs
        ctrlc::set_handler(move || {
            println!("\nBye!");
            std::process::exit(0);
        })
        .expect("Error setting Ctrl-C handler");

        // Access the --output/-o argument
        match matches.get_one::<String>("output").map(|s| s.as_str()) {
            Some("ast") | Some("parse") => parser_repl(),
            Some("tokens") | Some("lex") => lexer_repl(),
            Some("eval") | None => interpreter_repl(),
            _ => Err("Invalid output option provided".to_string()),
        }
    }
}

fn construct_info_message(title: &str, version: &str) -> String {
    // Retrieve information about the operating system
    let info = os_info::get();

    return format!(
        "{} v{} for {} {} [{}]",
        title,
        version,
        info.os_type(),
        info.version(),
        info.bitness(),
    );
}

fn cli() -> Command {
    let title = "SAP Launcher";
    let cli_version = crate::VERSION.unwrap_or("<unknown>");

    let description = construct_info_message(title, cli_version);

    Command::new("sap")
        .name("SAP Launcher")
        .about(description)
        .arg(
            Arg::new("FILE").help("Sets the input file to use").index(1), // Positional
        )
        .arg(
            Arg::new("output")
                .short('o') // Allows -o
                .long("output") // Allows --output
                .value_parser(["eval", "ast", "parse", "tokens", "lex"])
                .help("Sets the output type"),
        )
        .arg(
            Arg::new("timer")
                .short('t')
                .long("timer")
                .num_args(0)
                .help("Enables fhbsdjkhfb"),
        )
}

fn interpreter_repl() -> Result<(), String> {
    let title = "SAP Interpreter";
    let version = interpreter::VERSION.unwrap_or("<unknown>");
    println!("{}", construct_info_message(title, version));
    println!("Enter an expression to evaluate, or Ctrl-C to exit");

    let env = interpreter::create_env();

    loop {
        let line = readline()?;
        let line = line.trim();

        if line.is_empty() {
            continue;
        }

        match parser::parse(&line) {
            Ok(parsed_ast) => {
                let result = interpreter::eval_program(&env, parsed_ast);
                match result {
                    Ok(evaluation) => println!("{}", evaluation.1),
                    Err(err) => println!("{:?}: {}", err.kind, err.message),
                }
            }
            Err(errors) => {
                for err in errors {
                    println!("{:?}: {}", err.kind, err.message)
                }
            }
        }
    }
}

fn parser_repl() -> Result<(), String> {
    let title = "SAP Parser";
    let version = parser::VERSION.unwrap_or("<unknown>");
    println!("{}", construct_info_message(title, version));
    println!("Enter an expression to parse, or Ctrl-C to exit");

    loop {
        let line = readline()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let result = parser::parse(&line);
        print_parse_result(result);
    }
}

fn lexer_repl() -> Result<(), String> {
    let title = "SAP Lexer";
    let version = lexer::VERSION.unwrap_or("<unknown>");
    println!("{}", construct_info_message(title, version));
    println!("Enter an expression to generate tokens with, or Ctrl-C to exit");

    loop {
        let line = readline()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let mut lexer = lexer::Lexer::new(&line);
        generate_and_print_tokens(&mut lexer);
    }
}

fn readline() -> Result<String, String> {
    write!(std::io::stdout(), ">>> ").map_err(|e| e.to_string())?;
    std::io::stdout().flush().map_err(|e| e.to_string())?;
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .map_err(|e| e.to_string())?;
    Ok(buffer)
}

fn print_parse_result(result: Result<ast::Program, Vec<shared::error::Error>>) {
    match result {
        Ok(parsed_ast) => {
            print!("\n=== Serialised AST start ===\n");
            print!(
                "{}",
                ast::ast_to_json(&parsed_ast).unwrap_or(parsed_ast.to_string())
            );
            print!("\n=== Serialised AST end ===\n");
            println!("\nParsed as:\n{}", parsed_ast);
        }
        Err(errors) => {
            for err in errors {
                println!("{:?}: {}", err.kind, err.message)
            }
        }
    }
}

fn generate_and_print_tokens(lexer: &mut Lexer) {
    loop {
        let token = lexer.next_token();
        println!("{}", token);
        if token.kind == lexer::token::TokenKind::Eof {
            break;
        }
    }
}
