use std::fs::File;
use std::io::{Read, Write};
use std::time::Instant;

use clap::{Arg, Command};

// Attempt to obtain the current version of the CLI package
pub const VERSION: Option<&str> = std::option_env!("CARGO_PKG_VERSION");

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
                .value_parser(["eval", "ast", "env", "parse", "tokens", "lex"])
                .help("Sets the output type"),
        )
        .arg(
            Arg::new("timer")
                .short('t')
                .long("timer")
                .num_args(0)
                .help("Enables the timing of execution duration"),
        )
}

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
            Some("ast") | Some("parse") => parse_and_print(&contents),
            Some("tokens") | Some("lex") => lex_and_print(&contents),
            Some("env") => evaluate_and_print(&contents, None, true),
            Some("eval") | None => evaluate_and_print(&contents, None, false),
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
            Some("eval") | None => interpreter_repl(false),
            Some("env") => interpreter_repl(true),
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

fn interpreter_repl(display_env: bool) -> Result<(), String> {
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

        evaluate_and_print(line, Some(env.clone()), display_env)
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
        parse_and_print(line);
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
        lex_and_print(&line);
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

fn evaluate_and_print(input: &str, env: Option<interpreter::runtime::EnvRef>, display_env: bool) {
    let env = env.unwrap_or(interpreter::create_env());
    match parser::parse(&input) {
        Ok(parsed_ast) => {
            let result = interpreter::eval_program(&env, parsed_ast);
            match result {
                Ok(evaluation) => {
                    if display_env {
                        println!("\n{}", evaluation.0.borrow());
                        match *evaluation.1 {
                            interpreter::value::Value::Null => {}
                            _ => println!("\nEvaluated as: {}", evaluation.1),
                        }
                        println!("\nEvaluated as: {}", evaluation.1)
                    } else {
                        match *evaluation.1 {
                            interpreter::value::Value::Null => {}
                            _ => println!("{}", evaluation.1),
                        }
                    }
                }
                Err(err) => println!("{:?}: {}", err.kind, err.message),
            }
        }
        Err(err) => {
            println!("{:?}: {}", err.kind, err.message)
        }
    }
}

fn parse_and_print(input: &str) {
    let result = parser::parse(input);
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
        Err(err) => {
            println!("{:?}: {}", err.kind, err.message)
        }
    }
}

fn lex_and_print(input: &str) {
    let mut lexer = lexer::Lexer::new(input);
    loop {
        let token = lexer.next_token();
        println!("{}", token);
        if token.kind == lexer::token::TokenKind::Eof {
            break;
        }
    }
}
