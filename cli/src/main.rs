//! The `cli` module contains the command line interface (CLI) for the SAP programming
//! language.
//!
//! The CLI is responsible for parsing the command line arguments and options, and then
//! executing the appropriate functionality based on the provided input.
use std::cell::RefCell;
use std::fs::File;
use std::io::{Read, Write};
use std::rc::Rc;
use std::time::Instant;

use clap::{Arg, Command};
use interpreter::runtime::Environment;
use shared::{stdout, stdoutln};

// Attempt to obtain the current version of the CLI module.
pub const VERSION: Option<&str> = std::option_env!("CARGO_PKG_VERSION");

/// The `create_cli_parser` function constructs a `Command` object from the `clap` crate
/// which defines the command line arguments and options.
///
/// # Returns
///
/// A `Command` object which represents the command line arguments and options.
fn create_cli_parser() -> Command {
    // Construct the description message for the CLI, which is displayed when the `--help`
    // option is used.
    let title = "SAP Launcher";
    let cli_version = crate::VERSION.unwrap_or("<unknown>");
    let description = construct_info_message(title, cli_version);

    // Construct the CLI
    Command::new("sap")
        .name("SAP Launcher")
        .about(description)
        .arg(
            // Example: `sap ./src/main.sap`
            Arg::new("FILE").help("Sets the input file to use").index(1), // Positional
        )
        .arg(
            // The `--output` (`-o`) option is used to specify the type of output to be printed to
            // the console. The `--output` option can take one of the following values:
            //
            // * `eval` - (Default) Evaluate the input and print the result
            // * `env` - Evaluate the input and print the result, along with the environment (which
            //   is a hashmap storing variable names and their values)
            // * `ast` - Parse the input and print the Abstract Syntax Tree (AST)
            // * `parse` - alias for `ast`
            // * `lex` - Lex the input and print the tokens
            // * `tokens` - alias for `lex`
            //
            // Examples: `sap -o eval`, `sap ./src/my_program.sap --output ast`
            Arg::new("output")
                .short('o') // Allows -o
                .long("output") // Allows --output
                .value_parser(["eval", "ast", "env", "parse", "tokens", "lex"])
                .help("Sets the output type"),
        )
        .arg(
            // The `--timer` (`-t`) option is used to enable the timing of the execution duration.
            //
            // Example: `sap -t`, `sap --timer`
            Arg::new("timer")
                .short('t')
                .long("timer")
                .num_args(0)
                .help("Enables the timing of execution duration"),
        )
}

/// The `main` function is the entry point of the CLI, and is responsible for parsing the
/// command line arguments and options, and then executing the appropriate functionality
/// based on the provided input.
fn main() -> Result<(), String> {
    // Parse the command line arguments and options.
    let matches = create_cli_parser().get_matches();

    // The `start` variable is used to store the start time of the execution, if the `--timer`
    // option was used, if not, it will be `None`.
    let mut start: Option<Instant> = None;

    // Access the FILE argument
    if let Some(file_path) = matches.get_one::<String>("FILE") {
        // If the `--timer` option was used, then store the start time of the execution.
        if timer_argument_was_passed(&matches) {
            start = Some(Instant::now());
        }

        // Open the file and read its contents
        let mut file = File::open(file_path).map_err(|e| e.to_string())?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| e.to_string())?;

        // Access the --output/-o argument, and execute the appropriate functionality.
        match matches.get_one::<String>("output").map(|s| s.as_str()) {
            Some("ast") | Some("parse") => parse_and_print(&contents),
            Some("tokens") | Some("lex") => lex_and_print(&contents),
            Some("env") => evaluate_and_print(&contents, None, true),
            Some("eval") | None => evaluate_and_print(&contents, None, false),
            Some(_) => return Err("Invalid output option provided".to_string()),
        };

        // If the start variable is not `None`, then calculate the time elapsed and print it.
        if let Some(start) = start {
            let time_elapsed = Instant::now() - start;
            stdoutln!("Execution finished in {}ms", time_elapsed.as_millis())
        }

        return Ok(()); // Exit the program
    } else {
        // If the FILE argument was not provided, then enter the (Read Eval Print Loop) REPL mode.

        // Set the Ctrl-C handler to exit the program.
        // This overrides the default behaviour of Ctrl-C, instead of terminating the program, it
        // will print "Bye!" and then exit the program with a success state.
        ctrlc::set_handler(move || {
            stdoutln!("\nBye!");
            std::process::exit(0);
        })
        .expect("Error setting Ctrl-C handler");

        // Access the --output/-o argument, and execute the appropriate functionality.
        match matches.get_one::<String>("output").map(|s| s.as_str()) {
            Some("ast") | Some("parse") => parser_repl(),
            Some("tokens") | Some("lex") => lexer_repl(),
            Some("eval") | None => interpreter_repl(false),
            Some("env") => interpreter_repl(true),
            _ => Err("Invalid output option provided".to_string()),
        }
    }
}

/// Shorthand function to check if the `--timer` option was passed.
///
/// # Arguments
///
/// * `matches` - The `clap::ArgMatches` object containing the parsed command line
///   arguments and options.
///
/// # Returns
///
/// A boolean value indicating whether the `--timer` option was passed
fn timer_argument_was_passed(matches: &clap::ArgMatches) -> bool {
    *matches.get_one::<bool>("timer").unwrap_or(&false)
}

/// Function to construct the information message for various parts of the CLI, keeping a
/// consistent format.
///
/// # Arguments
///
/// * `title` - The title of the program
/// * `version` - The version of the program
///
/// # Returns
///
/// A string containing the constructed information message
fn construct_info_message(title: &str, version: &str) -> String {
    // Retrieve information about the operating system.
    let info = os_info::get();

    // Construct the information message, which includes the title, version, operating system
    // type, version, and bitness (64/32 bit).
    return format!(
        "{} v{} for {} {} [{}]",
        title,
        version,
        info.os_type(),
        info.version(),
        info.bitness(),
    );
}

/// The `interpreter_repl` function is responsible for the Read Eval Print Loop (REPL)
/// mode of the interpreter. The REPL mode allows the user to enter an expression to
/// evaluate, and then the interpreter will evaluate the expression and print the result.
///
/// # Arguments
///
/// * `display_env` - Whether to display the environment
///
/// # Returns
///
/// An `Ok` variant of `Result` if the function succeeds, otherwise a `String` containing
/// an error message.
fn interpreter_repl(display_env: bool) -> Result<(), String> {
    // Print the welcome message for the interpreter.
    let title = "SAP Interpreter";
    let version = interpreter::VERSION.unwrap_or("<unknown>");
    stdoutln!("{}", construct_info_message(title, version));
    stdoutln!("Enter an expression to evaluate, or Ctrl-C to exit");

    // Create the global environment (variable store).
    let env = interpreter::create_env();

    // Begin the REPL loop.
    loop {
        // Read the input from the user, and sanitise it.
        let line = readline()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Evaluate the input and print the result.
        evaluate_and_print(line, Some(env.clone()), display_env)
    }
}

/// The `parser_repl` function is responsible for the Read Eval Print Loop (REPL)
/// mode of the parser. The REPL mode allows the user to enter an expression to parse,
/// and then the parser will parse the expression and print the syntax tree.
///
/// # Returns
///
/// An `Ok` variant of `Result` if the function succeeds, otherwise a `String` containing
/// an error message.
fn parser_repl() -> Result<(), String> {
    // Print the welcome message for the parser.
    let title = "SAP Parser";
    let version = parser::VERSION.unwrap_or("<unknown>");
    stdoutln!("{}", construct_info_message(title, version));
    stdoutln!("Enter an expression to parse, or Ctrl-C to exit");

    // Begin the REPL loop.
    loop {
        // Read the input from the user, and sanitise it.
        let line = readline()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Parse the input and print the result.
        parse_and_print(line);
    }
}

/// The `lexer_repl` function is responsible for the Read Eval Print Loop (REPL)
/// mode of the lexer. The REPL mode allows the user to enter an expression to lex,
/// and then the lexer will lex the expression and print the generated tokens.
///
/// # Returns
///
/// An `Ok` variant of `Result` if the function succeeds, otherwise a `String` containing
/// an error message.
fn lexer_repl() -> Result<(), String> {
    // Print the welcome message for the lexer.
    let title = "SAP Lexer";
    let version = lexer::VERSION.unwrap_or("<unknown>");
    stdoutln!("{}", construct_info_message(title, version));
    stdoutln!("Enter an expression to generate tokens with, or Ctrl-C to exit");

    // Begin the REPL loop.
    loop {
        // Read the input from the user, and sanitise it.
        let line = readline()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Lex the input and print the result.
        lex_and_print(&line);
    }
}

/// The `readline` function is responsible for reading a line of input from the user.
///
/// # Returns
///
/// A `String` containing the input from the user if the function succeeds, otherwise a
/// `String` containing an error message.
fn readline() -> Result<String, String> {
    // Write a prompt (">>> ") to the standard output (usually your terminal or console).
    // `write!` is a macro that formats and writes data to a given output.
    // If an error occurs during writing, it converts the error to a String and returns it as
    // an Err variant of Result.
    write!(std::io::stdout(), ">>> ").map_err(|e| e.to_string())?;

    // Flush the output buffer of the standard output to ensure that ">>> " is displayed
    // before reading input. Without this, the output might not appear until after the
    // input is entered. If flushing fails, convert the error to a String and return it as
    // an Err.
    std::io::stdout().flush().map_err(|e| e.to_string())?;

    // Declare a mutable variable `buffer` of type String to hold the input from the user.
    let mut buffer = String::new();

    // Read a line from the standard input (`std::io::stdin()`) and append it to `buffer`.
    // If an error occurs during reading, convert the error to a String and return it as an
    // Err. `&mut buffer` is a mutable reference to `buffer`, allowing `read_line` to
    // modify its contents.
    std::io::stdin()
        .read_line(&mut buffer)
        .map_err(|e| e.to_string())?;

    // If everything succeeds, return the input as an Ok variant of Result.
    // `buffer` contains the line read from the input, including a newline character at the
    // end.
    Ok(buffer)
}

/// Helper function to evaluate and print the result of the input.
///
/// # Arguments
///
/// * `input` - The input to evaluate
/// * `env` - The environment to use for evaluation
/// * `display_env` - Whether to display the environment
fn evaluate_and_print(input: &str, env: Option<interpreter::runtime::EnvRef>, display_env: bool) {
    // If the environment is not provided, then create a new environment.
    let env = env.unwrap_or(interpreter::create_env());

    // First, parse the input, then evaluate the parsed AST.
    match parser::parse(input) {
        Ok(parsed_ast) => match interpreter::eval_program(&env, parsed_ast) {
            Ok(evaluation) => print_evaluation(&evaluation, display_env),
            Err(err) => print_error(&err),
        },
        Err(err) => print_error(&err),
    }
}

/// Helper function to print the evaluation result.
///
/// # Arguments
///
/// * `evaluation` - The result of calling `eval_program`
/// * `display_env` - Whether to display the environment
fn print_evaluation(
    evaluation: &(Rc<RefCell<Environment>>, Rc<interpreter::value::Value>),
    display_env: bool,
) {
    if display_env {
        // Display the environment.
        stdoutln!("\n{}", evaluation.0.borrow());
        // On a new line, display the evaluation result.
        match *evaluation.1 {
            interpreter::value::Value::Null => {}
            _ => stdoutln!("\nEvaluated as: {}", evaluation.1),
        }
    } else {
        // Seperate logic for displaying values if we don't want to display the environment.
        match *evaluation.1 {
            interpreter::value::Value::Null => {}
            _ => stdoutln!("{}", evaluation.1),
        }
    }
}

/// Helper function to parse and print the input.
///
/// # Arguments
///
/// * `input` - The input to parse
fn parse_and_print(input: &str) {
    let result = parser::parse(input);
    match result {
        Ok(parsed_ast) => {
            // Here, we serialise (convert to JSON) the AST before printing it. This improves the
            // readability of the AST, and makes it easier to follow.
            stdout!("\n=== Serialised AST start ===\n");
            stdout!(
                "{}",
                ast::ast_to_json(&parsed_ast).unwrap_or(parsed_ast.to_string())
            );
            stdout!("\n=== Serialised AST end ===\n");

            // Printing the AST directly like this converts it back into a form which is almost
            // identical to the original input, but with parentheses around expressions and uniform
            // whitespace.
            stdout!("\nParsed as:\n{}", parsed_ast);
        }
        Err(err) => print_error(&err),
    }
}

/// Helper function to lex and print the input.
fn lex_and_print(input: &str) {
    let mut lexer = lexer::Lexer::new(input);
    // Loop through the tokens and print them.
    loop {
        let token = lexer.next_token();
        stdoutln!("{}", token);
        // If the token is an end of file (EOF) token, then we have reached the end of the input.
        if token.kind == lexer::token::TokenKind::Eof {
            break;
        }
    }
}

// Helper function to print errors.
fn print_error(err: &shared::error::Error) {
    stdoutln!("{:?}: {}", err.kind, err.message);
}
