use presap_ast::{statement::Statement, Program};
use presap_lexer::Lexer;

use crate::{ParseError, Parser};

fn parse(input: &str) -> Result<Program, Vec<ParseError>> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    parser.parse_program()
}

fn test_program_length(program: &Program, length: usize) {
    assert_eq!(
        program.statements.len(),
        length,
        "expected {} program.statements, got {}",
        length,
        program.statements.len()
    )
}

#[test]
fn parse_let_statements() {
    // Initialise conditions
    let input = "let x = 5;
let y = 10;
let foobar = 838383;";

    let expected_identifiers = vec!["x", "y", "foobar"];

    // Perform tests
    let program = parse(input).expect("parse_program() failed");

    test_program_length(&program, 3);

    for (i, ident) in expected_identifiers.iter().enumerate() {
        let stmt = &program.statements[i];
        test_let_statement(stmt, ident);
    }
}

fn test_let_statement(stmt: &Statement, expected_identifier: &str) {
    match stmt {
        Statement::Let(let_stmt) => {
            let err_msg = format!(
                "expected Statement::Let with name '{}', got '{}'",
                expected_identifier, let_stmt.ident.name
            );

            assert_eq!(expected_identifier, let_stmt.ident.name, "{}", err_msg)
        }
        _ => panic!("expected Statement::Let, got '{:?}'", stmt),
    }
}
