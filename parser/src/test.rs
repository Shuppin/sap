use presap_ast::{expression::Expression, statement::Statement, Literal, Program};
use presap_lexer::token::{Span, TokenKind};

use crate::parse;

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

#[test]
fn parse_return_statements() {
    // Initialise conditions
    let input = "return 5;
return 10;
return 382939;";

    // Perform tests
    let program = parse(input).expect("parse_program() failed");

    test_program_length(&program, 3);

    for stmt in program.statements {
        match stmt {
            Statement::Return(_ret_stmt) => {
                // More tests may be implemented in future
                todo!("Implement return statements test")
            }
            _ => panic!("expected Statement::Return, got '{:?}'", stmt),
        }
    }
}

#[test]
fn parse_identifier_expression() {
    // Initialise conditions
    let input = "foobar";

    // Perform tests
    let program = parse(input).expect("parse_program() failed");

    test_program_length(&program, 1);

    let stmt = program.statements.first().unwrap();

    match stmt {
        Statement::Expression(expr) => match expr {
            Expression::Identifier(ident) => {
                let err_msg = format!("expected name 'foobar', got {:?}", expr);
                assert_eq!(ident.name, "foobar", "{}", err_msg);
                assert_eq!(ident.span, Span { start: 0, end: 6 })
            }
            _ => panic!("expected Expression::Identifier, got '{:?}'", expr),
        },
        _ => panic!("expected Statement::Expression, got '{:?}'", stmt),
    }
}

#[test]
fn parse_integer_literal_expression() {
    // Initialise conditions
    let input = "554";

    // Perform tests
    let program = parse(input).expect("parse_program() failed");

    test_program_length(&program, 1);

    let stmt = program.statements.first().unwrap();

    match stmt {
        Statement::Expression(expr) => match expr {
            Expression::Literal(literal) => match literal {
                Literal::Integer { value, span } => {
                    assert_eq!(*value, 554);
                    assert_eq!(*span, Span { start: 0, end: 3 })
                }
                _ => panic!("expected Literal::Integer, got '{:?}'", expr),
            },
            _ => panic!("expected Expression::Identifier, got '{:?}'", expr),
        },
        _ => panic!("expected Statement::Expression, got '{:?}'", stmt),
    }
}

#[test]
fn parse_prefix_expression() {
    let tests = vec![("!5;", TokenKind::Not, 5), ("-15;", TokenKind::Minus, 15), ("-0", TokenKind::Minus, 0)];

    for (input, operator, test_value) in tests {
        let program = parse(input).expect("parse_program() failed");

        test_program_length(&program, 1);

        let stmt = program.statements.first().unwrap();

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Unary(unary) => {
                    assert_eq!(unary.operator, operator);
                    match &*unary.operand {
                        Expression::Literal(Literal::Integer { value, .. }) => {
                            assert_eq!(*value, test_value);
                        }
                        _ => panic!("expected Literal::Integer, got '{:?}'", unary.operand),
                    }
                }
                _ => panic!("expected Expression::Unary, got '{:?}'", expr),
            },
            _ => panic!("expected Statement::Expression, got '{:?}'", stmt),
        }
    }
}
