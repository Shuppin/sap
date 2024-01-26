use presap_ast::expression::Expression;
use presap_ast::literal::Literal;
use presap_ast::statement::Statement;
use presap_ast::Program;
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

fn validate_parse_to_string(tests: &[(&str, &str)]) {
    for &(input, stringified_output) in tests {
        let program = parse(input).expect("parse_program() failed");

        assert_eq!(stringified_output, program.to_string());
    }
}

#[test]
fn parse_let_statements() {
    // Initialise conditions
    let input = "let x = 5;
let y = 10;
let foo_bar = 838383;";

    let expected_identifiers = vec!["x", "y", "foo_bar"];

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
    let tests = [
        ("return 5;", 5),
        ("return 10;", 10),
        ("return 382939;", 382939),
    ];

    // Perform tests
    for (input, return_value) in tests {
        let program = parse(input).expect("parse_program() failed");

        for stmt in program.statements {
            match stmt {
                Statement::Return(ret_stmt) => match ret_stmt.value {
                    Expression::Literal(literal) => match literal {
                        Literal::Integer { value, .. } => {
                            assert_eq!(value, return_value);
                        }
                        _ => panic!("expected Literal::Integer, got '{:?}'", literal),
                    },
                    _ => panic!("expected Expression::Literal, got '{:?}'", ret_stmt.value),
                },
                _ => panic!("expected Statement::Return, got '{:?}'", stmt),
            }
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
    let tests = [
        ("!5;", TokenKind::Not, 5),
        ("-15;", TokenKind::Minus, 15),
        ("-0", TokenKind::Minus, 0),
    ];

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

#[test]
fn parse_infix_expression() {
    let tests = [
        ("5+ 5;", 5, TokenKind::Plus, 5),
        ("5 - 5;", 5, TokenKind::Minus, 5),
        ("5 * 5;", 5, TokenKind::Mult, 5),
        ("5 / 5;", 5, TokenKind::Div, 5),
        ("5 > 5;", 5, TokenKind::Gt, 5),
        ("5 < 5;", 5, TokenKind::Lt, 5),
        ("5 == 5;", 5, TokenKind::Eq, 5),
        ("5 != 5;", 5, TokenKind::NotEq, 5),
    ];

    for (input, left, operator, right) in tests {
        let program = parse(input).expect("parse_program() failed");

        let stmt = program.statements.first().unwrap();

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Binary(binary) => {
                    assert_eq!(binary.operator, operator);
                    match &*binary.left {
                        Expression::Literal(Literal::Integer { value, .. }) => {
                            assert_eq!(*value, left);
                        }
                        _ => panic!("expected Literal::Integer, got '{:?}'", binary.left),
                    }
                    match &*binary.right {
                        Expression::Literal(Literal::Integer { value, .. }) => {
                            assert_eq!(*value, right);
                        }
                        _ => panic!("expected Literal::Integer, got '{:?}'", binary.right),
                    }
                }
                _ => panic!("expected Expression::Infix, got '{:?}'", expr),
            },
            _ => panic!("expected Statement::Expression, got '{:?}'", stmt),
        }
    }
}

#[test]
fn parse_binary_expression() {
    let tests = [
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4), ((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
    ];

    validate_parse_to_string(&tests);
}

#[test]
fn parse_brace_expression() {
    let tests = [
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
    ];

    validate_parse_to_string(&tests);
}

#[test]
fn parse_fn_call() {
    let tests = [("add(1, 2 * 3, 4 + 5);", "add(1, (2 * 3), (4 + 5))")];

    validate_parse_to_string(&tests);
}

#[test]
fn parse_index_expression() {
    let tests = [("a[1]", "a[1]"), ("a[1 + 1]", "a[(1 + 1)]")];

    validate_parse_to_string(&tests);
}

#[test]
fn parse_string_expression() {
    let tests = [(r#""hello world";"#, r#""hello world""#)];

    validate_parse_to_string(&tests);
}

#[test]
fn parse_array_expression() {
    let tests = [("[]", "[]"), ("[1, 2 * 2, 3 + 3]", "[1, (2 * 2), (3 + 3)]")];

    validate_parse_to_string(&tests);
}
