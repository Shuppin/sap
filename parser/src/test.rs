use ast::expression::Expression;
use ast::literal::Literal;
use ast::statement::Statement;
use ast::Program;
use lexer::token::TokenKind;
use shared::span::Span;

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

        assert_eq!(program.to_string(), stringified_output);
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
        ("3 + 4; -5 * 5", "(3 + 4); ((-5) * 5)"),
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
        ("a || b && c", "(a || (b && c))"),
        ("a && b || c", "((a && b) || c)"),
        ("a && b == c", "(a && (b == c))"),
        ("a == b && c", "((a == b) && c)"),
        ("a && b + c", "(a && (b + c))"),
        ("a + b && c", "((a + b) && c)"),
        (
            "6 || 5 || 7 && 6 || 9 && 3",
            "(((6 || 5) || (7 && 6)) || (9 && 3))",
        ),
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

#[test]
fn parse_if_else_expression() {
    let tests = [
        ("if x < y { x } else { y }", "if (x < y) { x } else { y }"),
        (
            "if x > y { x } else { if x < z { z } else { y } }",
            "if (x > y) { x } else { if (x < z) { z } else { y } }",
        ),
        (
            "if x == y { if y == z { x } else { z } } else { if x > z { x } else { z } }",
            "if (x == y) { if (y == z) { x } else { z } } else { if (x > z) { x } else { z } }",
        ),
        ("if x !=y{}else{}", "if (x != y) {} else {}"),
        ("if x < y { x }", "if (x < y) { x }"),
        ("if x > y { x }", "if (x > y) { x }"),
        ("if x == y { x }", "if (x == y) { x }"),
        ("if x != y { x }", "if (x != y) { x }"),
        ("if true { x }", "if true { x }"),
        ("if false { x }", "if false { x }"),
    ];
    validate_parse_to_string(&tests);
}

#[test]
fn parse_fn_decl_expression() {
    let tests = [
        ("fn() {};", "fn () {}"),
        ("fn(x) {};", "fn (x) {}"),
        ("fn(x, y, z) { x };", "fn (x, y, z) { x }"),
    ];
    validate_parse_to_string(&tests);
}

#[test]
fn parse_complex_expression() {
    let tests = [
        (
            "a + b * c == d - e / f && g || h[1] + i(2, 3) * j[4]",
            "((((a + (b * c)) == (d - (e / f))) && g) || (h[1] + (i(2, 3) * j[4])))",
        ),
        (
            "a + b * c == d - e / f && g || h[1] + i(2, 3) * j[4] && k || l + m * n == o - p / q && r || s[5] + t(6, 7) * u[8]",
            "((((((a + (b * c)) == (d - (e / f))) && g) || ((h[1] + (i(2, 3) * j[4])) && k)) || (((l + (m * n)) == (o - (p / q))) && r)) || (s[5] + (t(6, 7) * u[8])))",
        ),
        (
            "a + b * c == d - e / f && g || h[1] + i(2, 3) * j[4] && k || l + m * n == o - p / q && r || s[5] + t(6, 7) * u[8] && v || w + x * y == z - aa / bb && cc || dd[9] + ee(10, 11) * ff[12] && gg || hh[13] + ii(14, 15) * jj[16]",
            "(((((((((a + (b * c)) == (d - (e / f))) && g) || ((h[1] + (i(2, 3) * j[4])) && k)) || (((l + (m * n)) == (o - (p / q))) && r)) || ((s[5] + (t(6, 7) * u[8])) && v)) || (((w + (x * y)) == (z - (aa / bb))) && cc)) || ((dd[9] + (ee(10, 11) * ff[12])) && gg)) || (hh[13] + (ii(14, 15) * jj[16])))",
        ),
    ];

    validate_parse_to_string(&tests);
}

#[test]
fn parse_complex_program() {
    let tests = [
        (
            r#"let fizzbuzz = fn(n) {
            if n % 3 == 0 && n % 5 != 0 {
                print("Fizz")
            } else {
                if n % 5 == 0 && n % 3 != 0 {
                    print("Buzz");
                } else {
                    if n % 5 == 0 && n % 3 == 0 {
                        print("FizzBuzz");
                    } else {
                        print(n);
                    };
                };
            };
        };"#,
            r#"let fizzbuzz = fn (n) { if (((n % 3) == 0) && ((n % 5) != 0)) { print("Fizz") } else { if (((n % 5) == 0) && ((n % 3) != 0)) { print("Buzz") } else { if (((n % 5) == 0) && ((n % 3) == 0)) { print("FizzBuzz") } else { print(n) } } } }"#,
        ),
        (
            r#"let hello_world = "Hello, world";
let x = (true||false) && 1==5;
let y = 2.5;
1 + 2;
let ADD = fn (a, b) {
    return a + b;
};"#,
            r#"let hello_world = "Hello, world"; let x = ((true || false) && (1 == 5)); let y = 2.5; (1 + 2); let ADD = fn (a, b) { return (a + b) }"#,
        ),
    ];

    validate_parse_to_string(&tests);
}
