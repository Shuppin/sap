use core::Value;

fn eval(input: &str) -> Value {
    crate::eval(parser::parse(input).expect("parse() failed")).expect("eval() failed")
}

#[test]
fn eval_integer_literals() {
    let tests = [
        ("5", 5_i64),
        ("234", 234_i64),
        ("9223372036854775807", 9223372036854775807_i64),
    ];

    for (input, expected_output) in tests {
        match eval(input) {
            Value::Integer(n) => assert_eq!(n, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_boolean_literals() {
    let tests = [("true", true), ("false", false)];

    for (input, expected_output) in tests {
        match eval(input) {
            Value::Boolean(b) => assert_eq!(b, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_null() {
    assert_eq!(eval(""), Value::Null);
}

#[test]
fn eval_not_operations() {
    let tests = [("!true", false), ("!false", true), ("!!true", true)];

    for (input, expected_output) in tests {
        match eval(input) {
            Value::Boolean(b) => assert_eq!(b, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_negate_operations() {
    let tests = [
        ("-5", Value::Integer(-5)),
        ("-0", Value::Integer(0)),
        ("--1", Value::Integer(1)),
        ("-3.14", Value::Float(-3.14)),
        ("--1.01", Value::Float(1.01)),
        ("---1.00001", Value::Float(-1.00001)),
    ];

    for (input, expected_output) in tests {
        let eval = eval(input);
        assert_eq!(eval, expected_output)
    }
}

#[test]
fn eval_infix_integer_expressions() {
    let tests = [
        ("5", 5_i64),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5+5 + 5-10", 10),
        ("2*2*2*2*2", 32),
        ("-50 + 100 +-50", 0),
        ("5 * 2 + 10", 20),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected_output) in tests {
        match eval(input) {
            Value::Integer(n) => assert_eq!(n, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}
#[test]
fn eval_infix_comp_expressions() {
    let tests = [
        ("5 == 5", true),
        ("5 == 10", false),
        ("5 != 10", true),
        ("5 != 5", false),
        ("5 > 10", false),
        ("10 > 5", true),
        ("5 >= 10", false),
        ("10 >= 5", true),
        ("5 < 10", true),
        ("10 < 5", false),
        ("5 <= 10", true),
        ("10 <= 5", false),
    ];

    for (input, expected_output) in tests {
        match eval(input) {
            Value::Boolean(b) => assert_eq!(b, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_infix_boolean_expressions() {
    let tests = [
        ("true && true", true),
        ("true && false", false),
        ("false && true", false),
        ("false && false", false),
        ("true || true", true),
        ("true || false", true),
        ("false || true", true),
        ("false || false", false),
        ("true && true || false", true),
        ("true && (true || false)", true),
        ("(true && true) || false", true),
        ("(true && false) || true", true),
        ("(false && true) || true", true),
        ("(false && false) || true", true),
        ("(false && false) || false", false),
        ("true && true && true", true),
        ("true && true && false", false),
        ("true && false && true", false),
        ("false && true && true", false),
        ("true || true || true", true),
        ("true || true || false", true),
        ("true || false || true", true),
        ("false || true || true", true),
        ("true || false || false", true),
        ("false || true || false", true),
        ("false || false || true", true),
        ("false || false || false", false),
    ];

    for (input, expected_output) in tests {
        match eval(input) {
            Value::Boolean(b) => assert_eq!(b, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_if_else_expressions() {
    let tests = [
        ("if (true) { 10 }", Value::Integer(10)),
        ("if (false) { 10 }", Value::Null),
        ("if (1) { 10 }", Value::Integer(10)),
        ("if (1 < 2) { 10 }", Value::Integer(10)),
        ("if (1 > 2) { 10 }", Value::Null),
        ("if (1 > 2) { 10 } else { 20 }", Value::Integer(20)),
        ("if (1 < 2) { 10 } else { 20 }", Value::Integer(10)),
    ];

    for (input, expected_output) in tests {
        let eval = eval(input);
        assert_eq!(eval, expected_output)
    }
}
