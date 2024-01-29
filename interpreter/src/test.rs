use core::Value;

fn eval(input: &str) -> Value {
    crate::eval(parser::parse(input).expect("parse() failed"))
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
fn eval_prefix_operations() {
    let tests = [
        ("!true", false),
        ("!false", true),
        ("!!true", true),
        ("!5", false),
        ("!0", true),
        ("!!0", false),
    ];

    for (input, expected_output) in tests {
        match eval(input) {
            Value::Boolean(b) => assert_eq!(b, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}
