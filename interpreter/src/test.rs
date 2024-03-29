use std::cell::RefCell;
use std::rc::Rc;

use shared::error::{Error, ErrorKind};

use crate::runtime::Environment;
use crate::{eval_program, Value};

fn eval(input: &str) -> Rc<Value> {
    let env = Rc::new(RefCell::new(Environment::new()));
    eval_program(&env, parser::parse(input).expect("parse() failed"))
        .expect("eval() failed")
        .1
}

#[test]
fn eval_integer_literals() {
    let tests = [
        ("5", 5_i64),
        ("234", 234_i64),
        ("9223372036854775807", 9223372036854775807_i64),
    ];

    for (input, expected_output) in tests {
        match *eval(input) {
            Value::Integer(n) => assert_eq!(n, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_boolean_literals() {
    let tests = [("true", true), ("false", false)];

    for (input, expected_output) in tests {
        match *eval(input) {
            Value::Boolean(b) => assert_eq!(b, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_null() {
    assert_eq!(*eval(""), Value::Null);
}

#[test]
fn eval_not_operations() {
    let tests = [
        ("not true", false),
        ("not false", true),
        ("not not true", true),
    ];

    for (input, expected_output) in tests {
        match *eval(input) {
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
        assert_eq!(*eval, expected_output)
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
        match *eval(input) {
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
        match *eval(input) {
            Value::Boolean(b) => assert_eq!(b, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_infix_boolean_expressions() {
    let tests = [
        ("true and true", true),
        ("true and false", false),
        ("false and true", false),
        ("false and false", false),
        ("true or true", true),
        ("true or false", true),
        ("false or true", true),
        ("false or false", false),
        ("true and true or false", true),
        ("true and (true or false)", true),
        ("(true and true) or false", true),
        ("(true and false) or true", true),
        ("(false and true) or true", true),
        ("(false and false) or true", true),
        ("(false and false) or false", false),
        ("true and true and true", true),
        ("true and true and false", false),
        ("true and false and true", false),
        ("false and true and true", false),
        ("true or true or true", true),
        ("true or true or false", true),
        ("true or false or true", true),
        ("false or true or true", true),
        ("true or false or false", true),
        ("false or true or false", true),
        ("false or false or true", true),
        ("false or false or false", false),
    ];

    for (input, expected_output) in tests {
        match *eval(input) {
            Value::Boolean(b) => assert_eq!(b, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_if_else_expressions() {
    let tests = [
        ("if (true) then 10 end", Value::Integer(10)),
        ("if (false) then 10 end", Value::Null),
        ("if (1) then 10 end", Value::Integer(10)),
        ("if (1 < 2) then 10 end", Value::Integer(10)),
        ("if (1 > 2) then 10 end", Value::Null),
        ("if (1 > 2) then 10 otherwise 20 end", Value::Integer(20)),
        ("if (1 < 2) then 10 otherwise 20 end", Value::Integer(10)),
    ];

    for (input, expected_output) in tests {
        let eval = eval(input);
        assert_eq!(*eval, expected_output)
    }
}

#[test]
fn eval_err_return_outside_function() {
    let env = Rc::new(RefCell::new(Environment::new()));
    let err = eval_program(&env, parser::parse("return 10;").expect("parse() failed"));
    match err {
        Ok(_) => panic!("Return outside fn did not error"),
        Err(err) => assert_eq!(
            err,
            Error::new("'return' used outside of function", ErrorKind::TypeError)
        ),
    }
}

#[test]
fn eval_set_statements() {
    let tests = [
        ("set a = 5; a;", 5),
        ("set a = 5 * 5; a;", 25),
        ("set a = 5; set b = a; b;", 5),
        ("set a = 5; set b = a; set c = a + b + 5; c;", 15),
    ];

    for (input, expected_output) in tests {
        match *eval(input) {
            Value::Integer(n) => assert_eq!(n, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_function_declaration() {
    let input = "defineFunction abc(x) return x + 2 end; abc";
    let expected_body = "return (x + 2)";

    let value_rc = eval(input);
    let value = value_rc.as_ref();

    match value {
        Value::Function(func) => {
            if func.parameters.len() != 1 {
                panic!(
                    "Function has incorrect number of parameters, got {}",
                    func.parameters.len()
                )
            }

            if func.parameters.first().unwrap() != "x" {
                panic!(
                    "Function parameter is not the expected 'x', got {}",
                    func.parameters.first().unwrap().to_string()
                )
            }

            if func.body.len() != 1 {
                panic!(
                    "Function body has incorrect number of statements, got {}",
                    func.body.len()
                )
            }

            if func.body.first().unwrap().to_string() != expected_body {
                panic!(
                    "Expected body: {}, got {:?}",
                    expected_body,
                    func.body.first().unwrap().to_string()
                )
            }
        }
        _ => panic!("Unexpected value"),
    }
}

// Ok so like this is meant to be "eval_function_call" but I wrote it like this ok so cry
// about it.
#[test]
fn eval_function_cool() {
    let tests = [
        ("defineFunction identity(x) return x end; identity(5);", 5),
        ("defineFunction identity(x) return x; end; identity(5);", 5),
        ("defineFunction double(x) return x * 2 end; double(5);", 10),
        ("defineFunction add(x, y) return x + y; end; add(5, 5);", 10),
        (
            "defineFunction add(x, y) return x + y; end; add(5 + 5, add(5, 5));",
            20,
        ),
    ];

    for (input, expected_output) in tests {
        match *eval(input) {
            Value::Integer(n) => assert_eq!(n, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_repeat_n_times() {
    let tests = [
        ("set x = 0; repeat 10 times set x = x+1; end; x", 10),
        ("set y = 10; repeat 10 times set y = y-1; end; y", 0),
        ("set z = 5; repeat 5 times set z = z * 2; end; z", 160),
        ("set a = 1; repeat 0 times set a = a * 10; end; a", 1),
        ("set b = 100; repeat 1 times set b = b / 2; end; b", 50),
    ];

    for (input, expected_output) in tests {
        match *eval(input) {
            Value::Integer(n) => assert_eq!(n, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_repeat_until() {
    let tests = [
        ("set x = 0; repeat until x == 10 set x = x+1; end; x", 10),
        ("set y = 10; repeat until y == 0 set y = y-1; end; y", 0),
        (
            "set z = 5; repeat until z == 160 set z = z * 2; end; z",
            160,
        ),
        ("set a = 1; repeat until a == 1 set a = a * 10; end; a", 1),
        (
            "set b = 100; repeat until b == 50 set b = b / 2; end; b",
            50,
        ),
    ];

    for (input, expected_output) in tests {
        match *eval(input) {
            Value::Integer(n) => assert_eq!(n, expected_output),
            _ => panic!("Unexpected value"),
        }
    }
}

#[test]
fn eval_repeat_forever() {
    // TODO: Might add a 'break' keyword, or could add some examples of use inside
    // functions with return statements.
}

#[test]
fn value_to_boolean() {
    assert_eq!(
        Value::Integer(1).cast_to_boolean(),
        Ok(Value::Boolean(true))
    );
    assert_eq!(
        Value::Integer(-434324).cast_to_boolean(),
        Ok(Value::Boolean(true))
    );
    assert_eq!(
        Value::Integer(0).cast_to_boolean(),
        Ok(Value::Boolean(false))
    );
    assert_eq!(
        Value::Float(0.0).cast_to_boolean(),
        Ok(Value::Boolean(false))
    );
    assert_eq!(
        Value::Float(-0.1).cast_to_boolean(),
        Ok(Value::Boolean(true))
    );
    assert_eq!(
        Value::Float(3.14).cast_to_boolean(),
        Ok(Value::Boolean(true))
    );
    assert_eq!(
        Value::Boolean(true).cast_to_boolean(),
        Ok(Value::Boolean(true))
    );
    assert_eq!(
        Value::Boolean(false).cast_to_boolean(),
        Ok(Value::Boolean(false))
    );
    match Value::Null.cast_to_boolean() {
        Ok(_) => panic!("Null.to_boolean() returned Ok"),
        Err(err) => assert_eq!(err.kind, ErrorKind::TypeError),
    }
}
