use crate::parse;

fn test_to_string(input: &str, expected_result: &str) {
    assert_eq!(
        parse(input).expect("parse_program() failed").to_string(),
        expected_result
    );
}

#[test]
fn ast_interger_to_string() {
    test_to_string("5;", "5");
}

#[test]
fn ast_ident_to_string() {
    test_to_string("a;", "a");
    test_to_string("myVar;", "myVar");
    test_to_string("hello_world", "hello_world");
    test_to_string("_yes;", "_yes");
}