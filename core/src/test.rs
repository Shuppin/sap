use crate::error::ErrorKind;
use crate::Value;

#[test]
fn core_to_boolean() {
    assert_eq!(Value::Integer(1).to_boolean(), Ok(Value::Boolean(true)));
    assert_eq!(
        Value::Integer(-434324).to_boolean(),
        Ok(Value::Boolean(true))
    );
    assert_eq!(Value::Integer(0).to_boolean(), Ok(Value::Boolean(false)));
    assert_eq!(Value::Float(0.0).to_boolean(), Ok(Value::Boolean(false)));
    assert_eq!(Value::Float(-0.1).to_boolean(), Ok(Value::Boolean(true)));
    assert_eq!(Value::Float(3.14).to_boolean(), Ok(Value::Boolean(true)));
    assert_eq!(Value::Boolean(true).to_boolean(), Ok(Value::Boolean(true)));
    assert_eq!(
        Value::Boolean(false).to_boolean(),
        Ok(Value::Boolean(false))
    );
    match Value::Null.to_boolean() {
        Ok(_) => panic!("Null.to_boolean() returned Ok"),
        Err(err) => assert_eq!(err.kind, ErrorKind::TypeError),
    }
}
