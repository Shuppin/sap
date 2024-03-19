//! This module contains the [`Value`] struct, which represents a variable or function
//! stored within an `Environment`. It also contains all the logic for manipulating
//! values (such as addition, comparison or conversion between types).
use std::fmt::{self, Debug};

use ast::statement::Statement;
use shared::err;
use shared::error::{Error, ErrorKind};

use crate::runtime::EnvRef;

/// This struct is how the interpreter stores functions. It contains the function's
/// parameters, body and a reference to the environment in which it was defined.
///
/// By keeping a reference to the environment where the function was created, the
/// interpreter can access the variables within that environment when the function is
/// called. This also enables the interpreter to create closures.
///
/// Closures are functions that retain access to the environment in which they were
/// defined. Even if that environment goes out of scope, the function holds onto that
/// environment, keeping it alive, allowing it to still access the variables within.
#[derive(PartialEq)]
pub struct Function {
    pub parameters: Vec<String>,
    // TODO: Use StatementList instead of Vec<Statement>
    pub body: Vec<Statement>,
    pub env: EnvRef,
}

/// Implementing the `Debug` trait for [`Function`] allows printing of the function's
/// parameters and the number of statements in its body.
impl Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "params: ({}), body: [{}]",
            self.parameters.join(", "),
            self.body.len()
        )
    }
}

/// The [`Value`] enum represents a data type that can be stored in an `Environment`. Each
/// type contains the data it represents (e.g. a [`Value::Integer`] contains an `i64`).
///
/// There are 6 types of values: `Integer`, `Float`, `Boolean`, `String`, `Function` and
/// `Null`.
#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Function(Function),
    /// The `Null` variant is used to represent the absence of a value. It is used to
    /// represent the result of an expression that doesn't return anything (e.g. a
    /// function call that doesn't return anything).
    Null,
}

/// This macro generates a method for the [`Value`] enum that allows for comparison
/// operations. It takes in a function name, operator symbol, and a [`std::cmp::Ordering`]
/// pattern to match against.
///
/// # Arguments
///
/// * `method_name` - The name of the method to generate.
/// * `op` - The operator symbol to use in the error message if the comparison fails.
/// * `ordering` - A pattern consisting of [`std::cmp::Ordering`] variants to match
///   against the result of the comparison.
///
/// # Expands to
///
/// ```compile_fail ignore
/// pub fn method_name(&self, other: &Self) -> Result<Self, Error> {
///    ...
/// }
/// ```
macro_rules! generate_comparison_op {
    ($method_name:ident, $op:tt, $ordering:pat) => {
        pub fn $method_name(&self, other: &Self) -> Result<Self, Error> {
            match self.compare(other) {
                Some($ordering) => Ok(Self::Boolean(true)),
                Some(_) => Ok(Self::Boolean(false)),
                None => err!(
                    ErrorKind::TypeError,
                    "invalid operation '{}' between {} and {}",
                    stringify!($op),
                    self.variant_name(),
                    other.variant_name()
                ),
            }
        }
    };
}

/// This macro generates a method for the [`Value`] enum that allows for binary arithmetic
/// operations. It takes in a function name, a `checked_op` and a symbol operator.
///
/// # Arguments
///
/// * `method_name` - The name of the method to generate.
/// * `checked_op` - The name of the method to use on integer operations. Must be one of:
///   `checked_add`, `checked_sub`, `checked_mul` or `checked_div`.
/// * `symbol_op` - The symbol operator to use on float operations. Must be a valid
///   operator for two [`f64`] values. Also used in the error message if the operation
///   fails.
/// 
/// # Expands to
/// 
/// ```compile_fail ignore
/// pub fn method_name(&self, other: &Self) -> Result<Self, Error> {
///    ...  
/// }
/// ```
macro_rules! generate_binary_arithmetic_op {
    ($method_name:ident, $checked_op:tt, $symbol_op:tt) => {
        pub fn $method_name(&self, other: &Self) -> Result<Self, Error> {

            let overflow_error = Error::new(
                "this arithmetic operation overflows",
                ErrorKind::OverflowError
            );

            match (&self, &other) {
                (Self::Integer(a), Self::Integer(b)) => {
                    Ok(Self::Integer(a.$checked_op(*b).ok_or(overflow_error)?))
                },
                (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a $symbol_op b)),
                _ => err!(
                    ErrorKind::TypeError,
                    "invalid operation '{}' between {} and {}",
                    stringify!($symbol_op),
                    self.variant_name(),
                    other.variant_name()
                ),
            }
        }
    };
}

impl Value {
    /// Returns the display name of the variant of the [`Value`] enum.
    pub fn variant_name(&self) -> &'static str {
        match &self {
            Self::Integer(_) => "Integer",
            Self::Float(_) => "Float",
            Self::Boolean(_) => "Boolean",
            Self::String(_) => "String",
            Self::Function(_) => "Function",
            Self::Null => "null",
        }
    }

    // region: type coercion

    /// This method converts any [`Value`] to a [`Value::Boolean`]. It returns an error if
    /// the conversion is not possible.
    pub fn cast_to_boolean(&self) -> Result<Self, Error> {
        match self {
            Self::Integer(i) => Ok(Self::Boolean(*i != 0)),
            Self::Float(f) => Ok(Self::Boolean(*f != 0.0)),
            // goofy ahh code, it just obtians a reference to the internal value and wraps
            // it back inside itself.
            Self::Boolean(b) => Ok(Self::Boolean(*b)),
            _ => err!(
                ErrorKind::TypeError,
                "cannot convert {} to {}",
                self.variant_name(),
                Self::Boolean(true).variant_name() // i hate this i hate this i hate this
            ),
        }
    }

    /// This method converts any [`Value`] to a [`Value::Integer`]. It returns an error if
    /// the conversion is not possible. (For now, all conversions are possible.)
    pub fn cast_to_string(&self) -> Self {
        match self {
            Self::Integer(i) => Self::String(i.to_string()),
            Self::Float(f) => Self::String(f.to_string()),
            Self::Boolean(b) => Self::String(b.to_string()),
            // I suppose type coercion *should* provide ownership of the new value, so this is okay.
            Self::String(s) => Self::String(s.clone()),
            Self::Null => Self::String("null".to_string()),
            Self::Function(_) => Self::String("<function reference>".to_string()),
        }
    }

    // endregion: type coercion

    // region: comparitive operations

    pub fn eq(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::String(a), Self::String(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Null, Self::Null) => Ok(Self::Boolean(true)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '==' between {} and {}",
                self.variant_name(),
                other.variant_name()
            ),
        }
    }

    pub fn ne(&self, other: &Self) -> Result<Self, Error> {
        match self.eq(other) {
            Ok(value) => value.not(),
            Err(mut old_err) => {
                old_err.message = format!(
                    "invalid operation '!=' between {} and {}",
                    self.variant_name(),
                    other.variant_name()
                );
                Err(old_err)
            }
        }
    }

    /// This is a helper method for the comparison operations. it returns a `partial_cmp`
    /// of the two internal values of the [`Value`] enum. Primarily used for the comparison
    /// functions defined after this function.
    fn compare(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a.partial_cmp(b),
            (Self::Float(a), Self::Float(b)) => a.partial_cmp(b),
            (Self::Boolean(a), Self::Boolean(b)) => a.partial_cmp(b),
            _ => None,
        }
    }

    // Generate the remaining comparison operations using the `generate_comparison_op` macro.
    generate_comparison_op!(lt, >, std::cmp::Ordering::Less);
    generate_comparison_op!(le, >=, std::cmp::Ordering::Less | std::cmp::Ordering::Equal);
    generate_comparison_op!(gt, <, std::cmp::Ordering::Greater);
    generate_comparison_op!(ge, <=, std::cmp::Ordering::Greater | std::cmp::Ordering::Equal);

    // endregion: comparitive operations

    // region: boolean operations

    /// This method returns the logical negation of a [`Value::Boolean`]. It returns an
    /// error for all other types.
    pub fn not(&self) -> Result<Self, Error> {
        match self {
            Self::Boolean(b) => Ok(Self::Boolean(!b)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation 'not' for type {}",
                self.variant_name(),
            ),
        }
    }

    /// This method returns the logical and of two [`Value::Boolean`]s. It returns an error
    /// for all other types.
    pub fn and(&self, other: &Self) -> Result<Self, Error> {
        match (&self, &other) {
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a && *b)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation 'and' between {} and {}",
                self.variant_name(),
                other.variant_name(),
            ),
        }
    }

    /// This method returns the logical or of two [`Value::Boolean`]s. It returns an error
    /// for all other types.
    pub fn or(&self, other: &Self) -> Result<Self, Error> {
        match (&self, &other) {
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a || *b)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation 'or' between {} and {}",
                self.variant_name(),
                other.variant_name(),
            ),
        }
    }

    // endregion: boolean operations

    // region: arithmetic operations

    // Generate the arithmetic operations for subtract, multiply and modulo (rem) using
    // the `generate_binary_arithmetic_op` macro.
    generate_binary_arithmetic_op!(sub, checked_sub, -);
    generate_binary_arithmetic_op!(mul, checked_mul, *);
    generate_binary_arithmetic_op!(rem, checked_rem, %);

    // add is implemented separately because it also includes string concatenation.

    /// This method returns the sum of two [`Value`]s. It returns an error if the operation
    /// is not possible.
    pub fn add(&self, other: &Self) -> Result<Self, Error> {
        let overflow_error = Error::new(
            "this arithmetic operation overflows",
            ErrorKind::OverflowError,
        );

        match (&self, &other) {
            (Self::Integer(a), Self::Integer(b)) => {
                Ok(Self::Integer(a.checked_add(*b).ok_or(overflow_error)?))
            }
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a + b)),
            (Self::String(a), Self::String(b)) => Ok(Self::String(format!("{}{}", a, b))),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '+' between {} and {}",
                self.variant_name(),
                other.variant_name()
            ),
        }
    }

    // div is implemented separately because it also includes division by zero checks.

    /// This method returns the result of dividing two [`Value`]s. It returns an error if
    /// the operation is not possible, or if a division by zero occurs.
    pub fn div(&self, other: &Self) -> Result<Self, Error> {
        // Check for division by zero
        #[allow(illegal_floating_point_literal_pattern)]
        if matches!(&other, Self::Integer(0) | Self::Float(0.0)) {
            return err!(ErrorKind::DivisionByZero, "cannot divide by zero");
        }

        let overflow_error = Error::new(
            "this arithmetic operation overflows",
            ErrorKind::OverflowError,
        );

        match (&self, &other) {
            (Self::Integer(a), Self::Integer(b)) => {
                Ok(Self::Integer(a.checked_div(*b).ok_or(overflow_error)?))
            }
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a / b)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '/' between {} and {}",
                self.variant_name(),
                other.variant_name()
            ),
        }
    }

    /// This method returns the negation of a [`Value`]. It returns an error if the
    /// operation is not possible.
    pub fn neg(&self) -> Result<Self, Error> {
        match self {
            Self::Integer(n) => Ok(Self::Integer(-n)),
            Self::Float(n) => Ok(Self::Float(-n)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '-' for type {}",
                self.variant_name(),
            ),
        }
    }

    // endregion: arithmetic operations
}

/// Implementing the `Display` trait for [`Value`] allows for the pretty printing of
/// values.
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{}", i),
            Self::Float(fl) => write!(f, "{}", fl),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Function(_) => write!(f, "<function reference>"),
            Self::Null => write!(f, "null"),
        }
    }
}
