//! This module contains the [`Environment`] struct, which is used to store and lookup
//! SAP variables and functions. It also contains a type alias for a shared
//! [`Environment`] reference.
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::Value;

/// The `EnvRef` type is a reference-counted, mutable reference to an [`Environment`].
///
/// The [`Rc`] type is used to allow multiple references to the same [`Environment`]. This
/// is useful when creating a tree of environments, where each environment can lookup
/// all the variables and functions of it's parent enviornment.
///
/// The [`RefCell`] type is used to allow the [`Environment`] to be mutable, even when it
/// is shared between multiple references.
pub type EnvRef = Rc<RefCell<Environment>>;

/// An [`Environment`] is a hashmap in which SAP variables and functions are stored. It
/// also contains an optional reference to the outer environment. This creates a tree of
/// environments, where each environment can lookup all the variables and functions of
/// it's parent enviornment.
#[derive(Debug, PartialEq)]
pub struct Environment {
    /// A `HashMap` containing all variables and functions in the current environment.
    members: HashMap<String, Rc<Value>>,
    /// An optional reference to the parent environment.
    outer: Option<EnvRef>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            members: HashMap::new(),
            outer: None,
        }
    }

    /// Creates a new child [`Environment`] to the given `outer` environment.
    ///
    /// Equivalent to:
    /// ```compile_fail ignore
    /// let mut env = Environment::new();
    /// env.outer = Some(Rc::clone(outer));
    /// ```
    pub fn new_enclosed_environment(outer: &EnvRef) -> Self {
        let mut env = Self::new();
        env.outer = Some(Rc::clone(outer));
        return env;
    }

    /// Looks up the given `name` in the current environment and all parent environments.
    ///
    /// If the name is found, the function returns a reference to the `Value` associated
    /// with the name. If the name is not found, the function returns `None`.
    pub fn lookup(&self, name: &str) -> Option<Rc<Value>> {
        match self.members.get(name) {
            Some(obj) => Some(Rc::clone(obj)),
            None => {
                if let Some(outer) = &self.outer {
                    return outer.borrow().lookup(name);
                } else {
                    return None;
                }
            }
        }
    }

    /// Stores the given `name` and `value` in the current environment.
    pub fn store(&mut self, name: String, value: Rc<Value>) {
        self.members.insert(name, value);
    }
}

/// Implements the `Display` trait for [`Environment`]. This allows an [`Environment`] to
/// be printed in a human-readable format.
impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatted_members = self
            .members
            .iter()
            .map(|(key, value)| format!("    <{}> {} = {:?}", (**value).variant_name(), key, value))
            .collect::<Vec<String>>();
        formatted_members.sort();

        write!(f, "== Env ====================\n")?;

        if formatted_members.len() > 0 {
            write!(f, "\n{}\n", formatted_members.join("\n"))?
        } else {
            write!(f, "\n    <no members>\n")?
        }

        write!(f, "\n===========================")
    }
}
