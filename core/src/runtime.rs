use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::Value;

pub type EnvRef = Rc<RefCell<Environment>>;

#[derive(Debug, PartialEq)]
pub struct Environment {
    members: HashMap<String, Rc<Value>>,
    outer: Option<EnvRef>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            members: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: &EnvRef) -> Self {
        let mut env = Self::new();
        env.outer = Some(Rc::clone(outer));
        return env;
    }

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

    pub fn store(&mut self, name: String, val: Rc<Value>) {
        self.members.insert(name, val);
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let formatted_members_string = self
            .members
            .iter()
            .map(|(key, value)| format!("    <{}> {} = {:?}", (**value).variant_name(), key, value))
            .collect::<Vec<String>>()
            .join("\n");
        write!(
            f,
            "Environment:\n===========================\n\n{}\n\n===========================",
            formatted_members_string
        )
    }
}
