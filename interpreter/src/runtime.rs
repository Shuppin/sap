use core::Value;
use std::collections::HashMap;
use std::rc::Rc;

pub struct ActivationRecord {
    members: HashMap<String, Rc<Value>>,
}

impl ActivationRecord {
    pub fn new() -> Self {
        Self {
            members: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Value>> {
        // `cloned()` maps Option<&T> to Option<T> by cloning T
        self.members.get(name).cloned()
    }

    pub fn set(&mut self, name: &str, value: Rc<Value>) {
        self.members.insert(name.to_owned(), value);
    }
}

impl std::fmt::Display for ActivationRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let formatted_members_string = self
            .members
            .iter()
            .map(|(key, value)| format!("    <{}> {} = {:?}", (**value).variant_name(), key, value))
            .collect::<Vec<String>>()
            .join("\n");
        write!(
            f,
            "ACTIVATION RECORD:\n===========================\n\n{}\n\n===========================",
            formatted_members_string
        )
    }
}

pub struct CallStack {
    pub current_record: ActivationRecord,
    records: Vec<ActivationRecord>,
}

impl CallStack {
    pub fn new() -> Self {
        Self {
            current_record: ActivationRecord::new(),
            records: vec![],
        }
    }

    pub fn push_new_record(&mut self) {
        // Move the current ActivationRecord to the records stack,
        // and update the current with the new ActivationRecord.
        let current = std::mem::replace(&mut self.current_record, ActivationRecord::new());
        self.records.push(current);
    }

    pub fn discard_current_record(&mut self) {
        if let Some(ar) = self.records.pop() {
            self.current_record = ar;
        }
    }

    pub fn lookup(&self, name: &str) -> Option<Rc<Value>> {
        // First, try to get the value from the current activation record.
        if let Some(value) = self.current_record.get(name) {
            return Some(value.clone());
        }

        // If not found, search through the stack from top to bottom.
        for record in self.records.iter().rev() {
            if let Some(value) = record.get(name) {
                return Some(value.clone());
            }
        }

        // If the value is not found in any activation record, return None.
        None
    }
}
