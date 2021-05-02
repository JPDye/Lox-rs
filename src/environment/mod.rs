use crate::errors::RuntimeError;
use crate::tokens::{Kind, Token};
use crate::value::Value;

use std::collections::HashMap;

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, ident: Token) -> Result<Value, RuntimeError> {
        match ident {
            Token {
                kind: Kind::Identifier(ref name),
                ..
            } => self
                .values
                .get(name)
                .cloned()
                .ok_or(RuntimeError::UndefinedVariable { received: ident }),

            _ => unreachable!(),
        }
    }

    pub fn assign(&mut self, ident: Token, value: Value) -> Result<Value, RuntimeError> {
        match ident.kind {
            Kind::Identifier(ref name) => {
                if self.values.contains_key(name) {
                    Ok(self.values.insert(name.clone(), value).unwrap())
                } else {
                    Err(RuntimeError::UndefinedVariable { received: ident })
                }
            }

            _ => unreachable!(),
        }
    }
}
