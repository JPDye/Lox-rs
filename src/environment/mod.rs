use crate::errors::RuntimeError;
use crate::tokens::{Kind, Token};
use crate::value::Value;

use crate::errors::RuntimeError::UndefinedVariable;
use std::collections::HashMap;

pub struct Environments {
    envs: Vec<Environment>,
}

impl Environments {
    pub fn new() -> Self {
        Self {
            envs: vec![Environment::new()],
        }
    }

    pub fn add_env(&mut self) {
        self.envs.push(Environment::new());
    }

    pub fn pop_env(&mut self) {
        self.envs.pop();
    }

    /// Define variable within outermost environment.
    pub fn define(&mut self, ident: Token, value: Value) {
        match ident.kind {
            Kind::Identifier(i) => self.envs.last_mut().unwrap().define(i, value),
            _ => unreachable!(),
        };
    }

    /// Retrieve variable starting from outermost environment, working inwards.
    pub fn get(&self, ident: Token) -> Result<Value, RuntimeError> {
        match ident.kind {
            Kind::Identifier(ref i) => {
                for env in self.envs.iter().rev() {
                    if let Some(value) = env.get(i) {
                        return Ok(value);
                    }
                }

                Err(UndefinedVariable { received: ident })
            }

            _ => unreachable!(),
        }
    }

    /// Assign a variable, searching for binding from the outermost environment and working inwards.
    pub fn assign(&mut self, ident: Token, value: Value) -> Result<Value, RuntimeError> {
        match ident.kind {
            Kind::Identifier(ref i) => {
                for env in self.envs.iter_mut().rev() {
                    if let Some(out) = env.assign(i, &value) {
                        return Ok(out);
                    }
                }

                Err(UndefinedVariable { received: ident })
            }

            _ => unreachable!(),
        }
    }
}

struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn get(&self, ident: &str) -> Option<Value> {
        self.values.get(ident).cloned()
    }

    fn assign(&mut self, ident: &str, value: &Value) -> Option<Value> {
        if self.values.contains_key(ident) {
            return self.values.insert(ident.to_string(), value.clone());
        }

        None
    }
}
