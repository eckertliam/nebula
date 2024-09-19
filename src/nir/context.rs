// the context contains all declarations of types and functions

use std::collections::HashMap;
use crate::nir::function::Function;
use crate::nir::ty::Type;

pub struct Context {
    pub parent: Option<Box<Context>>,
    pub types: HashMap<String, Type>,
    pub functions: HashMap<String, Function>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            parent: None,
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn from_parent(parent: Context) -> Context {
        Context {
            parent: Some(Box::new(parent)),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn push_type(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    pub fn get_type(&self, name: &str) -> Option<Type> {
        if let Some(ty) = self.types.get(name) {
            Some(ty).cloned()
        } else if let Some(parent) = &self.parent {
            parent.get_type(name)
        } else {
            None
        }
    }
}