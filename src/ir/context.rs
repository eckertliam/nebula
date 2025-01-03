use std::collections::HashMap;

use super::{Function, Type, Udt};

/// Map of types and functions
/// Created for top-level
/// Child contextes are created temporarily when type checking functions
pub struct Context<'a> {
    pub parent: Option<Box<&'a mut Context<'a>>>,
    pub types: HashMap<String, Type>,
    pub functions: HashMap<String, Function>,
    // user defined types
    pub udts: HashMap<String, Udt>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            parent: None,
            types: HashMap::new(),
            functions: HashMap::new(),
            udts: HashMap::new(),
        }
    }

    pub fn add_function(&mut self, func: Function) {
        self.functions.insert(func.name.clone(), func.clone());
        self.types.insert(func.name.clone(), func.into());
    }

    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }

    pub fn get_function_mut(&mut self, name: &str) -> Option<&mut Function> {
        self.functions.get_mut(name)
    }

    pub fn add_type(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    pub fn get_type_mut(&mut self, name: &str) -> Option<&mut Type> {
        self.types.get_mut(name)
    }

    pub fn child(&'a mut self) -> Self {
        Self {
            parent: Some(Box::new(self)),
            functions: HashMap::new(),
            types: HashMap::new(),
            udts: HashMap::new(),
        }
    }

    pub fn add_udt(&mut self, name: String, udt: Udt) {
        self.udts.insert(name, udt);
    }

    pub fn get_udt(&self, name: &str) -> Option<&Udt> {
        self.udts.get(name)
    }

    pub fn get_udt_mut(&mut self, name: &str) -> Option<&mut Udt> {
        self.udts.get_mut(name)
    }
}