use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
    String,
    Void,
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Tuple(Vec<Type>),
    // A record is a map of field names to types
    Record(HashMap<String, Type>),
    // Type constructor application
    AppliedConstructor {
        constructor: Box<Type>,
        args: Vec<Type>,
    },
    // a type alias
    TypeAlias {
        name: String,
        ty: Box<Type>,
    },
    // a type variable
    TypeVar(String),
}

const ALPHA: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
];

const ALPHA_LEN: u8 = ALPHA.len() as u8;
const NUM_MAX: u8 = u8::MAX;

/// generates a unique type variable for variables that need type inference
#[derive(Debug, Clone, Copy)]
pub struct TypeVarGen {
    alpha_idx: u8,
    num: u8,
}

impl TypeVarGen {
    pub fn new() -> Self {
        Self {
            alpha_idx: 0,
            num: 0,
        }
    }

    pub fn next(&mut self) -> String {
        let alpha = ALPHA[self.alpha_idx as usize];
        let num = self.num;
        self.alpha_idx = (self.alpha_idx + 1) % ALPHA_LEN;
        self.num = (self.num + 1) % NUM_MAX;
        format!("{}{}", alpha, num)
    }
}
