use std::{collections::HashMap, fmt::Display};

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
    Array {
        element_type: Box<Type>,
        size: usize,
    },
    TypeVar(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::Function { params, return_type } => {
                let params_str = params.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", ");
                write!(f, "fn({}) -> {}", params_str, return_type)
            }
            Type::Tuple(types) => {
                let types_str = types.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ");
                write!(f, "({})", types_str)
            }
            Type::Array { element_type, size } => write!(f, "[{}; {}]", element_type, size),
            Type::TypeVar(name) => write!(f, "{}", name),
        }
    }
}

type FnSig<'t> = (Vec<&'t Type>, &'t Type);

pub struct TypeEnv<'t> {
    pub fn_sig: Option<FnSig<'t>>,
    parent: Option<Box<&'t TypeEnv<'t>>>,
    map: HashMap<String, Type>,
}

impl<'t> TypeEnv<'t> {
    /// creates a new type environment
    pub fn new() -> Self {
        Self {
            fn_sig: None,
            parent: None,
            map: HashMap::new(),
        }
    }

    /// creates a child environment
    pub fn child(&'t self) -> Self {
        Self {
            fn_sig: None,
            parent: Some(Box::new(self)),
            map: HashMap::new(),
        }
    }

    /// creates a child environment with a function signature
    /// this is the type environment for a function
    pub fn child_fn(&'t self, params: Vec<&'t Type>, return_type: &'t Type) -> Self {
        Self {
            fn_sig: Some((params, return_type)),
            parent: Some(Box::new(self)),
            map: HashMap::new(),
        }
    }

    pub fn get(&self, ident: &str) -> Option<&Type> {
        self.map.get(ident).or_else(|| self.parent.as_ref().and_then(|p| p.get(ident)))
    }

    pub fn insert(&mut self, ident: &str, ty: Type) {
        self.map.insert(ident.to_string(), ty);
    }
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
