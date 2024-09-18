use std::collections::HashMap;

// the type system for the NIR intermediate representation

/// A single type in the NIR type system
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
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
    Struct(HashMap<String, Type>),
    Enum(HashMap<String, Vec<Type>>),
    Function(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    // a generic variable with or without bounds
    TypeVar(String, Vec<Type>),
    Void,
}