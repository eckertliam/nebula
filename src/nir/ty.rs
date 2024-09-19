use std::collections::HashMap;

// the type system for the NIR intermediate representation

/// A single type in the NIR type system
#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// an array with a resolved compile time size
    Array(Box<Type>, u64),
    Struct(HashMap<String, Type>),
    Enum(HashMap<String, Vec<Type>>),
    Function(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    /// a generic variable with or without bounds
    TypeVar(String, Vec<Type>),
    /// an unresolved type that must be resolved once the entire program has been transformed
    /// if any remain, the program is invalid
    /// this is used for user defined types that might be defined later in the program
    // 0: the name of the unresolved type 1: the error message that gets displayed if the type is not resolved
    Unresolved(String, String),
    Void,
}