use super::span::{self, Loc, Span};

use super::tokenizer::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // A basic type like `i8` or `bool` or a user defined type
    Basic {
        symbol: String,
        span: Span,
    },
    // A generic type like `Vec<i8>` or `Ptr<str>`
    Generic {
        symbol: String,
        args: Vec<Type>,
        span: Span,
    },
    // A function type like `fn(i8) -> bool`
    Function {
        args: Vec<Type>,
        ret: Box<Type>,
        span: Span,
    },
    // A tuple type like `(i8, bool)`
    Tuple {
        types: Vec<Type>,
        span: Span,
    },
    // An array type like `[[i8; 4]; 3]` or `[i8; 4]`
    Array {
        ty: Box<Type>,
        size: Option<usize>,
        span: Span,
    },
    Union {
        types: Vec<Type>,
        span: Span,
    },
    Intersection {
        types: Vec<Type>,
        span: Span,
    },
    // A type variable like `T` or `U` with optional bounds like `T: std::Add`
    Variable {
        name: String,
        bounds: Vec<Type>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Symbol {
        symbol: String,
        span: Span,
    },
    Int {
        value: i64,
        span: Span,
    },
    Float {
        value: f64,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    Bool {
        value: bool,
        span: Span,
    },
    Array {
        elements: Vec<Expression>,
        span: Span,
    },
    Tuple {
        elements: Vec<Expression>,
        span: Span,
    },
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
        span: Span,
    },
    Index {
        owner: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },
    FieldAccess {
        owner: Box<Expression>,
        field: String,
        span: Span,
    },
    Unary {
        op: TokenKind,
        operand: Box<Expression>,
        span: Span,
    },
    Binary {
        op: TokenKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block {
        statements: Vec<Statement>,
        span: Span,
    },
    LetDecl {
        name: String,
        ty: Option<Type>,
        value: Expression,
        span: Span,
    },
    ConstDecl {
        name: String,
        ty: Option<Type>,
        value: Expression,
        span: Span,
    },
    FnDecl {
        name: String,
        generics: Vec<Type>,
        args: Vec<(String, Type)>,
        ret: Type,
        body: Vec<Statement>,
        span: Span,
    },
    TypeAlias {
        name: String,
        generics: Vec<Type>,
        ty: Type,
        span: Span,
    },
    EnumDecl {
        name: String,
        generics: Vec<Type>,
        variants: Vec<(String, Vec<Type>)>,
        span: Span,
    },
    StructDecl {
        name: String,
        generics: Vec<Type>,
        fields: Vec<(String, Type)>,
        span: Span,
    },
    TraitDecl {
        name: String,
        // traits this trait requires
        trait_bounds: Vec<Type>,
        generics: Vec<Type>,
        constants: Vec<(String, Type, Option<Expression>)>,
        // fields the trait requires implementors to have
        required: Vec<(String, Type)>,
        // fields that the trait gives to implementors
        given: Vec<Statement>,
        span: Span,
    },
    ImplBlock {
        // if this is an impl block for a trait
        _trait: Option<Type>,
        _type: Type,
        generics: Vec<Type>,
        fields: Vec<Statement>,
        span: Span,
    },
}