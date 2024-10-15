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

    pub fn push(&mut self, statement: Statement) {
        self.statements.push(statement);
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
        size: Option<Expression>,
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
}

impl Type {
    pub fn span(&self) -> &Span {
        match self {
            Type::Basic { span, .. } => span,
            Type::Generic { span, .. } => span,
            Type::Function { span, .. } => span,
            Type::Tuple { span, .. } => span,
            Type::Array { span, .. } => span,
            Type::Union { span, .. } => span,
            Type::Intersection { span, .. } => span,
        }
    }
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

impl Expression {
    pub fn span(&self) -> &Span {
        match self {
            Expression::Symbol { span, .. } => span,
            Expression::Int { span, .. } => span,
            Expression::Float { span, .. } => span,
            Expression::String { span, .. } => span,
            Expression::Bool { span, .. } => span,
            Expression::Array { span, .. } => span,
            Expression::Tuple { span, .. } => span,
            Expression::Call { span, .. } => span,
            Expression::Index { span, .. } => span,
            Expression::FieldAccess { span, .. } => span,
            Expression::Unary { span, .. } => span,
            Expression::Binary { span, .. } => span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub name: String,
    pub bounds: Vec<Type>,
    pub span: Span,
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
        generics: Vec<GenericParam>,
        args: Vec<(String, Type)>,
        ret: Type,
        body: Vec<Statement>,
        span: Span,
    },
    Return {
        value: Option<Expression>,
        span: Span,
    },
    TypeAlias {
        name: String,
        generics: Vec<GenericParam>,
        ty: Type,
        span: Span,
    },
    EnumDecl {
        name: String,
        generics: Vec<GenericParam>,
        variants: Vec<(String, Vec<Type>)>,
        span: Span,
    },
    StructDecl {
        name: String,
        generics: Vec<GenericParam>,
        fields: Vec<(String, Type)>,
        span: Span,
    },
    TraitDecl {
        name: String,
        // traits this trait requires
        trait_bounds: Vec<Type>,
        generics: Vec<GenericParam>,
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
        generics: Vec<GenericParam>,
        fields: Vec<Statement>,
        span: Span,
    },
    Expression {
        value: Expression,
        span: Span,
    },
}

impl Statement {
    pub fn span(&self) -> &Span {
        match self {
            Statement::Block { span, .. } => span,
            Statement::LetDecl { span, .. } => span,
            Statement::ConstDecl { span, .. } => span,
            Statement::FnDecl { span, .. } => span,
            Statement::Return { span, .. } => span,
            Statement::TypeAlias { span, .. } => span,
            Statement::EnumDecl { span, .. } => span,
            Statement::StructDecl { span, .. } => span,
            Statement::TraitDecl { span, .. } => span,
            Statement::ImplBlock { span, .. } => span,
            Statement::Expression { span, .. } => span,
        }
    }
}