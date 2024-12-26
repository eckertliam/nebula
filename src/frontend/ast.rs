use std::{fmt::Display, str::FromStr};

use crate::frontend::scanner::TokenKind;

pub struct Located<T> {
    pub node: T,
    pub line: usize,
}

impl<T> Located<T> {
    pub fn new(node: T, line: usize) -> Self {
        Self { node, line }
    }
}

pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: Vec::new() }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

pub enum TypeExpr {
    Int(Located<IntType>),
    Float(Located<FloatType>),
    Bool(Located<BoolType>),
    String(Located<StringType>),
    Void(Located<VoidType>),
    Array(Located<ArrayType>),
    Function(Located<FunctionType>),
    Udt(Located<UdtType>),
}

impl TypeExpr {
    pub fn new_int(int_str: &str, line: usize) -> Self {
        Self::Int(Located::new(IntType::from_str(int_str).unwrap(), line))
    }

    pub fn new_float(float_str: &str, line: usize) -> Self {
        Self::Float(Located::new(FloatType::from_str(float_str).unwrap(), line))
    }

    pub fn new_bool(line: usize) -> Self {
        Self::Bool(Located::new(BoolType, line))
    }

    pub fn new_string(line: usize) -> Self {
        Self::String(Located::new(StringType, line))
    }

    pub fn new_void(line: usize) -> Self {
        Self::Void(Located::new(VoidType, line))
    }
}

pub enum IntType {
    I8,
    I16,
    I32,
    I64,
}

impl FromStr for IntType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "i8" => Ok(IntType::I8),
            "i16" => Ok(IntType::I16),
            "i32" => Ok(IntType::I32),
            "i64" => Ok(IntType::I64),
            _ => Err(()),
        }
    }
}

pub enum FloatType {
    F32,
    F64,
}

impl FromStr for FloatType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "f32" => Ok(FloatType::F32),
            "f64" => Ok(FloatType::F64),
            _ => Err(()),
        }
    }
}

pub struct BoolType;

pub struct StringType;

pub struct VoidType;

pub struct ArrayType {
    pub element_type: Box<TypeExpr>,
    pub size: Box<Expression>,
}

pub struct FunctionType {
    pub params: Vec<TypeExpr>,
    pub return_type: Box<TypeExpr>,
}

pub struct UdtType {
    pub name: String,
    pub type_vars: Vec<TypeExpr>,
}

pub enum Expression {
    Binary(Located<BinaryExpr>),
    Unary(Located<UnaryExpr>),
    Integer(Located<i64>),
    Float(Located<f64>),
    Identifier(Located<String>),
    Call(Located<CallExpr>),
}

impl Expression {
    pub fn new_binary(lhs: Expression, op: TokenKind, rhs: Expression, line: usize) -> Self {
        Self::Binary(Located::new(BinaryExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }, line))
    }

    pub fn new_unary(op: TokenKind, expr: Expression, line: usize) -> Self {
        Self::Unary(Located::new(UnaryExpr {
            op,
            expr: Box::new(expr),
        }, line))
    }

    pub fn new_integer(value: i64, line: usize) -> Self {
        Self::Integer(Located::new(value, line))
    }

    pub fn new_float(value: f64, line: usize) -> Self {
        Self::Float(Located::new(value, line))
    }

    pub fn new_identifier(name: String, line: usize) -> Self {
        Self::Identifier(Located::new(name, line))
    }

    pub fn new_call(callee: Expression, args: Vec<Expression>, line: usize) -> Self {
        Self::Call(Located::new(CallExpr {
            callee: Box::new(callee),
            args,
        }, line))
    }

    pub fn line(&self) -> usize {
        match self {
            Self::Binary(Located { line, .. }) => *line,
            Self::Unary(Located { line, .. }) => *line,
            Self::Integer(Located { line, .. }) => *line,
            Self::Float(Located { line, .. }) => *line,
            Self::Identifier(Located { line, .. }) => *line,
            Self::Call(Located { line, .. }) => *line,
        }
    }
}

pub enum Statement {
    ExpressionStmt(Expression),
    ConstDecl(ConstDecl),
    LetDecl(LetDecl),
    Block(Block),
}

pub struct Block {
    pub statements: Vec<Statement>,
}

pub struct ConstDecl {
    pub name: String,
    pub ty: Option<TypeExpr>,
    pub value: Expression,
}

pub struct LetDecl {
    pub name: String,
    pub ty: Option<TypeExpr>,
    pub value: Expression,
}

pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<(String, TypeExpr)>,
    pub return_ty: TypeExpr,
    pub body: Block,
}

pub struct BinaryExpr {
    pub lhs: Box<Expression>,
    pub op: TokenKind,
    pub rhs: Box<Expression>,
}

pub struct UnaryExpr {
    pub op: TokenKind,
    pub expr: Box<Expression>,
}

pub struct CallExpr {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
}