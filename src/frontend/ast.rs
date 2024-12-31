use std::str::FromStr;

use crate::frontend::scanner::TokenKind;

#[derive(Debug, PartialEq)]
pub struct Located<T> {
    pub node: T,
    pub line: usize,
}

impl<T> Located<T> {
    pub fn new(node: T, line: usize) -> Self {
        Self { node, line }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Located<Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: Vec::new() }
    }

    pub fn add_statement(&mut self, statement: Located<Statement>) {
        self.statements.push(statement);
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
    Int(IntType),
    Float(FloatType),
    Bool,
    Char,
    String,
    Void,
    Array(ArrayType),
    Tuple(TupleType),
    Function(FunctionType),
    Udt(UdtType),
}

impl TypeExpr {
    pub fn new_i8(line: usize) -> Located<Self> {
        Located::new(Self::Int(IntType::I8), line)
    }

    pub fn new_i16(line: usize) -> Located<Self> {
        Located::new(Self::Int(IntType::I16), line)
    }

    pub fn new_i32(line: usize) -> Located<Self> {
        Located::new(Self::Int(IntType::I32), line)
    }

    pub fn new_i64(line: usize) -> Located<Self> {
        Located::new(Self::Int(IntType::I64), line)
    }

    pub fn new_u8(line: usize) -> Located<Self> {
        Located::new(Self::Int(IntType::U8), line)
    }

    pub fn new_u16(line: usize) -> Located<Self> {
        Located::new(Self::Int(IntType::U16), line)
    }

    pub fn new_u32(line: usize) -> Located<Self> {
        Located::new(Self::Int(IntType::U32), line)
    }

    pub fn new_u64(line: usize) -> Located<Self> {
        Located::new(Self::Int(IntType::U64), line)
    }

    pub fn new_f32(line: usize) -> Located<Self> {
        Located::new(Self::Float(FloatType::F32), line)
    }

    pub fn new_f64(line: usize) -> Located<Self> {
        Located::new(Self::Float(FloatType::F64), line)
    }

    pub fn new_bool(line: usize) -> Located<Self> {
        Located::new(Self::Bool, line)
    }

    pub fn new_char(line: usize) -> Located<Self> {
        Located::new(Self::Char, line)
    }

    pub fn new_string(line: usize) -> Located<Self> {
        Located::new(Self::String, line)
    }

    pub fn new_void(line: usize) -> Located<Self> {
        Located::new(Self::Void, line)
    }

    pub fn new_function(params: Vec<TypeExpr>, return_type: TypeExpr, line: usize) -> Located<Self> {
        Located::new(Self::Function(FunctionType { 
            params, 
            return_type: Box::new(return_type) 
        }), line)
    }

    pub fn new_tuple(elements: Vec<TypeExpr>, line: usize) -> Located<Self> {
        Located::new(Self::Tuple(TupleType { elements }), line)
    }

    pub fn new_array(element_type: TypeExpr, size: Expression, line: usize) -> Located<Self> {
        Located::new(Self::Array(ArrayType { 
            element_type: Box::new(element_type), 
            size: Box::new(size) 
        }), line)
    }

    pub fn new_udt(name: String, type_vars: Vec<TypeExpr>, line: usize) -> Located<Self> {
        Located::new(Self::Udt(UdtType { name, type_vars }), line)
    }
}

#[derive(Debug, PartialEq)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, PartialEq)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Debug, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<TypeExpr>,
    pub size: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct TupleType {
    pub elements: Vec<TypeExpr>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionType {
    pub params: Vec<TypeExpr>,
    pub return_type: Box<TypeExpr>,
}

#[derive(Debug, PartialEq)]
pub struct UdtType {
    pub name: String,
    pub type_vars: Vec<TypeExpr>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Integer(i64),
    UnsignedInteger(u64),
    Char(char),
    String(String),
    Float(f64),
    Bool(bool),
    Identifier(String),
    Call(CallExpr),
}

impl Expression {
    pub fn new_binary(lhs: Expression, op: TokenKind, rhs: Expression, line: usize) -> Located<Self> {
        Located::new(Self::Binary(BinaryExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }), line)
    }

    pub fn new_unary(op: TokenKind, expr: Expression, line: usize) -> Located<Self> {
        Located::new(Self::Unary(UnaryExpr {
            op,
            expr: Box::new(expr),
        }), line)
    }

    pub fn new_integer(value: i64, line: usize) -> Located<Self> {
        Located::new(Self::Integer(value), line)
    }

    pub fn new_unsigned_integer(value: u64, line: usize) -> Located<Self> {
        Located::new(Self::UnsignedInteger(value), line)
    }

    pub fn new_char(value: char, line: usize) -> Located<Self> {
        Located::new(Self::Char(value), line)
    }

    pub fn new_string(value: String, line: usize) -> Located<Self> {
        Located::new(Self::String(value), line)
    }

    pub fn new_float(value: f64, line: usize) -> Located<Self> {
        Located::new(Self::Float(value), line)
    }

    pub fn new_bool(value: bool, line: usize) -> Located<Self> {
        Located::new(Self::Bool(value), line)
    }

    pub fn new_identifier(name: String, line: usize) -> Located<Self> {
        Located::new(Self::Identifier(name), line)
    }

    pub fn new_call(callee: Expression, args: Vec<Expression>, line: usize) -> Located<Self> {
        Located::new(Self::Call(CallExpr {
            callee: Box::new(callee),
            args,
        }), line)
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    ExpressionStmt(Expression),
    ConstDecl(ConstDecl),
    LetDecl(LetDecl),
    Block(Block),
    FunctionDecl(FunctionDecl),
    ReturnStmt(Option<Expression>),
}

impl Statement {
    pub fn new_expression_stmt(expr: Expression, line: usize) -> Located<Self> {
        Located::new(Self::ExpressionStmt(expr), line)
    }

    pub fn new_const_decl(name: String, ty: Option<TypeExpr>, value: Expression, line: usize) -> Located<Self> {
        Located::new(Self::ConstDecl(ConstDecl { name, ty, value }), line)
    }

    pub fn new_let_decl(name: String, ty: Option<TypeExpr>, value: Expression, line: usize) -> Located<Self> {
        Located::new(Self::LetDecl(LetDecl { name, ty, value }), line)
    }

    pub fn new_block(block: Block, line: usize) -> Located<Self> {
        Located::new(Self::Block(block), line)
    }

    pub fn new_function_decl(name: String, params: Vec<(String, TypeExpr)>, return_ty: Option<TypeExpr>, body: Block, line: usize) -> Located<Self> {
        Located::new(Self::FunctionDecl(FunctionDecl { name, params, return_ty, body }), line)
    }

    pub fn new_return_stmt(expr: Option<Expression>, line: usize) -> Located<Self> {
        Located::new(Self::ReturnStmt(expr), line)
    }
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Located<Statement>>,
}

#[derive(Debug, PartialEq)]
pub struct ConstDecl {
    pub name: String,
    pub ty: Option<TypeExpr>,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct LetDecl {
    pub name: String,
    pub ty: Option<TypeExpr>,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<(String, TypeExpr)>,
    pub return_ty: Option<TypeExpr>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expression>,
    pub op: TokenKind,
    pub rhs: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: TokenKind,
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
}