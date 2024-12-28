use std::str::FromStr;

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
    Int(IntType),
    Float(FloatType),
    Bool,
    String,
    Void,
    Array(ArrayType),
    Tuple(TupleType),
    Function(FunctionType),
    Udt(UdtType),
}

impl TypeExpr {
    pub fn new_int(int_str: &str, line: usize) -> Located<Self> {
        let int_type = IntType::from_str(int_str).expect("Invalid integer type");
        Located::new(Self::Int(int_type), line)
    }

    pub fn new_float(float_str: &str, line: usize) -> Located<Self> {
        let float_type = FloatType::from_str(float_str).expect("Invalid float type");
        Located::new(Self::Float(float_type), line)
    }

    pub fn new_bool(line: usize) -> Located<Self> {
        Located::new(Self::Bool, line)
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

pub struct ArrayType {
    pub element_type: Box<TypeExpr>,
    pub size: Box<Expression>,
}

pub struct TupleType {
    pub elements: Vec<TypeExpr>,
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
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Integer(i64),
    Float(f64),
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

    pub fn new_float(value: f64, line: usize) -> Located<Self> {
        Located::new(Self::Float(value), line)
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

pub enum Statement {
    ExpressionStmt(Expression),
    ConstDecl(ConstDecl),
    LetDecl(LetDecl),
    Block(Block),
    FunctionDecl(FunctionDecl),
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

    pub fn new_function_decl(name: String, params: Vec<(String, TypeExpr)>, return_ty: TypeExpr, body: Block, line: usize) -> Located<Self> {
        Located::new(Self::FunctionDecl(FunctionDecl { name, params, return_ty, body }), line)
    }
}

pub struct Block {
    pub statements: Vec<Located<Statement>>,
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