use crate::frontend::scanner::TokenKind;

use crate::ir::Type;

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

    pub fn new_const_decl(name: String, ty: Type, value: Expression, line: usize) -> Located<Self> {
        Located::new(Self::ConstDecl(ConstDecl { name, ty, value }), line)
    }

    pub fn new_let_decl(name: String, ty: Type, value: Expression, line: usize) -> Located<Self> {
        Located::new(Self::LetDecl(LetDecl { name, ty, value }), line)
    }

    pub fn new_block(block: Block, line: usize) -> Located<Self> {
        Located::new(Self::Block(block), line)
    }

    pub fn new_function_decl(name: String, params: Vec<(String, Type)>, return_ty: Type, body: Block, line: usize) -> Located<Self> {
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
    pub ty: Type,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct LetDecl {
    pub name: String,
    pub ty: Type,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_ty: Type,
    pub body: Block,
}

// TODO: add record definitions
// TODO: add generic type definitions
// TODO: add type aliases

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