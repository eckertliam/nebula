use std::fmt::Display;

use super::{lexer::TokenKind, located::Located};

#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Located<Declaration>>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            declarations: Vec::new(),
        }
    }

    pub fn push_decl(&mut self, decl: Located<Declaration>) {
        self.declarations.push(decl);
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(Located<FunctionDecl>),
    Constant(Located<ConstDecl>),
    Let(Located<LetDecl>),
    TypeAlias(Located<TypeDecl>),
    Class(Located<ClassDecl>),
}

/*
 * Function Declaration
 * Example:
 * fn add(a: int, b: int) return int
 *   return a + b
 * end fn
 */
#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Located<String>,
    pub params: Vec<Located<Param>>,
    pub return_type: Located<TypeExpr>,
    pub body: Located<Block>,
}

/*
 * Parameter
 */
#[derive(Debug, Clone)]
pub struct Param {
    pub name: Located<String>,
    pub ty: Located<TypeExpr>,
}

/*
 * Type Expression
 * Example:
 * int
 * [int]
 * (int, int)
 */
#[derive(Debug, Clone)]
pub enum TypeExpr {
    Named(String),
    Array(Box<TypeExpr>),
    Tuple(Vec<TypeExpr>),
}
    
/*
 * Block of Begin End or body of statements
 * Example:
 * begin
 *   const a: int = 1
 *   const b: int = 2
 *   a + b
 * end
 */
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Located<Statement>>,
}

/*
 * Constant Declaration
 * Example:
 * const a: int = 1
 * const name = "John"
 */
#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: Located<String>,
    pub ty: Located<TypeExpr>,
    pub value: Located<Expr>,
}

/*
 * Let Declaration
 * Example:
 * let a: int = 1
 * let name = "John"
 */
#[derive(Debug, Clone)]
pub struct LetDecl {
    pub name: Located<String>,
    pub ty: Option<Located<TypeExpr>>,
    pub value: Located<Expr>,
}

/*
 * Type Declaration
 * Example:
 * type Token = (string, int, int)
 */
#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: Located<String>,
    pub ty: Located<TypeExpr>,
}

/*
 * Class Declaration
 * Example:
 * class Point
 *   x: int
 *   y: int
 *   color: string
 * 
 *   Point(x: int, y: int)
 *     this.x = x
 *     this.y = y
 *     this.color = "red"
 *     return this
 *   end
 * 
 *   fn add(other: Point) return Point
 *     return Point(x: this.x + other.x, y: this.y + other.y)
 *   end
 * end
 */
#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: Located<String>,
    pub superclass: Option<Located<TypeExpr>>,
    pub fields: Vec<Located<Field>>,
    pub methods: Vec<Located<MethodDecl>>,
    pub constructors: Vec<Located<ConstructorDecl>>,
}

/*
 * Method Declaration
 * Example:
 * fn add(other: Point) return Point
 *   return Point(x: this.x + other.x, y: this.y + other.y)
 * end
 * 
 * pub fn get_color() return string
 *   return this.color
 * end
 */
#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub public: bool,
    pub name: Located<String>,
    pub params: Vec<Located<Param>>,
    pub return_type: Located<TypeExpr>,
    pub body: Located<Block>,
}

/*
 * Constructor Declaration
 * <constructor> ::= <name> (<param>*)
 *   <body>
 * end
 */
#[derive(Debug, Clone)]
pub struct ConstructorDecl {
    pub params: Vec<Located<Param>>,
    pub body: Located<Block>,
}

/*
 * Field
 * Example:
 * x: int
 * color: string
 * pub x: int
 */
#[derive(Debug, Clone)]
pub struct Field {
    pub public: bool,
    pub name: Located<String>,
    pub ty: Located<TypeExpr>,
}

/*
 * Statement
 * Example:
 * let a: int = 1
 * const b: int = 2
 * return a + b
 */
#[derive(Debug, Clone)]
pub enum Statement {
    Let(Located<LetDecl>),
    Const(Located<ConstDecl>),
    Return(Located<Expr>),
    Expression(Located<Expr>),
    Block(Located<Block>),
}

/*
 * Expression
 */
#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Var(String),
    Call(Box<Call>),
    MethodCall(Box<MethodCall>),
    FieldAccess(Box<FieldAccess>),
    This,
    Super,
    BinExpr(Box<BinExpr>),
    UnaryExpr(Box<UnaryExpr>),
}

/*
 * Call
 * Example:
 * add(1, 2)
 * call()
 */
#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Located<Expr>,
    pub args: Vec<Located<Expr>>,
}

/*
 * Method Call
 * Example:
 * a.x()
 */
#[derive(Debug, Clone)]
pub struct MethodCall {
    pub receiver: Located<Expr>,
    pub method: Located<String>,
    pub args: Vec<Located<Expr>>,
}

/*
 * Field Access
 * Example:
 * a.x
 * a.color
 */
#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub receiver: Located<Expr>,
    pub field: Located<String>,
}

/*
 * Binary Expression
 * Example:
 * a + b
 * a - b
 * a * b
 * a / b
 */
#[derive(Debug, Clone)]
pub struct BinExpr {
    pub left: Located<Expr>,
    pub right: Located<Expr>,
    pub op: Located<TokenKind>,
}

/*
 * Unary Expression
 * Example:
 * !a
 * -a
 */
#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: Located<TokenKind>,
    pub expr: Located<Expr>,
}