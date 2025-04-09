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
 * fn <name> (<param>*) return <return_type>
 *   <body>
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
 * <param> ::= <name> : <type>
 */
#[derive(Debug, Clone)]
pub struct Param {
    pub name: Located<String>,
    pub ty: Located<TypeExpr>,
}

/*
 * Type Expression
 * <type> ::= <name> | [<type>] | (<type>, ...)
 */
#[derive(Debug, Clone)]
pub enum TypeExpr {
    Named(String),
    Array(Box<TypeExpr>),
    Tuple(Vec<TypeExpr>),
}
    
/*
 * Block
 * begin
 *   <statements>
 * end
 */
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Located<Statement>>,
}

/*
 * Constant Declaration
 * const <name> (: <type>)? = <expr>
 */
#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: Located<String>,
    pub ty: Located<TypeExpr>,
    pub value: Located<Expr>,
}

/*
 * Let Declaration
 * let <name> (: <type>)? = <expr>
 */
#[derive(Debug, Clone)]
pub struct LetDecl {
    pub name: Located<String>,
    pub ty: Option<Located<TypeExpr>>,
    pub value: Located<Expr>,
}

/*
 * Type Declaration
 * type <name> = <type>
 */
#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: Located<String>,
    pub ty: Located<TypeExpr>,
}

/*
 * Class Declaration
 * class <name> (extends <superclass>)?
 *   <fields>
 *   <methods>
 *   <constructors>
 * end class
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
 * <method> ::= fn <name> (<param>*) return <return_type>
 *   <body>
 * end
 */
#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub name: Located<String>,
    pub params: Vec<Located<Param>>,
    pub return_type: Located<TypeExpr>,
    pub body: Located<Block>,
    pub modifiers: Vec<Located<Modifier>>,
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
 * Modifier
 * <modifier> ::= pub | static
 */
#[derive(Debug, Clone)]
pub enum Modifier {
    Pub,
    Static,
}

/*
 * Field
 * <field> ::= <name> : <type>
 */
#[derive(Debug, Clone)]
pub struct Field {
    pub name: Located<String>,
    pub ty: Located<TypeExpr>,
}

/*
 * Statement
 * <statement> ::= <let> | <const> | <return> | <expression> | <block>
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
 * <expression> ::= <int> | <float> | <bool> | <char> | <string> | <variable> | <call> | <method_call> | <field_access> | <new> | <this> | <super>
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
 * <call> ::= <callee> (<arg>*)
 */
#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Located<Expr>,
    pub args: Vec<Located<Expr>>,
}

/*
 * Method Call
 * <method_call> ::= <receiver>.<method>(<arg>*)
 */
#[derive(Debug, Clone)]
pub struct MethodCall {
    pub receiver: Located<Expr>,
    pub method: Located<String>,
    pub args: Vec<Located<Expr>>,
}

/*
 * Field Access
 * <field_access> ::= <receiver>.<field>
 */
#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub receiver: Located<Expr>,
    pub field: Located<String>,
}

#[derive(Debug, Clone)]
pub struct BinExpr {
    pub left: Located<Expr>,
    pub right: Located<Expr>,
    pub op: Located<TokenKind>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: Located<TokenKind>,
    pub expr: Located<Expr>,
}