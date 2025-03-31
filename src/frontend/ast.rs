use std::fmt::Display;

use super::located::Located;

#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Located<Declaration>>,
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
 * <method> ::= <name> (<param>*) return <return_type>
 *   <body>
 * end method
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
 * end constructor
 */
#[derive(Debug, Clone)]
pub struct ConstructorDecl {
    pub params: Vec<Located<Param>>,
    pub body: Located<Block>,
}

/*
 * Modifier
 * <modifier> ::= public | private | static | abstract
 */
#[derive(Debug, Clone)]
pub enum Modifier {
    Public,
    Private,
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
 * <expression> ::= <literal> | <variable> | <call> | <method_call> | <field_access> | <new> | <this> | <super>
 */
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Var(Var),
    Call(Box<Call>),
    MethodCall(Box<MethodCall>),
    FieldAccess(Box<FieldAccess>),
    New(Box<NewExpr>),
    This(Located<()>),
    Super(Located<()>),
}

/*
 * Literal
 * <literal> ::= <int> | <float> | <bool> | <char> | <string>
 */
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

/*
 * Variable
 * <variable> ::= <name>
 */
#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
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

/*
 * New Expression
 * <new> ::= new <class> (<arg>*)
 */
#[derive(Debug, Clone)]
pub struct NewExpr {
    pub class: Located<TypeExpr>,
    pub args: Vec<Located<Expr>>,
}
