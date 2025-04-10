use std::fmt::Display;

use super::{located::Located, parse_rule::ParseRule, parser::{Parser, Precedence}, token::TokenKind};

pub trait Parse<'src, T: Sized> {
    fn parse(parser: Parser<'src>) -> Option<Located<T>>;

    #[allow(unused_variables)]
    fn infix(parser: Parser<'src>, expr: Located<T>) -> Option<Located<T>> {
        None
    }
}

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
    Int(Int),
    Float(Float),
    Bool(Bool),
    Char(Char),
    Str(Str),
    Ident(Ident),
    Call(Box<Call>),
    MethodCall(Box<MethodCall>),
    FieldAccess(Box<FieldAccess>),
    This,
    Super,
    BinExpr(Box<BinExpr>),
    UnaryExpr(Box<UnaryExpr>),
}

impl<'src> Expr {
    pub fn parse_precedence(mut parser: Parser<'src>, precedence: Precedence) -> Option<Located<Expr>> {
        parser.advance();

        let rule: ParseRule<'src> = parser.previous_rule();
        
        let mut expr = if let Some(prefix) = rule.prefix {
            prefix(parser)?
        } else {
            parser.error_at(parser.previous, "Expected expression");
            return None;
        };

        loop {
            let current_rule = parser.current_rule();
            if precedence > current_rule.precedence {
                break;
            }

            parser.advance();
            
            match current_rule.infix {
                Some(infix) => {
                    expr = infix(parser, expr)?;
                }
                None => {
                    parser.error_at(parser.current, "Expected infix operator");
                    return None;
                }
            }
        }

        Some(expr)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Int {
    pub value: i64,
}

impl<'src> Parse<'src, Expr> for Int {
    fn parse(parser: Parser<'src>) -> Option<Located<Expr>> {
        let lexeme = parser.previous.value.lexeme;
        if let Ok(value) = lexeme.parse::<i64>() {
            Some(Located::new(parser.previous.column, parser.previous.line, Expr::Int(Int { value })))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Float {
    pub value: f64,
}

impl<'src> Parse<'src, Expr> for Float {
    fn parse(parser: Parser<'src>) -> Option<Located<Expr>> {
        let lexeme = parser.previous.value.lexeme;
        if let Ok(value) = lexeme.parse::<f64>() {
            Some(Located::new(parser.previous.column, parser.previous.line, Expr::Float(Float { value })))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Bool {
    pub value: bool,
}

impl<'src> Parse<'src, Expr> for Bool {
    fn parse(parser: Parser<'src>) -> Option<Located<Expr>> {
        let kind = parser.previous.value.kind;
        match kind {
            TokenKind::True => Some(Located::new(parser.previous.column, parser.previous.line, Expr::Bool(Bool { value: true }))),
            TokenKind::False => Some(Located::new(parser.previous.column, parser.previous.line, Expr::Bool(Bool { value: false }))),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Char {
    pub value: char,
}

impl<'src> Parse<'src, Expr> for Char {
    fn parse(mut parser: Parser<'src>) -> Option<Located<Expr>> {
        let lexeme = parser.previous.value.lexeme;
        if lexeme.len() == 1 {
            Some(Located::new(parser.previous.column, parser.previous.line, Expr::Char(Char { value: lexeme.chars().next().unwrap() })))
        } else {
            parser.error_at(parser.previous, "Expected character");
            None
        }
    }
}
#[derive(Debug, Clone)]
pub struct Str {
    pub value: String,
}

impl<'src> Parse<'src, Expr> for Str {
    fn parse(parser: Parser<'src>) -> Option<Located<Expr>> {
        let lexeme = parser.previous.value.lexeme;
        let column = parser.previous.column;
        let line = parser.previous.line;
        Some(Located::new(column, line, Expr::Str(Str { value: lexeme.to_string() })))
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub value: String,
}

impl<'src> Parse<'src, Expr> for Ident {
    fn parse(parser: Parser<'src>) -> Option<Located<Expr>> {
        let lexeme = parser.previous.value.lexeme;
        Some(Located::new(parser.previous.column, parser.previous.line, Expr::Ident(Ident { value: lexeme.to_string() })))
    }
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

impl<'src> Parse<'src, Expr> for BinExpr {
    #[allow(unused_variables)]
    fn parse(parser: Parser<'src>) -> Option<Located<Expr>> {
        None
    }

    fn infix(parser: Parser<'src>, expr: Located<Expr>) -> Option<Located<Expr>> {
        // get operator type 
        let op = Located::new(parser.previous.column, parser.previous.line, parser.previous.value.kind);
        let precedence = op.value.get_parse_rule().precedence;

        let right = Expr::parse_precedence(parser, precedence.increment())?;

        let bin_expr = BinExpr {
            left: expr,
            right,
            op,
        };

        Some(Located::new(parser.previous.column, parser.previous.line, Expr::BinExpr(Box::new(bin_expr))))
    }
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

impl<'src> Parse<'src, Expr> for UnaryExpr {
    fn parse(parser: Parser<'src>) -> Option<Located<Expr>> {
        // we already know that the previous token is a unary operator
        let op = Located::new(parser.previous.column, parser.previous.line, parser.previous.value.kind);
        let operand = Expr::parse_precedence(parser, Precedence::Unary)?;
        let unary_expr = UnaryExpr {
            op,
            expr: operand,
        };

        Some(Located::new(
            parser.previous.column,
            parser.previous.line,
            Expr::UnaryExpr(Box::new(unary_expr)),
        ))
    }
}