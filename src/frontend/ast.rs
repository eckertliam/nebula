use crate::frontend::scanner::TokenKind;

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

pub enum Type {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
    Array(Box<Type>, Expression),
    Function(Vec<Type>, Box<Type>),
    // User-defined type
    Udt(String),
}

pub enum Expression {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Integer(Integer),
    Float(Float),
}

impl Expression {
    pub fn new_binary(lhs: Expression, op: TokenKind, rhs: Expression, line: usize) -> Self {
        Self::Binary(BinaryExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
            line,
        })
    }

    pub fn new_unary(op: TokenKind, expr: Expression, line: usize) -> Self {
        Self::Unary(UnaryExpr { op, expr: Box::new(expr), line })
    }

    pub fn new_integer(value: i64, line: usize) -> Self {
        Self::Integer(Integer { value, line })
    }

    pub fn new_float(value: f64, line: usize) -> Self {
        Self::Float(Float { value, line })
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
    pub line: usize,
}

pub struct ConstDecl {
    pub name: String,
    pub ty: Option<Type>,
    pub value: Expression,
    pub line: usize,
}

pub struct LetDecl {
    pub name: String,
    pub ty: Option<Type>,
    pub value: Expression,
    pub line: usize,
}

pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_ty: Type,
    pub body: Block,
    pub line: usize,
}

pub struct BinaryExpr {
    pub lhs: Box<Expression>,
    pub op: TokenKind,
    pub rhs: Box<Expression>,
    pub line: usize,
}

pub struct UnaryExpr {
    pub op: TokenKind,
    pub expr: Box<Expression>,
    pub line: usize,
}

pub struct Integer {
    pub value: i64,
    pub line: usize,
}

pub struct Float {
    pub value: f64,
    pub line: usize,
}