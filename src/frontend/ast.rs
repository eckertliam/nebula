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
    Array(Box<Type>, Box<Expression>),
    Function(Vec<Type>, Box<Type>),
    // User-defined type
    Udt(String),
}

pub type TypeExpr = Located<Type>;

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
    pub ty: Option<Type>,
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
