use crate::frontend::tokenizer::Loc;

pub struct Program {
    pub statements: Vec<Statement>,
}

pub struct Type {
    pub loc: Loc,
    pub kind: TypeNode,
}

pub enum TypeNode {
    Base(String),
    Generic(Box<Type>, Vec<Type>),
    Array(Box<Type>, Expression),
    Function(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
}

pub struct Expression {
    pub loc: Loc,
    pub kind: ExpressionNode,
}

pub enum ExpressionNode {
    Int(i64),
    Float(f64),
    String(String),
    True,
    False,
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),
}

pub struct Statement {
    pub loc: Loc,
    pub kind: StatementNode,
}

pub enum StatementNode {
    Block(Vec<Statement>),
    LetDeclaration(LetDeclaration),
    ConstDeclaration(ConstDeclaration),
    FnDeclaration(FnDeclaration),
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    TraitDeclaration(TraitDeclaration),
    Expression(Expression),
    Return(Expression),
}

pub struct LetDeclaration {
    pub name: String,
    pub _type: Type,
    pub value: Expression,
}

pub struct ConstDeclaration {
    pub name: String,
    pub _type: Type,
    pub value: Expression,
}

pub struct FnDeclaration {
    pub name: String,
    pub generic_params: Vec<String>,
    pub params: Vec<(String, Type)>,
    pub _type: Type,
    pub body: Vec<Statement>,
}

pub struct StructDeclaration {
    pub name: String,
    pub generic_params: Vec<String>,
    pub fields: Vec<(String, Type)>,
}

pub struct EnumDeclaration {
    pub name: String,
    pub generic_params: Vec<String>,
    pub variants: Vec<(String, Vec<Type>)>,
}

pub struct TraitDeclaration {
    pub name: String,
    pub required_impls: Vec<Type>,
    pub generic_params: Vec<String>,
    pub methods: Vec<FnDeclaration>,
}

pub struct ImplDeclaration {
    pub _trait: Option<Type>,
    pub _type: Type,
    pub methods: Vec<FnDeclaration>,
}