use crate::frontend::tokenizer::Loc;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: Vec::new() }
    }

    pub fn push(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
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

impl Statement {
    pub fn const_declaration(name: String, loc: Loc, _type: Type, value: Expression) -> Self {
        Self {
            loc,
            kind: StatementNode::ConstDeclaration(ConstDeclaration { name, _type, value }),
        }
    }

    pub fn let_declaration(name: String, loc: Loc, _type: Type, value: Expression) -> Self {
        Self {
            loc,
            kind: StatementNode::LetDeclaration(LetDeclaration { name, _type, value }),
        }
    }

    pub fn fn_declaration(name: String, loc: Loc, generic_params: Vec<GenericParam>, params: Vec<(String, Type)>, _type: Type, body: Vec<Statement>) -> Self {
        Self {
            loc,
            kind: StatementNode::FnDeclaration(FnDeclaration { name, generic_params, params, _type, body }),
        }
    }
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

pub struct GenericParam {
    pub type_var: String,
    pub bounds: Vec<Type>,
}

pub struct FnDeclaration {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub params: Vec<(String, Type)>,
    pub _type: Type,
    pub body: Vec<Statement>,
}

pub struct TypeDeclaration {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub value: TypeDeclValue,
}

pub enum TypeDeclValue {
    // A | B | C
    Union(Vec<TypeDeclaration>),
    // A & B & C
    Intersection(Vec<TypeDeclaration>),
    // A = B
    Alias(Type),
}

pub struct StructDeclaration {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub fields: Vec<(String, Type)>,
}

pub struct EnumDeclaration {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub variants: Vec<(String, Vec<Type>)>,
}

pub struct TraitDeclaration {
    pub name: String,
    pub required_impls: Vec<Type>,
    pub generic_params: Vec<GenericParam>,
    pub methods: Vec<FnDeclaration>,
}

pub struct ImplDeclaration {
    pub _trait: Option<Type>,
    pub _type: Type,
    pub methods: Vec<FnDeclaration>,
}