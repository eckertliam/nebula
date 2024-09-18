use crate::frontend::tokenizer::Loc;

use super::tokenizer::TokenKind;

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub loc: Loc,
    pub kind: TypeNode,
}

impl Type {
    pub fn base(name: String, loc: Loc) -> Self {
        Self {
            loc,
            kind: TypeNode::Base(name),
        }
    }

    pub fn generic(name: String, params: Vec<Type>, loc: Loc) -> Self {
        Self {
            loc,
            kind: TypeNode::Generic(name, params),
        }
    }

    pub fn tuple(params: Vec<Type>, loc: Loc) -> Self {
        Self {
            loc,
            kind: TypeNode::Tuple(params),
        }
    }

    pub fn function(params: Vec<Type>, ret: Box<Type>, loc: Loc) -> Self {
        Self {
            loc,
            kind: TypeNode::Function(params, ret),
        }
    }

    pub fn array(elem: Box<Type>, size: Expression, loc: Loc) -> Self {
        Self {
            loc,
            kind: TypeNode::Array(elem, size),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeNode {
    Base(String),
    Generic(String, Vec<Type>),
    Array(Box<Type>, Expression),
    Function(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub loc: Loc,
    pub kind: ExpressionNode,
}

impl Expression {
    pub fn identifier(name: String, loc: Loc) -> Self {
        Self {
            loc,
            kind: ExpressionNode::Identifier(name),
        }
    }

    pub fn bool_literal(value: bool, loc: Loc) -> Self {
        if value {
            Self {
                loc,
                kind: ExpressionNode::True,
            }
        } else {
            Self {
                loc,
                kind: ExpressionNode::False,
            }
        }
    }

    pub fn int_literal(value: i64, loc: Loc) -> Self {
        Self {
            loc,
            kind: ExpressionNode::Int(value),
        }
    }

    pub fn float_literal(value: f64, loc: Loc) -> Self {
        Self {
            loc,
            kind: ExpressionNode::Float(value),
        }
    }

    pub fn string_literal(value: String, loc: Loc) -> Self {
        Self {
            loc,
            kind: ExpressionNode::String(value),
        }
    }

    pub fn array_literal(values: Vec<Expression>, loc: Loc) -> Self {
        Self {
            loc,
            kind: ExpressionNode::Array(values),
        }
    }

    pub fn binary(op: TokenKind, lhs: Expression, rhs: Expression, loc: Loc) -> Self {
        match op {
            TokenKind::Plus => Self {
                loc,
                kind: ExpressionNode::Add(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::Minus => Self {
                loc,
                kind: ExpressionNode::Sub(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::Star => Self {
                loc,
                kind: ExpressionNode::Mul(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::Slash => Self {
                loc,
                kind: ExpressionNode::Div(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::Percent => Self {
                loc,
                kind: ExpressionNode::Mod(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::EqEq => Self {
                loc,
                kind: ExpressionNode::Equal(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::BangEq => Self {
                loc,
                kind: ExpressionNode::NotEqual(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::Lt => Self {
                loc,
                kind: ExpressionNode::Less(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::LtEq => Self {
                loc,
                kind: ExpressionNode::LessEqual(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::Gt => Self {
                loc,
                kind: ExpressionNode::Greater(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::GtEq => Self {
                loc,
                kind: ExpressionNode::GreaterEqual(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::And => Self {
                loc,
                kind: ExpressionNode::And(Box::new(lhs), Box::new(rhs)),
            },
            TokenKind::Or => Self {
                loc,
                kind: ExpressionNode::Or(Box::new(lhs), Box::new(rhs)),
            },
            _ => panic!("Invalid binary operator: {:?}", op),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionNode {
    Identifier(String),
    Int(i64),
    Float(f64),
    String(String),
    Array(Vec<Expression>),
    True,
    False,
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    Less(Box<Expression>, Box<Expression>),
    LessEqual(Box<Expression>, Box<Expression>),
    Greater(Box<Expression>, Box<Expression>),
    GreaterEqual(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
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

    pub fn struct_declaration(name: String, loc: Loc, generic_params: Vec<GenericParam>, fields: Vec<(String, Type)>) -> Self {
        Self {
            loc,
            kind: StatementNode::StructDeclaration(StructDeclaration { name, generic_params, fields }),
        }
    }

    pub fn enum_declaration(name: String, loc: Loc, generic_params: Vec<GenericParam>, variants: Vec<(String, Vec<Type>)>) -> Self {
        Self {
            loc,
            kind: StatementNode::EnumDeclaration(EnumDeclaration { name, generic_params, variants }),
        }
    }

    pub fn type_declaration(name: String, loc: Loc, generic_params: Vec<GenericParam>, value: TypeDeclValue) -> Self {
        Self {
            loc,
            kind: StatementNode::TypeDeclaration(TypeDeclaration { name, generic_params, value }),
        }
    }

    pub fn trait_declaration(name: String, loc: Loc, required_impls: Vec<Type>, generic_params: Vec<GenericParam>, methods: Vec<FnDeclaration>) -> Self {
        Self {
            loc,
            kind: StatementNode::TraitDeclaration(TraitDeclaration { name, required_impls, generic_params, methods }),
        }
    }

    pub fn impl_declaration(_trait: Option<Type>, _type: Type, methods: Vec<FnDeclaration>, loc: Loc) -> Self {
        Self {
            loc,
            kind: StatementNode::ImplDeclaration(ImplDeclaration { _trait, _type, methods }),
        }
    }

    pub fn block(statements: Vec<Statement>, loc: Loc) -> Self {
        Self {
            loc,
            kind: StatementNode::Block(statements),
        }
    }

    pub fn expression(expression: Expression, loc: Loc) -> Self {
        Self {
            loc,
            kind: StatementNode::Expression(expression),
        }
    }

    pub fn return_statement(expression: Expression, loc: Loc) -> Self {
        Self {
            loc,
            kind: StatementNode::Return(expression),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementNode {
    Block(Vec<Statement>),
    LetDeclaration(LetDeclaration),
    ConstDeclaration(ConstDeclaration),
    FnDeclaration(FnDeclaration),
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    TraitDeclaration(TraitDeclaration),
    TypeDeclaration(TypeDeclaration),
    ImplDeclaration(ImplDeclaration),
    Expression(Expression),
    Return(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclaration {
    pub name: String,
    pub _type: Type,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDeclaration {
    pub name: String,
    pub _type: Type,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub type_var: String,
    pub bounds: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDeclaration {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub params: Vec<(String, Type)>,
    pub _type: Type,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDeclaration {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub value: TypeDeclValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDeclValue {
    // A | B | C
    Union(Vec<TypeDeclaration>),
    // A & B & C
    Intersection(Vec<TypeDeclaration>),
    // A = B
    Alias(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDeclaration {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub variants: Vec<(String, Vec<Type>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDeclaration {
    pub name: String,
    pub required_impls: Vec<Type>,
    pub generic_params: Vec<GenericParam>,
    pub methods: Vec<FnDeclaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDeclaration {
    pub _trait: Option<Type>,
    pub _type: Type,
    pub methods: Vec<FnDeclaration>,
}
