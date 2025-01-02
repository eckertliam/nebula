use super::{Instruction, Type};

/// Represents a function in the IR
#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Instruction>,
}

impl Into<Type> for Function {
    fn into(self) -> Type {
        Type::Function {
            params: self.params.into_iter().map(|(_, ty)| ty).collect(),
            return_type: Box::new(self.return_type),
        }
    }
}
