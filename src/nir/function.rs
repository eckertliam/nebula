use crate::nir::instruction::Instruction;
use crate::nir::ty::Type;

pub struct Function {
    pub params: Vec<Type>,
    pub ret: Type,
    pub instructions: Vec<Instruction>,
}