use crate::nir::instruction::Instruction;
use crate::nir::ty::Type;

use super::instruction::Block;

pub struct Function {
    pub params: Vec<Type>,
    pub ret: Type,
    pub blocks: Block,
}