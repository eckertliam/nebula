use std::collections::HashMap;

use crate::nir::instruction::{Instruction, Block};
use crate::nir::ty::Type;

pub struct Function {
    pub params: HashMap<String, Type>,
    pub locals: HashMap<String, Type>,
    pub ret: Type,
    pub blocks: Block,
}