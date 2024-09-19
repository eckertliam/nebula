use crate::nir::ty::Type;

pub enum Instruction {
    // memory
    // move a value from on register to another
    // type, destination, source
    Mov(Type, usize, usize),
    // load a constant from the constant pool
    // type, destination, constant
    LoadConst(Type, usize, usize),
    // arithmetic
    // perform ops between two registers and store the result in a third
    // type, destination, left, right
    Add(Type, usize, usize, usize),
    Sub(Type, usize, usize, usize),
    Mul(Type, usize, usize, usize),
    Div(Type, usize, usize, usize),
    Mod(Type, usize, usize, usize),
    // control flow
    // jump to a block
    Jump(usize),
    // return from a function with a value in a register
    Return(Type, usize),
}

pub type Block = Vec<Instruction>;