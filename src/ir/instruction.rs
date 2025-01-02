use super::Type;
use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    // Variable declarations
    VarDecl {
        mutable: bool,
        name: String,
        ty: Type,
        value: Expression,
    },
    // Mutate var
    Mutate {
        name: String,
        value: Expression,
    },
    // Return
    Return(Expression),
    // Call function
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    }
}