use super::Function;
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
    Return(Option<Expression>),
    // Call function
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    // Function declaration never used converting ir to llvm only used to be added to context
    FuncDecl(Function),
}
