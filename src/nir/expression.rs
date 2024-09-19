use super::context::Context;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Float(f64),
    Int(i64),
    Unsigned(u64),
    String(String),
    Reg(usize),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Array(Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    StructInstantiation(String, Vec<Expression>),
    EnumInstantiation(String, String, Vec<Expression>),
    StructFieldAccess(Box<Expression>, String),
    EnumFieldAccess(Box<Expression>, String, String),
    Tuple(Vec<Expression>),
}

impl Expression {
    //TODO: add a function for determining if an expression is a constant or can be reduced to a constant
    
    /// Some expressions need to be evaluated at compile time
    /// this function check if the expression can be evaluated at compile time
    /// and returns the result if it can
    pub fn eval(&self, context: &Context) -> Option<Expression> {
        match self {
            // TODO: add more cases
            Expression::Int(_) | Expression::Float(_) | Expression::String(_) | Expression::Unsigned(_) => Some(self.clone()),
            Expression::Add(lhs, rhs) => {
                // check if both sides are reducible
                // TODO: add more cases
                match (lhs.eval(&context), rhs.eval(&context)) {
                    (Some(Expression::Int(lhs)), Some(Expression::Int(rhs))) => Some(Expression::Int(lhs + rhs)),
                    (Some(Expression::Float(lhs)), Some(Expression::Float(rhs))) => Some(Expression::Float(lhs + rhs)),
                    (Some(Expression::Unsigned(lhs)), Some(Expression::Unsigned(rhs))) => Some(Expression::Unsigned(lhs + rhs)),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}