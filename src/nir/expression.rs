macro_rules! match_binop {
    ($op:tt, $lhs:expr, $rhs:expr, $(($variant:ident, $res_variant:ident)),*) => {
        match ($lhs.fold(), $rhs.fold()) {
            $(
                (Some(Expression::$variant(lhs)), Some(Expression::$variant(rhs))) => Some(Expression::$res_variant(lhs $op rhs)),
            )*
            _ => None,
        }
    };
}

macro_rules! match_arith {
    ($op:tt, $lhs:expr, $rhs:expr, $($variant:ident),*) => {
        match ($lhs.fold(), $rhs.fold()) {
            $(
                (Some(Expression::$variant(lhs)), Some(Expression::$variant(rhs))) => Some(Expression::$variant(lhs $op rhs)),
            )*
            _ => None,
        }
    };
}

macro_rules! match_cmp {
    ($op:tt, $lhs:expr, $rhs:expr, $($variant:ident),*) => {
        match ($lhs.fold(), $rhs.fold()) {
            $(
                (Some(Expression::$variant(lhs)), Some(Expression::$variant(rhs))) => Some(Expression::Bool(lhs $op rhs)),
            )*
            _ => None,
        }
    };
    () => {
        
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Bool(bool),
    Float(f64),
    Int(i64),
    Unsigned(u64),
    String(String),
    Ident(String),
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
    /// Folds an expression into a constant if possible
    /// Returns None if the expression is not foldable
    pub fn fold(&self) -> Option<Expression> {
        match self {
            Expression::Bool(_) | Expression::Float(_) | Expression::Int(_) | Expression::Unsigned(_) | Expression::String(_) => Some(self.clone()),
            Expression::Ident(_) => None,
            Expression::Add(lhs, rhs) => match_arith!(+, lhs, rhs, Int, Float, Unsigned),
            Expression::Sub(lhs, rhs) => match_arith!(-, lhs, rhs, Int, Float, Unsigned),
            Expression::Mul(lhs, rhs) => match_arith!(*, lhs, rhs, Int, Float, Unsigned),
            Expression::Div(lhs, rhs) => match_arith!(/, lhs, rhs, Int, Float, Unsigned),
            Expression::Mod(lhs, rhs) => match_arith!(%, lhs, rhs, Int, Unsigned),
            Expression::Equal(lhs, rhs) => match_cmp!(==, lhs, rhs, Int, Float, Unsigned, Bool, String),
            Expression::NotEqual(lhs, rhs) => match_cmp!(!=, lhs, rhs, Int, Float, Unsigned, Bool, String),
            Expression::Less(lhs, rhs) => match_cmp!(<, lhs, rhs, Int, Float, Unsigned),
            Expression::LessEqual(lhs, rhs) => match_cmp!(<=, lhs, rhs, Int, Float, Unsigned),
            Expression::Greater(lhs, rhs) => match_cmp!(>, lhs, rhs, Int, Float, Unsigned),
            Expression::GreaterEqual(lhs, rhs) => match_cmp!(>=, lhs, rhs, Int, Float, Unsigned),
            Expression::And(lhs, rhs) => match_cmp!(&&, lhs, rhs, Bool),
            Expression::Or(lhs, rhs) => match_cmp!(||, lhs, rhs, Bool),
            Expression::Call(_, _) => None,
            Expression::Array(arr) => {
                // checks if all elements any elements are foldable
                let mut new_arr = Vec::new();
                for e in arr {
                    if let Some(e) = e.fold() {
                        new_arr.push(e);
                    } else {
                        return None;
                    }
                }
                Some(Expression::Array(new_arr))
            }
            Expression::Index(arr, idx) => {
                if let (Some(arr), Some(idx)) = (arr.fold(), idx.fold()) {
                    Some(Expression::Index(Box::new(arr), Box::new(idx)))
                } else {
                    None
                }
            }
            Expression::StructInstantiation(_, _) => None,
            Expression::EnumInstantiation(_, _, _) => None,
            Expression::StructFieldAccess(_, _) => None,
            Expression::EnumFieldAccess(_, _, _) => None,
            Expression::Tuple(tup) => {
                let mut new_tup = Vec::new();
                for e in tup {
                    if let Some(e) = e.fold() {
                        new_tup.push(e);
                    } else {
                        return None;
                    }
                }
                Some(Expression::Tuple(new_tup))
            }
        }
    }
}