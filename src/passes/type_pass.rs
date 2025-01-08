use crate::{Expression, TokenKind, Type, TypeEnv};

/// Returns the type that the expression qualifies for
fn infer_expr_type(expr: &Expression, type_env: &TypeEnv) -> Result<Type, String> {
    match expr {
        Expression::Binary { lhs, op, rhs } => infer_binary_expr_type(lhs, op, rhs, type_env),
        Expression::Unary { op, expr } => infer_unary_expr_type(op, expr, type_env),
        // Numeric types resolve to their 64 bit variant
        Expression::Integer(_) => Ok(Type::I64),
        Expression::UnsignedInteger(_) => Ok(Type::U64),
        Expression::Float(_) => Ok(Type::F64),
        Expression::Char(_) => Ok(Type::Char),
        Expression::String(_) => Ok(Type::String),
        Expression::Bool(_) => Ok(Type::Bool),
        Expression::Identifier(ident) => infer_ident_expr_type(ident, type_env),
        Expression::Call { callee, args } => infer_call_expr_type(callee, args, type_env),
        Expression::Array { elements } => infer_array_expr_type(elements, type_env),
    }
}

fn infer_binary_expr_type(lhs: &Expression, op: &TokenKind, rhs: &Expression, type_env: &TypeEnv) -> Result<Type, String> {
    todo!()
}

fn infer_unary_expr_type(op: &TokenKind, expr: &Expression, type_env: &TypeEnv) -> Result<Type, String> {
    todo!()
}

fn infer_ident_expr_type(ident: &str, type_env: &TypeEnv) -> Result<Type, String> {
    match type_env.get(ident).cloned() {
        Some(Type::TypeVar(name)) => Err(format!("Type variable {} not resolved", name)),
        Some(ty) => Ok(ty),
        None => Err(format!("Identifier {} not found in type environment", ident)),
    }
}

fn infer_call_expr_type(callee: &Expression, args: &Vec<Expression>, type_env: &TypeEnv) -> Result<Type, String> {
    let callee_type = infer_expr_type(callee, type_env)?;

    // Check if the callee is a function type
    let (param_types, return_type) = match callee_type {
        Type::Function { params, return_type } => (params, *return_type),
        _ => return Err(format!("Expected a function type, got {}", callee_type)),
    };

    // Check if argument count matches parameter count
    if args.len() != param_types.len() {
        return Err(format!("Expected {} arguments, got {}", param_types.len(), args.len()));
    }

    // Check if all argument types match parameter types
    for (arg, expected_type) in args.iter().zip(param_types.iter()) {
        let arg_type = infer_expr_type(arg, type_env)?;
        
        if arg_type != *expected_type {
            return Err(format!("Expected argument type {}, got {}", expected_type, arg_type));
        }
    }

    // Return the function's return type
    Ok(return_type)
}

fn infer_array_expr_type(elements: &Vec<Expression>, type_env: &TypeEnv) -> Result<Type, String> {
    todo!()
}
