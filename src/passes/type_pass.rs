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
        Expression::Array(elements) => infer_array_expr_type(elements, type_env),
    }
}

static NUMERIC_TYPES: [Type; 10] = [
    Type::I8,
    Type::I16,
    Type::I32,
    Type::I64,
    Type::U8,
    Type::U16,
    Type::U32,
    Type::U64,
    Type::F32,
    Type::F64,
];

fn is_numeric_type(ty: &Type) -> bool {
    NUMERIC_TYPES.contains(ty)
}

static ARITHMETIC_OPS: [TokenKind; 5] = [
    TokenKind::Plus,
    TokenKind::Minus,
    TokenKind::Star,
    TokenKind::Slash,
    TokenKind::Modulo,
];
static RELATIONAL_OPS: [TokenKind; 6] = [
    TokenKind::BangEq,
    TokenKind::EqEq,
    TokenKind::Lt,
    TokenKind::LtEq,
    TokenKind::Gt,
    TokenKind::GtEq,
];
static LOGICAL_OPS: [TokenKind; 2] = [TokenKind::And, TokenKind::Or];

fn infer_binary_expr_type(
    lhs: &Expression,
    op: &TokenKind,
    rhs: &Expression,
    type_env: &TypeEnv,
) -> Result<Type, String> {
    let lhs_type = infer_expr_type(lhs, type_env)?;
    let rhs_type = infer_expr_type(rhs, type_env)?;

    if ARITHMETIC_OPS.contains(op) {
        if !is_numeric_type(&lhs_type) || !is_numeric_type(&rhs_type) {
            return Err(format!(
                "Expected a numeric type, got {} and {}",
                lhs_type, rhs_type
            ));
        }

        if lhs_type != rhs_type {
            return Err(format!(
                "Cannot perform operation {} on types {} and {}",
                op, lhs_type, rhs_type
            ));
        }

        Ok(lhs_type)
    } else if RELATIONAL_OPS.contains(op) {
        // if lhs and rhs arent the same type then return an error
        if lhs_type != rhs_type {
            return Err(format!(
                "Cannot perform operation {} on types {} and {}",
                op, lhs_type, rhs_type
            ));
        }
        // everything can be compared except for function types, void, and tuples
        match (&lhs_type, &rhs_type) {
            (
                Type::Function {
                    params: _,
                    return_type: _,
                },
                Type::Function {
                    params: _,
                    return_type: _,
                },
            ) => {
                return Err(format!(
                    "Cannot perform operation {} on function types {}",
                    op, lhs_type
                ));
            }
            (Type::Void, Type::Void) => {
                return Err(format!(
                    "Cannot perform operation {} on void types",
                    op
                ));
            }
            // types that can be compared with only == and !=
            (Type::Tuple(_), Type::Tuple(_)) 
            | (Type::String, Type::String) 
            | (Type::Char, Type::Char) 
            | (Type::Array { element_type: _, size: _ }, Type::Array { element_type: _, size: _ }) => {
                if *op != TokenKind::EqEq && *op != TokenKind::BangEq {
                    return Err(format!(
                        "Cannot perform operation {} on types {}",
                        op, lhs_type
                    ));
                } else {
                    Ok(Type::Bool)
                }
            }
            _ => Ok(Type::Bool),
        }
    } else if LOGICAL_OPS.contains(op) {
        if lhs_type != Type::Bool || rhs_type != Type::Bool {
            return Err(format!(
                "Expected a boolean type, got {} and {}",
                lhs_type, rhs_type
            ));
        }

        Ok(Type::Bool)
    } else {
        unreachable!("No other tokens should be possible here")
    }
}

fn infer_unary_expr_type(
    op: &TokenKind,
    expr: &Expression,
    type_env: &TypeEnv,
) -> Result<Type, String> {
    let expr_type = infer_expr_type(expr, type_env)?;
    match op {
        TokenKind::Minus => match expr_type {
            Type::I64 | Type::U64 => Ok(Type::I64),
            Type::I32 | Type::U32 => Ok(Type::I32),
            Type::I16 | Type::U16 => Ok(Type::I16),
            Type::I8 | Type::U8 => Ok(Type::I8),
            Type::F32 => Ok(Type::F32),
            Type::F64 => Ok(Type::F64),
            _ => Err(format!("Expected a numeric type, got {}", expr_type)),
        },
        TokenKind::Bang => {
            if expr_type != Type::Bool {
                return Err(format!("Expected a boolean type, got {}", expr_type));
            } else {
                Ok(Type::Bool)
            }
        }
        _ => unreachable!("No other tokens should be possible here"),
    }
}

fn infer_ident_expr_type(ident: &str, type_env: &TypeEnv) -> Result<Type, String> {
    match type_env.get(ident).cloned() {
        Some(Type::TypeVar(name)) => Err(format!("Type variable {} not resolved", name)),
        Some(ty) => Ok(ty),
        None => Err(format!(
            "Identifier {} not found in type environment",
            ident
        )),
    }
}

fn infer_call_expr_type(
    callee: &Expression,
    args: &Vec<Expression>,
    type_env: &TypeEnv,
) -> Result<Type, String> {
    let callee_type = infer_expr_type(callee, type_env)?;

    // Check if the callee is a function type
    let (param_types, return_type) = match callee_type {
        Type::Function {
            params,
            return_type,
        } => (params, *return_type),
        _ => return Err(format!("Expected a function type, got {}", callee_type)),
    };

    // Check if argument count matches parameter count
    if args.len() != param_types.len() {
        return Err(format!(
            "Expected {} arguments, got {}",
            param_types.len(),
            args.len()
        ));
    }

    // Check if all argument types match parameter types
    for (arg, expected_type) in args.iter().zip(param_types.iter()) {
        let arg_type = infer_expr_type(arg, type_env)?;

        if arg_type != *expected_type {
            return Err(format!(
                "Expected argument type {}, got {}",
                expected_type, arg_type
            ));
        }
    }

    // Return the function's return type
    Ok(return_type)
}

fn infer_array_expr_type(elements: &Vec<Expression>, type_env: &TypeEnv) -> Result<Type, String> {
    // ensure all elements are of the same type
    let element_type = infer_expr_type(&elements[0], type_env)?;
    for element in elements.iter().skip(1) {
        if infer_expr_type(element, type_env)? != element_type {
            return Err(format!("All elements in array must be of the same type"));
        }
    }

    Ok(Type::Array { element_type: Box::new(element_type), size: elements.len() })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TokenKind;

    fn create_test_env() -> TypeEnv<'static> {
        let mut env = TypeEnv::new();
        env.insert("x", Type::I64);
        env.insert("y", Type::F64);
        env.insert("b", Type::Bool);
        env.insert("s", Type::String);
        env.insert("func", Type::Function {
            params: vec![Type::I64, Type::I64],
            return_type: Box::new(Type::I64),
        });
        env
    }

    #[test]
    fn test_literal_types() {
        let env = create_test_env();
        
        assert_eq!(infer_expr_type(&Expression::Integer(42), &env), Ok(Type::I64));
        assert_eq!(infer_expr_type(&Expression::UnsignedInteger(42), &env), Ok(Type::U64));
        assert_eq!(infer_expr_type(&Expression::Float(3.14), &env), Ok(Type::F64));
        assert_eq!(infer_expr_type(&Expression::Char('a'), &env), Ok(Type::Char));
        assert_eq!(infer_expr_type(&Expression::String("hello".to_string()), &env), Ok(Type::String));
        assert_eq!(infer_expr_type(&Expression::Bool(true), &env), Ok(Type::Bool));
    }

    #[test]
    fn test_binary_arithmetic() {
        let env = create_test_env();
        let ops = [TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash, TokenKind::Modulo];
        
        for op in ops.iter() {
            let expr = Expression::Binary {
                lhs: Box::new(Expression::Integer(1)),
                op: op.clone(),
                rhs: Box::new(Expression::Integer(2)),
            };
            assert_eq!(infer_expr_type(&expr, &env), Ok(Type::I64));

            // Test type mismatch
            let expr_mismatch = Expression::Binary {
                lhs: Box::new(Expression::Integer(1)),
                op: op.clone(),
                rhs: Box::new(Expression::Float(2.0)),
            };
            assert!(infer_expr_type(&expr_mismatch, &env).is_err());
        }
    }

    #[test]
    fn test_binary_comparison() {
        let env = create_test_env();
        let ops = [TokenKind::EqEq, TokenKind::BangEq, TokenKind::Lt, TokenKind::LtEq, TokenKind::Gt, TokenKind::GtEq];
        
        for op in ops.iter() {
            // Numeric comparison
            let expr = Expression::Binary {
                lhs: Box::new(Expression::Integer(1)),
                op: op.clone(),
                rhs: Box::new(Expression::Integer(2)),
            };
            assert_eq!(infer_expr_type(&expr, &env), Ok(Type::Bool));

            // String comparison (only == and !=)
            let str_expr = Expression::Binary {
                lhs: Box::new(Expression::String("a".to_string())),
                op: op.clone(),
                rhs: Box::new(Expression::String("b".to_string())),
            };
            let result = infer_expr_type(&str_expr, &env);
            match op {
                TokenKind::EqEq | TokenKind::BangEq => assert_eq!(result, Ok(Type::Bool)),
                _ => assert!(result.is_err()),
            }
        }
    }

    #[test]
    fn test_binary_logical() {
        let env = create_test_env();
        let ops = [TokenKind::And, TokenKind::Or];
        
        for op in ops.iter() {
            let expr = Expression::Binary {
                lhs: Box::new(Expression::Bool(true)),
                op: op.clone(),
                rhs: Box::new(Expression::Bool(false)),
            };
            assert_eq!(infer_expr_type(&expr, &env), Ok(Type::Bool));

            // Test type mismatch
            let expr_mismatch = Expression::Binary {
                lhs: Box::new(Expression::Bool(true)),
                op: op.clone(),
                rhs: Box::new(Expression::Integer(1)),
            };
            assert!(infer_expr_type(&expr_mismatch, &env).is_err());
        }
    }

    #[test]
    fn test_unary_operators() {
        let env = create_test_env();
        
        // Test numeric negation
        let neg_expr = Expression::Unary {
            op: TokenKind::Minus,
            expr: Box::new(Expression::Integer(42)),
        };
        assert_eq!(infer_expr_type(&neg_expr, &env), Ok(Type::I64));

        // Test logical negation
        let not_expr = Expression::Unary {
            op: TokenKind::Bang,
            expr: Box::new(Expression::Bool(true)),
        };
        assert_eq!(infer_expr_type(&not_expr, &env), Ok(Type::Bool));

        // Test invalid negation
        let invalid_neg = Expression::Unary {
            op: TokenKind::Minus,
            expr: Box::new(Expression::Bool(true)),
        };
        assert!(infer_expr_type(&invalid_neg, &env).is_err());
    }

    #[test]
    fn test_identifier_lookup() {
        let env = create_test_env();
        
        assert_eq!(infer_expr_type(&Expression::Identifier("x".to_string()), &env), Ok(Type::I64));
        assert_eq!(infer_expr_type(&Expression::Identifier("y".to_string()), &env), Ok(Type::F64));
        assert_eq!(infer_expr_type(&Expression::Identifier("b".to_string()), &env), Ok(Type::Bool));
        
        // Test undefined identifier
        assert!(infer_expr_type(&Expression::Identifier("undefined".to_string()), &env).is_err());
    }

    #[test]
    fn test_function_calls() {
        let env = create_test_env();
        
        // Test valid function call
        let valid_call = Expression::Call {
            callee: Box::new(Expression::Identifier("func".to_string())),
            args: vec![Expression::Integer(1), Expression::Integer(2)],
        };
        assert_eq!(infer_expr_type(&valid_call, &env), Ok(Type::I64));

        // Test wrong number of arguments
        let wrong_args = Expression::Call {
            callee: Box::new(Expression::Identifier("func".to_string())),
            args: vec![Expression::Integer(1)],
        };
        assert!(infer_expr_type(&wrong_args, &env).is_err());

        // Test wrong argument type
        let wrong_type = Expression::Call {
            callee: Box::new(Expression::Identifier("func".to_string())),
            args: vec![Expression::Integer(1), Expression::Bool(true)],
        };
        assert!(infer_expr_type(&wrong_type, &env).is_err());
    }

    #[test]
    fn test_array_type_inference() {
        let env = create_test_env();
        
        // Test homogeneous array
        let valid_array = Expression::Array(vec![
            Expression::Integer(1),
            Expression::Integer(2),
            Expression::Integer(3),
        ]);
        assert_eq!(
            infer_expr_type(&valid_array, &env),
            Ok(Type::Array {
                element_type: Box::new(Type::I64),
                size: 3
            })
        );

        // Test heterogeneous array (should fail)
        let invalid_array = Expression::Array(vec![
            Expression::Integer(1),
            Expression::Bool(true),
        ]);
        assert!(infer_expr_type(&invalid_array, &env).is_err());
    }
}
