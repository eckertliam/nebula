use crate::Expression;
use crate::frontend::scanner::TokenKind;

pub fn const_fold(expr: Expression) -> Option<Expression> {
    match expr {
        Expression::Integer(v) => Some(Expression::Integer(v)),
        Expression::UnsignedInteger(v) => Some(Expression::UnsignedInteger(v)),
        Expression::Char(v) => Some(Expression::Char(v)),
        Expression::String(v) => Some(Expression::String(v)),
        Expression::Float(v) => Some(Expression::Float(v)),
        Expression::Bool(v) => Some(Expression::Bool(v)),
        Expression::Binary { lhs, op, rhs } => fold_binary(lhs, op, rhs),
        Expression::Unary { op, expr } => fold_unary(op, expr),
        _ => None,
    }
}

fn fold_binary(lhs: Box<Expression>, op: TokenKind, rhs: Box<Expression>) -> Option<Expression> {
    let lhs = const_fold(*lhs)?;
    let rhs = const_fold(*rhs)?;
    
    match (lhs, op, rhs) {
        // Integer operations
        (Expression::Integer(l), TokenKind::Plus, Expression::Integer(r)) => Some(Expression::Integer(l + r)),
        (Expression::Integer(l), TokenKind::Minus, Expression::Integer(r)) => Some(Expression::Integer(l - r)),
        (Expression::Integer(l), TokenKind::Star, Expression::Integer(r)) => Some(Expression::Integer(l * r)),
        (Expression::Integer(l), TokenKind::Slash, Expression::Integer(r)) if r != 0 => Some(Expression::Integer(l / r)),
        (Expression::Integer(l), TokenKind::Modulo, Expression::Integer(r)) if r != 0 => Some(Expression::Integer(l % r)),
        
        // Float operations
        (Expression::Float(l), TokenKind::Plus, Expression::Float(r)) => Some(Expression::Float(l + r)),
        (Expression::Float(l), TokenKind::Minus, Expression::Float(r)) => Some(Expression::Float(l - r)),
        (Expression::Float(l), TokenKind::Star, Expression::Float(r)) => Some(Expression::Float(l * r)),
        (Expression::Float(l), TokenKind::Slash, Expression::Float(r)) if r != 0.0 => Some(Expression::Float(l / r)),
        
        // Boolean operations
        (Expression::Bool(l), TokenKind::And, Expression::Bool(r)) => Some(Expression::Bool(l && r)),
        (Expression::Bool(l), TokenKind::Or, Expression::Bool(r)) => Some(Expression::Bool(l || r)),
        
        // Comparison operations for integers
        (Expression::Integer(l), TokenKind::EqEq, Expression::Integer(r)) => Some(Expression::Bool(l == r)),
        (Expression::Integer(l), TokenKind::BangEq, Expression::Integer(r)) => Some(Expression::Bool(l != r)),
        (Expression::Integer(l), TokenKind::Lt, Expression::Integer(r)) => Some(Expression::Bool(l < r)),
        (Expression::Integer(l), TokenKind::Gt, Expression::Integer(r)) => Some(Expression::Bool(l > r)),
        (Expression::Integer(l), TokenKind::LtEq, Expression::Integer(r)) => Some(Expression::Bool(l <= r)),
        (Expression::Integer(l), TokenKind::GtEq, Expression::Integer(r)) => Some(Expression::Bool(l >= r)),
        
        // Comparison operations for floats
        (Expression::Float(l), TokenKind::EqEq, Expression::Float(r)) => Some(Expression::Bool(l == r)),
        (Expression::Float(l), TokenKind::BangEq, Expression::Float(r)) => Some(Expression::Bool(l != r)),
        (Expression::Float(l), TokenKind::Lt, Expression::Float(r)) => Some(Expression::Bool(l < r)),
        (Expression::Float(l), TokenKind::Gt, Expression::Float(r)) => Some(Expression::Bool(l > r)),
        (Expression::Float(l), TokenKind::LtEq, Expression::Float(r)) => Some(Expression::Bool(l <= r)),
        (Expression::Float(l), TokenKind::GtEq, Expression::Float(r)) => Some(Expression::Bool(l >= r)),
        
        // String comparisons
        (Expression::String(l), TokenKind::EqEq, Expression::String(r)) => Some(Expression::Bool(l == r)),
        (Expression::String(l), TokenKind::BangEq, Expression::String(r)) => Some(Expression::Bool(l != r)),
        (Expression::String(l), TokenKind::Lt, Expression::String(r)) => Some(Expression::Bool(l < r)),
        (Expression::String(l), TokenKind::Gt, Expression::String(r)) => Some(Expression::Bool(l > r)),
        (Expression::String(l), TokenKind::LtEq, Expression::String(r)) => Some(Expression::Bool(l <= r)),
        (Expression::String(l), TokenKind::GtEq, Expression::String(r)) => Some(Expression::Bool(l >= r)),
        
        // Character comparisons
        (Expression::Char(l), TokenKind::EqEq, Expression::Char(r)) => Some(Expression::Bool(l == r)),
        (Expression::Char(l), TokenKind::BangEq, Expression::Char(r)) => Some(Expression::Bool(l != r)),
        (Expression::Char(l), TokenKind::Lt, Expression::Char(r)) => Some(Expression::Bool(l < r)),
        (Expression::Char(l), TokenKind::Gt, Expression::Char(r)) => Some(Expression::Bool(l > r)),
        (Expression::Char(l), TokenKind::LtEq, Expression::Char(r)) => Some(Expression::Bool(l <= r)),
        (Expression::Char(l), TokenKind::GtEq, Expression::Char(r)) => Some(Expression::Bool(l >= r)),
        
        // If we can't fold, return None
        _ => None,
    }
}

fn fold_unary(op: TokenKind, expr: Box<Expression>) -> Option<Expression> {
    let expr = const_fold(*expr)?;
    
    match (op, expr) {
        // Numeric negation
        (TokenKind::Minus, Expression::Integer(v)) => Some(Expression::Integer(-v)),
        (TokenKind::Minus, Expression::Float(v)) => Some(Expression::Float(-v)),
        
        // Logical not
        (TokenKind::Bang, Expression::Bool(v)) => Some(Expression::Bool(!v)),
        
        // If we can't fold, return None
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to create a binary expression
    fn make_binary(lhs: Expression, op: TokenKind, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    // Helper function to create a unary expression
    fn make_unary(op: TokenKind, expr: Expression) -> Expression {
        Expression::Unary {
            op,
            expr: Box::new(expr),
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        // Test addition
        let expr = make_binary(
            Expression::Integer(5),
            TokenKind::Plus,
            Expression::Integer(3)
        );
        assert_eq!(const_fold(expr), Some(Expression::Integer(8)));

        // Test subtraction
        let expr = make_binary(
            Expression::Integer(5),
            TokenKind::Minus,
            Expression::Integer(3)
        );
        assert_eq!(const_fold(expr), Some(Expression::Integer(2)));

        // Test multiplication
        let expr = make_binary(
            Expression::Integer(5),
            TokenKind::Star,
            Expression::Integer(3)
        );
        assert_eq!(const_fold(expr), Some(Expression::Integer(15)));

        // Test division
        let expr = make_binary(
            Expression::Integer(6),
            TokenKind::Slash,
            Expression::Integer(2)
        );
        assert_eq!(const_fold(expr), Some(Expression::Integer(3)));

        // Test division by zero
        let expr = make_binary(
            Expression::Integer(6),
            TokenKind::Slash,
            Expression::Integer(0)
        );
        assert_eq!(const_fold(expr), None);

        // Test modulo
        let expr = make_binary(
            Expression::Integer(7),
            TokenKind::Modulo,
            Expression::Integer(4)
        );
        assert_eq!(const_fold(expr), Some(Expression::Integer(3)));
    }

    #[test]
    fn test_float_arithmetic() {
        // Test addition
        let expr = make_binary(
            Expression::Float(5.0),
            TokenKind::Plus,
            Expression::Float(3.5)
        );
        assert_eq!(const_fold(expr), Some(Expression::Float(8.5)));

        // Test division
        let expr = make_binary(
            Expression::Float(6.0),
            TokenKind::Slash,
            Expression::Float(2.0)
        );
        assert_eq!(const_fold(expr), Some(Expression::Float(3.0)));

        // Test division by zero
        let expr = make_binary(
            Expression::Float(6.0),
            TokenKind::Slash,
            Expression::Float(0.0)
        );
        assert_eq!(const_fold(expr), None);
    }

    #[test]
    fn test_boolean_operations() {
        // Test AND
        let expr = make_binary(
            Expression::Bool(true),
            TokenKind::And,
            Expression::Bool(false)
        );
        assert_eq!(const_fold(expr), Some(Expression::Bool(false)));

        // Test OR
        let expr = make_binary(
            Expression::Bool(true),
            TokenKind::Or,
            Expression::Bool(false)
        );
        assert_eq!(const_fold(expr), Some(Expression::Bool(true)));
    }

    #[test]
    fn test_comparisons() {
        // Test integer comparisons
        let expr = make_binary(
            Expression::Integer(5),
            TokenKind::Lt,
            Expression::Integer(10)
        );
        assert_eq!(const_fold(expr), Some(Expression::Bool(true)));

        // Test float comparisons
        let expr = make_binary(
            Expression::Float(5.0),
            TokenKind::GtEq,
            Expression::Float(5.0)
        );
        assert_eq!(const_fold(expr), Some(Expression::Bool(true)));

        // Test string comparisons
        let expr = make_binary(
            Expression::String("hello".to_string()),
            TokenKind::EqEq,
            Expression::String("hello".to_string())
        );
        assert_eq!(const_fold(expr), Some(Expression::Bool(true)));

        // Test char comparisons
        let expr = make_binary(
            Expression::Char('a'),
            TokenKind::Lt,
            Expression::Char('b')
        );
        assert_eq!(const_fold(expr), Some(Expression::Bool(true)));
    }

    #[test]
    fn test_unary_operations() {
        // Test integer negation
        let expr = make_unary(
            TokenKind::Minus,
            Expression::Integer(42)
        );
        assert_eq!(const_fold(expr), Some(Expression::Integer(-42)));

        // Test float negation
        let expr = make_unary(
            TokenKind::Minus,
            Expression::Float(3.14)
        );
        assert_eq!(const_fold(expr), Some(Expression::Float(-3.14)));

        // Test logical not
        let expr = make_unary(
            TokenKind::Bang,
            Expression::Bool(true)
        );
        assert_eq!(const_fold(expr), Some(Expression::Bool(false)));

        // Test nested unary
        let expr = make_unary(
            TokenKind::Bang,
            make_unary(
                TokenKind::Bang,
                Expression::Bool(true)
            )
        );
        assert_eq!(const_fold(expr), Some(Expression::Bool(true)));
    }

    #[test]
    fn test_invalid_operations() {
        // Test invalid binary operation (string + integer)
        let expr = make_binary(
            Expression::String("hello".to_string()),
            TokenKind::Plus,
            Expression::Integer(42)
        );
        assert_eq!(const_fold(expr), None);

        // Test invalid unary operation (logical not on integer)
        let expr = make_unary(
            TokenKind::Bang,
            Expression::Integer(42)
        );
        assert_eq!(const_fold(expr), None);

        // Test invalid division
        let expr = make_binary(
            Expression::Integer(42),
            TokenKind::Slash,
            Expression::Integer(0)
        );
        assert_eq!(const_fold(expr), None);
    }
}

