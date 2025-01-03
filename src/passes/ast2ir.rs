// Converts an AST to an IR

use crate::ir;
use crate::frontend::{ast, TokenKind};

/// Converts an AST expression to an IR expression
fn expr_to_ir(expr: &ast::Expression) -> ir::Expression {
    match expr {
        // for numeric literals we narrow down the type to the smallest possible
        ast::Expression::Integer(i) => {
            match *i {
                i if i >= i8::MIN as i64 && i <= i8::MAX as i64 => ir::Expression::I8(i as i8),
                i if i >= i16::MIN as i64 && i <= i16::MAX as i64 => ir::Expression::I16(i as i16),
                i if i >= i32::MIN as i64 && i <= i32::MAX as i64 => ir::Expression::I32(i as i32),
                i => ir::Expression::I64(i),
            }
        }
        ast::Expression::UnsignedInteger(i) => {
            match *i {
                i if i >= u8::MIN as u64 && i <= u8::MAX as u64 => ir::Expression::U8(i as u8),
                i if i >= u16::MIN as u64 && i <= u16::MAX as u64 => ir::Expression::U16(i as u16),
                i if i >= u32::MIN as u64 && i <= u32::MAX as u64 => ir::Expression::U32(i as u32),
                i => ir::Expression::U64(i),
            }
        }
        ast::Expression::Float(f) => {
            match *f {
                f if f >= f32::MIN as f64 && f <= f32::MAX as f64 => ir::Expression::F32(f as f32),
                f => ir::Expression::F64(f),
            }
        }
        ast::Expression::Bool(b) => ir::Expression::Bool(*b),
        ast::Expression::String(s) => ir::Expression::Str(s.clone()),
        ast::Expression::Char(c) => ir::Expression::Char(*c),
        ast::Expression::Identifier(i) => ir::Expression::Identifier(i.clone()),
        ast::Expression::Call(c) => ir::Expression::Call {
            callee: Box::new(expr_to_ir(&c.callee)),
            args: c.args.iter().map(|e| expr_to_ir(e)).collect(),
        },
        ast::Expression::Binary(b) => {// convert binary expressions to calls
            let callee: String = match b.op {
                TokenKind::Plus => "add".to_string(),
                TokenKind::Minus => "sub".to_string(),
                TokenKind::Star => "mul".to_string(),
                TokenKind::Slash => "div".to_string(),
                TokenKind::Modulo => "mod".to_string(),
                TokenKind::EqEq => "eq".to_string(),
                TokenKind::BangEq => "neq".to_string(),
                TokenKind::Lt => "lt".to_string(),
                TokenKind::Gt => "gt".to_string(),
                TokenKind::LtEq => "lte".to_string(),
                TokenKind::GtEq => "gte".to_string(),
                TokenKind::And => "and".to_string(),
                TokenKind::Or => "or".to_string(),
                _ => unreachable!(),
            };
            ir::Expression::Call {
                callee: Box::new(ir::Expression::Identifier(callee)),
                args: vec![expr_to_ir(&b.lhs), expr_to_ir(&b.rhs)],
            }
        }
        ast::Expression::Unary(u) => {// convert unary expressions to calls
            let callee: String = match u.op {
                TokenKind::Minus => "neg".to_string(),
                TokenKind::Bang => "not".to_string(),
                _ => unreachable!(),
            };
            ir::Expression::Call {
                callee: Box::new(ir::Expression::Identifier(callee)),
                args: vec![expr_to_ir(&u.expr)],
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::CallExpr;

    use super::*;

    #[test]
    fn test_expr_to_ir() {
        let expr = ast::Expression::Integer(1);
        let ir_expr = expr_to_ir(&expr);
        assert_eq!(ir_expr, ir::Expression::I8(1));
        let expr = ast::Expression::Call(ast::CallExpr {
            callee: Box::new(ast::Expression::Identifier("add".to_string())),
            args: vec![ast::Expression::Integer(1), ast::Expression::Integer(2)],
        });
        let ir_expr = expr_to_ir(&expr);
        assert_eq!(ir_expr, ir::Expression::Call {
            callee: Box::new(ir::Expression::Identifier("add".to_string())),
            args: vec![ir::Expression::I8(1), ir::Expression::I8(2)],
        });
        let expr = ast::Expression::Binary(ast::BinaryExpr {
            lhs: Box::new(ast::Expression::Integer(1)),
            op: TokenKind::Plus,
            rhs: Box::new(ast::Expression::Integer(2)),
        });
        let ir_expr = expr_to_ir(&expr);
        assert_eq!(ir_expr, ir::Expression::Call {
            callee: Box::new(ir::Expression::Identifier("add".to_string())),
            args: vec![ir::Expression::I8(1), ir::Expression::I8(2)],
        });
    }
}
