// Converts an AST to an IR

use crate::ir::{self, Context, Type};
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

/// Converts AST statement to IR instruction
fn stmt_to_ir(stmt: &ast::Statement) -> ir::Instruction {
    match stmt {
        ast::Statement::CallStmt(call) => ir::Instruction::Call {
            callee: Box::new(expr_to_ir(&call.callee)),
            args: call.args.iter().map(|e| expr_to_ir(e)).collect(),
        },
        ast::Statement::ConstDecl(decl) => ir::Instruction::VarDecl {
            mutable: false,
            name: decl.name.clone(),
            ty: decl.ty.clone(),
            value: expr_to_ir(&decl.value),
        },
        ast::Statement::LetDecl(decl) => ir::Instruction::VarDecl {
            mutable: true,
            name: decl.name.clone(),
            ty: decl.ty.clone(),
            value: expr_to_ir(&decl.value),
        },
        // TODO: handle mutate statements
        // TODO: handle block statements as a function call to an anonymous function
        ast::Statement::Block(block) => todo!(),
        ast::Statement::FunctionDecl(decl) => {
            let name = decl.name.clone();
            let params = decl.params.clone();
            let return_type = decl.return_ty.clone();
            let func = ir::Function {
                name,
                params,
                return_type,
                body: decl.body.statements.iter().map(|s| stmt_to_ir(&s.node)).collect(),
            };
            ir::Instruction::FuncDecl(func)
        }
        ast::Statement::ReturnStmt(expr) => match expr {
            Some(e) => ir::Instruction::Return(Some(expr_to_ir(e))),
            None => ir::Instruction::Return(None),
        }
        _ => unreachable!(),
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

    #[test]
    fn test_stmt_to_ir() {
        // Test const declaration
        let const_decl = ast::Statement::ConstDecl(ast::ConstDecl {
            name: "x".to_string(),
            ty: Type::I32,
            value: ast::Expression::Integer(42),
        });
        let ir_inst = stmt_to_ir(&const_decl);
        assert_eq!(ir_inst, ir::Instruction::VarDecl {
            mutable: false,
            name: "x".to_string(),
            ty: Type::I32,
            value: ir::Expression::I8(42),
        });

        // Test let declaration
        let let_decl = ast::Statement::LetDecl(ast::LetDecl {
            name: "y".to_string(),
            ty: Type::F32,
            value: ast::Expression::Float(3.14),
        });
        let ir_inst = stmt_to_ir(&let_decl);
        assert_eq!(ir_inst, ir::Instruction::VarDecl {
            mutable: true,
            name: "y".to_string(),
            ty: Type::F32,
            value: ir::Expression::F32(3.14),
        });

        // Test function declaration
        let func_decl = ast::Statement::FunctionDecl(ast::FunctionDecl {
            name: "test_func".to_string(),
            params: vec![(String::from("x"), Type::I32)],
            return_ty: Type::I32,
            body: ast::Block {
                statements: vec![
                    ast::Located {
                        node: ast::Statement::ReturnStmt(Some(ast::Expression::Identifier("x".to_string()))),
                        line: 1,
                    }
                ],
            },
        });
        let ir_inst = stmt_to_ir(&func_decl);
        assert_eq!(ir_inst, ir::Instruction::FuncDecl(ir::Function {
            name: "test_func".to_string(),
            params: vec![(String::from("x"), Type::I32)],
            return_type: Type::I32,
            body: vec![ir::Instruction::Return(Some(ir::Expression::Identifier("x".to_string())))],
        }));

        // Test return statement
        let return_stmt = ast::Statement::ReturnStmt(Some(ast::Expression::Integer(123)));
        let ir_inst = stmt_to_ir(&return_stmt);
        assert_eq!(ir_inst, ir::Instruction::Return(Some(ir::Expression::I8(123))));

        // Test void return statement
        let void_return = ast::Statement::ReturnStmt(None);
        let ir_inst = stmt_to_ir(&void_return);
        assert_eq!(ir_inst, ir::Instruction::Return(None));

        // Test call statement
        let call_stmt = ast::Statement::CallStmt(ast::CallExpr {
            callee: Box::new(ast::Expression::Identifier("print".to_string())),
            args: vec![ast::Expression::String("hello".to_string())],
        });
        let ir_inst = stmt_to_ir(&call_stmt);
        assert_eq!(ir_inst, ir::Instruction::Call {
            callee: Box::new(ir::Expression::Identifier("print".to_string())),
            args: vec![ir::Expression::Str("hello".to_string())],
        });
    }
}
