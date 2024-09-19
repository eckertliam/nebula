use std::collections::HashMap;
use crate::frontend::{AstType, EnumDeclaration, AstExpression, Program, StatementNode, StructDeclaration, TypeNode};
use crate::nir::context::Context;
use crate::nir::ty::Type;

use super::expression::Expression;

pub fn transform_program(program: Program) -> Context {
    let mut context = Context::new();
    for statement in program.statements {
        match statement.kind {
            StatementNode::StructDeclaration(struct_decl) => match transform_struct_decl(struct_decl.clone(), &mut context) {
                Ok(ty) => context.push_type(struct_decl.name, ty),
                Err(e) => eprintln!("error transforming struct declaration: {}", e),
            }
            StatementNode::EnumDeclaration(enum_decl) => match transform_enum_decl(enum_decl.clone(), &mut context) {
                Ok(ty) => context.push_type(enum_decl.name, ty),
                Err(e) => eprintln!("error transforming enum declaration: {}", e),
            }
            _ => {
                unimplemented!("transforming AST to NIR is a WIP and does not yet support this statement: {:?}", statement);
            }
        }
    }
    context
}

fn transform_struct_decl(struct_decl: StructDeclaration, context: &mut Context) -> Result<Type, String> {
    let mut fields = HashMap::new();
    for (name, ty) in struct_decl.fields {
        match transform_ty(ty, context) {
            Ok(ty) => fields.insert(name, ty),
            Err(e) => return Err(e),
        };
    }
    Ok(Type::Struct(fields))
}

fn transform_enum_decl(enum_decl: EnumDeclaration, context: &mut Context) -> Result<Type, String> {
    let mut variants = HashMap::new();
    for (name, types) in enum_decl.variants {
        let mut variant_fields = Vec::new();
        for ty in types {
            match transform_ty(ty, context) {
                Ok(ty) => variant_fields.push(ty),
                Err(e) => return Err(e),
            }
        }
        variants.insert(name, variant_fields);
    }
    Ok(Type::Enum(variants))
}

fn transform_ty(ty: AstType, context: &mut Context) -> Result<Type, String> {
    match &ty.kind {
        TypeNode::Base(base_ty) => match base_ty.as_str() {
            "i8" => Ok(Type::I8),
            "i16" => Ok(Type::I16),
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "u8" => Ok(Type::U8),
            "u16" => Ok(Type::U16),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            "bool" => Ok(Type::Bool),
            _ => {
                // check if it is a user defined type
                if let Some(user_ty) = context.get_type(base_ty.as_str()) {
                    Ok(user_ty)
                } else {
                    // add the symbol to the unresolved types
                    Ok(Type::Unresolved(base_ty.clone(), format!("type {} is not defined at {:?}", base_ty, ty.loc)))
                }
            }
        }
        TypeNode::Array(elem_ty, size_ast_expr) => match transform_ty(*elem_ty.clone(), context) {
            // TODO: come up with a better way to handle this resolving the size_ast_expr into a constant
            Ok(ty) => {
                let err_msg = format!("size of array {:?} must be reducible to a constant at compile time", ty);
                match transform_expression(size_ast_expr.clone(), context) {
                    Ok(size_expr) => if let Some(reduced_expr) = size_expr.eval(&context) {
                        match reduced_expr {
                            Expression::Int(int_size) => if int_size >= 0 {
                                Ok(Type::Array(Box::new(ty), int_size as u64))
                            } else {
                                Err(format!("size of array {:?} must be greater than or equal to 0", ty))
                            }
                            Expression::Unsigned(uint_size) => Ok(Type::Array(Box::new(ty), uint_size)),
                            _ => Err(err_msg),
                        }
                    } else {
                        Err(err_msg)
                    }
                    Err(e) => Err(e),
                }
            }
            Err(e) => Err(e),
        }
        _ => unimplemented!("unknown type: {:?} not yet supported or does not exist", ty),
    }
}

fn transform_expression(expr: AstExpression, context: &mut Context) -> Result<Expression, String> {
    unimplemented!()
}