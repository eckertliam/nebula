use std::collections::HashMap;
use crate::frontend::{AstExpression, AstType, EnumDeclaration, ExpressionNode, Program, StatementNode, StructDeclaration, TypeNode};
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
        TypeNode::Array(elem_ty, size_ast_expr) => transform_array_ty(*elem_ty.clone(), size_ast_expr.clone(), context),
        _ => {
            // TODO: implement remaining types
            unimplemented!("transforming type {:?} is not yet supported", ty);
        }
    }
}

fn transform_expression(ast_expr: AstExpression, context: &mut Context) -> Result<Expression, String> {
    let unfolded_res = match &ast_expr.kind {

        ExpressionNode::Int(val) => Ok(Expression::Int(*val)),
        ExpressionNode::Float(val) => Ok(Expression::Float(*val)),
        ExpressionNode::True => Ok(Expression::Bool(true)),
        ExpressionNode::False => Ok(Expression::Bool(false)),
        ExpressionNode::String(val) => Ok(Expression::String(val.clone())),
        ExpressionNode::Identifier(ident) => Ok(Expression::Ident(ident.clone())),
        ExpressionNode::Add(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Add(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Sub(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Sub(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Mul(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Mul(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Div(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Div(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Mod(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Mod(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Equal(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Equal(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::NotEqual(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::NotEqual(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Less(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Less(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::LessEqual(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::LessEqual(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Greater(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Greater(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::GreaterEqual(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::GreaterEqual(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::And(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::And(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Or(lhs_ast, rhs_ast) => match (transform_expression(*lhs_ast.clone(), context), transform_expression(*rhs_ast.clone(), context)) {
            (Ok(lhs), Ok(rhs)) => Ok(Expression::Or(Box::new(lhs), Box::new(rhs))),
            (Err(e), _) => Err(e),
            (_, Err(e)) => Err(e),
        }
        ExpressionNode::Array(exprs) => {
            let mut new_exprs = Vec::new();
            for expr in exprs {
                match transform_expression(expr.clone(), context) {
                    Ok(expr) => new_exprs.push(expr),
                    Err(e) => return Err(e),
                }
            }
            Ok(Expression::Array(new_exprs))
        }
    };
    if let Ok(unfolded) = unfolded_res.clone() {
        // try to fold the expression
        if let Some(folded) = unfolded.fold() {
            return Ok(folded)
        }
    }
    unfolded_res
}

fn transform_array_ty(ast_elem_ty: AstType, size_ast_expr: AstExpression, context: &mut Context) -> Result<Type, String> {
    let elem_ty = match transform_ty(ast_elem_ty, context) {
        Ok(ty) => ty,
        Err(e) => return Err(e),
    };
    // try to fold the size expression to a constant integer or unsigned
    // first make the error message 
    let const_size_expr_err = format!("array size expression {:?} could not be folded to a constant at {:?}", size_ast_expr, size_ast_expr.loc);
    let size: u64 = if let Ok(unfolded_expr) = transform_expression(size_ast_expr.clone(), context) {
        match unfolded_expr {
            Expression::Int(size) => {
                if size > 0 {
                    size as u64
                } else {
                    return Err(format!("array size must be greater than 0, found {} at {:?}", size, size_ast_expr.loc));
                }
            }
            Expression::Unsigned(size) => size,
            _ => return Err(const_size_expr_err),
        }
    } else {
        return Err(const_size_expr_err);
    };
    Ok(Type::Array(Box::new(elem_ty), size))
}