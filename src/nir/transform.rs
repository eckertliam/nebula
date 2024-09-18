use std::collections::HashMap;
use crate::frontend::{AstType, Program, StatementNode, StructDeclaration, EnumDeclaration, TypeNode};
use crate::nir::context::Context;
use crate::nir::ty::Type;

pub fn transform_program(program: Program) -> Context {
    let mut context = Context::new();
    for statement in program.statements {
        match statement.kind {
            StatementNode::StructDeclaration(struct_decl) => {
                context.push_type(struct_decl.name.clone(), transform_struct_decl(struct_decl, &context));
            }
            StatementNode::EnumDeclaration(enum_decl) => {
                context.push_type(enum_decl.name.clone(), transform_enum_decl(enum_decl, &context));
            }
            _ => {
                unimplemented!("transforming AST to NIR is a WIP and does not yet support this statement: {:?}", statement);
            }
        }
    }
    context
}

fn transform_struct_decl(struct_decl: StructDeclaration, context: &Context) -> Type {
    let mut fields = HashMap::new();
    for (name, ty) in struct_decl.fields {
        fields.insert(name, transform_ty(ty, context));
    }
    Type::Struct(fields)
}

fn transform_enum_decl(enum_decl: EnumDeclaration, context: &Context) -> Type {
    let mut variants = HashMap::new();
    for (name, types) in enum_decl.variants {
        let mut variant_fields = Vec::new();
        for ty in types {
            variant_fields.push(transform_ty(ty, context));
        }
        variants.insert(name, variant_fields);
    }
    Type::Enum(variants)
}

fn transform_ty(ty: AstType, context: &Context) -> Type {
    match ty.kind {
        TypeNode::Base(base_ty) => match base_ty.as_str() {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            _ => unimplemented!("unknown base type: {}", base_ty),
        }
        _ => unimplemented!("unknown type: {:?} not yet supported or does not exist", ty),
    }
}