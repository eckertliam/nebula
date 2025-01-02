use crate::ir::{Type, TypeEnv, TypeVarGen};

/// Applied to IR nodes to infer and type check them
pub trait Typeable {
    /// Infer the type of a node given 
    fn infer_type(&self, type_env: &TypeEnv, type_var_gen: &mut TypeVarGen) -> Type;
    /// Type check a node given an expected type
    fn type_check(&self, type_env: &TypeEnv, expected_type: Type) -> bool;
}
