mod const_fold;
mod type_pass;

pub use const_fold::const_fold;
pub use type_pass::type_check_program;