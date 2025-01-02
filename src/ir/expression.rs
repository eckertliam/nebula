#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Identifier(String),
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    }
}