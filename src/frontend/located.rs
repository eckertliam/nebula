#[derive(Debug, Clone, Copy)]
pub struct Located<T> {
    pub column: usize,
    pub line: usize,
    pub value: T,
}

impl<T> Located<T> {
    pub fn new(column: usize, line: usize, value: T) -> Self {
        Self { column, line, value }
    }
}

