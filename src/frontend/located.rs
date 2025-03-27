use std::fmt::{Debug, Display};

pub struct Located<T: Display + Debug> {
    pub column: usize,
    pub line: usize,
    pub value: T,
}

impl<T: Display + Debug> Located<T> {
    pub fn new(column: usize, line: usize, value: T) -> Self {
        Self { column, line, value }
    }
}

impl<T: Display + Debug> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.column, self.value)
    }
}

impl<T: Display + Debug> Debug for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.column, self.value)
    }
}

