use std::fmt::Display;

pub struct Loc {
    pub line: usize,
    pub col: usize,
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Default for Loc {
    fn default() -> Self {
        Self { line: 1, col: 1 }
    }
}