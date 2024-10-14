use std::fmt::Display;

// A Loc represents a point in the source file
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Loc {
    // The line number in the source file
    pub line: u32,
    // The column number in the source file
    pub column: u32,
}

impl Loc {
    pub fn new(line: u32, col: u32) -> Loc {
        Loc { line, column: col }
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Into<Span> for Loc {
    fn into(self) -> Span {
        Span::single(self)
    }
}

// A span represents a range in the source file
#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    // The start location of the span
    pub start: Loc,
    // The end location of the span
    pub end: Loc,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start == self.end {
            write!(f, "{}", self.start)
        } else {
            write!(f, "{}-{}", self.start, self.end)
        }
    }
}

impl Span {
    pub fn new(start: Loc, end: Loc) -> Span {
        Span { start, end }
    }

    pub fn single(loc: Loc) -> Span {
        Span { start: loc.clone(), end: loc }
    }
}