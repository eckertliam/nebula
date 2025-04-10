use std::fmt::Display;

use super::located::Located;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate)enum TokenKind {
    // Keywords
    Const,
    Let,
    Class,
    Type,
    If,
    Else,
    Match,
    Loop,
    Break,
    Continue,
    Return,
    True,
    False,
    Begin,
    End,
    And,
    Or,
    Not,
    Fn,
    This,
    Super,
    Extends,
    Pub,
    // Punctuation
    Colon,
    LBracket,
    RBracket,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    // Literals
    Ident,
    Number,
    String,
    Char,
    // Special
    Newline,
    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        match self {
            Const => write!(f, "const"),
            Let => write!(f, "let"),
            Class => write!(f, "class"),
            Type => write!(f, "type"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Match => write!(f, "match"),
            Loop => write!(f, "loop"),
            Break => write!(f, "break"),
            Continue => write!(f, "continue"),
            Return => write!(f, "return"),
            True => write!(f, "true"),
            False => write!(f, "false"),
            Begin => write!(f, "begin"),
            End => write!(f, "end"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Not => write!(f, "not"),
            Fn => write!(f, "fn"),
            This => write!(f, "this"),
            Super => write!(f, "super"),
            Extends => write!(f, "extends"),
            Pub => write!(f, "pub"),
            Colon => write!(f, ":"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Percent => write!(f, "%"),
            Lt => write!(f, "<"),
            Leq => write!(f, "<="),
            Gt => write!(f, ">"),
            Geq => write!(f, ">="),
            Eq => write!(f, "=="),
            Neq => write!(f, "!="),
            Ident => write!(f, "<ident>"),
            Number => write!(f, "<number>"),
            String => write!(f, "<string>"),
            Char => write!(f, "<char>"),
            Newline => write!(f, "<newline>"),
            Eof => write!(f, "<eof>"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Token<'src> {
    pub(crate) kind: TokenKind,
    pub(crate) lexeme: &'src str,
}

impl<'src> Token<'src> {
    pub(crate) fn new(kind: TokenKind, lexeme: &'src str) -> Self {
        Self { kind, lexeme }
    }

    pub(crate) fn located(kind: TokenKind, lexeme: &'src str, column: usize, line: usize) -> Located<Self> {
        Located::new(column, line, Self::new(kind, lexeme))
    }
}

// override the display for the token to print the lexeme if it is a literal
impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Ident => write!(f, "{}", self.lexeme),
            TokenKind::Number => write!(f, "{}", self.lexeme),
            TokenKind::String => write!(f, "{}", self.lexeme),
            TokenKind::Char => write!(f, "{}", self.lexeme),
            _ => write!(f, "{}", self.kind),
        }
    }
}