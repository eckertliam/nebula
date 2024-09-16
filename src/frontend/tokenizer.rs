pub enum TokenKind {
    Ident,
    Number,
    String,
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    // Punctuation
    Comma,
    Dot,
    Colon,
    ColonColon,
    Semicolon,
    // Keywords
    Const,
    Let,
    If,
    Else,
    Match,
    While,
    For,
    In,
    Fn,
    Return,
    True,
    False,
    Type,
    Struct,
    Enum,
    Trait,
    Impl,
    Import,
    From,
    As,
    Export,
    // Special
    Eof,
    Error,
} 

pub struct Loc {
    pub line: u32,
    pub column: u32,
}

pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
    pub lexeme: String,
}

/// Tokenizer: A simple iterator that takes a source string and returns a sequence of tokens
pub struct Tokenizer {
    source: String,
    idx: usize,
    line: u32,
    column: u32,
}

impl Tokenizer {
    pub fn new(src: &str) -> Tokenizer {
        Tokenizer {
            source: src.to_string(),
            idx: 0,
            line: 1,
            column: 1,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.idx)
    }

    fn advance(&mut self) {
        if let Some(ch) = self.peek() {
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.idx += 1;
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_arith() {
        let src = "1 + 2 * 3 - 4 / 5";
        let mut tokenizer = Tokenizer::new(src);
        let expected = vec![
            TokenKind::Number,
            TokenKind::Plus,
            TokenKind::Number,
            TokenKind::Star,
            TokenKind::Number,
            TokenKind::Minus,
            TokenKind::Number,
            TokenKind::Slash,
            TokenKind::Number,
        ];
        for kind in expected {
            let token = tokenizer.next().unwrap();
            assert_eq!(token.kind, kind);
        }
    }
}