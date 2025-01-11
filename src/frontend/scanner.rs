use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    ColonColon,
    Arrow,
    FatArrow,
    Plus,
    Minus,
    Star,
    Slash,
    Modulo,
    Bang,
    Eq,
    EqEq,
    BangEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    // Keywords
    And,
    Or,
    If,
    Else,
    True,
    False,
    Return,
    Const,
    Let,
    Fn,
    While, 
    Loop,
    For,
    In,
    Break,
    Continue,
    // Literals
    Number,
    String,
    Char,
    Identifier,
    // Special
    Eof,
    Error,
    Empty,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::ColonColon => write!(f, "::"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::FatArrow => write!(f, "=>"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Modulo => write!(f, "%"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::BangEq => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Const => write!(f, "const"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::While => write!(f, "while"),
            TokenKind::Loop => write!(f, "loop"),
            TokenKind::For => write!(f, "for"),
            TokenKind::In => write!(f, "in"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Continue => write!(f, "continue"),
            TokenKind::Number => write!(f, "<number>"),
            TokenKind::String => write!(f, "<string>"),
            TokenKind::Char => write!(f, "<char>"),
            TokenKind::Identifier => write!(f, "<identifier>"),
            TokenKind::Eof => write!(f, "<eof>"),
            TokenKind::Error => write!(f, "<error>"),
            TokenKind::Empty => write!(f, "<empty>"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub line: usize,
}

impl<'a> Default for Token<'a> {
    fn default() -> Self {
        Self { kind: TokenKind::Empty, lexeme: "", line: 0 }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        self.source.chars().nth(self.current + 1)
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek();
        self.current += 1;
        ch
    }

    fn make_token(&self, kind: TokenKind) -> Token<'a> {
        Token {
            kind,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static str) -> Token<'a> {
        Token {
            kind: TokenKind::Error,
            lexeme: &message,
            line: self.line,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                if ch == '\n' {
                    self.line += 1;
                }
                self.advance();
            } else {
                break;
            }
        }
    }

    fn scan_identifier(&mut self) -> Token<'a> {
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        
        let lexeme = &self.source[self.start..self.current];
        let kind = match lexeme{
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "return" => TokenKind::Return,
            "const" => TokenKind::Const,
            "let" => TokenKind::Let,
            "fn" => TokenKind::Fn,
            "while" => TokenKind::While,
            "loop" => TokenKind::Loop,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            _ => TokenKind::Identifier,
        };

        Token {
            kind,
            lexeme,
            line: self.line,
        }
    }

    fn scan_number(&mut self) -> Token<'a> {
        while let Some(ch) = self.peek() {
            if ch.is_digit(10) {
                self.advance();
            } else {
                break;
            }
        }

        if let Some('.') = self.peek() {
            if let Some(ch) = self.peek_next() {
                if ch.is_digit(10) {
                    // Consume the dot and the digit
                    self.current += 2;
                    while let Some(ch) = self.peek() {
                        if ch.is_digit(10) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        self.make_token(TokenKind::Number)
    }

    fn scan_string(&mut self) -> Token<'a> {
        while let Some(ch) = self.advance() {
            if ch == '"' {
                break;
            } else if ch == '\n' {
                self.line += 1;
            } 
            // TODO: Handle escape sequences
        }

        self.make_token(TokenKind::String)
    }

    fn scan_char(&mut self) -> Token<'a> {
        while let Some(ch) = self.advance() {
            if ch == '\'' {
                break;
            }
        }

        self.make_token(TokenKind::Char)
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;

        if let Some(ch) = self.advance() {
            match ch {
                '(' => self.make_token(TokenKind::LeftParen),
                ')' => self.make_token(TokenKind::RightParen),
                '{' => self.make_token(TokenKind::LeftBrace),
                '}' => self.make_token(TokenKind::RightBrace),
                '[' => self.make_token(TokenKind::LeftBracket),
                ']' => self.make_token(TokenKind::RightBracket),
                ',' => self.make_token(TokenKind::Comma),
                '.' => self.make_token(TokenKind::Dot),
                ';' => self.make_token(TokenKind::Semicolon),
                ':' => {
                    if let Some(':') = self.peek() {
                        self.advance();
                        self.make_token(TokenKind::ColonColon)
                    } else {
                        self.make_token(TokenKind::Colon)
                    }
                }
                '+' => self.make_token(TokenKind::Plus),
                '-' => {
                    if let Some('>') = self.peek() {
                        self.advance();
                        self.make_token(TokenKind::Arrow)
                    } else {
                        self.make_token(TokenKind::Minus)
                    }
                }
                '*' => self.make_token(TokenKind::Star),
                '/' => self.make_token(TokenKind::Slash),
                '%' => self.make_token(TokenKind::Modulo),
                '!' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.make_token(TokenKind::BangEq)
                    } else {
                        self.make_token(TokenKind::Bang)
                    }
                }
                '=' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.make_token(TokenKind::EqEq)
                    } else if let Some('>') = self.peek() {
                        self.advance();
                        self.make_token(TokenKind::FatArrow)
                    } else {
                        self.make_token(TokenKind::Eq)
                    }
                }
                '<' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.make_token(TokenKind::LtEq)
                    } else {
                        self.make_token(TokenKind::Lt)
                    }
                }
                '>' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.make_token(TokenKind::GtEq)
                    } else {
                        self.make_token(TokenKind::Gt)
                    }
                }
                '"' => self.scan_string(),
                '\'' => self.scan_char(),
                c if c.is_alphabetic() || c == '_' => self.scan_identifier(),
                c if c.is_digit(10) => self.scan_number(),
                _ => self.error_token("Unexpected character"),
            }
        } else {
            Token {
                kind: TokenKind::Eof,
                lexeme: "",
                line: self.line,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_token {
        ($scanner:expr, $kind:expr, $lexeme:expr) => {
            let token = $scanner.scan_token();
            assert_eq!(token.kind, $kind);
            assert_eq!(token.lexeme, $lexeme);
        };
    }

    macro_rules! assert_tokens {
        ($scanner:expr, $tokens:expr) => {
            for (kind, lexeme) in $tokens {
                assert_token!($scanner, kind, lexeme);
            }
        };
    }

    #[test]
    fn test_scan_token() {
        let mut scanner = Scanner::new("let x = 10;");
        assert_tokens!(scanner, [
            (TokenKind::Let, "let"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Eq, "="),
            (TokenKind::Number, "10"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Eof, ""),
        ]);
    }

    #[test]
    fn test_numbers() {
        let mut scanner = Scanner::new("123 45.67 0.123 42.");
        assert_tokens!(scanner, [
            (TokenKind::Number, "123"),
            (TokenKind::Number, "45.67"),
            (TokenKind::Number, "0.123"),
            (TokenKind::Number, "42"),
            (TokenKind::Dot, "."),
            (TokenKind::Eof, ""),
        ]);
    }

    #[test]
    fn test_strings() {
        let mut scanner = Scanner::new("\"hello world\" \"multi\nline\"");
        let token1 = scanner.scan_token();
        assert_eq!(token1.kind, TokenKind::String);
        assert_eq!(token1.lexeme, "\"hello world\"");
        assert_eq!(token1.line, 1);

        let token2 = scanner.scan_token();
        assert_eq!(token2.kind, TokenKind::String);
        assert_eq!(token2.lexeme, "\"multi\nline\"");
        assert_eq!(token2.line, 2);
    }

    #[test]
    fn test_chars() {
        let mut scanner = Scanner::new("'a' 'b'");
        assert_tokens!(scanner, [
            (TokenKind::Char, "'a'"),
            (TokenKind::Char, "'b'"),
            (TokenKind::Eof, ""),
        ]);
    }

    #[test]
    fn test_keywords() {
        let mut scanner = Scanner::new("if else while loop for in break continue fn let const return true false and or");
        assert_tokens!(scanner, [
            (TokenKind::If, "if"),
            (TokenKind::Else, "else"),
            (TokenKind::While, "while"),
            (TokenKind::Loop, "loop"),
            (TokenKind::For, "for"),
            (TokenKind::In, "in"),
            (TokenKind::Break, "break"),
            (TokenKind::Continue, "continue"),
            (TokenKind::Fn, "fn"),
            (TokenKind::Let, "let"),
            (TokenKind::Const, "const"),
            (TokenKind::Return, "return"),
            (TokenKind::True, "true"),
            (TokenKind::False, "false"),
            (TokenKind::And, "and"),
            (TokenKind::Or, "or"),
            (TokenKind::Eof, ""),
        ]);
    }

    #[test]
    fn test_compound_operators() {
        let mut scanner = Scanner::new("== != <= >= :: -> =>");
        assert_tokens!(scanner, [
            (TokenKind::EqEq, "=="),
            (TokenKind::BangEq, "!="),
            (TokenKind::LtEq, "<="),
            (TokenKind::GtEq, ">="),
            (TokenKind::ColonColon, "::"),
            (TokenKind::Arrow, "->"),
            (TokenKind::FatArrow, "=>"),
            (TokenKind::Eof, ""),
        ]);
    }

    #[test]
    fn test_identifiers() {
        let mut scanner = Scanner::new("x _y z123 hello_world");
        assert_tokens!(scanner, [
            (TokenKind::Identifier, "x"),
            (TokenKind::Identifier, "_y"),
            (TokenKind::Identifier, "z123"),
            (TokenKind::Identifier, "hello_world"),
            (TokenKind::Eof, ""),
        ]);
    }

    #[test]
    fn test_whitespace_and_newlines() {
        let mut scanner = Scanner::new("let\n  x\t=\r\n10;");
        let tokens = vec![
            scanner.scan_token(),
            scanner.scan_token(),
            scanner.scan_token(),
            scanner.scan_token(),
            scanner.scan_token(),
        ];
        
        assert_eq!(tokens[0].line, 1); // let
        assert_eq!(tokens[1].line, 2); // x
        assert_eq!(tokens[2].line, 2); // =
        assert_eq!(tokens[3].line, 3); // 10
        assert_eq!(tokens[4].line, 3); // ;
    }

    #[test]
    fn test_error_handling() {
        let mut scanner = Scanner::new("@#$");
        assert_token!(scanner, TokenKind::Error, "Unexpected character");
    }
}
