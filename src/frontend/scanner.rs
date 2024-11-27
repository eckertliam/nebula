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
    Identifier,
    // Special
    Eof,
    Error,
    Empty,
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

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();

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
                '-' => self.make_token(TokenKind::Minus),
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
                c if c.is_alphabetic() || c == '_' => self.scan_identifier(),
                c if c.is_digit(10) => self.scan_number(),
                _ => self.error_token("Unexpected character"),
            }
        } else {
            self.make_token(TokenKind::Eof)
        }
    }
}
