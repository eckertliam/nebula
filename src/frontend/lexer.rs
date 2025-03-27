use std::fmt::Display;

use super::located::Located;

#[derive(Debug)]
pub(crate)enum TokenKind {
    // Keywords
    Const,
    Let,
    Record,
    Enum,
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
    End,
    And,
    Or,
    Not,
    Fn,
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
            Record => write!(f, "record"),
            Enum => write!(f, "enum"),
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
            End => write!(f, "end"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Not => write!(f, "not"),
            Fn => write!(f, "fn"),
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

#[derive(Debug)]
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

macro_rules! token_branch {
    ($lexer:expr, $char_cond:expr, $true_kind:expr, $false_kind:expr) => {
        if let Some($char_cond) = $lexer.peek() {
            $lexer.advance();
            $lexer.take_token($true_kind)
        } else {
            $lexer.take_token($false_kind)
        }
    };
}

// basically an iterator over the tokens
struct Lexer<'src> {
    src: &'src str,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

impl<'src> Lexer<'src> {
    pub(crate) fn new(src: &'src str) -> Self {
        Self { src, start: 0, current: 0, line: 1, column: 1 }
    }

    fn peek(&self) -> Option<char> {
        self.src.chars().nth(self.current)
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.current += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(c)
    }

    fn peek_next(&self) -> Option<char> {
        self.src.chars().nth(self.current + 1)
    }

    fn take_lexeme(&mut self) -> &'src str {
        let lexeme: &'src str = &self.src[self.start..self.current];
        self.start = self.current;
        lexeme
    }

    fn take_token(&mut self, kind: TokenKind) -> Result<Located<Token<'src>>, Located<String>> {
        // capture the lexeme
        let lexeme: &'src str = self.take_lexeme();
        // create the token
        let token: Located<Token<'src>> = Token::located(kind, lexeme, self.column, self.line);
        // reset the start
        self.start = self.current;
        // return the token
        Ok(token)
    }

    fn take_error(&mut self, message: String) -> Located<String> {
        Located::new(self.column, self.line, message)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' || c == '\r' || c == '\n' {
                self.advance();
            } else {
                break;
            }
        }
    }
    


    fn next_token(&mut self) -> Result<Located<Token<'src>>, Located<String>> {
        // take the next character or return an EOF if it is None
        self.skip_whitespace();

        let c: char = match self.advance() {
            Some(c) => c,
            None => return self.take_token(TokenKind::Eof),
        };

        match c {
            // skip irrelevant characters
            ' ' | '\t' | '\r' => self.next_token(),
            '\n' => self.take_token(TokenKind::Newline),
            '(' => self.take_token(TokenKind::LParen),
            ')' => self.take_token(TokenKind::RParen),
            '[' => self.take_token(TokenKind::LBracket),
            ']' => self.take_token(TokenKind::RBracket),
            '{' => self.take_token(TokenKind::LBrace),
            '}' => self.take_token(TokenKind::RBrace),
            ',' => self.take_token(TokenKind::Comma),
            '.' => self.take_token(TokenKind::Dot),
            ':' => self.take_token(TokenKind::Colon),
            '+' => self.take_token(TokenKind::Plus),
            '-' => self.take_token(TokenKind::Minus),
            '*' => self.take_token(TokenKind::Star),
            '/' => self.take_token(TokenKind::Slash),
            '%' => self.take_token(TokenKind::Percent),
            '<' => token_branch!(self, '=', TokenKind::Leq, TokenKind::Lt),
            '>' => token_branch!(self, '=', TokenKind::Geq, TokenKind::Gt),
            '=' => token_branch!(self, '=', TokenKind::Eq, TokenKind::Neq),
            '!' => token_branch!(self, '=', TokenKind::Neq, TokenKind::Not),
            '"' => self.take_string(),
            '\'' => self.take_char(),
            '0'..='9' => self.take_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.take_ident(),
            _ => Err(self.take_error(format!("Unexpected character: {}", c))),
        }
    }

    fn take_string(&mut self) -> Result<Located<Token<'src>>, Located<String>> {
        while let Some(c) = self.advance() {
            if c == '"' { // end of string
                break;
            } else if c == '\\' { // escape character
                if let None = self.advance() {
                    return Err(Located::new(self.column, self.line, "Unexpected EOF while reading string".to_string()));
                }
            }
        }
        let lexeme = &self.src[self.start + 1..self.current - 1];
        Ok(Token::located(TokenKind::String, lexeme, self.column, self.line))
    }
    
    fn take_char(&mut self) -> Result<Located<Token<'src>>, Located<String>> {
        let mut escaped = false;
        if let Some(c) = self.advance() {
            if c == '\\' {
                escaped = true;
                if let None = self.advance() {
                    return Err(Located::new(self.column, self.line, "Unexpected EOF while reading char".to_string()));
                }
            }
        } else {
            return Err(Located::new(self.column, self.line, "Unexpected EOF while reading char".to_string()));
        }
        if let Some('\'') = self.peek() {
            self.advance();
        } else {
            return Err(Located::new(self.column, self.line, "Unclosed char literal".to_string()));
        }
        let lexeme: &'src str;
        if escaped {
            // the lexeme is the character after the escape character
            lexeme = &self.src[self.start + 2..self.current - 1];
        } else {
            // the lexeme is the character
            lexeme = &self.src[self.start + 1..self.current - 1];
        }
        Ok(Token::located(TokenKind::Char, lexeme, self.column, self.line))
    }
    
    fn take_number(&mut self) -> Result<Located<Token<'src>>, Located<String>> {
        let mut floated = false;
        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                self.advance();
            } else if c == '.' && ! floated {
                floated = true;
                self.advance();
            } else {
                break;
            }
        }
        return self.take_token(TokenKind::Number);
    }
    
    fn take_ident(&mut self) -> Result<Located<Token<'src>>, Located<String>> {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let lexeme = &self.src[self.start..self.current];
        let kind = match lexeme {
            "const" => TokenKind::Const,
            "let" => TokenKind::Let,
            "record" => TokenKind::Record,
            "enum" => TokenKind::Enum,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "loop" => TokenKind::Loop,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "end" => TokenKind::End,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "fn" => TokenKind::Fn,
            "type" => TokenKind::Type,
            "match" => TokenKind::Match,
            _ => TokenKind::Ident,
        };
        Ok(Token::located(kind, lexeme, self.column, self.line))
    }
}
