use super::{located::Located, token::{Token, TokenKind}};

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
            "class" => TokenKind::Class,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "loop" => TokenKind::Loop,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "begin" => TokenKind::Begin,
            "end" => TokenKind::End,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "fn" => TokenKind::Fn,
            "type" => TokenKind::Type,
            "match" => TokenKind::Match,
            "this" => TokenKind::This,
            "super" => TokenKind::Super,
            "extends" => TokenKind::Extends,
            "pub" => TokenKind::Pub,
            _ => TokenKind::Ident,
        };
        Ok(Token::located(kind, lexeme, self.column, self.line))
    }
}

pub(crate) fn lex(src: &str) -> Result<Vec<Located<Token>>, Vec<Located<String>>> {
    let mut lexer = Lexer::new(src);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    loop {
        match lexer.next_token() {
            Ok(token) => {
                if token.value.kind == TokenKind::Eof {
                    tokens.push(token);
                    break;
                } else {
                    tokens.push(token);
                }
            }
            Err(error) => errors.push(error),
        }
    }
    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}