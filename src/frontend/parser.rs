use crate::frontend::lexer::{lex, Token, TokenKind};
use crate::frontend::located::Located;
use crate::frontend::ast::*;

struct Parser<'src> {
    /// The parsed program
    program: Program,
    /// List of errors encountered during parsing
    errors: Vec<String>,
    /// Has the parser errored?
    errored: bool,
    /// Avoid a cascade of errors by entering failure mode
    /// exit once the statement is parsed
    failure_mode: bool,
    /// The tokens to parse
    tokens: Vec<Located<Token<'src>>>,
    /// The index of the current token
    current: usize,
    /// The index of the previous token
    previous: usize,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: Vec<Located<Token<'src>>>) -> Self {
        Self {
            program: Program::new(),
            errors: Vec::new(),
            errored: false,
            failure_mode: false,
            tokens,
            current: 0,
            previous: 0,
        }
    }

    fn get_current(&self) -> Located<Token<'src>> {
        self.tokens[self.current]
    }

    fn get_previous(&self) -> Located<Token<'src>> {
        self.tokens[self.previous]
    }

    fn advance(&mut self) {
        self.previous = self.current;
        self.current += 1;
    }

    /*
     * Error at a specific token index
     */
    fn error_at(&mut self, token_idx: usize, message: &str) {
        // if in failure mode dont add more errors
        if self.failure_mode {
            return;
        }

        let token = &self.tokens[token_idx];
        let emsg = format!("[{}:{}] Error: {} at {}", token.column, token.line, message, token.value);
        self.errors.push(emsg);
        self.errored = true;
        self.failure_mode = true;
    }

    /*
     * Error at the previous token
     */
    fn error_at_previous(&mut self, message: &str) {
        self.error_at(self.previous, message);
    }

    /*
     * Error at the current token
     */
    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message);
    }

    /*
     * Consume a token
     * Returns true if the token was of the expected kind
     * Returns false and adds an error if the token was not of the expected kind
     */
    fn consume(&mut self, kind: TokenKind, msg: &str) -> bool {
        if self.get_current().value.kind == kind {
            self.advance();
            true
        } else {
            self.error_at_current(msg);
            false
        }
    }

    /* 
     * Parse a literal 
     * Returns Some literal if there is a valid literal
     * Otherwise returns None and enters an error state
     */
    fn literal(&mut self) -> Option<Literal> {
        let token = self.get_current();
        match token.value.kind {
            TokenKind::Number => {
                self.advance();
                // determine if the number is an int or a float
                if let Ok(integer) = token.value.lexeme.parse::<i64>() {
                    Some(Literal::Int(integer))
                } else if let Ok(float) = token.value.lexeme.parse::<f64>() {
                    Some(Literal::Float(float))
                } else {
                    self.error_at_current("Invalid number");
                    None
                }
            }
            TokenKind::True => {
                self.advance();
                Some(Literal::Bool(true))
            }
            TokenKind::False => {
                self.advance();
                Some(Literal::Bool(false))
            }
            TokenKind::Char => {
                self.advance();
                let ch = token.value.lexeme.chars().next();
                if let Some(ch) = ch {
                    Some(Literal::Char(ch))
                } else {
                    self.error_at_current("Invalid character");
                    None
                }
            }
            TokenKind::String => {
                self.advance();
                let str = token.value.lexeme;
                Some(Literal::String(str.to_string()))
            }
            _ => {
                self.error_at_current("Expected literal");
                None
            }
        }
    }
}
