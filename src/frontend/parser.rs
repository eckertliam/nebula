use crate::frontend::lexer::{lex, Token, TokenKind};
use crate::frontend::located::Located;
use crate::frontend::ast::*;

struct Parser<'src> {
    program: Program,
    errors: Vec<Located<String>>,
    tokens: Vec<Located<Token<'src>>>,
    start: usize,
    current: usize,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: Vec<Located<Token<'src>>>) -> Self {
        Self {
            program: Program::new(),
            errors: Vec::new(),
            tokens,
            start: 0,
            current: 0,
        }
    }

    /*
     * Parse the tokens into a program.
     */
    pub fn parse(&mut self) -> Result<&Program, &Vec<Located<String>>> {
        // TODO: implement parsing loop
        if self.errors.len() > 0 {
            return Err(&self.errors);
        }

        Ok(&self.program)
    }

    /*
     * Peek at the current token.
     */
    fn peek(&self) -> &Located<Token<'src>> {
        &self.tokens[self.current]
    }

    /*
     * Advance to the next token and return it.
     */
    fn advance(&mut self) -> &Located<Token<'src>> {
        let token = &self.tokens[self.current];
        self.current += 1;
        token
    }
    
    /*
     * Match a token of a certain kind and return it if it matches.
     * Otherwise, return None.
     */
    fn match_token(&mut self, kind: TokenKind) -> Option<&Located<Token<'src>>> {
        let token = &self.tokens[self.current];
        if token.value.kind == kind {
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }

    /*
     * Expect a token of a certain kind and return it if it matches.
     * Otherwise, add an error to the error list and return None.
     */
    fn expect(&mut self, kind: TokenKind) -> Option<&Located<Token<'src>>> {
        let token = &self.tokens[self.current];
        if token.value.kind == kind {
            self.current += 1;
            Some(token)
        } else {
            let message = format!("Expected {} found {} at {}:{}", kind, token.value, token.column, token.line);
            self.errors.push(Located::new(token.column, token.line, message));
            None
        }
    }
}
