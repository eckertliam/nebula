use crate::frontend::located::Located;
use crate::frontend::ast::*;

use super::{parse_rule::ParseRule, token::{Token, TokenKind}};

#[derive(Debug, Clone, Copy)]
pub struct Parser<'src> {
    tokens: &'src [Located<Token<'src>>],
    pub current: Located<Token<'src>>,
    pub previous: Located<Token<'src>>,
    panic_mode: bool,
    had_error: bool,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: &'src [Located<Token<'src>>]) -> Self {
        Self { 
            tokens,
            current: tokens[0],
            previous: tokens[0],
            panic_mode: false,
            had_error: false,
        }
    }

    pub fn advance(&mut self) {
        self.previous = self.current;
        let (first, rest) = self.tokens.split_first().unwrap();
        self.current = *first;
        self.tokens = rest;
    }

    pub fn consume(&mut self, kind: TokenKind, msg: &str) -> bool {
        if self.current.value.kind == kind {
            self.advance();
            true
        } else {
            self.error_at(self.current, msg);
            false
        }
    }

    pub fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.current.value.kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }
    pub fn error_at(&mut self, token: Located<Token<'src>>, msg: &str) {
        // short circuit if we're already in panic mode
        if self.panic_mode {
            return;
        }

        // enter panic mode
        self.panic_mode = true;
        // construct error message
        eprintln!("[{}:{}] Error at {}: {}", token.line, token.column, token.value, msg);
        // set had_error to true
        self.had_error = true;
    }

    pub fn current_rule(&self) -> ParseRule<'src> {
        self.current.value.kind.get_parse_rule()
    }

    pub fn previous_rule(&self) -> ParseRule<'src> {
        self.previous.value.kind.get_parse_rule()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    pub fn increment(&self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => Self::Primary,
        }
    }
}
