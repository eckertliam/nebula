use crate::frontend::lexer::{lex, Token, TokenKind};
use crate::frontend::located::Located;
use crate::frontend::ast::*;

struct Parser<'src> {
    tokens: &'src [Located<Token<'src>>],
    current: &'src Located<Token<'src>>,
    previous: &'src Located<Token<'src>>,
    panic_mode: bool,
    had_error: bool,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: &'src [Located<Token<'src>>]) -> Self {
        Self { 
            tokens,
            current: &tokens[0],
            previous: &tokens[0],
            panic_mode: false,
            had_error: false,
        }
    }

    pub fn advance(&mut self) {
        self.previous = self.current;
        (self.current, self.tokens) = self.tokens.split_first().unwrap();
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

    pub fn error_at(&mut self, token: &Located<Token<'src>>, msg: &str) {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
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
    fn increment(&self) -> Self {
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

type ParseFn<'src> = fn(&'src mut Parser<'src>) -> Option<Located<Expr>>;

#[derive(Debug, Clone, Copy)]
struct ParseRule<'src> {
    prefix: Option<ParseFn<'src>>,
    infix: Option<ParseFn<'src>>,
    precedence: Precedence,
}

fn get_parse_rule<'src>(kind: TokenKind) -> ParseRule<'src> {
    // TODO: implement cases for all tokens involved in parsing expressions
    match kind {
        TokenKind::True | TokenKind::False => ParseRule {
            prefix: Some(parse_boolean),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Ident => ParseRule {
            prefix: Some(parse_ident),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Number => ParseRule {
            prefix: Some(parse_number),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::String => ParseRule {
            prefix: Some(parse_string),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Char => ParseRule {
            prefix: Some(parse_char),
            infix: None,
            precedence: Precedence::None,
        },
        _ => unimplemented!()
    }
}

fn parse_boolean<'src>(parser: &'src mut Parser<'src>) -> Option<Located<Expr>> {
    let line = parser.previous.line;
    let column = parser.previous.column;
    let value = parser.previous.value.kind == TokenKind::True;
    Some(Located::new(column, line, Expr::Bool(value)))
}

fn parse_ident<'src>(parser: &'src mut Parser<'src>) -> Option<Located<Expr>> {
    let line = parser.previous.line;
    let column = parser.previous.column;
    let lexeme = parser.previous.value.lexeme;
    Some(Located::new(column, line, Expr::Var(lexeme.to_string())))
}

fn parse_number<'src>(parser: &'src mut Parser<'src>) -> Option<Located<Expr>> {
    let line = parser.previous.line;
    let column = parser.previous.column;
    let lexeme = parser.previous.value.lexeme;
    if let Ok(value) = lexeme.parse::<i64>() {
        Some(Located::new(column, line, Expr::Int(value)))
    } else if let Ok(value) = lexeme.parse::<f64>() {
        Some(Located::new(column, line, Expr::Float(value)))
    } else {
        parser.error_at(parser.previous, "Expected number");
        None
    }
}

fn parse_string<'src>(parser: &'src mut Parser<'src>) -> Option<Located<Expr>> {
    let line = parser.previous.line;
    let column = parser.previous.column;
    let lexeme = parser.previous.value.lexeme;
    Some(Located::new(column, line, Expr::String(lexeme.to_string())))
}

fn parse_char<'src>(parser: &'src mut Parser<'src>) -> Option<Located<Expr>> {
    let line = parser.previous.line;
    let column = parser.previous.column;
    let lexeme = parser.previous.value.lexeme;
    if lexeme.len() != 1 {
        parser.error_at(parser.previous, "Expected character");
        None
    } else {
        Some(Located::new(column, line, Expr::Char(lexeme.chars().next().unwrap())))
    }
}
