use crate::frontend::lexer::{lex, Token, TokenKind};
use crate::frontend::located::Located;
use crate::frontend::ast::*;

macro_rules! expected_token {
    ($expected_kind:expr, $got_token:expr) => {
        {
            let line = $got_token.line;
            let column = $got_token.column;
            let token = $got_token.value;
            let msg = format!("[{}:{}] Error: Expected {} but got {}", line, column, $expected_kind, token);
            Err(msg)
        }
    }
}

type TokenStream<'src> = &'src [Located<Token<'src>>];

type ParseResult<'src, T> = Result<(TokenStream<'src>, T), String>;

fn or<'src, T>(a: ParseResult<'src, T>, b: ParseResult<'src, T>) -> ParseResult<'src, T> {
    match a {
        Ok(result) => Ok(result),
        Err(_) => b,
    }
}

fn parse_number(tokens: TokenStream) -> ParseResult<Located<Expr>> {
    let loc_tok = tokens.first().ok_or("Unexpected EOF while parsing number")?;
    let token = loc_tok.value;

    match token.kind {
        TokenKind::Number => or(parse_int(tokens), parse_float(tokens)),
        _ => expected_token!("number", loc_tok),
    }
}

fn parse_int(tokens: TokenStream) -> ParseResult<Located<Expr>> {
    let loc_tok = &tokens[0];
    let token = loc_tok.value;

    if let Ok(n) = token.lexeme.parse::<i64>() {
        Ok((&tokens[1..], Located::new(loc_tok.column, loc_tok.line, Expr::Int(n))))
    } else {
        expected_token!("integer", loc_tok)
    }
}

fn parse_float(tokens: TokenStream) -> ParseResult<Located<Expr>> {
    let loc_tok = &tokens[0];
    let token = loc_tok.value;

    if let Ok(n) = token.lexeme.parse::<f64>() {
        Ok((&tokens[1..], Located::new(loc_tok.column, loc_tok.line, Expr::Float(n))))
    } else {
        expected_token!("float", loc_tok)
    }
}

fn parse_bool(tokens: TokenStream) -> ParseResult<Located<Expr>> {
    let loc_tok = &tokens[0];

    match loc_tok.value.kind {
        TokenKind::True => Ok((&tokens[1..], Located::new(loc_tok.column, loc_tok.line, Expr::Bool(true)))),
        TokenKind::False => Ok((&tokens[1..], Located::new(loc_tok.column, loc_tok.line, Expr::Bool(false)))),
        _ => expected_token!("boolean", loc_tok),
    }
}

fn parse_char(tokens: TokenStream) -> ParseResult<Located<Expr>> {
    let loc_tok = tokens.first().ok_or("Unexpected EOF while parsing char")?;
    let token = loc_tok.value;

    match token.kind {
        TokenKind::Char => {
            if let Some(c) = token.lexeme.chars().next() {
                Ok((&tokens[1..], Located::new(loc_tok.column, loc_tok.line, Expr::Char(c))))
            } else {
                expected_token!("character", loc_tok)
            }
        }
        _ => expected_token!("character", loc_tok),
    }
}

fn parse_string(tokens: TokenStream) -> ParseResult<Located<Expr>> {
    let loc_tok = tokens.first().ok_or("Unexpected EOF while parsing string")?;
    let token = loc_tok.value;

    match token.kind {
        TokenKind::String => Ok((&tokens[1..], Located::new(loc_tok.column, loc_tok.line, Expr::String(token.lexeme.to_string())))),
        _ => expected_token!("string", loc_tok),
    }
}


fn parse_variable(tokens: TokenStream) -> ParseResult<Located<Expr>> {
    let loc_tok = tokens.first().ok_or("Unexpected EOF while parsing variable")?;
    let token = loc_tok.value;

    match token.kind {
        TokenKind::Ident => Ok((&tokens[1..], Located::new(loc_tok.column, loc_tok.line, Expr::Var(token.lexeme.to_string())))),
        _ => expected_token!("variable", loc_tok),
    }
}


