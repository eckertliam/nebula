use crate::frontend::ast::{Bool, Char, Float, Ident, Int, Parse, Str, UnaryExpr};

use super::{ast::Expr, located::Located, parser::{Parser, Precedence}, token::TokenKind};

use std::sync::LazyLock;

pub type PrefixFn<'src> = fn(Parser<'src>) -> Option<Located<Expr>>;
pub type InfixFn<'src> = fn(Parser<'src>, Located<Expr>) -> Option<Located<Expr>>;

#[derive(Debug, Clone, Copy)]
pub struct ParseRule<'src> {
    pub prefix: Option<PrefixFn<'src>>,
    pub infix: Option<InfixFn<'src>>,
    pub precedence: Precedence,
}

impl<'src> TokenKind {
    pub fn get_parse_rule(&self) -> ParseRule<'src> {
        unsafe {
            std::mem::transmute(PARSE_RULES[*self as usize])
        }
    }
}



macro_rules! or {
    ($first:expr, $second:expr) => {
        |parser| $first(parser).or_else(|| $second(parser))
    };
}

static PARSE_RULES: LazyLock<[ParseRule<'static>; TokenKind::COUNT as usize]> = LazyLock::new(|| {
    let mut rules = [ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    }; TokenKind::COUNT as usize];

    // utility macro to add a parse rule to the rules array
    macro_rules! parse_rule {
        ($token_kind:expr, $prefix:expr, $infix:expr, $precedence:expr) => {
            rules[$token_kind as usize] = ParseRule {
                prefix: $prefix,
                infix: $infix,
                precedence: $precedence,
            };
        };
    }

    parse_rule!(TokenKind::Number, Some(or!(Int::parse, Float::parse)), None, Precedence::None);
    parse_rule!(TokenKind::True, Some(Bool::parse), None, Precedence::None);
    parse_rule!(TokenKind::False, Some(Bool::parse), None, Precedence::None);
    parse_rule!(TokenKind::Char, Some(Char::parse), None, Precedence::None);
    parse_rule!(TokenKind::String, Some(Str::parse), None, Precedence::None);
    parse_rule!(TokenKind::Ident, Some(Ident::parse), None, Precedence::None);
    parse_rule!(TokenKind::Not, Some(UnaryExpr::parse), None, Precedence::None);
    // TODO: add infix rule for minus
    parse_rule!(TokenKind::Minus, Some(UnaryExpr::parse), None, Precedence::Term);
    
    rules    
});

