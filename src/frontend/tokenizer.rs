use std::{fmt::Display, iter::Peekable};
use std::str::Chars;

use super::{Loc, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Number,
    String,
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    // Punctuation
    Comma,
    Dot,
    Colon,
    ColonColon,
    Semicolon,
    Arrow,
    FatArrow,
    // Keywords
    Const,
    Let,
    If,
    Else,
    Match,
    While,
    For,
    In,
    Fn,
    Return,
    True,
    False,
    Type,
    Struct,
    Enum,
    Trait,
    Impl,
    Import,
    From,
    As,
    Export,
    And,
    Or,
    // Special
    Eof,
    Error,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::Ident => "Ident",
            TokenKind::Number => "Number",
            TokenKind::String => "String",
            TokenKind::Plus => "Plus",
            TokenKind::Minus => "Minus",
            TokenKind::Star => "Star",
            TokenKind::Slash => "Slash",
            TokenKind::Percent => "Percent",
            TokenKind::Caret => "Caret",
            TokenKind::Eq => "Eq",
            TokenKind::EqEq => "EqEq",
            TokenKind::Bang => "Bang",
            TokenKind::BangEq => "BangEq",
            TokenKind::Lt => "Lt",
            TokenKind::LtEq => "LtEq",
            TokenKind::Gt => "Gt",
            TokenKind::GtEq => "GtEq",
            TokenKind::LParen => "LParen",
            TokenKind::RParen => "RParen",
            TokenKind::LBrace => "LBrace",
            TokenKind::RBrace => "RBrace",
            TokenKind::LBracket => "LBracket",
            TokenKind::RBracket => "RBracket",
            TokenKind::Comma => "Comma",
            TokenKind::Dot => "Dot",
            TokenKind::Colon => "Colon",
            TokenKind::ColonColon => "ColonColon",
            TokenKind::Semicolon => "Semicolon",
            TokenKind::Arrow => "Arrow",
            TokenKind::FatArrow => "FatArrow",
            TokenKind::Const => "Const",
            TokenKind::Let => "Let",
            TokenKind::If => "If",
            TokenKind::Else => "Else",
            TokenKind::Match => "Match",
            TokenKind::While => "While",
            TokenKind::For => "For",
            TokenKind::In => "In",
            TokenKind::Fn => "Fn",
            TokenKind::Return => "Return",
            TokenKind::True => "True",
            TokenKind::False => "False",
            TokenKind::Type => "Type",
            TokenKind::Struct => "Struct",
            TokenKind::Enum => "Enum",
            TokenKind::Trait => "Trait",
            TokenKind::Impl => "Impl",
            TokenKind::Import => "Import",
            TokenKind::From => "From",
            TokenKind::As => "As",
            TokenKind::Export => "Export",
            TokenKind::And => "And",
            TokenKind::Or => "Or",
            TokenKind::Eof => "Eof",
            TokenKind::Error => "Error",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: Option<String>,
}

fn simple_token(kind: TokenKind, span: Span) -> Token {
    Token {
        kind,
        span,
        lexeme: None,
    }
}

/// Check if a symbol is a keyword
fn check_keyword(symbol: &str) -> TokenKind {
    match symbol {
        "const" => TokenKind::Const,
        "let" => TokenKind::Let,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "match" => TokenKind::Match,
        "while" => TokenKind::While,
        "for" => TokenKind::For,
        "in" => TokenKind::In,
        "fn" => TokenKind::Fn,
        "return" => TokenKind::Return,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "type" => TokenKind::Type,
        "struct" => TokenKind::Struct,
        "enum" => TokenKind::Enum,
        "trait" => TokenKind::Trait,
        "impl" => TokenKind::Impl,
        "import" => TokenKind::Import,
        "from" => TokenKind::From,
        "as" => TokenKind::As,
        "export" => TokenKind::Export,
        _ => TokenKind::Ident,
    }
}

fn tokenize_symbol(lead_ch: char, loc: &mut Loc, chars: &mut Peekable<Chars<'_>>, tokens: &mut Vec<Token>) {
    let start_loc = loc.clone();
    let mut symbol = String::from(lead_ch);
    while let Some(&c) = chars.peek() {
        if c.is_alphanumeric() || c == '_' {
            symbol.push(chars.next().unwrap());
            loc.column += 1;
        } else {
            break;
        }
    }
    let kind = check_keyword(&symbol);
    tokens.push(Token {
        kind,
        span: Span::new(start_loc, loc.clone()),
        lexeme: Some(symbol),
    });
}

fn tokenize_number(lead_ch: char, loc: &mut Loc, chars: &mut Peekable<Chars<'_>>, tokens: &mut Vec<Token>) {
    let start_loc = loc.clone();
    let mut num_str = String::from(lead_ch);
    while let Some(&c) = chars.peek() {
        if c.is_digit(10) {
            num_str.push(chars.next().unwrap());
            loc.column += 1;
        } else {
            break;
        }
    }
    if let Some('.') = chars.peek() {
        num_str.push(chars.next().unwrap());
        while let Some(&c) = chars.peek() {
            if c.is_digit(10) {
                num_str.push(chars.next().unwrap());
                loc.column += 1;
            } else {
                break;
            }
        }
    }
    tokens.push(Token {
        kind: TokenKind::Number,
        span: Span::new(start_loc, loc.clone()),
        lexeme: Some(num_str),
    });
}

fn tokenize_string(loc: &mut Loc, chars: &mut Peekable<Chars<'_>>, tokens: &mut Vec<Token>) {
    let start_loc = loc.clone();
    let mut str_val = String::new();
    while let Some(c) = chars.next() {
        loc.column += 1;
        match c {
            '"' => {
                tokens.push(Token {
                    kind: TokenKind::String,
                    span: Span::new(start_loc, loc.clone()),
                    lexeme: Some(str_val),
                });
                return;
            }
            '\\' => {
                if let Some(c) = chars.next() {
                    loc.column += 1;
                    match c {
                        'n' => str_val.push('\n'),
                        't' => str_val.push('\t'),
                        '\\' => str_val.push('\\'),
                        '"' => str_val.push('"'),
                        _ => {
                            let error_token = Token {
                                kind: TokenKind::Error,
                                span: Span::new(start_loc, loc.clone()),
                                lexeme: Some(c.to_string()),
                            };
                            tokens.push(error_token);
                            return;
                        }
                    }
                } else {
                    let error_token = Token {
                        kind: TokenKind::Error,
                        span: Span::new(start_loc, loc.clone()),
                        lexeme: Some("Expected character after '\\'".to_string()),
                    };
                    tokens.push(error_token);
                    return;
                }
            }
            _ => str_val.push(c),
        }
    }
    let error_token = Token {
        kind: TokenKind::Error,
        span: Span::new(start_loc, loc.clone()),
        lexeme: Some("Unterminated string".to_string()),
    };
    tokens.push(error_token);
}

#[inline]
pub fn tokenize(src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut loc = Loc { line: 1, column: 1 };
    let mut chars = src.chars().peekable();
    while let Some(c) = chars.next() {
        loc.column += 1;        
        match c {
            '+' => tokens.push(simple_token(TokenKind::Plus, loc.into())),
            '-' => if let Some('>') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::Arrow, loc.into()));
            } else {
                tokens.push(simple_token(TokenKind::Minus, loc.into()));
            }
            '*' => tokens.push(simple_token(TokenKind::Star, loc.into())),
            '/' => tokens.push(simple_token(TokenKind::Slash, loc.into())),
            '%' => tokens.push(simple_token(TokenKind::Percent, loc.into())),
            '^' => tokens.push(simple_token(TokenKind::Caret, loc.into())),
            '=' => if let Some('=') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::EqEq, loc.into()));
            } else {
                tokens.push(simple_token(TokenKind::Eq, loc.into()));
            }
            '!' => if let Some('=') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::BangEq, loc.into()));
            } else {
                tokens.push(simple_token(TokenKind::Bang, loc.into()));
            }
            '<' => if let Some('=') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::LtEq, loc.into()));
            } else {
                tokens.push(simple_token(TokenKind::Lt, loc.into()));
            }
            '>' => if let Some('=') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::GtEq, loc.into()));
            } else {
                tokens.push(simple_token(TokenKind::Gt, loc.into()));
            }
            '(' => tokens.push(simple_token(TokenKind::LParen, loc.into())),
            ')' => tokens.push(simple_token(TokenKind::RParen, loc.into())),
            '{' => tokens.push(simple_token(TokenKind::LBrace, loc.into())),
            '}' => tokens.push(simple_token(TokenKind::RBrace, loc.into())),
            '[' => tokens.push(simple_token(TokenKind::LBracket, loc.into())),
            ']' => tokens.push(simple_token(TokenKind::RBracket, loc.into())),
            ',' => tokens.push(simple_token(TokenKind::Comma, loc.into())),
            '.' => tokens.push(simple_token(TokenKind::Dot, loc.into())),
            ':' => if let Some(':') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::ColonColon, loc.into()));
            } else {
                tokens.push(simple_token(TokenKind::Colon, loc.into()));
            }
            ';' => tokens.push(simple_token(TokenKind::Semicolon, loc.into())),
            'a'..='z' | 'A'..='Z' | '_' => tokenize_symbol(c, &mut loc, &mut chars, &mut tokens),
            '0'..='9' => tokenize_number(c, &mut loc, &mut chars, &mut tokens),
            '"' => tokenize_string(&mut loc, &mut chars, &mut tokens),
            '\n' => {
                loc.line += 1;
                loc.column = 1;
            }
            '\t' => loc.column += 3,
            ' ' => (),
            _ => {
                let error_token = Token {
                    kind: TokenKind::Error,
                    span: loc.into(),
                    lexeme: Some(c.to_string()),
                };
                tokens.push(error_token);
            }
        };
    }
    return tokens;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_arith_test() {
        let src = "1 + 2 - 3 * 4 / 5 % 6 ^ 7";
        let tokens = tokenize(src);
        let expected = vec![
            TokenKind::Number,
            TokenKind::Plus,
            TokenKind::Number,
            TokenKind::Minus,
            TokenKind::Number,
            TokenKind::Star,
            TokenKind::Number,
            TokenKind::Slash,
            TokenKind::Number,
            TokenKind::Percent,
            TokenKind::Number,
            TokenKind::Caret,
            TokenKind::Number,
        ];
        assert_eq!(tokens.iter().map(|t| t.kind).collect::<Vec<TokenKind>>(), expected);
    }
}