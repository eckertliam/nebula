use std::iter::Peekable;
use std::str::Chars;

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

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
    pub lexeme: Option<String>,
}

fn simple_token(kind: TokenKind, loc: Loc) -> Token {
    Token {
        kind,
        loc,
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
        loc: start_loc,
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
        loc: start_loc,
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
                    loc: start_loc,
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
                                loc: start_loc,
                                lexeme: Some(c.to_string()),
                            };
                            tokens.push(error_token);
                            return;
                        }
                    }
                } else {
                    let error_token = Token {
                        kind: TokenKind::Error,
                        loc: start_loc,
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
        loc: start_loc,
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
            '+' => tokens.push(simple_token(TokenKind::Plus, loc)),
            '-' => if let Some('>') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::Arrow, loc));
            } else {
                tokens.push(simple_token(TokenKind::Minus, loc));
            }
            '*' => tokens.push(simple_token(TokenKind::Star, loc)),
            '/' => tokens.push(simple_token(TokenKind::Slash, loc)),
            '%' => tokens.push(simple_token(TokenKind::Percent, loc)),
            '^' => tokens.push(simple_token(TokenKind::Caret, loc)),
            '=' => if let Some('=') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::EqEq, loc));
            } else {
                tokens.push(simple_token(TokenKind::Eq, loc));
            }
            '!' => if let Some('=') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::BangEq, loc));
            } else {
                tokens.push(simple_token(TokenKind::Bang, loc));
            }
            '<' => if let Some('=') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::LtEq, loc));
            } else {
                tokens.push(simple_token(TokenKind::Lt, loc));
            }
            '>' => if let Some('=') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::GtEq, loc));
            } else {
                tokens.push(simple_token(TokenKind::Gt, loc));
            }
            '(' => tokens.push(simple_token(TokenKind::LParen, loc)),
            ')' => tokens.push(simple_token(TokenKind::RParen, loc)),
            '{' => tokens.push(simple_token(TokenKind::LBrace, loc)),
            '}' => tokens.push(simple_token(TokenKind::RBrace, loc)),
            '[' => tokens.push(simple_token(TokenKind::LBracket, loc)),
            ']' => tokens.push(simple_token(TokenKind::RBracket, loc)),
            ',' => tokens.push(simple_token(TokenKind::Comma, loc)),
            '.' => tokens.push(simple_token(TokenKind::Dot, loc)),
            ':' => if let Some(':') = chars.peek() {
                chars.next();
                loc.column += 1;
                tokens.push(simple_token(TokenKind::ColonColon, loc));
            } else {
                tokens.push(simple_token(TokenKind::Colon, loc));
            }
            ';' => tokens.push(simple_token(TokenKind::Semicolon, loc)),
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
                    loc,
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