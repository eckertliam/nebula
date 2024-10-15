use std::iter::Peekable;

use std::vec::IntoIter;

use crate::frontend::{
    tokenizer::{Token, TokenKind},
    ast::*,
    span::{Span, Loc},
};

type TokenIter = Peekable<IntoIter<Token>>;

pub fn parse(tokens: Vec<Token>) -> Result<Program, ()> {
    let mut tokens: TokenIter = tokens.into_iter().peekable();
    let mut program = Program::new();
    let mut errored = false;
    while let Some(token) = tokens.peek() {
        if token.kind == TokenKind::Eof {
            break;
        } else if token.kind == TokenKind::Error {
            eprintln!("Error: {}", token.clone().lexeme.unwrap());
        } else {
            let start_loc = token.span.start;
            let statement = match parse_statement(&mut tokens, start_loc) {
                Ok(stmt) => stmt,
                Err(e) => {
                    eprintln!("{}", e);
                    errored = true;
                    continue;
                }
            };
            program.push(statement);
        }
    }
    if errored {
        return Err(());
    } else {
        return Ok(program);
    }
}

fn parse_statement(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    match tokens.peek().unwrap().kind {
        TokenKind::Return => {
            tokens.next();
            let expr: Option<Expression> = match parse_expression(tokens) {
                Ok(e) => Some(e),
                Err(_) => None,
            };
            if let Err(e) = expect_kind(tokens, TokenKind::Semicolon) {
                return Err(e);
            }
            let end_loc = if let Some(e) = &expr {
                e.span().end
            } else {
                tokens.peek().unwrap().span.start
            };
            return Ok(Statement::Return { value: expr, span: Span::new(start_loc, end_loc) });
        }
        TokenKind::Let => parse_var_decl(tokens, true, start_loc),
        TokenKind::Const => parse_var_decl(tokens, false, start_loc),
        TokenKind::Fn => parse_fn_decl(tokens, start_loc),
        TokenKind::Struct => parse_struct_decl(tokens, start_loc),
        TokenKind::Enum => parse_enum_decl(tokens, start_loc),
        TokenKind::Trait => parse_trait_decl(tokens, start_loc),
        TokenKind::Type => parse_type_decl(tokens, start_loc),
        TokenKind::Ident | TokenKind::Number | TokenKind::String | TokenKind::True | TokenKind::False | TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => {
            let expr = match parse_expression(tokens) {
                Ok(e) => e,
                Err(e) => {
                    return Err(e);
                }
            };
            if let Err(e) = expect_kind(tokens, TokenKind::Semicolon) {
                return Err(e);
            }
            let end_loc = expr.span().end;
            return Ok(Statement::Expression { value: expr, span: Span::new(start_loc, end_loc) });
        }
        _ => {
            let token = tokens.next().unwrap();
            Err(format!("Error: Unexpected token {} at {}", token.kind, token.span))
        }
    }
}

fn expect_kind(tokens: &mut TokenIter, kind: TokenKind) -> Result<Token, String> {
    if let Some(token) = tokens.next() {
        if token.kind != kind {
            return Err(format!("Error: Expected {} but got {} at {}", kind, token.kind, token.span));
        } else {
            return Ok(token);
        }
    } else {
        return Err(format!("Error: Expected {} but got EOF", kind));
    }
}

fn parse_type(tokens: &mut TokenIter) -> Result<Type, String> {
    if let Some(token) = tokens.next() {
        let start_loc = token.span.start;
        match token.kind {
            TokenKind::Ident => {
                let type_ident = token;
                if let Some(peeked) = tokens.peek() {
                    if peeked.kind == TokenKind::Lt {
                        // parse generic type
                        tokens.next();
                        let mut generic_args = Vec::new();
                        loop {
                            let arg = match parse_type(tokens) {
                                Ok(t) => t,
                                Err(e) => {
                                    return Err(e);
                                }
                            };
                            generic_args.push(arg);
                            match tokens.peek() {
                                Some(token) => {
                                    if token.kind == TokenKind::Comma {
                                        tokens.next();
                                    } else {
                                        break;
                                    }
                                }
                                None => break,
                            }
                        }
                        if let Err(e) = expect_kind(tokens, TokenKind::Gt) {
                            return Err(e);
                        }
                        return Ok(Type::Generic { 
                            symbol: type_ident.lexeme.unwrap(), 
                            args: generic_args, 
                            span: Span::new(start_loc, tokens.peek().unwrap().span.start) 
                        });
                    } 
                }
                return Ok(Type::Basic { 
                    symbol: type_ident.lexeme.unwrap(), 
                    span: Span::new(start_loc, tokens.peek().unwrap().span.start)
                });
            }
            TokenKind::LParen => {
                // parse tuple or function type
                let mut types = Vec::new();
                loop {
                    let t = match parse_type(tokens) {
                        Ok(t) => t,
                        Err(e) => {
                            return Err(e);
                        }
                    };
                    types.push(t);
                    match tokens.peek() {
                        Some(token) => {
                            if token.kind == TokenKind::Comma {
                                tokens.next();
                            } else {
                                break;
                            }
                        }
                        None => break,
                    }
                }
                if let Err(e) = expect_kind(tokens, TokenKind::RParen) {
                    return Err(e);
                }
                if expect_kind(tokens, TokenKind::Arrow).is_ok() {
                    let return_type = match parse_type(tokens) {
                        Ok(t) => t,
                        Err(e) => {
                            return Err(e);
                        }
                    };
                    return Ok(Type::Function { 
                        args: types, 
                        ret: Box::new(return_type), 
                        span: Span::new(start_loc, tokens.peek().unwrap().span.start) })
                }
                return Ok(Type::Tuple { 
                    types: types, 
                    span: Span::new(start_loc, tokens.peek().unwrap().span.start) 
                });
            }
            TokenKind::LBracket => {
                // parse array type
                let ty = match parse_type(tokens) {
                    Ok(t) => t,
                    Err(e) => {
                        return Err(e);
                    }
                };
                let size = match parse_expression(tokens) {
                    Ok(expr) => expr,
                    Err(e) => {
                        return Err(e);
                    }
                };
                if let Ok(close) = expect_kind(tokens, TokenKind::RBracket) {
                    let end_loc = close.span.end;
                    return Ok(Type::Array { ty: Box::new(ty), size: Some(size), span: Span::new(start_loc, end_loc) });
                } else {
                    let current_tok = tokens.peek().unwrap();
                    return Err(format!("Error: Expected ] but got {} at {}", current_tok.kind, current_tok.span));
                }
            }
            _ => Err(format!("Error: Expected type but got {} at {}", token.kind, token.span)),
        }
    } else {
        return Err("Error: Expected type but got EOF".to_string());
    }
}

fn parse_expression(tokens: &mut TokenIter) -> Result<Expression, String> {
    if let Some(token) = tokens.peek() {
        let start_loc = token.span.start;
        // TODO: check for unary operators
        let lhs = match parse_primary_expression(tokens) {
            Ok(expr) => expr,
            Err(e) => {
                return Err(e);
            }
        };
        return parse_binary_expression(lhs, tokens, 0, start_loc);
    } else {
        return Err("Error: Expected expression but got EOF".to_string());
    }
}

fn parse_primary_expression(tokens: &mut TokenIter) -> Result<Expression, String> {
    if let Some(token) = tokens.next() {
        let start_loc = token.span.start;
        match token.kind {
            TokenKind::True => Ok(Expression::Bool { value: true, span: token.span }),
            TokenKind::False => Ok(Expression::Bool { value: false, span: token.span }),
            TokenKind::Ident => if let Some(symbol) = token.lexeme {
                Ok(Expression::Symbol {
                    symbol: symbol.to_string(),
                    span: token.span,
                })
            } else {
                Err(format!("Error: Symbol token is missing lexeme at {}", token.span))
            }
            TokenKind::Number => {
                let lexeme = token.lexeme.unwrap();
                if let Ok(int) = lexeme.parse::<i64>() {
                    Ok(Expression::Int { value: int, span: token.span })
                } else if let Ok(float) = lexeme.parse::<f64>() {
                    Ok(Expression::Float { value: float, span: token.span })
                } else {
                    Err(format!("Error: Failed to parse number {} at {}", lexeme, token.span))
                }
            }
            TokenKind::String => if let Some(lexeme) = token.lexeme {
                Ok(Expression::String { value: lexeme.to_string(), span: token.span })
            } else {
                Err(format!("Error: String token is missing lexeme at {}", token.span))
            },
            TokenKind::LParen => {
                let expr = parse_expression(tokens)?;
                if let Err(e) = expect_kind(tokens, TokenKind::RParen) {
                    return Err(e);
                }
                Ok(expr)
            },
            TokenKind::LBracket => {
                let mut elements = Vec::new();
                loop {
                    let expr = match parse_expression(tokens) {
                        Ok(e) => e,
                        Err(e) => {
                            return Err(e);
                        }
                    };
                    elements.push(expr);
                    match tokens.peek() {
                        Some(token) => {
                            if token.kind == TokenKind::Comma {
                                tokens.next();
                            } else {
                                break;
                            }
                        }
                        None => break,
                    }
                }
                if let Err(e) = expect_kind(tokens, TokenKind::RBracket) {
                    return Err(e);
                }
                Ok(Expression::Array { elements, span: Span::new(start_loc, tokens.peek().unwrap().span.start) })
            },
            TokenKind::LBrace => unimplemented!("Parsing of block expressions is not implemented yet"),
            _ => Err(format!("Error: Expected primary expression but got {} at {}", token.kind, token.span)),
        }
    } else {
        Err("Error: Expected primary expression but got EOF".to_string())
    }
}

fn parse_postfix_expression(lhs: Expression, tokens: &mut TokenIter) -> Result<Expression, String> {
    if let Some(token) = tokens.peek() {
        let start_loc = token.span.start;
        match token.kind {
            TokenKind::LParen => {
                tokens.next();
                let mut args = Vec::new();
                loop {
                    let arg = match parse_expression(tokens) {
                        Ok(e) => e,
                        Err(e) => {
                            return Err(e);
                        }
                    };
                    args.push(arg);
                    match tokens.peek() {
                        Some(token) => {
                            if token.kind == TokenKind::Comma {
                                tokens.next();
                            } else {
                                break;
                            }
                        }
                        None => break,
                    }
                }
                if let Err(e) = expect_kind(tokens, TokenKind::RParen) {
                    return Err(e);
                }
                Ok(Expression::Call { callee: Box::new(lhs), args, span: Span::new(start_loc, tokens.peek().unwrap().span.start) })
            }
            TokenKind::Dot => {
                tokens.next();
                let field = match expect_kind(tokens, TokenKind::Ident) {
                    Ok(token) => token.lexeme.unwrap().to_string(),
                    Err(e) => return Err(e),
                };
                let expr = Expression::FieldAccess {
                    owner: Box::new(lhs), 
                    field, 
                    span: Span::new(start_loc, tokens.peek().unwrap().span.start) 
                };
                parse_postfix_expression(expr, tokens)
            }
            TokenKind::LBracket => {
                tokens.next();
                let index = match parse_expression(tokens) {
                    Ok(e) => e,
                    Err(e) => {
                        return Err(e);
                    }
                };
                if let Err(e) = expect_kind(tokens, TokenKind::RBracket) {
                    return Err(e);
                }
                let expr = Expression::Index {
                    owner: Box::new(lhs),
                    index: Box::new(index),
                    span: Span::new(start_loc, tokens.peek().unwrap().span.start)
                };
                parse_postfix_expression(expr, tokens)
            },
            _ => Ok(lhs),
        }
    } else {
        Err("Error: Expected postfix expression but got EOF".to_string())
    }
    
}

fn parse_binary_expression(lhs: Expression, tokens: &mut TokenIter, prec: u8, loc: Loc) -> Result<Expression, String> {
    if let Some(binop_tok) = peek_binop(tokens) {
        // parse the right hand side of the binary expression
        let rhs = match parse_primary_expression(tokens) {
            Ok(expr) => expr,
            Err(e) => {
                return Err(e);
            }
        };
        // create the binary expression node
        let binary_expr = Expression::Binary { 
            op: binop_tok.kind, 
            lhs: Box::new(lhs.clone()), 
            rhs: Box::new(rhs.clone()), 
            span: Span::new(lhs.span().start, rhs.span().end) 
        };
        // check for more binary operators
        let binop_prec = binop_precedence(binop_tok.kind);
        if binop_prec > prec {
            return parse_binary_expression(binary_expr, tokens, binop_prec, loc);
        } else {
            return Ok(binary_expr);
        }
    } else {
        // there is no binary operator so just return the left hand side
        return Ok(lhs);
    }
}

/// Consumes and returns a token if it is a binary operator
fn peek_binop(tokens: &mut TokenIter) -> Option<Token> {
    if let Some(token) = tokens.peek() {
        match token.kind {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash | TokenKind::Percent | TokenKind::EqEq | TokenKind::BangEq | TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq | TokenKind::And | TokenKind::Or => {
                return Some(tokens.next().unwrap());
            }
            _ => return None,
        }
    } else {
        return None;
    }
}

fn binop_precedence(kind: TokenKind) -> u8 {
    match kind {
        TokenKind::Or | TokenKind::And => 2,
        TokenKind::EqEq | TokenKind::BangEq | TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => 5,
        TokenKind::Plus | TokenKind::Minus => 10,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 20,
        _ => 0,
    }
}

fn parse_var_decl(tokens: &mut TokenIter, is_let: bool, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    let ident = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => {
            return Err(e);
        }
    };
    let _type = if let Ok(colon) = expect_kind(tokens, TokenKind::Colon) {
        match parse_type(tokens) {
            Ok(t) => Some(t),
            Err(e) => return Err(format!("Error: Expected type after colon at {} but got {}", colon.span, e))
        }
    } else {
        None
    };
    match expect_kind(tokens, TokenKind::Eq) {
        Ok(_) => (),
        Err(e) => {
            return Err(e);
        }
    }
    let value = match parse_expression(tokens) {
        Ok(e) => e,
        Err(e) => {
            return Err(e);
        }
    };
    if let Err(e) = expect_kind(tokens, TokenKind::Semicolon) {
        return Err(e);
    }
    let span = *value.span();
    if is_let {
        return Ok(Statement::LetDecl {
            name: ident.lexeme.unwrap(),
            ty: _type,
            value,
            span,
        });
    } else {
        return Ok(Statement::ConstDecl {
            name: ident.lexeme.unwrap(),
            ty: _type,
            value,
            span,
        });
    }
}

fn parse_fn_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    let name = if let Ok(ident) = expect_kind(tokens, TokenKind::Ident) {
        if let Some(lexeme) = ident.lexeme {
            lexeme.to_string()
        } else {
            return Err(format!("Error: Identifier token is missing lexeme at {}", ident.span));
        }
    } else {
        return Err(format!("Error: Expected identifier after fn at {}", start_loc));
    };
    // check for generics
    let mut generics = Vec::new();
    if expect_kind(tokens, TokenKind::Lt).is_ok() {
        loop {
            let gen_param = parse_generic_param(tokens);
            match gen_param {
                Ok(param) => generics.push(param),
                Err(e) => {
                    return Err(e);
                }
            }
            // if there is a comma keep parsing generics
            match tokens.peek() {
                Some(token) => {
                    if token.kind == TokenKind::Comma {
                        tokens.next();
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
    }
    // expect a closing angle bracket if there were generics
    if !generics.is_empty() {
        if let Err(e) = expect_kind(tokens, TokenKind::Gt) {
            return Err(e);
        }
    }
    // parse params
    let mut args = Vec::new();
    if let Err(e) = expect_kind(tokens, TokenKind::LParen) {
        return Err(e);
    } else {
        loop {
            let arg_ident = match expect_kind(tokens, TokenKind::Ident) {
                Ok(token) => token,
                Err(e) => {
                    return Err(e);
                }
            };
            let arg_type = match parse_type(tokens) {
                Ok(t) => t,
                Err(e) => {
                    return Err(e);
                }
            };
            args.push((arg_ident.lexeme.unwrap(), arg_type));
            // keep parsing params if there is a comma
            match tokens.peek() {
                Some(token) => {
                    if token.kind == TokenKind::Comma {
                        tokens.next();
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
    }
    if let Err(e) = expect_kind(tokens, TokenKind::RParen) {
        return Err(e);
    }
    // expect an arrow
    if let Err(e) = expect_kind(tokens, TokenKind::Arrow) {
        return Err(e);
    }
    // parse the return type
    let ret = match parse_type(tokens) {
        Ok(t) => t,
        Err(e) => {
            return Err(e);
        }
    };
    // parse the body
    let mut body = Vec::new();
    if let Err(e) = expect_kind(tokens, TokenKind::LBrace) {
        return Err(e);
    } else {
        loop {
            match tokens.peek() {
                Some(token) => {
                    if token.kind == TokenKind::RBrace {
                        tokens.next();
                        break;
                    } else {
                        let inner_loc = token.span.start;
                        let stmt = match parse_statement(tokens, inner_loc) {
                            Ok(s) => s,
                            Err(e) => {
                                return Err(e);
                            }
                        };
                        body.push(stmt);
                    }
                }
                None => break,
            }
        }
    }
    // consume the closing brace
    let end_loc = match expect_kind(tokens, TokenKind::RBrace) {
        Ok(token) => token.span.end,
        Err(e) => {
            return Err(e);
        }
    };
    return Ok(Statement::FnDecl { name, generics, args, ret, body, span: Span::new(start_loc, end_loc) });
}

fn parse_generic_param(tokens: &mut TokenIter) -> Result<GenericParam, String> {
    let type_var_token = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => return Err(e),
    };
    let name = if let Some(lexeme) = type_var_token.lexeme {
        lexeme.to_string()
    } else {
        return Err(format!("Error: Generic parameter is missing lexeme at {}", type_var_token.span));
    };
    // check for a colon
    if let Ok(_) = expect_kind(tokens, TokenKind::Colon) {
        let start_loc = type_var_token.span.start;
        let mut bounds = Vec::new();
        loop {
            match tokens.peek() {
                Some(token) => {
                    match token.kind {
                        TokenKind::Comma => break,
                        _ => match parse_type(tokens) {
                            Ok(t) => bounds.push(t),
                            Err(e) => return Err(e),
                        }
                    }
                }
                None => break,
            }
        }
        let end_loc = if let Some(bound_end) = bounds.last() {
            bound_end.span().end
        } else {
            return Err(format!("Error: Expected type bounds after colon at {}", type_var_token.span));
        };
        return Ok(GenericParam { name, bounds, span: Span::new(start_loc, end_loc) });
    } else {
        return Ok(GenericParam { name, bounds: Vec::new(), span: type_var_token.span });
    }
}

fn parse_struct_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    let name: String = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => if let Some(lexeme) = token.lexeme {
            lexeme.to_string()
        } else {
            return Err(format!("Error: Identifier token is missing lexeme at {}", token.span));
        }
        Err(e) => return Err(e),
    };
    let mut generics = Vec::new();
    if expect_kind(tokens, TokenKind::Lt).is_ok() {
        loop {
            let gen_param = parse_generic_param(tokens);
            match gen_param {
                Ok(param) => generics.push(param),
                Err(e) => return Err(e),
            }
            match tokens.peek() {
                Some(token) => {
                    if token.kind == TokenKind::Comma {
                        tokens.next();
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
    }
    let mut fields = Vec::new();
    if let Err(e) = expect_kind(tokens, TokenKind::LBrace) {
       return Err(e);
    } else {
        loop {
            match tokens.peek() {
                Some(token) => {
                    match token.kind {
                        TokenKind::RBrace => {
                            tokens.next();
                            break;
                        }
                        TokenKind::Ident => {
                            let field_ident = tokens.next().unwrap();
                            match expect_kind(tokens, TokenKind::Colon) {
                                Ok(_) => (),
                                Err(e) => return Err(e),
                            }
                            let field_type = match parse_type(tokens) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            fields.push((field_ident.lexeme.unwrap(), field_type));
                            match tokens.peek() {
                                Some(token) => {
                                    if token.kind == TokenKind::Comma {
                                        tokens.next();
                                    } else {
                                        break;
                                    }
                                }
                                None => break,
                            }
                        }
                        _ => return Err(format!("Error: Unexpected token {} at {}", token.kind, token.span)),
                    }
                }
                None => break,
            };
        }
    }
    // consume the closing brace
    let end_loc = match expect_kind(tokens, TokenKind::RBrace) {
        Ok(token) => token.span.end,
        Err(e) => return Err(e),
    };
    Ok(Statement::StructDecl { name, generics, fields, span: Span::new(start_loc, end_loc) })
}

fn parse_enum_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    let name: String = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => if let Some(lexeme) = token.lexeme {
            lexeme.to_string()
        } else {
            return Err(format!("Error: Identifier token is missing lexeme at {}", token.span));
        }
        Err(e) => return Err(e),
    };
    let mut generics = Vec::new();
    if expect_kind(tokens, TokenKind::Lt).is_ok() {
        loop {
            let gen_param = parse_generic_param(tokens);
            match gen_param {
                Ok(param) => generics.push(param),
                Err(e) => return Err(e),
            }
            match tokens.peek() {
                Some(token) => {
                    if token.kind == TokenKind::Comma {
                        tokens.next();
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
    }
    if !generics.is_empty() {
        if let Err(e) = expect_kind(tokens, TokenKind::Gt) {
            return Err(e);
        }
    }
    let mut variants = Vec::new();
    // consume the opening brace
    if let Err(e) = expect_kind(tokens, TokenKind::LBrace) {
        return Err(e);
    } 
    loop {
        if let Some(peeked) = tokens.peek() {
            if peeked.kind == TokenKind::RBrace {
                tokens.next();
                break;
            } else if peeked.kind == TokenKind::Ident {
                let variant_ident = tokens.next().unwrap();
                let mut variant_types = Vec::new();
                if expect_kind(tokens, TokenKind::LParen).is_ok() {
                    loop {
                        let variant_type = match parse_type(tokens) {
                            Ok(t) => t,
                            Err(e) => return Err(e),
                        };
                        variant_types.push(variant_type);
                        match tokens.peek() {
                            Some(token) => {
                                if token.kind == TokenKind::Comma {
                                    tokens.next();
                                } else {
                                    break;
                                }
                            }
                            None => break,
                        }
                    }
                    if let Err(e) = expect_kind(tokens, TokenKind::RParen) {
                        return Err(e);
                    }
                }
                variants.push((variant_ident.lexeme.unwrap(), variant_types));
            } else {
                return Err(format!("Error: Unexpected token {} at {}", peeked.kind, peeked.span));
            }
        } else {
            return Err("Error: Unexpected EOF".to_string());
        }
    }
    // consume the closing brace
    let end_loc = match expect_kind(tokens, TokenKind::RBrace) {
        Ok(token) => token.span.end,
        Err(e) => return Err(e),
    };
    Ok(Statement::EnumDecl { name, generics, variants, span: Span::new(start_loc, end_loc) })
}

fn parse_trait_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    // TODO: parse trait declarations
    tokens.next();
    unimplemented!("Trait declaration parsing is not implemented yet");
}

fn parse_type_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    // TODO: parse type declarations
    tokens.next();
    unimplemented!("Type declaration parsing is not implemented yet");
}

fn parse_impl_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    // TODO: parse impl blocks
    tokens.next();
    unimplemented!("Implementation declaration parsing is not implemented yet");
}

// TODO: write tests for the parser
