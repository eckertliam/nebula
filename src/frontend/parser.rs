use std::iter::Peekable;

use std::vec::IntoIter;

use crate::frontend::{
    tokenizer::{Loc, Token, TokenKind},
    ast::*,
};

type TokenIter = Peekable<IntoIter<Token>>;

pub fn parse(tokens: Vec<Token>) -> Program {
    let mut tokens: TokenIter = tokens.into_iter().peekable();
    let mut program = Program::new();
    loop {
        if let Some(token) = tokens.peek() {
            if token.kind == TokenKind::Eof {
                break;
            } else if token.kind == TokenKind::Error {
                eprintln!("Error: {}", token.clone().lexeme.unwrap());
            } else {
                let start_loc = token.loc;
                let statement = match parse_statement(&mut tokens, start_loc) {
                    Ok(stmt) => stmt,
                    Err(e) => {
                        eprintln!("{}", e);
                        return Program::new();
                    }
                };
                program.push(statement);
            }
        }
    }
    program
}

fn parse_statement(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    let token = tokens.next().unwrap();
    match token.kind {
        TokenKind::Let => parse_var_decl(tokens, true, start_loc),
        TokenKind::Const => parse_var_decl(tokens, false, start_loc),
        TokenKind::Fn => parse_fn_decl(tokens, start_loc),
        _ => Err(format!("Error: Unexpected token {:?} at {:?}", token.kind, token.loc)),
    }
}

fn expect_kind(tokens: &mut TokenIter, kind: TokenKind) -> Result<Token, String> {
    if let Some(token) = tokens.next() {
        if token.kind != kind {
            return Err(format!("Error: Expected {:?} but got {:?} at {:?}", kind, token.kind, token.loc));
        } else {
            return Ok(token);
        }
    } else {
        return Err(format!("Error: Expected {:?} but got EOF", kind));
    }
}

fn parse_type(tokens: &mut TokenIter) -> Result<Type, String> {
    if let Some(token) = tokens.next() {
        let start_loc = token.loc;
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
                        match expect_kind(tokens, TokenKind::Gt) {
                            Ok(_) => (),
                            Err(e) => {
                                return Err(e);
                            }
                        }
                        return Ok(Type::generic(type_ident.lexeme.unwrap(), generic_args, start_loc));
                    } 
                }
                return Ok(Type::base(type_ident.lexeme.unwrap(), start_loc));
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
                match expect_kind(tokens, TokenKind::RParen) {
                    Ok(_) => (),
                    Err(e) => {
                        return Err(e);
                    }
                };
                if let Some(peeked) = tokens.peek() {
                    if peeked.kind == TokenKind::Arrow {
                        tokens.next();
                        let return_type = match parse_type(tokens) {
                            Ok(t) => t,
                            Err(e) => {
                                return Err(e);
                            }
                        };
                        return Ok(Type::function(types, Box::new(return_type), start_loc));
                    }
                }
                return Ok(Type::tuple(types, start_loc));
            }
            TokenKind::LBracket => {
                // parse array type
                let inner_type = match parse_type(tokens) {
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
                match expect_kind(tokens, TokenKind::RBracket) {
                    Ok(_) => (),
                    Err(e) => {
                        return Err(e);
                    }
                }
                return Ok(Type::array(Box::new(inner_type), size, start_loc));
            }
            _ => Err(format!("Error: Expected type but got {:?} at {:?}", token.kind, token.loc)),
        }
    } else {
        return Err("Error: Expected type but got EOF".to_string());
    }
}

fn parse_expression(tokens: &mut TokenIter) -> Result<Expression, String> {
    unimplemented!()
}

fn parse_var_decl(tokens: &mut TokenIter, is_let: bool, start_loc: Loc) -> Result<Statement, String> {
    let ident = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => {
            return Err(e);
        }
    };
    let _type = match parse_type(tokens) {
        Ok(t) => t,
        Err(e) => {
            return Err(e);
        }
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
    match expect_kind(tokens, TokenKind::Semicolon) {
        Ok(_) => (),
        Err(e) => {
            return Err(e);
        }
    }
    if is_let {
        return Ok(Statement::let_declaration(ident.lexeme.unwrap(), start_loc, _type, value));
    } else {
        return Ok(Statement::const_declaration(ident.lexeme.unwrap(), start_loc, _type, value));
    }
}

fn parse_fn_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    let ident = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => {
            return Err(e);
        }
    };
    // check for generics
    let mut generic_params = Vec::new();
    match expect_kind(tokens, TokenKind::Lt) {
        Ok(_) => {
            loop {
                let gen_param = parse_generic_param(tokens);
                match gen_param {
                    Ok(param) => generic_params.push(param),
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
        Err(_) => (),
    }
    // expect a closing angle bracket if there were generics
    if !generic_params.is_empty() {
        match expect_kind(tokens, TokenKind::Gt) {
            Ok(_) => (),
            Err(e) => {
                return Err(e);
            }
        }
    }
    // parse params
    let mut params = Vec::new();
    match expect_kind(tokens, TokenKind::LParen) {
        Ok(_) => {
            loop {
                let param_ident = match expect_kind(tokens, TokenKind::Ident) {
                    Ok(token) => token,
                    Err(e) => {
                        return Err(e);
                    }
                };
                let param_type = match parse_type(tokens) {
                    Ok(t) => t,
                    Err(e) => {
                        return Err(e);
                    }
                };
                params.push((param_ident.lexeme.unwrap(), param_type));
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
        Err(e) => {
            return Err(e);
        }
    }
    match expect_kind(tokens, TokenKind::RParen) {
        Ok(_) => (),
        Err(e) => {
            return Err(e);
        }
    }
    // expect an arrow
    match expect_kind(tokens, TokenKind::Arrow) {
        Ok(_) => (),
        Err(e) => {
            return Err(e);
        }
    }
    // parse the return type
    let return_type = match parse_type(tokens) {
        Ok(t) => t,
        Err(e) => {
            return Err(e);
        }
    };
    // parse the body
    let mut body = Vec::new();
    match expect_kind(tokens, TokenKind::LBrace) {
        Ok(_) => {
            loop {
                match tokens.peek() {
                    Some(token) => {
                        if token.kind == TokenKind::RBrace {
                            tokens.next();
                            break;
                        } else {
                            let inner_loc = token.loc;
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
        Err(e) => {
            return Err(e);
        }
    };
    return Ok(Statement::fn_declaration(ident.lexeme.unwrap(), start_loc, generic_params, params, return_type, body))
}

fn parse_generic_param(tokens: &mut TokenIter) -> Result<GenericParam, String> {
    let type_var = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => return Err(e),
    };
    let mut bounds = Vec::new();
    // check for a colon
    match tokens.peek() {
        Some(token) => {
            if token.kind == TokenKind::Colon {
                tokens.next();
                loop {
                    let bound = match parse_type(tokens) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    bounds.push(bound);
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
        }
        None => (),
    };
    Ok(GenericParam { type_var: type_var.lexeme.unwrap(), bounds })
}