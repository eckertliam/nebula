use std::iter::Peekable;

use std::vec::IntoIter;

use crate::frontend::{
    tokenizer::{Loc, Token, TokenKind},
    ast::*,
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
            let start_loc = token.loc;
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
    let token = tokens.peek().unwrap();
    match token.kind {
        TokenKind::Return => {
            tokens.next();
            let expr = match parse_expression(tokens) {
                Ok(e) => e,
                Err(e) => {
                    return Err(e);
                }
            };
            if let Err(e) = expect_kind(tokens, TokenKind::Semicolon) {
                return Err(e);
            }
            return Ok(Statement::return_statement(expr, start_loc));
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
            return Ok(Statement::expression(expr, start_loc));
        }
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

fn parse_type(tokens: &mut TokenIter) -> Result<AstType, String> {
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
                        if let Err(e) = expect_kind(tokens, TokenKind::Gt) {
                            return Err(e);
                        }
                        return Ok(AstType::generic(type_ident.lexeme.unwrap(), generic_args, start_loc));
                    } 
                }
                return Ok(AstType::base(type_ident.lexeme.unwrap(), start_loc));
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
                    return Ok(AstType::function(types, Box::new(return_type), start_loc));
                }
                return Ok(AstType::tuple(types, start_loc));
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
                if let Err(e) = expect_kind(tokens, TokenKind::RBracket) {
                    return Err(e);
                }
                return Ok(AstType::array(Box::new(inner_type), size, start_loc));
            }
            _ => Err(format!("Error: Expected type but got {:?} at {:?}", token.kind, token.loc)),
        }
    } else {
        return Err("Error: Expected type but got EOF".to_string());
    }
}

fn parse_expression(tokens: &mut TokenIter) -> Result<AstExpression, String> {
    if let Some(token) = tokens.peek() {
        let start_loc = token.loc;
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

fn parse_primary_expression(tokens: &mut TokenIter) -> Result<AstExpression, String> {
    if let Some(token) = tokens.next() {
        let start_loc = token.loc;
        match token.kind {
            TokenKind::True => Ok(AstExpression::bool_literal(true, start_loc)),
            TokenKind::False => Ok(AstExpression::bool_literal(false, start_loc)),
            TokenKind::Ident => Ok(AstExpression::identifier(token.lexeme.unwrap(), start_loc)),
            TokenKind::Number => {
                let lexeme = token.lexeme.unwrap();
                if let Ok(int) = lexeme.parse::<i64>() {
                    Ok(AstExpression::int_literal(int, start_loc))
                } else if let Ok(float) = lexeme.parse::<f64>() {
                    Ok(AstExpression::float_literal(float, start_loc))
                } else {
                    Err(format!("Error: Failed to parse number {:?} at {:?}", lexeme, start_loc))
                }
            }
            TokenKind::String => Ok(AstExpression::string_literal(token.lexeme.unwrap(), start_loc)),
            TokenKind::LParen => unimplemented!("Parsing of parenthesized expressions is not implemented yet"),
            TokenKind::LBracket => unimplemented!("Parsing of array literals is not implemented yet"),
            TokenKind::LBrace => unimplemented!("Parsing of block expressions is not implemented yet"),
            _ => Err(format!("Error: Expected primary expression but got {:?} at {:?}", token.kind, token.loc)),
        }
    } else {
        Err("Error: Expected primary expression but got EOF".to_string())
    }
}

fn parse_binary_expression(lhs: AstExpression, tokens: &mut TokenIter, prec: u8, loc: Loc) -> Result<AstExpression, String> {
    if let Some(binop_tok) = peek_binop(tokens) {
        // parse the right hand side of the binary expression
        let rhs = match parse_primary_expression(tokens) {
            Ok(expr) => expr,
            Err(e) => {
                return Err(e);
            }
        };
        // create the binary expression node
        let binary_expr = AstExpression::binary(binop_tok.kind, lhs, rhs, loc);
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
    // consume the colon preceding the type
    if let Err(e) = expect_kind(tokens, TokenKind::Colon) {
        return Err(e);
    }
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
    if let Err(e) = expect_kind(tokens, TokenKind::Semicolon) {
        return Err(e);
    }
    if is_let {
        return Ok(Statement::let_declaration(ident.lexeme.unwrap(), start_loc, _type, value));
    } else {
        return Ok(Statement::const_declaration(ident.lexeme.unwrap(), start_loc, _type, value));
    }
}

fn parse_fn_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    let ident = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => {
            return Err(e);
        }
    };
    // check for generics
    let mut generic_params = Vec::new();
    if expect_kind(tokens, TokenKind::Lt).is_ok() {
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
    // expect a closing angle bracket if there were generics
    if !generic_params.is_empty() {
        if let Err(e) = expect_kind(tokens, TokenKind::Gt) {
            return Err(e);
        }
    }
    // parse params
    let mut params = Vec::new();
    if let Err(e) = expect_kind(tokens, TokenKind::LParen) {
        return Err(e);
    } else {
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
    if let Err(e) = expect_kind(tokens, TokenKind::RParen) {
        return Err(e);
    }
    // expect an arrow
    if let Err(e) = expect_kind(tokens, TokenKind::Arrow) {
        return Err(e);
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
    // consume the closing brace
    if let Err(e) = expect_kind(tokens, TokenKind::RBrace) {
        return Err(e);
    }
    return Ok(Statement::fn_declaration(ident.lexeme.unwrap(), start_loc, generic_params, params, return_type, body))
}

fn parse_generic_param(tokens: &mut TokenIter) -> Result<GenericParam, String> {
    let type_var = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => return Err(e),
    };
    let mut bounds = Vec::new();
    // check for a colon
    if let Some(token) = tokens.peek() {
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
    };
    Ok(GenericParam { type_var: type_var.lexeme.unwrap(), bounds })
}

fn parse_struct_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    let ident = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => return Err(e),
    };
    let mut generic_params = Vec::new();
    if expect_kind(tokens, TokenKind::Lt).is_ok() {
        loop {
            let gen_param = parse_generic_param(tokens);
            match gen_param {
                Ok(param) => generic_params.push(param),
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
                        _ => return Err(format!("Error: Unexpected token {:?} at {:?}", token.kind, token.loc)),
                    }
                }
                None => break,
            };
        }
    }
    // consume the closing brace
    if let Err(e) = expect_kind(tokens, TokenKind::RBrace) {
        return Err(e);
    }
    Ok(Statement::struct_declaration(ident.lexeme.unwrap(), start_loc, generic_params, fields))
}

fn parse_enum_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    let ident = match expect_kind(tokens, TokenKind::Ident) {
        Ok(token) => token,
        Err(e) => return Err(e),
    };
    let mut generic_params = Vec::new();
    if expect_kind(tokens, TokenKind::Lt).is_ok() {
        loop {
            let gen_param = parse_generic_param(tokens);
            match gen_param {
                Ok(param) => generic_params.push(param),
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
    if !generic_params.is_empty() {
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
                return Err(format!("Error: Unexpected token {:?} at {:?}", peeked.kind, peeked.loc));
            }
        } else {
            return Err("Error: Unexpected EOF".to_string());
        }
    }
    // consume the closing brace
    if let Err(e) = expect_kind(tokens, TokenKind::RBrace) {
        return Err(e);
    }
    Ok(Statement::enum_declaration(ident.lexeme.unwrap(), start_loc, generic_params, variants))
}

fn parse_trait_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    unimplemented!("Trait declaration parsing is not implemented yet");
}

fn parse_type_decl(tokens: &mut TokenIter, start_loc: Loc) -> Result<Statement, String> {
    tokens.next();
    unimplemented!("Type declaration parsing is not implemented yet");
}

#[cfg(test)]
mod tests {
    use crate::frontend::tokenizer::tokenize;

    use super::*;

    #[test]
    fn parse_simple_const_decl() {
        let src = "const x: i32 = 42;";
        let tokens = tokenize(src);
        let program = parse(tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = program.statements[0].clone();
        match stmt.kind {
            StatementNode::ConstDeclaration(const_decl) => {
                assert_eq!(const_decl.name, "x");
                assert_eq!(const_decl.value.kind, ExpressionNode::Int(42));
                assert_eq!(const_decl._type.kind, TypeNode::Base("i32".to_string()));
            }
            _ => panic!("Expected const declaration but got {:?}", stmt.kind),
        }
    }

    #[test]
    fn parse_binary_expr() {
        let src = "const x: i32 = 42 + 3 * 2;";
        let tokens = tokenize(src);
        let program = parse(tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = program.statements[0].clone();
        if let StatementNode::ConstDeclaration(const_decl) = stmt.kind {
            if let ExpressionNode::Mul(lhs, rhs) = const_decl.value.kind.clone() {
                if let ExpressionNode::Add(lhs, rhs) = lhs.kind {
                    assert_eq!(lhs.kind, ExpressionNode::Int(42));
                    assert_eq!(rhs.kind, ExpressionNode::Int(3))
                } else {
                    panic!("Expected add expression but got {:?}", const_decl.value.kind);
                }
                assert_eq!(rhs.kind, ExpressionNode::Int(2));
            } else {
                panic!("Expected mul expression but got {:?}", const_decl.value.kind);
            }
        } else {
            panic!("Expected const declaration but got {:?}", stmt.kind);
        }
    }

    fn parse_func_decl() {
        let src = "fn add(x: i32, y: i32) -> i32 { return x + y; }";
        let tokens = tokenize(src);
        let program = parse(tokens).unwrap();
        assert_eq!(program.statements.len(), 1);

    }
}