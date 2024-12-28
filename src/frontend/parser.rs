use crate::frontend::scanner::TokenKind;

use super::scanner::{Scanner, Token};
use super::ast::{ArrayType, Block, Expression, FunctionType, Program, Statement, TupleType, TypeExpr};

pub struct Parser<'a> {
    pub scanner: Scanner<'a>,
    pub current: Token<'a>,
    pub previous: Token<'a>,
    pub had_error: bool,
    pub panic_mode: bool,
    pub program: Program,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        Self {
            scanner,
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
            program: Program::new(),
        }
    }
}

// Error handling ======

fn error_at<T>(parser: &mut Parser, token: &Token, message: &str) -> Option<T> {
    // If we're already in panic mode, don't print anything.
    if parser.panic_mode {
        return None;
    }

    parser.panic_mode = true;
    eprint!("[line {}] Error", token.line);
    if token.kind == TokenKind::Eof {
        eprint!(" at end");
    } else if token.kind != TokenKind::Error {
        eprint!(" at '{}'", token.lexeme);
    }
    eprintln!(": {}", message);
    parser.had_error = true;
    None
}

fn error_at_current<T>(parser: &mut Parser, message: &str) -> Option<T> {
    let token = parser.current;
    error_at(parser, &token, message)
}

fn error_at_previous<T>(parser: &mut Parser, message: &str) -> Option<T> {
    let token = parser.previous;
    error_at(parser, &token, message)
}

// Token handling ======

fn advance<'a>(parser: &mut Parser<'a>) {
    parser.previous = parser.current;
    
    loop {
        parser.current = parser.scanner.scan_token();
        if parser.current.kind != TokenKind::Error {
            break;
        }
        let _: Option<()> = error_at_current(parser, parser.current.lexeme);
    }
}

fn consume<'a>(parser: &mut Parser<'a>, kind: TokenKind, message: &str) -> Option<()> {
    if parser.current.kind == kind {
        advance(parser);
        Some(())
    } else {
        let _: Option<()> = error_at_current(parser, message);
        None
    }
}

fn check_token<'a>(parser: &mut Parser<'a>, kind: TokenKind) -> bool {
    if parser.current.kind == kind {
        true
    } else {
        false
    }
}

fn match_token<'a>(parser: &mut Parser<'a>, kind: TokenKind) -> bool {
    if check_token(parser, kind) {
        advance(parser);
        true
    } else {
        false
    }
}

// Expression parsing =====

fn expression<'a>(parser: &mut Parser<'a>) -> Option<Expression> {
    // call parse_precedence
    parse_precedence(parser, Precedence::Assignment)
}

fn group_expression<'a>(parser: &mut Parser<'a>) -> Option<Expression> {
    // parse the expression inside the parentheses
    let expr = expression(parser)?;
    // consume the closing parenthesis
    consume(parser, TokenKind::RightParen, "Expected a closing parenthesis.")?;
    Some(expr)
}

fn call_expression<'a>(parser: &mut Parser<'a>, callee: Expression) -> Option<Expression> {
    let mut args = Vec::new();
    if !check_token(parser, TokenKind::RightParen) {
        loop {
            let arg = expression(parser)?;
            args.push(arg);
            if !match_token(parser, TokenKind::Comma) {
                break;
            }
        }
    }
    Some(Expression::new_call(callee, args, parser.previous.line))
}

fn number_expression<'a>(parser: &mut Parser<'a>) -> Option<Expression> {
    let line = parser.previous.line;
    // determine if the number is an integer or a float
    if let Ok(i) = parser.previous.lexeme.parse::<i64>() {
        Some(Expression::new_integer(i, line))
    } else if let Ok(f) = parser.previous.lexeme.parse::<f64>() {
        Some(Expression::new_float(f, line))
    } else {
        error_at_previous(parser, "Invalid number.")
    }
}

fn identifier_expression<'a>(parser: &mut Parser<'a>) -> Option<Expression> {
    let line = parser.previous.line;
    Some(Expression::new_identifier(parser.previous.lexeme.to_string(), line))
}

fn binary_expression<'a>(parser: &mut Parser<'a>, left: Expression) -> Option<Expression> {
    // get the kind of the previous token which is the operator
    let kind = parser.previous.kind;
    // get the infix parse function for the operator
    let rule = get_expr_parse_rule(kind);
    // parse the right hand side of the expression
    let right = parse_precedence(parser, rule.precedence.increment())?;
    // create a binary expression
    let line = parser.previous.line;
    Some(Expression::new_binary(left, kind, right, line))
}

fn unary_expression<'a>(parser: &mut Parser<'a>) -> Option<Expression> {
    // get the operator
    let kind = parser.previous.kind;
    // parse the operand
    let operand = parse_precedence(parser, Precedence::Unary)?;
    // create a unary expression
    let line = parser.previous.line;
    Some(Expression::new_unary(kind, operand, line))
}

fn parse_precedence<'a>(parser: &mut Parser<'a>, precedence: Precedence) -> Option<Expression> {
    // advance to the next token
    advance(parser);
    // get the prefix parse function for the previous token
    let prefix = get_expr_parse_rule(parser.previous.kind).prefix;
    let mut left = if let Some(prefix) = prefix {
        match prefix(parser) {
            Some(expr) => expr,
            None => return None,
        }
    } else {
        return error_at_previous(parser, "Expected a valid expression.");
    };

    while precedence <= get_expr_parse_rule(parser.current.kind).precedence {
        advance(parser);
        let infix = get_expr_parse_rule(parser.previous.kind).infix;
        if let Some(infix_fn) = infix {
            match infix_fn(parser, left) {
                Some(expr) => left = expr,
                None => return None,
            }
        } else {
            return error_at_previous(parser, "Expected a valid infix expression.");
        }
    }

    Some(left)
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
    pub fn increment(&self) -> Self {
        if *self != Precedence::Primary {
            // quickest way to increment enum
            let next = *self as u8 + 1;
            unsafe { std::mem::transmute(next) }
        } else {
            *self
        }
    }
}

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Option<Expression>;
type InfixParseFn<'a> = fn(&mut Parser<'a>, Expression) -> Option<Expression>;

struct ExpressionParseRule<'a> {
    prefix: Option<PrefixParseFn<'a>>,
    infix: Option<InfixParseFn<'a>>,
    precedence: Precedence,
}

fn get_expr_parse_rule<'a>(kind: TokenKind) -> ExpressionParseRule<'a> {
    match kind {
        TokenKind::Plus => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Term,
        },
        TokenKind::Minus => ExpressionParseRule {
            prefix: Some(unary_expression),
            infix: Some(binary_expression),
            precedence: Precedence::Term,
        },
        TokenKind::Star => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Factor,
        },
        TokenKind::Slash => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Factor,
        },
        TokenKind::Bang => ExpressionParseRule {
            prefix: Some(unary_expression),
            infix: None,
            precedence: Precedence::Unary,
        },
        TokenKind::EqEq => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Equality,
        },
        TokenKind::BangEq => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Equality,
        },
        TokenKind::LtEq => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Comparison,
        },
        TokenKind::Lt => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Comparison,
        },
        TokenKind::Gt => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Comparison,
        },
        TokenKind::GtEq => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Comparison,
        },
        TokenKind::And => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::And,
        },
        TokenKind::Or => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Or,
        },
        TokenKind::Eq => ExpressionParseRule {
            prefix: None,
            infix: Some(binary_expression),
            precedence: Precedence::Assignment,
        },
        TokenKind::Number => ExpressionParseRule {
            prefix: Some(number_expression),
            infix: None,
            precedence: Precedence::Primary,
        },
        TokenKind::Identifier => ExpressionParseRule {
            prefix: Some(identifier_expression),
            infix: None,
            precedence: Precedence::Primary,
        },
        TokenKind::LeftParen => ExpressionParseRule {
            prefix: Some(group_expression),
            infix: Some(call_expression),
            precedence: Precedence::Call,
        },
        _ => ExpressionParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
    }
}

// TypeExpr parsing =====

fn type_expr<'a>(parser: &mut Parser<'a>) -> Option<TypeExpr> {
    let line = parser.previous.line;
    match parser.previous.lexeme {
        "i8" | "i16" | "i32" | "i64" => Some(TypeExpr::new_int(parser.previous.lexeme, line)),
        "f32" | "f64" => Some(TypeExpr::new_float(parser.previous.lexeme, line)),
        "bool" => Some(TypeExpr::new_bool(line)),
        "string" => Some(TypeExpr::new_string(line)),
        "void" => Some(TypeExpr::new_void(line)),
        _ => match parser.previous.kind {
            TokenKind::Fn => {
                let mut function_type = FunctionType::new();
                advance(parser);
                consume(parser, TokenKind::LeftParen, "Expected a left parenthesis after function type.")?;
                if !check_token(parser, TokenKind::RightParen) {
                    loop {
                        match type_expr(parser) {
                            Some(param) => function_type.params.push(param),
                            None => return error_at_previous(parser, "Expected a parameter type after function type.")
                        }
                        if !match_token(parser, TokenKind::Comma) {
                            break;
                        }
                    }
                }
                consume(parser, TokenKind::RightParen, "Expected a right parenthesis after function type.")?;
                if match_token(parser, TokenKind::Arrow) {
                    match type_expr(parser) {
                        Some(return_type) => function_type.return_type = Box::new(return_type),
                        None => return error_at_previous(parser, "Expected a return type after function type.")
                    }
                } else {
                    function_type.return_type = Box::new(TypeExpr::new_void(line));
                }
                Some(TypeExpr::new_function(function_type, line))
            }
            TokenKind::LeftBracket => {
                // advance to the next token
                advance(parser);
                // parse the element type
                let element_type = match type_expr(parser) {
                    Some(element_type) => element_type,
                    None => return error_at_previous(parser, "Expected an element type after array type.")
                };
                // parse the size
                let size = match expression(parser) {
                    Some(size) => size,
                    None => return error_at_previous(parser, "Expected a size after array type.")
                };
                // consume the closing bracket
                consume(parser, TokenKind::RightBracket, "Expected a right bracket after array type.")?;
                Some(TypeExpr::new_array(ArrayType::new(element_type, size), line))
            }
            TokenKind::LeftParen => {
                let mut tuple_type = TupleType::new();
                advance(parser);
                if !check_token(parser, TokenKind::RightParen) {
                    loop {
                        match type_expr(parser) {
                            Some(element) => tuple_type.elements.push(element),
                            None => return error_at_previous(parser, "Expected a type after tuple type.")
                        }
                        if !match_token(parser, TokenKind::Comma) {
                            break;
                        }
                    }
                }
                consume(parser, TokenKind::RightParen, "Expected a right parenthesis after tuple type.")?;
                Some(TypeExpr::new_tuple(tuple_type, line))
            }
            _ => error_at_previous(parser, "Expected a valid type."),
        },
    }
}

// Statement parsing =====

fn statement<'a>(parser: &mut Parser<'a>) -> Option<Statement> {
    // match on current token kind
    match parser.current.kind {
        TokenKind::Let => var_declaration(parser, false),
        TokenKind::Const => var_declaration(parser, true),
        TokenKind::Fn => function_declaration(parser),
        TokenKind::LeftBrace => block_statement(parser),
        _ => expression_statement(parser),
    }
}

fn block<'a>(parser: &mut Parser<'a>) -> Option<Block> {
    // advance over the left brace
    advance(parser);
    // parse the statements in the block
    let mut statements = Vec::new();
    while !check_token(parser, TokenKind::RightBrace) && parser.current.kind != TokenKind::Eof {
        statements.push(statement(parser)?);
    }
    // consume the right brace
    consume(parser, TokenKind::RightBrace, "Expected a right brace after block.")?;
    Some(Block { statements })
}

fn block_statement<'a>(parser: &mut Parser<'a>) -> Option<Statement> {
    // get line of left brace prior to calling block fn
    let line = parser.current.line;
    // parse the block
    let block = block(parser)?;
    Some(Statement::new_block(block, line))
}

fn var_declaration<'a>(parser: &mut Parser<'a>, is_const: bool) -> Option<Statement> {
    // advance over the let or const keyword
    advance(parser);
    let line = parser.previous.line;
    // parse the name
    let name = parser.previous.lexeme.to_string();
    // check for a colon indicating a type annotation
    let ty = if match_token(parser, TokenKind::Colon) {
        // parse the type if there is none provided return out of the fn with None
        Some(type_expr(parser)?)
    } else {
        None
    };
    // expect an equals sign
    consume(parser, TokenKind::Eq, "Expected an equals sign after variable declaration.")?;
    // parse the value
    let value = expression(parser)?;
    // expect a semicolon
    consume(parser, TokenKind::Semicolon, "Expected a semicolon after variable declaration.")?;
    if is_const {
        Some(Statement::new_const_decl(name, ty, value, line))
    } else {
        Some(Statement::new_let_decl(name, ty, value, line))
    }
}

fn function_declaration<'a>(parser: &mut Parser<'a>) -> Option<Statement> {
    // advance over the fn keyword
    advance(parser);
    let line = parser.previous.line;
    // parse the name
    let name = parser.previous.lexeme.to_string();
    // parse the parameters
    let params = parse_function_params(parser)?;
    // parse the return type
    let return_type = type_expr(parser)?;
    // parse the body
    let body = block(parser)?;
    Some(Statement::new_function_decl(name, params, return_type, body, line))
}

fn parse_function_params<'a>(parser: &mut Parser<'a>) -> Option<Vec<(String, TypeExpr)>> {
    // expect a left parenthesis
    consume(parser, TokenKind::LeftParen, "Expected a left parenthesis after function declaration.")?;
    // parse the parameters
    let mut params = Vec::new();
    if !check_token(parser, TokenKind::RightParen) {
        loop {
            let name = parser.previous.lexeme.to_string();
            let ty = type_expr(parser)?;
            params.push((name, ty));
            if !match_token(parser, TokenKind::Comma) {
                break;
            }
        }
    }
    // expect a right parenthesis
    consume(parser, TokenKind::RightParen, "Expected a right parenthesis after function declaration.")?;
    Some(params)
}

fn expression_statement<'a>(parser: &mut Parser<'a>) -> Option<Statement> {
    let line = parser.previous.line;
    let expr = expression(parser)?;
    consume(parser, TokenKind::Semicolon, "Expected a semicolon after expression statement.")?;
    Some(Statement::new_expression_stmt(expr, line))
}
