use crate::frontend::scanner::TokenKind;

use super::scanner::{Scanner, Token};
use super::ast::{ArrayType, Block, Expression, FunctionType, Located, Program, Statement, TupleType, TypeExpr};

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
        let mut parser = Self {
            scanner,
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
            program: Program::new(),
        };
        // advance to the first token
        advance(&mut parser);
        parser
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

fn expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
    // call parse_precedence
    parse_precedence(parser, Precedence::Assignment)
}

fn group_expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
    // parse the expression inside the parentheses
    let expr = expression(parser)?;
    // consume the closing parenthesis
    consume(parser, TokenKind::RightParen, "Expected a closing parenthesis.")?;
    Some(expr)
}

fn call_expression<'a>(parser: &mut Parser<'a>, callee: Located<Expression>) -> Option<Located<Expression>> {
    let mut args = Vec::new();
    if !check_token(parser, TokenKind::RightParen) {
        loop {
            let arg = expression(parser)?.node;
            args.push(arg);
            if !match_token(parser, TokenKind::Comma) {
                break;
            }
        }
    }
    Some(Expression::new_call(callee.node, args, callee.line))
}

fn number_expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
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

fn bool_expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
    let line = parser.previous.line;
    let value = match parser.previous.kind {
        TokenKind::True => true,
        TokenKind::False => false,
        _ => return error_at_previous(parser, "Invalid boolean."),
    };
    Some(Expression::new_bool(value, line))
}

fn identifier_expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
    let line = parser.previous.line;
    Some(Expression::new_identifier(parser.previous.lexeme.to_string(), line))
}

fn binary_expression<'a>(parser: &mut Parser<'a>, left: Located<Expression>) -> Option<Located<Expression>> {
    // get the kind of the previous token which is the operator
    let kind = parser.previous.kind;
    // get the infix parse function for the operator
    let rule = get_expr_parse_rule(kind);
    // parse the right hand side of the expression
    let right = parse_precedence(parser, rule.precedence.increment())?;
    Some(Expression::new_binary(left.node, kind, right.node, left.line))
}

fn unary_expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
    // get the operator
    let kind = parser.previous.kind;
    // parse the operand
    let operand = parse_precedence(parser, Precedence::Unary)?;
    // create a unary expression
    let line = parser.previous.line;
    Some(Expression::new_unary(kind, operand.node, line))
}

fn parse_precedence<'a>(parser: &mut Parser<'a>, precedence: Precedence) -> Option<Located<Expression>> {
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

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Option<Located<Expression>>;
type InfixParseFn<'a> = fn(&mut Parser<'a>, Located<Expression>) -> Option<Located<Expression>>;

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
        TokenKind::True => ExpressionParseRule {
            prefix: Some(bool_expression),
            infix: None,
            precedence: Precedence::Primary,
        },
        TokenKind::False => ExpressionParseRule {
            prefix: Some(bool_expression),
            infix: None,
            precedence: Precedence::Primary,
        },
        _ => ExpressionParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
    }
}

// TypeExpr parsing =====

fn type_expr<'a>(parser: &mut Parser<'a>) -> Option<Located<TypeExpr>> {
    let line = parser.previous.line;
    match parser.previous.lexeme {
        "i8" | "i16" | "i32" | "i64" => Some(TypeExpr::new_int(parser.previous.lexeme, line)),
        "f32" | "f64" => Some(TypeExpr::new_float(parser.previous.lexeme, line)),
        "bool" => Some(TypeExpr::new_bool(line)),
        "string" => Some(TypeExpr::new_string(line)),
        "void" => Some(TypeExpr::new_void(line)),
        _ => match parser.previous.kind {
            TokenKind::Fn => {
                let mut func_params = Vec::new();
                advance(parser);
                consume(parser, TokenKind::LeftParen, "Expected a left parenthesis after function type.")?;
                if !check_token(parser, TokenKind::RightParen) {
                    loop {
                        match type_expr(parser) {
                            Some(param) => func_params.push(param.node),
                            None => return error_at_previous(parser, "Expected a parameter type after function type.")
                        }
                        if !match_token(parser, TokenKind::Comma) {
                            break;
                        }
                    }
                }
                consume(parser, TokenKind::RightParen, "Expected a right parenthesis after function type.")?;
                let return_type = {
                    let rt = match match_token(parser, TokenKind::Arrow) {
                        true => type_expr(parser)?,
                        false => TypeExpr::new_void(line),
                    };
                    rt.node
                };
                Some(TypeExpr::new_function(func_params, return_type, line))
            }
            TokenKind::LeftBracket => {
                // advance to the next token
                advance(parser);
                // parse the element type
                let element_type = match type_expr(parser) {
                    Some(element_type) => element_type.node,
                    None => return error_at_previous(parser, "Expected an element type after array type.")
                };
                // parse the size
                let size = match expression(parser) {
                    Some(size) => size.node,
                    None => return error_at_previous(parser, "Expected a size after array type.")
                };
                // consume the closing bracket
                consume(parser, TokenKind::RightBracket, "Expected a right bracket after array type.")?;
                Some(TypeExpr::new_array(element_type, size, line))
            }
            TokenKind::LeftParen => {
                advance(parser);
                let mut tuple_type = Vec::new();
                if !check_token(parser, TokenKind::RightParen) {
                    loop {
                        match type_expr(parser) {
                            Some(element) => tuple_type.push(element.node),
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

fn statement<'a>(parser: &mut Parser<'a>) -> Option<Located<Statement>> {
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

fn block_statement<'a>(parser: &mut Parser<'a>) -> Option<Located<Statement>> {
    // get line of left brace prior to calling block fn
    let line = parser.current.line;
    // parse the block
    let block = block(parser)?;
    Some(Statement::new_block(block, line))
}

fn var_declaration<'a>(parser: &mut Parser<'a>, is_const: bool) -> Option<Located<Statement>> {
    // advance over the let or const keyword
    advance(parser);
    let line = parser.previous.line;
    // parse the name
    let name = parser.previous.lexeme.to_string();
    // check for a colon indicating a type annotation
    let ty = if match_token(parser, TokenKind::Colon) {
        // parse the type if there is none provided return out of the fn with None
        Some(type_expr(parser)?.node)
    } else {
        None
    };
    // expect an equals sign
    consume(parser, TokenKind::Eq, "Expected an equals sign after variable declaration.")?;
    // parse the value
    let value = expression(parser)?.node;
    // expect a semicolon
    consume(parser, TokenKind::Semicolon, "Expected a semicolon after variable declaration.")?;
    if is_const {
        Some(Statement::new_const_decl(name, ty, value, line))
    } else {
        Some(Statement::new_let_decl(name, ty, value, line))
    }
}

fn function_declaration<'a>(parser: &mut Parser<'a>) -> Option<Located<Statement>> {
    // advance over the fn keyword
    advance(parser);
    let line = parser.previous.line;
    // parse the name
    let name = parser.previous.lexeme.to_string();
    // parse the parameters
    let params = parse_function_params(parser)?;
    // parse the return type
    let return_type = type_expr(parser)?.node;
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
            let ty = type_expr(parser)?.node;
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

fn expression_statement<'a>(parser: &mut Parser<'a>) -> Option<Located<Statement>> {
    let line = parser.previous.line;
    let expr = expression(parser)?.node;
    consume(parser, TokenKind::Semicolon, "Expected a semicolon after expression statement.")?;
    Some(Statement::new_expression_stmt(expr, line))
}

#[cfg(test)]
mod tests {
    use crate::frontend::ast::BinaryExpr;

    use super::*;

    #[test]
    fn test_group_expression() {
        let scanner = Scanner::new("(1 + 2) * 3");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_bin_expr = match expr.unwrap().node {
            Expression::Binary(bin_expr) => bin_expr,
            _ => panic!("Expected a binary expression."),
        };
        assert_eq!(top_bin_expr.op, TokenKind::Star);
        let lhs = match *top_bin_expr.lhs {
            Expression::Binary(bin_expr) => bin_expr,
            _ => panic!("Expected a binary expression."),
        };
        assert_eq!(lhs.op, TokenKind::Plus);
        assert_eq!(*lhs.lhs, Expression::Integer(1));
        assert_eq!(*lhs.rhs, Expression::Integer(2));
        assert_eq!(*top_bin_expr.rhs, Expression::Integer(3));
    }

    #[test]
    fn test_call_expression() {
        let scanner = Scanner::new("func(1, 2, 3)");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_call_expr = match expr.unwrap().node {
            Expression::Call(call_expr) => call_expr,
            _ => panic!("Expected a call expression."),
        };
        assert_eq!(*top_call_expr.callee, Expression::Identifier("func".to_string()));
        assert_eq!(top_call_expr.args.len(), 3);
        assert_eq!(top_call_expr.args[0], Expression::Integer(1));
        assert_eq!(top_call_expr.args[1], Expression::Integer(2));
        assert_eq!(top_call_expr.args[2], Expression::Integer(3));
    }

    #[test]
    fn test_number_expression() {
        let scanner = Scanner::new("123");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        assert_eq!(expr.unwrap().node, Expression::Integer(123));
        let scanner = Scanner::new("123.456");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        assert_eq!(expr.unwrap().node, Expression::Float(123.456));
    }

    #[test]
    fn test_bool_expression() {
        let scanner = Scanner::new("true");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        assert_eq!(expr.unwrap().node, Expression::Bool(true));
        let scanner = Scanner::new("false");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        assert_eq!(expr.unwrap().node, Expression::Bool(false));
    }

    #[test]
    fn test_identifier_expression() {
        let scanner = Scanner::new("x");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        assert_eq!(expr.unwrap().node, Expression::Identifier("x".to_string()));
        let scanner = Scanner::new("_x");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        assert_eq!(expr.unwrap().node, Expression::Identifier("_x".to_string()));
    }

    #[test]
    fn test_binary_expr() {
        let scanner = Scanner::new("1 + 2 * 3");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_bin_expr = match expr.unwrap().node {
            Expression::Binary(bin_expr) => bin_expr,
            _ => panic!("Expected a binary expression."),
        };
        assert_eq!(top_bin_expr.op, TokenKind::Plus);
        assert!(matches!(*top_bin_expr.lhs, Expression::Integer(1)));
        let _expected_rhs = Expression::Binary(BinaryExpr {
            op: TokenKind::Star,
            lhs: Box::new(Expression::Integer(2)),
            rhs: Box::new(Expression::Integer(3)),
        });
        assert!(matches!(*top_bin_expr.rhs, _expected_rhs));
    }

    // TODO: test unary_expression
    #[test]
    fn test_unary_expression() {
        let scanner = Scanner::new("-1");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_unary_expr = match expr.unwrap().node {
            Expression::Unary(unary_expr) => unary_expr,
            _ => panic!("Expected a unary expression."),
        };
        assert_eq!(top_unary_expr.op, TokenKind::Minus);
        assert_eq!(*top_unary_expr.expr, Expression::Integer(1));
        let scanner = Scanner::new("!true");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_unary_expr = match expr.unwrap().node {
            Expression::Unary(unary_expr) => unary_expr,
            _ => panic!("Expected a unary expression."),
        };
        assert_eq!(top_unary_expr.op, TokenKind::Bang);
        assert_eq!(*top_unary_expr.expr, Expression::Bool(true));
    }

    // TODO: test type_expr
    // TODO: test block_statement
    // TODO: test var_declaration
    // TODO: test function_declaration
    // TODO: test expression_statement
}
