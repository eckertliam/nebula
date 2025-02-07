use crate::frontend::scanner::TokenKind;
use crate::passes::const_fold;

use super::scanner::{Scanner, Token};
use super::ast::{Block, Expression, Located, Program, Statement};
use super::types::{Type, TypeVarGen};

pub struct Parser<'a> {
    pub scanner: Scanner<'a>,
    pub current: Token<'a>,
    pub previous: Token<'a>,
    pub had_error: bool,
    pub panic_mode: bool,
    pub type_var_gen: TypeVarGen,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        let mut parser = Self {
            scanner,
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
            type_var_gen: TypeVarGen::new(),
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
    // consume the closing parenthesis
    consume(parser, TokenKind::RightParen, "Expected a closing parenthesis after function call.")?;
    Some(Expression::new_call(callee.node, args, callee.line))
}

fn number_expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
    let line = parser.previous.line;
    // determine if the number is an integer, unsigned integer, or float
    if let Ok(i) = parser.previous.lexeme.parse::<i64>() {
        Some(Expression::new_integer(i, line))
    } else if let Ok(u) = parser.previous.lexeme.parse::<u64>() {
        Some(Expression::new_unsigned_integer(u, line))
    } else if let Ok(f) = parser.previous.lexeme.parse::<f64>() {
        Some(Expression::new_float(f, line))
    } else {
        error_at_previous(parser, "Invalid number.")
    }
}

fn string_expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
    let line = parser.previous.line;
    let value = parser.previous.lexeme.to_string();
    // slice the first and last character
    let value = &value[1..value.len() - 1];
    Some(Expression::new_string(value.to_string(), line))
}

fn char_expression<'a>(parser: &mut Parser<'a>) -> Option<Located<Expression>> {
    let line = parser.previous.line;
    // check if the character is valid
    // TODO: handle escape characters
    if parser.previous.lexeme.len() != 3 {
        return error_at_previous(parser, "Invalid character.");
    }
    let value = parser.previous.lexeme.chars().nth(1).unwrap();
    Some(Expression::new_char(value, line))
}

// TODO: handle array expressions

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
        TokenKind::Char => ExpressionParseRule {
            prefix: Some(char_expression),
            infix: None,
            precedence: Precedence::Primary,
        },
        TokenKind::String => ExpressionParseRule {
            prefix: Some(string_expression),
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

fn type_expr<'a>(parser: &mut Parser<'a>) -> Option<Type> {
    // move to the next token
    advance(parser);
    match parser.previous.lexeme {
        "i8" => Some(Type::I8),
        "i16" => Some(Type::I16),
        "i32" => Some(Type::I32),
        "i64" => Some(Type::I64),
        "u8" => Some(Type::U8),
        "u16" => Some(Type::U16),
        "u32" => Some(Type::U32),
        "u64" => Some(Type::U64),
        "f32" => Some(Type::F32),
        "f64" => Some(Type::F64),
        "bool" => Some(Type::Bool),
        "char" => Some(Type::Char),
        "string" => Some(Type::String),
        "void" => Some(Type::Void),
        _ => match parser.previous.kind {
            TokenKind::Fn => {
                let mut func_params = Vec::new();
                consume(parser, TokenKind::LeftParen, "Expected a left parenthesis after function type.")?;
                if !check_token(parser, TokenKind::RightParen) {
                    loop {
                        match type_expr(parser) {
                            Some(param) => func_params.push(param),
                            None => return error_at_previous(parser, "Expected a parameter type after function type.")
                        }
                        if !match_token(parser, TokenKind::Comma) {
                            break;
                        }
                    }
                }
                consume(parser, TokenKind::RightParen, "Expected a right parenthesis after function type.")?;
                // expect an arrow
                consume(parser, TokenKind::Arrow, "Expected an arrow after function type.")?;
                // parse the return type
                let return_type = type_expr(parser)?;
                Some(Type::Function {
                    params: func_params,
                    return_type: Box::new(return_type),
                })
            }
            TokenKind::LeftBracket => {
                // an array type is of the form [type; size]
                // parse the element type
                let element_type = match type_expr(parser) {
                    Some(element_type) => element_type,
                    None => return error_at_previous(parser, "Expected an element type after array type.")
                };
                // expect a semicolon
                consume(parser, TokenKind::Semicolon, "Expected a semicolon after array type.")?;
                // parse the size
                let size = match expression(parser) {
                    Some(size) => match const_fold(size.node) {
                        Some(Expression::Integer(v)) => if v < 0 {
                            return error_at_previous(parser, "Expected a positive size after array type.");
                        } else {
                            v as usize
                        },
                        Some(Expression::UnsignedInteger(v)) => v as usize,
                        _ => return error_at_previous(parser, "Expected a valid size after array type.")
                    },
                    None => return error_at_previous(parser, "Expected a size after array type.")
                };
                // consume the closing bracket
                consume(parser, TokenKind::RightBracket, "Expected a right bracket after array type.")?;
                Some(Type::Array {
                    element_type: Box::new(element_type),
                    size,
                })
            }
            TokenKind::LeftParen => {
                let mut tuple_type = Vec::new();
                if !check_token(parser, TokenKind::RightParen) {
                    loop {
                        match type_expr(parser) {
                            Some(element) => tuple_type.push(element),
                            None => return error_at_previous(parser, "Expected a type after tuple type.")
                        }
                        if !match_token(parser, TokenKind::Comma) {
                            break;
                        }
                    }
                }
                consume(parser, TokenKind::RightParen, "Expected a right parenthesis after tuple type.")?;
                Some(Type::Tuple(tuple_type))
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
        TokenKind::Return => return_statement(parser),
        TokenKind::Record => record_declaration(parser),
        TokenKind::Type => alias_declaration(parser),
        _ => expression_statement(parser),
    }
}

fn block<'a>(parser: &mut Parser<'a>) -> Option<Block> {
    // advance over the left brace
    advance(parser);
    // parse the statements in the block
    let mut block = Vec::new();
    while !check_token(parser, TokenKind::RightBrace) && parser.current.kind != TokenKind::Eof {
        block.push(statement(parser)?);
    }
    // consume the right brace
    consume(parser, TokenKind::RightBrace, "Expected a right brace after block.")?;
    Some(block)
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
    // advance over the identifier
    advance(parser);
    // parse the name
    let name = parser.previous.lexeme.to_string();
    // check for a colon indicating a type annotation
    let ty = if match_token(parser, TokenKind::Colon) {
        // parse the type if there is none provided return out of the fn with None
        type_expr(parser)?
    } else {
        // generate a type variable for substitution during type inference
        Type::TypeVar(parser.type_var_gen.next())
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
    // advance over the function name
    advance(parser);
    // parse the name
    let name = parser.previous.lexeme.to_string();
    // parse the parameters
    let params = parse_function_params(parser)?;
    // if there is an arrow, parse the return type
    let return_type = if match_token(parser, TokenKind::Arrow) {
        type_expr(parser)?
    } else {
        Type::Void
    };
    // parse the body
    let body = block(parser)?;
    Some(Statement::new_function_decl(name, params, return_type, body, line))
}

fn parse_function_params<'a>(parser: &mut Parser<'a>) -> Option<Vec<(String, Type)>> {
    // expect a left parenthesis
    consume(parser, TokenKind::LeftParen, "Expected a left parenthesis after function declaration.")?;
    // parse the parameters
    let mut params = Vec::new();
    if !check_token(parser, TokenKind::RightParen) {
        loop {
            // advance over the identifier
            advance(parser);
            let name = parser.previous.lexeme.to_string();
            // skip the colon
            consume(parser, TokenKind::Colon, "Expected a colon after function parameter.")?;
            // parse the type
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

fn expression_statement<'a>(parser: &mut Parser<'a>) -> Option<Located<Statement>> {
    let line = parser.previous.line;
    let expr = expression(parser)?.node;
    consume(parser, TokenKind::Semicolon, "Expected a semicolon after expression statement.")?;
    Some(Statement::new_expression_stmt(expr, line))
}

fn return_statement<'a>(parser: &mut Parser<'a>) -> Option<Located<Statement>> {
    // advance over the return keyword
    advance(parser);
    let line = parser.previous.line;
    // if there is an expression, parse it
    if match_token(parser, TokenKind::Semicolon) {
        Some(Statement::new_return_stmt(None, line))
    } else {
        let expr = expression(parser)?.node;
        consume(parser, TokenKind::Semicolon, "Expected a semicolon after return statement.")?;
        Some(Statement::new_return_stmt(Some(expr), line))
    }
}

fn parse_record_fields<'a>(parser: &mut Parser<'a>) -> Option<Vec<(String, Type)>> {
    // expect a left brace
    consume(parser, TokenKind::LeftBrace, "Expected a left brace after record declaration.")?;
    // parse the fields
    let mut fields = Vec::new();
    while !check_token(parser, TokenKind::RightBrace) {
        // advance over the identifier
        advance(parser);
        let name = parser.previous.lexeme.to_string();
        // expect a colon
        consume(parser, TokenKind::Colon, "Expected a colon after record field name.")?;
        // parse the type
        let ty = type_expr(parser)?;
        fields.push((name, ty));
        // check for a comma
        if !match_token(parser, TokenKind::Comma) {
            break;
        }
    }
    // consume the right brace
    consume(parser, TokenKind::RightBrace, "Expected a right brace after record declaration.")?;
    Some(fields)
}

fn record_declaration<'a>(parser: &mut Parser<'a>) -> Option<Located<Statement>> {
    // advance over the record keyword
    advance(parser);
    // advance over the identifier
    advance(parser);
    let line = parser.previous.line;
    let name = parser.previous.lexeme.to_string();
    let fields = parse_record_fields(parser)?;
    // TODO: add generic handling
    let generics = vec![];
    Some(Statement::new_record_decl(name, generics, fields, line))
}

fn alias_declaration<'a>(parser: &mut Parser<'a>) -> Option<Located<Statement>> {
    // advance over the alias keyword
    advance(parser);
    // advance over the identifier
    advance(parser);
    let line = parser.previous.line;
    let name = parser.previous.lexeme.to_string();
    // TODO: handle generics
    let generics = vec![];
    // expect an equals sign
    consume(parser, TokenKind::Eq, "Expected an equals sign after alias declaration.")?;
    // parse the type
    let ty = type_expr(parser)?;
    Some(Statement::new_alias_decl(name, generics, ty, line))
}

// top level parsing =====

pub fn parse<'a>(src: &str) -> Option<Program> {
    let scanner = Scanner::new(src);
    let mut parser = Parser::new(scanner);
    let mut program = Program::new();
    while parser.current.kind != TokenKind::Eof {
        program.add_statement(statement(&mut parser)?);
    }
    Some(program)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_group_expression() {
        let scanner = Scanner::new("(1 + 2) * 3");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_bin_expr = expr.unwrap().node;
        assert_eq!(top_bin_expr, Expression::Binary {
            lhs: Box::new(Expression::Binary {
                lhs: Box::new(Expression::Integer(1)),
                op: TokenKind::Plus,
                rhs: Box::new(Expression::Integer(2)),
            }),
            op: TokenKind::Star,
            rhs: Box::new(Expression::Integer(3)),
        });
    }

    #[test]
    fn test_call_expression() {
        let scanner = Scanner::new("func(1, 2, 3)");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_call_expr = expr.unwrap().node;
        assert_eq!(top_call_expr, Expression::Call {
            callee: Box::new(Expression::Identifier("func".to_string())),
            args: vec![Expression::Integer(1), Expression::Integer(2), Expression::Integer(3)],
        });
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
    fn test_char_expression() {
        let scanner = Scanner::new("'a'");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        assert_eq!(expr.unwrap().node, Expression::Char('a'));
    }

    #[test]
    fn test_string_expression() {
        let scanner = Scanner::new("\"hello\"");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        assert_eq!(expr.unwrap().node, Expression::String("hello".to_string()));
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
        let top_bin_expr = expr.unwrap().node;
        assert_eq!(top_bin_expr, Expression::Binary {
            lhs: Box::new(Expression::Integer(1)),
            op: TokenKind::Plus,
            rhs: Box::new(Expression::Binary {
                lhs: Box::new(Expression::Integer(2)),
                op: TokenKind::Star,
                rhs: Box::new(Expression::Integer(3)),
            }),
        });
    }

    #[test]
    fn test_unary_expression() {
        let scanner = Scanner::new("-1");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_unary_expr = expr.unwrap().node;
        assert_eq!(top_unary_expr, Expression::Unary {
            op: TokenKind::Minus,
            expr: Box::new(Expression::Integer(1)),
        });
        let scanner = Scanner::new("!true");
        let mut parser = Parser::new(scanner);
        let expr = expression(&mut parser);
        assert!(expr.is_some());
        let top_unary_expr = expr.unwrap().node;
        assert_eq!(top_unary_expr, Expression::Unary {
            op: TokenKind::Bang,
            expr: Box::new(Expression::Bool(true)),
        });
    }

    #[test]
    fn test_primitive_type_expr() {
        let scanner = Scanner::new("i8");
        let mut parser = Parser::new(scanner);
        let expr = type_expr(&mut parser);
        assert!(expr.is_some());
    }
    
    #[test]
    fn test_array_type_expr() {
        let scanner = Scanner::new("[f32; 10]");
        let mut parser = Parser::new(scanner);
        let expr = type_expr(&mut parser);
        assert!(expr.is_some());
        let array_type = expr.unwrap();
        assert_eq!(array_type, Type::Array {
            element_type: Box::new(Type::F32),
            size: 10,
        });
        // test a matrix type expression
        let scanner = Scanner::new("[[f32; 2]; 2]");
        let mut parser = Parser::new(scanner);
        let expr = type_expr(&mut parser);
        assert!(expr.is_some());
        let array_type = expr.unwrap();
        assert_eq!(array_type, Type::Array {
            element_type: Box::new(Type::Array {
                element_type: Box::new(Type::F32),
                size: 2,
            }),
            size: 2,
        });
    }

    #[test]
    fn test_tuple_type_expr() {
        let scanner = Scanner::new("(i8, f32, bool)");
        let mut parser = Parser::new(scanner);
        let expr = type_expr(&mut parser);
        assert!(expr.is_some());
        let tuple_type = expr.unwrap();
        assert_eq!(tuple_type, Type::Tuple(vec![Type::I8, Type::F32, Type::Bool]));
    }

    #[test]
    fn test_function_type_expr() {
        let scanner = Scanner::new("fn(i8, f32) -> bool");
        let mut parser = Parser::new(scanner);
        let expr = type_expr(&mut parser);
        assert!(expr.is_some());
        let function_type = expr.unwrap();
        assert_eq!(function_type, Type::Function {
            params: vec![Type::I8, Type::F32],
            return_type: Box::new(Type::Bool),
        });
    }

    #[test]
    fn test_block_statement() {
        let scanner = Scanner::new("{ let x = 1; x + 2; }");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let block_stmt = stmt.unwrap().node;
        assert_eq!(block_stmt, Statement::Block(vec![
            Located::new(Statement::LetDecl {
                name: "x".to_string(),
                ty: Type::TypeVar("a0".to_string()),
                value: Expression::Integer(1),
            }, 1),
            Located::new(Statement::ExpressionStmt(Expression::Binary {
                lhs: Box::new(Expression::Identifier("x".to_string())),
                op: TokenKind::Plus,
                rhs: Box::new(Expression::Integer(2)),
            }), 1),
        ]));
    }
    
    #[test]
    fn test_var_declaration() {
        let scanner = Scanner::new("let y: f32 = 3.14;");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let var_decl = match stmt.unwrap().node {
            Statement::LetDecl { name, ty, value } => (name, ty, value),
            _ => panic!("Expected a variable declaration."),
        };
        assert_eq!(var_decl.0, "y".to_string());
        assert_eq!(var_decl.1, Type::F32);
        assert_eq!(var_decl.2, Expression::Float(3.14));

        let scanner = Scanner::new("const t: bool = true;");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let var_decl = match stmt.unwrap().node {
            Statement::ConstDecl { name, ty, value } => (name, ty, value),
            _ => panic!("Expected a variable declaration."),
        };
        assert_eq!(var_decl.0, "t".to_string());
        assert_eq!(var_decl.1, Type::Bool);
        assert_eq!(var_decl.2, Expression::Bool(true));

        let scanner = Scanner::new("let x = 1 + 1;");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let var_decl = match stmt.unwrap().node {
            Statement::LetDecl { name, ty, value } => (name, ty, value),
            _ => panic!("Expected a variable declaration."),
        };
        assert_eq!(var_decl.0, "x".to_string());
        assert_eq!(var_decl.1, Type::TypeVar("a0".to_string()));
        assert_eq!(var_decl.2, Expression::Binary {
            lhs: Box::new(Expression::Integer(1)),
            op: TokenKind::Plus,
            rhs: Box::new(Expression::Integer(1)),
        });
    }

    #[test]
    fn test_function_declaration() {
        let scanner = Scanner::new("fn main() { let x = 1; }");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let function_decl = stmt.unwrap().node;
        assert_eq!(function_decl, Statement::FunctionDecl { 
            name: "main".to_string(), 
            params: vec![], 
            return_ty: Type::Void, 
            body: vec![Located::new(Statement::LetDecl { 
                name: "x".to_string(), 
                ty: Type::TypeVar("a0".to_string()), 
                value: Expression::Integer(1), 
            }, 1)] 
        });

        let scanner = Scanner::new("fn add(x: f32, y: f32) -> f32 { return x + y; }");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let function_decl = stmt.unwrap().node;
        assert_eq!(function_decl, Statement::FunctionDecl { 
            name: "add".to_string(), 
            params: vec![("x".to_string(), Type::F32), ("y".to_string(), Type::F32)], 
            return_ty: Type::F32, 
            body: vec![Located::new(Statement::ReturnStmt(Some(Expression::Binary {
                lhs: Box::new(Expression::Identifier("x".to_string())),
                op: TokenKind::Plus,
                rhs: Box::new(Expression::Identifier("y".to_string())),
            })), 1)] 
        });
    }

    #[test]
    fn test_expression_statement() {
        let scanner = Scanner::new("x + 1;");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let expr_stmt = stmt.unwrap().node;
        assert_eq!(expr_stmt, Statement::ExpressionStmt(Expression::Binary {
            lhs: Box::new(Expression::Identifier("x".to_string())),
            op: TokenKind::Plus,
            rhs: Box::new(Expression::Integer(1)),
        }));
    }

    #[test]
    fn test_return_statement() {
        let scanner = Scanner::new("return 1;");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let return_stmt = stmt.unwrap().node;
        assert_eq!(return_stmt, Statement::ReturnStmt(Some(Expression::Integer(1))));
        let scanner = Scanner::new("return;");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let return_stmt = stmt.unwrap().node;
        assert_eq!(return_stmt, Statement::ReturnStmt(None));
    }

    #[test]
    fn test_parse_program() {
        let src = "fn add(l: i8, r: i8) -> i8 { return l + r; }\nfn main() { let x = add(1, 2); }";
        let program = parse(src).unwrap();
        assert_eq!(program.statements.len(), 2);
    }

    #[test]
    fn test_record_declaration() {
        let scanner = Scanner::new("record Point { x: f32, y: f32 }");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let record_decl = stmt.unwrap().node;
        assert_eq!(record_decl, Statement::RecordDecl { 
            name: "Point".to_string(),
            generics: vec![],
            fields: vec![
                ("x".to_string(), Type::F32),
                ("y".to_string(), Type::F32),
            ],
        });
    }

    #[test]
    fn test_alias_declaration() {
        let scanner = Scanner::new("type Vec = [f32; 10]");
        let mut parser = Parser::new(scanner);
        let stmt = statement(&mut parser);
        assert!(stmt.is_some());
        let alias_decl = stmt.unwrap().node;
        assert_eq!(alias_decl, Statement::AliasDecl { 
            name: "Vec".to_string(),
            generics: vec![],
            ty: Type::Array {
                element_type: Box::new(Type::F32),
                size: 10,
            },
        });
    }
}

