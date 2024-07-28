//
// Created by Liam Eckert on 7/25/24.
//

#include "frontend/parser.hpp"

/// advance forward one token
Token* Parser::advance() {
    if (current->kind != TokenKind::ENDMARKER) {
        current++;
    }
    return current;
}

/// expect the current token to be of a certain kind
/// used for branching in the parser
bool Parser::expect(TokenKind kind) {
    return current->kind == kind;
}

/// expect the current token to be of a certain kind and throw an error if it is not
/// used when only one kind of token is possible
void Parser::expect_or_err(TokenKind kind) {
    if (!expect(kind)) {
        throw std::runtime_error("Expected " + token_kind_string(kind) + " but got " + token_kind_string(current->kind) + " on line " + std::to_string(current->line));
    }
}

/// peek at the next token
Token* Parser::peek() {
    if (current->kind != TokenKind::ENDMARKER) {
        return current + 1;
    }
    return current;
}

std::unique_ptr<TypeUnit> Parser::type() {
    // called when a colon is encountered or when a type is expected
    expect_or_err(TokenKind::SYMBOL);
    std::string name = current->value;
    advance();
    if (current->kind == TokenKind::LESS) {
        advance();
        std::vector<std::string> type_variables;
        while (current->kind != TokenKind::GREATER) {
            expect_or_err(TokenKind::SYMBOL);
            type_variables.push_back(current->value);
            advance();
            if (current->kind == TokenKind::COMMA) {
                advance();
            }
        }
        advance();
        return std::make_unique<GenericType>(name, type_variables);
    }
    return std::make_unique<BaseType>(name);
}

/// parse a statement
std::unique_ptr<Statement> Parser::statement() {
    switch (current->kind) {
        case TokenKind::INDENT:
            return block();
        case TokenKind::FN:
            return function_declaration();
        case TokenKind::RETURN:
            return return_statement();
        case TokenKind::MUT:
            return var_decl();
        case TokenKind::SYMBOL:
            if (peek()->kind == TokenKind::EQUAL) {
                return var_decl();
            } else {
                auto expr = expression();
                return std::make_unique<ExprStmt>(std::move(expr));
            }
        case TokenKind::IF:
            return if_statement();
        default:
            throw std::runtime_error("Expected a valid statement instead got " + token_kind_string(current->kind) + " on line " + std::to_string(current->line));
    }
}

/// parse an expression
std::unique_ptr<Expression> Parser::expression() {
    switch (current->kind) {
        case TokenKind::NUMBER:
            return number_literal();
        default:
            throw std::runtime_error("Expected a valid expression instead got " + token_kind_string(current->kind) + " on line " + std::to_string(current->line));
    }
}

/// parse a block of code
/// called when current token is an INDENT
std::unique_ptr<Block> Parser::block() {
    // advance past the INDENT token
    advance();
    // create a new block
    std::vector<std::unique_ptr<Statement>> statements;
    // while the current token is not a DEDENT
    while (current->kind != TokenKind::DEDENT && current->kind != TokenKind::ENDMARKER) {
        // parse a statement and add it to the block
        auto block_statement = statement();
        statements.push_back(std::move(block_statement));
    }
    // advance past the DEDENT token
    advance();
    return std::make_unique<Block>(std::move(statements));
}


std::unique_ptr<VarDecl> Parser::var_decl() {
    bool is_mut = false;
    if (current->kind == TokenKind::MUT) {
        is_mut = true;
        advance();
    }
    // expect a symbol
    expect_or_err(TokenKind::SYMBOL);
    std::string name = current->value;
    advance();
    std::unique_ptr<TypeUnit> ty = nullptr;
    // if there is a colon, parse a type
    if (current->kind == TokenKind::COLON) {
        advance();
        ty = type();
    }
    expect_or_err(TokenKind::EQUAL);
    advance();
    auto value = expression();
    return std::make_unique<VarDecl>(is_mut, name, std::move(ty), std::move(value));
}

std::unique_ptr<FunctionDef> Parser::function_declaration() {
    // called when current token is FN
    advance();
    expect_or_err(TokenKind::SYMBOL);
    std::string name = current->value;
    advance();
    std::vector<std::string> type_variables = {};
    if (expect(TokenKind::LESS)) {
        // this is a generic function
        advance();
        while (current->kind != TokenKind::GREATER) {
            expect_or_err(TokenKind::SYMBOL);
            type_variables.push_back(current->value);
            advance();
            if (current->kind == TokenKind::COMMA) {
                advance();
            }
        }
        expect_or_err(TokenKind::GREATER);
        advance();
    }
    expect_or_err(TokenKind::LPAREN);
    advance();
    std::vector<std::unique_ptr<FunctionParameter>> parameters;
    while (current->kind != TokenKind::RPAREN) {
        expect_or_err(TokenKind::SYMBOL);
        std::string param_name = current->value;
        advance();
        expect_or_err(TokenKind::COLON);
        advance();
        auto param_ty = type();
        parameters.push_back(std::make_unique<FunctionParameter>(param_name, std::move(param_ty)));
        if (current->kind == TokenKind::COMMA) {
            advance();
        }
    }
    expect_or_err(TokenKind::RPAREN);
    advance();
    expect_or_err(TokenKind::COLON);
    advance();
    auto return_ty = type();
    expect_or_err(TokenKind::INDENT);
    auto body = block();
    if (type_variables.empty()) {
        return std::make_unique<FunctionDef>(name, std::move(parameters), std::move(return_ty), std::move(body));
    } else {
        return std::make_unique<FunctionDef>(name, std::move(parameters), type_variables, std::move(return_ty), std::move(body));

    }
}

std::unique_ptr<Return> Parser::return_statement() {
    // called when current token is RETURN
    advance();
    auto value = expression();
    return std::make_unique<Return>(std::move(value));
}

std::unique_ptr<If> Parser::if_statement() {
    // called when current token is IF or ELIF
    advance();
    auto condition = expression();
    expect_or_err(TokenKind::COLON);
    advance();
    expect_or_err(TokenKind::INDENT);
    auto body = block();
    std::unique_ptr<Statement> else_body = nullptr;
    if (current->kind == TokenKind::ELSE) {
        advance();
        expect_or_err(TokenKind::COLON);
        advance();
        expect_or_err(TokenKind::INDENT);
        else_body = block();
    } else if (current->kind == TokenKind::ELIF) {
        else_body = if_statement();
    }
    return std::make_unique<If>(std::move(condition), std::move(body), std::move(else_body));
}

std::unique_ptr<NumberLiteral> Parser::number_literal() {
    // we already know that we have a number at current but there is a chance it is the beginning of a binop
    // TODO: implement binop parsing
    return std::make_unique<NumberLiteral>(current->value);
}