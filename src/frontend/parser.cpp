//
// Created by Liam Eckert on 7/25/24.
//

#include "parser.h"

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
        case TokenKind::MUT:
            return declaration();
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


std::unique_ptr<VarDecl> Parser::declaration() {
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

std::unique_ptr<NumberLiteral> Parser::number_literal() {
    // we already know that we have a number at current but there is a chance it is the beginning of a binop
    // TODO: implement binop parsing
    return std::make_unique<NumberLiteral>(current->value);
}