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

/// parse a statement
Statement Parser::statement() {
    switch (current->kind) {
        case TokenKind::INDENT:
            return block();
        case TokenKind::MUT:
            return declaration();
    }
}

/// parse a block of code
/// called when current token is an INDENT
Block Parser::block() {
    // advance past the INDENT token
    advance();
    // create a new block
    Block block;
    // while the current token is not a DEDENT
    while (current->kind != TokenKind::DEDENT && current->kind != TokenKind::ENDMARKER) {
        // parse a statement and add it to the block
        auto block_statement = statement();
        block.push(block_statement);
    }
    // advance past the DEDENT token
    advance();
    return block;
}

VarDecl Parser::declaration() {
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
        ty = std::make_unique<TypeUnit>(type());
    }
    expect_or_err(TokenKind::EQUAL);
    advance();
    auto value = std::make_unique<Expression>(expression());
    return {is_mut, name, std::move(ty), std::move(value)};
}
