//
// Created by Liam Eckert on 7/25/24.
//

#ifndef NEBULA_PARSER_H
#define NEBULA_PARSER_H


#include <vector>
#include "token.h"
#include "ast.h"

class Parser {
public:
    Parser(std::vector<Token> tokens) : tokens(std::move(tokens)), current(&this->tokens[0]) {};
private:
    std::vector<Token> tokens;
    Token* current;
    std::vector<AstNode> ast;

    Token* advance();
    bool expect(TokenKind kind);
    void expect_or_err(TokenKind kind);
    Token* peek();

    void parse();

    std::unique_ptr<Statement>statement();
    std::unique_ptr<Expression> expression();
    std::unique_ptr<TypeUnit> type();

    std::unique_ptr<Block> block();
    std::unique_ptr<VarDecl> declaration();

    std::unique_ptr<NumberLiteral> number_literal();
};


#endif //NEBULA_PARSER_H
