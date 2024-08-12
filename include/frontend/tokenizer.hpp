//
// Created by Liam Eckert on 7/22/24.
//

#ifndef NEBULA_TOKENIZER_H
#define NEBULA_TOKENIZER_H

#include <vector>
#include "token.hpp"

class Tokenizer {
private:
    size_t line;
    std::string source;
    size_t start;
    size_t current;
    std::vector<Token> tokens;
    bool finished = false;

    char peek();
    char peek_next();
    char advance();
    void simple_token(TokenKind kind);
    void token(TokenKind kind);
    Lexeme get_lexeme();
    void error_token(const std::string& message);
    void number();
    void string();
    void symbol();
    void skip_whitespace();
    void docstring();
    void next_token();
    bool is_at_end();
public:
    Tokenizer(std::string source) : source(std::move(source)), line(1), start(0), current(0) {};
    std::vector<Token> tokenize();
};

#endif //NEBULA_TOKENIZER_H
