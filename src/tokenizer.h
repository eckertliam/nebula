//
// Created by Liam Eckert on 7/22/24.
//

#include <vector>
#include "token.h"

#ifndef NEBULA_TOKENIZER_H
#define NEBULA_TOKENIZER_H


class Tokenizer {
private:
    /// indentation level stack
    std::vector<uint8_t> indents = {0};
    uint8_t line;
    std::string source;
    uint8_t start;
    uint8_t current;
    std::vector<Token> tokens;

    char peek();
    char peek_next();
    char advance();
    void simple_token(TokenKind kind);
    void token(TokenKind kind);
    std::string get_lexeme();
    void error_token(const std::string& message);
    void number();
    void string();
    void symbol();
    void newline();
    void indentation();
    void next_token();
    bool is_at_end();
public:
    Tokenizer(std::string source) : source(std::move(source)), line(1), start(0), current(0) {};
    std::vector<Token> tokenize();
};

#endif //NEBULA_TOKENIZER_H
