//
// Created by Liam Eckert on 7/22/24.
//

#ifndef NEBULA_TOKEN_H
#define NEBULA_TOKEN_H

#include <string>
#include <utility>

enum class TokenKind {
    ERROR,
    INDENT,
    DEDENT,
    NEWLINE,
    ENDMARKER,
    SYMBOL,
    NUMBER,
    STRING,
    // single-character tokens
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    AMPERSAND,
    PIPE,
    CARET,
    BANG,
    LESS,
    EQUAL,
    GREATER,
    DOT,
    // double-character tokens
    PLUS_EQUAL,
    MINUS_EQUAL,
    STAR_EQUAL,
    SLASH_EQUAL,
    PERCENT_EQUAL,
    AMPERSAND_EQUAL,
    PIPE_EQUAL,
    CARET_EQUAL,
    EQUAL_EQUAL,
    BANG_EQUAL,
    LESS_EQUAL,
    GREATER_EQUAL,
    DOT_DOT,
    MINUS_GREATER, // ->
    EQUAL_GREATER, // =>
    // keywords
    IF,
    ELSE,
    ELIF,
    MATCH,
    CASE,
    DEFAULT,
    WHILE,
    BREAK,
    MUT,
    FN,
    RETURN,
    LAMBDA,
    FOR,
    IN,
};

class Token {
public:
    TokenKind kind;
    std::string value;
    int line;
    Token(TokenKind kind, std::string value, int line) : kind(kind), value(std::move(value)), line(line) {}
    Token(TokenKind kind, int line) : kind(kind), line(line) {}
};


#endif //NEBULA_TOKEN_H
