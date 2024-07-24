//
// Created by Liam Eckert on 7/22/24.
//

#ifndef NEBULA_TOKEN_H
#define NEBULA_TOKEN_H

#include <string>
#include <utility>
#include <iostream>

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
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    COMMA,
    COLON,
    SEMICOLON,
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
    COLON_COLON,
    MINUS_GREATER, // ->
    EQUAL_GREATER, // =>
    // keywords
    IF,
    ELSE,
    ELIF,
    MATCH,
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
    uint8_t line;
    Token(TokenKind kind, std::string value, uint8_t line) : kind(kind), value(std::move(value)), line(line) {}
    Token(TokenKind kind, uint8_t line) : kind(kind), line(line) {}
    friend std::ostream& operator<<(std::ostream& os, const Token& token);
};



class IndentToken : public Token {
public:
    uint8_t level;
    explicit IndentToken(uint8_t level, uint8_t line) : Token(TokenKind::INDENT, line), level(level) {}
};

class DedentToken : public Token {
public:
    uint8_t level;
    explicit DedentToken(uint8_t level, uint8_t line) : Token(TokenKind::DEDENT, line), level(level) {}
};

#endif //NEBULA_TOKEN_H
