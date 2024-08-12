//
// Created by Liam Eckert on 7/22/24.
//

#ifndef NEBULA_TOKEN_HPP
#define NEBULA_TOKEN_HPP

#include <string>
#include <utility>
#include <iostream>
#include <optional>

enum class TokenKind {
    ERROR,
    DOCSTRING,
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
    AND,
    OR,
    NOT,
    TRUE,
    FALSE,
    TRAIT,
    CLASS,
    IMPL,
    ENUM,
    TYPE,
    IMPORT,
    FROM,
    AS,
};

std::string token_kind_string(TokenKind kind);

class Lexeme {
private:
    const std::string& source;
    size_t start;
    size_t length;
public:
    Lexeme(const std::string& source, size_t start, size_t length) : source(source), start(start), length(length) {};
    [[nodiscard]] std::string_view view() const {
        return std::string_view(source).substr(start, length);
    }
};


class Token {
public:
    TokenKind kind;
    Lexeme lexeme;
    size_t line;
    Token(TokenKind kind, Lexeme lexeme, size_t line) : kind(kind), lexeme(lexeme), line(line) {}
    friend std::ostream& operator<<(std::ostream& os, const Token& token);
};

#endif //NEBULA_TOKEN_HPP
