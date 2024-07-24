//
// Created by Liam Eckert on 7/22/24.
//



#include <unordered_map>

#include "tokenizer.h"

char Tokenizer::peek() {
    if (is_at_end()) return '\0';
    return source[current];
}

char Tokenizer::peekNext() {
    if (current + 1 >= source.size()) return '\0';
    return source[current + 1];
}

char Tokenizer::advance() {
    current++;
    return source[current - 1];
}

void Tokenizer::simple_token(TokenKind kind) {
    tokens.emplace_back(kind, "", line);
}

void Tokenizer::token(TokenKind kind) {
    std::string lexeme = get_lexeme();
    tokens.emplace_back(kind, lexeme, line);
}

void Tokenizer::error_token(const std::string& message) {
    tokens.emplace_back(TokenKind::ERROR, message, line);
}

std::string Tokenizer::get_lexeme() {
    return source.substr(start, current - start);
}

void Tokenizer::number() {
    while (std::isdigit(peek())) advance();
    if (peek() == '.' && std::isdigit(peekNext())) {
        advance();
        while (std::isdigit(peek())) advance();
    }
    token(TokenKind::NUMBER);
}

void Tokenizer::string() {
    while (peek() != '"' && !is_at_end()) {
        if (peek() == '\n') line++;
        advance();
    }
    // unterminated string
    if (is_at_end()) {
        error_token("Unterminated string on line " + std::to_string(line));
        return;
    }
    // consume closing quote
    advance();
    token(TokenKind::STRING);
}

 /// KEYWORD_MAP is a hash map that maps keywords to their respective TokenKind
 const std::unordered_map<std::string, TokenKind> KEYWORD_MAP = {
        {"if", TokenKind::IF},
        {"else", TokenKind::ELSE},
        {"elif", TokenKind::ELIF},
        {"match", TokenKind::MATCH},
        {"case", TokenKind::CASE},
        {"default", TokenKind::DEFAULT},
        {"while", TokenKind::WHILE},
        {"break", TokenKind::BREAK},
        {"mut", TokenKind::MUT},
        {"fn", TokenKind::FN},
        {"return", TokenKind::RETURN},
        {"lambda", TokenKind::LAMBDA},
        {"for", TokenKind::FOR},
        {"in", TokenKind::IN}
};

void Tokenizer::symbol() {
    while (std::isalnum(peek()) || peek() == '_') advance();
    std::string lexeme = get_lexeme();
    if (KEYWORD_MAP.find(lexeme) != KEYWORD_MAP.end()) {
        tokens.emplace_back(KEYWORD_MAP.at(lexeme), lexeme, line);
    } else {
        tokens.emplace_back(TokenKind::SYMBOL, lexeme, line);
    }
}