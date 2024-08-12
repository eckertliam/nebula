//
// Created by Liam Eckert on 7/22/24.
//



#include <unordered_map>
#include <iostream>
#include "frontend/token.hpp"
#include "frontend/tokenizer.hpp"

/// peek - returns the current character in the source code
char Tokenizer::peek() {
    if (is_at_end()) return '\0';
    return source[current];
}

/// peek_next - returns the next character in the source code
char Tokenizer::peek_next() {
    if (current + 1 >= source.size()) return '\0';
    return source[current + 1];
}


/// advance - increments the current character in the source code
char Tokenizer::advance() {
    current++;
    return source[current - 1];
}

/// simple_token - creates a token with no lexeme
void Tokenizer::simple_token(TokenKind kind) {
    Lexeme lexeme = {"", 0, 0};
    tokens.emplace_back(kind, lexeme, line);
}

/// token - creates a token with a lexeme
void Tokenizer::token(TokenKind kind) {
    Lexeme lexeme = get_lexeme();
    tokens.emplace_back(kind, lexeme, line);
}

/// error_token - creates an error token with a message
void Tokenizer::error_token(const std::string& message) {
    Lexeme lexeme = {message, 0, message.size()};
    tokens.emplace_back(TokenKind::ERROR, lexeme, line);
}

/// get_lexeme - returns the lexeme of the current token
Lexeme Tokenizer::get_lexeme() {
    return {source, start, current - start};
}

/// number - consumes a number and creates a number token
void Tokenizer::number() {
    while (std::isdigit(peek())) advance();
    if (peek() == '.' && std::isdigit(peek_next())) {
        advance();
        while (std::isdigit(peek())) advance();
    }
    token(TokenKind::NUMBER);
}

/// string - consumes a string and creates a string token
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
    // remove quotes
    Lexeme lexeme = {source, start + 1, current - start - 2};
    tokens.emplace_back(TokenKind::STRING, lexeme, line);
}

 /// KEYWORD_MAP is a hash map that maps keywords to their respective TokenKind
 const std::unordered_map<std::string, TokenKind> KEYWORD_MAP = {
        {"if", TokenKind::IF},
        {"else", TokenKind::ELSE},
        {"elif", TokenKind::ELIF},
        {"match", TokenKind::MATCH},
        {"while", TokenKind::WHILE},
        {"break", TokenKind::BREAK},
        {"mut", TokenKind::MUT},
        {"fn", TokenKind::FN},
        {"return", TokenKind::RETURN},
        {"lambda", TokenKind::LAMBDA},
        {"for", TokenKind::FOR},
        {"in", TokenKind::IN},
        {"and", TokenKind::AND},
        {"or", TokenKind::OR},
        {"not", TokenKind::NOT},
        {"true", TokenKind::TRUE},
        {"false", TokenKind::FALSE},
        {"trait", TokenKind::TRAIT},
        {"class", TokenKind::CLASS},
        {"impl", TokenKind::IMPL},
        {"type", TokenKind::TYPE},
        {"enum", TokenKind::ENUM},
};

/// symbol - consumes a symbol and creates a symbol token checking for keywords
void Tokenizer::symbol() {
    while (std::isalnum(peek()) || peek() == '_') advance();
    // we need the literal symbol
    std::string literal = source.substr(start, current - start);
    if (KEYWORD_MAP.find(literal) != KEYWORD_MAP.end()) {
        tokens.emplace_back(KEYWORD_MAP.at(literal), get_lexeme(), line);
    } else {
        tokens.emplace_back(TokenKind::SYMBOL, get_lexeme(), line);
    }
}

void Tokenizer::docstring() {
    while (peek() != '\n' && !is_at_end()) {
        advance();
    }
    // unterminated string
    if (is_at_end()) {
        error_token("Unterminated docstring on line " + std::to_string(line));
        return;
    }
    // remove the three slashes from the beginning of the docstring
    Lexeme lexeme = {source, start + 3, current - start - 3};
    tokens.emplace_back(TokenKind::DOCSTRING, lexeme, line);
}

void Tokenizer::skip_whitespace() {
    while (std::isspace(peek())) {
        if (peek() == '\n') line++;
        advance();
    }
}

/// next_token - consumes the next token in the source code
void Tokenizer::next_token() {
    skip_whitespace();
    start = current;
    char c = advance();
    switch (c) {
        case '+':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::PLUS_EQUAL);
            } else {
                simple_token(TokenKind::PLUS);
            }
            break;
        case '-':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::MINUS_EQUAL);
            } else if (peek() == '>') {
                advance();
                simple_token(TokenKind::MINUS_GREATER);
            } else {
                simple_token(TokenKind::MINUS);
            }
            break;
        case '*':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::STAR_EQUAL);
            } else {
                simple_token(TokenKind::STAR);
            }
            break;
        case '/':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::SLASH_EQUAL);
            } else if (peek() == '/') {
                // consume the slash
                advance();
                if (peek() == '/') {
                    // consume the third slash
                    advance();
                    docstring();
                    return;
                }
                while (peek() != '\n' && !is_at_end()) advance();
                if (!is_at_end()) next_token();
            } else {
                simple_token(TokenKind::SLASH);
            }
            break;
        case '%':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::PERCENT_EQUAL);
            } else {
                simple_token(TokenKind::PERCENT);
            }
            break;
        case '&':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::AMPERSAND_EQUAL);
            } else {
                simple_token(TokenKind::AMPERSAND);
            }
            break;
        case '|':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::PIPE_EQUAL);
            } else {
                simple_token(TokenKind::PIPE);
            }
            break;
        case '^':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::CARET_EQUAL);
            } else {
                simple_token(TokenKind::CARET);
            }
            break;
        case '!':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::BANG_EQUAL);
            } else {
                simple_token(TokenKind::BANG);
            }
            break;
        case '<':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::LESS_EQUAL);
            } else {
                simple_token(TokenKind::LESS);
            }
            break;
        case '>':
            if (peek() == '=') {
                advance();
                simple_token(TokenKind::GREATER_EQUAL);
            } else {
                simple_token(TokenKind::GREATER);
            }
            break;
        case '=':
            if (peek() == '>') {
                advance();
                simple_token(TokenKind::EQUAL_GREATER);
            } else if (peek() == '=') {
                advance();
                simple_token(TokenKind::EQUAL_EQUAL);
            } else {
                simple_token(TokenKind::EQUAL);
            }
            break;
        case '.':
            if (peek() == '.') {
                advance();
                simple_token(TokenKind::DOT_DOT);
            } else {
                simple_token(TokenKind::DOT);
            }
            break;
        case ':':
            if (peek() == ':') {
                advance();
                simple_token(TokenKind::COLON_COLON);
            } else {
                simple_token(TokenKind::COLON);
            }
            break;
        case '(':
            simple_token(TokenKind::LPAREN);
            break;
        case ')':
            simple_token(TokenKind::RPAREN);
            break;
        case '{':
            simple_token(TokenKind::LBRACE);
            break;
        case '}':
            simple_token(TokenKind::RBRACE);
            break;
        case '[':
            simple_token(TokenKind::LBRACKET);
            break;
        case ']':
            simple_token(TokenKind::RBRACKET);
            break;
        case ',':
            simple_token(TokenKind::COMMA);
            break;
        case ';':
            simple_token(TokenKind::SEMICOLON);
            break;
        case '"':
            string();
            break;
        case '\0':
            simple_token(TokenKind::ENDMARKER);
            finished = true;
            break;
        default:
            if (std::isdigit(c)) {
                number();
            } else if (std::isalpha(c) || c == '_') {
                symbol();
            } else {
                // get the literal lexeme
                std::string literal = source.substr(start, current - start);
                error_token("Unexpected lexeme " + literal);
                finished = true;
            }
    }
}

/// is_at_end - returns true if the tokenizer has reached the end of the source code
bool Tokenizer::is_at_end() {
    finished = current >= source.size();
    return finished;
}

/// tokenize - returns a vector of tokens from the source code
std::vector<Token> Tokenizer::tokenize() {
    while (!finished) {
        next_token();
    }
    return tokens;
}