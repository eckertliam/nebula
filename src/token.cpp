//
// Created by Liam Eckert on 7/23/24.
//

#include "token.h"

std::ostream& operator<<(std::ostream& os, const Token& token) {
    switch (token.kind) {
        case TokenKind::ERROR:
            os << "ERROR: " << token.value << " on line " << static_cast<int>(token.line);
            break;
        case TokenKind::INDENT:
            os << "INDENT: " << static_cast<int>(token.line);
            break;
        case TokenKind::DEDENT:
            os << "DEDENT: " << static_cast<int>(token.line);
            break;
        case TokenKind::NEWLINE:
            os << "NEWLINE: " << static_cast<int>(token.line);
            break;
        case TokenKind::ENDMARKER:
            os << "ENDMARKER: " << static_cast<int>(token.line);
            break;
        case TokenKind::SYMBOL:
            os << "SYMBOL: " << token.value << " on line " << static_cast<int>(token.line);
            break;
        case TokenKind::NUMBER:
            os << "NUMBER: " << token.value << " on line " << static_cast<int>(token.line);
            break;
        case TokenKind::STRING:
            os << "STRING: " << token.value << " on line " << static_cast<int>(token.line);
            break;
        case TokenKind::PLUS:
            os << "PLUS: " << static_cast<int>(token.line);
            break;
        case TokenKind::MINUS:
            os << "MINUS: " << static_cast<int>(token.line);
            break;
        case TokenKind::STAR:
            os << "STAR: " << static_cast<int>(token.line);
            break;
        case TokenKind::SLASH:
            os << "SLASH: " << static_cast<int>(token.line);
            break;
        case TokenKind::PERCENT:
            os << "PERCENT: " << static_cast<int>(token.line);
            break;
        case TokenKind::AMPERSAND:
            os << "AMPERSAND: " << static_cast<int>(token.line);
            break;
        case TokenKind::PIPE:
            os << "PIPE: " << static_cast<int>(token.line);
            break;
        case TokenKind::CARET:
            os << "CARET: " << static_cast<int>(token.line);
            break;
        case TokenKind::BANG:
            os << "BANG: " << static_cast<int>(token.line);
            break;
        case TokenKind::LESS:
            os << "LESS: " << static_cast<int>(token.line);
            break;
        case TokenKind::EQUAL:
            os << "EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::GREATER:
            os << "GREATER: " << static_cast<int>(token.line);
            break;
        case TokenKind::DOT:
            os << "DOT: " << static_cast<int>(token.line);
            break;
        case TokenKind::LPAREN:
            os << "LPAREN: " << static_cast<int>(token.line);
            break;
        case TokenKind::RPAREN:
            os << "RPAREN: " << static_cast<int>(token.line);
            break;
        case TokenKind::LBRACE:
            os << "LBRACE: " << static_cast<int>(token.line);
            break;
        case TokenKind::RBRACE:
            os << "RBRACE: " << static_cast<int>(token.line);
            break;
        case TokenKind::LBRACKET:
            os << "LBRACKET: " << static_cast<int>(token.line);
            break;
        case TokenKind::RBRACKET:
            os << "RBRACKET: " << static_cast<int>(token.line);
            break;
        case TokenKind::COMMA:
            os << "COMMA: " << static_cast<int>(token.line);
            break;
        case TokenKind::COLON:
            os << "COLON: " << static_cast<int>(token.line);
            break;
        case TokenKind::SEMICOLON:
            os << "SEMICOLON: " << static_cast<int>(token.line);
            break;
        case TokenKind::PLUS_EQUAL:
            os << "PLUS_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::MINUS_EQUAL:
            os << "MINUS_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::STAR_EQUAL:
            os << "STAR_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::SLASH_EQUAL:
            os << "SLASH_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::PERCENT_EQUAL:
            os << "PERCENT_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::AMPERSAND_EQUAL:
            os << "AMPERSAND_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::PIPE_EQUAL:
            os << "PIPE_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::CARET_EQUAL:
            os << "CARET_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::EQUAL_EQUAL:
            os << "EQUAL_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::BANG_EQUAL:
            os << "BANG_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::LESS_EQUAL:
            os << "LESS_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::GREATER_EQUAL:
            os << "GREATER_EQUAL: " << static_cast<int>(token.line);
            break;
        case TokenKind::DOT_DOT:
            os << "DOT_DOT: " << static_cast<int>(token.line);
            break;
        case TokenKind::COLON_COLON:
            os << "COLON_COLON: " << static_cast<int>(token.line);
            break;
        case TokenKind::MINUS_GREATER:
            os << "MINUS_GREATER: " << static_cast<int>(token.line);
            break;
        case TokenKind::EQUAL_GREATER:
            os << "EQUAL_GREATER: " << static_cast<int>(token.line);
            break;
        case TokenKind::IF:
            os << "IF: " << static_cast<int>(token.line);
            break;
        case TokenKind::ELSE:
            os << "ELSE: " << static_cast<int>(token.line);
            break;
        case TokenKind::ELIF:
            os << "ELIF: " << static_cast<int>(token.line);
            break;
        case TokenKind::MATCH:
            os << "MATCH: " << static_cast<int>(token.line);
            break;
        case TokenKind::WHILE:
            os << "WHILE: " << static_cast<int>(token.line);
            break;
        case TokenKind::BREAK:
            os << "BREAK: " << static_cast<int>(token.line);
            break;
        case TokenKind::MUT:
            os << "MUT: " << static_cast<int>(token.line);
            break;
        case TokenKind::FN:
            os << "FN: " << static_cast<int>(token.line);
            break;
        case TokenKind::RETURN:
            os << "RETURN: " << static_cast<int>(token.line);
            break;
        case TokenKind::LAMBDA:
            os << "LAMBDA: " << static_cast<int>(token.line);
            break;
        case TokenKind::FOR:
            os << "FOR: " << static_cast<int>(token.line);
            break;
        case TokenKind::IN:
            os << "IN: " << static_cast<int>(token.line);
            break;
    }
    return os;
}