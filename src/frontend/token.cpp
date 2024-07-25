//
// Created by Liam Eckert on 7/23/24.
//

#include "token.h"

std::string token_kind_string(TokenKind kind) {
    switch (kind) {
        case TokenKind::ERROR:
            return "ERROR";
        case TokenKind::DOCSTRING:
            return "DOCSTRING";
        case TokenKind::INDENT:
            return "INDENT";
        case TokenKind::DEDENT:
            return "DEDENT";
        case TokenKind::NEWLINE:
            return "NEWLINE";
        case TokenKind::ENDMARKER:
            return "ENDMARKER";
        case TokenKind::NUMBER:
            return "NUMBER";
        case TokenKind::STRING:
            return "STRING";
        case TokenKind::SYMBOL:
            return "SYMBOL";
        case TokenKind::PLUS:
            return "PLUS";
        case TokenKind::MINUS:
            return "MINUS";
        case TokenKind::STAR:
            return "STAR";
        case TokenKind::SLASH:
            return "SLASH";
        case TokenKind::PERCENT:
            return "PERCENT";
        case TokenKind::AMPERSAND:
            return "AMPERSAND";
        case TokenKind::PIPE:
            return "PIPE";
        case TokenKind::CARET:
            return "CARET";
        case TokenKind::BANG:
            return "BANG";
        case TokenKind::LESS:
            return "LESS";
        case TokenKind::EQUAL:
            return "EQUAL";
        case TokenKind::GREATER:
            return "GREATER";
        case TokenKind::DOT:
            return "DOT";
        case TokenKind::LPAREN:
            return "LPAREN";
        case TokenKind::RPAREN:
            return "RPAREN";
        case TokenKind::LBRACE:
            return "LBRACE";
        case TokenKind::RBRACE:
            return "RBRACE";
        case TokenKind::LBRACKET:
            return "LBRACKET";
        case TokenKind::RBRACKET:
            return "RBRACKET";
        case TokenKind::COMMA:
            return "COMMA";
        case TokenKind::COLON:
            return "COLON";
        case TokenKind::SEMICOLON:
            return "SEMICOLON";
        case TokenKind::PLUS_EQUAL:
            return "PLUS_EQUAL";
        case TokenKind::MINUS_EQUAL:
            return "MINUS_EQUAL";
        case TokenKind::STAR_EQUAL:
            return "STAR_EQUAL";
        case TokenKind::SLASH_EQUAL:
            return "SLASH_EQUAL";
        case TokenKind::PERCENT_EQUAL:
            return "PERCENT_EQUAL";
        case TokenKind::AMPERSAND_EQUAL:
            return "AMPERSAND_EQUAL";
        case TokenKind::PIPE_EQUAL:
            return "PIPE_EQUAL";
        case TokenKind::CARET_EQUAL:
            return "CARET_EQUAL";
        case TokenKind::EQUAL_EQUAL:
            return "EQUAL_EQUAL";
        case TokenKind::BANG_EQUAL:
            return "BANG_EQUAL";
        case TokenKind::LESS_EQUAL:
            return "LESS_EQUAL";
        case TokenKind::GREATER_EQUAL:
            return "GREATER_EQUAL";
        case TokenKind::DOT_DOT:
            return "DOT_DOT";
        case TokenKind::COLON_COLON:
            return "COLON_COLON";
        case TokenKind::MINUS_GREATER:
            return "MINUS_GREATER";
        case TokenKind::EQUAL_GREATER:
            return "EQUAL_GREATER";
        case TokenKind::IF:
            return "IF";
        case TokenKind::ELSE:
            return "ELSE";
        case TokenKind::ELIF:
            return "ELIF";
        case TokenKind::MATCH:
            return "MATCH";
        case TokenKind::WHILE:
            return "WHILE";
        case TokenKind::BREAK:
            return "BREAK";
        case TokenKind::MUT:
            return "MUT";
        case TokenKind::FN:
            return "FN";
        case TokenKind::RETURN:
            return "RETURN";
        case TokenKind::LAMBDA:
            return "LAMBDA";
        case TokenKind::FOR:
            return "FOR";
        case TokenKind::IN:
            return "IN";
        case TokenKind::AND:
            return "AND";
        case TokenKind::OR:
            return "OR";
        case TokenKind::NOT:
            return "NOT";
        case TokenKind::TRUE:
            return "TRUE";
        case TokenKind::FALSE:
            return "FALSE";
        case TokenKind::TRAIT:
            return "TRAIT";
        case TokenKind::CLASS:
            return "CLASS";
        case TokenKind::IMPL:
            return "IMPL";
        case TokenKind::ENUM:
            return "ENUM";
        case TokenKind::TYPE:
            return "TYPE";
        case TokenKind::IMPORT:
            return "IMPORT";
        case TokenKind::FROM:
            return "FROM";
        case TokenKind::AS:
            return "AS";
    }
}

std::ostream& operator<<(std::ostream& os, const Token& token) {
    std::string line = std::to_string(token.line);
    std::string kind = token_kind_string(token.kind);
    std::string value;
    if (kind == "ERROR" || kind == "DOCSTRING" || kind == "NUMBER" || kind == "STRING" || kind == "SYMBOL") {
        value = token.value;
    }
    if (value.empty()) {
        os << kind << " line " << line;
    } else {
        os << kind << " " << value << " line " << line;
    }
    return os;
}