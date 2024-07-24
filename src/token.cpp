//
// Created by Liam Eckert on 7/23/24.
//

#include "token.h"

std::ostream& operator<<(std::ostream& os, const Token& token) {
    std::string line = std::to_string(token.line);
    std::string kind;
    std::string value;
    switch (token.kind) {
        case TokenKind::ERROR:
            kind = "ERROR";
            value = token.value;
            break;
        case TokenKind::DOCSTRING:
            kind = "DOCSTRING";
            value = token.value;
            break;
        case TokenKind::INDENT:
            kind = "INDENT";
            break;
        case TokenKind::DEDENT:
            kind = "DEDENT";
            break;
        case TokenKind::NEWLINE:
            kind = "NEWLINE";
            break;
        case TokenKind::ENDMARKER:
            kind = "ENDMARKER";
            break;
        case TokenKind::NUMBER:
            kind = "NUMBER";
            value = token.value;
            break;
        case TokenKind::STRING:
            kind = "STRING";
            value = token.value;
            break;
        case TokenKind::SYMBOL:
            kind = "SYMBOL";
            value = token.value;
            break;
        case TokenKind::PLUS:
            kind = "PLUS";
            break;
        case TokenKind::MINUS:
            kind = "MINUS";
            break;
        case TokenKind::STAR:
            kind = "STAR";
            break;
        case TokenKind::SLASH:
            kind = "SLASH";
            break;
        case TokenKind::PERCENT:
            kind = "PERCENT";
            break;
        case TokenKind::AMPERSAND:
            kind = "AMPERSAND";
            break;
        case TokenKind::PIPE:
            kind = "PIPE";
            break;
        case TokenKind::CARET:
            kind = "CARET";
            break;
        case TokenKind::BANG:
            kind = "BANG";
            break;
        case TokenKind::LESS:
            kind = "LESS";
            break;
        case TokenKind::EQUAL:
            kind = "EQUAL";
            break;
        case TokenKind::GREATER:
            kind = "GREATER";
            break;
        case TokenKind::DOT:
            kind = "DOT";
            break;
        case TokenKind::LPAREN:
            kind = "LPAREN";
            break;
        case TokenKind::RPAREN:
            kind = "RPAREN";
            break;
        case TokenKind::LBRACE:
            kind = "LBRACE";
            break;
        case TokenKind::RBRACE:
            kind = "RBRACE";
            break;
        case TokenKind::LBRACKET:
            kind = "LBRACKET";
            break;
        case TokenKind::RBRACKET:
            kind = "RBRACKET";
            break;
        case TokenKind::COMMA:
            kind = "COMMA";
            break;
        case TokenKind::COLON:
            kind = "COLON";
            break;
        case TokenKind::SEMICOLON:
            kind = "SEMICOLON";
            break;
        case TokenKind::PLUS_EQUAL:
            kind = "PLUS_EQUAL";
            break;
        case TokenKind::MINUS_EQUAL:
            kind = "MINUS_EQUAL";
            break;
        case TokenKind::STAR_EQUAL:
            kind = "STAR_EQUAL";
            break;
        case TokenKind::SLASH_EQUAL:
            kind = "SLASH_EQUAL";
            break;
        case TokenKind::PERCENT_EQUAL:
            kind = "PERCENT_EQUAL";
            break;
        case TokenKind::AMPERSAND_EQUAL:
            kind = "AMPERSAND_EQUAL";
            break;
        case TokenKind::PIPE_EQUAL:
            kind = "PIPE_EQUAL";
            break;
        case TokenKind::CARET_EQUAL:
            kind = "CARET_EQUAL";
            break;
        case TokenKind::EQUAL_EQUAL:
            kind = "EQUAL_EQUAL";
            break;
        case TokenKind::BANG_EQUAL:
            kind = "BANG_EQUAL";
            break;
        case TokenKind::LESS_EQUAL:
            kind = "LESS_EQUAL";
            break;
        case TokenKind::GREATER_EQUAL:
            kind = "GREATER_EQUAL";
            break;
        case TokenKind::DOT_DOT:
            kind = "DOT_DOT";
            break;
        case TokenKind::COLON_COLON:
            kind = "COLON_COLON";
            break;
        case TokenKind::MINUS_GREATER:
            kind = "MINUS_GREATER";
            break;
        case TokenKind::EQUAL_GREATER:
            kind = "EQUAL_GREATER";
            break;
        case TokenKind::IF:
            kind = "IF";
            break;
        case TokenKind::ELSE:
            kind = "ELSE";
            break;
        case TokenKind::ELIF:
            kind = "ELIF";
            break;
        case TokenKind::MATCH:
            kind = "MATCH";
            break;
        case TokenKind::WHILE:
            kind = "WHILE";
            break;
        case TokenKind::BREAK:
            kind = "BREAK";
            break;
        case TokenKind::MUT:
            kind = "MUT";
            break;
        case TokenKind::FN:
            kind = "FN";
            break;
        case TokenKind::RETURN:
            kind = "RETURN";
            break;
        case TokenKind::LAMBDA:
            kind = "LAMBDA";
            break;
        case TokenKind::FOR:
            kind = "FOR";
            break;
        case TokenKind::IN:
            kind = "IN";
            break;
        case TokenKind::AND:
            kind = "AND";
            break;
        case TokenKind::OR:
            kind = "OR";
            break;
        case TokenKind::NOT:
            kind = "NOT";
            break;
        case TokenKind::TRUE:
            kind = "TRUE";
            break;
        case TokenKind::FALSE:
            kind = "FALSE";
            break;
        case TokenKind::TRAIT:
            kind = "TRAIT";
            break;
        case TokenKind::CLASS:
            kind = "CLASS";
            break;
        case TokenKind::IMPL:
            kind = "IMPL";
            break;
        case TokenKind::ENUM:
            kind = "ENUM";
            break;
        case TokenKind::TYPE:
            kind = "TYPE";
            break;
    }
    if (value.empty()) {
        os << kind << " line " << line;
    } else {
        os << kind << " " << value << " line " << line;
    }
    return os;
}