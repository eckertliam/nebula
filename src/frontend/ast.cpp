//
// Created by Liam Eckert on 7/24/24.
//

#include "ast.h"

/// Block Statement
void Block::push(const Statement& statement) {
    statements.push_back(std::make_unique<Statement>(statement));
}