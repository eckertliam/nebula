//
// Created by Liam Eckert on 7/24/24.
//

#ifndef NEBULA_AST_H
#define NEBULA_AST_H

#include <string>
#include <vector>
#include <optional>

/// The base class for all AST nodes
class AstNode {
public:
    uint8_t line;
    virtual ~AstNode() = default;
};


class Expression : public AstNode {
};

class Statement : public AstNode {
};

// BEGIN STATEMENTS====================================================================================================

/// a block of code created by an indent to dedent
class Block : public Statement {
public:
    std::vector<std::unique_ptr<Statement>> statements;
};

/// a function parameter
class FunctionParameter : public Expression {
public:
    std::string name;
    std::string type;
};

/// a function definition
class FunctionDef : public Statement {
public:
    std::string name;
    std::vector<std::unique_ptr<FunctionParameter>> parameters;
    std::string return_type;
    std::unique_ptr<Block> body;
};

/// a variable declaration
class VarDecl : public Statement {
public:
    bool is_mutable;
    std::string name;
    // optional type. If not specified, type is inferred. if cannot be inferred, error
    std::optional<std::string> type;
    std::unique_ptr<Expression> value;
};


/// Var mutation
class VarMut : public Statement {
public:
    std::string name;
    std::unique_ptr<Expression> value;
};

/// a return statement
class Return : public Statement {
public:
    std::unique_ptr<Expression> value;
};

/// if statement
class If : public Statement {
public:
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Block> then_block;
    std::unique_ptr<Block> else_block;
};

/// match case
class MatchCase : public Statement {
public:
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Block> block;
};

/// match statement
class Match : public Statement {
public:
    std::unique_ptr<Expression> value;
    std::vector<std::unique_ptr<MatchCase>> cases;
};

/// while loop
class While : public Statement {
public:
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Block> block;
};

/// for loop
class For : public Statement {
public:
    std::string name;
    std::unique_ptr<Expression> iterable;
    std::unique_ptr<Block> block;
};

/// lambda statement, just an anonymous function with similar syntax to function definition
class Lambda : public Statement {
public:
    std::vector<std::unique_ptr<FunctionParameter>> parameters;
    std::string return_type;
    std::unique_ptr<Block> body;
};

// END STATEMENTS======================================================================================================

// BEGIN EXPRESSIONS===================================================================================================

/// binary operation type
enum class BinOpKind {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    BIT_AND,
    BIT_OR,
    BIT_XOR,
    EQ,
    NEQ,
    LT,
    GT,
    LTE,
    GTE,
};

/// a binary operation
class BinaryOp : public Expression {
public:
    std::unique_ptr<Expression> left;
    std::unique_ptr<Expression> right;
    BinOpKind op;
};

/// any expression enclosed in parentheses
class Parenthesized : public Expression {
public:
    std::unique_ptr<Expression> expression;
};

/// a variable reference
class VarRef : public Expression {
public:
    std::string name;
};

/// a function call
class FunctionCall : public Expression {
public:
    std::string name;
    std::vector<std::unique_ptr<Expression>> arguments;
};

/// Number literal
class NumberLiteral : public Expression {
public:
    double value;
};

/// String literal
class StringLiteral : public Expression {
public:
    std::string value;
};

/// Boolean literal
class BooleanLiteral : public Expression {
public:
    bool value;
};

/// Unary operation type
enum class UnOpKind {
    NEG,
    NOT,
    BIT_NOT,
};

/// a unary operation
class UnaryOp : public Expression {
public:
    std::unique_ptr<Expression> operand;
    UnOpKind op;
};

/// Array literal
class ArrayLiteral : public Expression {
public:
    std::vector<std::unique_ptr<Expression>> elements;
};

/// Indexing operation
class Index : public Expression {
public:
    std::unique_ptr<Expression> array;
    std::unique_ptr<Expression> index;
};

/// a range expression
class Range : public Expression {
public:
    std::unique_ptr<Expression> start;
    std::unique_ptr<Expression> end;
};

// END EXPRESSIONS=====================================================================================================

#endif //NEBULA_AST_H
