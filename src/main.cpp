#include <iostream>
#include <fstream>
#include "frontend/tokenizer.h"
#include "frontend/parser.h"


int main() {
    // read in source code from ../examples/hello_world.neb
    std::ifstream file("../examples/type_sys_examples.neb");
    std::string source((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    Tokenizer tokenizer(source);
    std::vector<Token> tokens = tokenizer.tokenize();
    Parser parser(tokens);
    return 0;
}

// th