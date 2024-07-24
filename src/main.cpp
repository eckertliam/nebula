#include <iostream>
#include <fstream>
#include "tokenizer.h"


int main() {
    // read in source code from ../examples/hello_world.neb
    std::ifstream file("../examples/hello_world.neb");
    std::string source((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    Tokenizer tokenizer(source);
    std::vector<Token> tokens = tokenizer.tokenize();
    for (Token& token : tokens) {
        std::cout << token << std::endl;
    }
    return 0;
}
