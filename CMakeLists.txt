cmake_minimum_required(VERSION 3.27)
project(nebula)

set(CMAKE_CXX_STANDARD 20)

include_directories(include)

add_executable(nebula src/main.cpp
        include/frontend/token.hpp
        src/frontend/token.cpp
        include/frontend/tokenizer.hpp
        src/frontend/tokenizer.cpp
        src/frontend/ast.cpp
        include/frontend/ast.hpp
        src/frontend/parser.cpp
        include/frontend/parser.hpp
        include/frontend/lexeme.hpp
        src/frontend/lexeme.cpp)
