cmake_minimum_required(VERSION 3.27)
project(nebula)

set(CMAKE_CXX_STANDARD 20)

add_executable(nebula src/main.cpp
        src/frontend/token.h
        src/frontend/token.cpp
        src/frontend/tokenizer.h
        src/frontend/tokenizer.cpp
        src/frontend/ast.cpp
        src/frontend/ast.h
        src/frontend/parser.cpp
        src/frontend/parser.h)
