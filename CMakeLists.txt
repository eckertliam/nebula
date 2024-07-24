cmake_minimum_required(VERSION 3.27)
project(nebula)

set(CMAKE_CXX_STANDARD 20)

add_executable(nebula src/main.cpp
        src/token.h
        src/token.cpp
        src/tokenizer.h
        src/tokenizer.cpp
        src/ast.cpp
        src/ast.h)
