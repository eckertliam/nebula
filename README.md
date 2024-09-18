# Nebula

## 🚧 WIP 🚧

Nebula is a learning project I am working on.
Nebula is a programming language influenced by Rust and TypeScript.
It is a statically typed, compiled language, with a syntax similar to Rust.
Where Nebula differs from Rust is in its memory management model.
Nebula will use a runtime garbage collector.
This allows for a more flexible and expressive language without a borrow checker stepping in the way.

## Roadmap

This is a rough roadmap of the features I want to implement in Nebula.

- [ ] Give the AST full parser coverage:
    - [ ] Comments
    - [x] Arithmetic expressions
    - [x] Logical expressions
    - [x] Comparison expressions
    - [ ] Assignment expressions
    - [ ] Function calls
    - [ ] Struct literals
    - [ ] Enum literals
    - [ ] Block expressions
    - [ ] Control flow
    - [x] Function declarations
    - [x] Variable declarations (const and let)
    - [ ] Type declarations
    - [x] Struct declarations
    - [x] Enum declarations
    - [ ] Trait declarations
    - [ ] Impl blocks
    - [ ] Export statements
    - [ ] Import statements
- [ ] Implement NIR (Nebula Intermediate Representation)
- [ ] Implement transformation from AST to NIR
- [ ] Implement type checking
- [ ] Implement optimization passes
- [ ] Implement code generation
- [ ] Implement runtime garbage collector
- [ ] Implement runtime
- [ ] Implement standard library
- [ ] Implement build system
- [ ] Implement package manager?
