# Nebula

A simple programming language written in Rust. This is a learning project for me. I have experimented with multiple syntaxes and paradigms for Nebula. Currently,
I've settled into a simple type system with records, enums, and type aliases. The syntax resembles Julia or Pascal. I decided to move away from a bracket based syntax because the `end` based syntax seems to be more ergonomic and less common. 

## Syntax

### Functions

```
fn add(a: int, b: int) return int
    return a + b
end fn
```

```
fn main
    println("Hello, World!")
end fn
```

### Variables
Variables can be typed or the type can be inferred. They can be constants or mutable.
#### Constants

```
const pi = 3.14159
const name: string = "Alan Turing"
```

#### Mutable Variables

```
let x = 10
x = 20
let y: int = 30
y = y + x
```

### Records
Records are similar to C or Rust structs. They are used to group related data together.

```
record Point 
    x: int
    y: int
end record

let p: Point = Point(x: 10, y: 20)
```

### Enums
Enums are similar to C or Rust enums. They are used to define a set of named constants. They are also used to create algebraic data types one of my favorite features of Rust and Haskell.

```
enum Color
    Red
    Green
    Blue
end enum
```

```
enum Shape
    Circle(radius: float)
    Rectangle(width: float, height: float)
end enum
```