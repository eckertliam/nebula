# Nebula

A simple programming language written in Rust. This is a learning project for me. I have experimented with multiple syntaxes and paradigms for Nebula. Currently,
I've settled into a simple type system with records, enums, and type aliases. The syntax resembles Julia or Pascal. I decided to move away from a bracket based syntax because the `end` based syntax seems to be more ergonomic and less common. 

## Syntax

### Functions

```
fn add(a: int, b: int) return int
    return a + b
end
```

```
fn main()
    println("Hello, World!")
end
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

### Classes
Classes are used to define objects with their own state and behavior. They support single inheritance through the `extends` keyword.

```
class Point
    x: int
    y: int

    Point(x: int, y: int)
        this.x = x
        this.y = y
    end

    fn add(other: Point) return Point
        return Point(x: this.x + other.x, y: this.y + other.y)
    end

    pub fn get_coords() return (int, int)
        return (this.x, this.y)
    end
end

let p: Point = Point(x: 10, y: 20)
```

Classes can also extend other classes:

```
class ColoredPoint extends Point
    color: string

    ColoredPoint(x: int, y: int, color: string)
        super.new(x, y)
        this.color = color
    end
end

let cp: ColoredPoint = ColoredPoint(x: 10, y: 20, color: "red")
```