**Brainzen** is a programming language that compiles to Brainfuck. It has a C-like syntax and allows you to perform arithmetic and manipulate basic objects like numbers, strings and arrays.

# Brainfuck

Brainfuck is an extremely basic programming language created by Urban Müller in 1993. A Brainfuck program consists of a string of characters each representing a command (or a comment). For the list of available commands, see [the Wikipedia article on Brainfuck](https://en.wikipedia.org/wiki/Brainfuck#Commands). Unlike most languages, there is no official specification for Brainfuck. This means behavior may vary slightly between interpreters. Brainzen uses the original interpreter as the definition of Brainfuck, with the exception that the array is expected to be infinitely expandable to the right (which allows for [Turing-completeness](https://en.wikipedia.org/wiki/Turing_completeness)), although it is not required that the pointer is able to access cells to the left of its starting position. It is also important to note (although this is the behavior of the original interpreter) that numbers are expected to be 8-bit integers, and overflows should result in a wrap around.

# Brainzen

## Specification

Brainzen does not have an official specification yet. You can consider the compiler as the definition of the language (bugs aside). However, the language is currently under heavy development, so everything is bound to change at any time (including syntax and major features).

## Features

Here is a non-exhaustive list of major available features:

- Subroutines.
- Loops.
- Conditional statements.
- Types (characters, static arrays and [product types](https://en.wikipedia.org/wiki/Product_type)).
- Namespaces.

## Considered features

Here is a list of considered features in arbitrary order (this list is *not* commitment, merely an indication of where the language is heading):

- Type inference.
- Some form of [overloading](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism).
- Separating a program in multiple files (import statements).
- Ternary operator.
- Python-like array comprehensions.
- Python-like chained comparison (granted it preserves transitivity).
- [String interpolation](https://en.wikipedia.org/wiki/String_interpolation).
- Some form of named tuples (records or interfaces, depending on whether duck typing is used).
- Generating more "readable" files (relevant comments).
- In the long run, bigger integers (32-bit integers).
- In the very long run, floating point numbers ([IEEE 754](https://en.wikipedia.org/wiki/IEEE_754)).

There are also some features that will likely never be added to the language, because they are too complicated to implement efficiently. Such features include:

- Referencing subroutines before their respective declaration (including recursion).
- Early exits (including exceptions and exception handling).
- Dynamic arrays (including arrays of arbitrary size).
- Any form of interaction with the operating system (due to the limitations related to the nature of Brainfuck).

## Code examples

This section contains examples of basic programs and subroutines written in Brainzen.

### Hello world

```brainzen
proc main() {
    println("Hello, World!");
}
```

### Print numbers in base 10

The following procedure prints the passed number in base 10.

```brainzen
proc print10(char n) {
    let char d2 = n / 100;
    let char d1 = (n / 10) % 10;
    let char d0 = n % 10;
    if (d2) print('0' + d2);
    if (d1 || d2) print('0' + d1);
    print('0' + d0);
}
```

### Truth machine

```braiznen
proc main() {
    let char i = input();
    if (i == '0') {
        print('0');
    } else {
        while (1) print(i);
    }
}
```

### Fibonacci

The following function computes the `n`th number of the [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_number).

```brainzen
func fibonacci(char n) -> char {
    let char a = 0;
    let char b = 1;
    loop (n) {
        let char c = a + b;
        a = b;
        b = c;
    }
    return b;
}
```

### Fizz Buzz

```brainzen
proc fizzbuzz(char n) {
    let char i = 1;
    loop (n) {
        if (i % 3 == 0 && i % 5 == 0) {
            println("FizzBuzz");
        } else if (i % 3 == 0) {
            println("Fizz");
        } else if (i % 5 == 0) {
            println("Buzz");
        } else {
            print10(i);
            println();
        }
        i++;
    }
}
```

# Contributing

This is a personal project I want to lead myself. As such, I will likely not be accepting pull requests. But feel free to clone the project and fork it if you wish.

## Project structure

The current project structure is far from optimal, but structuring a Python project is a real pain. So it's not the priority at the moment to have a better project structure.

# License

This project is not yet licensed. The general guideline is to always link to this GitHub repo if you use it somewhere (although I have trouble understanding why anyone would do that).
