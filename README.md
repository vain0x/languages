# PICOMET-LANG

[![Build Status](https://travis-ci.org/vain0x/picomet-lang.svg?branch=master)](https://travis-ci.org/vain0x/picomet-lang)

*WIP: The project is at very early stage. PRs are welcome.*

**Picomet-lang** is a programming language. The goal is to solve competitive programming problems, especially [AtCoder Beginners Selection](https://atcoder.jp/contests/abs/tasks).

## Example

The following code is a solution written in picomet-lang to solve this problem: [ABC086A - Product](https://atcoder.jp/contests/abs/tasks/abc086_a?lang=en).

```rust
    let a = read_int();
    let b = read_int();
    print(if a * b % 2 == 0 { "Even\n" } else { "Odd\n" });
```

This reads two integers from standard input and prints a word to standard output that indicates whether the product of two integers is even or odd.

Other examples are available at the bottom of [compiler/tests/tests.rs](compiler/tests/tests.rs).

## Documents: How to Write Picomet-lang

- Before read, please check last-modified date because I might forget to update the document after changes.
- See [compiler/tests/tests.rs](compiler/tests/tests.rs) for actually working picomet-lang codes.

### Docs: Language properties

- Rust-like syntax
- C-like semantics (*Not determined yet*)
    - Imperative
    - Strict evaluation
    - No garbage collection
    - Use of pointers
- Single-threaded
- Virtual register machine

### Docs: Common features

These features are common in other programming languages.

- Integers: `0`, `42`, etc.
- Strings: `"Hello!\n"`, etc.
- Operators: `+`, `=`, etc.
- Comments: From `//` to end of line.

### Docs: Types

There are five types in picomet-lang for now.

- Integer (64 bit signed)
- Byte (8 bit unsigned)
- Slice of byte (`&[u8]` in Rust)
- Unit (`()` in Rust)
- Function (not first-class)

Static type of expressions are inferred with naive unification-based inference algorithm.

### Docs: Slices

Slices are similar to arrays in C. They are represented as pairs of pointer to represent a range of memory.

For now, you can use only slices of byte.

- String literals are slices of byte filled with ASCII codes.
- `mem_alloc(size)` allocates a chunk of memory dynamically. (Similar to `malloc` in C.)
- `slice[i]` gets the value of i'th element.
- `slice[i] = value` sets a value to the i'th element.

### Docs: Variables

Variables hold any value except for functions.

- `let x = a;` defines new local variable named `x` with initial value `a`.
- `x = b;` changes the value of variable `x` to `b`.

You can also use `+=` that stands for `x = x + a`.

### Docs: If

If expressions look like `if condition { body } else { alt }`.

Note that you sometimes need to wrap `if` expressions around parenthesis to combine it with binary/prefix/suffix operations. For example,

```rust
    1 + (if p { b } else { a })
```

The same goes for `while` and `fun`.

### Docs: Loops

While expressions look like `while condition { body }`. The result is unit.

### Docs: Functions

You can define functions like this:

```rust
    let fact = fun(n) {
        let x = 1;
        while n >= 2 {
            x = x * n;
            n = n - 1;
        }
        x
    };
    println_int(fact(3)); //=> 6
```

When the function body is just an expression, you don't need wrap it with braces:

```rust
    let succ = fun(x) x + 1;
    println_int(succ(1)); //=> 2
```

- Functions are not closures for now, i.e. functions can't refer to local variables outside it. Global variables are available.

### Docs: Standard Input/Output

For convenience in competitive programming, standard input is considered a list of words separated by spaces or line breaks by default.

- `read_str()` reads a word from standard input and gets the string (slice of byte).
- `read_int()` does the same as `read_str` except it parses the word as integer.
- `print(x)` prints a slice of byte to standard output.
- `println_int(x)` prints an integer and a line break to standard output.

## Stages

- Tokenize
    - Source → Tokens
    - Hand-made tokenizer.
- Parse
    - Tokens → Syntax tree
    - Hand-made recursively descendant parser.
- Compile
    - Syntax tree → IR instructions
    - The IR is assembly for register machine with infinite registers.
- Optimize
    - *Not implemented yet.*
- Evaluate
    - Interpreter evaluates the IR instructions.

## Solve

To solve a problem using picomet-lang, write your code in [solver/src/main.picomet](solver/src/main.picomet).

In order to run, you have two options:

1. Using Rust tools
2. Using Docker

### Solve: Use Rust tools

See the `Develop` section below for setting up.

Work inside the [solver](solver) directory.

To run your code, use `cargo run`.

To submit your code, run `cargo build` and copy from `solver/src/main.rs`.

### Solve: Use Docker

- Install Docker.
- Build a docker image with `./docker-build`.

You can run your code with `./docker-run` inside the [solver](solver) directory.

## Develop

To develop this project, first install Rust.

- To install Rust tools, follow the instructions written in [Rust programming language](https://www.rust-lang.org/)

Second, in both `compiler` and `runtime` directories, run `./setup` for configure your environment.

To build, run `cargo build` inside `compiler` or `runtime` directory.

To run tests, run `cargo test` in `compiler` directory. Tests are written in [compiler/tests/tests.rs](compiler/tests/tests.rs).
