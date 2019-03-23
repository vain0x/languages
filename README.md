# PICOMET-LANG

[![Build Status](https://travis-ci.org/vain0x/picomet-lang.svg?branch=master)](https://travis-ci.org/vain0x/picomet-lang)

*WIP: The project is still at early stage.*

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
- `slice[l..r]` makes a sub-slice tha spans over l to r elements (exclusive).

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
    let fact = |n| {
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
    let succ = |x| x + 1;
    println_int(succ(1)); //=> 2
```

- Functions are not closures for now, i.e. functions can't refer to local variables outside it. Global variables are available.

### Docs: Standard Input/Output

For convenience in competitive programming, standard input is considered a list of words separated by spaces or line breaks by default.

- `read_str()` reads a word from standard input and gets the string (slice of byte).
- `read_int()` does the same as `read_str` except it parses the word as integer.
- `print(x)` prints a slice of byte to standard output.
- `println_int(x)` prints an integer and a line break to standard output.

## Solve

Let's solve problems with picomet-lang!

### Solve: Set up

For now, you need to build picomet-lang compiler and VSCode extension. See the `Develop` section below.

### Solve: Write your solution

Open the `solver` directory with VSCode.

Write your code in [solver/src/main.picomet](solver/src/main.picomet).

- The `run` task runs your code.
- Copy-and-paste `solver/src/main.rs` to submit as `Rust` language *after executing run or gen task*.

## Develop

To develop this project or try the latest compiler, install tools:

- **Rust tools** from <https://www.rust-lang.org> for compiler/runtime
- **Node.js** from <https://nodejs.org> for vscode-ext
- **Yarn** from <https://yarnpkg.com> for vscode-ext
- **VSCode** from <https://code.visualstudio.com> for vscode-ext

Download (or git-clone) this repository.

### Dev: Build Things for Release

For bash users: run `./install` instead. To uninstall, run `./uninstall`.

- In `compiler` directory, run `cargo build --release`.
- Copy the generated compiler at `compiler/target/release/picomet` to `vscode-ext/out/picomet`.
- In `vscode-ext`, run there commands:
    ```sh
    yarn
    yarn build
    yarn vsce package --yarn -o picomet-lang.vsix
    ```
- Install generated `vscode-ext/picomet-lang.vsix` with VSCode's `Install from VSIX` command.
- To uninstall, uninstall the extension from VSCode and remove all files.

### Dev: Configure [Optional]

Further configurations for convenience.

In `runtime` directory, configure Rust tools to use specific version of Rust compiler to match AtCoder's version.

```
rustup install 1.15.1
rustup override set 1.15.1
```

Install `cargo-watch`, which is very helpful and required to run `compiler/dev`.

```sh
cargo install cargo-watch
```

### Dev: Test

To test the compiler, run `cargo test` in `compiler`. Tests are written in [compiler/tests/tests.rs](compiler/tests/tests.rs).

To test the vscode extension, open `compiler` or `vscode-ext` with VSCode and press F5 to launch new instance of VSCode with extension temporally installed.

## Internals

### Internals: Stages

- Tokenize
    - Source → Tokens
    - Hand-made tokenizer.
- Parse
    - Tokens → Syntax tree
    - Hand-made recursively descendant parser.
- Sema
    - Syntax tree → Semantic Model
    - Type checking, symbol resolution, etc.
- MIR
    - Syntax tree + Semantic Model → MIR instructions
    - The MIR is assembly for register machine with infinite registers.
- Optimize
    - *Not implemented yet.*
- Evaluate
    - Runtime evaluates the MIR instructions.

## Contribution

Pull requests are welcome.

- Feel free to communicate in Japanese as well as English.
- See [the TODO list](TODO.md) for tasks.
