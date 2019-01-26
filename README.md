# PICOMET-LANG

*WIP: The project is at very early stage. PRs are welcome.*

**Picomet-lang** is a pico programming language. *Pico* means that the processor is written in single file.

The goal is to solve competitive programming problems, especially [AtCoder Beginners Selection](https://atcoder.jp/contests/abs/tasks).

## Example

The following is a solution written in picomet-lang to solve this problem: [ABC086A - Product](https://atcoder.jp/contests/abs/tasks/abc086_a?lang=en).

```js
(begin
    (let a (read_int))
    (let b (read_int))
    (println (cond (== (% (* a b) 2) 0) "Even" "Odd"))
)
```

This reads two integers from STDIN and print a word to STDOUT that indicates whether the product of two integers are even or odd.

Other examples are available at the bottom of `tests/tests.rs`.

## Stages

- Tokenize
    - Source → Tokens
    - Hand-made tokenizer.
- Parse
    - Tokens → Syntax tree
    - Hand-made recursively descendant parser. Thanks to S-expression syntax, it is very simple.
- Compile
    - Syntax tree → IR instructions
    - The IR is assembly for register machines with infinite registers.
- Optimize
    - *Not implemented yet.*
- Evaluate
    - Interpreter evaluates the IR instructions.

## Solve

To solve a problem with picomet-lang, write your code in `examples/solver.picomet`.

To run your code, use `./run`.

To submit your code, run `./build` and copy-and-paste `examples/solver.rs`.

## Develop

To develop this project, firstly:

- Install Rust
    - Follow the instructions written in [Rust programming language](https://www.rust-lang.org/)

Inside the project directory, run the following command.

```sh
rustup install 1.15.1
rustup override set 1.15.1
```

- Note: This command tells rust processor to use old version of Rust inside the directory. Picomet-lang is written in the version because AtCoder doesn't support newer version of Rust for now.
- Note: Due to the version, you tend to get more compile errors than the usual. The version doesn't support "match ergonomics" and "non-lexical-lifetime" features and some of standard library APIs.

To build, `cargo build`.

To run tests, `cargo test`. Tests are written in `tests/tests.rs`.
