# PICOMET-LANG

*WIP: The project is at very early stage. PRs are welcome.*

**Picomet-lang** is a pico programming language. *Pico* means that the processor is written in single file.

The goal is to solve competitive programming problems, especially [AtCoder Beginners Selection](https://atcoder.jp/contests/abs/tasks).

## Example

The following code is a solution written in picomet-lang to solve this problem: [ABC086A - Product](https://atcoder.jp/contests/abs/tasks/abc086_a?lang=en).

```js
(begin
    (let a (read_int))
    (let b (read_int))
    (println (cond (== (% (* a b) 2) 0) "Even" "Odd"))
)
```

This reads two integers from STDIN and prints a word to STDOUT that indicates whether the product of two integers is even or odd.

Other examples are available at the bottom of [picomet/tests/tests.rs](picomet/tests/tests.rs).

## Document: How to Write Picomet-lang

- Before read, please check last-modified date because I might forget to update the document after changes.
- See [picomet/tests/tests.rs](picomet/tests/tests.rs) for actually working picomet-lang codes.

### Doc: Language properties

Picomet-lang is more similar to imperative languages than LISP except for syntax.

- LISP-like grammar
- Imperative
- Strict evaluation
- No memory management
- Single-threaded
- *Type systems are to be determined. No type checks for now!*

### Doc: Block expression

The whole picomet-lang program is an expression.

It tends to start with `begin`. `(begin x y .. z)` evaluates arguments in left-to-right order. The result is the final argument.

Note that `begin` is similar to block expressions in Rust. `(begin x y z)` is `{ x; y; z }` in Rust.

### Doc: Common features

These features are common in other programming languages.

- Numbers: `0`, `42`, etc. (Negative numbers are not supported yet.)
- Strings: `"Hello"`, etc. (Escape sequences are not supported yet.)
- Comments: From `//` to end of line. (Multi-line comments will not supported.)

### Doc: Identifiers

*Identifiers* are name of variables, functions or keywords. You can use almost all ASCII symbols including `+-*/%=<>!?` etc. for names.

For example, `+` is just a name of function rather than operator. There does not exist operators in picomet-lang.

### Doc: Numeric operations

Instead of operators, use functions such as `+`, `-`, `*`, `/`, `%` for calculation. For example, `(+ 2 3)` means `2 + 3` (= 5).

These arithmetic operators *fold* arguments. E.g. `(+ 2 3 4)` works the same as `((+ 2 3) 4)`, i.e. `(2 + 3) + 4`. In the same way, `(- 7 2 3)` is `((7 - 2) - 3)` (= 2).

Comparison functions such as `==`, `!=`, `<`, `>=`,`<=`, `>=` don't fold arguments. Pass exactly two arguments like this: `(== x 0)`.

### Docs: String operations

*Note that string manipulations are still in development. It will change completely.*

- `(++ x y)` concatenates strings.
- `(to_str x)` converts an integer `x` to string (decimal representation).

### Docs: Standard Input/Output

For convenience in competitive programming, standard input is considered a list of words separated by spaces or line breaks by default.

- `(read_str)` reads a word from STDIN and gets the string.
- `(read_int)` does the same except it parses the word as integer.
- `(print x y..)` prints strings to STDOUT.
    - Arguments must be strings.
- `(println x y ..)` prints strings to STDOUT.
    - Different from `print`, it also writes a line break after all arguments and flushes.
    - Arguments must be strings.

### Doc: Variables

Variables hold any kind of value (for now).

- `(let x a)` defines new variable named `x` with initial value `a`.
- `(set x b)` changes the content of variable `x` to `b`.

### Doc: Conditional branches

- `(cond true x y)` equals to `x`.
- `(cond false x y)` equals to `y`.

You can omit "else" part (`y`) if unnecessary.

### Docs: Loops

- `(while b x)` repeatedly tests `b` is true or not and if it's true, evaluates `x`.
- Once `b` is evaluated to false, the loop ends.

### Docs: Others

Other too detailed things.

- `cond` can take any number of condition-then part in arguments for else-if syntax. `(cond b1 x1 b2 x2 y)` is equivalent to `(cond b1 x1 (cond b2 x2 y))`.
- The result of `(cond false x)` is indeterminate value.
- The result of `while` is indeterminate value.

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

To solve a problem with picomet-lang, write your code in [picomet/examples/solver.picomet](picomet/examples/solver.picomet).

To run your code, use `./run`.

To submit your code, run `./build` and copy-and-paste `picomet/examples/solver.rs`.

## Develop

To develop this project, first install Rust.

- To install Rust tools, follow the instructions written in [Rust programming language](https://www.rust-lang.org/)

Second, inside the `picomet` directory, run the following command.

```sh
rustup install 1.15.1
rustup override set 1.15.1
```

- Note: This command tells rust processor to use old version of Rust only for the directory. Picomet-lang is written in the version because AtCoder doesn't support newer version of Rust for now.
- Note: Due to the version, you tend to get more compile errors than the usual. The version doesn't support "match ergonomics" and "non-lexical-lifetime" features and some of standard library APIs.

To build, run `cargo build`.

To run tests, run `cargo test`. Tests are written in [picomet/tests/tests.rs](picomet/tests/tests.rs).
