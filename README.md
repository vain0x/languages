# My Programming Languages

## Working On

### Milone Language

[milone-lang](https://github.com/vain0x/milone-lang)

- Self-hosting F#-subset compiler
- Written in: milone-lang (F#), 2018-2019

## Examples

### Curage Language

[curage-lang](https://github.com/vain0x/curage-lang)

- Example of Language Server Protocol server implementation for obvious language as first step
- Written in: TypeScript, 2018-2019
- Note on 2022: I want to write another one that has more features especially completion.

### Pattern Matching Exhaustivity Checking

[pattern-matching-exhaustivity-checking](https://github.com/vain0x/pattern-matching-exhaustivity-checking)

- Example of pattern matching exhaustivity checking based on spaces
- Written in: Rust, 2019

## Others

### Friends Language

[friends-lang](https://github.com/vain0x/friends-lang)

- "Programming language for friends in the Japaripark"
- Logical programming language with Japanese animation-reference joke syntax
- Written in: F#, 2017

### Jacco Language

[jacco-lang](https://github.com/vain0x/jacco-lang/tree/develop)

- Rust-ish syntax, C-ish semantics, language server protocol (LSP) server
- Written in Rust, 2020-incomplete
- Note on 2022: C with generics seems good (excluding trait bounds and monomorphization.) Zig and Hare seem going in the area.

### Confwell

[confwell](./confwell)

- Data notation for configuration
- Written in 2020. Specification only, not implemented.

### X-BNF Language

[xbnf-lang](./xbnf-lang)

- Extended BNF processor using regex-like syntax
- Written in: F#, 2019-incomplete

### Zoaride Language

[zoaride-lang](https://github.com/vain0x/zoaride-lang)

- (Would be) Example of IDE-oriented programming language
- Written in: TypeScript, 2019-incomplete

### Altery Language

[altery-lang](./altery-lang)

- (Would be) Yet another dynamic language
- Written in: Rust, 2019-incomplete

### Picomet Language

[picomet-lang](./picomet-lang)

- Statically typed, C-ish language
- Written in: Rust, 2019-incomplete

### Negi Language

[negi-lang](https://github.com/vain0x/negi-lang)

- Etude
- JavaScript-like programming language
- Written in: HSP3 (a script language popular in Japan), 2019

### \~Klac Language

- No longer available. Horrible implementation and terribly poor performance.
- Script language inspired with JavaScript and Ruby
- Written in: HSP3, 2008-2015

----
----

## Random Idea

- Logical programming language with C-ish syntax should exist
- Purely functional dynamic language could be good
    - Chance for safe abstraction without complicated type system
    - (puqqing is incomplete trial of this)
- C with generics seem good (see [#Jacco](#jacco-language))
    - Monomorphization should be avoided for compile time. Longer compile time is bad for developer motivation. Instead, allow generic types appear only through a pointer and implicitly pass information of types.
    - Trait bounds should be avoided for readability. Trait-bounded generic functions tends to have complicated signature, which makes more difficult for users to read types as documentation. Instead, pass function objects around.
- TypeScript happens to have good characteristics
    - Type checking and code generation can run *in parallel*, which is good for productivity
        - Type checking is typically slow and waiting for it after every code modification is a source of frustration. When type checking can be skipped and non-optimizing code generation is fast, iteration of debugging becomes faster.
    - Unsound type systems are still useful for abstraction
        - `any` is useful for lightweight meta-programming
        - Note that languages including C FFI are unsound
- Concrete syntax:
    - Go's auto semicolon insertion (ASI) mechanism is good. It's helpful yet not error-prone.
    - Avoid `<>` syntax for generics. It makes parsing much harder. See ['Why does Go use square brackets for type parameter lists?' in Go lang FAQ](https://go.dev/doc/faq#generic_brackets).
    - Avoid arrow function syntax (`() => E`) for closures. It makes parsing much harder. PHP 7's arrow function syntax (`fn() => E`) is good. See [PHP: Arrow Functions - Manual](https://www.php.net/manual/en/functions.arrow.php).
- Tool to check if a language server protocol (LSP) server is conformant to the specification should exist
    - One for debug adapter protocol (DAP) too

## Notes

- [2020 compiling-with-continuations](https://github.com/vain0x/playground/tree/main/2020-01-04-compiling-with-continuations)
- [2020 raii-lang](https://github.com/vain0x/playground/tree/main/2020-01-01-raii-lang)
- [2019 algebraic-effects](https://github.com/vain0x/playground/tree/main/2019-11-05-algebraic-effects)
- [2019 ide-by-wasm](https://github.com/vain0x/playground/tree/main/2019-11-02-ide-by-wasm)
- [2019 clog](https://github.com/vain0x/playground/tree/main/2019-10-29-clog-lang)
- [2019 brorog](https://github.com/vain0x/playground/tree/main/2019-10-29-brorog)
- [2019 parse-study](https://github.com/vain0x/playground/tree/main/2019-08-31-parse-study)
- [2019 event-based parser](https://github.com/vain0x/playground/tree/main/2019-05-04-event-based-parser)
- [2018 puqqing-lang](https://github.com/vain0x/playground/tree/main/2018-07-05-puqqing-lang)
- [2017 Bracky-lang](https://github.com/vain0x/playground/tree/main/2017-01-24-bracky-lang)
- [2015 TaPL in F#](https://github.com/vain0x/playground/tree/main/2015-09-29-tapl-fs)
- [2015 "Write Yourself a Scheme in 48 Hours"](https://github.com/vain0x/playground/tree/main/2015-09-08-scheme-in-48h)
- [2015 KlacFs](https://github.com/vain0x/playground/tree/main/2015-08-22-klac-fs)
