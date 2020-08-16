# Regrammar

(Not implemented yet.)

Regrammar is a formal language to describe concrete syntax. Yet another variant of extended Backus-Naur Form (BNF) with flavor of regular expressions (regex).

## Comparison

- Compared to regex:
    - More verbose but better in readability and maintainability (I hope)
- Compared to (non-extended) BNF:
    - More rich in features
- Compared to extended BNF variants:
    - More intuitive syntax (in my opinion)

## Comments

Unquoted `#` to the next end of line is comment: just ignored.

## Escape sequences

Escape sequences work outside of comments and single-quotes.

- `\0`: U+0000
- `\t`: tab (U+0009)
- `\n`: LF (U+000a)
- `\r`: CR (U+000d)
- `\xHH`: Single byte (0xHH)
- ~`\u{HHHH}`: unicode~ TBD
- `\c`: c (for any ASCII sign c: one of `' " / \ ! ? + * ( ) [ ] { } < > . : ; , - @ # $ % & | = ~` or backtick \`)

## Unquoted characters

Call non-escaped characters outside of comments and quotes *unquoted*.
Unquoted spaces and newlines are just ignored, except for the sides of `:`.
Others (non-space ASCII control characters and non-ASCII characters) must not be unquoted.

## Single quotes

`'...'` matches the contents between the quotes.
Escape sequences are NOT expanded between the quotes and `\` just represents `\` itself.
Contents must NOT contain newline and U+0000.

## Double quotes

`"..."` matches the contents between the quotes.
Unlike single quotes, escape sequences are expanded.
Contents must NOT contain newline and U+0000.

## Character sets

`[x1 x2 ... xN]` matches one of `xi`s; order doesn't matter here. Item is an element or a range.

An element represents a byte:

- an ascii character such as `0`,
- an escape sequence such as `\xHH` or
- a quote such as `'0123456789'` (any byte in the quote).

For any element x and y, `x - y` is a range that matches a byte between `x` and `y`, inclusive.

E.g. `[\x00-\x20 \x7f]` matches an ASCII control character. (Spaces are unnecessary; `[\x00-\x20\x7f]` is same.)

`[^ xi... ]` is a negative set, which matches when `[ xi... ]` fails.

`.` is `[^ \0 \r \n]`, i.e. matches any byte except for newlines and U+0000.

## Grouping

`(x)` is a pattern that works same as pattern `x`. Unlike regex, `(x)` doesn't represent output. (See labels below.)

E.g. `('0' | '1')*`

## Sequences

`x y` is a *sequence* pattern, which matches `x` and then `y`.

## Alternative

`x | y` matches `x` or `y`. If both can match, (longer, leftmost) is preferred.

TODO: `x / y` for greedy?

## Repetition

These are repetitive patterns:

- `x?`: zero or one
    - This must NOT occur in the form of `+?` or `*?` for forward compatibility.
- `x+`: one or more
- `x*`: zero or more ([Kleene-star](https://en.wikipedia.org/wiki/Kleene_star))

TODO: `x{n, m}`, `x*?` etc.

## Labels

A labeled pattern represents an item of output.

- `n:x` is a pattern with label `n`. `n` is a quote or identifier. An identifier consists of ASCII alphabets, ASCII digits and '_'.
    - No space is allowed between `n`, `:` and `x`.
    - If `x` matches, the result is added to output, keyed by `n`.
- `:x` is a pattern with positional label.
    - No space is allowed between `:` and `x`.
    - If `x` matches, the result is added to output, not keyed.

Matching succeeded yet a labeled pattern didn't succeed, the associated key is not included to the output or filled by some default value such as `null`.

E.g. when a pattern `zero:0 | one:1` matches a string `1`, the output can be `{ one: "1" }` or `{ zero: null, one: "1" }` (JavaScript)

### Primitives

- `\A`: start of input. (Not start of lines.)
- `\z`: end of input. (Not end of lines.)
- `\b`: word boundary.
- `\d` = `[0-9]`: ascii digit.
- `\w` = `[0-9 A-Z a-z]`: ascii alphabet or digit.
- `\s` = `[\n \r ' ' \t]`: ascii whitespace.

### Definition and variables

`n = x` binds name `n` to pattern `x`. Mutual references are allowed.

`$n` is a substitution of variable, which is just replaced with the definition. E.g.

```
pattern:
    _ = \s*
    int = [0-9]*

    _ (:$int _)*

input:
    123 456

output:
    ["123", "456"]
```

Recursively-defined variables can't substitute.

`n` is an indirect occurrence of variable, which forms a node of syntax tree. Output is structured.

```
pattern:
    literal = value:([0-9]+)
    expr = <literal> | lhs:<expr> op:[\+ \-] rhs:<expr>
    expr

input:
    2-3+5

output:
    {
        lhs: {
            lhs: {
                value: "2",
            },
            op: "-",
            rhs: {
                value: "3",
            },
        },
        op: "+",
        rhs: {
            value: "5",
        },
    }
```
