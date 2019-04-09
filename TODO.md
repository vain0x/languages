# TODOs

*PRs are welcome.*

- `&=`, etc.
- pipeline operator (`x |> f(y)` ---> `f(x, y)`)
- address-of (`&x`)
    - slice of one element
- type annotations (`|x: int| -> unit { .. }`)
- `for`
    - `for x in l..r { body }` ---> `let x = l; while x < r { body; x += 1 }`
- block expressions
- block-local scope
- update README
    - let rec
    - as
    - lsp

## Heavy Topics

- slice of any type
- tuple
- vec (dynamic array)
- map (dynamic hash map)
- closure
- standard library
    - println, println_int, etc.
    - string manipulations
- docker
    - it is good if solver works with docker, without rust/node.js/etc.

## Low Priority Topics

- Underscores in integer literals
- Hex literals
- Better escape sequence processing in string literals
    - Currently only `\n` is supported.
- 64bit float
- Optimization

## To Be Determined

- int/slice cast
- for expressions to iterate over slices or vectors?
- abstraction (trait, class or nothing?)
- rename the language
