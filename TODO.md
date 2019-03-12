# TODOs

*PRs are welcome.*

- block expressions
- break
- continue
- return
- subslice (`slice[l..r]`)
- `-=` etc.
- bit operators
- `&&`, `||`
- pipeline operator (`x |> f(y)` ---> `f(x, y)`)
- address-of (`&x`)
- type annotations (`fun(x: int): unit { .. }`)

## To Be Determined

- int/byte conversion
- int/ptr cast
- for expressions (to be determined)
    - `for x in l..r { }` where `l`, `r`: int
    - how to iterate over slices or vectors?
- recursive functions
- char literals
    - whether support non-utf8 Unicode or not
- Rust-style function/lambda syntax
- abstraction (trait, class or nothing?)
- rename of the language

## Heavy Topics

- slice of any type
- tuple
- vec (dynamic array)
- map (dynamic hash map)
- closure
- standard library
    - println, println_int, etc.
    - string manipulations
- vscode-ext syntax highlights
- docker
    - it is good if solver works with docker, without rust/node.js/etc.

## Low Priority Topics

- Underscores in integer literals
- Hex literals
- Better escape sequence processing in string literals
    - Currently only `\n` is supported.
- 64bit float
- Optimization
