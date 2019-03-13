# TODOs

*PRs are welcome.*

- subslice (`slice[l..r]`)
- `-=` etc.
- bit operators
- `&&`, `||`
- pipeline operator (`x |> f(y)` ---> `f(x, y)`)
- address-of (`&x`)
    - slice of one element
- char literals
    - `'a'` : byte type
- lambda syntax
    - `fun() x` → `|| x` or `| | x`
    - `fun(x, y) x + y` → `|x, y| x + y`
- type annotations (`|x: int| -> unit { .. }`)
- numeric cast
    - `as int`
    - `as byte`
- `for`
    - `for x in l..r { body }` ---> `let x = l; while x < r { body; x += 1 }`
- `let rec`
    - `let rec` is same as `let` except that the declaration is hoisted to the top of the scope
    - `let rec f = fun() g(); let rec g = fun() f()` ---> `let f; let g; f = fun() g(); g = fun() f()`
- block expressions

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

## To Be Determined

- int/slice cast
- for expressions to iterate over slices or vectors?
- abstraction (trait, class or nothing?)
- rename the language
