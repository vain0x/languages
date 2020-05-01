# Confwell

WIP

**Confwell** ~~is~~ (would be) an unambiguously specified object notation for configuration files.

Partly inspired with [StrictYAML](https://hitchdev.com/strictyaml).

## Cheatsheet

```sh
# Comment lines start with '#'.

# Key-value pair.
key = value

# Quoted keys and values.
four_spaces = '    '
' ' = space

# Escape sequences in "...".
cr_lf_tab_del = "\r\n\t\x7f"

# Here documents for multiline strings.
text = <<EOF
first line
    second line (indented by 4 spaces)
last line
EOF
```

## Features

- Untyped values for least surprise and simplicity of specification
    - Remember `Null` (name) `NO` (Norway) `1.0` (version number)
- Flat structure for automation and simplicity of specification
    - Just doing `echo key=value >>app.confwell` work.
- Unambiguous specification
    - Don't worry slightly different behaviors of implementations.
    - (TODO: See below. Works not done yet.)
- Here documents
    - (TODO: Should be removed?)

## Implementations

Not available yet.

## Specification

(Not unambiguous yet.)

- Extension: `.confwell`
- Encoding: UTF-8 (without BOM)
- Spaces:
    - ` ` (U+0020)
    - `\t` (U+0009)
- End of line:
    - `\n` (U+000a)
    - `\r\n` (U+000d, U+000a)

### Syntax

```ini
# End of line.
EOL = /\r?\n/

# ASCII whitespace.
SPACE = /[ \t]/

# ASCII hex digit.
HEX = /[0-9A-Fa-f]/

# Normal character (code unit).
CHAR = /[^ \t\r\n='"\\#]/
# '

IDENTIFIER = /[0-9A-Za-z_-]+/

# Can contain any chars but EOLs.
comment-row = SPACE* '#' (SPACE | CHAR | '='| "'" | '"' | "\\" | '#')*

escape-sequence =
    | '\''
    | '\"'
    | '\\'
    | '\0'
    | '\n'
    | '\r'
    | '\t'
    | '\u' HEX HEX HEX HEX
    | '\x' HEX HEX

# Can contain any chars but single quotes and EOLs.
single-quoted = "'" (SPACE | CHAR | '=' | '"' | "\\" | '#')* "'"

# Can contain any chars but double quotes, unescaped backslashes and EOLs.
double-quoted = '"' (escape-sequence | SPACE | CHAR | '=' | "'" | '#')* '"'

# IDENTIFIERs must be same.
# Escape sequences are expanded unless `<<'IDENT'` is used.
here-document =
    '<<' (IDENTIFIER | "'" IDENTIFIER "'" | '"' IDENTIFIER '"') EOL
    (EOL | SPACE | CHAR | '=' | "'" | '"' | "\\" | '#')*
    EOL IDENTIFIER

# Can contain any chars but equals, hashes and EOLs.
# Note that equals are disallowed.
bare-key = CHAR (SPACE | CHAR | "'" | '"' | "\\")* (CHAR | "'" | '"' | "\\")

# Can contain any chars but EOLs.
# Note meta characters are allowed.
bare-value = CHAR (SPACE | CHAR | '=' | "'" | '"' | "\\" | '#')* (CHAR | '=' | "'" | '"' | "\\" | '#')

key =
    | bare-key
    | single-quoted
    | double-quoted

value =
    | bare-value
    | single-quoted
    | double-quoted
    | here-document

entry-row = SPACE* key SPACE* '=' SPACE* value SPACE*

row =
    | SPACE*
    | comment-row
    | entry-row

root = (row (EOL row)*)?
```

### Semantics

- Duplicated keys
    - Error
- Missing keys (`= value`)
    - Error
- Missing values (`key =`)
    - Value is an empty string. Same as `key = ''`.

## Design choices

- Why separator is `=`?
    - As `=` is less likely used in key names, compared to `:`.
- Why comments start with `#`s?
    - Consistent with .conf and `#!` (shebang).
- Why `#`s are allowed in bare-value?
    - Value could be `https://example.com/#fragment`.
