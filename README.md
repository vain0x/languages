# X-BNF

**X-BNF** は BNF 風の構文定義言語の処理系です。

## 構文

X-BNF 言語の構文は以下の通りです。(少し簡略化したものになります。)

```ini
# LOUD: LOUD_CASE の識別子
# SNAKE: snake_case の識別子
# CHAR: '...'
# STR: "..."
atom_term = LOUD | SNAKE | CHAR | STR | "(" term ")"

suffix_term = atom_term (("?" | "*" | "+") atom_term)*

concat_term = suffix_term+

term = concat_term ("|" concat_term)*

rule_stmt = SNAKE "=" term

root = rule_stmt* EOF
```

完全版は [xbnf_lang.xbnf](./xbnf_boot/tests/xbnf_lang/xbnf_lang.xbnf) にあります。
