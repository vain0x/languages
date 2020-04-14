# メモ

記述は誤っているかもしれないし、古いかもしれない。

## CPS 変換

CPS 変換の方法。

関連記事:

- [コンパイラの作り方 (詳解)](https://www.is.s.u-tokyo.ac.jp/vu/96/cad/compilerresume/)
- [RustでCPS変換が簡単になったよという話 | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/12/07/rustdecpshenkangakantanninattayotoiuhanashi/)

### CPS ノード

凡例

```
    (ノードの種類 [引数列...] [結果列...] [継続列...])
```

x, y を加算して、結果を z に束縛し、継続 n を行う (+) 命令のノード:

```
    (+ [x y] [z] [n])
```

x, y が等しければ継続 eq, そうでなければ継続 ne を行う (=) 命令のノード:

```
    (= [x y] [] [eq ne])
```

### 例1: 加算のみ

```rust
fn f() {
    40 + 2
}
```

スタックマシン用のコードに変換する。 `+/2:1` の 2, 1 はそれぞれ引数・継続の個数。

```
push 40
push 2
prim +/2:1
jump/1
```

はじめ、スタックは空とする。上から順番に処理していく。
push 命令は term をスタックに積む。

prim +/2:1 の時点で、スタックは次の状態になっている。

```
    term:40, term:2
```

+/2:1 は2つの引数を取るので2つの term (40, 2) をポップする。
1つの結果を生成するので、フレッシュな変数を1つ生成する。仮に x とする。
1つの継続が必要なので、再帰的に残りの部分を処理することにより、1つの継続を得る。

再帰的に処理する場合、スタックに結果を積まれた状態で処理する?

```
    term:x
```

次に jump/1 を処理する。1個の引数をポップして、CPS ノード (jump [x]) を生成する。

再帰から +/2:1 の処理に戻ってくる。継続を得たので、CPS ノードを生成できる。

```
(+ [40 2] [x] [
    (jump [x])
])
```

### 例2: 入れ子の式

```rust
fn f() {
    (2 + 3) * 4
}
```

スタックマシン用のコードに変換する。

```
push 2
push 3
prim +/2:1
push 4
prim */2:1
jump/1
```

+/2:1 の処理中に再帰が起こる。

*/2:1 の時点でスタックは次の状態:

```
    term:x, term:4
```

\*/2:1 の継続は (jump [y]) になる。
(\*) の CPS ノードは k=`(* [x 4] [y] [(jump y)])` である。

+/2:1 に戻ってくる。

```
(+ [2 3] [x] [
    (* [x 4] [y] [
        (jump y)
    ])
])
```

### 例3: 分岐

```rust
fn f() {
    if a > 0 {
        1
    } else {
        0
    }
}
```

スタックマシン用のコードでは、通常 if は goto を使うが、ここでは継続を2つ持つプリミティブ if/1:2 を使う。if の後の計算は fix で関数として定める。

```
    push a
    push 0
    prim >/2:1
    prim if/1:2

    // then
    push 1
    jump(endif)/1

    // else
    push 0
    jump(endif)/1

fix endif/1:
    jump(return)/1
```

(>) がスタックから a, 0 をポップし、継続を作りはじめる。

次に、(if) が (>) の結果である p をポップして、継続を作りはじめる。
if は2つの継続を持つので、残りの部分から継続を構築する再帰処理も2回行う。

fix は最上位に持ち上げればよい。

```
    (fix [
        (label endif [x] [
            (jump(return) [x])
        ])
    ] [
        (> [a 0] [p] [
            (if [p] [] [
                // then
                (jump(endif) [1])
                // else
                (jump(endif) [0])
            ])
        ])
    ])
```