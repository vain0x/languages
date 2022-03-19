# Linear

線形型を試す

## 抽象構文

```
    型 T :=
        int
        | unit
        | ( T )         カッコ
        | T * T         ペア
        | T -> T        関数型
        | __linear<T>   線形型 [^lin]

    パターン P :=
        ident           識別子
        | _             ワイルドカード
        | ( P )         カッコ
        | P, P          ペア
        | ident P       バリアント分解

    式 E :=
        number          数値リテラル
        | ( E )         カッコ式
        | { E }         ブロック
        | E ; E         順次
        | E E           関数適用
        | E, E          ペア
        | E + E         加算
        | E = E         等価性の比較
        | let P = E     束縛
        | if E then E   分岐
               else E
          end
        | assert E      表明
        | __acquire E   線形獲得
        | __dispose E   線形破棄

    宣言 D :=
        let ident params* : T = E   関数宣言
        | expect string { E }       成功テスト
        | expect_error string { E } 失敗テスト
```

### ブロック

letの右辺が複数の式になるときは `{}` で囲む

### 関数宣言

関数宣言の構文は F# と同様。ただし型注釈は必須

`let ident param1 param2 ... : result = E`

本体が1行のとき

```fs
let f1 (x: int) (y: int) : int = x + y
```

本体が複数行のとき

```fs
let f2 (x: int) (y: int) : int = {
    let xx = x * x
    let yy = y * y
    xx + yy
}
```

### expect宣言

`expect "タイトル" { 本体 }`

本体の式の評価が正常に完了することを表す

### expect_error宣言

`expect_error "タイトル" { 本体 }`

本体の式の型検査が通らないか、評価した結果がエラーになることを表す

### 自動セミコロン挿入

字句解析の後、行末が特定の種類のトークンである行の末尾に ';' を挿入する。対象となるトークンの種類は式の末尾になりうるもの:

- 数値、識別子、`end`, `)`, `}`

また、`()` の内部にはセミコロンを挿入しない

### 線形型のためのプリミティブ

- `__linear<'T>`
    - ビルトインの線形型。`'T` 型の値をラップしたもの
- `__acquire<'T>` : `'T -> __linear<'T>`
    - 任意の値をラップして `__linear` 型を作る
    - unsafe操作
- `__dispose<'T>` : `__linear<'T> -> 'T`
    - `__linear` 型の値を破棄して中身を取り出す
    - unsafe操作

## 余談

- 構文は F# をベースにしつつ、レイアウトルールを排除したかたちになっている
