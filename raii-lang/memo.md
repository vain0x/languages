# メモ

(未定)

## コンセプト

C言語 with RAII + ジェネリクス + 安全

- データをリソースとみなす
- アルゴリズムを明示する
- 安全性を保つ

やらないこと:

- ゼロコスト抽象化
- aliasable XOR mutable (参考: [Notes on a smaller Rust](https://boats.gitlab.io/blog/post/notes-on-a-smaller-rust/))
- 参照型 (`&T`/`&mut T`)
- 関数型プログラミング
- マルチスレッド

参考にする言語:

- C
- C++ (デストラクタなど)
- C# (ref パラメータなど)
- Rust (構文や型システムなど)
- TypeScript (データ型と型システムの分離など)

## 機能

### 変数のモード

変数はモードを持つ。

- 参照性: オブジェクトを参照(ポインタ)越しに持つかどうか指定する。
    - 利点: 変数が共有されうるか分かりやすくする
- 可変性: 不変な変数を通しての変更操作はエラーにする。
    - 利点: 変数が変更の対象であるか分かりやすくする

```rust
// 文字列が空か判定する。
// in を指定された引数は読み取り専用の参照で渡される。
fn is_empty(in s: String) -> bool {
    // 変更操作はコンパイルエラーになる。
    // s = "";

    s.len() == 0
}

// 文字列の末尾に '!' を付加する。
// ref を指定された引数は参照で渡される。
// mut を指定された引数・変数には変更操作ができる。
fn bang(ref mut s: String) -> bool {
    s.push('!');
}

{
    let s = "hello";

    // s を読み取り専用の参照 (in) で渡す。
    is_empty(s);

    // 変数を変更可能な参照で渡すときはキーワードが必須。
    bang(ref mut s);
}
```

### インターフェイス

メモリ上での表現を決定する型 (データ型) と、静的検査としての型 (インターフェイス) を分離する。

```rust
// 構文解析の結果を持つデータ型。
struct ParseResult {
    success: bool
    union {
        tokens: Vec[Token]
        errors: Vec[String]
    }
} as {
    // 構文解析に成功したときのインターフェイス
    Success {
        // success が true であることを示す。
        success @ true

        // tokens が少なくとも1つの要素を持つベクタであることを示す。
        tokens @ [_, ..]
    }

    // 失敗したとき
    Failure {
        success @ false
        errors @ [_, ..]
    }
}

fn parse(in text: String) -> ParseResult {
    // 構文解析した結果を tokens, errors に入れる。
    let (tokens, errors) = ...;

    match errors {
        // エラーがなければ成功。
        [] => {
            // 動的検査
            assert(tokens @ [_, ..]);

            return ParseResult::Success {
                success: true

                // 上の assert のおかげで Success インターフェイスが要求する
                // tokens は空でないという条件に合致することが分かる。
                // (そうでなければコンパイルエラーになる。)
                tokens: move tokens
            }
        }
        [_, ..] => {
            return ParseResult::Failure {
                success: false
                errors: move errors
            }
        }
    }
}

{
    let text = "...";

    let result = parse(text);
    if result.success {
        // ここで result.success = true なので
        // result は Success インターフェイスを満たすことが分かり、
        // tokens にアクセスできる。
        println!("OK: {} token(s)\n", result.tokens.len());
    } else {
        println!("ERROR: {} syntax error(s)\n", result.errors.len());
    }
}
```

マーカーインターフェイスと事後条件

```rust
impl String {
    marker interface IsNonEmpty

    fn is_non_empty(in self)
    ensures {
        true => self @ IsNonEmpty
    }
    {
        //
    }
}

struct User {
    name: String
    age: i64
}

interface User::Validated {
    name @ String::IsNonEmpty
    age @ i64::IsNonNegative
}

fn user_validate(in user: User) -> bool
ensures {
    // 結果が true なら user は User::Validated インターフェイスに合致する。
    // (これが成立しないような return があったらコンパイルエラー。)
    true => {
        user @ User::Validated
    }
}
{
    user.name.is_non_empty() && user.age.is_non_negative()
}
```

## 諸機能

### 型

数値型:

- u8, i64, f64, etc.

複合データ型:

- タプル
- struct
- enum
- union

```rust
struct Position {
    line: i64
    character: i64
}

// タグ付きユニオン (直和のケース)
enum Value {
    Int {
        as_int: i64
    }
    Float {
        as_float: f64
    }
}

// タグ付きユニオン (特殊なケース)
struct ParseResult {
    // 上記参照
}
```

型エイリアス

```rust
type str = Vec[byte];
```

関数ポインタ型

```rust
fn(x: i64, y: i64) -> u64
```

関数型(?)

```rust
Fn(x: i64, ref y: i64) -> i64
```

ジェネリクス

```rust
generic[T]
enum Option[T] {
    None
    Some(T)
}
```

その他 (標準ライブラリ)

```rust
type String = Vec[u8];
```

### トレイト

(TBD)

### 制御構文

変数定義

```fs
    let x = ...;
    let mut y = ...;
```

条件分岐

```fs
    match cond {
        p1 => { e1 }
        p2 => { e2 }
        ...
    }

    if cond {
        body
    } else {
        alt
    }

    if cond1 {
        body1
    } else if cond2 {
        body2
    } else {
        alt
    }
```

ループ

```rust
    break

    continue

    loop {
        body
    }

    while cond {
        body
    }

    for x in xs {
        body(x)
    }
```

### モード

- 間接性
    - ポインタで持つかどうか
- 可変性
    - 変更操作を許可するかどうか

キーワード

- ref: 間接参照
- mut: 変更可能
- mut[A]: 変更可能か否かは定数式 A: bool による
- mut?: 変更可能か否かは文脈による

```rust
generic[T]
struct Pair {
    first: T
    second: T
}

generic[T, M: bool]
fn pair_first(ref mut[M] pair: Pair[T]) -> ref mut[M] T {
    ref pair.first
}

// pair を可変参照 (ref mut) で渡したので、可変参照が返される。
let mut pair = Pair { first: 0, second: 0 };
pair_first(ref mut pair) += 1;

// pair を不変参照 (ref) で渡したので、不変参照が渡される。
let pair = Pair { first: 0, second: 0 };
// pair_first(ref pair) += 1;
```

参照のルール:

- スコープがより広い変数に参照をもたせることはできない
- スコープがより狭い変数に move することはできない
- move された変数は借用できない
- move された変数は move できない

```rust
    let ref r;
    {
        let a = "hello".clone();
        r = ref a; // NG: a より r の方がスコープが広い

        // ここで drop(a)
    }
    // r は不正
```

```rust
    let a = "hello".clone();
    let r = ref a;
    {
        // NG: a より b の方がスコープが狭い
        let b = move a;

        // ここで drop(b)
    }
    // r は不正
```

```rust
    let a = "hello".clone();
    let b = move a;

    //
    print(ref a);
```

### 関数

```rust
generic[T]
fn vec_is_empty(ref vec: Vec[T]) -> bool {
    vec.len() != 0
}
```

## モード

- own?: 所有するか否か。
- ptr?: ポインタ越しに (参照として) 保持する否か。
- read?: 読み取り可能か否か。
- write?: 書き込み可能か否か。

| own?  | ptr?  | read? | write?    | Rust              | raii-lang         |
|:------|:------|:------|:----------|:------------------|:------------------|
| Yes   | Yes   | Yes   | Yes       | `mut x: Box<T>`   | `box mut x`       |
| Yes   | Yes   | Yes   | No        | `x: Box<T>`       | `box [in] x`      |
| Yes   | Yes   | No    | Yes       | --                | `box out x`       |
| Yes   | Yes   | No    | No        | --                | --                |
| Yes   | No    | Yes   | Yes       | `mut x: T`        | `mut x`           |
| Yes   | No    | Yes   | No        | `x: T`            | `x`               |
| Yes   | No    | No    | Yes       | --                | `out`             |
| Yes   | No    | No    | No        | --                | --                |
| No    | Yes   | Yes   | Yes       | `x: &mut T`       | `ref mut x`       |
| No    | Yes   | Yes   | No        | `x: &T`           | `ref x`           |
| No    | Yes   | No    | Yes       | --                | `ref out x`       |
| No    | Yes   | No    | No        | --                | --                |
| No    | No    | Yes   | Yes       | `mut x: C`        | `mut x: C`        |
| No    | No    | Yes   | No        | `x: C`            | `x: C`            |
| No    | No    | No    | Yes       | --                | `out x: C`        |
| No    | No    | No    | No        | --                | --                |

- T は型。
- C は Copy トレイトを満たす型 (i64 など)。
- x は変数。

## CPS

- [コンパイラの作り方 (詳解)](https://www.is.s.u-tokyo.ac.jp/vu/96/cad/compilerresume/)
