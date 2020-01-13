# メモ

(未定)

## コンセプト

C言語 with リソース管理 + ジェネリクス + 安全

- データをリソースとみなす
- アルゴリズムを明示する
- 安全性を保つ

やらないこと:

- ゼロコスト抽象化
- 関数型プログラミング
- マルチスレッド

参考にする言語:

- C
- C++ (デストラクタなど)
- C# (ref パラメータなど)
- Rust (構文や型システムなど)
- TypeScript (データ型と型検査の分離など)

参考にする記事:

- [Notes on a smaller Rust](https://boats.gitlab.io/blog/post/notes-on-a-smaller-rust/)
- [［翻訳］なぜそんなに確信が持てるのか？](https://onihusube.hatenablog.com/entry/2019/12/13/211603)

## メモリ管理

C++/Rust に類似した RAII を備える。メモリ安全性は動的検査等により保証される。

所有権:

- 数値などの一部の型を除いて、オブジェクトは変数に所有される。
- 変数がスコープを外れる際に、それが所有しているオブジェクトは drop される。

参照:

- オブジェクトへのポインタをとれる。ポインタの操作は安全ではない。
- 変数への参照を取れる。
    - 参照される変数は、参照カウントという整数を持つ。
    - 参照 (`&T`) はオブジェクトへのポインタと参照カウントへのポインタのペア。
    - 参照が生成されるとき、参照カウントを1増やし、参照が破棄される際に1減らす。
    - オブジェクトが move または drop する際に、参照カウントがゼロでなければランタイムエラー。
- ベクタの要素の参照を取れる。
    - ベクタの要素など、オブジェクトが間接的に所有するものへの参照は、オブジェクトが持つ参照カウントを使う。
    - 例えば `&v[i]` は `Vec::ref_count` を参照カウントとして使う。(そのようにライブラリ側で実装する。)

備考:

- ランタイムエラーを避けるため、オブジェクトの move はなるべく避ける。
    - 関数からオブジェクトを受け取るより、関数にオブジェクトを参照で渡してフィールドに値を代入してもらう方が安全。

無効参照の問題が生じるよくあるケース:

```rust
fn leak_to_outer_block() {
    let r: &String;
    {
        let a = "hello".clone();
        r = &a;

        // ここで drop(a) が起こり、参照 r が無効になる。
        // OK: a の参照カウントは 1 (r) なのでランタイムエラー
    }

    f(r);
}
```

```rust
fn leak_to_callee() -> &String {
    let a = "hello".clone();
    let r = &a;
    // OK: a の drop 時に参照カウントが 1 (関数の結果) なのでランタイムエラー
    r
}
```

```rust
fn invalidate_ref() {
    let a = "hello".clone();
    let r = &a;
    {
        // a を move すると参照 r は無効になる。
        // OK: 参照カウントが 1 なのでランタイムエラー
        let b = move a;

        // ここで drop(b) が起こる。
    }
    f(r);
}
```

```rust
generic[T]
impl Vec[T] {
    fn get(v: &Vec[T], i: i64) -> &T {
        // v と借用カウンタを共有する参照を作る。
        Ref::with_data(v, Ptr::add(v.data, i))
    }

    fn push(v: &mut Vec[T], item: T) {
        if v.capacity() == v.len() {
            // 新しいバッファを確保して各要素を移動する。
            let new_buffer: Ptr[T] = ...;
            let k = address_of(v.ref_count);

            for i in 0..n {
                let s = Ref::with_data(Ptr::add(old_buffer, i), k);
                let t = Ref::with_data(Ptr::add(new_buffer, i), k);

                // OK: 要素への参照があるとここでパニック
                *t = move *s;
            }
        }
        // ...
    }
}

fn vec_push() {
    let mut v = ["hello".clone()];
    let r = &v[0];

    // v のバッファが移動すると参照 r は無効になる。
    // OK: バッファの移動のために move する際にランタイムエラー
    v.push("world".clone());

    f(r);
}
```

## フェイズ

- struct/union は型のメモリレイアウトを決定する。
- phase はデータ型が満たす条件を付加する。
    - (補足: phase は state の類義語で、state より使用頻度の低い単語として選びました。[State Synonyms](https://www.thesaurus.com/browse/state?s=t))

```rust
// 構文解析の結果を持つデータ型。
struct ParseResult {
    success: bool
    union {
        tokens: Vec[Token]
        errors: Vec[String]
    }
}

impl ParseResult {
    // default phase は ParseResult が常に持つべきフェイズを示す。
    // ParseResult は Success または Failure フェイズのどちらか。
    // (union を含む型は、デストラクタで利用するために既定フェイズが必須。)
    default phase {
        // 構文解析に成功したときのフェイズ
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
}

fn parse(in text: String, out mut r: ParseResult) {
    Vec::new(out r.tokens);
    Vec::new(out r.errors);

    // 略

    // エラーがなければ成功。
    Vec::is_empty(r.errors, out r.success);

    if !r.success {
        // ここで r が Failure フェイズを満たしていることは、
        // is_empty の事後条件から、コンパイル時に分かる。
        // (そうでなければコンパイルエラー。)
        return;
    }

    // 動的検査。
    assert(r.tokens @ [_, ..]);

    // 上の assert のおかげで r @ Success が分かる。
    return;
}

fn main() {
    let text = "...";

    parse(text, let r);
    if r.success {
        // ここで r.success = true なので、
        // r は Success フェイズを満たすことがコンパイル時に分かる。
        // そのため r.tokens にアクセスできる。
        println!("OK: {} token(s)\n", r.tokens.len());
    } else {
        println!("ERROR: {} syntax error(s)\n", r.errors.len());
    }
}
```

### 事後条件

```rust
impl String {
    phase {
        // 特に条件のないフェイズ
        IsNonEmpty

        // String が IsNonEmpty フェイズを満たさないかもしれないことを示す。
        ..
    }

    fn is_non_empty(in self, out result)
    ensures {
        // result = true で return したら、
        // self は IsNonEmpty フェイズを満たす、という事後条件。
        result @ true => self @ IsNonEmpty
    }
    {
        result = self.len() != 0;
    }
}

struct User {
    name: String
    age: i64
}

impl User {
    phase {
        ValidatedUser {
            name @ String::IsNonEmpty
            age @ i64::IsNonNegative
        }
        ..
    }

    fn validate(in self, out valid: bool)
    ensures {
        // 結果が true なら user は ValidatedUser フェイズを満たす。
        // (これが成立しないような return はコンパイルエラー。)
        valid @ true => user @ ValidatedUser
    };
    {
        user.name.is_non_empty(let name_ok);
        user.age.is_non_negative(let age_ok);
        valid = name_ok && age_ok;
    }
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

変数や参照の可変性をコンパイル時に検査する。

```rust
generic[T]
struct Pair {
    first: T
    second: T
}

generic[T, M: bool]
fn pair_first(pair: &mut[M] Pair[T]) -> &mut[M] T {
    &mut[M] pair.first
}

// pair を可変参照 (ref mut) で渡したので、可変参照が返される。
let mut pair = Pair { first: 0, second: 0 };
pair_first(ref mut pair) += 1;

// pair を不変参照 (ref) で渡したので、不変参照が渡される。
let pair = Pair { first: 0, second: 0 };
// pair_first(ref pair) += 1;
```

### 関数

```rust
generic[T]
fn vec_is_empty(vec: &Vec[T]) -> bool {
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
