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

C++/Rust に類似した RAII を備える。メモリ安全性は静的検査や動的検査により保証される。

所有権:

- 数値などの一部の型を除いて、オブジェクトは変数に所有される。
- 変数がスコープを外れる際に、それが所有しているオブジェクトは drop される。

参照:

- オブジェクトへのポインタをとれる。ポインタの操作は安全ではない。
- 参照型 `struct Ref[S, T] { addr: i64 }`
    - S は参照先が有効であることを保証するための幽霊型
    - T は参照先の型
    - 実体はただのポインタ (i64)
- ローカル変数への参照
    - 変数自身を含む型をつける。
    - `s: T` → `&s: Ref[%s, T]`
    - 変数 s がスコープから外れた後に脱参照するとコンパイルエラーになる。
- フィールドへの参照
    - ローカル変数と同様。
    - `&s: S`, `&s.t: T` → `&s.t: Ref[S, T]`
- オブジェクトが所有するデータへの参照
    - オブジェクトごとに異なる型を使って表現する。
    - 例えば、ベクタの要素への参照には `VecRef[S, T]` 型を使う。
    - 具体的には、ベクタへの参照とインデックスのペア
        `struct VecRef { v: S, i: i64 }`
    - ベクタの内部バッファが移動しても無効化しない。
    - `xs: Ref[X, Vec[T]]`, `i: i64` → `VecRef[Ref[X, Vec[T]], T]`
- `&s` や `*x` をトレイトでオーバーロードすることで使いやすくする。

無効参照の問題が生じるよくあるケース:

```rust
fn leak_to_outer_block() {
    let r = {
        let a = clone("hello");
        &a

        // drop(a) が起こり、参照 &a が無効になる。
    };

    // OK: Ref[%a, String] 型の値 r を脱参照しようとしているが、
    //     a のスコープではないのでコンパイルエラー
    print(r);
}
```

```rust
fn leak_to_callee() -> impl Deref[Output = String] {
    let a = clone("hello");
    let r = &a;

    // drop(a) が起こり、参照 r が無効になる。
    // OK: ローカル変数型 %a を含む型を返そうとしているのでコンパイルエラー
    r
}
```

```rust
fn invalidate_ref() {
    let a = clone("hello");
    let r = &a;
    {
        // a を move すると参照 r は無効になる。
        let b = move a;

        // ここで drop(b) が起こる。
    }

    // OK: Ref[%a, String] 型の値 r を脱参照しようとしているが、
    //     a は move 済みなのでコンパイルエラー
    print(*r);
}
```

```rust
fn invalidate_vec_item() {
    let mut v = [clone("hello")];

    let r = &v[0];
    let c = &r[0];

    // v のバッファが移動するかもしれない。
    v.push("world".clone());

    // OK: ベクタにはインデックス経由でアクセスするので、
    //     バッファが移動しても参照オブジェクト r や c は無効にならない。
    print_str(*r);
    print_char(*c);

    v.clear();

    // OK: ベクタにはインデックス経由でアクセスするので、
    //     範囲検査によりランタイムエラー
    print_str(*r);
    print_char(*c);
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
