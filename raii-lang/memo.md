# メモ

(未定)

## コンセプト

目標:

- 静的検査による安全性の確保と、効率的な手続き的プログラミングのバランスをとる。

要点:

- Rust から借用検査を取り除き、代わりに契約の仕組みを入れる。

やらないこと:

- ゼロコスト抽象化
- aliasable XOR mutable (参考: [Notes on a smaller Rust](https://boats.gitlab.io/blog/post/notes-on-a-smaller-rust/))
- 参照型 (`&T`/`&mut T`)
- 関数型プログラミング
- マルチスレッド

## 中心機能

- 代数的データ型・パターンマッチ
- RAII
    - 参照をスコープの外側に出さないことでライフタイムを自明に保つ
- 変数のモード
    - 参照性: オブジェクトを参照(ポインタ)越しに持つかどうか明確にすることで、変数が共有されうるか分かりやすくする
    - 可変性: 不変な変数を通しての変更操作はエラーにすることで、変数が変更の対象であるか分かりやすくする
- 契約
    - 生存変数は他の変数の性質を主張するルール
    - 事前条件を満たさない関数呼び出しはコンパイルエラー
- アスペクト
    - 型の動的な状態を表現するもの
    - 主に事前条件・事後条件で、パラメータに条件を課すのに使う

## 諸機能

### 型

基本型:

- byte (u8)
- int (i64)

代数的データ型:

- struct
- enum

```rust
struct Position {
    line: int
    character: int
}

enum Document {
    PlainText(str)
    Markdown {
        blocks: Vec[str]
    }
}
```

型エイリアス

```rust
type str = Vec[byte];
```

外部型

```rust
// uint64_t in C
extern type u64;
```

関数ポインタ型

```rust
fn(x: int, y: int) -> u64;
```

関数型(?)

```rust
Fn(x: int, ref y: int) -> int;
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
generic[T]
extern type Ptr[T];
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
fn pair_first(ref mut[M] self) -> ref mut[M] T {
    ref self.first
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

### 契約

生存変数は変数に関する性質を主張できる。

関数は事後条件として、引数や結果が何らかの性質を持つことを、結果値に主張させることができる。(assert)

```rust
generic[T]
fn is_some(ref option: Option[T]) -> bool
assert {
    // 結果が true なら、option は Some(_) にマッチする、と主張する。
    true => option match Some(_)
}
{
    match option {
        Some(_) => {
            // ここで assert の主張がコンパイル時に検証される。
            true
        }
        None => {
            false
        }
    }
}
```

関数は事前条件として、引数が一定の性質を持つことを要求できる。(require)

```rust
generic[T]
fn get(ref option: Option[T]) -> ref T
require
    option match Some(_)
assert
    option is move[false]
{
    match option {
        Some(ref value) => {
            value
        }

        // 事前条件から None => ... は不要と分かる。
    }
}
```

```rust
{
    let option: Option[T] = ...;

    // ここで option match Some(_) の事前条件を満たさないので
    // get(ref option) はコンパイルエラーになる。
    // print(get(ref option));

    let ok = is_some(ref option);
    if ok {
        // ここで is_some の事後条件より
        //      ok match true => option match Some(_)
        // であり、また if の判定より
        //      ok match true
        // だから option match Some(_) がいえて、
        // 事前条件を満たすので get が呼べる。
        print(get(ref option));
    }
}
```

```rust
{
    let mut option = Some(1);

    let ref r = get(ref option);
    // get の結果である変数 r のスコープにおいて、
    // get の事後条件 option is move[false] から、
    // option は move 可能でない。

    // コンパイルエラーになる。(これを許すと r が不正な参照になる。)
    // let other = move option;

    foo(r); // r はここまで生存する。
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
