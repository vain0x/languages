# メモ

(未定)

## コンセプト

C言語に安全性・抽象性を高めるための仕組みをつける

根本機能

- C言語の機能
- スコープに基づくリソース管理
- スコープに基づく暗黙引数
- 契約

設計思想

- C言語を基本とみなす
    - 型システムや糖衣構文を除いて、ほぼC言語であるべき
- 安全なコードや抽象的なコードを書くための仕組みを提供する
    - 自動的に安全かつ抽象的になるわけではない
- 非自明なことを暗黙化しない
    - 特にアルゴリズムとインターフェイスは明確にする

参考になる言語:

- C
- C++ (デストラクタなど)
- C# (in/out パラメータなど)
- Rust (構文など)
- TypeScript (データの表現と型検査の分離など)
- Scala (暗黙引数(implicit))

参考になる記事:

- [Notes on a smaller Rust](https://boats.gitlab.io/blog/post/notes-on-a-smaller-rust/)
- [［翻訳］なぜそんなに確信が持てるのか？](https://onihusube.hatenablog.com/entry/2019/12/13/211603)

## C由来の機能

制御構文

- let
- if, match
- while, for, loop, break, continue
- fn, return

データ型

- 整数型 u8, i64, usize, etc.
- 浮動小数点数型 f32, f64
- 複合型 struct, union

## ファントム

ファントムは型の一種。型レベルの代数的データ型。

- ファントムはいくつかのフェイズからなる
    - フェイズは互いに排他的
- フェイズはファントムの部分型である
    - 「ファントム」にフェイズ型を含むことがある
- フェイズは条件式を持つ
    - 条件が静的検査を通る式にのみフェイズ型がつく
    - フェイズではなくファントムの型をつける際は、いずれかのフェイズの条件を満たせばよい
- 以下の式はファントムを得る
    - ファントムにキャストされる式
    - ファントムの引数に渡される式
    - ファントムの結果を持つ関数の呼び出し
    - ensures 句を持つ関数呼び出し

```rust
// 式 x はパターン p にマッチする (束縛はしない)
x @ p

// 式 x は型 t を持ち、型 t のフェイズ条件を静的に満たす
x: t
```

事前条件

```rust
phantom SignedI64: i64 {
    Zero {
        self @ 0
    }
    Positive {
        self @ 1..
    }
    Negative {
        self @ ..0
    }
}

fn log2(x: SignedI64::Positive) -> f64 {
    // 略
}

{
    // 静的検査が通る。
    log2(8); //=> 3
}

{
    let x: i64 = f();

    // 動的検査で条件を通す。
    if (x @ 1..) {
        log2(x);
    }
}
```

事後条件

```rust
fn is_positive(x: i64) -> bool
ensures(result) {
    result @ true => x: SignedI64::Positive
}
{
    x @ 1..
}

{
    let x: i64 = f();

    if is_positive(x) {
        log2(x);
    }
}
```

## 暗黙引数

```rust
generic[T]
phantom Default: T;

// Default[i64] 型の変数 を impl 変数として定義する。
impl Default[i64] = {
    0
};

fn default() use(d: Default[i64]) -> i64 {
    d
}

default(); //=> 0
```

- impl 変数はローカルからレキシカルスコープを遡って検索される。use も辿る。

### 機能: シンボル型

ローカル変数 `x` は通常の型に加えて固有の型を持つ。`%x` と書く。

- 束縛・代入・変更されるたびに異なるシンボル型を得る。(リフレッシュ)
- move されるとシンボル型を失う。

```rust
    let x = "1";
    // x: String + %x (A)

    x = "2";
    // x: String + %x (B)
    // A と B に互換性はない
```

### 機能: 関数ファントム

関数ポインタの型 `fn(T1, T2, ...) -> U` はファントム。

```rust
fn add(x: i64, y: i64) -> i64 {
    x + y
}

{
    let mut p = address_of(add);
    p(2, 3); //=> 5

    p = address_of(fn(x, y) { x - y });
    p(2, 3); //=> -1
}
```

クロージャの型 `Fn[S](T1, T2, ...) -> U` はファントム。

```rust
fn foo() {
    let n = 2;

    // struct S { n: i64 }
    // f: S + Fn[S](i64) -> i64
    let f = fn(x: i64) { x + n };

    f(3); //=> 5
}
```

### 例: ポインタ

```rust
generic[T]
phantom Ptr: usize;
```

```rust
generic[T]
fn ptr_add(p: Ptr[T], i: i64) -> Ptr[T] {
    (p as usize) + i * size_of[T]
}

let p: Ptr[i32];
ptr_add(p, 1); //=> p + 4

let q: Ptr[i64];
ptr_add(q, 1); //=> q + 8
```

### 例: 列挙体

```rust
phantom Color: i64 {
    // Red { self @ 1 } の略
    Red = 1
    Green = 2
    Blue = 3
}

fn color_to_hex(color: Color) -> String {
    match color {
        Color::Red => {
            "#FF0000"
        }
        Color::Green => {
            "#00FF00"
        }
        Color::Blue => {
            "#0000FF"
        }
    }
}

color_to_hex(Color::Red); //=> #FF0000
```

### 例: 代数的データ型

```rust
struct ParseResultState {
    ok: bool
    union {
        tokens: Vec[Token]
        errors: Vec[Error]
    }
}

phantom ParseResult: ParseResultState {
    Ok {
        self.ok @ true
        self.tokens @ [_, ..]
    }
    Err {
        self.ok @ false
        self.errors @ [_, ..]
    }
}
```

```rust
generic[]
fn parse(text: Ref[_, String]) -> ParseResult {
    let mut tokens = [];
    let mut errors = [];

    // 略

    if !Vec::is_empty(errors) {
        return ParseResult::Err {
            ok: false
            errors: move errors
        };
    }

    if tokens @ [_, ..] {
        ParseResult::Ok {
            ok: true
            tokens: move tokens
        }
    } else {
        // ???
    }
}

{
    let text = "...";

    let r = parse(text);
    if r.ok {
        println!("OK: {} token(s)\n", r.tokens.len());
    } else {
        println!("ERROR: {} syntax error(s)\n", r.errors.len());
    }
}
```

### 例: データ検証

```rust
phantom NonEmptyString: String;

generic[S]
fn is_empty(s: Ref[S, String]) -> bool
ensures(result) {
    result @ false => s: NonEmptyString
} {
    s.len() == 0
}

generic[S]
fn first(s: Ref[S, NonEmptyString]) -> char {
    s[0]
}

{
    let s = "hello";
    if !is_empty(&s) {
        first(&s);
    }

    String::clear(s);
    // first(&s);
}
```

## メモリ管理

C++/Rust のような RAII を備える。

所有権:

- 数値などの一部の型を除いて、オブジェクトは変数に所有される。
- 変数がスコープを外れる際に、それが所有しているオブジェクトは drop される。

参照:

- オブジェクトへのポインタをとれる。ポインタの操作は安全ではない。
- 参照型 `generic[S, T] phantom Ref for Ptr[T]`
    - S は参照の有効性を検査するための幽霊型
    - T は参照先の型
    - 実体はポインタ
- ローカル変数への参照
    - S にシンボル型が入る
    - `s: T` → `&s: Ref[%s, T]`
- フィールドへの参照
    - ローカル変数と同様。
    - `&s: S`, `&s.t: T` → `&s.t: Ref[S, T]`
- オブジェクトが所有するデータへの参照
    - `&v: Ref[%v, Vec[T]]` → `&v(0): Ref[S, T]`
- 参照の無効化
    - 可変なパラメータに型 S の式を渡すとき、型 S を変更状態にする
    - `Ref[S, T]` を変更状態にするには、S, T を変更状態にする
    - `%x` を変更状態にするには、`%x` を変更状態にマークし、x のシンボル型をリフレッシュする
    - 型 S の式を move するときは S を同様に無効状態にする
- 参照の検査
    - `Ref[S, T]` 型の式をパラメータに渡すには、S が変更状態でなく、T が無効状態でないことを検査する

無効参照の問題が生じるよくあるケース:

```rust
fn leak_to_outer_block() {
    let r = {
        let a = clone("hello");
        &a

        // drop(a) が起こり、参照 &a が無効になる。
    };

    // OK: %a が無効なので Ref[%a, String] 型の r を渡せない
    print(r);
}
```

```rust
fn leak_to_callee() -> &String {
    let a = clone("hello");
    let r = &a;

    // drop(a) が起こり、参照 r が無効になる。
    // OK: %a が無効なので結果に渡せない。
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

    // OK: %a は無効なので r を渡せない。
    print(r);
}
```

```rust
fn invalidate_vec_item() {
    let mut v = [clone("hello")];
    let r = &v(0);

    v.push("world".clone());

    // OK: %v が変更状態なので Ref[%v, _] 型の r は渡せない
    print(r);
}
```

## その他

関数を含むトレイト。

```rust
generic[T]
struct Monoid {
    empty: T
    append: fn(T, T) -> T
}

impl Monoid[i64] {
    empty: 0
    append: fn(x: i64, y: i64) {
        x + y
    }
}

generic[S]
fn sum(xs: Ref[S, Vec[T]]) use(monoid: Monoid[T]) -> T {
    let mut sum = monoid.empty;
    for x in xs {
        sum = (monoid.append)(sum, x);
    }
    sum
}
```

参照引数の略記

```rust
fn borrow(ref s: String) {}
// generic[S] fn borrow(s_1: Ref[S, String]) { let alias s = *s1; }
```

## ライブラリ

```rust
generic[T]
phantom Ptr: usize {
    Null = 0
    NonNull {
        !(self @ 0)
    }
}

generic[S, T]
phantom Ref: Ptr[T];

generic[R, T]
struct Deref {
    deref: fn(R) -> Ptr[T]
}

generic[S, T]
impl Deref[Ref[S, T], T] {
    deref: fn(s: Ref[S, T]) -> Ptr[T] {
        s as Ptr[T]
    }
}

generic[T]
struct Vec {
    data: Ptr[T]
    size: usize
    capacity: usize
}

generic[T]
phantom AddAssign {
    add_assign: generic[S] fn(Ref[S, T], T)
}

generic[S, T]
fn add_assign(first: Ref[S, T], second: T)
use(
    deref: Deref[S, T]
    adder: AddAssign[T]
) {
    adder.add_assign[S](deref.deref(first), second);
}

impl AddAssign[i32] {
    value: generic[S] fn(first: Ref[S, i32], second: i32) use(deref: Deref[S]) {
        let p = deref.deref(first);
        let value = ptr_read[i32](p);
        ptr_write[i32](p, value + second);
    }
}
```

## CPS

- [コンパイラの作り方 (詳解)](https://www.is.s.u-tokyo.ac.jp/vu/96/cad/compilerresume/)
