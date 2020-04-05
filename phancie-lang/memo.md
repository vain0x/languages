# メモ

(未定)

## コンセプト

C言語に安全性・抽象性を高めるための仕組みをつける

根本機能

- C言語の機能
- 型レベル計算による契約

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

## 参照型

- ref T :> ref ?mut T :> ref mut T

## 可変性

- 式は可変性を持つ
- オブジェクトの状態変化を追跡しやすくするため
- 可変性自体は型ではない
- 関数型はパラメータや結果の可変性を含む

可変性の記号

- !mut (読み取り専用)
- ?mut (可変性の伝播)
- mut (読み書き可能)

可変性の既定値

- 変数の可変性は既定で !mut
- 関数の結果の可変性は既定で mut

式の可変性

- mut な変数の式は mut
- mut な式の参照、フィールドは mut
- mut な ref mut 型の式の脱参照は mut
- move 式は mut
- 結果が mut な操作は mut

可変性の伝播

- mut でない変数の式は ?mut
- ?mut な式の参照、脱参照、フィールドは ?mut
- 結果が ?mut である操作は、次の条件を満たせば ?mut
    - 条件: ?mut/mut なパラメータに渡す引数がすべて ?mut/mut であること

参照の可変性

- x: ?mut, x: T → ref ?mut x: ref ?mut T
- x: mut, x: T → ref mut x: ref mut T

例

```rust
// 可変性の制約

let x = 1;
let mut y = 2;

// NG
// x += 1;

// OK
y += 1;
```

```rust
// 可変性の伝播

struct Count {
    inner: i64
}

fn get(?mut count: ref ?mut Count) -> ref ?mut T {
    ref ?mut count.inner
}

let x = Count { inner: 1 };
get(x); //=> 1

let mut y = Count { inner: 2 };
get(y) += 1;
```

## 依存解析

- 参照の無効化を追跡する

別名集合

- 同じオブジェクトを指している可能性を追跡するためのもの
- 依存集合の構築に使う
- 変数やフィールドは別名集合を持つ
- 別名集合には自身も含む
- x ~ y : x, y が互いの別名である可能性がある

依存集合

- 他のオブジェクトに依存している可能性を追跡するためのもの
- オブジェクトが変更されたら、それに依存している参照は無効化しなければいけない
- x ~> y : y は x に依存している
- 初め、依存集合は空
- 依存先の別名集合・依存集合に含まれるすべての要素に依存するとみなす
- s ~> s.t
- v ~> v(i)

## 契約

ensures 句 (事後条件)

- 関数の結果は ensures 句を持つ
- パラメータや結果の名前を使って条件を指定する
- すべてのコントロールパスにおいて、ensures に書かれたすべての条件が成り立つことが静的検査される
- 関数の呼び出しの後、その関数の ensures 句に書かれたすべての条件が成立するとみなし、後続の静的検査で利用される
- バージョン番号が変化した変数やフィールドを含む条件は無効となる

述語

```rust
// 式 x はパターン p にマッチする
x @ p

// 式 x に型 t がつく
x: t

// 条件 p が成立する場合は必ず、条件 q も成立する
p => q

// 式 y は式 x に依存している
x ~> y
```

## ファントム

ファントムは型の一種。

- ファントムはいくつかのフェイズからなる
    - フェイズは互いに排他的
- フェイズはファントムの部分型
    - フェイズ型を含めて「ファントム」と呼ぶことがある
- フェイズは条件式を持つ
    - 式がフェイズの条件を満たすときのみ、式にフェイズが型としてつく
    - 式がいずれかのフェイズの条件を満たすときのみ、式にファントムが型としてつく
- 以下の式にはファントムが型としてつく
    - ファントムにキャストされる式
    - ファントムを持つパラメータに渡される式
    - 結果がファントムを持つ関数の呼び出し

糖衣構文

```rust
// すべてのフェイズが条件 self: T を持つ
phantom P: T { ... }

phantom P {
    // Q { self @ p }
    Q @ p
}

// 無条件のフェイズを1つ持つ
phantom P;
```

### 例: ensures

```rust
phantom Signed {
    Zero
    Positive
    Negative
}

fn is_positive(x: i64) -> bool
ensures(result) {
    result @ true => x: Signed::Positive
}
{
    x > 0
}

{
    let x: i64 = f();

    if is_positive(x) {
        log2(x);
    }
}
```

### 例: ポインタ

```rust
generic[T]
phantom Ptr: usize {
    Null @ 0
    NonNull
}
```

```rust
generic[T]
fn ptr_add(p: Ptr[T], i: i64) -> Ptr[T] {
    p + i * size_of[T]
}

let p: Ptr[i32];
ptr_add(p, 1); //=> p + 4

let q: Ptr[i64];
ptr_add(q, 1); //=> q + 8
```

### 例: 列挙体

```rust
phantom Color: i64 {
    Red @ 1
    Green @ 2
    Blue @ 3
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
struct ParseResult: ParseResultInvariant {
    ok: bool
    union {
        tokens: Vec[Token]
        errors: Vec[Error]
    }
}

phantom ParseResultInvariant: ParseResult {
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
fn parse(text: ref String) -> ParseResult {
    let mut tokens = [];
    let mut errors = [];

    // 略

    if !Vec::is_empty(errors) {
        return ParseResult {
            ok: false
            errors: move errors
        }
    }

    if tokens @ [_, ..] {
        ParseResult {
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
phantom NonEmpty;

fn is_empty(s: ref String) -> bool
ensures(result) {
    result @ false => s: NonEmpty
} {
    s.len() == 0
}

fn first(s: ref ?mut String + NonEmpty) -> ref ?mut char {
    s[0]
}

{
    let mut s = "hello";
    if !is_empty(&s) {
        first(&s);
    }

    String::clear(ref s);
    // first(&s);
}
```

## 関数のファントム

関数ポインタを表す特殊なファントム

`fn(T1, T2, ...) -> U`

```rust
fn add(x: i64, y: i64) -> i64 {
    x + y
}

{
    let mut p = &add;
    p(2, 3); //=> 5

    p = &fn(x, y) { x - y };
    p(2, 3); //=> -1
}
```

クロージャの型が持つ特殊なファントム

`Fn[S](T1, T2, ...) -> U`

```rust
{
    let n = 2;

    // struct S { n: i64 }
    // f: S + Fn[S](i64) -> i64
    let f = fn(x: i64) { x + n };

    f(3); //=> 5
}
```

## 所有権

- リソース管理のための機能

所有権

- Drop ファントムを持つ変数やフィールドはオブジェクトへの所有権を持つ

ルール

- 所有権を持つ変数は move されない限りコピーできない
- move された変数やフィールドは未初期化になる
- すべてのコントロールパスにおいて、スコープから外れる変数が所有権を持たないことが静的検査される

```rust
generic[T]
phantom Box[T]: Ptr[T] + Drop;

generic[T]
fn box_new(content: T) -> Box[T] {
    malloc(size_of[T]) as Box[T]
}

generic[T]
fn box_drop(p: Box[T]) {
    free(move p);
}

{
    let p = box_new("hello");
    box_drop(move p);
}
```

## メモリ管理

ポインタ

- オブジェクトへのポインタをとれる
- ポインタの操作は安全ではない

変数への参照

- S にシンボル型が入る
- `s: T + %s[v]` → `&s: Ref[%s[v], T]`, `s ~ &s`

フィールドへの参照

- ローカル変数と同様。
- `&s: S`, `&s.t: T` → `&s.t: Ref[S, T]`, `&s ~> &s.t`

オブジェクトが所有するデータへの参照

- `&v: Ref[%v, Vec[T]]` → `&v(0): Ref[S, T]`

参照を持つオブジェクト

```rust
generic[S, T]
struct Borrow {
    r: Ref[S, T]
}
```

参照の検査

- S が有効なときのみ、参照をコピーしたり脱参照したりできる

無効参照の問題が生じるよくあるケース:

```rust
fn leak_to_outer_block() {
    let r = {
        let a = clone("hello");
        defer {
            String::drop(move a);
        }

        &a
    };

    // OK: a は無効
    print(r);
}
```

```rust
fn leak_to_callee() -> &String {
    let a = clone("hello");
    defer {
        String::drop(move a);
    }
    let r = &a;

    // OK: a は無効
    r
}
```

```rust
fn invalidate_ref() {
    let a = clone("hello");
    let r = &a;
    {
        let b = move a;

        String::drop(move b);
    }

    // OK: a は無効
    print(r);
}
```

```rust
fn invalidate_vec_item() {
    let mut v = [clone("hello")];
    let r = &v(0);

    Vec::push(ref v, "world".clone());

    // OK: r は無効
    print(r);
}
```

## CPS

- [コンパイラの作り方 (詳解)](https://www.is.s.u-tokyo.ac.jp/vu/96/cad/compilerresume/)

## Similar projects

- [ZetZ](https://github.com/zetzit/zz)
