# メモ

(未定)

## コンセプト

C言語に安全性・抽象性を高めるための仕組みをつける

根本機能

- スコープに基づくリソース管理 (RAII)
- スコープに基づく暗黙引数 (implicit)
- メモリレイアウトから分離した型検査 (インターフェイス)

設計思想

- C言語をデフォルトとする
- 安全なコードや抽象的なコードを書くための道具を提供する
    - 自動的に安全かつ抽象的になるわけではない
- 非自明なことは明示的にする
    - 自明なことはなるべく暗黙化する (ジェネリクスやトレイトなど)

参考にする言語:

- C
- C++ (デストラクタなど)
- C# (in/out パラメータなど)
- Rust (構文など)
- TypeScript (メモリレイアウトと型検査の分離など)
- Scala (暗黙引数(implicit))

参考にする記事:

- [Notes on a smaller Rust](https://boats.gitlab.io/blog/post/notes-on-a-smaller-rust/)
- [［翻訳］なぜそんなに確信が持てるのか？](https://onihusube.hatenablog.com/entry/2019/12/13/211603)

## 暗黙引数

関数は暗黙の引数を取る。use の後に宣言されたパラメータリストには impl 変数が束縛される。

型クラスの代わりに使える。

```rust
generic[T]
struct Default[T] {
    value: T
}

// Default[i32] 型の静的変数を impl 変数として宣言する。
// static let _ = Default[i32] { value: 0 }; のような意味。
static impl Default[i32] {
    value: 0
}

fn default() use(d: impl Default[i32]) -> i32 {
    // d には上の impl Default[i32] が束縛されている。
    d.value
}

default(); //=> 0
```

## データ型

データ型はオブジェクトのメモリレイアウトを決定するための型。C言語と同様。

- 整数型 u8, i64, usize, etc.
- 浮動小数点数型 f32, f64
- 複合型 struct, union

## インターフェイス型

インターフェイス型はオブジェクトのメモリレイアウトに影響しない。以下の用途に使う。

- 操作を拡張する。一定のインターフェイスが「ついている」型にだけ適用できる関数など
- 操作を制限する。一定のインターフェイスが「ついていない」型にだけ適用できる関数など
- 静的解析のヒントにする。

インターフェイス型がついた変数の用途や制約を表現する。

### 機能

インターフェイスの宣言は、インターフェイスを満たす可能性がある既存の型を指定する。ブロック内にはインターフェイスが取りうる状態を表す「フェイズ」を列挙し、各フェイズにはそのフェイズを満たす条件を書く。

- 複数のフェイズの条件を満たすときは、最初のフェイズにだけ該当する。複数のフェイズを同時に満たすことはない。
- どのフェイズの条件も満たさないような値には、インターフェイスの型はつかないはず。

```rust
interface SignedI32 for i32 {
    Zero {
        // self が定数パターン 0 にマッチする、という条件
        self @ 0
    }
    Positive {
        self @ 1..
    }
    Negative {
        self @ ..0
    }
}
```

変数にインターフェイス型やフェイズ型を持たせるにはキャスト構文を使う。上記の条件が静的検査される。

```rust
// 正の整数しか受け取らない対数関数
fn log2(_: SignedI32::Positive) -> f64 {
    // 略
}

fn main() {
    // 条件 8 @ 1.. は静的に分かるので、8 が Positive フェイズを満たすことは静的に分かる。
    log2(8 as SignedI32::Positive); //=> 3
}
```

キャストが静的検査を通らないときは、事前に動的検査をする。

```rust
    let x: i32 = f();
    // 動的検査。条件に合致しなければパニック。
    assert(x @ 1..);
    // assert のおかげで x @ 1.. が分かるので x @ Positive も分かる。
    log2(x as SignedI32::Positive);
}
```

事後条件として、引数の型にインターフェイス型を足せる。

```rust
fn is_positive(x: i32) -> bool
ensures {
    true => x @ SignedI32::Positive
}
{
    x @ 1..
}

fn main() {
    let x: i32 = f(); //=> 8

    if is_positive(x) {
        // is_positive の結果が true なので
        // x は Positive 型を持つ。
        log2(x); //=> 3
    }
}
```

### 機能: 既定インターフェイス

struct/union の定義に続けて、その型の既定インターフェイスを書ける。

- 既定インターフェイスを持つ型は、すべてのフィールドが有効で、そのいずれかのフェイズを満たす限り有効である。
    - 有効でない変数の drop は呼ばれず、有効なフィールドが各々 drop される。
- union は必ず既定インターフェイスを持ち、常に「どのフィールドも初期化されていない」か「いずれかのフェイズを満たす」。
    - drop がフェイズに基づいて生成される。

```rust
struct Connection {
    connected: bool
    session: Session
} with {
    Connected {
        self.connected @ true
    }
}

fn main() {
    // 無効状態。
    let connection = Connection {
        connected: false,
        session: create_session()
    };

    if flip() {
        // drop(connection) は呼ばれない。
        // session は初期化されているので drop(session) だけ呼ばれる。
        return;
    }

    connection.connected = true;

    // drop(connection) が呼ばれる。
}
```

### 機能: ローカル変数型

ローカル変数 `x` は代入されるたびに相異なるインターフェイスを満たす。ここでは `%x` と書く。

```rust
    let x = "1";
    // x は一意なインターフェイス %x (A) を満たす。

    x = "2";
    // 代入文を経ると別のインターフェイス %x (B) を満たすようになる。A, B に互換性はない。
```

### 例: ポインタ

```rust
generic[T]
interface Ptr for usize;
```

```rust
// ポインタの加算。p にポインタではない usize を渡すことはできない。
generic[T]
fn ptr_add(p: Ptr[T], i: i64) -> Ptr[T] {
    (p as usize) + i * sizeof(T)
}

let p: Ptr[i32];
ptr_add(p, 1); //=> p + 4

let q: Ptr[i64];
ptr_add(q, 1); //=> q + 8
```

### 例: 列挙体

```rust
interface Color for i32 {
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

### 例: 直和型

代数的データ型は union と既定インターフェイスにより実現できる。

```rust
struct JsonParseResult {
    ok: bool
    union {
        value: JsonValue
        syntax_errors: Vec[Error]
    }
} with {
    Ok {
        self.ok @ true
        self.value @ [_, ..]
    }
    Err {
        self.ok @ false
        self.syntax_errors @ [_, ..]
    }
}
```

```rust
fn parse(text: &String) -> ParseResult {
    let mut tokens = [];
    let mut errors = [];

    // 略

    // エラーがあると失敗
    if !Vec::is_empty(errors) {
        return JsonParseResult::Err {
            ok: false
            syntax_errors: move errors
        };
    }

    // 動的検査
    assert(tokens @ [_, ..]);

    // 生成される状態が有効 (Ok) であることはコンパイル時に検査される。
    JsonParseResult::Ok {
        ok: true
        tokens: move tokens
    }
}

fn main() {
    let text = "...";

    parse(text, let r);
    if r.success {
        // ここで r.success = true なので、
        // r が Ok を満たすことがコンパイル時に分かる。
        // そのため r.tokens にアクセスできる。
        println!("OK: {} token(s)\n", r.tokens.len());
    } else {
        println!("ERROR: {} syntax error(s)\n", r.errors.len());
    }
}
```

### 例: データ検証

```rust
generic[S]
interface IsEmpty[S] for bool {
    Empty = true
    NonEmpty = false
}

// fn ... -> impl T は関数の結果を自動で呼び出し側のローカル環境に impl する。
fn is_empty(s: &String) -> impl IsEmpty[%s] {
    s.len() == 0
}

// 変数 s が空でないときだけ呼べる関数
fn first(s: &String)[_: IsEmpty[%s]::NonEmpty] -> char {
    s[0]
}

fn main() {
    let s = "hello";
    if !is_empty(&s) {
        // ここで is_empty の結果として e = impl IsEmpty[%s] が存在し、
        // if 文から条件 e @ false を満たすことが静的に分かる。
        // IsEmpty[%s]::NonEmpty の impl 変数があるので、
        // first を呼べる。
        first(&s); //=> h
    }
}
```

## メモリ管理

C++/Rust のような RAII を備える。メモリ安全性は静的検査や動的検査により保証される。

所有権:

- 数値などの一部の型を除いて、オブジェクトは変数に所有される。
- 変数がスコープを外れる際に、それが所有しているオブジェクトは drop される。

参照:

- オブジェクトへのポインタをとれる。ポインタの操作は安全ではない。
- 参照型 `generic[S, T] interface Ref for Ptr[T]`
    - S は参照先が有効であることを保証するための幽霊型
    - T は参照先の型
    - 実体はポインタ
- ローカル変数への参照
    - 型がその変数自身を指し、その変数が有効でない位置では使用できないことにする。
    - `s: T` → `&s: Ref[%s, T]`
- フィールドへの参照
    - ローカル変数と同様。
    - `&s: S`, `&s.t: T` → `&s.t: Ref[S, T]`
- オブジェクトが所有するデータへの参照
    - オブジェクトごとに異なる型を使って表現する。
    - 例えば、ベクタの要素への参照には `VecRef[S, T]` 型を使う。
    - 具体的には、ベクタへの参照とインデックスのペア
        `generic[T] struct VecRef { v: &Vec[T], i: i64 }`
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
fn leak_to_callee() -> &String {
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

## その他

関数

```rust
generic[T]
struct Monoid {
    empty: T
    append: Fn(T, T) -> T // 存在型？ 関数ポインタのインターフェイス型？
}

static impl Monoid[i32] {
    empty: 0
    append: fn(first: i32, second: i32) {
        first + second
    }
}

generic[T]
fn sum(xs: &Vec[T]) use(monoid: Monoid[T]) -> T {
    let mut sum = monoid.empty;
    for x in xs {
        sum = (monoid.append)(sum, x);
    }
    sum
}
```

参照

```rust
fn bang(mut s: &String) {
    String::push(s, '!');
}

// ↓

generic[S, T]
struct Deref {
    deref: Fn(S) -> Ptr[T]
}

generic[S, T]
static impl Deref[Ref[S, T], T] {
    deref: fn(s: Ref[S, T]) -> Ptr[T] {
        s.inner
    }
}

generic[S, T]
static impl Deref[VecRef[S, T], T] {
    deref: fn(s: VecRef[S, T]) -> Ptr[T] {
        ptr_add(s.v.data, s.i)
    }
}

generic[] use[R]
fn bang(mut s: R) use(d: impl Deref[D, String]) {
    String::push((d.deref)(s), '!');
}
```

## CPS

- [コンパイラの作り方 (詳解)](https://www.is.s.u-tokyo.ac.jp/vu/96/cad/compilerresume/)
