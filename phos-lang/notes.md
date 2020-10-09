# メモ

静的型つけ + 動的なインタプリタ

## 値

値はプリミティブ型 (文字列、整数、小数)、オブジェクト、セルの3通り。

文字列: str 型 (utf-8)

```
    s = "hello"
```

整数: int 型 (64-bit signed)

```
    n = 42
    m = -1
```

小数: float 型 (64-bit)

```
    d = 3.14
```

- 整数は小数にアップキャストできる。(int <: float)

### オブジェクト

オブジェクトは構造を持ち、タグとフィールドの並びからなる。

```
    Point(x:float, y:float)
```

- タグは文字列で、オブジェクトの種類を区別するもの。先頭は大文字。
- フィールドはオブジェクトが持つデータ。それぞれ名前があってもなくてもよい。

同値性:

- 構造的な同値性を備える。
- オブジェクトに関する l ~< r の関係は次のように検査する。(l ~< r かつ r ~< l のとき l, r は同値とみなす。)
    - タグが等しくなければ NG.
    - l が持つ名前なしフィールドが、それぞれ r の同じ位置のフィールドに対して ~< の関係を持つことを検査する。
    - l が持つ名前つきフィールドが、それぞれ r にある同名のフィールドに対して ~< の関係を持つことを検査する。
    - l の名前つきフィールドの前方にあって r には対応する名前のフィールドがなかったものに、r の余剰な名前なしフィールドを順番に対応させていき、それらの ~< の関係を検査する。
    - (TODO: 可変個のフィールドがあるケース)

例:

- `Point(x:int, y:int) ~< Point(x:int, y:int)` 左右とも同じなので OK
- `Point(x:int, y:int) ~< Point(y:int, x:int)` フィールドの順番が違うだけなので OK
- `Point(x:int, y:int) ~< Point(  int,   int)` 右の名前なしのフィールドをそれぞれ x, y という名前を持つフィールドとみなせばよいので OK
    - これは例えば分解代入 `(s, t) = Point(x: 1, y: 2)` で発生する。(右辺はフィールドに名前があるが、左辺はない。)

可変個のフィールドを持つオブジェクトは、末尾に `...T` を付与する (T はそれらのフィールドの型の上界)。例えば前から2つが string で残りが int なら `K(string, string, ...int)` と書く。

### 特殊なオブジェクト

以下の特殊なオブジェクトが組み込みで定義される。

```
    null = Err(null)
    true = Ok(true)
    false = Err(false)
```

null, true, false の型はそれぞれ null, true, false と書ける。型 `bool = true | false` が定義される。型エイリアス `Result[T, E] = Ok(T) | Err(E)` と `Option[T] = Ok(T) | false` が定義される。

配列リテラルは Array オブジェクトで表現される。

```
    a = [2, 3, 5]
    # a: Array(2, 3, 5)
```

配列の型は `Array(int, int, int)` や `Array(...int)` などと書ける。また、`Array(...T)` は `[]T` と略記できる。

### 参照セル

参照セルは可変な状態を持ち、状態を共有できる。参照的な同値性を備える。

新しいセルを生成するには ref 式を使う。セルが入っているオブジェクトからセル自体を値として取るには share 式を使う。他の式がセル型になるときはセルが持つ値を参照する。

```
    # 生成
    # r: ref int
    r = ref 0

    # 更新
    r = 2
    r += 3
    r           #=> 5

    # 共有
    s = share r   # s: ref int
    s *= 2
    r           #=> 10 (s と r はセルを共有しているため)
```

```
    a = A(n: ref 0)

    # 更新
    a.n = 5
    a.n         #=> 5

    # セルを取り出して他のフィールドに持たせる (b.n は a.n への参照になる)
    b = B(n: share a.n)
    b.n         #=> 5
    b.n *= 2
    b.n         #=> 10
    a.n         #=> 10
```

なお、セルを使わなくてもローカル変数は状態を変更できる。フィールドは変更できない。

```
    # x: int
    x = 0
    x += 1
    x           #=> 1
    share x     # error (x はセルではないため)

    a = A(n: 0)
    a.n += 1    # error (a.n はセルではないため)
    share a.n   # error (a.n はセルではないため)

    # 非破壊更新ができる。
    a = A(n: a.n + 1)
```

## 型

値は対応する型を持つ。

それ以外に unknown, never, 交差型、合併型、シグネチャがある。(予定)

## 変数

宣言はいらない。

```
    x = "hello"
```

未定義の変数の使用はコンパイル時に警告、実行時にエラー。

## 分岐: if

条件が Ok(x) なら body、Err(e) なら alt を評価する。それ以外の値ならエラー。

```
    if cond     # 改行しないときは if cond then body ...
        body
    else
        alt
    end
```

## 分岐: match

```
type Expr = Int(int) | Add(Expr, Expr)

fn eval expr:Expr to int
    match expr
    when Int(value)         # 改行しないときは when pat do body
        value

    when Add(l, r)
        eval l + eval r
```

## ループ: loop

無限ループ

```
    loop
        body
    end
```

body の中では break, continue が使える。

## ループ: while

```
    while cond          # 改行しないときは while cond do body
        body
    end
#=>
    loop
        if cond
            body
        else
            break
```

## ループ: for

整数の範囲によるループ:

```
    for i in 0..n  # 改行しないときは for x in 0..n do body end
        body i
    end
#=>
    i = 0
    while i < n
        body i
        i += 1
```

イテレータによるループ:

```
    for x in iterable
        body x
    end
#=>
    iter = iterable.to_iter
    loop
        match iter.advance
        when Ok(x)
            body x
        when Err(_)
            break
```

## 関数

型注釈は省略可。

```
fn add x:int, y:int to int
    x + y
end

add 2, 3 #=> 5
```

fn などのトップレベルキーワードは開いているブロック (fn, if など) をすべて閉じる。

```
fn f
    if cond
        foo

# 次の fn があるので fn, if は自動で閉じる

fn g
    goo
```

if のブロックの中に fn を書きたかったら local キーワードを使う。(local fn はトップレベルキーワードとはみなされず、ブロックが閉じない。)

```
    if cond
        local fn aux
            foo
        end
        aux
```

## メソッド

名前とシグネチャが一致したら呼ばれる。

```
type ArrayIter[T] = ArrayIter(a:[]T, i:ref int)

# 配列を for で回すとき
method to_iter[T] array:[]T to ArrayIter[T]
    ArrayIter(a:array, i:ref 0)

method advance[T] iter:ArrayIter[T] to Option[T]
    i = iter.i
    iter.i += 1

    if i < iter.a.len
        Ok(iter.a[i], i)
    else
        false
```

```
    a = [2, 3, 5]

    # シグネチャが合致する method to_iter が呼ばれる
    iter = a.to_iter
```
