#![cfg(test)]

use picomet_lang_compiler::tests::{eval_tests, test_err};

#[test]
fn test_hello() {
    eval_tests(
        r#"
            println_int(42)
        "#,
        &[("", "42\n")],
    )
}

#[test]
fn test_unit_literal() {
    eval_tests(r#"(); (())"#, &[("", "")]);

    test_err("() + 0", "At 1:1..1:3 Type Error");
}

#[test]
fn test_trailing_semicolons_discard_result() {
    test_err(r#"if 0 == 0 { 0; } else { 0 }"#, "At 1:25..1:26 Type Error");
}

#[test]
fn test_syntax_error_unexpected_eof() {
    test_err(r#"1 ++"#, "At 1:3..1:5 Expected EOF");
}

#[test]
fn test_arithmetic() {
    eval_tests(
        r#"
            println_int(1 + 2 * 3)
        "#,
        &[("", "7\n")],
    )
}

#[test]
fn test_prefix_parsing() {
    eval_tests(
        r#"
            println_int(-1)
        "#,
        &[("", "-1\n")],
    )
}

#[test]
fn test_comparison() {
    eval_tests(
        r#"
            let l = read_int();
            let r = read_int();
            println_int(if l == r { 1 } else { 0 });
            println_int(if l != r { 1 } else { 0 });
            println_int(if l < r { 1 } else { 0 });
            println_int(if l <= r { 1 } else { 0 });
            println_int(if l > r { 1 } else { 0 });
            println_int(if l >= r { 1 } else { 0 });
        "#,
        &[
            ("1\n1\n", "1\n0\n0\n1\n0\n1\n"),
            ("1\n2\n", "0\n1\n1\n1\n0\n0\n"),
        ],
    )
}

#[test]
fn test_arithmetic_type_error() {
    test_err(
        "1 + (while 0 == 0 {})",
        r#"
            At 1:6..1:21 Type Error
        "#,
    )
}

#[test]
fn test_bitwise_operation() {
    eval_tests(
        r#"
            println_int(12 & 10);
            println_int(12 | 10);
            println_int(12 ^ 10);
        "#,
        &[("", "8\n14\n6\n")],
    );
}

#[test]
fn test_str() {
    eval_tests(
        r#"
            let i = read_int();
            println_int(byte_to_int("Hello, world!"[i]))
        "#,
        &[("0", "72\n"), ("1", "101\n")],
    )
}

#[test]
fn test_str_literal_escape_sequence() {
    eval_tests(
        r#"
            print("Hello, world!\n");
        "#,
        &[("", "Hello, world!\n")],
    );
}

#[test]
fn test_char_literal_escape_sequence() {
    eval_tests(
        r#"
            println_int(byte_to_int('A'));
            println_int(byte_to_int('\0'));
            println_int(byte_to_int('\n'));
            println_int(byte_to_int('\r'));
            println_int(byte_to_int('\''));
        "#,
        &[("", "65\n0\n10\n13\n39\n")],
    );
}

#[test]
fn test_char_literal_errors() {
    test_err("'", "At 1:1..1:2 Single quote missing");

    test_err("'\n", "At 1:1..1:2 Single quote missing");

    test_err("''", "At 1:1..1:3 Expected exactly one ASCII character");

    test_err(
        r#"'😄'"#,
        "At 1:1..1:7 Expected exactly one ASCII character",
    );

    // HELP: Edge case of poor message.
    test_err(r#"'\'"#, "At 1:1..1:4 Unknown escape sequence")
}

#[test]
fn test_write() {
    eval_tests(
        r#"
            let p = mem_alloc(2);
            p[0] = "A"[0];
            p[1] = int_to_byte(10);
            print(p);
        "#,
        &[("", "A\n")],
    )
}

#[test]
fn test_slice_len() {
    eval_tests(
        r#"
            let s = "Hello!";
            println_int(slice_len(s));
        "#,
        &[("", "6\n")],
    );

    eval_tests(
        r#"
            let xs = mem_alloc(1);
            xs[0] = 0;
            println_int(slice_len(xs));
        "#,
        &[("", "1\n")],
    );
}

#[test]
fn test_slice_subslice() {
    eval_tests(
        r#"
            print("Happy"[1..1 + 3]);
            print("\n");
        "#,
        &[("", "app\n")],
    );

    test_err("0..0", "At 1:1..1:5 Invalid use of range");
}

#[test]
fn test_slice_of_int() {
    eval_tests(
        r#"
            let buf = mem_alloc(2);
            buf[0] = 1000000000000000000;
            buf[1] = 2000000000000000000;
            println_int(buf[0]);
            println_int(buf[1]);
        "#,
        &[("", "1000000000000000000\n2000000000000000000\n")],
    );
}

#[test]
fn test_slice_of_int_subslice() {
    eval_tests(
        r#"
            let buf = mem_alloc(3);
            let sub = buf[1..2];
            sub[1] = 2000;
            println_int(buf[2]);
        "#,
        &[("", "2000\n")],
    );
}

#[test]
fn test_read_int() {
    eval_tests(
        r#"
            println_int(read_int() + 1)
        "#,
        &[("41", "42\n"), ("-1", "0\n")],
    );

    test_err(r#"println_int()"#, "At 1:1..1:12 Type Error");
}

#[test]
fn test_read_str() {
    eval_tests(
        r#"
            let s = read_str();
            print(s);
        "#,
        &[("hello", "hello"), ("a", "a")],
    );
}

#[test]
fn test_local_var() {
    eval_tests(
        r#"
            let a = 1
            println_int(a)
        "#,
        &[("", "1\n")],
    )
}

#[test]
fn test_local_var_two() {
    eval_tests(
        r#"
            let a = 1;
            let b = 2;
            println_int(a);
            println_int(b);
        "#,
        &[("", "1\n2\n")],
    )
}

#[test]
fn test_local_var_shadowing() {
    eval_tests(
        r#"
            let a = 1;
            let a = {
                println_int(a);
                2
            };
            println_int(a);
        "#,
        &[("", "1\n2\n")],
    )
}

#[test]
fn test_local_var_set() {
    eval_tests(
        r#"
            let a = 1;
            a = a + 1;
            println_int(a);
        "#,
        &[("", "2\n")],
    )
}

#[test]
fn test_local_var_set_add() {
    eval_tests(
        r#"
            let a = 1;
            a += 1;
            println_int(a);
        "#,
        &[("", "2\n")],
    )
}

#[test]
fn test_local_var_set_etc() {
    eval_tests(
        r#"
            let a = 10;
            a += 1;
            println_int(a); // 11
            a -= 2;
            println_int(a); // 9
            a *= 3;
            println_int(a); // 27
            a /= 4;
            println_int(a); // 6
            a %= 5;
            println_int(a); // 1
        "#,
        &[("", "11\n9\n27\n6\n1\n")],
    );
}

#[test]
fn test_local_var_with_type_annotation() {
    eval_tests(
        r#"
            let u: unit = ();
            let b: byte = 'b';
            let n: int = 1;
            let s: _ = || ();
        "#,
        &[("", "")],
    );

    test_err("let x: unit = 1", "At 1:15..1:16 Type Error");
}

#[test]
fn test_if() {
    eval_tests(
        r#"
            println_int(if read_int() == 0 {
                42
            } else {
                -1
            })
        "#,
        &[("0", "42\n"), ("1", "-1\n")],
    )
}

#[test]
fn test_if_else_if_chain() {
    eval_tests(
        r#"
            let x = read_int();
            println_int(if x == 1 {
                -1
            } else if x == 2 {
                -2
            } else {
                x
            })
        "#,
        &[("1", "-1\n"), ("2", "-2\n"), ("3", "3\n")],
    )
}

#[test]
fn test_log_or() {
    eval_tests(
        r#"
            let x = read_int();
            println_int(if x == 0 || 4 % x == 0 { 1 } else { 0 })
        "#,
        &[("0\n", "1\n"), ("2\n", "1\n"), ("3\n", "0\n")],
    )
}

#[test]
fn test_log_and() {
    eval_tests(
        r#"
            let x = read_int();
            println_int(if x != 0 && 4 % x == 0 { 1 } else { 0 })
        "#,
        &[("0\n", "0\n"), ("2\n", "1\n"), ("3\n", "0\n")],
    )
}

#[test]
fn test_while() {
    eval_tests(
        r#"
            while read_int() == 0 {
                println_int(1)
            }
        "#,
        &[("0\n0\n0\n4\n0\n", "1\n1\n1\n"), ("1\n", "")],
    )
}

#[test]
fn test_break() {
    eval_tests(
        r#"
            let i = 0;
            while i < 4 {
                i += 1;
                if i == 3 { break }
                println_int(i);
            }
        "#,
        &[("", "1\n2\n")],
    );

    test_err("break", "At 1:1..1:6 Out of loop");

    test_err(
        r#"
            while 0 != 0 {
                let f = || break;
            }
        "#,
        "At 3:28..3:33 Out of loop",
    )
}

#[test]
fn test_continue() {
    eval_tests(
        r#"
            let i = 0;
            while i < 4 {
                i += 1;
                if i == 2 { continue }
                println_int(i);
            }
        "#,
        &[("", "1\n3\n4\n")],
    );

    test_err("continue", "At 1:1..1:9 Out of loop");

    test_err(
        r#"
            while 0 != 0 {
                let f = || continue;
            }
        "#,
        "At 3:28..3:36 Out of loop",
    );
}

#[test]
fn test_continue_in_nested_loop() {
    eval_tests(r#""#, &[("", "")])
}

#[test]
fn test_fun_with_no_args() {
    eval_tests(
        r#"
            let f = || 2;
            println_int(84 / f());

            let g = | | 2;
            println_int(84 / g());
        "#,
        &[("", "42\n42\n")],
    )
}

#[test]
fn test_fun_with_arg() {
    eval_tests(
        r#"
            let half = |x| x / 2;
            println_int(half(84));
        "#,
        &[("", "42\n")],
    )
}

#[test]
fn test_fun_with_args() {
    eval_tests(
        r#"
            let div = |x, y| x / y;
            println_int(div(126, 3));
        "#,
        &[("", "42\n")],
    )
}

#[test]
fn test_fun_with_args_and_locals() {
    eval_tests(
        r#"
            let digit_sum = |x| {
                let s = 0;
                while x > 0 {
                    s += x % 10;
                    x = x / 10;
                }
                s
            };
            println_int(digit_sum(314159));
        "#,
        &[("", "23\n")],
    )
}

#[test]
fn test_fun_to_modify_globals() {
    eval_tests(
        r#"
            let x = 0;
            let f = || x += 1;
            f();
            println_int(x);
        "#,
        &[("", "1\n")],
    )
}

#[test]
fn test_fun_return() {
    eval_tests(
        r#"
            let f = |x| {
                if x % 2 == 0 {
                    return x / 2;
                }
                x * 3 + 1
            };
            println_int(1 + f(1) + 1);
            println_int(4 + f(4) + 4);
            if 0 == 0 { return }
            println_int(-1)
        "#,
        &[("", "6\n10\n")],
    )
}

#[test]
fn test_fun_return_same_type() {
    test_err(
        r#"
            let f = |x| {
                if x == 0 { return }
                0
            };
            f(0);
        "#,
        "At 4:17..4:18 Type Error",
    )
}

#[test]
fn test_fun_rec() {
    eval_tests(
        r#"
            let rec fact = |x| if x <= 1 {
                1
            } else {
                x * fact(x - 1)
            };
            println_int(fact(read_int()));
        "#,
        &[("1", "1\n"), ("5", "120\n")],
    );
}

#[test]
fn test_fun_rec_mutually() {
    eval_tests(
        r#"
            let rec even = |x| if x > 0 { odd(x - 1) } else { 1 };
            let rec odd = |x| if x > 0 { even(x - 1) } else { 0 };
            println_int(even(8));
        "#,
        &[("", "1\n")],
    )
}

#[test]
fn test_fun_shadowing() {
    eval_tests(
        r#"
            let f = || 2;
            println_int(f());
            let f = || 3 * f();
            println_int(f());
        "#,
        &[("", "2\n6\n")],
    )
}

#[test]
fn test_fun_rec_allow_name_conflict() {
    eval_tests(
        r#"
            println_int(f());
            let rec f = || 1;
            println_int(f());
            let rec f = || 2;
            println_int(f());
        "#,
        &[("", "2\n2\n2\n")],
    );
}

#[cfg(skip)]
#[test]
fn test_fun_generic() {
    eval_tests(
        r#"
            let f = |x| byte_to_int(x);
            let id = |x| x;
            println_int(if id(0) == 0 { 1 } else { 0 });
            println_int(if f(id('a')) == f('a') { 1 } else { 0 });
        "#,
        &[("", "1\n1\n")],
    );

    test_err(
        r#"
            let f = |x| x + 1;
            f("");
        "#,
        "At 3:15..3:17 Type Error",
    );
}

#[test]
fn test_type_annotations() {
    eval_tests(
        r#"
            // In expression.
            println_int(2: int + 3); //=> 5

            // In reference.
            let x = 2;
            x: int += 4;
            println_int(x); //=> 6

            // Unify type variable.
            let id = |x| x: int;
            println_int(id(3) + 4); //=> 7

            // On local variable;
            let x: int = 8;
            println_int(x); //=> 8
        "#,
        &[("", "5\n6\n7\n8\n")],
    )
}

#[test]
fn test_casts() {
    eval_tests(
        r#"
            // byte to int
            println_int('A' :> int);

            // int to byte
            let s = mem_alloc(1);
            s[0] = 65 :> byte;
            println_str(s);
        "#,
        &[("", "65\nA\n")],
    )
}

static STDLIB: &str = r#"
    let DIGIT_CHARS = "0123456789";
    let HYPHEN_CHAR = "-"[0];
    let calculate_digit_len = |x| {
        if x == 0 {
            1
        } else {
            let len = 0;
            if x < 0 {
                x = -x;
                len += 1;
            }
            while x > 0 {
                len += 1;
                x = x / 10;
            }
            len
        }
    };
    let int_to_str = |x| {
        let n = calculate_digit_len(x);
        let s = mem_alloc(n);
        if x == 0 {
            s[0] = DIGIT_CHARS[0];
        } else {
            if x < 0 {
                s[0] = HYPHEN_CHAR;
                x = -x;
            }
            while x > 0 {
                n = n - 1;
                s[n] = DIGIT_CHARS[x % 10];
                x = x / 10;
            }
        }
        s
    };
"#;

#[test]
fn test_int_to_str() {
    eval_tests(
        &format!(
            r#"
            {}
            print(int_to_str(read_int()));
            print("\n");
        "#,
            STDLIB
        ),
        &[("314159", "314159\n"), ("0", "0\n"), ("-42", "-42\n")],
    )
}

#[test]
fn test_use_prelude() {
    eval_tests(
        r#"println_str("Hello, world!");"#,
        &[("", "Hello, world!\n")],
    );
}

// Example in the README.
#[test]
fn test_example_fact() {
    eval_tests(
        r#"
            let fact = |n| {
                let x = 1;
                while n >= 2 {
                    x = x * n;
                    n = n - 1;
                }
                x
            };
            println_int(fact(3)); //=> 6
        "#,
        &[("", "6\n")],
    )
}

#[test]
fn test_example_succ() {
    eval_tests(
        r#"
            let succ = |x| x + 1;
            println_int(succ(1)); //=> 2
        "#,
        &[("", "2\n")],
    );
}

/// <https://atcoder.jp/contests/abs/tasks/practice_1?lang=en>
#[test]
fn test_welcome_to_atcoder() {
    eval_tests(
        &format!(
            r#"
            {}
            let a = read_int();
            let b = read_int();
            let c = read_int();
            let s = read_str();
            print(int_to_str(a + b + c));
            print(" ");
            print(s);
            print("\n");
        "#,
            STDLIB
        ),
        &[
            ("1\n2 3\ntest\n", "6 test\n"),
            ("72\n128 256\nmyonmyon\n", "456 myonmyon\n"),
        ],
    )
}

/// <https://atcoder.jp/contests/abs/tasks/abc086_a?lang=en>
#[test]
fn test_abc086_a_product() {
    eval_tests(
        r#"
            let a = read_int();
            let b = read_int();
            print(if a * b % 2 == 0 { "Even\n" } else { "Odd\n" });
        "#,
        &[("3 4\n", "Even\n"), ("1 21\n", "Odd\n")],
    )
}

/// <https://atcoder.jp/contests/abs/tasks/abc081_a?lang=en>
#[test]
fn test_abc081_a_placing_marbles() {
    eval_tests(
        r#"
            let n = read_int();
            let k = 0;
            while n > 0 {
                if n % 10 == 1 {
                    k += 1;
                }
                n = n / 10;
            }
            println_int(k);
        "#,
        &[
            ("101\n", "2\n"),
            ("000\n", "0\n"),
            ("111\n", "3\n"),
            ("010\n", "1\n"),
        ],
    )
}

/// <https://atcoder.jp/contests/abs/tasks/abc081_b?lang=en>
#[test]
fn test_abc081_b_shift_only() {
    eval_tests(
        r#"
            let N = read_int();
            let min_count = 1000000000;
            let i = 0;
            while i < N {
                let A = read_int();
                let count = 0;
                while A % 2 == 0 {
                    A = A / 2;
                    count += 1;
                }
                if min_count > count {
                    min_count = count;
                }
                i += 1;
            }
            println_int(min_count);
        "#,
        &[("3\n8 12 40\n", "2\n"), ("4\n5 6 8 10\n", "0\n")],
    )
}

/// <https://atcoder.jp/contests/abs/tasks/abc087_b?lang=en>
#[test]
fn test_abc087_b_coins() {
    eval_tests(
        r#"
            let A = read_int();
            let B = read_int();
            let C = read_int();
            let X = read_int();
            let k = 0;
            let a = 0;
            while a <= A {
                let b = 0;
                while b <= B {
                    let Y = X - a * 500;
                    Y = Y - b * 100;
                    if Y >= 0 {
                        let c = Y / 50;
                        if c <= C {
                            k += 1;
                        }
                    }
                    b += 1;
                }
                a += 1;
            }
            println_int(k);
        "#,
        &[
            ("2\n2\n2\n100\n", "2\n"),
            ("5\n1\n0\n150\n", "0\n"),
            ("30\n40\n50\n6000\n", "213\n"),
        ],
    )
}

/// <https://atcoder.jp/contests/abs/tasks/abc083_b>
#[test]
fn test_abc083_b_some_sums() {
    eval_tests(
        r#"
            let digit_sum = |x| {
                let s = 0;
                while x > 0 {
                    s += x % 10;
                    x = x / 10; // FIXME: missing /=
                }
                s
            };

            let N = read_int();
            let A = read_int();
            let B = read_int();

            let k = 0;
            let x = 1;
            while x <= N {
                let s = digit_sum(x);
                if A <= s {
                    if s <= B { // FIXME: missing &&
                        k += x;
                    }
                }
                x += 1;
            }
            println_int(k);
        "#,
        &[
            ("20 2 5", "84\n"),
            ("10 1 2", "13\n"),
            ("100 4 16", "4554\n"),
        ],
    )
}

/// <https://atcoder.jp/contests/abs/tasks/abc088_b>
#[test]
fn test_abc088_b_card_game_for_two() {
    eval_tests(
        r#"
            let vi_swap = |xs, i, j| {
                let t = xs[i] + 0;
                xs[i] = xs[j];
                xs[j] = t;
            };

            /// Push to binary heap. Slice must have room for item.
            let vi_heap_push = |xs, len, x| {
                xs[len] = x + 0;

                let i = len;
                while i > 0 {
                    let p = (i - 1) / 2;
                    if xs[i] < xs[p] {
                        vi_swap(xs, i, p);
                    }
                    i = p;
                }
            };

            /// Pop from binary heap.
            let vi_heap_pop = |xs, len| {
                // assert len >= 1

                let x = xs[0] + 0;
                let t = xs[len - 1];

                xs[0] = t;
                len += -1;

                let i = 0;
                while 0 == 0 {
                    let l = i * 2 + 1;
                    let r = i * 2 + 2;
                    if r < len {
                        if xs[l] > xs[r] {
                            l = r;
                        }
                    }
                    if l >= len {
                        break;
                    }
                    if xs[l] < xs[i] {
                        vi_swap(xs, i, l);
                    }
                    i = l;
                }

                x
            };

            let vi_sort = |xs, len| {
                let ys = mem_alloc(len);
                let yn = 0;
                while yn < len {
                    vi_heap_push(ys, yn, xs[yn]);
                    yn += 1;
                }

                while yn > 0 {
                    xs[len - yn] = vi_heap_pop(ys, yn);
                    yn += -1;
                }
            };

            let N = read_int();
            let A = mem_alloc(N);

            let i = 0;
            while i < N {
                A[i] = read_int();
                i += 1;
            }

            vi_sort(A, N);

            let alice = 0;
            let bob = 0;
            let i = N - 1;
            while i >= 0 {
                alice += A[i];
                i += -1;

                if i >= 0 {
                    bob += A[i];
                    i += -1;
                }
            }

            println_int(alice - bob);
        "#,
        &[
            ("2\n3 1\n", "2\n"),
            ("3\n2 7 4", "5\n"),
            ("4\n20 18 2 18", "18\n"),
            ("7\n3 1 4 1 5 9 2", "7\n"),
        ],
    )
}
