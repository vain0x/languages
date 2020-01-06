int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_main() {
    int result;
    _ = 1;
    goto next;

next:;
    int ok;
    _ = result;
    goto let_next;

let_next:;
    int result_2;
    _ = 0;
    goto next_2;

next_2:;
    int a;
    _ = result_2;
    goto let_next_2;

let_next_2:;
    goto do_continue;

do_continue:;
    int result_5;
    _ = fn_assert_eq(a, a);
    goto next_5;

next_5:;
    int result_6;
    _ = 1;
    goto next_6;

next_6:;
    int result_7;
    _ = a + result_6;
    goto next_7;

next_7:;
    int result_8;
    _ = &a = result_7;
    goto next_8;

next_8:;
    int result_9;
    _ = 5;
    goto next_9;

next_9:;
    int result_10;
    _ = a == result_9;
    goto next_10;

next_10:;
    int res;
    int result_11;
    _ = 0;
    goto next_11;

next_11:;
    int result_12;
    _ = &ok = result_11;
    goto next_12;

next_12:;
    _ = result_12;
    goto if_next;

if_next:;
    goto do_continue;

do_break:;
    int result_3;
    _ = 5;
    goto next_3;

next_3:;
    int result_4;
    _ = fn_assert_eq(a, result_3);
    goto next_4;

next_4:;
    return result_4;
}
