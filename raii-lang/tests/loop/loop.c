int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_print(char* s) {
    return print(s);
}

int fn_main() {
    char* result;
    _ = "before loop";
    goto next;

next:;
    int result_2;
    _ = fn_print(result);
    goto next_2;

next_2:;
    int result_3;
    _ = 0;
    goto next_3;

next_3:;
    int a;
    _ = result_3;
    goto let_next;

let_next:;
    goto do_continue;

do_continue:;
    int result_6;
    _ = fn_assert_eq(a, a);
    goto next_6;

next_6:;
    int result_7;
    _ = 5;
    goto next_7;

next_7:;
    int result_8;
    _ = a == result_7;
    goto next_8;

next_8:;
    int res;
    goto do_break;

if_next:;
    int result_9;
    _ = 2;
    goto next_9;

next_9:;
    int result_10;
    _ = a == result_9;
    goto next_10;

next_10:;
    int res_2;
    int result_14;
    _ = 2;
    goto next_14;

next_14:;
    int result_15;
    _ = a + result_14;
    goto next_15;

next_15:;
    int result_16;
    _ = &a = result_15;
    goto next_16;

next_16:;
    goto do_continue;

if_next_2:;
    int result_11;
    _ = 1;
    goto next_11;

next_11:;
    int result_12;
    _ = a + result_11;
    goto next_12;

next_12:;
    int result_13;
    _ = &a = result_12;
    goto next_13;

next_13:;
    goto do_continue;

do_break:;
    char* result_4;
    _ = "after loop";
    goto next_4;

next_4:;
    int result_5;
    _ = fn_print(result_4);
    goto next_5;

next_5:;
    return result_5;
}
