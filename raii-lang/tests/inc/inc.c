int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_inc(int* x) {
    int result;
    result = 1;
    goto next;

next:;
    int result_2;
    result_2 = x + result;
    goto next_2;

next_2:;
    int result_3;
    result_3 = &x = result_2;
    goto next_3;

next_3:;
    return result_3;
}

int fn_main() {
    int result_4;
    result_4 = 1;
    goto next_4;

next_4:;
    int a;
    a = result_4;
    goto let_next;

let_next:;
    int result_5;
    result_5 = 1;
    goto next_5;

next_5:;
    int result_6;
    result_6 = fn_assert_eq(a, result_5);
    goto next_6;

next_6:;
    int result_7;
    result_7 = fn_inc(&a);
    goto next_7;

next_7:;
    int result_8;
    result_8 = 2;
    goto next_8;

next_8:;
    int result_9;
    result_9 = fn_assert_eq(a, result_8);
    goto next_9;

next_9:;
    int result_10;
    result_10 = fn_inc(&a);
    goto next_10;

next_10:;
    int result_11;
    result_11 = 3;
    goto next_11;

next_11:;
    int result_12;
    result_12 = fn_assert_eq(a, result_11);
    goto next_12;

next_12:;
    return result_12;
}
