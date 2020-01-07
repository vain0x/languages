int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_inc(int* x) {
    int result = 1;

next:;
    int result_2 = x + result;

next_2:;
    int result_3 = *x = result_2;

next_3:;
    return result_3;
}

int fn_main() {
    int result_4 = 1;

next_4:;
    int a = result_4;

let_next:;
    int result_5 = 1;

next_5:;
    int result_6 = fn_assert_eq(a, result_5);

next_6:;
    int result_7 = fn_inc(&a);

next_7:;
    int result_8 = 2;

next_8:;
    int result_9 = fn_assert_eq(a, result_8);

next_9:;
    int result_10 = fn_inc(&a);

next_10:;
    int result_11 = 3;

next_11:;
    int result_12 = fn_assert_eq(a, result_11);

next_12:;
    return result_12;
}
