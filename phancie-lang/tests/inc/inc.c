extern int assert_eq(int actual, int expected);

int fn_assert_eq(int actual, int expected) {
    int result = assert_eq(actual, expected);
    return result;
}

int fn_inc(int* x) {
    int result_2 = x + 1;
    int result_3 = x = result_2;
    return result_3;
}

int fn_main() {
    int a = 1;
    int result_4 = fn_assert_eq(a, 1);
    int result_5 = fn_inc(&a);
    int result_6 = fn_assert_eq(a, 2);
    int result_7 = fn_inc(&a);
    int result_8 = fn_assert_eq(a, 3);
    return result_8;
}
