extern int assert_eq(int actual, int expected);

int fn_assert_eq(int actual, int expected) {
    int result = assert_eq(actual, expected);
    return result;
}

extern int print(char* s);

int fn_print(char* s) {
    int result_2 = print(s);
    return result_2;
}

int fn_main() {
    int result_6 = 0 == 0;
    int result_3;
    if (result_6) {
        goto if_body;
    } else {
        goto if_alt;
    }

if_body:;
    int result_4 = fn_print("0 == 0\n");
    result_3 = result_4;
    goto if_next;

if_alt:;
    int result_5 = fn_print("0 != 0\n");
    result_3 = result_5;

if_next:;
    int result_10 = 0 == 1;
    int result_7;
    if (result_10) {
        goto if_body_2;
    } else {
        goto if_alt_2;
    }

if_body_2:;
    int result_8 = fn_print("0 == 1\n");
    result_7 = result_8;
    goto if_next_2;

if_alt_2:;
    int result_9 = fn_print("0 != 1\n");
    result_7 = result_9;

if_next_2:;
    int x = 2;
    int result_17 = x == 1;
    int result_11;
    if (result_17) {
        goto if_body_3;
    } else {
        goto if_alt_3;
    }

if_body_3:;
    int result_12 = fn_print("0 == 1\n");
    result_11 = result_12;
    goto if_next_3;

if_alt_3:;
    int result_16 = x == 2;
    int result_13;
    if (result_16) {
        goto if_body_4;
    } else {
        goto if_alt_4;
    }

if_body_4:;
    int result_14 = fn_print("x == 2\n");
    result_13 = result_14;
    goto if_next_4;

if_alt_4:;
    int result_15 = fn_print("x != 1, x != 2\n");
    result_13 = result_15;

if_next_4:;
    result_11 = result_13;

if_next_3:;
    return result_11;
}
