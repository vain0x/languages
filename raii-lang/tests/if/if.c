extern int assert_eq(int actual, int expected);

int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

extern int print(char* s);

int fn_print(char* s) {
    return print(s);
}

int fn_main() {
    int result_5 = 0;

next_5:;
    int result_6 = 0;

next_6:;
    int result_7 = result_5 == result_6;

next_7:;
    int res;
    if (result_7) {
        goto if_body;
    } else {
        goto if_alt;
    }

if_body:;
    char* result = "0 == 0\n";

next:;
    int result_2 = fn_print(result);

next_2:;
    res = result_2;
    goto if_next;

if_alt:;
    char* result_3 = "0 != 0\n";

next_3:;
    int result_4 = fn_print(result_3);

next_4:;
    res = result_4;

if_next:;
    int result_12 = 0;

next_12:;
    int result_13 = 1;

next_13:;
    int result_14 = result_12 == result_13;

next_14:;
    int res_2;
    if (result_14) {
        goto if_body_2;
    } else {
        goto if_alt_2;
    }

if_body_2:;
    char* result_8 = "0 == 1\n";

next_8:;
    int result_9 = fn_print(result_8);

next_9:;
    res_2 = result_9;
    goto if_next_2;

if_alt_2:;
    char* result_10 = "0 != 1\n";

next_10:;
    int result_11 = fn_print(result_10);

next_11:;
    res_2 = result_11;

if_next_2:;
    int result_15 = 2;

next_15:;
    int x = result_15;

let_next:;
    int result_24 = 1;

next_24:;
    int result_25 = x == result_24;

next_25:;
    int res_3;
    if (result_25) {
        goto if_body_3;
    } else {
        goto if_alt_3;
    }

if_body_3:;
    char* result_16 = "0 == 1\n";

next_16:;
    int result_17 = fn_print(result_16);

next_17:;
    res_3 = result_17;
    goto if_next_3;

if_alt_3:;
    int result_22 = 2;

next_22:;
    int result_23 = x == result_22;

next_23:;
    int res_4;
    if (result_23) {
        goto if_body_4;
    } else {
        goto if_alt_4;
    }

if_body_4:;
    char* result_18 = "x == 2\n";

next_18:;
    int result_19 = fn_print(result_18);

next_19:;
    res_4 = result_19;
    goto if_next_4;

if_alt_4:;
    char* result_20 = "x != 1, x != 2\n";

next_20:;
    int result_21 = fn_print(result_20);

next_21:;
    res_4 = result_21;

if_next_4:;
    res_3 = res_4;

if_next_3:;
    return res_3;
}
