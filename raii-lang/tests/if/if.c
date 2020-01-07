int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_print(char* s) {
    return print(s);
}

int fn_main() {
    int result_5;
    result_5 = 0;
    goto next_5;

next_5:;
    int result_6;
    result_6 = 0;
    goto next_6;

next_6:;
    int result_7;
    result_7 = result_5 == result_6;
    goto next_7;

next_7:;
    int res;
    if (result_7) {
        goto if_body;
    } else {
        goto if_alt;
    }

if_body:;
    char* result;
    result = "0 == 0";
    goto next;

next:;
    int result_2;
    result_2 = fn_print(result);
    goto next_2;

next_2:;
    res = result_2;
    goto if_next;

if_alt:;
    char* result_3;
    result_3 = "0 != 0";
    goto next_3;

next_3:;
    int result_4;
    result_4 = fn_print(result_3);
    goto next_4;

next_4:;
    res = result_4;
    goto if_next;

if_next:;
    int result_12;
    result_12 = 0;
    goto next_12;

next_12:;
    int result_13;
    result_13 = 1;
    goto next_13;

next_13:;
    int result_14;
    result_14 = result_12 == result_13;
    goto next_14;

next_14:;
    int res_2;
    if (result_14) {
        goto if_body_2;
    } else {
        goto if_alt_2;
    }

if_body_2:;
    char* result_8;
    result_8 = "0 == 1";
    goto next_8;

next_8:;
    int result_9;
    result_9 = fn_print(result_8);
    goto next_9;

next_9:;
    res_2 = result_9;
    goto if_next_2;

if_alt_2:;
    char* result_10;
    result_10 = "0 != 1";
    goto next_10;

next_10:;
    int result_11;
    result_11 = fn_print(result_10);
    goto next_11;

next_11:;
    res_2 = result_11;
    goto if_next_2;

if_next_2:;
    int result_15;
    result_15 = 2;
    goto next_15;

next_15:;
    int x;
    x = result_15;
    goto let_next;

let_next:;
    int result_24;
    result_24 = 1;
    goto next_24;

next_24:;
    int result_25;
    result_25 = x == result_24;
    goto next_25;

next_25:;
    int res_3;
    if (result_25) {
        goto if_body_3;
    } else {
        goto if_alt_3;
    }

if_body_3:;
    char* result_16;
    result_16 = "0 == 1";
    goto next_16;

next_16:;
    int result_17;
    result_17 = fn_print(result_16);
    goto next_17;

next_17:;
    res_3 = result_17;
    goto if_next_3;

if_alt_3:;
    int result_22;
    result_22 = 2;
    goto next_22;

next_22:;
    int result_23;
    result_23 = x == result_22;
    goto next_23;

next_23:;
    int res_4;
    if (result_23) {
        goto if_body_4;
    } else {
        goto if_alt_4;
    }

if_body_4:;
    char* result_18;
    result_18 = "x == 2";
    goto next_18;

next_18:;
    int result_19;
    result_19 = fn_print(result_18);
    goto next_19;

next_19:;
    res_4 = result_19;
    goto if_next_4;

if_alt_4:;
    char* result_20;
    result_20 = "x != 1, x != 2";
    goto next_20;

next_20:;
    int result_21;
    result_21 = fn_print(result_20);
    goto next_21;

next_21:;
    res_4 = result_21;
    goto if_next_4;

if_next_4:;
    res_3 = res_4;
    goto if_next_3;

if_next_3:;
    return res_3;
}
