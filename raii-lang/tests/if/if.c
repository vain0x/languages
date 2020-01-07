int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_print(char* s) {
    return print(s);
}

int fn_main() {
    int result;
    result = 0;
    goto next;

next:;
    int result_2;
    result_2 = 0;
    goto next_2;

next_2:;
    int result_3;
    result_3 = result == result_2;
    goto next_3;

next_3:;
    int res;
    char* result_22;
    result_22 = "0 == 0";
    goto next_22;

next_22:;
    int result_23;
    result_23 = fn_print(result_22);
    goto next_23;

next_23:;
    res = result_23;
    goto if_next;

if_next:;
    int result_4;
    result_4 = 0;
    goto next_4;

next_4:;
    int result_5;
    result_5 = 1;
    goto next_5;

next_5:;
    int result_6;
    result_6 = result_4 == result_5;
    goto next_6;

next_6:;
    int res_2;
    char* result_18;
    result_18 = "0 == 1";
    goto next_18;

next_18:;
    int result_19;
    result_19 = fn_print(result_18);
    goto next_19;

next_19:;
    res_2 = result_19;
    goto if_next_2;

if_next_2:;
    int result_7;
    result_7 = 2;
    goto next_7;

next_7:;
    int x;
    x = result_7;
    goto let_next;

let_next:;
    int result_8;
    result_8 = 1;
    goto next_8;

next_8:;
    int result_9;
    result_9 = x == result_8;
    goto next_9;

next_9:;
    int res_3;
    char* result_10;
    result_10 = "0 == 1";
    goto next_10;

next_10:;
    int result_11;
    result_11 = fn_print(result_10);
    goto next_11;

next_11:;
    res_3 = result_11;
    goto if_next_3;

if_next_3:;
    return res_3;
}
