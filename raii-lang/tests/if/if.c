int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_print(char* s) {
    return print(s);
}

int fn_main() {
    int result;
    _ = 0;
    goto next;

next:;
    int result_2;
    _ = 0;
    goto next_2;

next_2:;
    int result_3;
    _ = result == result_2;
    goto next_3;

next_3:;
    int res;
    char* result_22;
    _ = "0 == 0";
    goto next_22;

next_22:;
    int result_23;
    _ = fn_print(result_22);
    goto next_23;

next_23:;
    _ = result_23;
    goto if_next;

if_next:;
    int result_4;
    _ = 0;
    goto next_4;

next_4:;
    int result_5;
    _ = 1;
    goto next_5;

next_5:;
    int result_6;
    _ = result_4 == result_5;
    goto next_6;

next_6:;
    int res_2;
    char* result_18;
    _ = "0 == 1";
    goto next_18;

next_18:;
    int result_19;
    _ = fn_print(result_18);
    goto next_19;

next_19:;
    _ = result_19;
    goto if_next_2;

if_next_2:;
    int result_7;
    _ = 2;
    goto next_7;

next_7:;
    int x;
    _ = result_7;
    goto let_next;

let_next:;
    int result_8;
    _ = 1;
    goto next_8;

next_8:;
    int result_9;
    _ = x == result_8;
    goto next_9;

next_9:;
    int res_3;
    char* result_10;
    _ = "0 == 1";
    goto next_10;

next_10:;
    int result_11;
    _ = fn_print(result_10);
    goto next_11;

next_11:;
    _ = result_11;
    goto if_next_3;

if_next_3:;
    return res_3;
}
