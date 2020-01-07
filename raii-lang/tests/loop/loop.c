int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_print(char* s) {
    return print(s);
}

int fn_main() {
    char* result = "before loop";

next:;
    int result_2 = fn_print(result);

next_2:;
    int result_3 = 0;

next_3:;
    int a = result_3;

let_next:;

do_continue:;
    int result_6 = fn_assert_eq(a, a);

next_6:;
    int result_7 = 5;

next_7:;
    int result_8 = a == result_7;

next_8:;
    int res;
    if (result_8) {
        goto if_body;
    } else {
        goto if_alt;
    }

if_body:;
    goto do_break;

if_alt:;
    res = 0;

if_next:;
    int result_12 = 2;

next_12:;
    int result_13 = a == result_12;

next_13:;
    int res_2;
    if (result_13) {
        goto if_body_2;
    } else {
        goto if_alt_2;
    }

if_body_2:;
    int result_9 = 2;

next_9:;
    int result_10 = a + result_9;

next_10:;
    int result_11 = a = result_10;

next_11:;
    goto do_continue;

if_alt_2:;
    res_2 = 0;

if_next_2:;
    int result_14 = 1;

next_14:;
    int result_15 = a + result_14;

next_15:;
    int result_16 = a = result_15;

next_16:;
    goto do_continue;

do_break:;
    char* result_4 = "after loop";

next_4:;
    int result_5 = fn_print(result_4);

next_5:;
    return result_5;
}
