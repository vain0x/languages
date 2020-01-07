int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_print(char* s) {
    return print(s);
}

int fn_main() {
    char* result;
    result = "before loop";
    goto next;

next:;
    int result_2;
    result_2 = fn_print(result);
    goto next_2;

next_2:;
    int result_3;
    result_3 = 0;
    goto next_3;

next_3:;
    int a;
    a = result_3;
    goto let_next;

let_next:;
    goto do_continue;

do_continue:;
    int result_6;
    result_6 = fn_assert_eq(a, a);
    goto next_6;

next_6:;
    int result_7;
    result_7 = 5;
    goto next_7;

next_7:;
    int result_8;
    result_8 = a == result_7;
    goto next_8;

next_8:;
    goto do_break;

if_next:;
    int result_9;
    result_9 = 2;
    goto next_9;

next_9:;
    int result_10;
    result_10 = a == result_9;
    goto next_10;

next_10:;
    int result_14;
    result_14 = 2;
    goto next_14;

next_14:;
    int result_15;
    result_15 = a + result_14;
    goto next_15;

next_15:;
    int result_16;
    result_16 = *a = result_15;
    goto next_16;

next_16:;
    goto do_continue;

if_next_2:;
    int result_11;
    result_11 = 1;
    goto next_11;

next_11:;
    int result_12;
    result_12 = a + result_11;
    goto next_12;

next_12:;
    int result_13;
    result_13 = *a = result_12;
    goto next_13;

next_13:;
    goto do_continue;

do_break:;
    char* result_4;
    result_4 = "after loop";
    goto next_4;

next_4:;
    int result_5;
    result_5 = fn_print(result_4);
    goto next_5;

next_5:;
    return result_5;
}
