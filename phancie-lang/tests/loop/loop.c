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

extern int print(char* s);

extern int assert_eq(int actual, int expected);

extern int print(char* s);

int fn_main() {
    int result_3 = print("before loop\n");
    int a = 0;

do_continue:;
    int result_5 = assert_eq(a, a);
    int result_7 = a == 5;
    if (result_7) {
        goto if_body;
    } else {
        goto if_alt;
    }

if_body:;
    goto do_break;

if_alt:;

if_next:;
    int result_11 = a == 2;
    if (result_11) {
        goto if_body_2;
    } else {
        goto if_alt_2;
    }

if_body_2:;
    int result_9 = a + 2;
    int result_10 = a = result_9;
    goto do_break;

if_alt_2:;

if_next_2:;
    int result_12 = a + 1;
    int result_13 = a = result_12;
    goto do_continue;

do_break:;
    int result_4 = print("after loop\n");
    return result_4;
}
