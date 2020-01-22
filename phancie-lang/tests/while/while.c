extern int assert_eq(int actual, int expected);

int fn_assert_eq(int actual, int expected) {
    int result = assert_eq(actual, expected);
    return result;
}

int fn_main() {
    int ok = 1;
    int a = 0;

do_continue:;
    if (ok) {
        goto body;
    } else {
        goto do_break;
    }

body:;
    int result_3 = fn_assert_eq(a, a);
    int result_4 = a + 1;
    int result_5 = a = result_4;
    int result_8 = a == 5;
    int result_6;
    if (result_8) {
        goto if_body;
    } else {
        goto if_alt;
    }

if_body:;
    int result_7 = ok = 0;
    result_6 = result_7;
    goto if_next;

if_alt:;
    result_6 = /* unit */ 0;

if_next:;
    goto do_continue;

do_break:;
    int result_2 = fn_assert_eq(a, 5);
    return result_2;
}
