extern int assert_eq(int actual, int expected);

int fn_assert_eq(int actual, int expected) {
    return assert_eq(actual, expected);
}

int fn_main() {
    int result = 1;

next:;
    int ok = result;

let_next:;
    int result_2 = 0;

next_2:;
    int a = result_2;

let_next_2:;

do_continue:;
    if (ok) {
        goto body;
    } else {
        goto do_break;
    }

body:;
    int result_5 = fn_assert_eq(a, a);

next_5:;
    int result_6 = 1;

next_6:;
    int result_7 = a + result_6;

next_7:;
    int result_8 = a = result_7;

next_8:;
    int result_11 = 5;

next_11:;
    int result_12 = a == result_11;

next_12:;
    int res;
    if (result_12) {
        goto if_body;
    } else {
        goto if_alt;
    }

if_body:;
    int result_9 = 0;

next_9:;
    int result_10 = ok = result_9;

next_10:;
    res = result_10;
    goto if_next;

if_alt:;
    res = 0;

if_next:;
    goto do_continue;

do_break:;
    int result_3 = 5;

next_3:;
    int result_4 = fn_assert_eq(a, result_3);

next_4:;
    return result_4;
}
