int fn_assert_eq(int* actual, int* expected) {
    int assert_eq_res = assert_eq(&actual, &expected);
    return assert_eq_res;
}

int fn_main() {
    int b = 1;
    void ok;
    goto let_next;

let_next:;
    int n = 0;
    void a;
    goto let_next_2;

let_next_2:;
    goto do_continue;

do_continue:;
    void assert_eq_res_3 = fn_assert_eq(&a, &a);
    int n_3 = 1;
    int prim_add_res = a + n_3;
    int prim_assign_res = &a = prim_add_res;
    int n_4 = 5;
    int prim_eq_res = &a == &n_4;
    int res;
    int b_2 = 0;
    int prim_assign_res_2 = &ok = b_2;
    goto if_next;

if_next:;
    goto do_continue;

do_break:;
    int n_2 = 5;
    void assert_eq_res_2 = fn_assert_eq(&a, &n_2);
    return assert_eq_res_2;
}
