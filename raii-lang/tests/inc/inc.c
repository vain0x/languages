int fn_assert_eq(int* actual, int* expected) {
    int assert_eq_res = assert_eq(&actual, &expected);
    return assert_eq_res;
}

int fn_inc(int* x) {
    int n = 1;
    int prim_add_res = x + n;
    int prim_assign_res = &x = prim_add_res;
    return prim_assign_res;
}

int fn_main() {
    int n_2 = 1;
    void a;
    goto let_next;

let_next:;
    int n_3 = 1;
    int assert_eq_res_2 = fn_assert_eq(&a, &n_3);
    int inc_res = fn_inc(&a);
    int n_4 = 2;
    int assert_eq_res_3 = fn_assert_eq(&a, &n_4);
    int inc_res_2 = fn_inc(&a);
    int n_5 = 3;
    int assert_eq_res_4 = fn_assert_eq(&a, &n_5);
    return assert_eq_res_4;
}
