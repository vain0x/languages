int fn_assert_eq(int* actual, int* expected) {
    int assert_eq_res = assert_eq(&actual, &expected);
    return assert_eq_res;
}

int fn_inc(int* x) {
    int prim_add_res = x + 1;
    int prim_assign_res = &x = prim_add_res;
    return prim_assign_res;
}

int fn_main() {
    int a;
    goto a_next;

a_next:;
    int assert_eq_res_2 = fn_assert_eq(&a, &1);
    int inc_res = fn_inc(&a);
    int assert_eq_res_3 = fn_assert_eq(&a, &2);
    int inc_res_2 = fn_inc(&a);
    int assert_eq_res_4 = fn_assert_eq(&a, &3);
    return assert_eq_res_4;
}
