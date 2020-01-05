int fn_assert_eq(int* actual, int* expected) {
    int assert_eq_res = assert_eq(&actual, &expected);
    return assert_eq_res;
}

int fn_main() {
    int ok;
    goto ok_next;

ok_next:;
    int a;
    goto a_next;

a_next:;
    goto do_continue;

do_continue:;
    int assert_eq_res_3 = fn_assert_eq(&a, &a);
    int prim_add_res = a + 1;
    int prim_assign_res = &a = prim_add_res;
    int prim_eq_res = &a == &5;
    int res;
    int prim_assign_res_2 = &ok = 0;
    goto if_next;

if_next:;
    goto do_continue;

do_break:;
    int assert_eq_res_2 = fn_assert_eq(&a, &5);
    return assert_eq_res_2;
}
