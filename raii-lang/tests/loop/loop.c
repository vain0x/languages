int fn_assert_eq(int actual, int expected) {
    int assert_eq_res = assert_eq(actual, expected);
    return assert_eq_res;
}

int fn_print(char* s) {
    int print_res = print(s);
    return print_res;
}

int fn_main() {
    char* s_2 = "before loop";
    void print_res_2 = fn_print(s_2);
    int n = 0;
    void a;
    goto let_next;

let_next:;
    goto do_continue;

do_continue:;
    void assert_eq_res_2 = fn_assert_eq(a, a);
    int n_2 = 5;
    int prim_eq_res = a == n_2;
    int res;
    goto do_break;

if_next:;
    int n_3 = 2;
    int prim_eq_res_2 = a == n_3;
    int res_2;
    int n_5 = 2;
    int prim_add_res_2 = a + n_5;
    int prim_assign_res_2 = &a = prim_add_res_2;
    goto do_continue;

if_next_2:;
    int n_4 = 1;
    int prim_add_res = a + n_4;
    int prim_assign_res = &a = prim_add_res;
    goto do_continue;

do_break:;
    char* s_3 = "after loop";
    void print_res_3 = fn_print(s_3);
    return print_res_3;
}
