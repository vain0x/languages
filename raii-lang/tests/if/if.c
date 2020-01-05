int fn_assert_eq(int* actual, int* expected) {
    int assert_eq_res = assert_eq(&actual, &expected);
    return assert_eq_res;
}

int fn_print(char** s) {
    int print_res = print(&s);
    return print_res;
}

int fn_main() {
    int n = 0;
    int n_2 = 0;
    int prim_eq_res = &n == &n_2;
    int res;
    char* s_7 = "0 == 0";
    int print_res_7 = fn_print(&s_7);
    goto if_next;

if_next:;
    int n_3 = 0;
    int n_4 = 1;
    int prim_eq_res_2 = &n_3 == &n_4;
    int res_2;
    char* s_5 = "0 == 1";
    int print_res_5 = fn_print(&s_5);
    goto if_next_2;

if_next_2:;
    int n_5 = 2;
    void x;
    goto let_next;

let_next:;
    int n_6 = 1;
    int prim_eq_res_3 = &x == &n_6;
    int res_3;
    char* s_2 = "0 == 1";
    int print_res_2 = fn_print(&s_2);
    goto if_next_3;

if_next_3:;
    return res_3;
}
