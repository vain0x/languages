void assert_eq(int* actual, int* expected) {
    int assert_eq_res = assert_eq(&actual, &expected);
    return assert_eq_res;
}

void print(int* s) {
    int print_res = print(&s);
    return print_res;
}

void main() {
    int prim_eq_res = &0 == &0;
    int res;
    int print_res_7 = print(&"0 == 0");
    goto if_next;

if_next:;
    int prim_eq_res_2 = &0 == &1;
    int res_2;
    int print_res_5 = print(&"0 == 1");
    goto if_next_2;

if_next_2:;
    int x;
    goto x_next;

x_next:;
    int prim_eq_res_3 = &x == &1;
    int res_3;
    int print_res_2 = print(&"0 == 1");
    goto if_next_3;

if_next_3:;
    return res_3;
}
