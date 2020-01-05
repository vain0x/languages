void assert_eq(int* actual, int* expected) {
    int assert_eq_res = assert_eq(&actual, &expected);
    return assert_eq_res;
}

void print(int* s) {
    int print_res = print(&s);
    return print_res;
}

void main() {
    int print_res_2 = print(&"before loop");
    int a;
    goto a_next;

a_next:;
    goto do_continue;

do_continue:;
    int assert_eq_res_2 = assert_eq(&a, &a);
    int prim_eq_res = &a == &5;
    int res;
    goto do_break;

if_next:;
    int prim_eq_res_2 = &a == &2;
    int res_2;
    int prim_add_res_2 = a + 2;
    int prim_assign_res_2 = &a = prim_add_res_2;
    goto do_continue;

if_next_2:;
    int prim_add_res = a + 1;
    int prim_assign_res = &a = prim_add_res;
    goto do_continue;

do_break:;
    int print_res_3 = print(&"after loop");
    return print_res_3;
}
