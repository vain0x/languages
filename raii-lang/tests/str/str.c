int fn_print(int* s) {
    int print_res = print(&s);
    return print_res;
}

int fn_string_clone(int* s_2) {
    int string_clone_res = string_clone(&s_2);
    return string_clone_res;
}

int fn_main() {
    int string_clone_res_2 = fn_string_clone(&"hello");
    int s;
    goto s_next;

s_next:;
    int print_res_2 = fn_print(&s);
    return print_res_2;
}
