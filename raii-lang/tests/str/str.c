int fn_print(char* s) {
    int print_res = print(s);
    return print_res;
}

char* fn_string_clone(char* s_2) {
    char* string_clone_res = string_clone(s_2);
    return string_clone_res;
}

int fn_main() {
    char* s_4 = "hello";
    char* string_clone_res_2 = fn_string_clone(s_4);
    char* s_3;
    goto let_next;

let_next:;
    int print_res_2 = fn_print(s);
    return print_res_2;
}
