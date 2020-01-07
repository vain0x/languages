int fn_print(char* s) {
    return print(s);
}

char* fn_string_clone(char* s_2) {
    return string_clone(s_2);
}

int fn_main() {
    char* result;
    result = "hello";
    goto next;

next:;
    char* result_2;
    result_2 = fn_string_clone(result);
    goto next_2;

next_2:;
    char* s_3;
    s_3 = result_2;
    goto let_next;

let_next:;
    int result_3;
    result_3 = fn_print(s);
    goto next_3;

next_3:;
    return result_3;
}
