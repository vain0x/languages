int fn_print(char* s) {
    return print(s);
}

char* fn_string_clone(char* s_2) {
    return string_clone(s_2);
}

int fn_main() {
    char* result = "hello";

next:;
    char* result_2 = fn_string_clone(result);

next_2:;
    char* s_3 = result_2;

let_next:;
    int result_3 = fn_print(s);

next_3:;
    return result_3;
}
