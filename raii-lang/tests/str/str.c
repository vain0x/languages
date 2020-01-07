extern int print(char* s1);

int fn_print(char* s1) {
    return print(s1);
}

extern char* string_clone(char* s2);

char* fn_string_clone(char* s2) {
    return string_clone(s2);
}

int fn_main() {
    char* result = "hello";

next:;
    char* result_2 = fn_string_clone(result);

next_2:;
    char* s = result_2;

let_next:;
    int result_3 = fn_print(s);

next_3:;
    return result_3;
}
