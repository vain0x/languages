extern int print(char* s1);

int fn_print(char* s1) {
    int result = print(s1);
    return result;
}

extern char* string_clone(char* s2);

char* fn_string_clone(char* s2) {
    char* result_2 = string_clone(s2);
    return result_2;
}

int fn_main() {
    char* result_3 = fn_string_clone("hello");
    char* s = result_3;
    int result_4 = fn_print(s);
    return result_4;
}
