int fn_drop(char* _d) {
    return __noop;
}

int fn_borrow(char** _b) {
    return __noop;
}

int fn_main() {
    char* result = "hello";

next:;
    char* a = result;

let_next:;
    int result_2 = fn_drop(a);

next_2:;
    int result_3 = fn_borrow(&a);

next_3:;
    return result_3;
}
