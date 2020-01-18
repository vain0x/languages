#include <assert.h>
#include <stdio.h>

extern int fn_main();

int assert_eq(int actual, int expected) {
    printf("assert_eq(%d, %d)\n", actual, expected);
    assert(actual == expected);
    return 1;
}

int print(char* str) {
    printf("%s", str);
    return 0;
}

char* string_clone(char* str) {
    return str;
}

int main() {
    fn_main();
    return 0;
}
