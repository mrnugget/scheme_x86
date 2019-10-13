#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "runtime.h"

#define fixnum_mask       3
#define fixnum_tag        0
#define fixnum_shift      2

#define boolean_mask       255 // Look at 8 bits
#define boolean_tag        31
#define boolean_shift      8

#define char_mask       255 // Look at 8 bits
#define char_tag        15
#define char_shift      8

#define empty_list 47

#define object_mask       7 // Look at 3 bits
#define object_tag_pair   1
#define object_tag_vector 2
#define object_tag_string 3
#define object_tag_closure 6

#define heap_size 0x400000

int unshift(int val) {
    int shifted = 0;
    if ((val & fixnum_mask) == fixnum_tag){
        shifted = val >> fixnum_shift;
    } else if ((val & boolean_mask) == boolean_tag){
        shifted = val >> boolean_shift;
    } else if ((val & char_mask) == char_tag){
        shifted = val >> char_shift;
    }
    return shifted;
}

int shift_fixnum(int num) {
    return num << fixnum_shift;
}

char *string_data(int str) {
    int* p = (int*)(str - object_tag_string);
    if (p == NULL) {
        return NULL;
    }

    return (char *)(p + 1);
}

int string_len(int str) {
    int* p = (int*)(str - object_tag_string);
    if (p == NULL) {
        return 0;
    }

    return (int)*p;
}

int *vector_data(int vec) {
    int* p = (int*)(vec - object_tag_vector);
    if (p == NULL) {
        return p;
    }

    return (int *)(p + 1);
}

int vector_len(int vec) {
    int* p = (int*)(vec - object_tag_vector);
    if (p == NULL) {
        return 0;
    }

    return (int)*p;
}

void print_value(int val) {
    if ((val & fixnum_mask) == fixnum_tag){
        printf("%d", unshift(val));
    } else if ((val & boolean_mask) == boolean_tag){
        int boolval = unshift(val);
        if (boolval) {
            printf("#t");
        } else {
            printf("#f");
        }
    } else if (val == empty_list){
        printf("()");
    } else if ((val & char_mask) == char_tag){
        int c = unshift(val);

        if      (c == '\t') { printf("#\\tab"); }
        else if (c == '\n') { printf("#\\newline"); }
        else if (c == '\r') { printf("#\\return"); }
        else if (c == ' ')  { printf("#\\space"); }
        else                { printf("#\\%c", c); }
    } else if ((val & object_mask) == object_tag_pair) {
        int* ptr = (int*)(val - object_tag_pair);
        if (ptr == NULL) {
            printf("()");
            return;
        }

        int car = ptr[0];
        int cdr = ptr[1];

        putchar('(');
        print_value(car);

        while ((cdr & object_mask) == object_tag_pair) {
            ptr = (int*)(cdr - object_tag_pair);
            if (ptr == NULL) break;

            car = ptr[0];
            cdr = ptr[1];
            putchar(' ');
            print_value(car);
        }

        if (cdr != empty_list && (cdr & object_mask) != object_tag_pair) {
            printf(" . ");
            print_value(cdr);
        }
        putchar(')');
    } else if ((val & object_mask) == object_tag_string) {
        int length = string_len(val);
        char *str = string_data(val);

        putchar('"');
        for (int i = 0; i < length; i++) {
            putchar(*str);
            str++;
        }
        putchar('"');
    } else if ((val & object_mask) == object_tag_vector) {
        int length = vector_len(val);
        int *elements = vector_data(val);

        printf("#(");
        for (int i = 0; i < length; i++) {
            print_value(*elements);
            if (i + 1 != length) { putchar(' '); }
            elements++;
        }
        putchar(')');
    } else if ((val & object_mask) == object_tag_closure) {
        printf("<closure>");
    } else {
        printf("unrecognized value: %d", val);
    }
}

int scm_hello_from_c() {
    printf("Hello from C!\n");
    return empty_list;
}

int scm_add_two(int val) {
    return shift_fixnum((unshift(val) + 2));
}

int scm_print_three_args(int arg1, int arg2, int arg3) {
    printf("arg1=%d, arg2=%d, arg3=%d\n", unshift(arg1), unshift(arg2), unshift(arg3));
    return empty_list;
}

int scm_print_two_strings(int str1, int str2) {
    write(1, "str1=", 5);
    write(1, string_data(str1), string_len(str1));
    write(1, ", str2=", 7);
    write(1, string_data(str2), string_len(str2));
    printf("\n");
    return empty_list;
}

int scm_exit(int exit_code) {
    exit(unshift(exit_code));
}

int scm_write(int fd, int str, int len) {
    int c = write(unshift(fd), string_data(str), unshift(len));
    return shift_fixnum(c);
}

void scm_error(int origin, int msg) {
    char *o = string_data(origin);
    int olen = string_len(origin);
    char *m = string_data(msg);
    int mlen = string_len(msg);

    fprintf(stderr, "Exception in ");
    write(2, o, (size_t)olen);
    fprintf(stderr, ": ");
    write(2, m, (size_t)mlen);
    fprintf(stderr, "\n");
    exit(0);
}

int main(int argc, char** argv) {
    void* heap = aligned_alloc(8, heap_size);

    int val = scheme_entry(heap);
    print_value(val);
    printf("\n");

    free(heap);
    return 0;
}
