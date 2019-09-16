#include <stdio.h>
#include <stdlib.h>
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

void print_value(int val) {
    if ((val & fixnum_mask) == fixnum_tag){
        printf("%d", val >> fixnum_shift);
    } else if ((val & boolean_mask) == boolean_tag){
        int boolval = val >> boolean_shift;
        if (boolval) {
            printf("#t");
        } else {
            printf("#f");
        }
    } else if (val == empty_list){
        printf("()");
    } else if ((val & char_mask) == char_tag){
        int c = val >> char_shift;

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
        int* ptr = (int*)(val - object_tag_string);
        if (ptr == NULL) {
            printf("()");
            return;
        }

        int length = *ptr;
        char *str = (char *)(ptr + 1);

        putchar('"');
        for (int i = 0; i < length; i++) {
            putchar(*str);
            str++;
        }
        putchar('"');
    } else if ((val & object_mask) == object_tag_vector) {
        int* ptr = (int*)(val - object_tag_vector);
        if (ptr == NULL) {
            printf("()");
            return;
        }

        int length = *ptr;
        int *elements = (int *)(ptr + 1);

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

int main(int argc, char** argv) {
    void* heap = aligned_alloc(8, heap_size);

    int val = scheme_entry(heap);
    print_value(val);
    printf("\n");

    free(heap);
    return 0;
}
