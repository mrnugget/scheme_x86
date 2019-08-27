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

#define object_mask     7 // Look at 3 bits
#define object_pair_tag 1

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
    } else if ((val & object_mask) == object_pair_tag) {
        int* ptr = (int*)(val - object_pair_tag);
        if (ptr == NULL) {
            printf("()");
            return;
        }

        // either a list or a dotted pair
        int car = ptr[0];
        int cdr = ptr[1];
        putchar('(');
        print_value(car);

        // show additional space-separated elems
        while ((cdr & object_mask) == object_pair_tag) {
            ptr = (int*)(cdr - object_pair_tag);
            if(ptr == NULL) break;

            car = ptr[0];
            cdr = ptr[1];
            putchar(' ');
            print_value(car);
        }

        // show dotted pair notation if relevant
        if((cdr & object_mask) != object_pair_tag) {
            printf(" . ");
            print_value(cdr);
        }
        putchar(')');
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
