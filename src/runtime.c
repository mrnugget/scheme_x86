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

#define heap_size 0x400000

void print_value(int val) {
    if ((val & fixnum_mask) == fixnum_tag){
        printf("%d\n", val >> fixnum_shift);
    } else if ((val & boolean_mask) == boolean_tag){
        int boolval = val >> boolean_shift;
        if (boolval) {
            printf("#t\n");
        } else {
            printf("#f\n");
        }
    } else if (val == empty_list){
        printf("()\n");
    } else if ((val & char_mask) == char_tag){
        int c = val >> char_shift;

        if      (c == '\t') { printf("#\\tab\n"); }
        else if (c == '\n') { printf("#\\newline\n"); }
        else if (c == '\r') { printf("#\\return\n"); }
        else if (c == ' ')  { printf("#\\space\n"); }
        else                { printf("#\\%c\n", c); }
    } else {
        printf("unrecognized value: %d\n", val);
    }
}

int main(int argc, char** argv) {
    void* heap = aligned_alloc(8, heap_size);

    int val = scheme_entry(heap);
    print_value(val);

    free(heap);
    return 0;
}
