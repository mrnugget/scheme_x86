#include <stdio.h>
#include "runtime.h"

#define fixnum_mask       3
#define fixnum_tag        0
#define fixnum_shift      2

#define boolean_mask       255
#define boolean_tag        15
#define boolean_shift      8

int main(int argc, char** argv) {
    int val = scheme_entry();

    if ((val & fixnum_mask) == fixnum_tag){
        printf("%d\n", val >> fixnum_shift);
    } else if ((val & boolean_mask) == boolean_tag){
        int boolval = val >> boolean_shift;
        if (boolval) {
            printf("#t\n");
        } else {
            printf("#f\n");
        }
    } else {
        printf("unrecognized value\n");
    }

    return 0;
}
