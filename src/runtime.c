#include <stdio.h>
#include "runtime.h"

#define fixnum_mask       3
#define fixnum_tag        0
#define fixnum_shift      2

int main(int argc, char** argv) {
    int val = scheme_entry();

    if ((val & fixnum_mask) == fixnum_tag){
        printf("%d\n", val >> fixnum_shift);
    } else {
        printf("unrecognized value\n");
    }

    return 0;
}
