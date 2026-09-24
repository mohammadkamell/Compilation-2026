#include <stdio.h>         /* make sure these two includes are    */
#include <inttypes.h>      /* present in the start of your C file */
#include <stdlib.h>        /* for exit() */

// your LLVM program must produce a function called dolphin_main of the following type.
extern int64_t dolphin_main();

int64_t read_integer () {
    int64_t value;
    printf("Please enter an integer: ");
    scanf("%" PRId64 "" , &value);
    return value;
}

void print_integer (int64_t value) {
    printf("%" PRId64 "\n" , value);
}

void dolphin_div_by_zero() {
    fprintf(stderr, "Runtime error: division by zero\n");
    exit(1);
}

int main(){
    return dolphin_main();
}