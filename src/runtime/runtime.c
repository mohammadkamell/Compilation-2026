#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdint.h>
#include <string.h>

struct array {int64_t len; char contents[]; };

struct array dolphin_rc_empty_string = {0, {}};

extern int64_t dolphin_fun_main();

struct array *cmd_args = NULL;

struct array *get_cmd_args(){
  return cmd_args;
}

void *raw_allocate_on_heap(int32_t size){
    if(size < 0){
        fprintf(stderr, "Internal error: allocation of element negative size!\n");
        exit(1);
    }
    if(size == 0){
        return NULL;
    }
    void *res = malloc(size);
    if(res == NULL){
        fprintf(stderr, "Runtime error: memory allocation failed. This is likely the result of running out of memory!\n");
        exit(1);
    }
    return res;
}

void* allocate_record(int32_t size) {
    return raw_allocate_on_heap(size == 0 ? 1 : size );
}

struct array *raw_allocate_array(int32_t elem_size, int64_t numelems){
    if(numelems < 0){
        fprintf(stderr, "Runtime error: the program attempted to create an array of negative length!\n");
        exit(1);
    }
    if(elem_size < 0){
        fprintf(stderr, "Internal error: the attempted to create an array with elements of negative size!\n");
        exit(1);
    }
    int size_metadata = sizeof(struct array);
    if(numelems > (INT32_MAX - size_metadata + 1) / elem_size){
        fprintf(stderr, "Runtime error: array is too large to allocate!\n");
        exit(1);
    }
    int32_t numbytes = size_metadata + elem_size * numelems;
    struct array *arr = raw_allocate_on_heap(numbytes);
    arr->len = numelems;
    return arr;
}

struct array *allocate_array(int32_t elem_size, int64_t numelems, void* contents){
    struct array *arr = raw_allocate_array(elem_size, numelems);
    void *ptr = arr->contents;
    for(int32_t i = 0; i < numelems; i++){
        memcpy(ptr, contents, elem_size);
        ptr += elem_size;
    }
    return arr;
}

void report_error_division_by_zero(){
    fprintf(stderr, "Runtime error: division by zero!\n");
    exit(1);
}

void report_error_nil_access(){
    fprintf(stderr, "Runtime error: attempt to access nil!\n");
    exit(1);
}

void report_error_array_index_out_of_bounds(){
    fprintf(stderr, "Runtime error: array index out of bounds!\n");
    exit(1);
}

int64_t compare_strings(struct array *s1, struct array *s2){
    int64_t polarity = 1;
    if (s1->len > s2->len){
        struct array *s = s1;
        s1 = s2;
        s2 = s;
        polarity = -1;
    }
    // from now on we can assume s1->len <= s2->len.
    int64_t res = 0;
    int64_t i;
    char *c1 = s1->contents;
    char *c2 = s2->contents;
    for(i = 0; i < s1->len; i++, c1++, c2++){
        if (*c1 < *c2){ res = -1; break; }
        if (*c1 > *c2){ res = 1; break; }
    }
    // if res = 0 then the two strings agree upto s1->len; so, if s2 is longer, it must be greater.
    if(res == 0 && s1->len < s2->len) res = -1;
    return res * polarity;
}

int main(int argsc, char **argsv){
    cmd_args = raw_allocate_array(sizeof(struct array *), argsc);
    struct array **cmd_args_contents = (struct array **) cmd_args->contents;
    for(int i = 0; i < argsc; i++){
        int len = strlen(argsv[i]);
        cmd_args_contents[i] = raw_allocate_array(sizeof(char), len);
        memcpy(cmd_args_contents[i]->contents, argsv[i], len);
    }
    return dolphin_fun_main();
}