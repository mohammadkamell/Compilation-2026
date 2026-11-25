#include <stdint.h>

struct array {int64_t len; char contents[]; };

extern struct array *dolphin_rc_empty_string;

void *raw_allocate_on_heap(int32_t size);
void *allocate_record (int32_t size);

struct array *raw_allocate_array(int32_t size, int64_t numelems);
struct array *allocate_array(int32_t size, int64_t numelems, void* contents);

void report_error_division_by_zero();

void report_error_nil_access();

void report_error_array_index_out_of_bounds();