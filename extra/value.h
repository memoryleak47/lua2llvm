#pragma once

#include <stdint.h>
#include <vector>
#include <iostream>

// index into the vector `tables` is table_ptr.
using table_ptr = int64_t;

enum tag_t : int32_t {
    TABLE_PTR = 0,
    FN = 1,
    NIL = 2,
    NUM = 3,
    STR = 4,
    BOOL = 5,
};

struct Value {
    tag_t tag;
    int32_t uvstack_index;
    union {
        table_ptr t;
        Value (*f)(Value);
        double d;
        char* s;
        bool b;
    };
};

