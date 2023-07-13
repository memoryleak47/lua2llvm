#pragma once

#include <stdint.h>
#include <vector>
#include <iostream>

// index into the vector `tables` is table_ptr.
using table_ptr = int64_t;

enum tag_t : int64_t {
    TABLE_PTR = 0,
    FN = 1,
    NIL = 2,
    NUM = 3,
    STR = 4,
    BOOL = 5,
};

struct Value {
    tag_t tag;
    union {
        table_ptr t;
        void (*f)(Value*, Value*);
        double d;
        char* s;
        bool b;
    };
};

