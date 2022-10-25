#include <vector>

#include "value.h"

// upvalue-stack functions:
static std::vector<Value> uvstack;

extern "C" {

int32_t uvstack_push(Value* v) {
    int32_t i = uvstack.size();
    uvstack.push_back(*v);
    return i;
}

void uvstack_get(int32_t idx, Value* out) {
    *out = uvstack[idx];
}

}
