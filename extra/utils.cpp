#include "value.h"

extern "C" {

Value nil() {
    Value v;
    v.tag = NIL;
    return v;
}

Value num(double d) {
    Value v;
    v.tag = NUM;
    v.d = d;
    return v;
}

bool eq(Value a, Value b) {
    if (a.tag != b.tag) return false;
    if (a.tag == NUM) return a.d == b.d;
    if (a.tag == TABLE_PTR) return a.t == b.t;
    if (a.tag == FN) return a.uvstack_index == b.uvstack_index && a.f == b.f;
    if (a.tag == NIL) return true;

    std::cout << "invalid tag (" << a.tag << ") in eq!" << std::endl;
    exit(1);
    return false;
}

}
