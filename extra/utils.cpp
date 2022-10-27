#include "value.h"

#include <string.h>

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

}
