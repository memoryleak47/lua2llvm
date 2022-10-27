#include "value.h"
#include "table.h"
#include "utils.h"

extern "C" {

// table-wrapped native fns:
void print(Value *t, int32_t i, Value *out) {
    Value one = num(1);
    Value v;
    table_get(t, &one, &v);
    if (v.tag == TABLE_PTR) std::cout << "table ptr" << std::endl;
    if (v.tag == FN) std::cout << "function" << std::endl;
    if (v.tag == NIL) std::cout << "nil" << std::endl;
    if (v.tag == NUM) std::cout << v.d << std::endl;
    if (v.tag == STR) std::cout << v.s << std::endl;
    if (v.tag == BOOL) std::cout << std::boolalpha << v.b << std::endl;

    new_table(out);
    Value zero = num(0);
    table_set(out, &zero, &zero);
}

void next(Value t, int32_t i, Value *out) {
    // TODO
    new_table(out);
    Value zero = num(0);
    table_set(out, &zero, &zero);
}

void pairs(Value t, int32_t i, Value *out) {
    // TODO
    new_table(out);
    Value zero = num(0);
    table_set(out, &zero, &zero);
}

void type(Value t, int32_t i, Value *out) {
    // TODO
    new_table(out);
    Value zero = num(0);
    table_set(out, &zero, &zero);
}

}
