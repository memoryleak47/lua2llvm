#include "value.h"
#include "table.h"
#include "utils.h"
#include "ops.h"

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

void next(Value *t, int32_t i, Value *out) {
    Value zero = num(0);
    Value one = num(1);
    Value two = num(2);

    Value table_arg;
    Value key_arg;

    table_get(t, &one, &table_arg);
    table_get(t, &two, &key_arg);

    if (table_arg.tag != TABLE_PTR) {
        std::cout << "called next on non-table!\n";
        exit(1);
    }

    auto& entries = tables[table_arg.t].entries;

    Value l_out = {.tag = NIL};
    Value r_out = {.tag = NIL};

    if (key_arg.tag == NIL) {
        if (entries.size() > 0) {
            l_out = entries[0].key;
            r_out = entries[0].value;
        }
    } else {
        for (int i = 0; i < entries.size(); i++) {
            if (eq(&entries[i].key, &key_arg)) {
                if (entries.size() > i+1) {
                    l_out = entries[i+1].key;
                    r_out = entries[i+1].value;
                }
                break;
            }
        }
    }

    new_table(out);
    table_set(out, &zero, &two);
    table_set(out, &one, &l_out);
    table_set(out, &two, &r_out);
}

void pairs(Value *t, int32_t i, Value *out) {
    Value zero = num(0);
    Value one = num(1);
    Value two = num(2);

    Value next_fn = {
        .tag = FN,
        .f = next,
    };

    Value arg;
    table_get(t, &one, &arg);

    new_table(out);

    table_set(out, &zero, &two);
    table_set(out, &one, &next_fn);
    table_set(out, &two, &arg);
}

void type(Value *t, int32_t i, Value *out) {
    Value zero = num(0);
    Value one = num(1);

    Value v;

    table_get(t, &one, &v);

    const char* s;

    if (v.tag == TABLE_PTR) {
        s = "table";
    } else if (v.tag == FN) {
        s = "function";
    } else if (v.tag == NIL) {
        s = "nil";
    } else if (v.tag == NUM) {
        s = "number";
    } else if (v.tag == STR) {
        s = "string";
    } else if (v.tag == BOOL) {
        s = "boolean";
    } else {
        std::cout << "type(): invalid tag!\n";
        exit(1);
    }

    new_table(out);

    Value sv;
    sv.tag = STR;
    sv.s = const_cast<char*>(s);

    table_set(out, &zero, &one);
    table_set(out, &one, &sv);
}

}
