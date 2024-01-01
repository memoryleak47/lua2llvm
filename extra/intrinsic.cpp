#include "value.h"
#include "table.h"
#include "utils.h"
#include "ops.h"

extern "C" {

// table-wrapped native fns:
void print(Value *v) {
    if (v->tag == TABLE_PTR) std::cout << "table ptr" << std::endl;
    if (v->tag == FN) std::cout << "function" << std::endl;
    if (v->tag == NIL) std::cout << "nil" << std::endl;
    if (v->tag == NUM) std::cout << v->d << std::endl;
    if (v->tag == STR) std::cout << v->s << std::endl;
    if (v->tag == BOOL) std::cout << std::boolalpha << v->b << std::endl;
}

void next(Value *table_arg, Value* key_arg, Value *out) {
    if (table_arg->tag != TABLE_PTR) {
        std::cout << "called next on non-table!\n";
        exit(1);
    }

    auto& entries = tables[table_arg->t].entries;

    *out = {.tag = NIL};

    if (key_arg->tag == NIL) {
        if (entries.size() > 0) {
            *out = entries[0].key;
        }
    } else {
        for (int i = 0; i < entries.size(); i++) {
            if (eq(&entries[i].key, key_arg)) {
                if (entries.size() > i+1) {
                    *out = entries[i+1].key;
                }
                break;
            }
        }
    }
}

void type(Value *t, Value *out) {
    const char* s;

    if (t->tag == TABLE_PTR) {
        s = "table";
    } else if (t->tag == FN) {
        s = "function";
    } else if (t->tag == NIL) {
        s = "nil";
    } else if (t->tag == NUM) {
        s = "number";
    } else if (t->tag == STR) {
        s = "string";
    } else if (t->tag == BOOL) {
        s = "boolean";
    } else {
        std::cout << "type(): invalid tag!\n";
        exit(1);
    }

    out->tag = STR;
    out->s = const_cast<char*>(s);
}

}
