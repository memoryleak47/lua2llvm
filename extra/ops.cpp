#include "ops.h"
#include "table.h"
#include "utils.h"

#include <string.h>

extern "C" {

bool eq(Value* a, Value* b) {
    if (a->tag != b->tag) return false;

    if (a->tag == TABLE_PTR) return a->t == b->t;
    if (a->tag == FN) return a->uvstack_index == b->uvstack_index && a->f == b->f;
    if (a->tag == NIL) return true;
    if (a->tag == NUM) return a->d == b->d;
    if (a->tag == STR) return strcmp(a->s, b->s) == 0;
    if (a->tag == BOOL) return a->b == b->b;

    std::cout << "invalid tag (" << a->tag << ") in eq!" << std::endl;
    exit(1);
    return false;
}

void concat(Value* a, Value* b, Value* out) {
    if (a->tag != STR || b->tag != STR) {
        std::cout << "concat called on non-strings!\n";
        exit(1);
    }

    out->tag = STR;

    char* s1 = a->s;
    char* s2 = b->s;

    int n1 = strlen(s1);
    int n2 = strlen(s2);

    out->s = (char*) malloc(n1 + n2 + 1);
    for (int i = 0; i < n1; i++) {
        out->s[i] = s1[i];
    }
    for (int i = 0; i < n2; i++) {
        out->s[i + n1] = s2[i];
    }
    out->s[n1+n2] = '\0';
}

void len(Value* arg, Value* out) {
    int l;
    if (arg->tag == STR) {
        l = strlen(arg->s);
    } else if (arg->tag == TABLE_PTR) {
        l = table_len(arg->t);
    } else {
        std::cout << "attempt to get length of non-table/str value\n";
        exit(1);
    }

    *out = num((double) l);
}

}
