#include <vector>
#include <iostream>
#include <cstring>

extern "C" {

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
        double d;
        Value (*f)(Value);
    };
};

struct TableEntry {
    Value key;
    Value value;
};

struct TableData {
    std::vector<TableEntry> entries;
};

static std::vector<TableData> tables;

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

// required for n_ <- {};
void new_table(Value *out) {
    table_ptr t = tables.size();
    tables.push_back({});

    out->tag = TABLE_PTR;
    out->t = t;
}

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

// required for n_[n_] <- n_;
void table_set(Value* t, Value* key, Value* val) {
    if (t->tag != TABLE_PTR) {
        std::cout << "table_set called on non-table value with tag " << t->tag << "!" << std::endl;
        exit(1);
    }
    if (key->tag == NIL) {
        std::cout << "table_set called with nil index!" << std::endl;
        exit(1);
    }
    std::vector<TableEntry>& entries = tables[t->t].entries;
    for (auto& e : entries) {
        if (eq(e.key, *key)) {
            e.value = *val;
            return;
        }
    }
    TableEntry e;
    e.key = *key;
    e.value = *val;
    entries.push_back(e);
}

// required for n_ =  n_[n_];
void table_get(Value *t, Value *key, Value *out) {
    if (t->tag != TABLE_PTR) {
        std::cout << "table_get called on non-table value with tag " << t->tag << "!" << std::endl;
        exit(1);
    }
    std::vector<TableEntry>& entries = tables[t->t].entries;
    for (auto e : entries) {
        if (eq(e.key, *key)) {
            *out = e.value;
            return;
        }
    }
    *out = nil();
}

// upvalue-stack functions:
static std::vector<Value> uvstack;

int32_t uvstack_push(Value* v) {
    int32_t i = uvstack.size();
    uvstack.push_back(*v);
    return i;
}

void uvstack_get(int32_t idx, Value* out) {
    *out = uvstack[idx];
}

// table-wrapped native fns:
void print(Value *t, int32_t i, Value *out) {
    Value one = num(1);
    Value v;
    table_get(t, &one, &v);
    if (v.tag == NUM) std::cout << v.d << std::endl;
    if (v.tag == TABLE_PTR) std::cout << "table ptr" << std::endl;
    if (v.tag == FN) std::cout << "function" << std::endl;
    if (v.tag == NIL) std::cout << "nil" << std::endl;

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
