#include <vector>
#include <iostream>
#include <cstring>

extern "C" {

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
    if (a.tag == FN) return a.f == b.f;
    if (a.tag == NIL) return true;

    std::cout << "invalid tag in eq!" << std::endl;
    exit(1);
    return false;
}

// required for n_ <- {};
Value new_table() {
    Value out;
    table_ptr t = tables.size();
    tables.push_back({});
    out.tag = TABLE_PTR;
    out.t = t;
    return out;
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
void table_set(Value t, Value key, Value val) {
    if (t.tag != TABLE_PTR) {
        std::cout << "table_get called on non-table value with tag " << t.tag << "!" << std::endl;
        exit(1);
    }
    if (key.tag == NIL) {
        std::cout << "table_get called with nil index!" << std::endl;
        exit(1);
    }
    std::vector<TableEntry>& entries = tables[t.t].entries;
    for (auto& e : entries) {
        if (eq(e.key, key)) {
            e.value = val;
            return;
        }
    }
    TableEntry e;
    e.key = key;
    e.value = val;
    entries.push_back(e);
}

// required for n_ =  n_[n_];
Value table_get(Value t, Value key) {
    if (t.tag != TABLE_PTR) {
        std::cout << "table_set called on non-table value with tag " << t.tag << "!" << std::endl;
        exit(1);
    }
    std::vector<TableEntry>& entries = tables[t.t].entries;
    for (auto e : entries) {
        if (eq(e.key, key)) return e.value;
    }
    return nil();
}

// table-wrapped native fns:
Value print(Value t) {
    Value v = table_get(t, num(1));
    if (v.tag == NUM) std::cout << v.d << std::endl;
    if (v.tag == TABLE_PTR) std::cout << "table ptr" << std::endl;
    if (v.tag == FN) std::cout << "function" << std::endl;
    if (v.tag == NIL) std::cout << "nil" << std::endl;

    Value ret = new_table();
    table_set(ret, num(0), num(0));
    return ret;
}

Value next(Value t) {
    // TODO
    Value ret = new_table();
    table_set(ret, num(0), num(0));
    return ret;
}

Value pairs(Value t) {
    // TODO
    Value ret = new_table();
    table_set(ret, num(0), num(0));
    return ret;
}

Value type(Value t) {
    // TODO
    Value ret = new_table();
    table_set(ret, num(0), num(0));
    return ret;
}

}
