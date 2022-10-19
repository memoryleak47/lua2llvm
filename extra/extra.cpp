#include <vector>
#include <iostream>
#include <cstring>

extern "C" {

void err(const char* msg) {
    std::cout << "ERROR: " << msg << std::endl;
    exit(1);
}

// index into the vector `tables` is table_ptr.
using table_ptr = uint64_t;

enum tag_t : uint64_t {
    DOUBLE = 0,
    TABLE_PTR = 1,
    FN = 2,
    NIL = 3,
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
    if (a.tag == DOUBLE) return a.d == b.d;
    if (a.tag == TABLE_PTR) return a.t == b.t;
    if (a.tag == FN) return a.f == b.f;
    if (a.tag == NIL) return true;

    err("invalid tag in eq!");
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
    v.tag = DOUBLE;
    v.d = d;
    return v;
}

Value fn_call(Value f, Value arg) {
    if (f.tag != FN) err("calling fn_call with non-function!");
    return f.f(arg);
}

// required for n_[n_] <- n_;
void table_set(Value t, Value key, Value val) {
    if (t.tag != TABLE_PTR) err("table_get called on non-table!");
    if (key.tag == NIL) err("table_get called with nil index!");
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
    if (t.tag != TABLE_PTR) err("table_get called on non-table!");
    std::vector<TableEntry>& entries = tables[t.t].entries;
    for (auto e : entries) {
        if (eq(e.key, key)) return e.value;
    }
    return nil();
}

// user-accessible function
// hence table-wrapped!
Value print(Value t) {
    Value v = table_get(t, num(1));
    if (v.tag == DOUBLE) std::cout << v.d << std::endl;
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

Value lookup_native_fn(uint64_t i) {
    Value v;
    v.tag = FN;
    if (i == 0) {
        v.f = print;
    } else if (i == 1) {
        v.f = next;
    } else if (i == 2) {
        v.f = pairs;
    } else if (i == 3) {
        v.f = type;
    } else {
        std::cout << "unsupported native_fn " << i << std::endl;
        err("...");
    }
    return v;
}

}
