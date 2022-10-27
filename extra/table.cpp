#include "value.h"
#include "table.h"
#include "utils.h"

struct TableEntry {
    Value key;
    Value value;
};

struct TableData {
    std::vector<TableEntry> entries;
};

static std::vector<TableData> tables;

extern "C" {

// required for n_ <- {};
void new_table(Value *out) {
    table_ptr t = tables.size();
    tables.push_back({});

    out->tag = TABLE_PTR;
    out->t = t;
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
        if (eq(&e.key, key)) {
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
        if (eq(&e.key, key)) {
            *out = e.value;
            return;
        }
    }
    *out = nil();
}

}
