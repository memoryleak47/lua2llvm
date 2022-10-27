#include "value.h"
#include "table.h"
#include "utils.h"
#include "ops.h"

std::vector<TableData> tables;

extern "C" {

// required for n_ <- {};
void new_table(Value *out) {
    table_ptr t = tables.size();
    tables.push_back({
        .entries = {},
        .length = 0,
    });

    out->tag = TABLE_PTR;
    out->t = t;
}

void recompute_table_len(Value* t) {
    tables[t->t].length = 0;
    for (int64_t i = 1;; i++) {
        Value key;
        key.tag = NUM;
        key.d = (double) i;

        Value out;
        table_get(t, &key, &out);

        if (out.tag == NIL) {
            break;
        } else {
            tables[t->t].length = i;
        }
    }
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
    auto oldl = tables[t->t].length;
    for (int i = 0; i < entries.size(); i++) {
        if (eq(key, &entries[i].key)) {
            entries.erase(entries.begin() + i);
            break;
        }
    }
    if (val->tag == NIL) {
        if (key->tag == NUM && key->d == (double) oldl) {
            recompute_table_len(t);
        }
    } else {
        entries.push_back({
            .key = *key,
            .value = *val,
        });

        if (key->tag == NUM && key->d == (double) (oldl + 1)) {
            recompute_table_len(t);
        }
    }
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

int table_len(table_ptr t) {
    return tables[t].length;
}

}
