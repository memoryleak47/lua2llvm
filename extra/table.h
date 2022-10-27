#include "value.h"
#include <vector>

struct TableEntry {
    Value key;
    Value value;
};

struct TableData {
    std::vector<TableEntry> entries;
    int length;
};

extern std::vector<TableData> tables;

extern "C" {

void new_table(Value *out);
void table_set(Value* t, Value* key, Value* val);
void table_get(Value *t, Value *key, Value *out);

int table_len(table_ptr t);

}
