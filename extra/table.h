#include "value.h"

extern "C" {

void new_table(Value *out);
void table_set(Value* t, Value* key, Value* val);
void table_get(Value *t, Value *key, Value *out);

}
