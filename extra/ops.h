#include "value.h"

extern "C" {

bool eq(Value* a, Value* b);
void concat(Value* a, Value* b, Value* out);
void len(Value* arg, Value* out);

}
