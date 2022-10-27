#include "value.h"

extern "C" {

Value nil();
Value num(double d);
bool eq(Value* a, Value* b);

}
