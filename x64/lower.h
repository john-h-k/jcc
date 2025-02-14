#ifndef X64_LOWER_H
#define X64_LOWER_H

#include "../ir/build.h"

// performs platform specific lowering transformations to the IR
void x64_lower(struct ir_unit *unit);

void x64_debug_print_custom_ir_op(FILE *file, const struct ir_func *func,
                                      const struct ir_op *op);

#endif
