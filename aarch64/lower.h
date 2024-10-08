#ifndef AARCH64_LOWER_H
#define AARCH64_LOWER_H

#include "../ir/build.h"

// performs platform specific lowering transformations to the IR
void aarch64_lower(struct ir_builder *func);

// does final stack bits
void aarch64_post_reg_lower(struct ir_builder *func);

void aarch64_debug_print_custom_ir_op(FILE *file, const struct ir_builder *func,
                                      const struct ir_op *op);

#endif
