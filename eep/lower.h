#ifndef EEP_LOWER_H
#define EEP_LOWER_H

#include "../ir/build.h"

// performs platform specific lowering transformations to the IR
void eep_pre_reg_lower(struct ir_builder *func);

void eep_debug_print_custom_ir_op(FILE *file, const struct ir_builder *func,
                                      const struct ir_op *op);


#endif
