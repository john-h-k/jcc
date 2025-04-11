#ifndef RV32I_LOWER_H
#define RV32I_LOWER_H

#include "../ir/ir.h"

// performs platform specific lowering transformations to the IR
void rv32i_lower_variadic(struct ir_func *func);
void rv32i_lower(struct ir_unit *unit);

struct ir_func_info rv32i_lower_func_ty(struct ir_func *func,
                                        struct ir_var_func_ty func_ty,
                                        struct ir_op **args, size_t num_args);

void rv32i_debug_print_custom_ir_op(FILE *file, const struct ir_func *func,
                                    const struct ir_op *op);

#endif
