#ifndef LSRA_H
#define LSRA_H

#include "ir/ir.h"
#include "target.h"

void print_ir_intervals(FILE *file, struct ir_op *op, void *metadata);
void lsra_register_alloc(struct ir_func *irb, struct reg_info reg_info);

#endif
