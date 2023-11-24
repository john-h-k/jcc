#ifndef LSRA_H
#define LSRA_H

#include "alloc.h"
#include "ir/build.h"

#include <limits.h>
#include <stdlib.h>

struct reg_info {
  size_t num_regs;
};

void print_ir_intervals(FILE *file, struct ir_op *op, void *metadata);
void register_alloc(struct ir_builder *irb, struct reg_info reg_info);

#endif
