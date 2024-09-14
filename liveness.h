#ifndef LIVENESS_H
#define LIVENESS_H

#include "ir/ir.h"

#include <stddef.h>

struct interval {
  struct ir_op *op;
  size_t start;
  size_t end;
};

struct interval_data {
  struct interval *intervals;
  size_t num_intervals;
};

struct interval_callback_data {
  struct ir_op *op;
  struct interval_data *data;
};

struct interval_data construct_intervals(struct ir_builder *irb);

void print_live_regs(FILE *file, unsigned long live_regs);
void print_ir_intervals(FILE *file, struct ir_op *op, void *metadata);

#endif
