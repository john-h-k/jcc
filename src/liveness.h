#ifndef LIVENESS_H
#define LIVENESS_H

#include "ir/ir.h"

#include <stddef.h>
#include <stdio.h>

enum FLAG_ENUM interval_flags {
  INTERVAL_FLAG_NONE = 0,
  INTERVAL_FLAG_LIVE_ACROSS_BASICBLOCKS = 1 << 0,
};

struct interval {
  struct ir_op *op;
  size_t start;
  size_t end;

  enum interval_flags flags;
};

struct interval_data {
  struct interval *intervals;
  size_t num_intervals;
};

struct interval_data construct_intervals(struct ir_func *irb);

void print_live_regs(FILE *file, const struct ir_reg_usage *reg_usage);
void print_ir_intervals(FILE *file, struct ir_op *op, void *metadata);

int sort_interval_by_start_point(const void *a, const void *b);

#endif
