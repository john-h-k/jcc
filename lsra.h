#ifndef LSRA_H
#define LSRA_H

#include <stdlib.h>
#include "alloc.h"
#include "ir.h"

#define REG_SPILLED (SIZE_T_MAX)

struct reg_info {
  size_t num_regs;
};

struct interval {
  size_t op_id;
  size_t reg;
  size_t start;
  size_t end;
};

struct interval_data {
  struct interval *intervals;
  size_t num_intervals;
};

struct interval_data register_alloc(struct ir_builder *irb, struct reg_info reg_info);

#endif
