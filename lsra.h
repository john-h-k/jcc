#ifndef LSRA_H
#define LSRA_H

#include "ir/build.h"

#include <stddef.h>

// This follows a very specific ordering. Indices are used to represent
// registers For register N, if `N < num_volatile`, it represents a volatile reg
// For register N, if `N >= num_volatile && N < (num_volatile +
// num_nonvolatile)`, it is an involatile reg Else, it is a reserved reg
struct reg_set_info {
  size_t num_volatile;
  size_t num_nonvolatile;
  size_t num_reserved; // e.g x29 and x30 - can't be touched by regalloc
};

struct reg_info {
  struct reg_set_info integral_registers;
  struct reg_set_info fp_registers;
};

void print_ir_intervals(FILE *file, struct ir_op *op, void *metadata);
void lsra_register_alloc(struct ir_builder *irb, struct reg_info reg_info);

#endif
