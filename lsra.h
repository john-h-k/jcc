#ifndef LSRA_H
#define LSRA_H

#include <stdlib.h>
#include "alloc.h"
#include "ir.h"
#include <limits.h>

struct reg_info {
  size_t num_regs;
};


void register_alloc(struct ir_builder *irb, struct reg_info reg_info);

#endif
