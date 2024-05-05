#ifndef GRAPHCOL_H
#define GRAPHCOL_H

#include <stddef.h>

#include "ir/build.h"

struct reg_info {
  size_t num_regs;
};

void graphcol_register_alloc(struct ir_builder *irb, struct reg_info reg_info);

#endif

