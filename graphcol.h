#ifndef GRAPHCOL_H
#define GRAPHCOL_H

#include "ir/build.h"

#include <stddef.h>

struct reg_info {
  size_t num_regs;
};

void graphcol_register_alloc(struct ir_func *irb, struct reg_info reg_info);

#endif
