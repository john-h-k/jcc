#ifndef GRAPHCOL_H
#define GRAPHCOL_H

#include "target.h"
#include "ir/ir.h"

void graphcol_register_alloc(struct ir_func *irb, struct reg_info reg_info);

#endif
