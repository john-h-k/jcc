#ifndef IR_REORDER_MOVES_H
#define IR_REORDER_MOVES_H

#include "build.h"
#include "../lsra.h"

void reorder_moves(struct ir_builder *irb, const struct reg_info *reg_info);

#endif
