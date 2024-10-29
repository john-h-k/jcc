#ifndef LOWER_H
#define LOWER_H

#include "target.h"
#include "ir/ir.h"

// Performs platform agnostic lowering
void lower(struct ir_unit *unit, const struct target *target);

#endif
