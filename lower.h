#ifndef LOWER_H
#define LOWER_H

#include "ir/ir.h"
#include "target.h"

// Performs platform agnostic lowering
void lower(struct ir_unit *unit, const struct target *target);

#endif
