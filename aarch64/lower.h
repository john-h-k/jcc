#ifndef AARCH64_LOWER_H
#define AARCH64_LOWER_H

#include "../ir.h"

// performs platform specific lowering transformations to the IR
void lower(struct ir_builder *func);

#endif
