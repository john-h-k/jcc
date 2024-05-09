#ifndef AARCH64_LOWER_H
#define AARCH64_LOWER_H

#include "../ir/build.h"

// performs platform specific lowering transformations to the IR
void aarch64_pre_reg_lower(struct ir_builder *func);

// does final stack bits
void aarch64_post_reg_lower(struct ir_builder *func);

#endif
