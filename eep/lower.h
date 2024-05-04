#ifndef EEP_LOWER_H
#define EEP_LOWER_H

#include "../ir/build.h"

// performs platform specific lowering transformations to the IR
void eep_lower(struct ir_builder *func);

#endif
