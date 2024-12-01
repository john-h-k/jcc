#ifndef GRAPHCOL_H
#define GRAPHCOL_H

#include "ir/build.h"

#include <stddef.h>

void graphcol_register_alloc(struct ir_func *irb, struct reg_info reg_info);

#endif
