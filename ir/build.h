#ifndef IR_BUILD_H
#define IR_BUILD_H

#include "ir.h"

struct ir_builder *
build_ir_for_function(/* needed for `associated_text */ struct parser *parser,
                      struct arena_allocator *arena, struct ast_funcdef *def);

#endif
