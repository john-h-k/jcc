#ifndef IR_BUILD_H
#define IR_BUILD_H

#include "ir.h"

struct ir_builder *
build_ir_for_function(/* needed for `associated_text */ struct parser *parser,
                      struct arena_allocator *arena, struct ast_funcdef *def);

typedef void(debug_print_op_callback)(FILE *file, struct ir_op *op,
                                      void *metadata);

void debug_print_ir(struct ir_builder *irb, struct ir_basicblock *basicblock,
                    debug_print_op_callback *cb, void *cb_metadata);

#endif
