#ifndef IR_PRETTYPRINT_H
#define IR_PRETTYPRINT_H

#include "ir.h"

#include <stdio.h>

typedef void(debug_print_op_callback)(FILE *file, struct ir_op *op,
                                      void *metadata);

void debug_print_ir(struct ir_builder *irb, struct ir_basicblock *basicblock,
                    debug_print_op_callback *cb, void *cb_metadata);

#endif
