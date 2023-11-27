#ifndef IR_PRETTYPRINT_H
#define IR_PRETTYPRINT_H

#include "ir.h"

#include <stdio.h>

typedef void(debug_print_op_callback)(FILE *file, struct ir_op *op,
                                      void *metadata);

typedef void(prettyprint_begin_visit_basicblock)(struct ir_builder *irb, struct ir_basicblock *basicblock, void *metadata);
typedef void(prettyprint_end_visit_basicblock)(struct ir_builder *irb, struct ir_basicblock *basicblock, void *metadata);

typedef void(prettyprint_begin_visit_stmt)(struct ir_builder *irb, struct ir_basicblock *basicblock, void *metadata);
typedef void(prettyprint_end_visit_stmt)(struct ir_builder *irb, struct ir_basicblock *basicblock, void *metadata);

typedef void(prettyprint_visit_op)(struct ir_builder *irb, struct ir_op *op, void *metadata);

struct prettyprint_callbacks {
  prettyprint_begin_visit_basicblock *begin_visit_basicblock;
  prettyprint_end_visit_basicblock *end_visit_basicblock;

  prettyprint_begin_visit_stmt *begin_visit_stmt;
  prettyprint_end_visit_stmt *end_visit_stmt;

  prettyprint_visit_op *visit_op;
};

void debug_print_ir(FILE *file, struct ir_builder *irb,
                    struct ir_basicblock *basicblock,
                    debug_print_op_callback *cb, void *cb_metadata);

void debug_print_basicblock(FILE *file, struct ir_builder *irb,
                            struct ir_basicblock *basicblock,
                            debug_print_op_callback *cb, void *cb_metadata);

void debug_print_ir_graph(FILE *file, struct ir_builder *irb);

#endif
