#ifndef IR_PRETTYPRINT_H
#define IR_PRETTYPRINT_H

#include "ir.h"

#include <stdio.h>

typedef void(prettyprint_begin_visit_basicblock)(
    struct ir_func *irb, struct ir_basicblock *basicblock, void *metadata);
typedef void(prettyprint_end_visit_basicblock)(struct ir_func *irb,
                                               struct ir_basicblock *basicblock,
                                               void *metadata);

typedef void(prettyprint_begin_visit_stmt)(struct ir_func *irb,
                                           struct ir_stmt *stmt,
                                           void *metadata);
typedef void(prettyprint_end_visit_stmt)(struct ir_func *irb,
                                         struct ir_stmt *stmt, void *metadata);

typedef void(prettyprint_visit_op)(struct ir_func *irb, struct ir_op *op,
                                   void *metadata);

struct prettyprint_callbacks {
  prettyprint_begin_visit_basicblock *begin_visit_basicblock;
  prettyprint_end_visit_basicblock *end_visit_basicblock;

  prettyprint_begin_visit_stmt *begin_visit_stmt;
  prettyprint_end_visit_stmt *end_visit_stmt;

  prettyprint_visit_op *visit_op;
};

extern const struct prettyprint_callbacks FILE_WRITER_CALLBACKS;
extern const struct prettyprint_callbacks GRAPH_WRITER_CALLBACKS;

/* Generic visitor methods that can be used to walk the graph */

void debug_visit_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                      const struct prettyprint_callbacks *callbacks,
                      void *metadata);

void debug_visit_basicblock(struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            const struct prettyprint_callbacks *callbacks,
                            void *metadata);

void debug_visit_ir(struct ir_func *irb,
                    const struct prettyprint_callbacks *callbacks,
                    void *metadata);

/* Debug methods that write textual, visual output to a file */

typedef void(debug_print_op_callback)(FILE *file, struct ir_op *op,
                                      void *metadata);

void debug_print_ir_reg(FILE *file, struct ir_reg reg);

void debug_print_var_ty_string(FILE *file, struct ir_unit *iru,
                               const struct ir_var_ty *var_ty);

void debug_print_ir_graph(FILE *file, struct ir_unit *iru);

void debug_print_ir(FILE *file, struct ir_unit *iru,
                    debug_print_op_callback *cb, void *cb_metadata);

void debug_print_lcl(FILE *file, struct ir_lcl *lcl);

void debug_print_glb(FILE *file, struct ir_glb *glb,
                     debug_print_op_callback *cb, void *cb_metadata);

void debug_print_op(FILE *file, struct ir_func *irb, struct ir_op *op);

void debug_print_stmt(FILE *file, struct ir_func *irb, struct ir_stmt *stmt,
                      debug_print_op_callback *cb, void *cb_metadata);

void debug_print_basicblock(FILE *file, struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            debug_print_op_callback *cb, void *cb_metadata);

void debug_print_ir_func(FILE *file, struct ir_func *irb,
                         debug_print_op_callback *cb, void *cb_metadata);

void debug_print_ir_var(FILE *file, struct ir_var *var);

void debug_print_ir_object(FILE *file, const struct ir_object *object);

void debug_print_func_info(FILE *file, struct ir_unit *iru,
                           const struct ir_func_info *func_info);

#define FDEBUG_PRINT_IR(file, obj)                                             \
  do {                                                                         \
    struct ir_object ir_obj = IR_MK_OBJECT((obj));                             \
    debug_print_ir_object((file), &ir_obj);                                    \
  } while (0)

#define DEBUG_PRINT_IR(obj)                                                    \
  do {                                                                         \
    struct ir_object ir_obj = IR_MK_OBJECT((obj));                             \
    debug_print_ir_object(stderr, &ir_obj);                                    \
  } while (0)

#endif
