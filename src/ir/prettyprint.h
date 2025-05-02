#ifndef IR_PRETTYPRINT_H
#define IR_PRETTYPRINT_H

#include "ir.h"

#include <stdio.h>

typedef const char *(*demangle_sym)(struct arena_allocator *arena,
                                    const char *sym);

typedef void(debug_print_op_callback)(FILE *file, struct ir_op *op,
                                      void *metadata);

struct debug_print_ir_opts {
  // FIXME: across proj, standardise whether `typedef fn` or `typedef fn-ptr` is
  // used
  demangle_sym demangle_sym;
  debug_print_op_callback *cb;
  void *cb_metadata;
};

#define DEBUG_PRINT_IR_OPTS_DEMANGLE(fn)                                       \
  &((struct debug_print_ir_opts){.demangle_sym = (fn)})
#define DEBUG_PRINT_IR_OPTS_CALLBACK(fn, data)                                 \
  &((struct debug_print_ir_opts){.cb = (fn), .cb_metadata = (data)})

#define DEBUG_PRINT_IR_OPTS_DEFAULT &((struct debug_print_ir_opts){0})

typedef void(prettyprint_begin_visit_basicblock)(
    struct ir_func *irb, struct ir_basicblock *basicblock,
    const struct debug_print_ir_opts *opts, void *metadata);
typedef void(prettyprint_end_visit_basicblock)(
    struct ir_func *irb, struct ir_basicblock *basicblock,
    const struct debug_print_ir_opts *opts, void *metadata);

typedef void(prettyprint_begin_visit_stmt)(
    struct ir_func *irb, struct ir_stmt *stmt,
    const struct debug_print_ir_opts *opts, void *metadata);
typedef void(prettyprint_end_visit_stmt)(struct ir_func *irb,
                                         struct ir_stmt *stmt,
                                         const struct debug_print_ir_opts *opts,
                                         void *metadata);

typedef void(prettyprint_visit_op)(struct ir_op *op,
                                   const struct debug_print_ir_opts *opts,
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
                      const struct debug_print_ir_opts *opts,
                      const struct prettyprint_callbacks *callbacks,
                      void *metadata);

void debug_visit_basicblock(struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            const struct debug_print_ir_opts *opts,
                            const struct prettyprint_callbacks *callbacks,
                            void *metadata);

void debug_visit_ir(struct ir_func *irb, const struct debug_print_ir_opts *opts,
                    const struct prettyprint_callbacks *callbacks,
                    void *metadata);

/* Debug methods that write textual, visual output to a file */

void debug_print_ir_reg(FILE *file, struct ir_reg reg);

void debug_print_var_ty_string(FILE *file, const struct ir_var_ty *var_ty);

void debug_print_ir_graph(FILE *file, struct ir_unit *iru);

void debug_print_ir(FILE *file, struct ir_unit *iru,
                    const struct debug_print_ir_opts *opts);

void debug_print_lcl(FILE *file, struct ir_lcl *lcl,
                     const struct debug_print_ir_opts *opts);

void debug_print_glb(FILE *file, struct ir_glb *glb,
                     const struct debug_print_ir_opts *opts);

void debug_print_op(FILE *file, struct ir_op *op,
                    const struct debug_print_ir_opts *opts);

void debug_print_stmt(FILE *file, struct ir_func *irb, struct ir_stmt *stmt,
                      const struct debug_print_ir_opts *opts);

void debug_print_basicblock(FILE *file, struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            const struct debug_print_ir_opts *opts);

void debug_print_ir_func(FILE *file, struct ir_func *irb,
                         const struct debug_print_ir_opts *opts);

void debug_print_ir_var(FILE *file, struct ir_var *var,
                        const struct debug_print_ir_opts *opts);

void debug_print_ir_object(FILE *file, const struct ir_object *object,
                           const struct debug_print_ir_opts *opts);

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
    debug_print_ir_object(stderr, &ir_obj, DEBUG_PRINT_IR_OPTS_DEFAULT);       \
  } while (0)

#endif
