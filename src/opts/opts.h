#ifndef OPTS_OPTS_H
#define OPTS_OPTS_H

#include "../ir/ir.h"

typedef void(opts_func_callback)(struct ir_func *func, void *data);
typedef bool(opts_op_callback)(struct ir_func *func, struct ir_op *op,
                               void *data);

// passes which modify single/small groups of ops
struct opts_op_pass {
  const char *name;
  opts_op_callback *op_callback;
  opts_func_callback *begin_func_callback;
  opts_func_callback *end_func_callback;

  void *data;
};

void opts_run_op_pass(struct ir_unit *unit, const struct opts_op_pass *pass);

struct opts_func_pass {
  const char *name;
  opts_func_callback *func_callback;

  void *data;
};

void opts_run_func_pass(struct ir_unit *unit,
                        const struct opts_func_pass *pass);

#endif
