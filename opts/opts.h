#ifndef OPTS_OPTS_H
#define OPTS_OPTS_H

#include "../ir/ir.h"

typedef bool(opts_op_callback)(struct ir_func *func, struct ir_op *op);

struct opts_pass {
  const char *name;
  opts_op_callback *op_callback;
};

void opts_run_pass(struct ir_unit *unit, const struct opts_pass *pass);

#endif
