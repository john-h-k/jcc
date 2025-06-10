#ifndef IR_INTERP_H
#define IR_INTERP_H

#if JCC_INTERP

#include "../util.h"
#include "ir.h"

struct ir_interp;

struct ir_interp_create_args {
  struct ir_unit *unit;
  ustr_t entrypoint;
};

struct ir_interp *ir_interp_create(struct ir_interp_create_args args);

struct ir_interp_exec_info {
  int exc;
};

struct ir_interp_exec_info ir_interp_exec(struct ir_interp *interp);

void ir_interp_free(struct ir_interp **interp);

#endif

#endif
