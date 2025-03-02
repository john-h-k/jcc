#include "inline.h"
#include "opts.h"

static bool opts_inline_op(UNUSED struct ir_func *func, struct ir_op *op) {
  if (op->ty != IR_OP_TY_CALL) {
    return false;
  }

  // TODO: impl
  return false;
}

void opts_inline(struct ir_unit *unit) {
  struct opts_pass pass = {.name = __func__, .op_callback = opts_inline_op};

  opts_run_pass(unit, &pass);
}
