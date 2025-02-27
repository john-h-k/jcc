#include "opts.h"

#include "../ir/prettyprint.h"
#include "../log.h"

static void opts_run_pass_func(struct ir_func *func,
                               const struct opts_pass *pass) {
  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    struct ir_op prev = *op;

    bool opt = pass->op_callback(func, op);

    if (opt) {
      debug("%s: optimised %%%zu", pass->name, op->id);
      if (debug_enabled()) {
        debug_print_op(stderr, func, &prev);
        debug_print_op(stderr, func, op);
      }
      debug_nl();
    }
  }
}

void opts_run_pass(struct ir_unit *unit, const struct opts_pass *pass) {
  struct ir_glb *glb = unit->first_global;

  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC:
      opts_run_pass_func(glb->func, pass);
      eliminate_redundant_ops(glb->func, ELIMINATE_REDUNDANT_OPS_FLAG_NONE);
      break;
    }

    glb = glb->succ;
  }
}
