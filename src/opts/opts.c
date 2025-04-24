#include "opts.h"

#include "../ir/prettyprint.h"
#include "../ir/validate.h"
#include "../log.h"

static void opts_run_pass_func(struct ir_func *func,
                               const struct opts_op_pass *pass) {
  // NOTE: some passes (notably cnst_branches) rely on this being done
  // sequentially as they use the data per-func so you can't just parallelise it
  // or anything

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    struct ir_op prev = *op;

    bool opt = pass->op_callback(func, op, pass->data);

    if (opt) {
      debug("%s: optimised %%%zu", pass->name, op->id);
      if (debug_enabled()) {
        debug_print_op(stderr, &prev);
        debug_print_op(stderr, op);
      }
      debug_nl();
    }
  }
}

void opts_run_op_pass(struct ir_unit *unit, const struct opts_op_pass *pass) {
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
      if (pass->begin_func_callback) {
        pass->begin_func_callback(glb->func, pass->data);
      }

      opts_run_pass_func(glb->func, pass);

      if (pass->end_func_callback) {
        pass->end_func_callback(glb->func, pass->data);
      }

      // passes should really choose which of these are run, for efficiency
      ir_eliminate_redundant_ops(glb->func,
                                 IR_ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_MOVS);
      ir_prune_basicblocks(glb->func);
      ir_order_basicblocks(glb->func);
      ir_simplify_phis(glb->func);

      ir_validate(unit, IR_VALIDATE_FLAG_NONE);
      break;
    }

    glb = glb->succ;
  }
}

void opts_run_func_pass(struct ir_unit *unit,
                        const struct opts_func_pass *pass) {
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
      pass->func_callback(glb->func, pass->data);

      // passes should really choose which of these are run, for efficiency
      ir_eliminate_redundant_ops(glb->func,
                                 IR_ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_MOVS);
      ir_prune_basicblocks(glb->func);
      ir_order_basicblocks(glb->func);
      ir_simplify_phis(glb->func);

      ir_validate(unit, IR_VALIDATE_FLAG_NONE);
      break;
    }

    glb = glb->succ;
  }
}
