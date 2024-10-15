#include "eliminate_phi.h"

#include "ir.h"
#include "prettyprint.h"

void eliminate_phi(struct ir_builder *irb) {
  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    if (stmt) {
      // phis always at start of bb
      struct ir_op *op = stmt->first;

      // TODO: make all phis in seperate statement
      struct ir_op *first_non_phi = op;
      while (first_non_phi && first_non_phi->ty == IR_OP_TY_PHI) {
        first_non_phi = first_non_phi->succ;
      }

      while (op && op->ty == IR_OP_TY_PHI) {
        for (size_t i = 0; i < op->phi.num_values; i++) {
          struct ir_op *value = op->phi.values[i];
          struct ir_basicblock *basicblock = value->stmt->basicblock;

          struct ir_op *last = basicblock->last->last;

          invariant_assert(op_is_branch(last->ty),
                           "bb ended in non-branch instruction!");

          while (last->pred && op_is_branch(last->pred->ty)) {
            last = last->pred;
          }

          debug_assert(op->reg.ty != IR_REG_TY_NONE, "expected op %zu to have reg by now",
                       op->id);

          // insert juuust before the branch
          // struct ir_op *storelcl =
          //     insert_before_ir_op(irb, last,
          //                         IR_OP_TY_STORE_LCL, IR_OP_VAR_TY_NONE);
          // storelcl->store_lcl.value = value;
          // storelcl->store_lcl.lcl_idx = lcl_idx;
          struct ir_op *mov =
              insert_before_ir_op(irb, last, IR_OP_TY_MOV, op->var_ty);
          mov->mov.value = value;
          mov->reg = op->reg;
          // FIXME: live regs should be properly propogated when modifying IR,
          // or rebuilt between passes should be pred but easier to use succ,
          // only wastes 1 reg max
          mov->live_gp_regs = mov->succ->live_gp_regs;
          mov->live_fp_regs = mov->succ->live_fp_regs;

          // HACK: using spills for phi
          op->phi.values[i] = mov;
        }

        op = op->succ;
      }
    }

    basicblock = basicblock->succ;
  }
}
