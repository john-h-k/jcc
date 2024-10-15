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

          struct ir_basicblock *mov_bb = alloc_ir_basicblock(irb);
          mov_bb->num_preds = 1;
          mov_bb->preds = arena_alloc(irb->arena, sizeof(struct ir_basicblock *));
          mov_bb->preds[0] = value->stmt->basicblock;

          merge
          

          struct ir_basicblock *basicblock = value->stmt->basicblock;

          struct ir_op *last = basicblock->last->last;

          invariant_assert(op_is_branch(last->ty),
                           "bb ended in non-branch instruction!");

          while (last->pred && op_is_branch(last->pred->ty)) {
            last = last->pred;
          }

          struct ir_op *mov =
              insert_before_ir_op(irb, last, IR_OP_TY_MOV, op->var_ty);
          mov->mov.value = value;
          mov->reg = op->reg;

          op->phi.values[i] = mov;
        }

        op = op->succ;
      }
    }

    basicblock = basicblock->succ;
  }
}
