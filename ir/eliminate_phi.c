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

      while (op && op->ty == IR_OP_TY_PHI) {
        for (size_t i = 0; i < op->phi.num_values; i++) {
          struct ir_op *value = op->phi.values[i];
          struct ir_basicblock *basicblock = value->stmt->basicblock;

          debug("bb %zu", basicblock->id);
          debug("last instr %zu", basicblock->last->last->id);

          invariant_assert(op_is_branch(basicblock->last->last->ty),
                           "bb ended in non-branch instruction!");

          // insert juuust before the branch
          struct ir_op *mov = insert_before_ir_op(irb, basicblock->last->last,
                                                  IR_OP_TY_MOV, value->var_ty);
          mov->reg = op->reg;
          mov->mov.value = value;

          op->phi.values[i] = mov;
        }

        op = op->succ;
      }
    }

    basicblock = basicblock->succ;
  }

  debug_print_ir(irb, irb->first, NULL, NULL);

  // we now need to reorder the inserted movs so they don't overwrite each other
  // or similar
  basicblock = irb->first;

  while (basicblock) {
    // moves always at end of bb
    struct ir_stmt *stmt = basicblock->last;

    if (stmt) {
      struct ir_op *op = stmt->first;

      // count then alloc just to prevent thrashing
      size_t num_movs = 0;
      for (struct ir_op *next_op = op; next_op && next_op->ty == IR_OP_TY_MOV; next_op = next_op->succ) {
        num_movs++;
      }

      struct ir_op **movs = arena_alloc(irb->arena, sizeof(struct ir_op *) * num_movs);
      size_t i = num_movs - 1; 
      for (struct ir_op *next_op = op; next_op && next_op->ty == IR_OP_TY_MOV; next_op = next_op->succ, i--) {
        movs[i] = next_op;
      }

      // as this is likely to be small, just do an insertion sort
      for (size_t i = 1; i < num_movs; i++) {
        for (size_t j = i; j > 0 && movs[j - 1]->reg > movs[j]->reg; j--) {
          struct ir_op *tmp = movs[j - 1];
          movs[j - 1] = movs[j];
          movs[j] = tmp;
    
          swap_ir_ops(irb, movs[j - 1], movs[j]);
        }
      }
    }

    basicblock = basicblock->succ;
  }
}
