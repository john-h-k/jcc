#include "eliminate_phi.h"

#include "ir.h"
#include "prettyprint.h"

void eliminate_phi(struct ir_builder *irb) {
  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    struct ir_stmt *phis = basicblock->phis;
    struct ir_op *op = phis ? phis->first : NULL;

    while (op) {
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

    basicblock = basicblock->succ;
  }

  debug_print_ir(stderr, irb, NULL, NULL);
}
