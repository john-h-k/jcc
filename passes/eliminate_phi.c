#include "eliminate_phi.h"

void eliminate_phi(struct ir_builder *irb) {
  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        if (op->ty == IR_OP_TY_PHI) {
          for (size_t i = 0; i < op->phi.num_values; i++) {
            struct ir_op *value = op->phi.values[i];
            struct ir_basicblock *basicblock = value->stmt->basicblock;
            
            // insert juuust before the branch
            struct ir_op *mov = insert_before_ir_op(irb, basicblock->last->last, IR_OP_TY_MOV, value->var_ty);
            mov->reg = op->reg;
            mov->mov.value = value;

            op->phi.values[i] = mov;
          }
        }

        op = op->succ;
      }
      
      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}

