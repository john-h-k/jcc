#include "eliminate_phi.h"

#include "ir.h"
#include "prettyprint.h"

// TODO: properly handle critical edges
// needs analysis to determine phi paths to a given BB

static void remove_critical_edges(struct ir_func *irb) {
  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    size_t num_preds = basicblock->num_preds;

    if (num_preds > 1) {
      for (size_t i = 0; i < num_preds; i++) {
        struct ir_basicblock *pred = basicblock->preds[i];

        if (pred->ty != IR_BASICBLOCK_TY_SWITCH &&
            pred->ty != IR_BASICBLOCK_TY_SPLIT) {
          continue;
        }

        // we have a critical edge
        struct ir_basicblock *intermediate =
            insert_before_ir_basicblock(irb, basicblock);
        intermediate->ty = IR_BASICBLOCK_TY_MERGE;
        intermediate->merge =
            (struct ir_basicblock_merge){.target = basicblock};

        struct ir_stmt *stmt = alloc_ir_stmt(irb, intermediate);
        struct ir_op *op = alloc_ir_op(irb, stmt);
        op->ty = IR_OP_TY_BR;
        op->var_ty = IR_VAR_TY_NONE;

        basicblock->preds[i] = intermediate;

        switch (pred->ty) {
        case IR_BASICBLOCK_TY_SPLIT:
          if (pred->split.true_target == basicblock) {
            pred->split.true_target = intermediate;
          } else {
            pred->split.false_target = intermediate;
          }
          break;
        case IR_BASICBLOCK_TY_SWITCH:
          for (size_t j = 0; j < pred->switch_case.num_cases; j++) {
            if (pred->switch_case.cases[j].target == basicblock) {
              pred->switch_case.cases[j].target = intermediate;
              break;
            }
          }
          break;
        default:
          unreachable();
        }
      }
    }

    basicblock = basicblock->succ;
  }
}

void eliminate_phi(struct ir_func *irb) {
  remove_critical_edges(irb);

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
          struct ir_op *value = op->phi.values[i].value;
          struct ir_basicblock *phi_basicblock = value->stmt->basicblock;

          struct ir_op *last = phi_basicblock->last->last;

          invariant_assert(op_is_branch(last->ty),
                           "bb ended in non-branch instruction!");

          while (last->pred && op_is_branch(last->pred->ty)) {
            last = last->pred;
          }

          debug_assert(op->reg.ty != IR_REG_TY_NONE,
                       "expected op %zu to have reg by now", op->id);

          // insert juuust before the branch
          // struct ir_op *storelcl =
          //     insert_before_ir_op(irb, last,
          //                         IR_OP_TY_STORE_LCL, IR_OP_VAR_TY_NONE);
          // storelcl->store_lcl.value = value;
          // storelcl->store_lcl.lcl_idx = lcl_idx;
          if (op->lcl) {
            struct ir_op *load =
                insert_before_ir_op(irb, last, IR_OP_TY_LOAD_LCL, op->var_ty);
            load->load_lcl = (struct ir_op_load_lcl){.lcl = op->lcl};
            load->reg = op->reg;
            op->phi.values[i].value = load;
          } else {
            struct ir_op *mov =
                insert_before_ir_op(irb, last, IR_OP_TY_MOV, op->var_ty);
            mov->mov.value = value;
            mov->reg = op->reg;
            op->phi.values[i].value = mov;
          }
        }

        op = op->succ;
      }
    }

    basicblock = basicblock->succ;
  }
}
