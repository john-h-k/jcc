#include "reorder_moves.h"

#include "ir.h"

void reorder_moves(struct ir_builder *irb, const struct reg_info *reg_info) {
  struct ir_basicblock *basicblock = irb->first;
  if (1) {
    return;
  }

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->last;

    struct ir_op **write_map =
        arena_alloc(irb->arena, sizeof(struct ir_op *) * reg_info->num_regs);
    struct ir_op **read_map =
        arena_alloc(irb->arena, sizeof(struct ir_op *) * reg_info->num_regs);
    memset(write_map, 0, sizeof(struct ir_op *) * reg_info->num_regs);
    memset(read_map, 0, sizeof(struct ir_op *) * reg_info->num_regs);

    if (stmt) {
      // moves always at end of bb
      struct ir_op *op = stmt->first;

      while (op && op->ty == IR_OP_TY_MOV) {
        write_map[op->reg] = op;
        read_map[op->mov.value->reg] = op;

        op = op->succ;
      }

      bool cyclic = true;
      for (size_t reg = 0; reg < reg_info->num_regs; reg++) {
        if (write_map[reg] && !read_map[reg]) {
          cyclic = false;
        }
      }

      if (cyclic) {
        // currently we keep reg 17 free for this purpose
        // size_t src = read_map[0]->reg;
      }

      struct ir_op *head = NULL;
      for (size_t reg = 0; reg < reg_info->num_regs; reg++) {
        if (write_map[reg] && !read_map[reg]) {
          if (head) {
            move_after_ir_op(irb, write_map[reg], head);
          } else {
            move_before_ir_op(irb, write_map[reg], stmt->first);
          }
        }
      }
    }

    basicblock = basicblock->succ;
  }

  if (1) {
    return;
  }

  // we now need to reorder the inserted movs so they don't overwrite each other
  // or similar
  basicblock = irb->first;

  while (basicblock) {
    // moves always at end of bb
    struct ir_stmt *stmt = basicblock->last;

    if (stmt) {
      struct ir_op *op = stmt->last;
      invariant_assert(op_is_branch(op->ty), "bb doesn't end with branch");
      while (op && op_is_branch(op->ty)) {
        op = op->pred;
      }

      if (op) {
        // count then alloc just to prevent thrashing
        size_t num_movs = 0;
        for (struct ir_op *next_op = op; next_op && next_op->ty == IR_OP_TY_MOV;
             next_op = next_op->pred) {
          num_movs++;
        }

        struct ir_op **movs =
            arena_alloc(irb->arena, sizeof(struct ir_op *) * num_movs);
        size_t i = num_movs - 1;
        for (struct ir_op *next_op = op; next_op && next_op->ty == IR_OP_TY_MOV;
             next_op = next_op->pred, i--) {
          movs[i] = next_op;
        }

        // // as this is likely to be small, just do an insertion sort
        // for (size_t i = 1; i < num_movs; i++) {
        //   for (size_t j = i; j > 0 && movs[j - 1]->mov.value->reg >
        //   movs[j]->reg; j--) {
        //     struct ir_op *tmp = movs[j - 1];
        //     movs[j - 1] = movs[j];
        //     movs[j] = tmp;

        //     swap_ir_ops(irb, movs[j - 1], movs[j]);
        //   }
        // }
      }
    }

    basicblock = basicblock->succ;
  }
}
