#include "eliminate_phi.h"

#include "../hashtbl.h"
#include "../vector.h"
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

struct bb_reg {
  size_t reg;
  struct ir_basicblock *bb;
};

struct bb_moves {
  struct vector *gp_from, *gp_to;
  struct vector *fp_from, *fp_to;
};

static void gen_moves(struct ir_func *irb, struct ir_basicblock *basicblock,
                      struct hashtbl *reg_to_val, struct move_set moves,
                      size_t tmp_index, struct ir_lcl *spill_lcl) {
  struct ir_op *last = basicblock->last->last;

  for (size_t j = 0; j < moves.num_moves; j++) {
    struct move move = moves.moves[j];

    // struct ir_reg from = reg_for_unique_idx(move.from);
    struct ir_reg to = reg_for_unique_idx(move.to);

    printf("ANS from %zu -> %zu\n", move.from, move.to);

    // printf("from %zu -> %zu\n", from.idx, to.idx);

    struct ir_op *value = NULL;
    if (move.from != tmp_index) {
      struct bb_reg key = {.reg = move.from, .bb = basicblock};
      value = *(struct ir_op **)hashtbl_lookup(reg_to_val, &key);
    } else {
      struct bb_reg key = {.reg = move.to, .bb = basicblock};
      value = *(struct ir_op **)hashtbl_lookup(reg_to_val, &key);
    }

    if ((move.to == tmp_index || move.from == tmp_index) && !spill_lcl) {
      spill_lcl = add_local(irb, &IR_VAR_TY_I64);
    }

    if (move.to == tmp_index) {
      struct ir_op *store =
          insert_before_ir_op(irb, last, IR_OP_TY_STORE_LCL, value->var_ty);
      store->lcl = spill_lcl;
      store->store_lcl = (struct ir_op_store_lcl){.value = value};
    } else if (move.from == tmp_index) {
      struct ir_op *load =
          insert_before_ir_op(irb, last, IR_OP_TY_LOAD_LCL, value->var_ty);
      load->reg = to;
      load->load_lcl = (struct ir_op_load_lcl){
          .lcl = spill_lcl,
      };

      struct bb_reg key = {.reg = move.to, .bb = basicblock};
      struct ir_op **op = hashtbl_lookup(reg_to_val, &key);

      if (op) {
        *op = load;
      }
    } else {
      struct ir_op *mov =
          insert_before_ir_op(irb, last, IR_OP_TY_MOV, value->var_ty);
      mov->reg = to;
      mov->mov.value = value;

      struct bb_reg key = {.reg = move.to, .bb = basicblock};
      hashtbl_insert(reg_to_val, &key, &mov);
    }
  }
}

void eliminate_phi(struct ir_func *irb) {
  remove_critical_edges(irb);

  struct ir_basicblock *basicblock = irb->first;

  struct bb_moves *bb_moves =
      arena_alloc(irb->arena, sizeof(*bb_moves) * irb->basicblock_count);

  for (size_t i = 0; i < irb->basicblock_count; i++) {
    bb_moves[i].gp_from = vector_create(sizeof(size_t));
    bb_moves[i].gp_to = vector_create(sizeof(size_t));
    bb_moves[i].fp_from = vector_create(sizeof(size_t));
    bb_moves[i].fp_to = vector_create(sizeof(size_t));
  }

  struct hashtbl *reg_to_val =
      hashtbl_create(sizeof(struct bb_reg), sizeof(struct ir_op *), NULL, NULL);

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    if (basicblock->ty == IR_BASICBLOCK_TY_RET) {
      // no phis
      basicblock = basicblock->succ;
      continue;
    }

    if (stmt) {
      // phis always at start of bb
      struct ir_op *op = stmt->first;

      while (op && op->ty == IR_OP_TY_PHI) {
        for (size_t i = 0; i < op->phi.num_values; i++) {
          struct ir_op *value = op->phi.values[i].value;
          struct ir_basicblock *val_basicblock = value->stmt->basicblock;
          struct ir_op *last = val_basicblock->last->last;

          struct ir_basicblock *mov_bb;

          if (basicblock->num_preds == 1) {
            mov_bb = basicblock->preds[0];
          } else {
            mov_bb = basicblock;
          }

          switch (basicblock->ty) {
          case IR_BASICBLOCK_TY_SPLIT:
          case IR_BASICBLOCK_TY_SWITCH:
            break;
          case IR_BASICBLOCK_TY_MERGE:
            // do the moves here
            mov_bb = basicblock;
            break;
          case IR_BASICBLOCK_TY_RET:
            unreachable();
          }

          struct vector *gp_move_from = bb_moves[val_basicblock->id].gp_from;
          struct vector *gp_move_to = bb_moves[val_basicblock->id].gp_to;
          struct vector *fp_move_from = bb_moves[val_basicblock->id].fp_from;
          struct vector *fp_move_to = bb_moves[val_basicblock->id].fp_to;

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
          } else if (op->reg.ty == IR_REG_TY_FP ||
                     op->reg.ty == IR_REG_TY_INTEGRAL) {
            size_t from_reg = unique_idx_for_reg(value->reg);
            size_t to_reg = unique_idx_for_reg(op->reg);

            if (from_reg != to_reg) {
              struct bb_reg key = {.reg = from_reg, .bb = val_basicblock};
              hashtbl_insert(reg_to_val, &key, &value);

              if (var_ty_is_integral(&value->var_ty)) {
                vector_push_back(gp_move_from, &from_reg);
                vector_push_back(gp_move_to, &to_reg);
              } else {
                vector_push_back(fp_move_from, &from_reg);
                vector_push_back(fp_move_to, &to_reg);
              }
            }
          }
        }

        op = op->succ;
      }
    }

    basicblock = basicblock->succ;
  }

  basicblock = irb->first;
  for (size_t i = 0; i < irb->basicblock_count;
       i++, basicblock = basicblock->succ) {

    struct vector *gp_move_from = bb_moves[basicblock->id].gp_from;
    struct vector *gp_move_to = bb_moves[basicblock->id].gp_to;
    struct vector *fp_move_from = bb_moves[basicblock->id].fp_from;
    struct vector *fp_move_to = bb_moves[basicblock->id].fp_to;

    size_t tmp_index = 3333;

    struct ir_lcl *spill_lcl = NULL;

    struct move_set gp_moves = gen_move_order(
        irb->arena, vector_head(gp_move_from), vector_head(gp_move_to),
        vector_length(gp_move_from), tmp_index);
    gen_moves(irb, basicblock, reg_to_val, gp_moves, tmp_index, spill_lcl);

    for (size_t j = 0; j < vector_length(fp_move_from); j++) {
      size_t f = *(size_t *)vector_get(fp_move_from, j);
      size_t t = *(size_t *)vector_get(fp_move_to, j);
      printf("REQ from %zu to %zu\n", f, t);
    }

    struct move_set fp_moves = gen_move_order(
        irb->arena, vector_head(fp_move_from), vector_head(fp_move_to),
        vector_length(fp_move_from), tmp_index);
    gen_moves(irb, basicblock, reg_to_val, fp_moves, tmp_index, spill_lcl);
  }
}
