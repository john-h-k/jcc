#include "eliminate_phi.h"

#include "../bitset.h"
#include "../hashtbl.h"
#include "../vector.h"
#include "ir.h"

static void remove_critical_edges(struct ir_func *irb) {
  // FIXME: i believe this doesn't properly propogate phis through the arms of a
  // switch expr. see lower.c

  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    size_t num_preds = basicblock->num_preds;

    struct ir_stmt *phi_stmt = NULL;
    if (basicblock->first && basicblock->first->first &&
        basicblock->first->first->ty == IR_OP_TY_PHI) {
      phi_stmt = basicblock->first;
    }

    if (num_preds > 1) {
      for (size_t i = 0; i < num_preds; i++) {
        struct ir_basicblock *pred = basicblock->preds[i];

        if (pred->ty == IR_BASICBLOCK_TY_MERGE) {
          // not critical edge
          continue;
        }

        // we have a critical edge
        struct ir_basicblock *intermediate =
            insert_before_ir_basicblock(irb, basicblock);
        intermediate->ty = IR_BASICBLOCK_TY_MERGE;
        intermediate->merge =
            (struct ir_basicblock_merge){.target = basicblock};

        struct ir_stmt *intermediate_phi_stmt = NULL;
        if (phi_stmt) {
          intermediate_phi_stmt = alloc_ir_stmt(irb, intermediate);
        }

        struct ir_stmt *br_stmt = alloc_ir_stmt(irb, intermediate);
        struct ir_op *op = alloc_ir_op(irb, br_stmt);
        op->ty = IR_OP_TY_BR;
        op->var_ty = IR_VAR_TY_NONE;

        basicblock->preds[i] = intermediate;

        add_pred_to_basicblock(irb, intermediate, pred);

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
        case IR_BASICBLOCK_TY_MERGE:
          pred->merge.target = intermediate;
          break;
        case IR_BASICBLOCK_TY_RET:
          unreachable();
        }

        if (phi_stmt) {
          struct ir_op *phi = phi_stmt->first;
          while (phi && phi->ty == IR_OP_TY_PHI) {
            struct ir_op *int_phi = alloc_ir_op(irb, intermediate_phi_stmt);
            int_phi->ty = IR_OP_TY_PHI;
            int_phi->var_ty = phi->var_ty;
            int_phi->reg = phi->reg;
            int_phi->phi = (struct ir_op_phi){
                .num_values = 1,
                .values =
                    arena_alloc(irb->arena, sizeof(*int_phi->phi.values))};

            bool found = false;
            for (size_t j = 0; j < phi->phi.num_values; j++) {
              struct ir_phi_entry *entry = &phi->phi.values[j];

              if (entry->basicblock == pred) {
                int_phi->phi.values[0] = (struct ir_phi_entry){
                    .basicblock = pred, .value = entry->value};
                *entry = (struct ir_phi_entry){.basicblock = intermediate,
                                               .value = int_phi};
                found = true;
                break;
              }
            }

            DEBUG_ASSERT(found, "failed to gen phi");

            phi = phi->succ;
          }
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

#define REG_POS(reg) (reg)
#define LCL_POS(lcl) (SIZE_MAX - 1 - lcl)

static void gen_moves(struct ir_func *irb, struct ir_basicblock *basicblock,
                      struct hashtbl *reg_to_val, struct move_set moves,
                      size_t tmp_index, struct ir_lcl *spill_lcl,
                      enum ir_reg_ty reg_ty, bool early_moves) {
  struct ir_op *last =
      early_moves ? basicblock->first->first : basicblock->last->last;

  struct bitset *reg_usage;
  struct ir_var_ty lcl_ty;
  struct reg_set_info reg_info;
  switch (reg_ty) {
  case IR_REG_TY_INTEGRAL:
    reg_usage = irb->reg_usage.gp_registers_used;
    lcl_ty = var_ty_for_pointer_size(irb->unit);
    reg_info = irb->unit->target->reg_info.gp_registers;
    break;
  case IR_REG_TY_FP:
    reg_usage = irb->reg_usage.fp_registers_used;
    lcl_ty = IR_VAR_TY_F64;
    reg_info = irb->unit->target->reg_info.fp_registers;
    break;
  default:
    unreachable();
  }

  struct ir_var_ty store_var_ty;

  size_t next_free = bitset_length(reg_usage) - bitset_lzcnt(reg_usage);

  bool has_free_reg;
  struct ir_reg free_reg;
  if (next_free < reg_info.num_volatile) {
    has_free_reg = true;
    free_reg = (struct ir_reg){.ty = reg_ty, .idx = next_free};
  } else {
    has_free_reg = false;
  }

  struct ir_op *tmp_mov = NULL;

  for (size_t j = 0; j < moves.num_moves; j++) {
    struct move move = moves.moves[j];

    struct ir_reg to = ir_reg_for_unique_idx(move.to.idx);

    struct ir_op *value = NULL;
    if (move.from.idx != tmp_index) {
      struct bb_reg key = {.reg = move.from.idx, .bb = basicblock};
      value = *(struct ir_op **)hashtbl_lookup(reg_to_val, &key);
    } else {
      struct bb_reg key = {.reg = move.to.idx, .bb = basicblock};
      value = *(struct ir_op **)hashtbl_lookup(reg_to_val, &key);
    }

    if (!has_free_reg &&
        (move.to.idx == tmp_index || move.from.idx == tmp_index) &&
        !spill_lcl) {
      spill_lcl = add_local(irb, &lcl_ty);
    }

    if (move.to.idx == tmp_index) {
      DEBUG_ASSERT(move.from.idx < SIZE_MAX / 2, "can't do stack<->stack move");

      if (has_free_reg) {
        struct ir_op *mov =
            insert_before_ir_op(irb, last, IR_OP_TY_MOV, lcl_ty);
        mov->mov = (struct ir_op_mov){.value = value};
        mov->reg = free_reg;
        mov->flags |= IR_OP_FLAG_PHI_MOV;
        // store_var_ty = value->var_ty;
        store_var_ty = lcl_ty;

        tmp_mov = mov;
      } else {
        struct ir_op *addr =
            insert_before_ir_op(irb, last, IR_OP_TY_ADDR, var_ty_for_pointer_size(irb->unit));
        addr->addr = (struct ir_op_addr){
            .ty = IR_OP_ADDR_TY_LCL,
            .lcl = spill_lcl,
        };
        addr->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = irb->unit->target->reg_info.ssp};

        struct ir_op *store =
            insert_after_ir_op(irb, addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
        store->store = (struct ir_op_store){
            .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = value};
        store->flags |= IR_OP_FLAG_PHI_MOV;
        // store_var_ty = value->var_ty;
        store_var_ty = lcl_ty;
      }
    } else if (move.from.idx == tmp_index) {
      struct ir_op *phi_op;

      if (has_free_reg) {
        struct ir_op *mov =
            insert_before_ir_op(irb, last, IR_OP_TY_MOV, lcl_ty);
        mov->mov = (struct ir_op_mov){.value = tmp_mov};
        mov->reg = to;
        mov->flags |= IR_OP_FLAG_PHI_MOV;
        store_var_ty = value->var_ty;

        phi_op = mov;
      } else {
        struct ir_op *addr =
            insert_before_ir_op(irb, last, IR_OP_TY_ADDR, var_ty_for_pointer_size(irb->unit));
        addr->addr = (struct ir_op_addr){
            .ty = IR_OP_ADDR_TY_LCL,
            .lcl = spill_lcl,
        };
        addr->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = irb->unit->target->reg_info.ssp};

        struct ir_op *load =
            insert_after_ir_op(irb, addr, IR_OP_TY_LOAD, store_var_ty);
        load->reg = to;
        load->load =
            (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};

        phi_op = load;
      }

      struct bb_reg key = {.reg = move.to.idx, .bb = basicblock};
      struct ir_op **op = hashtbl_lookup(reg_to_val, &key);

      if (op) {
        *op = phi_op;
      }
    } else if (move.from.idx >= SIZE_MAX / 2) {
      struct ir_lcl *lcl = move.from.metadata[0];

      struct ir_op *addr =
          insert_before_ir_op(irb, last, IR_OP_TY_ADDR, var_ty_for_pointer_size(irb->unit));
      addr->addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = lcl,
      };
      addr->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                  .idx = irb->unit->target->reg_info.ssp};

      struct ir_op *load =
          insert_after_ir_op(irb, addr, IR_OP_TY_LOAD, lcl->var_ty);
      load->reg = to;
      load->load = (struct ir_op_load){
          .ty = IR_OP_LOAD_TY_ADDR,
          .addr = addr,
      };
    } else if (move.to.idx >= SIZE_MAX / 2) {
      struct ir_lcl *lcl = move.to.metadata[0];

      struct ir_op *addr =
          insert_before_ir_op(irb, last, IR_OP_TY_ADDR, var_ty_for_pointer_size(irb->unit));
      addr->addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = lcl,
      };
      addr->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                  .idx = irb->unit->target->reg_info.ssp};

      struct ir_op *store =
          insert_after_ir_op(irb, addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
      store->store = (struct ir_op_store){
          .ty = IR_OP_STORE_TY_ADDR,
          .value = value,
          .addr = addr,
      };
    } else {
      struct ir_op *mov =
          insert_before_ir_op(irb, last, IR_OP_TY_MOV, value->var_ty);
      mov->flags |= IR_OP_FLAG_PHI_MOV;
      mov->reg = to;
      mov->mov.value = value;

      struct bb_reg key = {.reg = move.to.idx, .bb = basicblock};
      hashtbl_insert(reg_to_val, &key, &mov);
    }
  }
}

struct bb_moves {
  struct vector *gp_from, *gp_to;
  struct vector *fp_from, *fp_to;
};

void eliminate_phi(struct ir_func *irb) {
  remove_critical_edges(irb);
  rebuild_ids(irb);

  struct ir_basicblock *basicblock = irb->first;

  struct bb_moves *bb_moves =
      arena_alloc(irb->arena, sizeof(*bb_moves) * irb->basicblock_count * 2);

  for (size_t i = 0; i < irb->basicblock_count * 2; i++) {
    bb_moves[i].gp_from = vector_create(sizeof(struct location));
    bb_moves[i].gp_to = vector_create(sizeof(struct location));
    bb_moves[i].fp_from = vector_create(sizeof(struct location));
    bb_moves[i].fp_to = vector_create(sizeof(struct location));
  }

  struct hashtbl *reg_to_val =
      hashtbl_create(sizeof(struct bb_reg), sizeof(struct ir_op *), NULL, NULL);

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    bool moves_in_preds;

    if (basicblock->num_preds == 1) {
      moves_in_preds = false;
    } else if (basicblock->num_preds > 1) {
      moves_in_preds = true;
    } else {
      basicblock = basicblock->succ;
      continue;
    }

    if (stmt) {
      // phis always at start of bb
      struct ir_op *op = stmt->first;

      while (op && op->ty == IR_OP_TY_PHI) {
        for (size_t i = 0; i < op->phi.num_values; i++) {
          struct ir_op *value = op->phi.values[i].value;

          struct ir_basicblock *mov_bb;
          size_t bb_move_id;
          if (moves_in_preds) {
            mov_bb = value->stmt->basicblock;
            bb_move_id = mov_bb->id * 2;
          } else {
            mov_bb = basicblock;
            bb_move_id = mov_bb->id * 2 + 1;
          }

          struct vector *gp_move_from = bb_moves[bb_move_id].gp_from;
          struct vector *gp_move_to = bb_moves[bb_move_id].gp_to;
          struct vector *fp_move_from = bb_moves[bb_move_id].fp_from;
          struct vector *fp_move_to = bb_moves[bb_move_id].fp_to;

          DEBUG_ASSERT(op->reg.ty != IR_REG_TY_NONE,
                       "expected op %zu to have reg by now", op->id);

          // FIXME: logic here is iffy at best
          // we should move to a model where all ops have registers, but may or
          // may not be in them (and so need reloads)

          // TODO: disabled for new phi logic. keeping in case it is needed to
          // undo if (op->lcl) {
          //   struct ir_op *load =
          //       insert_before_ir_op(irb, last, IR_OP_TY_LOAD, op->var_ty);
          //   load->load = (struct ir_op_load){
          //     .ty = IR_OP_LOAD_TY_LCL,
          //     .lcl = op->lcl};
          //   load->reg = op->reg;
          //   op->phi.values[i].value = load;
          if (op->reg.ty == IR_REG_TY_FP || op->reg.ty == IR_REG_TY_INTEGRAL) {
            size_t from_pos;
            size_t to_pos;
            void *from_metadata = NULL;
            void *to_metadata = NULL;

            // this should really be done by regalloc, but we need crit edges to
            // be split first and we get better regalloc if we don't split crit
            // edges beforehand

            if (value->lcl) {
              from_pos = LCL_POS(value->lcl->id);
              from_metadata = value->lcl;
            } else {
              from_pos = REG_POS(unique_idx_for_ir_reg(value->reg));
            }

            if (op->lcl) {
              to_pos = LCL_POS(op->lcl->id);
              to_metadata = op->lcl;
            } else {
              to_pos = REG_POS(unique_idx_for_ir_reg(op->reg));
            }

            if (from_pos != to_pos) {
              struct location from = {.idx = from_pos,
                                      .metadata[0] = from_metadata};
              struct location to = {.idx = to_pos, .metadata[0] = to_metadata};

              struct bb_reg key = {.reg = from_pos, .bb = mov_bb};
              hashtbl_insert(reg_to_val, &key, &value);

              if (var_ty_is_integral(&value->var_ty)) {
                vector_push_back(gp_move_from, &from);
                vector_push_back(gp_move_to, &to);
              } else {
                vector_push_back(fp_move_from, &from);
                vector_push_back(fp_move_to, &to);
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

    for (size_t j = 0; j < 2; j++) {
      struct vector *gp_move_from = bb_moves[basicblock->id * 2 + j].gp_from;
      struct vector *gp_move_to = bb_moves[basicblock->id * 2 + j].gp_to;
      struct vector *fp_move_from = bb_moves[basicblock->id * 2 + j].fp_from;
      struct vector *fp_move_to = bb_moves[basicblock->id * 2 + j].fp_to;

      size_t tmp_index = SIZE_MAX;

      struct ir_lcl *spill_lcl = NULL;

      bool early = j;

      struct move_set gp_moves = gen_move_order(
          irb->arena, vector_head(gp_move_from), vector_head(gp_move_to),
          vector_length(gp_move_from), tmp_index);
      gen_moves(irb, basicblock, reg_to_val, gp_moves, tmp_index, spill_lcl,
                IR_REG_TY_INTEGRAL, early);

      struct move_set fp_moves = gen_move_order(
          irb->arena, vector_head(fp_move_from), vector_head(fp_move_to),
          vector_length(fp_move_from), tmp_index);
      gen_moves(irb, basicblock, reg_to_val, fp_moves, tmp_index, spill_lcl,
                IR_REG_TY_FP, early);
    }
  }
}
