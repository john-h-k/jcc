#include "eliminate_phi.h"

#include "../bitset.h"
#include "../hashtbl.h"
#include "../vector.h"
#include "ir.h"
#include "prettyprint.h"

struct bb_reg {
  size_t reg;
  struct ir_basicblock *bb;
};

#define REG_POS(reg) (reg)
#define LCL_POS(lcl) (SIZE_MAX - 1 - lcl)

// FIXME: known bug - when lots of spills happen, we get spilled phi with spilled source, which hits asserts in here

static void gen_moves(struct ir_func *irb, struct ir_basicblock *basicblock,
                      struct hashtbl *reg_to_val, struct move_set moves,
                      size_t tmp_index, struct ir_lcl *spill_lcl,
                      enum ir_reg_ty reg_ty, bool early_moves) {
  if (!moves.num_moves) {
    return;
  }

  struct ir_stmt *stmt;
  if (early_moves) {
    stmt = basicblock->first;
  } else if (basicblock->last && basicblock->last->pred && (basicblock->last->pred->flags & IR_STMT_FLAG_PHI_MOV)) {
    stmt = basicblock->last->pred;
  } else {
    stmt = ir_insert_before_stmt(irb, basicblock->last);
    stmt->flags |= IR_STMT_FLAG_PHI_MOV;
  }

  // size_t next_free;
  struct ir_var_ty lcl_ty;
  struct reg_set_info reg_info;
  switch (reg_ty) {
  case IR_REG_TY_INTEGRAL:
    // next_free = irb->reg_usage.num_volatile_used;
    lcl_ty = ir_var_ty_for_pointer_size(irb->unit);
    reg_info = irb->unit->target->reg_info.gp_registers;
    break;
  case IR_REG_TY_FP:
    // next_free = irb->reg_usage.num_volatile_used;
    lcl_ty = IR_VAR_TY_F64;
    reg_info = irb->unit->target->reg_info.fp_registers;
    break;
  default:
    unreachable();
  }

  struct ir_var_ty store_var_ty;

  // size_t next_free = bitset_length(reg_usage) - bitset_lzcnt(reg_usage);
  size_t next_free = SIZE_MAX;
  // FIXME: the "try find free vol reg" logic is disabled

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
      spill_lcl = ir_add_local(irb, &lcl_ty);
    }

    if (move.to.idx == tmp_index) {
      DEBUG_ASSERT(move.from.idx < SIZE_MAX / 2, "can't do stack<->stack move");

      if (has_free_reg) {
        struct ir_op *mov = ir_append_op(irb, stmt, IR_OP_TY_MOV, lcl_ty);
        mov->mov = (struct ir_op_mov){.value = value};
        mov->reg = free_reg;
        mov->flags |= IR_OP_FLAG_PHI_MOV | IR_OP_FLAG_SIDE_EFFECTS;
        // store_var_ty = value->var_ty;
        store_var_ty = lcl_ty;

        tmp_mov = mov;
      } else {
        struct ir_op *addr = ir_append_op(irb, stmt, IR_OP_TY_ADDR,
                                          ir_var_ty_for_pointer_size(irb->unit));
        addr->addr = (struct ir_op_addr){
            .ty = IR_OP_ADDR_TY_LCL,
            .lcl = spill_lcl,
        };
        addr->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = irb->unit->target->reg_info.ssp};

        struct ir_op *store =
            ir_append_op(irb, stmt, IR_OP_TY_STORE, IR_VAR_TY_NONE);
        store->store = (struct ir_op_store){
            .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = value};
        store->flags |= IR_OP_FLAG_PHI_MOV | IR_OP_FLAG_SIDE_EFFECTS;
        // store_var_ty = value->var_ty;
        store_var_ty = lcl_ty;
      }
    } else if (move.from.idx == tmp_index) {
      DEBUG_ASSERT(move.to.idx < SIZE_MAX / 2, "can't do stack<->stack move");

      struct ir_op *phi_op;

      if (has_free_reg) {
        struct ir_op *mov = ir_append_op(irb, stmt, IR_OP_TY_MOV, lcl_ty);
        mov->mov = (struct ir_op_mov){.value = tmp_mov};
        mov->reg = to;
        mov->flags |= IR_OP_FLAG_PHI_MOV | IR_OP_FLAG_SIDE_EFFECTS;
        store_var_ty = value->var_ty;

        phi_op = mov;
      } else {
        struct ir_op *addr = ir_append_op(irb, stmt, IR_OP_TY_ADDR,
                                          ir_var_ty_for_pointer_size(irb->unit));
        addr->addr = (struct ir_op_addr){
            .ty = IR_OP_ADDR_TY_LCL,
            .lcl = spill_lcl,
        };
        addr->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = irb->unit->target->reg_info.ssp};

        struct ir_op *load =
            ir_append_op(irb, stmt, IR_OP_TY_LOAD, store_var_ty);
        load->reg = to;
        load->load =
            (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};
        load->flags |= IR_OP_FLAG_SIDE_EFFECTS;

        phi_op = load;
      }

      struct bb_reg key = {.reg = move.to.idx, .bb = basicblock};
      struct ir_op **op = hashtbl_lookup(reg_to_val, &key);

      if (op) {
        *op = phi_op;
      }
    } else if (move.from.idx >= SIZE_MAX / 2) {
      DEBUG_ASSERT(move.to.idx < SIZE_MAX / 2, "can't do stack<->stack move");

      struct ir_lcl *lcl = move.from.metadata[0];

      struct ir_op *addr = ir_append_op(irb, stmt, IR_OP_TY_ADDR,
                                        ir_var_ty_for_pointer_size(irb->unit));
      addr->addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = lcl,
      };
      addr->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                  .idx = irb->unit->target->reg_info.ssp};

      struct ir_op *load = ir_append_op(irb, stmt, IR_OP_TY_LOAD, lcl->var_ty);
      load->reg = to;
      load->load = (struct ir_op_load){
          .ty = IR_OP_LOAD_TY_ADDR,
          .addr = addr,
      };
      load->flags |= IR_OP_FLAG_SIDE_EFFECTS;
    } else if (move.to.idx >= SIZE_MAX / 2) {
      struct ir_lcl *lcl = move.to.metadata[0];

      struct ir_op *addr = ir_append_op(irb, stmt, IR_OP_TY_ADDR,
                                        ir_var_ty_for_pointer_size(irb->unit));
      addr->addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = lcl,
      };
      addr->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                  .idx = irb->unit->target->reg_info.ssp};

      struct ir_op *store =
          ir_insert_after_op(irb, addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
      store->store = (struct ir_op_store){
          .ty = IR_OP_STORE_TY_ADDR,
          .value = value,
          .addr = addr,
      };
      store->flags |= IR_OP_FLAG_SIDE_EFFECTS;
    } else {
      struct ir_op *mov = ir_append_op(irb, stmt, IR_OP_TY_MOV, value->var_ty);
      mov->flags |= IR_OP_FLAG_PHI_MOV | IR_OP_FLAG_SIDE_EFFECTS;
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
  ir_rebuild_ids(irb);

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

  // TODO: lots of code cleaning up here especially the double loop

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    bool moves_in_preds;

    if (basicblock->num_preds == 1) {
      moves_in_preds = false;
    } else if (basicblock->num_preds > 1) {
      moves_in_preds = true;
      basicblock = basicblock->succ;
      continue;
    } else {
      // dead code
      basicblock = basicblock->succ;
      continue;
    }

    if (stmt && stmt->flags & IR_STMT_FLAG_PHI) {
      // phis always at start of bb
      struct ir_op *op = stmt->first;

      while (op) {
        DEBUG_ASSERT(op->ty == IR_OP_TY_PHI, "expected phi in phi stmt");

        for (size_t i = 0; i < op->phi.num_values; i++) {
          struct ir_phi_entry entry = op->phi.values[i];
          struct ir_op *value = entry.value;

          struct ir_basicblock *mov_bb;
          size_t bb_move_id;
          if (moves_in_preds) {
            mov_bb = entry.basicblock;
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
              from_pos = REG_POS(ir_unique_idx_for_reg(value->reg));
            }

            if (op->lcl) {
              to_pos = LCL_POS(op->lcl->id);
              to_metadata = op->lcl;
            } else {
              to_pos = REG_POS(ir_unique_idx_for_reg(op->reg));
            }

            if (from_pos != to_pos) {
              struct location from = {.idx = from_pos,
                                      .metadata[0] = from_metadata};
              struct location to = {.idx = to_pos, .metadata[0] = to_metadata};

              struct bb_reg key = {.reg = from_pos, .bb = mov_bb};
              hashtbl_insert(reg_to_val, &key, &value);

              if (ir_var_ty_is_integral(&value->var_ty)) {
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
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    bool moves_in_preds;

    if (basicblock->num_preds == 1) {
      moves_in_preds = false;
      basicblock = basicblock->succ;
      continue;
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
          struct ir_phi_entry entry = op->phi.values[i];
          struct ir_op *value = entry.value;

          struct ir_basicblock *mov_bb;
          size_t bb_move_id;
          if (moves_in_preds) {
            mov_bb = entry.basicblock;
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
              from_pos = REG_POS(ir_unique_idx_for_reg(value->reg));
            }

            if (op->lcl) {
              to_pos = LCL_POS(op->lcl->id);
              to_metadata = op->lcl;
            } else {
              to_pos = REG_POS(ir_unique_idx_for_reg(op->reg));
            }

            if (from_pos != to_pos) {
              struct location from = {.idx = from_pos,
                                      .metadata[0] = from_metadata};
              struct location to = {.idx = to_pos, .metadata[0] = to_metadata};

              struct bb_reg key = {.reg = from_pos, .bb = mov_bb};
              hashtbl_insert(reg_to_val, &key, &value);

              if (ir_var_ty_is_integral(&value->var_ty)) {
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

  // we (+ regalloc) may have introduced new locals, so allocate them
  ir_alloc_locals(irb);

  hashtbl_free(&reg_to_val);

  for (size_t i = 0; i < irb->basicblock_count * 2; i++) {
    vector_free(&bb_moves[i].gp_from);
    vector_free(&bb_moves[i].gp_to);
    vector_free(&bb_moves[i].fp_from);
    vector_free(&bb_moves[i].fp_to);
  }
}
