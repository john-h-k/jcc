#include "lower.h"

#include "bit_twiddle.h"
#include "ir/ir.h"
#include "log.h"
#include "util.h"

static void propogate_switch_phis(UNUSED struct ir_func *func,
                                  struct ir_basicblock *bb_switch,
                                  struct ir_basicblock *pred_cond,
                                  struct ir_basicblock *basicblock) {
  // FIXME: this does NOT properly propogate
  // it needs to add phis to all intermediates too

  struct ir_stmt *phi_stmt = basicblock->first;

  struct ir_op *phi = phi_stmt ? phi_stmt->first : NULL;
  while (phi && phi->ty == IR_OP_TY_PHI) {
    for (size_t i = 0; i < phi->phi.num_values; i++) {
      if (phi->phi.values[i].basicblock == bb_switch) {
        phi->phi.values[i].basicblock = pred_cond;
      }
    }

    phi = phi->succ;
  }
}

static void lower_br_switch(struct ir_func *func, struct ir_op *op) {
  // lowers a `br.switch` into a series of if-else statements

  struct ir_basicblock *bb = op->stmt->basicblock;
  struct ir_basicblock_switch *bb_switch = &bb->switch_case;

  struct ir_basicblock *prev_bb = op->stmt->basicblock;

  detach_ir_op(func, op);

  struct ir_var_ty var_ty = op->br_switch.value->var_ty;

  size_t num_cases = bb_switch->num_cases;
  struct ir_split_case *split_cases = bb_switch->cases;
  for (size_t i = 0; i < num_cases; i++) {
    struct ir_split_case *split_case = &split_cases[i];

    for (size_t j = 0; j < split_case->target->num_preds; j++) {
      if (split_case->target->preds[j] == bb) {
        // remove pred
        memmove(&split_case->target->preds[j],
                &split_case->target->preds[j + 1],
                (split_case->target->num_preds - j - 1) *
                    sizeof(struct ir_basicblock *));
        split_case->target->num_preds--;
      }
    }

    struct ir_stmt *cmp_stmt = alloc_ir_stmt(func, prev_bb);
    struct ir_op *cnst = alloc_ir_op(func, cmp_stmt);
    cnst->ty = IR_OP_TY_CNST;
    cnst->var_ty = var_ty;
    cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                     .int_value = split_case->value};

    struct ir_op *cmp_op = alloc_ir_op(func, cmp_stmt);
    cmp_op->ty = IR_OP_TY_BINARY_OP;
    cmp_op->var_ty = IR_VAR_TY_I32;
    cmp_op->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_EQ, .lhs = op->br_switch.value, .rhs = cnst};

    struct ir_op *br_op = alloc_ir_op(func, cmp_stmt);
    br_op->ty = IR_OP_TY_BR_COND;
    br_op->var_ty = IR_VAR_TY_NONE;
    br_op->br_cond = (struct ir_op_br_cond){.cond = cmp_op};

    if (i + 1 < num_cases) {
      struct ir_basicblock *next_cond =
          insert_after_ir_basicblock(func, prev_bb);

      propogate_switch_phis(func, bb, prev_bb, split_case->target);
      make_basicblock_split(func, prev_bb, split_case->target, next_cond);

      prev_bb = next_cond;
    } else {
      struct ir_basicblock *default_target = bb_switch->default_target;

      for (size_t j = 0; j < default_target->num_preds; j++) {
        if (bb_switch->default_target->preds[j] == bb) {
          // remove pred
          memmove(&default_target->preds[j], &default_target->preds[j + 1],
                  (default_target->num_preds - j - 1) *
                      sizeof(struct ir_basicblock *));
          default_target->num_preds--;
        }
      }

      propogate_switch_phis(func, bb, prev_bb, split_case->target);
      propogate_switch_phis(func, bb, prev_bb, bb_switch->default_target);
      make_basicblock_split(func, prev_bb, split_case->target,
                            bb_switch->default_target);
    }
  }
}

enum load_bitfield {
  LOAD_BITFIELD_MASK_IN,
  LOAD_BITFIELD_MASK_OUT,
};

static struct ir_op *load_unshifted_bitfield(struct ir_func *func,
                                             struct ir_op *op,
                                             struct ir_bitfield bitfield,
                                             enum load_bitfield load_bitfield) {
  debug_assert(op->var_ty.ty == IR_VAR_TY_TY_POINTER, "expected ptr");

  struct ir_op *load =
      insert_after_ir_op(func, op, IR_OP_TY_LOAD, IR_VAR_TY_I32);
  load->load = (struct ir_op_load){
    .ty = IR_OP_LOAD_TY_ADDR,
    .addr = op};

  unsigned int mask_val;

  switch (load_bitfield) {
  case LOAD_BITFIELD_MASK_IN:
    mask_val = ~MASK_OUT(unsigned, bitfield.width + bitfield.offset, bitfield.offset);
    break;
  case LOAD_BITFIELD_MASK_OUT:
    mask_val = MASK_OUT(unsigned, bitfield.width + bitfield.offset, bitfield.offset);
    break;
  }

  // printf("mask lo %zu = %u\n", offset, bitfield.width, MASK_HI(unsigned, bitfield.width + bitfield.offset, bitfield.offset));
  // printf("mask hi %zu = %u\n", bitfield.offset, bitfield.width, MASK_LO(unsigned, bitfield.width + bitfield.offset, bitfield.offset));
  // bug("mask (%zu, %zu) = %u", bitfield.offset, bitfield.width, mask_val);

  struct ir_op *mask_cnst =
      insert_after_ir_op(func, load, IR_OP_TY_CNST, IR_VAR_TY_I32);
  mask_cnst->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = mask_val};

  struct ir_op *mask = insert_after_ir_op(
      func, mask_cnst, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
  mask->binary_op = (struct ir_op_binary_op){
    .ty = IR_OP_BINARY_OP_TY_AND,
    .lhs = load,
    .rhs = mask_cnst
  };

  return mask;
}

// TODO: signs and stuff are wrong
static void lower_store_bitfield(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = op->store_bitfield.addr;
  struct ir_bitfield bitfield = op->store_bitfield.bitfield;

  struct ir_op *masked_out = load_unshifted_bitfield(func, addr, bitfield, LOAD_BITFIELD_MASK_OUT);
  masked_out->comment = "masked out";

  struct ir_op *shifted_op;
  if (bitfield.offset) {
    struct ir_op *shift_cnst =
        insert_before_ir_op(func, op, IR_OP_TY_CNST, IR_VAR_TY_I32);
    shift_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                           .int_value = bitfield.offset};

    shifted_op =
        insert_before_ir_op(func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
    shifted_op->binary_op =
        (struct ir_op_binary_op){.ty = IR_OP_BINARY_OP_TY_LSHIFT,
                                 .lhs = op->store_bitfield.value,
                                 .rhs = shift_cnst};
  } else {
    shifted_op = op->store_bitfield.value;
  }

  struct ir_op *mask_in =
      insert_before_ir_op(func, op, IR_OP_TY_BINARY_OP, IR_VAR_TY_I32);
  mask_in->binary_op = (struct ir_op_binary_op){
      .ty = IR_OP_BINARY_OP_TY_OR, .lhs = masked_out, .rhs = shifted_op};

  op->ty = IR_OP_TY_STORE;
  op->var_ty = IR_VAR_TY_NONE;
  op->store = (struct ir_op_store){
    .ty = IR_OP_STORE_TY_ADDR,
    .addr = addr,
    .value = mask_in
  };
}

static void lower_load_bitfield(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = op->load_bitfield.addr;
  struct ir_bitfield bitfield = op->load_bitfield.bitfield;

  struct ir_op *masked_in = load_unshifted_bitfield(func, addr, bitfield, LOAD_BITFIELD_MASK_IN);

  if (bitfield.offset) {
    struct ir_op *shift_cnst = insert_after_ir_op(func, masked_in, IR_OP_TY_CNST, IR_VAR_TY_I32);
    shift_cnst->cnst = (struct ir_op_cnst){
      .ty = IR_OP_CNST_TY_INT,
      .int_value = bitfield.offset
    };

    op->ty = IR_OP_TY_BINARY_OP;
    op->var_ty = IR_VAR_TY_I32;
    op->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_URSHIFT, .lhs = masked_in, .rhs = shift_cnst};
  } else {
    // ugly
    op->ty = IR_OP_TY_MOV;
    op->var_ty = IR_VAR_TY_I32;
    op->mov = (struct ir_op_mov){
      .value = masked_in
    };
  }
}

static void lower_store_glb(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr =
      insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->store.glb};

  struct ir_op *value = op->store.value;

  op->store = (struct ir_op_store){
    .ty = IR_OP_STORE_TY_ADDR,
    .addr = addr, .value = value};
}

static void lower_load_glb(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr =
      insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->load.glb};

  op->ty = IR_OP_TY_LOAD;
  op->load = (struct ir_op_load){
    .ty = IR_OP_LOAD_TY_ADDR,
    .addr = addr};
}

void lower(struct ir_unit *unit, const struct target *target) {
  struct ir_glb *glb = unit->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC: {
      struct ir_func *func = glb->func;
      struct ir_basicblock *basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            switch (op->ty) {
            case IR_OP_TY_UNKNOWN:
              bug("unknown op!");
            case IR_OP_TY_UNDF:
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_CNST:
            case IR_OP_TY_PHI:
            case IR_OP_TY_UNARY_OP:
            case IR_OP_TY_BINARY_OP:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_BR:
            case IR_OP_TY_MOV:
            case IR_OP_TY_RET:
            case IR_OP_TY_BR_COND:
            case IR_OP_TY_CALL:
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_BR_SWITCH:
              lower_br_switch(func, op);
              break;
            case IR_OP_TY_STORE:
              if (op->store.ty == IR_OP_STORE_TY_GLB) {
                lower_store_glb(func, op);
              }
              break;
            case IR_OP_TY_LOAD:
              if (op->load.ty == IR_OP_LOAD_TY_GLB) {
                lower_load_glb(func, op);
              }
              break;
            case IR_OP_TY_STORE_BITFIELD:
              lower_store_bitfield(func, op);
              break;
            case IR_OP_TY_LOAD_BITFIELD:
              lower_load_bitfield(func, op);
              break;
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }

      // Final step: turn all TY_POINTER into TY_I64 as the information is no
      // longer needed
      basicblock = func->first;
      while (basicblock) {
        struct ir_stmt *stmt = basicblock->first;

        while (stmt) {
          struct ir_op *op = stmt->first;

          while (op) {
            if (op->var_ty.ty == IR_VAR_TY_TY_POINTER) {
              op->var_ty = var_ty_for_pointer_size(func->unit);
            }

            op = op->succ;
          }

          stmt = stmt->succ;
        }

        basicblock = basicblock->succ;
      }
      break;
    }
    }

    glb = glb->succ;
  }

  // now target-specific lowering
  if (target->lower) {
    target->lower(unit);
  }

  // now we do pruning before regalloc
  glb = unit->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      break;
    case IR_GLB_TY_FUNC: {
      struct ir_func *func = glb->func;

      struct ir_op_uses uses = build_op_uses_map(func);

      for (size_t i = 0; i < uses.num_use_datas; i++) {
        struct ir_op_use *use = &uses.use_datas[i];

        if (!use->num_uses && !op_has_side_effects(use->op)) {
          debug_assert(!(use->op->flags & IR_OP_FLAG_CONTAINED),
                       "contained op must have uses");

          debug("detaching op %zu", use->op->id);
          detach_ir_op(func, use->op);
        }
      }

      prune_basicblocks(func);
    }
    }
    glb = glb->succ;
  }

  // TODO: more IR validation good
  // e.g no type mismatches (like in build)
  // but also making sure no ops use ops from in front of them
}
