#include "lower.h"

#include "bit_twiddle.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
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

static void lower_mem_set(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = op->mem_set.addr;

  struct ir_glb *memset =
      add_well_known_global(func->unit, IR_WELL_KNOWN_GLB_MEMSET);

  struct ir_op *value_cnst = insert_after_ir_op(func, addr, IR_OP_TY_CNST, IR_VAR_TY_I32);
  value_cnst->cnst = (struct ir_op_cnst){
    .ty = IR_OP_CNST_TY_INT,
    .int_value = op->mem_set.value
  };

  struct ir_var_ty ptr_int = var_ty_for_pointer_size(func->unit);

  struct ir_op *length_cnst = insert_after_ir_op(func, addr, IR_OP_TY_CNST, ptr_int);
  length_cnst->cnst = (struct ir_op_cnst){
    .ty = IR_OP_CNST_TY_INT,
    .int_value = op->mem_set.length
  };

  struct ir_op *memset_addr = insert_after_ir_op(func, length_cnst, IR_OP_TY_ADDR, ptr_int);
  memset_addr->flags |= IR_OP_FLAG_CONTAINED;
  memset_addr->addr = (struct ir_op_addr){
    .ty = IR_OP_ADDR_TY_GLB,
    .glb = memset,
  };

  size_t num_args = 3;
  struct ir_op **args = arena_alloc(func->arena, sizeof(struct ir_op *) * num_args);

  args[0] = addr;
  args[1] = value_cnst;
  args[2] = length_cnst;

  op->ty = IR_OP_TY_CALL;
  op->var_ty = *memset->var_ty.func.ret_ty;
  op->call = (struct ir_op_call){
    .target = memset_addr,
    .num_args = num_args,
    .args = args,
    .func_ty = memset->var_ty,
  };
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

// TODO: signs and stuff are wrong
static void lower_store_bitfield(struct ir_func *func, struct ir_op *op) {
  struct ir_op *value = op->store_bitfield.value;
  struct ir_op *addr = build_addr(func, op);

  struct ir_bitfield bitfield = op->store_bitfield.bitfield;

  struct ir_op *load = insert_before_ir_op(func, op, IR_OP_TY_LOAD, value->var_ty);
  load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};

  op->ty = IR_OP_TY_BITFIELD_INSERT;
  op->var_ty = value->var_ty;
  op->bitfield_insert = (struct ir_op_bitfield_insert){
    .value = value,
    .target = load,
    .bitfield = bitfield
  };

  struct ir_op *store = insert_after_ir_op(func, op, IR_OP_TY_STORE, IR_VAR_TY_NONE);
  store->store = (struct ir_op_store){.ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = op};
}

static void lower_load_bitfield(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = build_addr(func, op);

  struct ir_bitfield bitfield = op->load_bitfield.bitfield;

  struct ir_op *load = insert_before_ir_op(func, op, IR_OP_TY_LOAD, op->var_ty);
  load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};

  op->ty = IR_OP_TY_BITFIELD_EXTRACT;
  op->var_ty = op->var_ty;
  op->bitfield_extract = (struct ir_op_bitfield_extract){
    .value = load,
    .bitfield = bitfield
  };
}

static void lower_store_glb(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr =
      insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->store.glb};

  struct ir_op *value = op->store.value;

  op->store = (struct ir_op_store){
      .ty = IR_OP_STORE_TY_ADDR, .addr = addr, .value = value};
}

static void lower_load_glb(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr =
      insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->load.glb};

  op->ty = IR_OP_TY_LOAD;
  op->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};
}

static void lower_pointers(struct ir_unit *unit) {
  // Final step: turn all TY_POINTER into TY_I64 as the information is no
  // longer needed
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
              BUG("unknown op!");
            case IR_OP_TY_UNDF:
            case IR_OP_TY_CUSTOM:
            case IR_OP_TY_CNST:
            case IR_OP_TY_PHI:
            case IR_OP_TY_UNARY_OP:
            case IR_OP_TY_BINARY_OP:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_ADDR_OFFSET:
            case IR_OP_TY_BR:
            case IR_OP_TY_MOV:
            case IR_OP_TY_RET:
            case IR_OP_TY_BITFIELD_EXTRACT:
            case IR_OP_TY_BITFIELD_INSERT:
            case IR_OP_TY_BR_COND:
            case IR_OP_TY_CALL:
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_MEM_SET:
              lower_mem_set(func, op);
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
    }
    }

    glb = glb->succ;
  }

  lower_pointers(unit);

  // now target-specific lowering
  if (target->lower) {
    target->lower(unit);
  }

  // lower again, in case target lowering introduced any
  lower_pointers(unit);

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
          DEBUG_ASSERT(!(use->op->flags & IR_OP_FLAG_CONTAINED),
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
