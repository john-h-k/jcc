#include "lower.h"

#include "ir/ir.h"

typedef int a;

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
        // remove
        memcpy(&split_case->target->preds[j], &split_case->target->preds[j + 1], split_case->target->num_preds - j);
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


      make_basicblock_split(func, prev_bb, split_case->target, next_cond);

      prev_bb = next_cond;
    } else {
      make_basicblock_split(func, prev_bb, split_case->target,
                                  bb_switch->default_target);
    }
  }
}

static void lower_store_glb(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->store_glb.glb};

  struct ir_op *value = op->store_glb.value;

  op->ty = IR_OP_TY_STORE_ADDR;
  op->store_addr = (struct ir_op_store_addr){.addr = addr, .value = value};
}

static void lower_load_glb(struct ir_func *func, struct ir_op *op) {
  struct ir_op *addr = insert_before_ir_op(func, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->load_glb.glb};

  op->ty = IR_OP_TY_LOAD_ADDR;
  op->load_addr = (struct ir_op_load_addr){.addr = addr};
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
            case IR_OP_TY_STORE_ADDR:
            case IR_OP_TY_LOAD_ADDR:
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
            case IR_OP_TY_STORE_LCL:
            case IR_OP_TY_LOAD_LCL:
              break;
            case IR_OP_TY_STORE_GLB:
              lower_store_glb(func, op);
              break;
            case IR_OP_TY_LOAD_GLB:
              lower_load_glb(func, op);
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
          debug("detaching op %zu", use->op->id);
          detach_ir_op(func, use->op);
        }
      }
    }
    }
    glb = glb->succ;
  }
}
