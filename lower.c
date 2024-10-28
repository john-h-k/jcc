#include "lower.h"

static void lower_store_glb(struct ir_builder *func, struct ir_op *op) {
  struct ir_op_var_ty pointer_ty = var_ty_make_pointer(func, &op->var_ty);

  struct ir_op *addr = insert_before_ir_op(func, op, IR_OP_TY_ADDR, pointer_ty);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->store_glb.glb};

  struct ir_op *value = op->store_glb.value;

  op->ty = IR_OP_TY_STORE_ADDR;
  op->store_addr = (struct ir_op_store_addr){.addr = addr, .value = value};
}

static void lower_load_glb(struct ir_builder *func, struct ir_op *op) {
  struct ir_op_var_ty pointer_ty = var_ty_make_pointer(func, &op->var_ty);

  struct ir_op *addr = insert_before_ir_op(func, op, IR_OP_TY_ADDR, pointer_ty);
  addr->addr =
      (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = op->load_glb.glb};

  op->ty = IR_OP_TY_LOAD_ADDR;
  op->load_addr = (struct ir_op_load_addr){.addr = addr};
}

// Most architectures require turning `br.cond <true> <false>` into 2 instructions
// we represent this as just the `true` part of the `br.cond`, and then a `br`
// after branching to the false target
static void lower_br_cond(struct ir_builder *irb, struct ir_op *op) {
  if (1 == 2)
  insert_after_ir_op(irb, op, IR_OP_TY_BR, IR_OP_VAR_TY_NONE);
}


void lower(struct ir_unit *unit) {
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
      struct ir_builder *func = glb->func;
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
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_BR_COND:
              lower_br_cond(func, op);
              break;
            case IR_OP_TY_CALL:
              // lower_call(func, op);
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
            if (op->var_ty.ty == IR_OP_VAR_TY_TY_POINTER) {
              op->var_ty = var_ty_for_pointer_size(func);
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
