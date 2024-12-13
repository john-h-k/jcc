#include "lower.h"

#include "../ir/build.h"
#include "../util.h"

static void lower_comparison(struct ir_func *irb, struct ir_op *op) {
  invariant_assert(op->ty == IR_OP_TY_BINARY_OP &&
                       binary_op_is_comparison(op->binary_op.ty),
                   "non comparison op");

  if (op->flags & IR_OP_FLAG_CONTAINED) {
    return;
  }

  struct ir_op *br_cond = op->succ;
  if (br_cond && br_cond->ty == IR_OP_TY_BR_COND) {
    // contain within branch
    struct ir_op *contained = alloc_contained_ir_op(irb, op, br_cond);

    br_cond->br_cond.cond = contained;
  }
}

static void lower_fp_cnst(struct ir_func *func, struct ir_op *op) {
  // transform into creating an integer, and then mov to float reg

  struct ir_var_ty int_ty;
  unsigned long long int_value;

  DEBUG_ASSERT(var_ty_is_fp(&op->var_ty), "float constant not fp type?");

  switch (op->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_F32: {
    int_ty = IR_VAR_TY_I32;

    union {
      float f;
      unsigned u;
    } v;
    v.f = (float)op->cnst.flt_value;
    int_value = v.u;

    break;
  }
  case IR_VAR_PRIMITIVE_TY_F64: {
    int_ty = IR_VAR_TY_I64;

    union {
      double d;
      unsigned long long ull;
    } v;
    v.d = (double)op->cnst.flt_value;
    int_value = v.ull;

    break;
  }
  default:
    unreachable();
  }

  struct ir_op *int_mov = insert_before_ir_op(func, op, IR_OP_TY_CNST, int_ty);
  int_mov->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = int_value};

  op->ty = IR_OP_TY_MOV;
  op->mov = (struct ir_op_mov){.value = int_mov};
}

void rv32i_lower(struct ir_unit *unit) {
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
            case IR_OP_TY_PHI:
            case IR_OP_TY_RET:
            case IR_OP_TY_STORE:
            case IR_OP_TY_LOAD:
            case IR_OP_TY_STORE_BITFIELD:
            case IR_OP_TY_LOAD_BITFIELD:
            case IR_OP_TY_BITFIELD_EXTRACT:
            case IR_OP_TY_BITFIELD_INSERT:
            case IR_OP_TY_ADDR:
            case IR_OP_TY_BR:
            case IR_OP_TY_BR_SWITCH:
            case IR_OP_TY_MOV:
            case IR_OP_TY_UNARY_OP:
            case IR_OP_TY_BR_COND:
            case IR_OP_TY_CAST_OP:
              break;
            case IR_OP_TY_CNST: {
              if (op->cnst.ty == IR_OP_CNST_TY_FLT) {
                lower_fp_cnst(func, op);
                break;
              }

              break;
            }
            case IR_OP_TY_BINARY_OP:
              if (binary_op_is_comparison(op->binary_op.ty)) {
                lower_comparison(func, op);
              }
              break;
            case IR_OP_TY_CALL:
              TODO("call");
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
}
