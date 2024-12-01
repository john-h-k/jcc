#include "lower.h"

#include "../ir/build.h"
#include "../util.h"

static void lower_quot(struct ir_func *func, struct ir_op *op) {
  debug_assert(op->ty == IR_OP_TY_BINARY_OP &&
                   (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
                    op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT),
               "lower_quot called on invalid op");

  enum ir_op_binary_op_ty div_ty;
  enum ir_op_sign sign = binary_op_sign(op->binary_op.ty);
  switch (sign) {
  case IR_OP_SIGN_NA:
    bug("trying to `lower_quot` but `binary_op_sign` return `IR_OP_SIGN_NA`");
  case IR_OP_SIGN_SIGNED:
    div_ty = IR_OP_BINARY_OP_TY_SDIV;
    break;
  case IR_OP_SIGN_UNSIGNED:
    div_ty = IR_OP_BINARY_OP_TY_UDIV;
    break;
  }

  // we could directly generate an MSUB here but we instead rely on fusing later

  // c = a / b

  struct ir_op *div =
      insert_before_ir_op(func, op, IR_OP_TY_BINARY_OP, op->var_ty);
  div->binary_op.ty = div_ty;
  div->binary_op.lhs = op->binary_op.lhs;
  div->binary_op.rhs = op->binary_op.rhs;

  // y = c * b

  struct ir_op *mul =
      insert_after_ir_op(func, div, IR_OP_TY_BINARY_OP, op->var_ty);
  mul->binary_op.ty = IR_OP_BINARY_OP_TY_MUL;
  mul->binary_op.lhs = div;
  mul->binary_op.rhs = op->binary_op.rhs;

  // x = a - y

  // Now we replace `op` with `sub` (as `sub` is the op that actually produces
  // the value) this preserves links, as other ops pointing to the div will now
  // point at the sub
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = op->var_ty;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_SUB;
  op->binary_op.lhs = op->binary_op.lhs;
  op->binary_op.rhs = mul;
}

enum rv32i_lower_stage { RV32I_LOWER_STAGE_QUOT, RV32I_LOWER_STAGE_ALL };

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

      for (enum rv32i_lower_stage stage = RV32I_LOWER_STAGE_QUOT;
           stage <= RV32I_LOWER_STAGE_ALL; stage++) {

        struct ir_basicblock *basicblock = func->first;
        while (basicblock) {
          struct ir_stmt *stmt = basicblock->first;

          while (stmt) {
            struct ir_op *op = stmt->first;

            while (op) {
              if (stage == RV32I_LOWER_STAGE_QUOT) {
                if (op->ty == IR_OP_TY_BINARY_OP &&
                    (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
                     op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT)) {
                  lower_quot(func, op);
                }
              } else {

                switch (op->ty) {
                case IR_OP_TY_UNKNOWN:
                  bug("unknown op!");
                case IR_OP_TY_UNDF:
                case IR_OP_TY_CUSTOM:
                case IR_OP_TY_PHI:
                case IR_OP_TY_CNST:
                case IR_OP_TY_RET:
                case IR_OP_TY_STORE_LCL:
                case IR_OP_TY_LOAD_LCL:
                case IR_OP_TY_STORE_ADDR:
                case IR_OP_TY_LOAD_ADDR:
                case IR_OP_TY_ADDR:
                case IR_OP_TY_BR:
                case IR_OP_TY_BR_SWITCH:
                case IR_OP_TY_MOV:
                case IR_OP_TY_UNARY_OP:
                case IR_OP_TY_BR_COND:
                case IR_OP_TY_CAST_OP:
                  break;
                case IR_OP_TY_CALL:
                  todo("call");
                case IR_OP_TY_BINARY_OP:
                  switch (op->binary_op.ty) {
                  case IR_OP_BINARY_OP_TY_UQUOT:
                  case IR_OP_BINARY_OP_TY_SQUOT:
                    lower_quot(func, op);
                    break;
                  default:
                    break;
                  }
                  break;
                case IR_OP_TY_STORE_GLB:
                case IR_OP_TY_LOAD_GLB:
                  bug("should have been lowered");
                }
              }

              op = op->succ;
            }

            stmt = stmt->succ;
          }

          basicblock = basicblock->succ;
        }
      }
    }
    }

    glb = glb->succ;
  }
}
