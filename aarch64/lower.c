#include "lower.h"

#include "../ir/build.h"
#include "../util.h"

// ARM has no quotient function
// so instead of `x = a % b` we do
// `c = a / b; x = a - (c * b)`
void lower_quot(struct ir_builder *func, struct ir_op *op) {
  debug_assert(op->ty == IR_OP_TY_BINARY_OP &&
                   (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
                    op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT),
               "lower_quot called on invalid op");

  enum ir_op_binary_op_ty div_ty;
  enum ir_op_sign sign = binary_op_sign(op->binary_op.ty);
  switch (sign) {
  case IR_OP_SIGN_NA:
    bug("trying to `lower_quot` but `binary_op_sign` return `IR_OP_SIGN_NA`");
    break;
  case IR_OP_SIGN_SIGNED:
    div_ty = IR_OP_BINARY_OP_TY_SDIV;
    break;
  case IR_OP_SIGN_UNSIGNED:
    div_ty = IR_OP_BINARY_OP_TY_UDIV;
    break;
  }

  // we could directly generate an MSUB here but we instead rely on fusing later

  // c = a / b

  debug("op %zu, pred %zu, succ %zu", op->id,
        op->pred ? op->pred->id : SIZE_T_MAX,
        op->succ ? op->succ->id : SIZE_T_MAX);

  struct ir_op *div =
      insert_before_ir_op(func, op, IR_OP_TY_BINARY_OP, op->var_ty);
  div->binary_op.ty = div_ty;
  div->binary_op.lhs = op->binary_op.lhs;
  div->binary_op.rhs = op->binary_op.rhs;

  // y = c * b
  debug("op %zu, pred %zu, succ %zu", op->id,
        op->pred ? op->pred->id : SIZE_T_MAX,
        op->succ ? op->succ->id : SIZE_T_MAX);
  debug("div %zu, pred %zu, succ %zu", div->id,
        div->pred ? div->pred->id : SIZE_T_MAX,
        div->succ ? div->succ->id : SIZE_T_MAX);

  struct ir_op *mul =
      insert_after_ir_op(func, div, IR_OP_TY_BINARY_OP, op->var_ty);
  mul->binary_op.ty = IR_OP_BINARY_OP_TY_MUL;
  mul->binary_op.lhs = div;
  mul->binary_op.rhs = op->binary_op.rhs;

  // x = a - y
  debug("op %zu, pred %zu, succ %zu", op->id,
        op->pred ? op->pred->id : SIZE_T_MAX,
        op->succ ? op->succ->id : SIZE_T_MAX);
  debug("div %zu, pred %zu, succ %zu", div->id,
        div->pred ? div->pred->id : SIZE_T_MAX,
        div->succ ? div->succ->id : SIZE_T_MAX);
  debug("mul %zu, pred %zu, succ %zu", mul->id,
        mul->pred ? mul->pred->id : SIZE_T_MAX,
        mul->succ ? mul->succ->id : SIZE_T_MAX);

  // Now we replace `op` with `sub` (as `sub` is the op that actually produces
  // the value) this preserves links, as other ops pointing to the div will now
  // point at the sub
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = op->var_ty;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_SUB;
  op->binary_op.lhs = op->binary_op.lhs;
  op->binary_op.rhs = mul;

  debug("op %zu, pred %zu, succ %zu", op->id,
        op->pred ? op->pred->id : SIZE_T_MAX,
        op->succ ? op->succ->id : SIZE_T_MAX);
  debug("div %zu, pred %zu, succ %zu", div->id,
        div->pred ? div->pred->id : SIZE_T_MAX,
        div->succ ? div->succ->id : SIZE_T_MAX);
  debug("mul %zu, pred %zu, succ %zu", mul->id,
        mul->pred ? mul->pred->id : SIZE_T_MAX,
        mul->succ ? mul->succ->id : SIZE_T_MAX);
}

void lower(struct ir_builder *func) {
  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        switch (op->ty) {
        case IR_OP_TY_PHI:
        case IR_OP_TY_CNST:
        case IR_OP_TY_RET:
        case IR_OP_TY_STORE_LCL:
        case IR_OP_TY_LOAD_LCL:
        case IR_OP_TY_BR:
        case IR_OP_TY_BR_COND:
        case IR_OP_TY_MOV:
          break;
        case IR_OP_TY_BINARY_OP:
          if (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
              op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT) {
            lower_quot(func, op);
          }
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}
