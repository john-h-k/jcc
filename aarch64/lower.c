#include "lower.h"
#include "../ir.h"
#include "../util.h"

// ARM has no quotient function
// so instead of `x = a % b` we do
// `c = a / b; x = a - (c * b)`
void lower_quot(struct ir_builder *func, struct ir_stmt *stmt, struct ir_op *op) {
  debug_assert(op->ty == IR_OP_TY_BINARY_OP &&
                   (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
                    op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT),
               "lower_quot called on invalid op");

  enum ir_op_binary_op_ty div_ty;
  (void)div_ty;
  (void)op;
  (void)func;
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

  struct ir_op *div = alloc_ir_op(func, stmt);
  div->ty = IR_OP_TY_BINARY_OP;
  div->var_ty = op->var_ty;
  div->binary_op.ty = div_ty;
  div->binary_op.lhs = op->binary_op.lhs;
  div->binary_op.rhs = op->binary_op.rhs;

  // op->pred->succ = div;
  // div->pred = op->pred;

  // y = c * b

  struct ir_op *mul = alloc_ir_op(func, stmt);
  mul->ty = IR_OP_TY_BINARY_OP;
  mul->var_ty = op->var_ty;
  mul->binary_op.ty = IR_OP_BINARY_OP_TY_MUL;
  mul->binary_op.lhs = div;
  mul->binary_op.rhs = op->binary_op.rhs;

  // mul->pred = div;
  // div->succ = mul;

  // x = a - y

  // Now we replace `op` with `sub` (as `sub` is the op that actually produces
  // the value) this preserves links, as other ops pointing to the div will now
  // point at the sub
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = op->var_ty;
  op->binary_op.ty = IR_OP_BINARY_OP_TY_SUB;
  op->binary_op.lhs = op->binary_op.lhs;
  op->binary_op.rhs = mul;

  // op->pred = mul;
  // mul->succ = op;

  // err("%d -> %d", mul->id, mul->succ->id);
  // err("%d -> %d", div->id, div->succ->id);
  // err("%d -> %d", op->id, op->succ->id);

  // func->last = last;
}

void lower(struct ir_builder *func) {
  struct ir_stmt *stmt = func->first;

  while (stmt) {
    struct ir_op *op = stmt->first;

    while (op) {
      debug("%zu", op->id);
      switch (op->ty) {
      case IR_OP_TY_PHI:
      case IR_OP_TY_CNST:
      case IR_OP_TY_RET:
        break;
      case IR_OP_TY_BINARY_OP:
        if (op->binary_op.ty == IR_OP_BINARY_OP_TY_UQUOT ||
            op->binary_op.ty == IR_OP_BINARY_OP_TY_SQUOT) {
          lower_quot(func, stmt, op);
        }
      }

      op = op->succ;
    }

    stmt = stmt->succ;
  }
}
