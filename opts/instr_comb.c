#include "../util.h"
#include "../log.h"
#include "../ir/prettyprint.h"
#include "opts.h"
#include "instr_comb.h"

static bool opts_instr_comb_binary_op(struct ir_func *func, struct ir_op *op) {
  enum ir_op_binary_op_ty ty = op->binary_op.ty;
  struct ir_op *lhs = op->binary_op.lhs;
  struct ir_op *rhs = op->binary_op.rhs;

  switch (ty) {
  case IR_OP_BINARY_OP_TY_URSHIFT:
  case IR_OP_BINARY_OP_TY_SRSHIFT:
  case IR_OP_BINARY_OP_TY_LSHIFT: {
    if (rhs->ty == IR_OP_TY_CNST && var_ty_is_integral(&rhs->var_ty) && rhs->cnst.int_value == 0) {
      op->ty = IR_OP_TY_MOV;
      op->mov = (struct ir_op_mov){
        .value = lhs
      };
    }
    return true;
  }
  case IR_OP_BINARY_OP_TY_MUL: {
    struct ir_op *cnst;
    bool is_lhs;
    if (lhs->ty == IR_OP_TY_CNST && var_ty_is_integral(&lhs->var_ty)) {
      cnst = lhs;
      is_lhs = true;
    } else if (rhs->ty == IR_OP_TY_CNST && var_ty_is_integral(&rhs->var_ty)) {
      cnst = rhs;
      is_lhs = false;
    } else {
      break;
    }

    unsigned long long value = cnst->cnst.int_value;
    if (value == 0) {
      mk_integral_constant(func->unit, op, op->var_ty.primitive, 0);
      return true;
    } else if (value == 1) {
      op->ty = IR_OP_TY_MOV;
      op->mov = (struct ir_op_mov){
        .value = is_lhs ? rhs : lhs
      };
      return true;
    } else if (ISPOW2(value)) {
      struct ir_op *shift_cnst = insert_before_ir_op(func, op, IR_OP_TY_CNST, IR_VAR_TY_I32);
      mk_integral_constant(func->unit, shift_cnst, IR_VAR_PRIMITIVE_TY_I32, ILOG2(value));

      op->binary_op.ty = IR_OP_BINARY_OP_TY_LSHIFT;
      op->binary_op.lhs = is_lhs ? rhs : lhs;
      op->binary_op.rhs = shift_cnst;
      return true;
    }

    break;
  }
  // TODO: optimise `SDIV`
  case IR_OP_BINARY_OP_TY_UDIV: {
    struct ir_op *cnst;
    bool is_lhs;
    if (lhs->ty == IR_OP_TY_CNST && var_ty_is_integral(&lhs->var_ty)) {
      cnst = lhs;
      is_lhs = true;
    } else if (rhs->ty == IR_OP_TY_CNST && var_ty_is_integral(&rhs->var_ty)) {
      cnst = rhs;
      is_lhs = false;
    } else {
      break;
    }

    unsigned long long value = cnst->cnst.int_value;
    if (value == 1) {
      op->ty = IR_OP_TY_MOV;
      op->mov = (struct ir_op_mov){
        .value = is_lhs ? rhs : lhs
      };
      return true;
    } else if (ISPOW2(value)) {
      struct ir_op *shift_cnst = insert_before_ir_op(func, op, IR_OP_TY_CNST, IR_VAR_TY_I32);
      mk_integral_constant(func->unit, shift_cnst, IR_VAR_PRIMITIVE_TY_I32, ILOG2(value));

      op->binary_op.ty = IR_OP_BINARY_OP_TY_URSHIFT;
      op->binary_op.lhs = is_lhs ? rhs : lhs;
      op->binary_op.rhs = shift_cnst;
      return true;
    }

    break;
  }
  default:
    break;
  }

  return false;
}

static bool opts_instr_comb_op(struct ir_func *func, struct ir_op *op) {
  switch (op->ty) {
    case IR_OP_TY_BINARY_OP:
      return opts_instr_comb_binary_op(func, op);
    default:
      return false;
  }
}


void opts_instr_comb(struct ir_unit *unit) {
  struct opts_pass pass = {
    .name = __func__,
    .op_callback = opts_instr_comb_op
  };

  opts_run_pass(unit, &pass);
}
