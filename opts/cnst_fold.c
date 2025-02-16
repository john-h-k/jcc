#include "cnst_fold.h"

#include "opts.h"
#include "../util.h"

static unsigned long long round_integral(struct ir_func *func,
                                         unsigned long long value,
                                         struct ir_var_ty var_ty) {
  enum ir_var_primitive_ty primitive;
  if (var_ty.ty == IR_VAR_TY_TY_POINTER) {
    primitive = var_ty_pointer_primitive_ty(func->unit);
  } else if (var_ty.ty == IR_VAR_TY_TY_PRIMITIVE) {
    primitive = var_ty.primitive;
  } else {
    BUG("called with non primitive/ptr type");
  }

  switch (primitive) {
  case IR_VAR_PRIMITIVE_TY_I8:
    return (uint8_t)value;
  case IR_VAR_PRIMITIVE_TY_I16:
    return (uint16_t)value;
  case IR_VAR_PRIMITIVE_TY_I32:
    return (uint32_t)value;
  case IR_VAR_PRIMITIVE_TY_I64:
    return (uint64_t)value;
  default:
    unreachable();
  }
}

static bool opts_cnst_fold_binary_op(struct ir_func *func, struct ir_op *op) {
  enum ir_op_binary_op_ty ty = op->binary_op.ty;
  struct ir_op *lhs = op->binary_op.lhs;
  struct ir_op *rhs = op->binary_op.rhs;
  struct ir_var_ty var_ty = op->var_ty;

  unsigned long long lhs_cnst, rhs_cnst;

  if (lhs->ty != IR_OP_TY_CNST || !var_ty_is_integral(&lhs->var_ty) ||
      rhs->ty != IR_OP_TY_CNST || !var_ty_is_integral(&rhs->var_ty)) {
    return false;
  }

  lhs_cnst = lhs->cnst.int_value;
  rhs_cnst = rhs->cnst.int_value;

  unsigned long long value;
#define CNST_FLD_BINOP(ty, op)                                                 \
  case IR_OP_BINARY_OP_TY_##ty:                                                \
    value = lhs_cnst op rhs_cnst;                                              \
    break;

#define CNST_FLD_SBINOP(ty, op)                                                \
  case IR_OP_BINARY_OP_TY_##ty:                                                \
    value = (signed long long)lhs_cnst op(signed long long) rhs_cnst;          \
    break;

  switch (ty) {
    CNST_FLD_BINOP(MUL, *);
    CNST_FLD_BINOP(ADD, +);
    CNST_FLD_BINOP(SUB, -);
    CNST_FLD_BINOP(EQ, ==);
    CNST_FLD_BINOP(NEQ, !=);
    CNST_FLD_BINOP(UGT, >);
    CNST_FLD_SBINOP(SGT, >);
    CNST_FLD_BINOP(UGTEQ, >=);
    CNST_FLD_SBINOP(SGTEQ, >=);
    CNST_FLD_BINOP(ULT, <);
    CNST_FLD_SBINOP(SLT, <);
    CNST_FLD_BINOP(ULTEQ, <=);
    CNST_FLD_SBINOP(SLTEQ, <=);
    CNST_FLD_BINOP(LSHIFT, <<);
    CNST_FLD_SBINOP(SRSHIFT, >>);
    CNST_FLD_BINOP(URSHIFT, >>);
    CNST_FLD_BINOP(AND, &);
    CNST_FLD_BINOP(OR, |);
    CNST_FLD_BINOP(XOR, ^);
    CNST_FLD_SBINOP(SDIV, /);
    CNST_FLD_BINOP(UDIV, /);
    CNST_FLD_SBINOP(SQUOT, %);
    CNST_FLD_BINOP(UQUOT, %);
  default:
    BUG("bad type");
  }
#undef CNST_FLD_BINOP

  value = round_integral(func, value, op->var_ty);
  mk_integral_constant(func->unit, op, op->var_ty.primitive, value);
  op->var_ty = var_ty;

  return true;
}

static bool opts_cnst_fold_op(struct ir_func *func, struct ir_op *op) {
  switch (op->ty) {
    case IR_OP_TY_BINARY_OP:
      return opts_cnst_fold_binary_op(func, op);
    default:
      return false;
  }
}


void opts_cnst_fold(struct ir_unit *unit) {
  struct opts_pass pass = {
    .name = __func__,
    .op_callback = opts_cnst_fold_op
  };

  opts_run_pass(unit, &pass);
}
