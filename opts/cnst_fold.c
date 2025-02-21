#include "cnst_fold.h"

#include "../util.h"
#include "opts.h"

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

static struct ir_op *opts_follow_movs(struct ir_op *op) {
  while (op->ty == IR_OP_TY_MOV) {
    if (!op->mov.value) {
      return NULL;
    }

    op = op->mov.value;
  }

  return op;
}

static bool opts_cnst_fold_binary_op(struct ir_func *func, struct ir_op *op) {
  struct ir_var_ty var_ty = op->var_ty;
  enum ir_op_binary_op_ty ty = op->binary_op.ty;

  struct ir_op *lhs = opts_follow_movs(op->binary_op.lhs);
  struct ir_op *rhs = opts_follow_movs(op->binary_op.rhs);

  unsigned long long lhs_cnst, rhs_cnst;

  if (!lhs || lhs->ty != IR_OP_TY_CNST || !var_ty_is_integral(&lhs->var_ty) ||
      !rhs || rhs->ty != IR_OP_TY_CNST || !var_ty_is_integral(&rhs->var_ty)) {
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
#undef CNST_FLD_SBINOP

  value = round_integral(func, value, op->var_ty);
  mk_integral_constant(func->unit, op, op->var_ty.primitive, value);
  op->var_ty = var_ty;

  return true;
}

static bool opts_cnst_fold_unary_op(struct ir_func *func, struct ir_op *op) {
  struct ir_var_ty var_ty = op->var_ty;
  enum ir_op_unary_op_ty ty = op->unary_op.ty;
  struct ir_op *value = opts_follow_movs(op->unary_op.value);

  unsigned long long cnst;

  if (!value || value->ty != IR_OP_TY_CNST || !var_ty_is_integral(&value->var_ty)) {
    return false;
  }

  cnst = value->cnst.int_value;

  unsigned long long new_cnst;
#define CNST_FLD_UNNOP(ty, op)                                                 \
  case IR_OP_UNARY_OP_TY_##ty:                                                 \
    new_cnst = op cnst;                                                        \
    break;

#define CNST_FLD_SUNNOP(ty, op)                                                \
  case IR_OP_UNARY_OP_TY_##ty:                                                 \
    new_cnst = op(signed long long) cnst;                                      \
    break;

  switch (ty) {
    CNST_FLD_SUNNOP(NEG, -);
    CNST_FLD_UNNOP(NOT, ~);
    CNST_FLD_UNNOP(LOGICAL_NOT, !);
  default:
    BUG("bad type");
  }
#undef CNST_FLD_UNNOP
#undef CNST_FLD_SUNNOP

  new_cnst = round_integral(func, new_cnst, op->var_ty);
  mk_integral_constant(func->unit, op, op->var_ty.primitive, new_cnst);
  op->var_ty = var_ty;

  return true;
}

static bool opts_cnst_fold_cast_op(struct ir_func *func, struct ir_op *op) {
  struct ir_var_ty var_ty = op->var_ty;
  enum ir_op_cast_op_ty ty = op->cast_op.ty;
  struct ir_op *value =  opts_follow_movs(op->cast_op.value);

  unsigned long long cnst;

  if (!value || value->ty != IR_OP_TY_CNST || !var_ty_is_integral(&value->var_ty)) {
    return false;
  }

  cnst = value->cnst.int_value;

  unsigned long long new_cnst;
  switch (ty) {
  case IR_OP_CAST_OP_TY_SEXT:
  case IR_OP_CAST_OP_TY_ZEXT:
  case IR_OP_CAST_OP_TY_TRUNC:
    new_cnst = round_integral(func, cnst, op->var_ty);
    mk_integral_constant(func->unit, op, op->var_ty.primitive, new_cnst);
    op->var_ty = var_ty;

    return true;
  case IR_OP_CAST_OP_TY_CONV:
  case IR_OP_CAST_OP_TY_UCONV:
  case IR_OP_CAST_OP_TY_SCONV:
    if (cnst != 0) {
      return false;
    }

    op->ty = IR_OP_TY_CNST;
    op->var_ty = var_ty;
    op->cnst = (struct ir_op_cnst){
      .ty = IR_OP_CNST_TY_FLT,
      .flt_value = 0
    };
    return true;
  }
}

static bool opts_cnst_fold_addr_offset(struct ir_func *func, struct ir_op *op) {
  op = opts_follow_movs(op);

  if (!op) {
    return false;
  }

  struct ir_op_addr_offset *addr_offset = &op->addr_offset;

  // only need to fold `index * scale` as the rest is already constant-ified
  if (addr_offset->index && addr_offset->index->ty == IR_OP_TY_CNST) {
    unsigned long long value = addr_offset->index->cnst.int_value * addr_offset->scale;
    addr_offset->index = NULL;
    addr_offset->offset += value;
    return true;
  }

  return false;
}

static bool opts_cnst_fold_op(struct ir_func *func, struct ir_op *op) {
  switch (op->ty) {
  case IR_OP_TY_BINARY_OP:
    return opts_cnst_fold_binary_op(func, op);
  case IR_OP_TY_UNARY_OP:
    return opts_cnst_fold_unary_op(func, op);
  case IR_OP_TY_CAST_OP:
    return opts_cnst_fold_cast_op(func, op);
  case IR_OP_TY_ADDR_OFFSET:
    return opts_cnst_fold_addr_offset(func, op);
  default:
    return false;
  }
}

void opts_cnst_fold(struct ir_unit *unit) {
  struct opts_pass pass = {.name = __func__, .op_callback = opts_cnst_fold_op};

  opts_run_pass(unit, &pass);
}
