#include "ir.h"

#include "../alloc.h"
#include "../log.h"
#include "../target.h"
#include "../util.h"
#include "../vector.h"
#include "prettyprint.h"

enum ir_var_primitive_ty var_ty_pointer_primitive_ty(struct ir_unit *iru) {
  switch (iru->target->lp_sz) {
  case TARGET_LP_SZ_LP32:
    return IR_VAR_PRIMITIVE_TY_I32;
  case TARGET_LP_SZ_LP64:
    return IR_VAR_PRIMITIVE_TY_I64;
  }
}

bool binary_op_is_comparison(enum ir_op_binary_op_ty ty) {
  switch (ty) {
  case IR_OP_BINARY_OP_TY_EQ:
  case IR_OP_BINARY_OP_TY_NEQ:
  case IR_OP_BINARY_OP_TY_UGT:
  case IR_OP_BINARY_OP_TY_SGT:
  case IR_OP_BINARY_OP_TY_UGTEQ:
  case IR_OP_BINARY_OP_TY_SGTEQ:
  case IR_OP_BINARY_OP_TY_ULT:
  case IR_OP_BINARY_OP_TY_SLT:
  case IR_OP_BINARY_OP_TY_ULTEQ:
  case IR_OP_BINARY_OP_TY_SLTEQ:
  case IR_OP_BINARY_OP_TY_FEQ:
  case IR_OP_BINARY_OP_TY_FNEQ:
  case IR_OP_BINARY_OP_TY_FGT:
  case IR_OP_BINARY_OP_TY_FLT:
  case IR_OP_BINARY_OP_TY_FGTEQ:
  case IR_OP_BINARY_OP_TY_FLTEQ:
    return true;
  case IR_OP_BINARY_OP_TY_AND:
  case IR_OP_BINARY_OP_TY_OR:
  case IR_OP_BINARY_OP_TY_XOR:
  case IR_OP_BINARY_OP_TY_LSHIFT:
  case IR_OP_BINARY_OP_TY_SRSHIFT:
  case IR_OP_BINARY_OP_TY_URSHIFT:
  case IR_OP_BINARY_OP_TY_ADD:
  case IR_OP_BINARY_OP_TY_SUB:
  case IR_OP_BINARY_OP_TY_MUL:
  case IR_OP_BINARY_OP_TY_SDIV:
  case IR_OP_BINARY_OP_TY_UDIV:
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_UQUOT:
  case IR_OP_BINARY_OP_TY_FADD:
  case IR_OP_BINARY_OP_TY_FSUB:
  case IR_OP_BINARY_OP_TY_FMUL:
  case IR_OP_BINARY_OP_TY_FDIV:
  case IR_OP_BINARY_OP_TY_FMAX:
  case IR_OP_BINARY_OP_TY_FMIN:
    return false;
  }
}

enum ir_op_binary_op_ty invert_binary_comparison(enum ir_op_binary_op_ty ty) {
  switch (ty) {
  case IR_OP_BINARY_OP_TY_EQ:
    return IR_OP_BINARY_OP_TY_NEQ;
  case IR_OP_BINARY_OP_TY_NEQ:
    return IR_OP_BINARY_OP_TY_EQ;
  case IR_OP_BINARY_OP_TY_UGT:
    return IR_OP_BINARY_OP_TY_ULTEQ;
  case IR_OP_BINARY_OP_TY_SGT:
    return IR_OP_BINARY_OP_TY_SLTEQ;
  case IR_OP_BINARY_OP_TY_UGTEQ:
    return IR_OP_BINARY_OP_TY_ULT;
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return IR_OP_BINARY_OP_TY_SLT;
  case IR_OP_BINARY_OP_TY_ULT:
    return IR_OP_BINARY_OP_TY_UGTEQ;
  case IR_OP_BINARY_OP_TY_SLT:
    return IR_OP_BINARY_OP_TY_SGTEQ;
  case IR_OP_BINARY_OP_TY_ULTEQ:
    return IR_OP_BINARY_OP_TY_UGT;
  case IR_OP_BINARY_OP_TY_SLTEQ:
    return IR_OP_BINARY_OP_TY_SGT;
  case IR_OP_BINARY_OP_TY_FEQ:
    return IR_OP_BINARY_OP_TY_FNEQ;
  case IR_OP_BINARY_OP_TY_FNEQ:
    return IR_OP_BINARY_OP_TY_FEQ;
  case IR_OP_BINARY_OP_TY_FGT:
    return IR_OP_BINARY_OP_TY_FLTEQ;
  case IR_OP_BINARY_OP_TY_FGTEQ:
    return IR_OP_BINARY_OP_TY_FLT;
  case IR_OP_BINARY_OP_TY_FLT:
    return IR_OP_BINARY_OP_TY_FGTEQ;
  case IR_OP_BINARY_OP_TY_FLTEQ:
    return IR_OP_BINARY_OP_TY_FGT;
  default:
    BUG("binary op was not comparison");
  }
}

bool op_has_side_effects(const struct ir_op *op) {
  if (op->flags & IR_OP_FLAG_SIDE_EFFECTS) {
    return true;
  }

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op ty");
  case IR_OP_TY_PHI:
  case IR_OP_TY_UNDF:
  case IR_OP_TY_CNST:
  case IR_OP_TY_CAST_OP:
  case IR_OP_TY_LOAD:
  case IR_OP_TY_LOAD_BITFIELD:
  case IR_OP_TY_BITFIELD_EXTRACT:
  case IR_OP_TY_BITFIELD_INSERT:
  case IR_OP_TY_ADDR:
  case IR_OP_TY_ADDR_OFFSET:
  case IR_OP_TY_GATHER:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_UNARY_OP:
    return false;
  case IR_OP_TY_MOV:
  case IR_OP_TY_CALL:
  case IR_OP_TY_STORE:
  case IR_OP_TY_MEM_SET:
  case IR_OP_TY_MEM_COPY:
  case IR_OP_TY_STORE_BITFIELD:
  case IR_OP_TY_BR_COND:
  case IR_OP_TY_BR_SWITCH:
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
    return true;
  case IR_OP_TY_CUSTOM:
    BUG("not well defined for IR_OP_TY_CUSTOM");
  }
}

bool op_produces_value(const struct ir_op *op) {
  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op ty");
  case IR_OP_TY_PHI:
  case IR_OP_TY_UNDF:
  case IR_OP_TY_MOV:
  case IR_OP_TY_CNST:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_UNARY_OP:
  case IR_OP_TY_CAST_OP:
  case IR_OP_TY_LOAD:
  case IR_OP_TY_LOAD_BITFIELD:
  case IR_OP_TY_BITFIELD_INSERT:
  case IR_OP_TY_BITFIELD_EXTRACT:
  case IR_OP_TY_GATHER:
  case IR_OP_TY_ADDR:
  case IR_OP_TY_ADDR_OFFSET:
    return true;
  case IR_OP_TY_CALL:
    return op->var_ty.ty != IR_VAR_TY_TY_NONE;
  case IR_OP_TY_STORE:
  case IR_OP_TY_STORE_BITFIELD:
  case IR_OP_TY_BR_COND:
  case IR_OP_TY_BR_SWITCH:
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
  case IR_OP_TY_MEM_SET:
  case IR_OP_TY_MEM_COPY:
    return false;
  case IR_OP_TY_CUSTOM:
    BUG("`op_produces_value` not well defined for IR_OP_TY_CUSTOM");
  }
}

bool op_is_branch(enum ir_op_ty ty) {
  switch (ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op ty");
  case IR_OP_TY_BR_COND:
  case IR_OP_TY_BR_SWITCH:
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
    return true;
  // calls are NOT branches, because while they do leave, they guarantee return
  case IR_OP_TY_CALL:
  case IR_OP_TY_UNDF:
  case IR_OP_TY_PHI:
  case IR_OP_TY_GATHER:
  case IR_OP_TY_MOV:
  case IR_OP_TY_CNST:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_UNARY_OP:
  case IR_OP_TY_CAST_OP:
  case IR_OP_TY_STORE:
  case IR_OP_TY_LOAD:
  case IR_OP_TY_MEM_SET:
  case IR_OP_TY_MEM_COPY:
  case IR_OP_TY_STORE_BITFIELD:
  case IR_OP_TY_LOAD_BITFIELD:
  case IR_OP_TY_BITFIELD_EXTRACT:
  case IR_OP_TY_BITFIELD_INSERT:
  case IR_OP_TY_ADDR:
  case IR_OP_TY_ADDR_OFFSET:
    return false;
  case IR_OP_TY_CUSTOM:
    BUG("`op_produces_value` not well defined for IR_OP_TY_CUSTOM");
  }
}

bool var_ty_eq(struct ir_func *irb, const struct ir_var_ty *l,
               const struct ir_var_ty *r) {
  if (l == r) {
    return true;
  }

  if (l->ty != r->ty) {
    return false;
  }

  switch (l->ty) {
  case IR_VAR_TY_TY_NONE:
    return r->ty == IR_VAR_TY_TY_NONE;
  case IR_VAR_TY_TY_PRIMITIVE:
    return l->primitive == r->primitive;
  case IR_VAR_TY_TY_VARIADIC:
    return r->ty == IR_VAR_TY_TY_VARIADIC;
  case IR_VAR_TY_TY_POINTER:
    return true;
  case IR_VAR_TY_TY_ARRAY:
    return l->array.num_elements == r->array.num_elements &&
           var_ty_eq(irb, l->array.underlying, r->array.underlying);
  case IR_VAR_TY_TY_FUNC:
    if (!var_ty_eq(irb, l->func.ret_ty, r->func.ret_ty)) {
      return false;
    }
    if (l->func.num_params != r->func.num_params) {
      return false;
    }
    for (size_t i = 0; i < l->func.num_params; i++) {
      if (!var_ty_eq(irb, &l->func.params[i], &r->func.params[i])) {
        return false;
      }
    }

    return true;
  case IR_VAR_TY_TY_STRUCT: {
    if (l->aggregate.num_fields != r->aggregate.num_fields) {
      return false;
    }

    struct ir_var_ty_info l_info = var_ty_info(irb->unit, l);
    struct ir_var_ty_info r_info = var_ty_info(irb->unit, r);

    // currently we do not have custom alignment/size but it is possible
    if (l_info.size != r_info.size || l_info.alignment != r_info.alignment) {
      return false;
    }

    for (size_t i = 0; i < l->aggregate.num_fields; i++) {
      if (!var_ty_eq(irb, &l->aggregate.fields[i], &r->aggregate.fields[i])) {
        return false;
      }
    }

    return true;
  }
  case IR_VAR_TY_TY_UNION: {
    if (l->aggregate.num_fields != r->aggregate.num_fields) {
      return false;
    }

    struct ir_var_ty_info l_info = var_ty_info(irb->unit, l);
    struct ir_var_ty_info r_info = var_ty_info(irb->unit, r);

    // currently we do not have custom alignment/size but it is possible
    if (l_info.size != r_info.size || l_info.alignment != r_info.alignment) {
      return false;
    }

    for (size_t i = 0; i < l->aggregate.num_fields; i++) {
      if (!var_ty_eq(irb, &l->aggregate.fields[i], &r->aggregate.fields[i])) {
        return false;
      }
    }

    return true;
  }
  }

  unreachable();
}

static void walk_br_cond(struct ir_op_br_cond *br_cond, walk_op_callback *cb,
                         void *cb_metadata) {
  cb(&br_cond->cond, cb_metadata);
}

static void walk_cnst(UNUSED_ARG(struct ir_op_cnst *cnst),
                      UNUSED_ARG(walk_op_callback *cb),
                      UNUSED_ARG(void *cb_metadata)) {
  // nada
}

static void walk_binary_op(struct ir_op_binary_op *binary_op,
                           walk_op_callback *cb, void *cb_metadata) {
  walk_op(binary_op->lhs, cb, cb_metadata);
  walk_op(binary_op->rhs, cb, cb_metadata);
}

static void walk_unary_op(struct ir_op_unary_op *unary_op, walk_op_callback *cb,
                          void *cb_metadata) {
  walk_op(unary_op->value, cb, cb_metadata);
}

static void walk_cast_op(struct ir_op_cast_op *cast_op, walk_op_callback *cb,
                         void *cb_metadata) {
  walk_op(cast_op->value, cb, cb_metadata);
}

static void walk_ret(struct ir_op_ret *ret, walk_op_callback *cb,
                     void *cb_metadata) {
  if (ret->value) {
    walk_op(ret->value, cb, cb_metadata);
  }
}

void walk_op_uses(struct ir_op *op, walk_op_callback *cb, void *cb_metadata) {
  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op!");
  case IR_OP_TY_CUSTOM:
  case IR_OP_TY_UNDF:
    break;
  case IR_OP_TY_GATHER:
    for (size_t i = 0; i < op->gather.num_values; i++) {
      cb(&op->gather.values[i].value, cb_metadata);
    }
    break;
  case IR_OP_TY_CALL: {
    cb(&op->call.target, cb_metadata);
    for (size_t i = 0; i < op->call.num_args; i++) {
      cb(&op->call.args[i], cb_metadata);
    }
    break;
  }
  case IR_OP_TY_PHI: {
    for (size_t i = 0; i < op->phi.num_values; i++) {
      cb(&op->phi.values[i].value, cb_metadata);
    }
    break;
  }
  case IR_OP_TY_CNST:
    break;
  case IR_OP_TY_BINARY_OP:
    cb(&op->binary_op.lhs, cb_metadata);
    cb(&op->binary_op.rhs, cb_metadata);
    break;
  case IR_OP_TY_UNARY_OP:
    cb(&op->unary_op.value, cb_metadata);
    break;
  case IR_OP_TY_CAST_OP:
    cb(&op->cast_op.value, cb_metadata);
    break;
  case IR_OP_TY_STORE:
    cb(&op->store.value, cb_metadata);
    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
    case IR_OP_STORE_TY_GLB:
      break;
    case IR_OP_STORE_TY_ADDR:
      cb(&op->store.addr, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_STORE_BITFIELD:
    cb(&op->store_bitfield.value, cb_metadata);
    switch (op->store_bitfield.ty) {
    case IR_OP_STORE_TY_LCL:
    case IR_OP_STORE_TY_GLB:
      break;
    case IR_OP_STORE_TY_ADDR:
      cb(&op->store_bitfield.addr, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_LOAD:
    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
    case IR_OP_LOAD_TY_GLB:
      break;
    case IR_OP_LOAD_TY_ADDR:
      cb(&op->load.addr, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_LOAD_BITFIELD:
    switch (op->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
    case IR_OP_LOAD_TY_GLB:
      break;
    case IR_OP_LOAD_TY_ADDR:
      cb(&op->load_bitfield.addr, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_ADDR:
    break;
  case IR_OP_TY_ADDR_OFFSET:
    cb(&op->addr_offset.base, cb_metadata);
    if (op->addr_offset.index) {
      cb(&op->addr_offset.index, cb_metadata);
    }
    break;
  case IR_OP_TY_BR:
    break;
  case IR_OP_TY_BR_SWITCH:
    cb(&op->br_switch.value, cb_metadata);
    break;
  case IR_OP_TY_BR_COND:
    cb(&op->br_cond.cond, cb_metadata);
    break;
  case IR_OP_TY_MOV:
    if (op->mov.value && !(op->flags & IR_OP_FLAG_PARAM)) {
      cb(&op->mov.value, cb_metadata);
    }
    break;
  case IR_OP_TY_RET:
    if (op->ret.value) {
      cb(&op->ret.value, cb_metadata);
    }
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    cb(&op->bitfield_extract.value, cb_metadata);
    break;
  case IR_OP_TY_BITFIELD_INSERT:
    cb(&op->bitfield_insert.target, cb_metadata);
    cb(&op->bitfield_insert.value, cb_metadata);
    break;
  case IR_OP_TY_MEM_SET:
    cb(&op->mem_set.addr, cb_metadata);
    break;
  case IR_OP_TY_MEM_COPY:
    cb(&op->mem_copy.source, cb_metadata);
    cb(&op->mem_copy.dest, cb_metadata);
    break;
  }
}

void walk_op(struct ir_op *op, walk_op_callback *cb, void *cb_metadata) {
  cb(&op, cb_metadata);

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op!");
  case IR_OP_TY_CUSTOM:
    TODO("walk custom");
  case IR_OP_TY_CALL:
    TODO("walk call");
  case IR_OP_TY_PHI:
    TODO("walk phi");
  case IR_OP_TY_MOV:
    TODO("walk mov");
  case IR_OP_TY_LOAD:
    TODO("walk load");
  case IR_OP_TY_STORE:
    TODO("walk store");
  case IR_OP_TY_LOAD_BITFIELD:
    TODO("walk load bitfield");
  case IR_OP_TY_STORE_BITFIELD:
    TODO("walk store bitfield");
  case IR_OP_TY_ADDR:
    TODO("walk addr");
  case IR_OP_TY_BR_SWITCH:
    TODO("walk br.switch");
  case IR_OP_TY_UNDF:
    break;
  case IR_OP_TY_CNST:
    walk_cnst(&op->cnst, cb, cb_metadata);
    break;
  case IR_OP_TY_BINARY_OP:
    walk_binary_op(&op->binary_op, cb, cb_metadata);
    break;
  case IR_OP_TY_UNARY_OP:
    walk_unary_op(&op->unary_op, cb, cb_metadata);
    break;
  case IR_OP_TY_CAST_OP:
    walk_cast_op(&op->cast_op, cb, cb_metadata);
    break;
  case IR_OP_TY_BR:
    // nada
    break;
  case IR_OP_TY_BR_COND:
    walk_br_cond(&op->br_cond, cb, cb_metadata);
    break;
  case IR_OP_TY_RET:
    walk_ret(&op->ret, cb, cb_metadata);
    break;
  default:
    TODO("other ops in walk op");
  }
}

void walk_stmt(struct ir_stmt *stmt, walk_op_callback *cb, void *cb_metadata) {
  struct ir_op *op = stmt->last;

  walk_op(op, cb, cb_metadata);
}

enum ir_op_sign binary_op_sign(enum ir_op_binary_op_ty ty) {
  switch (ty) {
  case IR_OP_BINARY_OP_TY_OR:
  case IR_OP_BINARY_OP_TY_XOR:
  case IR_OP_BINARY_OP_TY_AND:
  case IR_OP_BINARY_OP_TY_LSHIFT:
  case IR_OP_BINARY_OP_TY_ADD:
  case IR_OP_BINARY_OP_TY_SUB:
  case IR_OP_BINARY_OP_TY_MUL:
  case IR_OP_BINARY_OP_TY_EQ:
  case IR_OP_BINARY_OP_TY_NEQ:
  case IR_OP_BINARY_OP_TY_FADD:
  case IR_OP_BINARY_OP_TY_FSUB:
  case IR_OP_BINARY_OP_TY_FMUL:
  case IR_OP_BINARY_OP_TY_FDIV:
  case IR_OP_BINARY_OP_TY_FEQ:
  case IR_OP_BINARY_OP_TY_FNEQ:
  case IR_OP_BINARY_OP_TY_FGT:
  case IR_OP_BINARY_OP_TY_FGTEQ:
  case IR_OP_BINARY_OP_TY_FLT:
  case IR_OP_BINARY_OP_TY_FLTEQ:
  case IR_OP_BINARY_OP_TY_FMAX:
  case IR_OP_BINARY_OP_TY_FMIN:
    return IR_OP_SIGN_NA;
  case IR_OP_BINARY_OP_TY_SRSHIFT:
  case IR_OP_BINARY_OP_TY_SDIV:
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_SLT:
  case IR_OP_BINARY_OP_TY_SLTEQ:
  case IR_OP_BINARY_OP_TY_SGT:
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return IR_OP_SIGN_SIGNED;
  case IR_OP_BINARY_OP_TY_URSHIFT:
  case IR_OP_BINARY_OP_TY_UDIV:
  case IR_OP_BINARY_OP_TY_UQUOT:
  case IR_OP_BINARY_OP_TY_ULT:
  case IR_OP_BINARY_OP_TY_ULTEQ:
  case IR_OP_BINARY_OP_TY_UGT:
  case IR_OP_BINARY_OP_TY_UGTEQ:
    return IR_OP_SIGN_UNSIGNED;
  }
}

const struct ir_var_ty IR_VAR_TY_NONE = {.ty = IR_VAR_TY_TY_NONE};
const struct ir_var_ty IR_VAR_TY_POINTER = {.ty = IR_VAR_TY_TY_POINTER};
const struct ir_var_ty IR_VAR_TY_I8 = {.ty = IR_VAR_TY_TY_PRIMITIVE,
                                       .primitive = IR_VAR_PRIMITIVE_TY_I8};
const struct ir_var_ty IR_VAR_TY_I16 = {.ty = IR_VAR_TY_TY_PRIMITIVE,
                                        .primitive = IR_VAR_PRIMITIVE_TY_I16};
const struct ir_var_ty IR_VAR_TY_I32 = {.ty = IR_VAR_TY_TY_PRIMITIVE,
                                        .primitive = IR_VAR_PRIMITIVE_TY_I32};
const struct ir_var_ty IR_VAR_TY_I64 = {.ty = IR_VAR_TY_TY_PRIMITIVE,
                                        .primitive = IR_VAR_PRIMITIVE_TY_I64};
const struct ir_var_ty IR_VAR_TY_F16 = {.ty = IR_VAR_TY_TY_PRIMITIVE,
                                        .primitive = IR_VAR_PRIMITIVE_TY_F16};
const struct ir_var_ty IR_VAR_TY_F32 = {.ty = IR_VAR_TY_TY_PRIMITIVE,
                                        .primitive = IR_VAR_PRIMITIVE_TY_F32};
const struct ir_var_ty IR_VAR_TY_F64 = {.ty = IR_VAR_TY_TY_PRIMITIVE,
                                        .primitive = IR_VAR_PRIMITIVE_TY_F64};
const struct ir_var_ty IR_VAR_TY_VARIADIC = {.ty = IR_VAR_TY_TY_VARIADIC};

bool is_func_variadic(const struct ir_var_func_ty *ty) {
  return ty->flags & IR_VAR_FUNC_TY_FLAG_VARIADIC;
}

void initialise_ir_op(struct ir_op *op, size_t id, enum ir_op_ty ty,
                      struct ir_var_ty var_ty, struct ir_reg reg,
                      struct ir_lcl *lcl) {

  op->id = id;
  op->ty = ty;
  op->flags = IR_OP_FLAG_NONE;
  op->var_ty = var_ty;
  op->pred = NULL;
  op->succ = NULL;
  op->stmt = NULL;
  op->reg = reg;
  op->lcl = lcl;
  op->write_info = (struct ir_op_write_info){.num_reg_writes = 0};
  op->metadata = NULL;
  op->comment = NULL;
}

static void remove_pred(struct ir_basicblock *basicblock,
                        struct ir_basicblock *pred) {
  for (size_t i = 0; i < basicblock->num_preds; i++) {
    if (basicblock->preds[i] == pred) {
      if (i + 1 < basicblock->num_preds) {
        memmove(&basicblock->preds[i], &basicblock->preds[i + 1],
                (basicblock->num_preds - i - 1) *
                    sizeof(struct ir_basicblock *));
      }

      basicblock->num_preds--;
      break;
    }
  }
}

void detach_ir_basicblock(struct ir_func *irb, struct ir_basicblock *basicblock,
                          enum detach_ir_basicblock_flags flags) {
  if (basicblock->id == DETACHED_BASICBLOCK) {
    return;
  }

  invariant_assert(irb->basicblock_count,
                   "`detach_ir_basicblock` would underflow basicblock count "
                   "for `ir_builder`");

  invariant_assert((flags & DETACH_IR_BASICBLOCK_FLAG_ALLOW_PREDS) ||
                       !basicblock->num_preds,
                   "trying to detach BB with preds");

  size_t stmt_count = 0;
  size_t op_count = 0;
  struct ir_stmt *stmt = basicblock->first;
  while (stmt) {
    stmt_count++;

    struct ir_op *op = stmt->first;
    while (op) {
      op_count++;

      op = op->succ;
    }

    stmt = stmt->succ;
  }

  irb->basicblock_count--;
  irb->stmt_count -= stmt_count;
  irb->op_count -= op_count;

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    break;

  case IR_BASICBLOCK_TY_SPLIT:
    remove_pred(basicblock->split.false_target, basicblock);
    remove_pred(basicblock->split.true_target, basicblock);
    break;

  case IR_BASICBLOCK_TY_SWITCH:
    for (size_t i = 0; i < basicblock->switch_case.num_cases; i++) {
      remove_pred(basicblock->switch_case.cases[i].target, basicblock);
    }

    if (basicblock->switch_case.default_target) {
      remove_pred(basicblock->switch_case.default_target, basicblock);
    }
    break;

  case IR_BASICBLOCK_TY_MERGE:
    remove_pred(basicblock->merge.target, basicblock);
    break;
  }

  basicblock->id = DETACHED_BASICBLOCK;

  // fix links on either side of basicblock
  if (basicblock->pred) {
    basicblock->pred->succ = basicblock->succ;
  } else {
    irb->first = basicblock->succ;
  }

  if (basicblock->succ) {
    basicblock->succ->pred = basicblock->pred;
  } else {
    irb->last = basicblock->pred;
  }

  basicblock->func = NULL;
}

void detach_ir_stmt(struct ir_func *irb, struct ir_stmt *stmt) {
  if (stmt->id == DETACHED_STMT) {
    return;
  }

  invariant_assert(
      irb->stmt_count,
      "`detach_ir_stmt` would underflow stmt count for `ir_builder`");

  irb->stmt_count--;

  // fix links on either side of stmt
  if (stmt->pred) {
    stmt->pred->succ = stmt->succ;
  } else {
    stmt->basicblock->first = stmt->succ;
  }

  stmt->id = DETACHED_STMT;

  if (stmt->succ) {
    stmt->succ->pred = stmt->pred;
  } else {
    stmt->basicblock->last = stmt->pred;
  }

  stmt->basicblock = NULL;
}

void detach_ir_op(struct ir_func *irb, struct ir_op *op) {
  if (op->id == DETACHED_OP) {
    return;
  }

  invariant_assert(irb->op_count,
                   "`detach_ir_op` would underflow op count for `ir_builder`");
  invariant_assert(op->stmt, "can't detach `op` not attached to stmt");

  irb->op_count--;

  op->id = DETACHED_LCL;

  // fix links on either side of op
  if (op->pred) {
    op->pred->succ = op->succ;
  } else {
    op->stmt->first = op->succ;
  }

  if (op->succ) {
    op->succ->pred = op->pred;
  } else {
    op->stmt->last = op->pred;
  }

  op->stmt = NULL;
  op->pred = NULL;
  op->succ = NULL;
}

bool stmt_is_empty(struct ir_stmt *stmt) {
  invariant_assert(
      (stmt->first && stmt->last) || (!stmt->first && !stmt->last),
      "stmt had `first` or `last` NULL but not both; this is badly formed");

  // first will be NULL
  return !stmt->first;
}

bool basicblock_is_empty(struct ir_basicblock *basicblock) {
  invariant_assert((basicblock->first && basicblock->last) ||
                       (!basicblock->first && !basicblock->last),
                   "basicblock had `first` or `last` NULL but not both; this "
                   "is badly formed");

  return !basicblock->first || (!basicblock->first->first->succ &&
                                basicblock->first->first->ty == IR_OP_TY_BR);
}

bool ir_reg_eq(struct ir_reg left, struct ir_reg right) {
  return left.ty == right.ty && left.idx == right.idx;
}

void ir_order_basicblocks(struct ir_func *func) {
  // TODO: topological sort basicblocks

  // for now, just swap conditions that result in two branches becoming one

  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    if (basicblock->ty == IR_BASICBLOCK_TY_SPLIT) {
      struct ir_basicblock_split *split = &basicblock->split;

      if (split->true_target == basicblock->succ) {
        struct ir_op *br_cond = basicblock->last->last;
        DEBUG_ASSERT(br_cond->ty == IR_OP_TY_BR_COND,
                     "expected `br.cond` at end of SPLIT bb");
        struct ir_op *cond = br_cond->br_cond.cond;

        if (cond->ty == IR_OP_TY_BINARY_OP &&
            binary_op_is_comparison(cond->binary_op.ty)) {
          cond->binary_op.ty = invert_binary_comparison(cond->binary_op.ty);

          struct ir_basicblock *tmp = split->true_target;
          split->true_target = split->false_target;
          split->false_target = tmp;
        }
      }
    }

    basicblock = basicblock->succ;
  }
}

void eliminate_redundant_ops(struct ir_func *func,
                             enum eliminate_redundant_ops_flags flags) {
  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op_use_map use_map = build_op_uses_map(func);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    switch (op->ty) {
    case IR_OP_TY_MOV:
      if (!(flags & ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_MOVS)) {
        continue;
      }

      if (op->flags & IR_OP_FLAG_SIDE_EFFECTS) {
        continue;
      }

      if (op->mov.value && op->reg.ty != IR_REG_TY_NONE &&
          ir_reg_eq(op->reg, op->mov.value->reg)) {
        struct ir_op_usage usage = use_map.op_use_datas[op->id];

        for (size_t i = 0; i < usage.num_uses; i++) {
          *usage.uses[i].op = op->mov.value;
        }

        detach_ir_op(func, op);
      }
      break;
    case IR_OP_TY_BR: {
      if (!(flags & ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_BRANCHES)) {
        continue;
      }

      struct ir_basicblock *basicblock = op->stmt->basicblock;
      DEBUG_ASSERT(basicblock->ty == IR_BASICBLOCK_TY_MERGE,
                   "br op in non MERGE bb");
      if (basicblock->succ == basicblock->merge.target) {
        detach_ir_op(func, op);
      }
      break;
    }
    default:
      if (op_has_side_effects(op)) {
        continue;
      }

      if (!use_map.op_use_datas[op->id].num_uses) {
        detach_ir_op(func, op);
      }
      break;
    }
  }

  use_map = build_op_uses_map(func);
  iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  while (ir_func_iter_next(&iter, &op)) {
    if (!use_map.op_use_datas[op->id].num_uses && !op_has_side_effects(op)) {
      detach_ir_op(func, op);
    }
  }
}

void prune_basicblocks(struct ir_func *irb) {
  if (!irb->first) {
    return;
  }

  bool *seen = arena_alloc(irb->arena, sizeof(*seen) * irb->basicblock_count);
  memset(seen, 0, sizeof(*seen) * irb->basicblock_count);

  prune_stmts(irb, irb->first);

  struct vector *stack = vector_create(sizeof(struct ir_basicblock *));
  vector_push_back(stack, &irb->first);

  while (vector_length(stack)) {
    struct ir_basicblock *bb = *(struct ir_basicblock **)vector_pop(stack);

    if (seen[bb->id]) {
      continue;
    }

    seen[bb->id] = true;

    switch (bb->ty) {
    case IR_BASICBLOCK_TY_RET:
      break;
    case IR_BASICBLOCK_TY_SPLIT:
      vector_push_back(stack, &bb->split.true_target);
      vector_push_back(stack, &bb->split.false_target);
      break;
    case IR_BASICBLOCK_TY_MERGE:
      vector_push_back(stack, &bb->merge.target);
      break;
    case IR_BASICBLOCK_TY_SWITCH:
      for (size_t i = 0; i < bb->switch_case.num_cases; i++) {
        vector_push_back(stack, &bb->switch_case.cases[i].target);
      }

      if (bb->switch_case.default_target) {
        vector_push_back(stack, &bb->switch_case.default_target);
      }
      break;
    }
  }

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    // save succ before we detach
    struct ir_basicblock *succ = basicblock->succ;

    if (!seen[basicblock->id]) {
      detach_ir_basicblock(irb, basicblock,
                           DETACH_IR_BASICBLOCK_FLAG_ALLOW_PREDS);
    } else {
      prune_stmts(irb, basicblock);
    }

    basicblock = succ;
  }

  // means bb->id < bb_count for all bbs
  rebuild_ids(irb);

  vector_free(&stack);
}

void prune_stmts(struct ir_func *irb, struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;

  // skip phi stmt
  if (stmt) {
    stmt = stmt->succ;
  }

  while (stmt) {
    // save succ before we detach
    struct ir_stmt *succ = stmt->succ;

    if (stmt_is_empty(stmt)) {
      detach_ir_stmt(irb, stmt);
    }

    stmt = succ;
  }
}

void clear_metadata(struct ir_func *irb) {
  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    basicblock->metadata = NULL;

    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        op->metadata = NULL;

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}

void rebuild_ids(struct ir_func *irb) {
  irb->next_lcl_id = 0;
  struct ir_lcl *lcl = irb->first_lcl;
  while (lcl) {
    lcl->id = irb->next_lcl_id++;

    lcl = lcl->succ;
  }

  irb->next_basicblock_id = 0;
  irb->next_stmt_id = 0;
  irb->next_op_id = 0;

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    basicblock->id = irb->next_basicblock_id++;

    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      stmt->id = irb->next_stmt_id++;

      struct ir_op *op = stmt->first;

      while (op) {
        op->id = irb->next_op_id++;

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}

void attach_ir_op(struct ir_func *irb, struct ir_op *op, struct ir_stmt *stmt,
                  struct ir_op *pred, struct ir_op *succ) {
  invariant_assert(!op->stmt && !op->pred && !op->succ,
                   "non-detached op trying to be attached");
  invariant_assert((!pred || pred->succ == succ) &&
                       (!succ || succ->pred == pred),
                   "`pred` and `succ` are not next to each other");
  invariant_assert(stmt, "cannot attach op without stmt");

  irb->op_count++;

  op->stmt = stmt;

  if (op != pred) {
    op->pred = pred;
  }

  if (op != succ) {
    op->succ = succ;
  }

  if (op->pred) {
    op->pred->succ = op;
  } else {
    op->stmt->first = op;
  }

  if (op->succ) {
    op->succ->pred = op;
  } else {
    op->stmt->last = op;
  }
}

void attach_ir_basicblock(struct ir_func *irb, struct ir_basicblock *basicblock,
                          struct ir_basicblock *pred,
                          struct ir_basicblock *succ) {
  invariant_assert(!basicblock->pred && !basicblock->succ,
                   "non-detached basicblock trying to be attached");
  invariant_assert((!pred || pred->succ == succ) &&
                       (!succ || succ->pred == pred),
                   "`pred` and `succ` are not next to each other");

  irb->basicblock_count++;

  basicblock->func = irb;

  basicblock->preds = NULL;
  basicblock->num_preds = 0;

  if (basicblock != pred) {
    basicblock->pred = pred;
  }

  if (basicblock != succ) {
    basicblock->succ = succ;
  }

  if (basicblock->pred) {
    basicblock->pred->succ = basicblock;
  } else {
    irb->first = basicblock;
  }

  if (basicblock->succ) {
    basicblock->succ->pred = basicblock;
  } else {
    irb->last = basicblock;
  }
}

void move_after_ir_op(struct ir_func *irb, struct ir_op *op,
                      struct ir_op *move_after) {
  invariant_assert(op->id != move_after->id, "trying to move op after itself!");
  invariant_assert(op->stmt == NULL || op->stmt == move_after->stmt,
                   "moving between ir_stmts not supported");

  if (op->stmt) {
    detach_ir_op(irb, op);
  }

  attach_ir_op(irb, op, move_after->stmt, move_after, move_after->succ);
}

void move_before_ir_op(struct ir_func *irb, struct ir_op *op,
                       struct ir_op *move_before) {
  invariant_assert(op->id != move_before->id,
                   "trying to move op before itself!");
  invariant_assert(op->stmt == NULL || op->stmt == move_before->stmt,
                   "moving between ir_stmts not supported");

  if (op->stmt) {
    detach_ir_op(irb, op);
  }

  attach_ir_op(irb, op, move_before->stmt, move_before->pred, move_before);
}

void move_after_ir_basicblock(struct ir_func *irb,
                              struct ir_basicblock *basicblock,
                              struct ir_basicblock *move_after) {
  invariant_assert(basicblock->id != move_after->id,
                   "trying to move basicblock after itself!");

  if (basicblock->func) {
    detach_ir_basicblock(irb, basicblock, DETACH_IR_BASICBLOCK_FLAG_NONE);
  }

  attach_ir_basicblock(irb, basicblock, move_after, move_after->succ);
}

void move_before_ir_basicblock(struct ir_func *irb,
                               struct ir_basicblock *basicblock,
                               struct ir_basicblock *move_before) {
  invariant_assert(basicblock->id != move_before->id,
                   "trying to move basicblock before itself!");

  if (basicblock->func) {
    detach_ir_basicblock(irb, basicblock, DETACH_IR_BASICBLOCK_FLAG_NONE);
  }

  attach_ir_basicblock(irb, basicblock, move_before->pred, move_before);
}

struct ir_op *replace_ir_op(UNUSED struct ir_func *irb, struct ir_op *op,
                            enum ir_op_ty ty, struct ir_var_ty var_ty) {
  DEBUG_ASSERT(op, "invalid replacement point!");

  op->ty = ty;
  op->var_ty = var_ty;

  return op;
}

struct ir_op *insert_before_ir_op(struct ir_func *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_var_ty var_ty) {
  DEBUG_ASSERT(insert_before, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->next_op_id++, ty, var_ty, NO_REG, NULL);

  move_before_ir_op(irb, op, insert_before);

  return op;
}

void initialise_ir_basicblock(struct ir_basicblock *basicblock, size_t id) {
  basicblock->id = id;
  basicblock->func = NULL;
  basicblock->pred = NULL;
  basicblock->succ = NULL;
  basicblock->first = NULL;
  basicblock->last = NULL;
  basicblock->metadata = NULL;
  basicblock->comment = NULL;
  basicblock->first_instr = NULL;
  basicblock->last_instr = NULL;

  basicblock->preds = NULL;
  basicblock->num_preds = 0;
}

struct ir_op *insert_after_ir_op(struct ir_func *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_var_ty var_ty) {
  DEBUG_ASSERT(insert_after, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->next_op_id++, ty, var_ty, NO_REG, NULL);

  move_after_ir_op(irb, op, insert_after);

  return op;
}

struct ir_op *insert_phi(struct ir_func *irb, struct ir_basicblock *basicblock,
                         struct ir_var_ty var_ty) {
  struct ir_stmt *stmt = basicblock->first;
  if (!stmt) {
    stmt = alloc_ir_stmt(irb, basicblock);
  }

  struct ir_op *op = stmt->first;

  struct ir_op *phi;
  if (op) {
    phi = insert_before_ir_op(irb, op, IR_OP_TY_PHI, var_ty);
  } else {
    phi = alloc_ir_op(irb, stmt);
    phi->ty = IR_OP_TY_PHI;
    phi->var_ty = var_ty;
  }

  phi->phi = (struct ir_op_phi){.num_values = 0, .values = NULL};

  return phi;
}

struct ir_basicblock *
insert_before_ir_basicblock(struct ir_func *irb,
                            struct ir_basicblock *insert_before) {
  DEBUG_ASSERT(insert_before, "invalid insertion point!");

  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  initialise_ir_basicblock(basicblock, irb->next_basicblock_id++);

  move_before_ir_basicblock(irb, basicblock, insert_before);

  return basicblock;
}

struct ir_basicblock *
insert_after_ir_basicblock(struct ir_func *irb,
                           struct ir_basicblock *insert_after) {
  DEBUG_ASSERT(insert_after, "invalid insertion point!");

  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  initialise_ir_basicblock(basicblock, irb->next_basicblock_id++);

  move_after_ir_basicblock(irb, basicblock, insert_after);

  return basicblock;
}

void swap_ir_ops_in_place(struct ir_func *irb, struct ir_op *left,
                          struct ir_op *right) {
  // WARN: known buggy!

  DEBUG_ASSERT(left->succ == right && right->pred == left,
               "can only swap in place ops that are adjacent");

  struct ir_op *tmp = arena_alloc(irb->arena, sizeof(*tmp));
  *tmp = *left;
  *left = *right;
  *right = *tmp;
}

void swap_ir_ops(struct ir_func *irb, struct ir_op *left, struct ir_op *right) {
  if (left == right) {
    return;
  }

  // if they are next to each other, normalize to `left` being pred to `right`
  if (right->succ == left) {
    struct ir_op *tmp = right;
    right = left;
    left = tmp;
  }

  // if they are next to each other, the normal logic won't work
  // instead, remove left and insert it after right
  if (left->succ == right) {
    detach_ir_op(irb, left);
    move_after_ir_op(irb, left, right);
    return;
  }

  invariant_assert(left && right, "can't swap op with NULL!");
  invariant_assert(left->stmt == right->stmt, "cannot swap ops across stmts");

  struct ir_op *left_pred = left->pred;
  struct ir_op *left_succ = left->succ;
  struct ir_stmt *left_stmt = left->stmt;

  struct ir_op *right_pred = right->pred;
  struct ir_op *right_succ = right->succ;
  invariant_assert(
      right_pred->succ == right && right_pred->succ->succ == right_succ, "wtf");
  invariant_assert(
      left_pred->succ == left && left_pred->succ->succ == left_succ, "wtf");
  struct ir_stmt *right_stmt = right->stmt;

  detach_ir_op(irb, left);
  detach_ir_op(irb, right);

  attach_ir_op(irb, left, right_stmt, right_pred, right_succ);
  attach_ir_op(irb, right, left_stmt, left_pred, left_succ);
}

struct ir_basicblock *alloc_ir_basicblock(struct ir_func *irb) {
  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  if (!irb->first) {
    irb->first = basicblock;
  }

  irb->basicblock_count++;

  basicblock->id = irb->next_basicblock_id++;
  basicblock->func = irb;
  basicblock->pred = irb->last;
  basicblock->succ = NULL;
  basicblock->first = NULL;
  basicblock->last = NULL;
  basicblock->metadata = NULL;
  basicblock->comment = NULL;
  basicblock->first_instr = NULL;
  basicblock->last_instr = NULL;

  basicblock->preds = NULL;
  basicblock->num_preds = 0;

  if (irb->last) {
    irb->last->succ = basicblock;
  }

  irb->last = basicblock;

  // this is the statement used by phis
  alloc_ir_stmt(irb, basicblock);

  return basicblock;
}

struct ir_stmt *alloc_ir_stmt(struct ir_func *irb,
                              struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = arena_alloc(irb->arena, sizeof(*stmt));

  if (!basicblock->first) {
    basicblock->first = stmt;
  }

  irb->stmt_count++;

  stmt->id = irb->next_stmt_id++;
  stmt->basicblock = basicblock;
  stmt->pred = basicblock->last;
  stmt->succ = NULL;
  stmt->first = NULL;
  stmt->last = NULL;

  if (basicblock->last) {
    basicblock->last->succ = stmt;
  }

  basicblock->last = stmt;

  return stmt;
}

// TODO: this should call `initialise_ir_op`
struct ir_op *alloc_ir_op(struct ir_func *irb, struct ir_stmt *stmt) {
  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  if (!stmt->first) {
    stmt->first = op;
  }

  irb->op_count++;

  op->id = irb->next_op_id++;
  op->ty = IR_OP_TY_UNKNOWN;
  op->flags = IR_OP_FLAG_NONE;
  op->stmt = stmt;
  op->pred = stmt->last;
  op->succ = NULL;
  op->metadata = NULL;
  op->comment = NULL;
  op->reg = NO_REG;
  op->lcl = NULL;
  op->write_info = (struct ir_op_write_info){.num_reg_writes = 0};

  if (stmt->last) {
    stmt->last->succ = op;
  }

  stmt->last = op;

  return op;
}

struct ir_op *alloc_contained_ir_op(struct ir_func *irb, struct ir_op *op,
                                    struct ir_op *consumer) {
  struct ir_op *contained =
      insert_before_ir_op(irb, consumer, op->ty, op->var_ty);

  contained->flags |= IR_OP_FLAG_CONTAINED;

  switch (op->ty) {
  case IR_OP_TY_CNST:
    contained->cnst = op->cnst;
    break;
  case IR_OP_TY_ADDR:
    contained->addr = op->addr;
    break;
  case IR_OP_TY_ADDR_OFFSET:
    contained->addr_offset = op->addr_offset;
    break;
  case IR_OP_TY_CAST_OP:
    contained->cast_op = op->cast_op;
    break;
  case IR_OP_TY_BINARY_OP:
    contained->binary_op = op->binary_op;
    break;
  case IR_OP_TY_UNARY_OP:
    contained->unary_op = op->unary_op;
    break;
  default:
    TODO("unsupported type for contained op");
  }

  return contained;
}

struct ir_op *alloc_fixed_reg_dest_ir_op(struct ir_func *irb, struct ir_op **op,
                                         struct ir_op *consumer,
                                         struct ir_reg reg) {
  struct ir_op *mov =
      insert_before_ir_op(irb, consumer, IR_OP_TY_MOV, (*op)->var_ty);

  mov->flags |= IR_OP_FLAG_FIXED_REG;
  mov->reg = reg;
  mov->mov = (struct ir_op_mov){.value = *op};

  *op = mov;

  return mov;
}

struct ir_op *alloc_fixed_reg_source_ir_op(struct ir_func *irb,
                                           struct ir_op *producer,
                                           struct ir_reg reg) {
  struct ir_op *mov =
      insert_before_ir_op(irb, producer, producer->ty, producer->var_ty);

  switch (producer->ty) {
  case IR_OP_TY_CNST:
    mov->cnst = producer->cnst;
    break;
  case IR_OP_TY_ADDR:
    mov->addr = producer->addr;
    break;
  case IR_OP_TY_CAST_OP:
    mov->cast_op = producer->cast_op;
    break;
  case IR_OP_TY_BINARY_OP:
    mov->binary_op = producer->binary_op;
    break;
  case IR_OP_TY_UNARY_OP:
    mov->unary_op = producer->unary_op;
    break;
  case IR_OP_TY_CALL:
    mov->call = producer->call;
    break;
  default:
    TODO("unsupported type for fixed op");
  }

  mov->flags |= IR_OP_FLAG_FIXED_REG;
  mov->reg = reg;
  mov->write_info = producer->write_info;

  producer->ty = IR_OP_TY_MOV;
  producer->mov = (struct ir_op_mov){.value = mov};
  producer->write_info = (struct ir_op_write_info){.num_reg_writes = 0};

  return mov;
}

static bool primitive_ty_is_integral(enum ir_var_primitive_ty ty);
static bool primitive_ty_is_fp(enum ir_var_primitive_ty ty);

void mk_floating_zero_constant(UNUSED_ARG(struct ir_unit *iru),
                               struct ir_op *op, enum ir_var_primitive_ty ty) {
  DEBUG_ASSERT(primitive_ty_is_fp(ty), "expected fp ty");

  op->ty = IR_OP_TY_CNST;
  op->var_ty =
      (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE, .primitive = ty};
  op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_FLT, .int_value = 0};
}

void mk_integral_constant(UNUSED_ARG(struct ir_unit *iru), struct ir_op *op,
                          enum ir_var_primitive_ty ty,
                          unsigned long long value) {
  DEBUG_ASSERT(primitive_ty_is_integral(ty), "expected integral ty");

  op->ty = IR_OP_TY_CNST;
  op->var_ty =
      (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE, .primitive = ty};
  op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = value};
}

void mk_pointer_constant(struct ir_unit *iru, struct ir_op *op,
                         unsigned long long value) {
  op->ty = IR_OP_TY_CNST;
  op->var_ty = var_ty_for_pointer_size(iru);
  op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = value};
}

struct ir_op *build_addr(struct ir_func *irb, struct ir_op *op) {
  DEBUG_ASSERT(op->ty == IR_OP_TY_LOAD || op->ty == IR_OP_TY_STORE ||
                   op->ty == IR_OP_TY_LOAD_BITFIELD ||
                   op->ty == IR_OP_TY_STORE_BITFIELD,
               "only makes sense on load/store ops");

  struct ir_op_addr addr;

  if (op->ty == IR_OP_TY_LOAD) {
    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
      addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = op->load.lcl,
      };
      goto mk_op;
    case IR_OP_LOAD_TY_GLB:
      addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_GLB,
          .glb = op->load.glb,
      };
      goto mk_op;
    case IR_OP_LOAD_TY_ADDR:
      return op->load.addr;
    }
  } else if (op->ty == IR_OP_TY_LOAD_BITFIELD) {
    switch (op->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
      addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = op->load_bitfield.lcl,
      };
      goto mk_op;
    case IR_OP_LOAD_TY_GLB:
      addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_GLB,
          .glb = op->load_bitfield.glb,
      };
      goto mk_op;
    case IR_OP_LOAD_TY_ADDR:
      return op->load_bitfield.addr;
    }
  } else if (op->ty == IR_OP_TY_STORE_BITFIELD) {
    switch (op->store_bitfield.ty) {
    case IR_OP_STORE_TY_LCL:
      addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = op->store_bitfield.lcl,
      };
      goto mk_op;
    case IR_OP_STORE_TY_GLB:
      addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_GLB,
          .glb = op->store_bitfield.glb,
      };
      goto mk_op;
    case IR_OP_STORE_TY_ADDR:
      return op->store_bitfield.addr;
    }
  } else {
    DEBUG_ASSERT(op->ty == IR_OP_TY_STORE, "expected store");

    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
      addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_LCL,
          .lcl = op->store.lcl,
      };
      goto mk_op;
    case IR_OP_STORE_TY_GLB:
      addr = (struct ir_op_addr){
          .ty = IR_OP_ADDR_TY_GLB,
          .glb = op->store.glb,
      };
      goto mk_op;
    case IR_OP_STORE_TY_ADDR:
      return op->store.addr;
    }
  }

mk_op: {
  struct ir_op *res =
      insert_before_ir_op(irb, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  res->addr = addr;
  return res;
}
}

struct ir_var_ty var_ty_make_array(struct ir_unit *iru,
                                   const struct ir_var_ty *underlying,
                                   size_t num_elements) {
  struct ir_var_ty *copied = arena_alloc(iru->arena, sizeof(*copied));

  *copied = *underlying;

  struct ir_var_ty var_ty;
  var_ty.ty = IR_VAR_TY_TY_ARRAY;
  var_ty.array = (struct ir_var_array_ty){.num_elements = num_elements,
                                          .underlying = copied};

  return var_ty;
}

struct ir_var_ty var_ty_for_pointer_size(struct ir_unit *iru) {
  // TODO: again, similar to parser:
  // either we need a pointer-sized int type or for `ir_func` to know the
  // native integer size
  return (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE,
                            .primitive = var_ty_pointer_primitive_ty(iru)};
}

struct ir_op *alloc_integral_constant(struct ir_func *irb, struct ir_stmt *stmt,
                                      enum ir_var_primitive_ty primitive,
                                      unsigned long long value);

bool valid_basicblock(struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;
  while (stmt) {
    struct ir_op *op = stmt->first;
    while (op) {
      if (op_is_branch(op->ty)) {
        // it must be the *last* op
        return op->succ == NULL && stmt->succ == NULL;
      }

      op = op->succ;
    }

    stmt = stmt->succ;
  }

  // we have seen no branches, this cannot be a valid basicblock
  return false;
}

void add_pred_to_basicblock(struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            struct ir_basicblock *pred) {
  for (size_t i = 0; i < basicblock->num_preds; i++) {
    if (basicblock->preds[i] == pred) {
      // pred already present
      return;
    }
  }

  basicblock->num_preds++;
  basicblock->preds =
      arena_realloc(irb->arena, basicblock->preds,
                    sizeof(struct ir_basicblock *) * basicblock->num_preds);

  basicblock->preds[basicblock->num_preds - 1] = pred;
}

void make_basicblock_split(struct ir_func *irb,
                           struct ir_basicblock *basicblock,
                           struct ir_basicblock *true_target,
                           struct ir_basicblock *false_target) {
  basicblock->ty = IR_BASICBLOCK_TY_SPLIT;
  basicblock->split = (struct ir_basicblock_split){
      .true_target = true_target, .false_target = false_target};

  add_pred_to_basicblock(irb, true_target, basicblock);
  add_pred_to_basicblock(irb, false_target, basicblock);
}

void make_basicblock_merge(struct ir_func *irb,
                           struct ir_basicblock *basicblock,
                           struct ir_basicblock *target) {
  basicblock->ty = IR_BASICBLOCK_TY_MERGE;
  basicblock->merge = (struct ir_basicblock_merge){.target = target};

  add_pred_to_basicblock(irb, target, basicblock);
}

void make_basicblock_switch(struct ir_func *irb,
                            struct ir_basicblock *basicblock, size_t num_cases,
                            struct ir_split_case *cases,
                            struct ir_basicblock *default_target) {
  basicblock->ty = IR_BASICBLOCK_TY_SWITCH;
  basicblock->switch_case = (struct ir_basicblock_switch){
      .cases = arena_alloc(irb->arena,
                           sizeof(*basicblock->switch_case.cases) * num_cases),
      .num_cases = num_cases,
      .default_target = default_target};

  memcpy(basicblock->switch_case.cases, cases,
         sizeof(*basicblock->switch_case.cases) * num_cases);

  for (size_t i = 0; i < num_cases; i++) {
    add_pred_to_basicblock(irb, cases[i].target, basicblock);
  }

  add_pred_to_basicblock(irb, default_target, basicblock);
}

struct ir_basicblock *insert_basicblocks_after_op(struct ir_func *irb,
                                                  struct ir_op *insert_after,
                                                  struct ir_basicblock *first) {
  struct ir_basicblock *orig_bb = insert_after->stmt->basicblock;

  struct ir_basicblock *end_bb = alloc_ir_basicblock(irb);
  struct ir_stmt *end_stmt = alloc_ir_stmt(irb, end_bb);

  // now move all later instructions to the end bb
  // first break up the stmt we are in
  end_stmt->first = insert_after->succ;
  if (insert_after->succ) {
    insert_after->succ->pred = NULL;
  }

  end_stmt->last = insert_after->stmt->last;
  insert_after->stmt->last->succ = NULL;

  insert_after->stmt->last = insert_after;
  insert_after->succ = NULL;

  end_stmt->succ = insert_after->stmt->succ;
  insert_after->stmt->pred = end_stmt;

  end_bb->last = insert_after->stmt == orig_bb->last ? end_stmt : orig_bb->last;

  orig_bb->last = insert_after->stmt;
  insert_after->stmt->succ = NULL;

  insert_after->stmt->last = insert_after;
  insert_after->succ = NULL;

  // forward block end
  end_bb->ty = orig_bb->ty;
  if (orig_bb->ty == IR_BASICBLOCK_TY_SPLIT) {
    end_bb->split = orig_bb->split;
  } else if (orig_bb->ty == IR_BASICBLOCK_TY_MERGE) {
    end_bb->merge = orig_bb->merge;
  }

  orig_bb->ty = IR_BASICBLOCK_TY_MERGE;
  orig_bb->merge.target = first;

  return end_bb;
}

struct ir_glb *add_well_known_global(struct ir_unit *iru,
                                     enum ir_well_known_glb glb) {
  switch (glb) {
  case IR_WELL_KNOWN_GLB_MEMMOVE: {
    if (iru->well_known_glbs.memmove) {
      return iru->well_known_glbs.memmove;
    }

    struct ir_var_ty *ptr = arena_alloc(iru->arena, sizeof(*ptr));
    *ptr = IR_VAR_TY_POINTER;

    size_t num_params = 3;
    struct ir_var_ty *params =
        arena_alloc(iru->arena, sizeof(*params) * num_params);

    params[0] = IR_VAR_TY_POINTER;
    params[1] = IR_VAR_TY_POINTER; // TODO: const-qualified
    params[2] = var_ty_for_pointer_size(iru);

    struct ir_var_ty var_ty = {.ty = IR_VAR_TY_TY_FUNC,
                               .func = {.ret_ty = ptr,
                                        .num_params = num_params,
                                        .params = params,
                                        .flags = IR_VAR_FUNC_TY_FLAG_NONE}};

    struct ir_glb *memmove = add_global(iru, IR_GLB_TY_FUNC, &var_ty,
                                        IR_GLB_DEF_TY_UNDEFINED, "memmove");

    iru->well_known_glbs.memmove = memmove;
    return memmove;
  }

  case IR_WELL_KNOWN_GLB_MEMCPY: {
    if (iru->well_known_glbs.memcpy) {
      return iru->well_known_glbs.memcpy;
    }

    struct ir_var_ty *ptr = arena_alloc(iru->arena, sizeof(*ptr));
    *ptr = IR_VAR_TY_POINTER;

    size_t num_params = 3;
    struct ir_var_ty *params =
        arena_alloc(iru->arena, sizeof(*params) * num_params);

    params[0] = IR_VAR_TY_POINTER;
    params[1] = IR_VAR_TY_POINTER; // TODO: const-qualified
    params[2] = var_ty_for_pointer_size(iru);

    struct ir_var_ty var_ty = {.ty = IR_VAR_TY_TY_FUNC,
                               .func = {.ret_ty = ptr,
                                        .num_params = num_params,
                                        .params = params,
                                        .flags = IR_VAR_FUNC_TY_FLAG_NONE}};

    struct ir_glb *memcpy = add_global(iru, IR_GLB_TY_FUNC, &var_ty,
                                       IR_GLB_DEF_TY_UNDEFINED, "memcpy");

    iru->well_known_glbs.memcpy = memcpy;
    return memcpy;
  }
  case IR_WELL_KNOWN_GLB_MEMSET: {
    if (iru->well_known_glbs.memset) {
      return iru->well_known_glbs.memset;
    }

    struct ir_var_ty *ptr = arena_alloc(iru->arena, sizeof(*ptr));
    *ptr = IR_VAR_TY_POINTER;

    size_t num_params = 3;
    struct ir_var_ty *params =
        arena_alloc(iru->arena, sizeof(*params) * num_params);

    params[0] = IR_VAR_TY_POINTER;
    params[1] = IR_VAR_TY_I32;
    params[2] = var_ty_for_pointer_size(iru);

    struct ir_var_ty var_ty = {.ty = IR_VAR_TY_TY_FUNC,
                               .func = {.ret_ty = ptr,
                                        .num_params = num_params,
                                        .params = params,
                                        .flags = IR_VAR_FUNC_TY_FLAG_NONE}};

    struct ir_glb *memset = add_global(iru, IR_GLB_TY_FUNC, &var_ty,
                                       IR_GLB_DEF_TY_UNDEFINED, "memset");

    iru->well_known_glbs.memset = memset;
    return memset;
  }
  }
}

struct ir_glb *add_global(struct ir_unit *iru, enum ir_glb_ty ty,
                          const struct ir_var_ty *var_ty,
                          enum ir_glb_def_ty def_ty, const char *name) {
  struct ir_glb *glb = arena_alloc(iru->arena, sizeof(*glb));

  struct ir_glb *pred = iru->last_global;

  iru->num_globals++;
  glb->id = pred ? pred->id + 1 : 0;
  glb->succ = NULL;
  glb->pred = pred;
  glb->ty = ty;
  glb->name = name;
  glb->def_ty = def_ty;
  glb->var_ty = *var_ty;

  // TODO: for debugging
  glb->func = NULL;
  glb->var = NULL;

  if (!iru->first_global) {
    iru->first_global = glb;
  }

  if (iru->last_global) {
    iru->last_global->succ = glb;
  }

  iru->last_global = glb;

  return glb;
}

struct ir_lcl *add_local(struct ir_func *irb, const struct ir_var_ty *var_ty) {
  struct ir_lcl *lcl = arena_alloc(irb->arena, sizeof(*lcl));
  lcl->id = irb->lcl_count++;

  lcl->func = irb;
  lcl->flags = IR_LCL_FLAG_NONE;
  lcl->var_ty = *var_ty;
  lcl->store = NULL;
  lcl->pred = irb->last_lcl;
  lcl->succ = NULL;
  lcl->metadata = NULL;
  lcl->alloc_ty = IR_LCL_ALLOC_TY_NONE;

  if (!irb->first_lcl) {
    irb->first_lcl = lcl;
  }

  if (irb->last_lcl) {
    irb->last_lcl->succ = lcl;
  }

  irb->last_lcl = lcl;

  return lcl;
}

void detach_local(struct ir_func *irb, struct ir_lcl *lcl) {
  if (lcl->id == DETACHED_LCL) {
    return;
  }

  irb->lcl_count--;

  lcl->id = DETACHED_LCL;

  // fix links on either side of lcl
  if (lcl->pred) {
    lcl->pred->succ = lcl->succ;
  } else {
    irb->first_lcl = lcl->succ;
  }

  if (lcl->succ) {
    lcl->succ->pred = lcl->pred;
  } else {
    irb->last_lcl = lcl->pred;
  }

  lcl->func = NULL;
}

bool var_ty_is_aggregate(const struct ir_var_ty *var_ty) {
  return var_ty->ty == IR_VAR_TY_TY_STRUCT || var_ty->ty == IR_VAR_TY_TY_UNION;
}

bool var_ty_is_primitive(const struct ir_var_ty *var_ty,
                         enum ir_var_primitive_ty primitive) {
  return var_ty->ty == IR_VAR_TY_TY_PRIMITIVE && var_ty->primitive == primitive;
}

static bool primitive_ty_is_integral(enum ir_var_primitive_ty ty) {
  switch (ty) {
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
  case IR_VAR_PRIMITIVE_TY_I64:
    return true;
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    return false;
  }
}

static bool primitive_ty_is_fp(enum ir_var_primitive_ty ty) {
  switch (ty) {
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    return true;
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
  case IR_VAR_PRIMITIVE_TY_I64:
    return false;
  }
}

bool var_ty_is_integral(const struct ir_var_ty *var_ty) {
  if (var_ty->ty == IR_VAR_TY_TY_POINTER) {
    return true;
  }

  if (var_ty->ty != IR_VAR_TY_TY_PRIMITIVE) {
    return false;
  }

  return primitive_ty_is_integral(var_ty->primitive);
}

bool var_ty_is_fp(const struct ir_var_ty *var_ty) {
  if (var_ty->ty != IR_VAR_TY_TY_PRIMITIVE) {
    return false;
  }

  return primitive_ty_is_fp(var_ty->primitive);
}

struct ir_var_ty_info var_ty_info(struct ir_unit *iru,
                                  const struct ir_var_ty *ty) {
  switch (ty->ty) {
  case IR_VAR_TY_TY_NONE:
    BUG("IR_OP_VAR_TY_TY_NONE has no size");
  case IR_VAR_TY_TY_VARIADIC:
    BUG("IR_OP_VAR_TY_TY_VARIADIC has no size");
  case IR_VAR_TY_TY_FUNC:
  case IR_VAR_TY_TY_POINTER:
    switch (iru->target->lp_sz) {
    case TARGET_LP_SZ_LP32:
      return (struct ir_var_ty_info){.size = 4, .alignment = 4};
    case TARGET_LP_SZ_LP64:
      return (struct ir_var_ty_info){.size = 8, .alignment = 8};
    }
  case IR_VAR_TY_TY_PRIMITIVE:
    switch (ty->primitive) {
    case IR_VAR_PRIMITIVE_TY_I8:
      return (struct ir_var_ty_info){.size = 1, .alignment = 1};
    case IR_VAR_PRIMITIVE_TY_I16:
      return (struct ir_var_ty_info){.size = 2, .alignment = 2};
    case IR_VAR_PRIMITIVE_TY_I32:
      return (struct ir_var_ty_info){.size = 4, .alignment = 4};
    case IR_VAR_PRIMITIVE_TY_I64:
      return (struct ir_var_ty_info){.size = 8, .alignment = 8};
    case IR_VAR_PRIMITIVE_TY_F16:
      return (struct ir_var_ty_info){.size = 2, .alignment = 2};
    case IR_VAR_PRIMITIVE_TY_F32:
      return (struct ir_var_ty_info){.size = 4, .alignment = 4};
    case IR_VAR_PRIMITIVE_TY_F64:
      return (struct ir_var_ty_info){.size = 8, .alignment = 8};
    }
  case IR_VAR_TY_TY_ARRAY: {
    struct ir_var_ty_info element_info = var_ty_info(iru, ty->array.underlying);
    size_t size = ty->array.num_elements * element_info.size;
    return (struct ir_var_ty_info){.size = size,
                                   .alignment = element_info.alignment};
  }
  case IR_VAR_TY_TY_STRUCT: {
    size_t max_alignment = 0;
    size_t size = 0;
    size_t num_fields = ty->aggregate.num_fields;
    size_t *offsets = arena_alloc(iru->arena, sizeof(*offsets) * num_fields);

    for (size_t i = 0; i < ty->aggregate.num_fields; i++) {
      struct ir_var_ty *field = &ty->aggregate.fields[i];
      struct ir_var_ty_info info = var_ty_info(iru, field);
      max_alignment = MAX(max_alignment, info.alignment);

      size = ROUND_UP(size, info.alignment);

      offsets[i] = size;

      size += info.size;
    }

    return (struct ir_var_ty_info){.size = size,
                                   .alignment = max_alignment,
                                   .num_fields = num_fields,
                                   .offsets = offsets};
  }
  case IR_VAR_TY_TY_UNION: {
    size_t max_alignment = 0;
    size_t size = 0;
    size_t num_fields = ty->aggregate.num_fields;

    for (size_t i = 0; i < ty->aggregate.num_fields; i++) {
      struct ir_var_ty *field = &ty->aggregate.fields[i];
      struct ir_var_ty_info info = var_ty_info(iru, field);
      max_alignment = MAX(max_alignment, info.alignment);

      size = MAX(size, info.size);
    }

    return (struct ir_var_ty_info){.size = size,
                                   .alignment = max_alignment,
                                   .num_fields = num_fields,
                                   .offsets = NULL};
  }
  }
}

struct ir_op *spill_op(struct ir_func *irb, struct ir_op *op) {
  DEBUG_ASSERT(!(op->flags & IR_OP_FLAG_FIXED_REG),
               "spilling fixed reg illegal (op %zu)", op->id);

  debug("spilling %zu\n", op->id);

  if (!op->lcl) {
    op->lcl = add_local(irb, &op->var_ty);
  }

  if (op->flags & IR_OP_FLAG_SPILLED) {
    return NULL;
  }

  if (op->ty != IR_OP_TY_UNDF && !(op->flags & IR_OP_FLAG_SPILLED)) {
    op->flags |= IR_OP_FLAG_SPILLED;

    struct ir_op *store;
    if (op->ty == IR_OP_TY_PHI) {
      // FIXME: phis should be in their own statement!

      struct ir_stmt *succ = op->stmt->succ;
      DEBUG_ASSERT(!succ || !succ->first || succ->first->ty != IR_OP_TY_PHI,
                   "expected all phi to be in same stmt");

      if (succ->first) {
        store = insert_before_ir_op(irb, succ->first, IR_OP_TY_STORE,
                                    IR_VAR_TY_NONE);
      } else {
        store = alloc_ir_op(irb, succ);
        store->ty = IR_OP_TY_STORE;
        store->var_ty = IR_VAR_TY_NONE;
      }
    } else {
      store = insert_after_ir_op(irb, op, IR_OP_TY_STORE, IR_VAR_TY_NONE);
    }

    store->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_LCL, .lcl = op->lcl, .value = op};

    store->reg = NO_REG;
    store->flags |= IR_OP_FLAG_SPILL;

    op->lcl->store = store;
    return store;
  }

  return NULL;
}

struct build_op_uses_callback_data {
  struct ir_op *op;
  struct op_use_data *use_data;
};

struct op_use_data {
  struct ir_op *op;
  struct vector *uses;
};

struct lcl_use_data {
  struct ir_lcl *lcl;
  struct vector *uses;
};

static void build_op_uses_callback(struct ir_op **op, void *cb_metadata) {
  struct build_op_uses_callback_data *data = cb_metadata;

  struct ir_op_use use = {.op = op, .consumer = data->op};

  DEBUG_ASSERT((*op)->id != DETACHED_OP, "detached op consumed by %zu",
               use.consumer->id);
  vector_push_back(data->use_data[(*op)->id].uses, &use);
}

struct ir_op_use_map build_op_uses_map(struct ir_func *func) {
  rebuild_ids(func);

  struct build_op_uses_callback_data data = {
      .op = NULL,
      .use_data =
          arena_alloc(func->arena, sizeof(*data.use_data) * func->op_count)};

  struct lcl_use_data *lcl_usage =
      arena_alloc(func->arena, sizeof(*lcl_usage) * func->lcl_count);

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  {
    size_t i = 0;
    struct ir_op *op;
    while (ir_func_iter_next(&iter, &op)) {
      DEBUG_ASSERT(op->id == i, "ids unordered");

      // because walk_op_uses can be out of order we need to create the vectors
      // in advance
      data.use_data[i++] = (struct op_use_data){
          .op = op, .uses = vector_create(sizeof(struct ir_op_use))};
    }
  }

  {
    size_t i = 0;
    struct ir_lcl *lcl = func->first_lcl;
    while (lcl) {
      DEBUG_ASSERT(lcl->id == i, "ids unordered");

      // because walk_op_uses can be out of order we need to create the vectors
      // in advance
      lcl_usage[i++] = (struct lcl_use_data){
          .lcl = lcl, .uses = vector_create(sizeof(struct ir_op *))};

      lcl = lcl->succ;
    }
  }

  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        struct ir_lcl *lcl = NULL;
#define GET_LCL(hi, ty_name, lo)                                               \
  case IR_OP_TY_##hi:                                                          \
    if (op->lo.ty == IR_OP_##ty_name##_TY_LCL) {                               \
      lcl = op->lo.lcl;                                                        \
    }                                                                          \
    break;

        switch (op->ty) {
          GET_LCL(ADDR, ADDR, addr);

          GET_LCL(LOAD, LOAD, load);
          GET_LCL(STORE, STORE, store);

          GET_LCL(LOAD_BITFIELD, LOAD, load_bitfield);
          GET_LCL(STORE_BITFIELD, STORE, store_bitfield);
        default:
          break;
        }
#undef GET_LCL

        if (lcl) {
          DEBUG_ASSERT(lcl->id != DETACHED_LCL, "lcl detached");

          struct lcl_use_data *usage = &lcl_usage[lcl->id];
          usage->lcl = lcl;
          vector_push_back(usage->uses, &op);
        }

        data.op = op;
        data.use_data[op->id].op = op;

        walk_op_uses(op, build_op_uses_callback, &data);

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  struct ir_op_use_map uses = {
      .num_op_use_datas = func->op_count,
      .op_use_datas =
          arena_alloc(func->arena, sizeof(*uses.op_use_datas) * func->op_count),

      .num_lcl_use_datas = func->lcl_count,
      .lcl_use_datas = arena_alloc(func->arena, sizeof(*uses.lcl_use_datas) *
                                                    func->lcl_count),
  };

  for (size_t i = 0; i < func->op_count; i++) {
    struct op_use_data *use_data = &data.use_data[i];

    DEBUG_ASSERT(i == use_data->op->id, "ops were not keyed");

    uses.op_use_datas[i] = (struct ir_op_usage){
        .op = use_data->op,
        .num_uses = vector_length(use_data->uses),
        .uses = arena_alloc(func->arena, vector_byte_size(use_data->uses))};

    vector_copy_to(use_data->uses, uses.op_use_datas[i].uses);
    vector_free(&use_data->uses);
  }

  for (size_t i = 0; i < func->lcl_count; i++) {
    struct lcl_use_data *use_data = &lcl_usage[i];

    DEBUG_ASSERT(i == use_data->lcl->id, "ops were not keyed");

    uses.lcl_use_datas[i] = (struct ir_lcl_usage){
        .lcl = use_data->lcl,
        .num_consumers = vector_length(use_data->uses),
        .consumers =
            arena_alloc(func->arena, vector_byte_size(use_data->uses))};

    vector_copy_to(use_data->uses, uses.lcl_use_datas[i].consumers);
    vector_free(&use_data->uses);
  }

  return uses;
}

size_t unique_idx_for_ir_reg(struct ir_reg reg) {
  return reg.idx * /* num tys */ 5 + reg.ty;
}

struct ir_reg ir_reg_for_unique_idx(size_t idx) {
  return (struct ir_reg){.ty = idx % 5, .idx = idx / 5};
}

struct move_set gen_move_order(struct arena_allocator *arena,
                               struct location *from, struct location *to,
                               size_t num, size_t tmp_index) {
  enum stage { NOT_READY, READY };
  enum status { TODO, INPROC, DONE };
  struct item {
    enum stage stage;
    size_t idx;
  };

  struct vector *result = vector_create(sizeof(struct move));

  enum status *status = arena_alloc(arena, sizeof(*status) * num);
  if (status) {
    memset(status, 0, sizeof(*status) * num);
  }

  struct vector *in_proc = vector_create(sizeof(struct item));
  for (size_t i = 0; i < num; i++) {
    vector_clear(in_proc);

    if (status[i] != TODO) {
      continue;
    }

    struct item init = {.stage = NOT_READY, .idx = i};
    vector_push_back(in_proc, &init);

    while (!vector_empty(in_proc)) {
      struct item *item = vector_pop(in_proc);
      size_t index = item->idx;

      if (from[index].idx != to[index].idx) {
        if (item->stage == NOT_READY) {
          switch (status[index]) {
          case TODO:
            status[index] = INPROC;

            // Finalise this relation later
            struct item todo = {.stage = READY, .idx = index};

            vector_push_back(in_proc, &todo);

            // Add dependencies
            for (int j = num - 1; j >= 0; j--) {
              if (from[j].idx == to[index].idx) {
                struct item start = {.stage = NOT_READY, .idx = j};
                vector_push_back(in_proc, &start);
              }
            }
            break;

          case INPROC: {
            struct move move = {.from = from[index], .to = {.idx = tmp_index}};
            vector_push_back(result, &move);
            from[index] = (struct location){.idx = tmp_index};
            break;
          }

          case DONE:
            break;
          }
        } else if (item->stage == READY) {
          // Emit the move
          struct move move = {.from = from[index], .to = to[index]};
          vector_push_back(result, &move);
          status[index] = DONE;
        }
      }
    }
  }

  struct move_set move_set = {.num_moves = vector_length(result),
                              .moves =
                                  arena_alloc(arena, vector_byte_size(result))};

  vector_copy_to(result, move_set.moves);
  vector_free(&result);
  vector_free(&in_proc);

  return move_set;
}

struct ir_func_iter ir_func_iter(struct ir_func *func,
                                 enum ir_func_iter_flags flags) {
  struct ir_basicblock *basicblock = func->first;

  while (basicblock && !basicblock->first) {
    basicblock = basicblock->succ;
  }

  struct ir_stmt *stmt = basicblock ? basicblock->first : NULL;

  while (stmt && !stmt->first) {
    stmt = stmt->succ;
  }

  struct ir_op *op = stmt ? stmt->first : NULL;

  return (struct ir_func_iter){
      .func = func,
      .flags = flags,
      .basicblock = basicblock,
      .stmt = stmt,
      .op = op,
  };
}

bool ir_func_iter_next(struct ir_func_iter *iter, struct ir_op **op) {
  if (!iter->op) {
    return false;
  }

  *op = iter->op;

  iter->op = iter->op->succ;
  if (!iter->op) {
    do {
      iter->stmt = iter->stmt->succ;

      if (!iter->stmt) {
        do {
          iter->basicblock = iter->basicblock->succ;

          if (!iter->basicblock) {
            iter->op = NULL;
            return true;
          }

          iter->stmt = iter->basicblock->first;
        } while (!iter->basicblock->first);
      }

      iter->op = iter->stmt->first;
    } while (!iter->stmt->first);
  }

  return true;
}

static struct ir_basicblock *intersect(struct ir_basicblock **idoms,
                                       struct ir_basicblock *left,
                                       struct ir_basicblock *right,
                                       size_t *rpo) {
  while (left != right) {
    while (rpo[left->id] < rpo[right->id]) {
      left = idoms[left->id];
    }

    while (rpo[right->id] < rpo[left->id]) {
      right = idoms[right->id];
    }
  }
  return left;
}

static void dfs_postorder(struct ir_basicblock *basicblock, bool *visited,
                          struct ir_basicblock **rpo_order, size_t *count) {
  if (visited[basicblock->id]) {
    return;
  }

  visited[basicblock->id] = true;

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    dfs_postorder(basicblock->split.true_target, visited, rpo_order, count);
    dfs_postorder(basicblock->split.false_target, visited, rpo_order, count);
    break;
  case IR_BASICBLOCK_TY_MERGE:
    dfs_postorder(basicblock->merge.target, visited, rpo_order, count);
    break;
  case IR_BASICBLOCK_TY_SWITCH:
    for (size_t i = 0; i < basicblock->switch_case.num_cases; i++) {
      dfs_postorder(basicblock->switch_case.cases[i].target, visited, rpo_order,
                    count);
    }

    if (basicblock->switch_case.default_target) {
      dfs_postorder(basicblock->switch_case.default_target, visited, rpo_order,
                    count);
    }
    break;
  }

  rpo_order[(*count)++] = basicblock;
}

static void compute_idoms(struct ir_func *func, struct ir_basicblock **idoms) {
  size_t *rpo =
      arena_alloc(func->arena, func->basicblock_count * sizeof(size_t));
  struct ir_basicblock **rpo_order = arena_alloc(
      func->arena, func->basicblock_count * sizeof(struct ir_basicblock *));

  bool *visited =
      arena_alloc(func->arena, func->basicblock_count * sizeof(bool));
  memset(visited, false, func->basicblock_count * sizeof(bool));

  size_t count = 0;

  struct ir_basicblock *entry = func->first;

  dfs_postorder(entry, visited, rpo_order, &count);

  for (size_t i = 0; i < count; i++) {
    rpo[rpo_order[i]->id] = count - 1 - i;
  }

  memset(idoms, 0, func->basicblock_count * sizeof(struct ir_basicblock *));
  idoms[entry->id] = entry;

  size_t changed = 1;

  while (changed) {
    changed = 0;

    for (size_t i = 0; i < count; i++) {
      struct ir_basicblock *b = rpo_order[i];

      if (b == entry) {
        continue;
      }

      struct ir_basicblock *new_idom = NULL;

      for (size_t j = 0; j < b->num_preds; j++) {
        struct ir_basicblock *p = b->preds[j];

        if (idoms[p->id]) {
          new_idom = p;
          break;
        }
      }

      if (!new_idom) {
        continue;
      }

      for (size_t j = 0; j < b->num_preds; j++) {
        struct ir_basicblock *p = b->preds[j];
        if (!idoms[p->id] || p == new_idom) {
          continue;
        }

        new_idom = intersect(idoms, new_idom, p, rpo);
      }

      if (idoms[b->id] != new_idom) {
        idoms[b->id] = new_idom;
        changed = 1;
      }
    }
  }
}

static void compute_df_recursive(struct ir_basicblock *basicblock,
                                 struct ir_basicblock **idoms,
                                 struct vector **domf,
                                 struct vector **children) {
  for (size_t i = 0; i < vector_length(children[basicblock->id]); i++) {
    struct ir_basicblock *child =
        *(struct ir_basicblock **)vector_get(children[basicblock->id], i);

    compute_df_recursive(child, idoms, domf, children);

    for (size_t j = 0; j < vector_length(domf[child->id]); j++) {
      struct ir_basicblock *w =
          *(struct ir_basicblock **)vector_get(domf[child->id], j);

      if (idoms[w->id] != basicblock) {
        vector_push_back(domf[basicblock->id], &w);
      }
    }
  }

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    if (idoms[basicblock->split.true_target->id] != basicblock) {
      vector_push_back(domf[basicblock->id], &basicblock->split.true_target);
    }

    if (idoms[basicblock->split.false_target->id] != basicblock) {
      vector_push_back(domf[basicblock->id], &basicblock->split.false_target);
    }
    break;
  case IR_BASICBLOCK_TY_MERGE:
    if (idoms[basicblock->merge.target->id] != basicblock) {
      vector_push_back(domf[basicblock->id], &basicblock->merge.target);
    }
    break;
  case IR_BASICBLOCK_TY_SWITCH:
    for (size_t i = 0; i < basicblock->switch_case.num_cases; i++) {
      if (idoms[basicblock->switch_case.cases[i].target->id] != basicblock) {
        vector_push_back(domf[basicblock->id],
                         &basicblock->switch_case.cases[i].target);
      }
    }

    if (basicblock->switch_case.default_target &&
        idoms[basicblock->switch_case.default_target->id] != basicblock) {
      vector_push_back(domf[basicblock->id],
                       &basicblock->switch_case.default_target);
    }
    break;
  }
}

struct ir_dominance_frontier
ir_compute_dominance_frontier(struct ir_func *func) {
  struct vector **domf = arena_alloc(func->arena, func->basicblock_count *
                                                      sizeof(struct vector *));
  struct vector **children = arena_alloc(
      func->arena, func->basicblock_count * sizeof(struct vector *));

  struct ir_basicblock **idoms = arena_alloc(
      func->arena, func->basicblock_count * sizeof(struct ir_basicblock *));
  compute_idoms(func, idoms);

  for (size_t i = 0; i < func->basicblock_count; i++) {
    domf[i] = vector_create(sizeof(struct ir_basicblock *));
    children[i] = vector_create(sizeof(struct ir_basicblock *));
  }

  struct ir_basicblock *entry = func->first;

  struct ir_basicblock *basicblock = entry->succ;
  while (basicblock) {
    vector_push_back(children[idoms[basicblock->id]->id], &basicblock);

    basicblock = basicblock->succ;
  }

  compute_df_recursive(entry, idoms, domf, children);

  return (struct ir_dominance_frontier){.idom_children = children,
                                        .domfs = domf};
}

void alloc_locals(struct ir_func *func) {
  struct ir_lcl *lcl = func->first_lcl;
  while (lcl) {
    if (lcl->alloc_ty != IR_LCL_ALLOC_TY_NONE) {
      lcl = lcl->succ;
      continue;
    }

    struct ir_var_ty_info ty_info = var_ty_info(func->unit, &lcl->var_ty);

    size_t lcl_pad =
        (ty_info.alignment - (func->total_locals_size % ty_info.alignment)) %
        ty_info.alignment;

    size_t lcl_size = ty_info.size;

    func->total_locals_size += lcl_pad;

    lcl->alloc_ty = IR_LCL_ALLOC_TY_NORMAL;
    lcl->offset = func->total_locals_size;

    func->total_locals_size += lcl_size;

    lcl = lcl->succ;
  }
}
