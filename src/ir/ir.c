#include "ir.h"

#include "../alloc.h"
#include "../log.h"
#include "../target.h"
#include "../util.h"
#include "../vector.h"
#include "prettyprint.h"

enum ir_var_primitive_ty ir_var_ty_pointer_primitive_ty(struct ir_unit *iru) {
  switch (iru->target->lp_sz) {
  case TARGET_LP_SZ_LP32:
    return IR_VAR_PRIMITIVE_TY_I32;
  case TARGET_LP_SZ_LP64:
    return IR_VAR_PRIMITIVE_TY_I64;
  }
}

bool ir_binary_op_is_comparison(enum ir_op_binary_op_ty ty) {
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
  case IR_OP_BINARY_OP_TY_SMOD:
  case IR_OP_BINARY_OP_TY_UMOD:
  case IR_OP_BINARY_OP_TY_FADD:
  case IR_OP_BINARY_OP_TY_FSUB:
  case IR_OP_BINARY_OP_TY_FMUL:
  case IR_OP_BINARY_OP_TY_FDIV:
  case IR_OP_BINARY_OP_TY_FMAX:
  case IR_OP_BINARY_OP_TY_FMIN:
    return false;
  }
}

enum ir_op_binary_op_ty
ir_invert_binary_comparison(enum ir_op_binary_op_ty ty) {
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

enum ir_op_binary_op_ty ir_flip_binary_comparison(enum ir_op_binary_op_ty ty) {
  switch (ty) {
  case IR_OP_BINARY_OP_TY_EQ:
    return IR_OP_BINARY_OP_TY_EQ;
  case IR_OP_BINARY_OP_TY_NEQ:
    return IR_OP_BINARY_OP_TY_NEQ;
  case IR_OP_BINARY_OP_TY_UGT:
    return IR_OP_BINARY_OP_TY_ULT;
  case IR_OP_BINARY_OP_TY_SGT:
    return IR_OP_BINARY_OP_TY_SLT;
  case IR_OP_BINARY_OP_TY_UGTEQ:
    return IR_OP_BINARY_OP_TY_ULTEQ;
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return IR_OP_BINARY_OP_TY_SLTEQ;
  case IR_OP_BINARY_OP_TY_ULT:
    return IR_OP_BINARY_OP_TY_UGT;
  case IR_OP_BINARY_OP_TY_SLT:
    return IR_OP_BINARY_OP_TY_SGT;
  case IR_OP_BINARY_OP_TY_ULTEQ:
    return IR_OP_BINARY_OP_TY_UGTEQ;
  case IR_OP_BINARY_OP_TY_SLTEQ:
    return IR_OP_BINARY_OP_TY_SGTEQ;
  case IR_OP_BINARY_OP_TY_FEQ:
    return IR_OP_BINARY_OP_TY_FEQ;
  case IR_OP_BINARY_OP_TY_FNEQ:
    return IR_OP_BINARY_OP_TY_FNEQ;
  case IR_OP_BINARY_OP_TY_FGT:
    return IR_OP_BINARY_OP_TY_FLT;
  case IR_OP_BINARY_OP_TY_FGTEQ:
    return IR_OP_BINARY_OP_TY_FLTEQ;
  case IR_OP_BINARY_OP_TY_FLT:
    return IR_OP_BINARY_OP_TY_FGT;
  case IR_OP_BINARY_OP_TY_FLTEQ:
    return IR_OP_BINARY_OP_TY_FGTEQ;
  default:
    BUG("binary op was not comparison");
  }
}
bool ir_op_has_side_effects(const struct ir_op *op) {
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
    return op->reg.ty != IR_REG_TY_NONE || !op->mov.value ||
           !ir_reg_eq(op->reg, op->mov.value->reg);
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
  }
}

bool ir_op_produces_value(const struct ir_op *op) {
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
  case IR_OP_TY_GATHER:
    return false;
  }
}

bool ir_op_is_branch(enum ir_op_ty ty) {
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
  }
}

bool ir_var_ty_eq(struct ir_unit *iru, const struct ir_var_ty *l,
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
           ir_var_ty_eq(iru, l->array.underlying, r->array.underlying);
  case IR_VAR_TY_TY_FUNC:
    if (!ir_var_ty_eq(iru, l->func.ret_ty, r->func.ret_ty)) {
      return false;
    }
    if (l->func.num_params != r->func.num_params) {
      return false;
    }
    for (size_t i = 0; i < l->func.num_params; i++) {
      if (!ir_var_ty_eq(iru, &l->func.params[i], &r->func.params[i])) {
        return false;
      }
    }

    return true;
  case IR_VAR_TY_TY_STRUCT: {
    if (l->aggregate.num_fields != r->aggregate.num_fields) {
      return false;
    }

    struct ir_var_ty_info l_info = ir_var_ty_info(iru, l);
    struct ir_var_ty_info r_info = ir_var_ty_info(iru, r);

    // currently we do not have custom alignment/size but it is possible
    if (l_info.size != r_info.size || l_info.alignment != r_info.alignment) {
      return false;
    }

    for (size_t i = 0; i < l->aggregate.num_fields; i++) {
      if (!ir_var_ty_eq(iru, &l->aggregate.fields[i],
                        &r->aggregate.fields[i])) {
        return false;
      }
    }

    return true;
  }
  case IR_VAR_TY_TY_UNION: {
    if (l->aggregate.num_fields != r->aggregate.num_fields) {
      return false;
    }

    struct ir_var_ty_info l_info = ir_var_ty_info(iru, l);
    struct ir_var_ty_info r_info = ir_var_ty_info(iru, r);

    // currently we do not have custom alignment/size but it is possible
    if (l_info.size != r_info.size || l_info.alignment != r_info.alignment) {
      return false;
    }

    for (size_t i = 0; i < l->aggregate.num_fields; i++) {
      if (!ir_var_ty_eq(iru, &l->aggregate.fields[i],
                        &r->aggregate.fields[i])) {
        return false;
      }
    }

    return true;
  }
  }

  unreachable();
}

void ir_walk_op_uses(struct ir_op *op, ir_walk_op_uses_callback *cb,
                     void *cb_metadata) {
  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op!");
  case IR_OP_TY_UNDF:
    break;
  case IR_OP_TY_GATHER:
    for (size_t i = 0; i < op->gather.num_values; i++) {
      cb(&op->gather.values[i].value, IR_OP_USE_TY_READ, cb_metadata);
    }
    break;
  case IR_OP_TY_CALL: {
    cb(&op->call.target, IR_OP_USE_TY_READ, cb_metadata);
    for (size_t i = 0; i < op->call.num_args; i++) {
      cb(&op->call.args[i], IR_OP_USE_TY_READ, cb_metadata);
    }
    break;
  }
  case IR_OP_TY_PHI: {
    for (size_t i = 0; i < op->phi.num_values; i++) {
      cb(&op->phi.values[i].value, IR_OP_USE_TY_READ, cb_metadata);
    }
    break;
  }
  case IR_OP_TY_CNST:
    break;
  case IR_OP_TY_BINARY_OP:
    cb(&op->binary_op.lhs, IR_OP_USE_TY_READ, cb_metadata);
    cb(&op->binary_op.rhs, IR_OP_USE_TY_READ, cb_metadata);
    break;
  case IR_OP_TY_UNARY_OP:
    cb(&op->unary_op.value, IR_OP_USE_TY_READ, cb_metadata);
    break;
  case IR_OP_TY_CAST_OP:
    cb(&op->cast_op.value, IR_OP_USE_TY_READ, cb_metadata);
    break;
  case IR_OP_TY_STORE:
    cb(&op->store.value, IR_OP_USE_TY_READ, cb_metadata);
    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
    case IR_OP_STORE_TY_GLB:
      break;
    case IR_OP_STORE_TY_ADDR:
      cb(&op->store.addr, IR_OP_USE_TY_DEREF, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_STORE_BITFIELD:
    cb(&op->store_bitfield.value, IR_OP_USE_TY_READ, cb_metadata);
    switch (op->store_bitfield.ty) {
    case IR_OP_STORE_TY_LCL:
    case IR_OP_STORE_TY_GLB:
      break;
    case IR_OP_STORE_TY_ADDR:
      cb(&op->store_bitfield.addr, IR_OP_USE_TY_DEREF, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_LOAD:
    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
    case IR_OP_LOAD_TY_GLB:
      break;
    case IR_OP_LOAD_TY_ADDR:
      cb(&op->load.addr, IR_OP_USE_TY_DEREF, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_LOAD_BITFIELD:
    switch (op->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
    case IR_OP_LOAD_TY_GLB:
      break;
    case IR_OP_LOAD_TY_ADDR:
      cb(&op->load_bitfield.addr, IR_OP_USE_TY_DEREF, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_ADDR:
    break;
  case IR_OP_TY_ADDR_OFFSET:
    cb(&op->addr_offset.base, IR_OP_USE_TY_READ, cb_metadata);
    if (op->addr_offset.index) {
      cb(&op->addr_offset.index, IR_OP_USE_TY_READ, cb_metadata);
    }
    break;
  case IR_OP_TY_BR:
    break;
  case IR_OP_TY_BR_SWITCH:
    cb(&op->br_switch.value, IR_OP_USE_TY_READ, cb_metadata);
    break;
  case IR_OP_TY_BR_COND:
    cb(&op->br_cond.cond, IR_OP_USE_TY_READ, cb_metadata);
    break;
  case IR_OP_TY_MOV:
    if (op->mov.value && !(op->flags & IR_OP_FLAG_PARAM)) {
      DEBUG_ASSERT(op->mov.value, "mov %zu had no value but no PARAM flag",
                   op->id);
      cb(&op->mov.value, IR_OP_USE_TY_READ, cb_metadata);
    }
    break;
  case IR_OP_TY_RET:
    if (op->ret.value) {
      cb(&op->ret.value, IR_OP_USE_TY_READ, cb_metadata);
    }
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    cb(&op->bitfield_extract.value, IR_OP_USE_TY_READ, cb_metadata);
    break;
  case IR_OP_TY_BITFIELD_INSERT:
    cb(&op->bitfield_insert.target, IR_OP_USE_TY_READ, cb_metadata);
    cb(&op->bitfield_insert.value, IR_OP_USE_TY_READ, cb_metadata);
    break;
  case IR_OP_TY_MEM_SET:
    cb(&op->mem_set.addr, IR_OP_USE_TY_READ, cb_metadata);
    break;
  case IR_OP_TY_MEM_COPY:
    cb(&op->mem_copy.source, IR_OP_USE_TY_READ, cb_metadata);
    cb(&op->mem_copy.dest, IR_OP_USE_TY_READ, cb_metadata);
    break;
  }
}

void ir_walk_op(struct ir_op *op, ir_walk_op_callback *cb, void *cb_metadata) {
  cb(op, cb_metadata);

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("unknown op!");
  case IR_OP_TY_UNDF:
    break;
  case IR_OP_TY_GATHER:
    for (size_t i = 0; i < op->gather.num_values; i++) {
      ir_walk_op(op->gather.values[i].value, cb, cb_metadata);
    }
    break;
  case IR_OP_TY_CALL: {
    ir_walk_op(op->call.target, cb, cb_metadata);
    for (size_t i = 0; i < op->call.num_args; i++) {
      ir_walk_op(op->call.args[i], cb, cb_metadata);
    }
    break;
  }
  case IR_OP_TY_PHI: {
    for (size_t i = 0; i < op->phi.num_values; i++) {
      ir_walk_op(op->phi.values[i].value, cb, cb_metadata);
    }
    break;
  }
  case IR_OP_TY_CNST:
    break;
  case IR_OP_TY_BINARY_OP:
    ir_walk_op(op->binary_op.lhs, cb, cb_metadata);
    ir_walk_op(op->binary_op.rhs, cb, cb_metadata);
    break;
  case IR_OP_TY_UNARY_OP:
    ir_walk_op(op->unary_op.value, cb, cb_metadata);
    break;
  case IR_OP_TY_CAST_OP:
    ir_walk_op(op->cast_op.value, cb, cb_metadata);
    break;
  case IR_OP_TY_STORE:
    ir_walk_op(op->store.value, cb, cb_metadata);
    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
    case IR_OP_STORE_TY_GLB:
      break;
    case IR_OP_STORE_TY_ADDR:
      ir_walk_op(op->store.addr, cb, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_STORE_BITFIELD:
    ir_walk_op(op->store_bitfield.value, cb, cb_metadata);
    switch (op->store_bitfield.ty) {
    case IR_OP_STORE_TY_LCL:
    case IR_OP_STORE_TY_GLB:
      break;
    case IR_OP_STORE_TY_ADDR:
      ir_walk_op(op->store_bitfield.addr, cb, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_LOAD:
    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
    case IR_OP_LOAD_TY_GLB:
      break;
    case IR_OP_LOAD_TY_ADDR:
      ir_walk_op(op->load.addr, cb, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_LOAD_BITFIELD:
    switch (op->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
    case IR_OP_LOAD_TY_GLB:
      break;
    case IR_OP_LOAD_TY_ADDR:
      ir_walk_op(op->load_bitfield.addr, cb, cb_metadata);
      break;
    }
    break;
  case IR_OP_TY_ADDR:
    break;
  case IR_OP_TY_ADDR_OFFSET:
    ir_walk_op(op->addr_offset.base, cb, cb_metadata);
    if (op->addr_offset.index) {
      ir_walk_op(op->addr_offset.index, cb, cb_metadata);
    }
    break;
  case IR_OP_TY_BR:
    break;
  case IR_OP_TY_BR_SWITCH:
    ir_walk_op(op->br_switch.value, cb, cb_metadata);
    break;
  case IR_OP_TY_BR_COND:
    ir_walk_op(op->br_cond.cond, cb, cb_metadata);
    break;
  case IR_OP_TY_MOV:
    if (op->mov.value && !(op->flags & IR_OP_FLAG_PARAM)) {
      ir_walk_op(op->mov.value, cb, cb_metadata);
    }
    break;
  case IR_OP_TY_RET:
    if (op->ret.value) {
      ir_walk_op(op->ret.value, cb, cb_metadata);
    }
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    ir_walk_op(op->bitfield_extract.value, cb, cb_metadata);
    break;
  case IR_OP_TY_BITFIELD_INSERT:
    ir_walk_op(op->bitfield_insert.target, cb, cb_metadata);
    ir_walk_op(op->bitfield_insert.value, cb, cb_metadata);
    break;
  case IR_OP_TY_MEM_SET:
    ir_walk_op(op->mem_set.addr, cb, cb_metadata);
    break;
  case IR_OP_TY_MEM_COPY:
    ir_walk_op(op->mem_copy.source, cb, cb_metadata);
    ir_walk_op(op->mem_copy.dest, cb, cb_metadata);
    break;
  }
}

void ir_walk_stmt(struct ir_stmt *stmt, ir_walk_op_callback *cb,
                  void *cb_metadata) {
  struct ir_op *op = stmt->last;

  ir_walk_op(op, cb, cb_metadata);
}

enum ir_op_sign ir_binary_op_sign(enum ir_op_binary_op_ty ty) {
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
  case IR_OP_BINARY_OP_TY_SMOD:
  case IR_OP_BINARY_OP_TY_SLT:
  case IR_OP_BINARY_OP_TY_SLTEQ:
  case IR_OP_BINARY_OP_TY_SGT:
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return IR_OP_SIGN_SIGNED;
  case IR_OP_BINARY_OP_TY_URSHIFT:
  case IR_OP_BINARY_OP_TY_UDIV:
  case IR_OP_BINARY_OP_TY_UMOD:
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

bool ir_is_func_variadic(const struct ir_var_func_ty *ty) {
  return ty->flags & IR_VAR_FUNC_TY_FLAG_VARIADIC;
}

void ir_initialise_stmt(struct ir_stmt *stmt, size_t id) {
  stmt->id = id;
  stmt->first = NULL;
  stmt->last = NULL;
  stmt->basicblock = NULL;
  stmt->pred = NULL;
  stmt->succ = NULL;
  stmt->flags = IR_STMT_FLAG_NONE;
  stmt->comment = NULL;
}

void ir_initialise_op(struct ir_op *op, size_t id, enum ir_op_ty ty,
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

void ir_remove_basicblock_successors(struct ir_basicblock *basicblock) {
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
}

void ir_detach_basicblock(struct ir_func *irb, struct ir_basicblock *basicblock,
                          enum ir_detach_ir_basicblock_flags flags) {
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

  ir_remove_basicblock_successors(basicblock);

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

void ir_detach_stmt(struct ir_func *irb, struct ir_stmt *stmt) {
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

void ir_detach_op(struct ir_func *irb, struct ir_op *op) {
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

bool ir_stmt_is_empty(struct ir_stmt *stmt) {
  invariant_assert(
      (stmt->first && stmt->last) || (!stmt->first && !stmt->last),
      "stmt had `first` or `last` NULL but not both; this is badly formed");

  // first will be NULL
  return !stmt->first;
}

bool ir_basicblock_is_empty(struct ir_basicblock *basicblock) {
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

bool ir_var_ty_has_reg(const struct ir_var_ty var_ty) {
  return var_ty.ty == IR_VAR_TY_TY_POINTER ||
         var_ty.ty == IR_VAR_TY_TY_PRIMITIVE;
}

enum ir_reg_ty ir_reg_ty_for_var_ty(const struct ir_var_ty var_ty) {
  switch (var_ty.ty) {
  case IR_VAR_TY_TY_POINTER:
    return IR_REG_TY_INTEGRAL;
  case IR_VAR_TY_TY_PRIMITIVE:
    return ir_primitive_ty_is_fp(var_ty.primitive) ? IR_REG_TY_FP
                                                   : IR_REG_TY_INTEGRAL;
  default:
    BUG("doesn't make sense");
  }
}

void ir_order_basicblocks(struct ir_func *func) {
  ir_rebuild_func_ids(func);

  // TODO: topological sort basicblocks

  // for now, just swap conditions that result in two branches becoming one

  struct bb_with_iter {
    struct ir_basicblock *basicblock;
    struct ir_basicblock_succ_iter iter;
  };

  size_t total = func->basicblock_count;
  bool *visited = arena_alloc(func->arena, total * sizeof(bool));
  memset(visited, 0, total * sizeof(bool));

  struct bb_with_iter *stack = arena_alloc(func->arena, total * sizeof(*stack));
  size_t top = 0;

  struct ir_basicblock **postorder =
      arena_alloc(func->arena, total * sizeof(struct ir_basicblock *));
  size_t postorder_count = 0;

  struct ir_basicblock *entry = func->first;

  visited[entry->id] = true;
  stack[top].basicblock = entry;
  stack[top].iter = ir_basicblock_succ_iter(entry);
  top++;

  while (top > 0) {
    struct bb_with_iter *elem = &stack[top - 1];
    struct ir_basicblock *succ;

    if (ir_basicblock_succ_iter_next(&elem->iter, &succ)) {
      if (!visited[succ->id]) {
        visited[succ->id] = true;
        stack[top].basicblock = succ;
        stack[top].iter = ir_basicblock_succ_iter(succ);
        top++;
      }
    } else {
      postorder[postorder_count++] = elem->basicblock;
      top--;
    }
  }

  struct ir_basicblock *prev = func->first;
  DEBUG_ASSERT(postorder[postorder_count - 1] == entry,
               "first bb should always be entry");

  for (size_t i = postorder_count - 1; i; i--) {
    struct ir_basicblock *succ = postorder[i - 1];

    prev->succ = succ;
    if (succ) {
      succ->pred = prev;
      succ->succ = NULL;
    }

    prev = succ;
  }

  ir_rebuild_func_ids(func);
  func->basicblock_count = postorder_count;

  // struct ir_basicblock *basicblock = func->first;
  // while (basicblock) {
  //   if (basicblock->ty == IR_BASICBLOCK_TY_SPLIT) {
  //     struct ir_basicblock_split *split = &basicblock->split;

  //     if (split->true_target == basicblock->succ) {
  //       struct ir_op *br_cond = basicblock->last->last;
  //       DEBUG_ASSERT(br_cond->ty == IR_OP_TY_BR_COND,
  //                    "expected `br.cond` at end of SPLIT bb");
  //       struct ir_op *cond = br_cond->br_cond.cond;

  //       if (cond->ty == IR_OP_TY_BINARY_OP &&
  //           ir_binary_op_is_comparison(cond->binary_op.ty)) {
  //         cond->binary_op.ty =
  //         ir_invert_binary_comparison(cond->binary_op.ty);

  //         struct ir_basicblock *tmp = split->true_target;
  //         split->true_target = split->false_target;
  //         split->false_target = tmp;
  //       }
  //     }
  //   }

  //   basicblock = basicblock->succ;
  // }
}

void ir_rebuild_flags(struct ir_func *func) {
  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  enum ir_func_flags flags = func->flags & ~IR_FUNC_FLAG_MAKES_CALL;

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    if (op->ty == IR_OP_TY_CALL) {
      flags |= IR_FUNC_FLAG_MAKES_CALL;
      break;
    }
  }

  func->flags = flags;
}

void ir_eliminate_redundant_ops(struct ir_func *func,
                                enum ir_eliminate_redundant_ops_flags flags) {
  struct ir_op_use_map use_map = ir_build_op_uses_map(func);

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct vector *detach = vector_create(sizeof(struct ir_op *));

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    switch (op->ty) {
    case IR_OP_TY_MOV:
      if (!(flags & IR_ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_MOVS)) {
        goto side_effects;
      }

      if (!op->mov.value) {
        break;
      }

      // mov is between registers, but not needed
      bool redundant_mov = op->mov.value->reg.ty != IR_REG_TY_NONE &&
                           ir_reg_eq(op->reg, op->mov.value->reg);

      bool cross_reg_mov = ir_var_ty_has_reg(op->var_ty) &&
                           ir_var_ty_has_reg(op->mov.value->var_ty) &&
                           ir_reg_ty_for_var_ty(op->var_ty) !=
                               ir_reg_ty_for_var_ty(op->mov.value->var_ty);

      // mov is useless, and is just an IR artifact
      // so it doesn't move from or to a param/fixed reg op
      bool pointless_mov = !cross_reg_mov &&
                           op->mov.value->reg.ty == IR_REG_TY_NONE &&
                           op->reg.ty == IR_REG_TY_NONE &&
                           !((op->flags | op->mov.value->flags) &
                             (IR_OP_FLAG_PARAM | IR_OP_FLAG_FIXED_REG));

      if (redundant_mov || pointless_mov) {
        struct ir_op_usage usage = use_map.op_use_datas[op->id];

        for (size_t i = 0; i < usage.num_uses; i++) {
          *usage.uses[i].op = op->mov.value;
        }

        if (op->flags & IR_OP_FLAG_SIDE_EFFECTS) {
          op->mov.value->flags |= IR_OP_FLAG_SIDE_EFFECTS;
        }

        vector_push_back(detach, &op);
      }
      break;
    case IR_OP_TY_BR: {
      if (!(flags & IR_ELIMINATE_REDUNDANT_OPS_FLAG_ELIM_BRANCHES)) {
        continue;
      }

      struct ir_basicblock *basicblock = op->stmt->basicblock;
      DEBUG_ASSERT(basicblock->ty == IR_BASICBLOCK_TY_MERGE,
                   "br op in non MERGE bb");
      if (basicblock->succ == basicblock->merge.target) {
        vector_push_back(detach, &op);
      }
      break;
    }
    default:
    side_effects:
      if ((op->flags & IR_OP_FLAG_PARAM) || ir_op_has_side_effects(op)) {
        continue;
      }

      if (!use_map.op_use_datas[op->id].num_uses) {
        vector_push_back(detach, &op);
      }
      break;
    }
  }

  while (vector_length(detach)) {
    ir_detach_op(func, *(struct ir_op **)vector_pop(detach));
  }

  use_map = ir_build_op_uses_map(func);
  iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  while (ir_func_iter_next(&iter, &op)) {
    if (!use_map.op_use_datas[op->id].num_uses &&
        !(op->flags & IR_OP_FLAG_PARAM) && !ir_op_has_side_effects(op)) {
      vector_push_back(detach, &op);
    }
  }

  while (vector_length(detach)) {
    ir_detach_op(func, *(struct ir_op **)vector_pop(detach));
  }

  if (!(flags & IR_ELIMINATE_REDUNDANT_OPS_FLAG_DONT_ELIM_LCLS)) {
    use_map = ir_build_op_uses_map(func);
    struct ir_lcl *lcl = func->first_lcl;
    while (lcl) {
      if (lcl->alloc_ty != IR_LCL_ALLOC_TY_FIXED &&
          !use_map.lcl_use_datas[lcl->id].num_consumers) {
        ir_detach_local(func, lcl);
      }

      lcl = lcl->succ;
    }
  }

  vector_free(&detach);
}

void ir_detach_global(struct ir_unit *iru, struct ir_glb *glb) {
  glb->id = DETACHED_GLB;

  iru->num_globals--;

  // fix links on either side of glb
  if (glb->pred) {
    glb->pred->succ = glb->succ;
  } else {
    iru->first_global = glb->succ;
  }

  if (glb->succ) {
    glb->succ->pred = glb->pred;
  } else {
    iru->last_global = glb->pred;
  }

  glb->succ = NULL;
  glb->pred = NULL;
}

static void ir_prune_globals_walk_var_value(struct ir_unit *iru, bool *seen,
                                            const struct ir_var_value *value) {
  switch (value->ty) {
  case IR_VAR_VALUE_TY_ADDR:
    if (value->addr.glb) {
      seen[value->addr.glb->id] = true;
    }
    break;
  case IR_VAR_VALUE_TY_VALUE_LIST:
    for (size_t i = 0; i < value->value_list.num_values; i++) {
      // can this inf loop?
      ir_prune_globals_walk_var_value(iru, seen, &value->value_list.values[i]);
    }
    break;
  default:
    break;
  }
}

void ir_prune_globals(struct ir_unit *iru) {
  bool *seen = arena_alloc(iru->arena, sizeof(*seen) * iru->num_globals);
  memset(seen, 0, sizeof(*seen) * iru->num_globals);

  struct ir_glb *glb = iru->first_global;

  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      glb = glb->succ;
      continue;
    }

    switch (glb->ty) {
    case IR_GLB_TY_DATA: {
      struct ir_var *var = glb->var;
      ir_prune_globals_walk_var_value(iru, seen, &var->value);
      break;
    }
    case IR_GLB_TY_FUNC: {
      struct ir_func *func = glb->func;

      struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);
      struct ir_op *op;
      while (ir_func_iter_next(&iter, &op)) {
        switch (op->ty) {
        case IR_OP_TY_ADDR:
          if (op->addr.ty == IR_OP_ADDR_TY_GLB) {
            seen[op->addr.glb->id] = true;
          }
          break;
        case IR_OP_TY_LOAD:
          if (op->load.ty == IR_OP_LOAD_TY_GLB) {
            seen[op->load.glb->id] = true;
          }
          break;
        case IR_OP_TY_LOAD_BITFIELD:
          if (op->load_bitfield.ty == IR_OP_LOAD_TY_GLB) {
            seen[op->load_bitfield.glb->id] = true;
          }
          break;
        case IR_OP_TY_STORE:
          if (op->store.ty == IR_OP_STORE_TY_GLB) {
            seen[op->store.glb->id] = true;
          }
          break;
        case IR_OP_TY_STORE_BITFIELD:
          if (op->store_bitfield.ty == IR_OP_STORE_TY_GLB) {
            seen[op->store_bitfield.glb->id] = true;
          }
          break;
        default:
          continue;
        }
      }
      break;
    }
    }

    glb = glb->succ;
  }

  glb = iru->first_global;
  while (glb) {
    // can only strip internal linkage defined things
    if (glb->def_ty == IR_GLB_DEF_TY_DEFINED &&
        glb->linkage == IR_LINKAGE_EXTERNAL) {
      glb = glb->succ;
      continue;
    }

    if (seen[glb->id]) {
      glb = glb->succ;
      continue;
    }

    struct ir_glb *succ = glb->succ;
    ir_detach_global(iru, glb);
    glb = succ;
  }
}

void ir_prune_basicblocks(struct ir_func *irb) {
  if (!irb->first) {
    return;
  }

  bool *seen = arena_alloc(irb->arena, sizeof(*seen) * irb->basicblock_count);
  memset(seen, 0, sizeof(*seen) * irb->basicblock_count);

  ir_prune_stmts(irb, irb->first);

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
      ir_detach_basicblock(irb, basicblock,
                           DETACH_IR_BASICBLOCK_FLAG_ALLOW_PREDS);
    } else {
      ir_prune_stmts(irb, basicblock);
    }

    basicblock = succ;
  }

  // now remove phis pointing to removed blocks
  basicblock = irb->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    if (stmt && stmt->flags & IR_STMT_FLAG_PHI) {
      struct ir_op *op = stmt->first;
      while (op) {
        for (size_t i = 0; i < op->phi.num_values;) {
          struct ir_phi_entry *entry = &op->phi.values[i];

          if (entry->basicblock->id == DETACHED_BASICBLOCK) {
            size_t rem = op->phi.num_values - i - 1;
            memmove(&op->phi.values[i], &op->phi.values[i + 1],
                    rem * sizeof(op->phi.values[i]));

            op->phi.num_values--;
          } else {
            i++;
          }
        }

        op = op->succ;
      }
    }

    basicblock = basicblock->succ;
  }

  // i recommend doing a simplify phi run where you call this

  // means bb->id < bb_count for all bbs
  ir_rebuild_func_ids(irb);

  vector_free(&stack);
}

void ir_prune_stmts(struct ir_func *irb, struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;

  // skip phi stmt
  if (stmt) {
    stmt = stmt->succ;
  }

  while (stmt) {
    // save succ before we detach
    struct ir_stmt *succ = stmt->succ;

    if (ir_stmt_is_empty(stmt)) {
      ir_detach_stmt(irb, stmt);
    }

    stmt = succ;
  }
}

void ir_clear_metadata(struct ir_func *irb) {
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

void ir_rebuild_glb_ids(struct ir_unit *iru) {
  size_t next_glb_id = 0;

  struct ir_glb *glc = iru->first_global;
  while (glc) {
    glc->id = next_glb_id++;

    glc = glc->succ;
  }

  DEBUG_ASSERT(next_glb_id == iru->num_globals,
               "found diff number of globals to expected");
}

void ir_rebuild_func_ids(struct ir_func *irb) {
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

void ir_attach_op(struct ir_func *irb, struct ir_op *op, struct ir_stmt *stmt,
                  struct ir_op *pred, struct ir_op *succ) {
  invariant_assert(!op->stmt && !op->pred && !op->succ,
                   "non-detached op trying to be attached");
  invariant_assert((!pred || pred->succ == succ) &&
                       (!succ || succ->pred == pred),
                   "`pred` and `succ` are not next to each other");
  invariant_assert(stmt, "cannot attach op without stmt");

  irb->op_count++;

  op->id = irb->next_op_id++;

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

void ir_attach_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                    struct ir_basicblock *basicblock, struct ir_stmt *pred,
                    struct ir_stmt *succ) {
  invariant_assert(!stmt->basicblock && !stmt->pred && !stmt->succ,
                   "non-detached stmt trying to be attached");
  invariant_assert((!pred || pred->succ == succ) &&
                       (!succ || succ->pred == pred),
                   "`pred` and `succ` are not next to each other");
  invariant_assert(stmt, "cannot attach stmt without stmt");

  irb->stmt_count++;

  stmt->basicblock = basicblock;

  if (stmt != pred) {
    stmt->pred = pred;
  }

  if (stmt != succ) {
    stmt->succ = succ;
  }

  if (stmt->pred) {
    stmt->pred->succ = stmt;
  } else {
    stmt->basicblock->first = stmt;
  }

  if (stmt->succ) {
    stmt->succ->pred = stmt;
  } else {
    stmt->basicblock->last = stmt;
  }
}

static void ir_attach_basicblock_pred(struct ir_func *irb,
                                      struct ir_basicblock *basicblock,
                                      struct ir_basicblock *pred) {
  if (basicblock != pred) {
    basicblock->pred = pred;
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

static void ir_attach_basicblock_succ(struct ir_func *irb,
                                      struct ir_basicblock *basicblock,
                                      struct ir_basicblock *succ) {
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

void ir_attach_basicblock(struct ir_func *irb, struct ir_basicblock *basicblock,
                          struct ir_basicblock *pred,
                          struct ir_basicblock *succ) {
  invariant_assert(!basicblock->pred && !basicblock->succ,
                   "non-detached basicblock trying to be attached");

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

void ir_move_after_op(struct ir_func *irb, struct ir_op *op,
                      struct ir_op *move_after) {
  invariant_assert(op->id != move_after->id, "trying to move op after itself!");
  invariant_assert(op->stmt == NULL || op->stmt == move_after->stmt,
                   "moving between ir_stmts not supported");

  if (op->stmt) {
    ir_detach_op(irb, op);
  }

  ir_attach_op(irb, op, move_after->stmt, move_after, move_after->succ);
}

void ir_move_before_op(struct ir_func *irb, struct ir_op *op,
                       struct ir_op *move_before) {
  invariant_assert(op->id != move_before->id,
                   "trying to move op before itself!");
  invariant_assert(op->stmt == NULL || op->stmt == move_before->stmt,
                   "moving between ir_stmts not supported");

  if (op->stmt) {
    ir_detach_op(irb, op);
  }

  ir_attach_op(irb, op, move_before->stmt, move_before->pred, move_before);
}

void ir_move_after_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                        struct ir_stmt *move_after) {
  invariant_assert(stmt->id != move_after->id,
                   "trying to move stmt after itself!");
  invariant_assert(stmt->basicblock == NULL ||
                       stmt->basicblock == move_after->basicblock,
                   "moving between ir_stmts not supported");

  if (stmt->basicblock) {
    ir_detach_stmt(irb, stmt);
  }

  ir_attach_stmt(irb, stmt, move_after->basicblock, move_after,
                 move_after->succ);
}

void ir_move_before_stmt(struct ir_func *irb, struct ir_stmt *stmt,
                         struct ir_stmt *move_before) {
  invariant_assert(stmt->id != move_before->id,
                   "trying to move stmt before itself!");
  invariant_assert(stmt->basicblock == NULL ||
                       stmt->basicblock == move_before->basicblock,
                   "moving between ir_stmts not supported");

  if (stmt->basicblock) {
    ir_detach_stmt(irb, stmt);
  }

  ir_attach_stmt(irb, stmt, move_before->basicblock, move_before->pred,
                 move_before);
}

void ir_insert_basicblock_chain(struct ir_func *irb,
                                struct ir_basicblock *chain,
                                struct ir_basicblock *insert_after,
                                struct ir_basicblock *first_succ) {
  invariant_assert(chain->id != insert_after->id,
                   "trying to move basicblock after itself!");

  if (chain->func) {
    ir_detach_basicblock(irb, chain, DETACH_IR_BASICBLOCK_FLAG_NONE);
  }

  chain->func = irb;

  struct ir_basicblock *last = chain;
  while (true) {
    irb->basicblock_count++;
    last->func = irb;

    if (last->succ) {
      last = last->succ;
    } else {
      break;
    }
  }

  invariant_assert(!chain->pred, "chain had pred");
  invariant_assert(!first_succ || !first_succ->pred, "first_succ had pred");
  // chain->pred = insert_after;

  ir_attach_basicblock_pred(irb, chain, insert_after);
  ir_attach_basicblock_succ(irb, last, first_succ);

  // // fix up the start link
  // if (insert_after) {
  //   insert_after->succ = chain;
  //   chain->pred = insert_after;
  // } else {
  //   irb->first = chain;
  //   chain->pred = NULL;
  //   BUG("inserting chain with no insert_after - correct logic is probably to
  //   "
  //       "insert at start of func, but is this intentional?");
  // }

  // // fix up the end link
  // if (first_succ) {
  //   first_succ->pred = last;
  //   last->succ = first_succ;
  // } else {
  //   last->succ = NULL;
  //   irb->last = last;
  // }
}

void ir_move_after_basicblock(struct ir_func *irb,
                              struct ir_basicblock *basicblock,
                              struct ir_basicblock *move_after) {
  invariant_assert(basicblock->id != move_after->id,
                   "trying to move basicblock after itself!");

  if (basicblock->func) {
    ir_detach_basicblock(irb, basicblock, DETACH_IR_BASICBLOCK_FLAG_NONE);
  }

  ir_attach_basicblock(irb, basicblock, move_after, move_after->succ);
}

void ir_move_before_basicblock(struct ir_func *irb,
                               struct ir_basicblock *basicblock,
                               struct ir_basicblock *move_before) {
  invariant_assert(basicblock->id != move_before->id,
                   "trying to move basicblock before itself!");

  if (basicblock->func) {
    ir_detach_basicblock(irb, basicblock, DETACH_IR_BASICBLOCK_FLAG_NONE);
  }

  ir_attach_basicblock(irb, basicblock, move_before->pred, move_before);
}

struct ir_op *ir_replace_op(UNUSED struct ir_func *irb, struct ir_op *op,
                            enum ir_op_ty ty, struct ir_var_ty var_ty) {
  DEBUG_ASSERT(op, "invalid replacement point!");
  DEBUG_ASSERT(op->id != DETACHED_OP, "op is detached");

  op->ty = ty;
  op->var_ty = var_ty;

  return op;
}

struct ir_op *ir_insert_after_op(struct ir_func *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_var_ty var_ty) {
  DEBUG_ASSERT(insert_after, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  ir_initialise_op(op, irb->next_op_id++, ty, var_ty, NO_REG, NULL);

  ir_move_after_op(irb, op, insert_after);

  return op;
}

struct ir_op *ir_insert_before_op(struct ir_func *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_var_ty var_ty) {
  DEBUG_ASSERT(insert_before, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  ir_initialise_op(op, irb->next_op_id++, ty, var_ty, NO_REG, NULL);

  ir_move_before_op(irb, op, insert_before);

  return op;
}

struct ir_op *ir_append_op(struct ir_func *irb, struct ir_stmt *stmt,
                           enum ir_op_ty ty, struct ir_var_ty var_ty) {
  struct ir_op *op = ir_alloc_op(irb, stmt);

  op->ty = ty;
  op->var_ty = var_ty;

  return op;
}

struct ir_stmt *ir_insert_after_stmt(struct ir_func *irb,
                                     struct ir_stmt *insert_after) {
  DEBUG_ASSERT(insert_after, "invalid insertion point!");

  struct ir_stmt *stmt = arena_alloc(irb->arena, sizeof(*stmt));

  ir_initialise_stmt(stmt, irb->next_stmt_id++);

  ir_move_after_stmt(irb, stmt, insert_after);

  return stmt;
}

struct ir_stmt *ir_insert_before_stmt(struct ir_func *irb,
                                      struct ir_stmt *insert_before) {
  DEBUG_ASSERT(insert_before, "invalid insertion point!");

  struct ir_stmt *stmt = arena_alloc(irb->arena, sizeof(*stmt));

  ir_initialise_stmt(stmt, irb->next_stmt_id++);

  ir_move_before_stmt(irb, stmt, insert_before);

  return stmt;
}

void ir_initialise_basicblock(struct ir_basicblock *basicblock, size_t id) {
  *basicblock = (struct ir_basicblock){
      .id = id,
  };
}

struct ir_op *ir_insert_phi(struct ir_func *irb,
                            struct ir_basicblock *basicblock,
                            struct ir_var_ty var_ty) {
  struct ir_stmt *stmt = basicblock->first;
  if (!stmt) {
    stmt = ir_alloc_stmt(irb, basicblock);
  } else if (!(stmt->flags & IR_STMT_FLAG_PHI)) {
    stmt = ir_insert_before_stmt(irb, stmt);
  }

  stmt->flags |= IR_STMT_FLAG_PHI;

  struct ir_op *op = stmt->first;

  struct ir_op *phi;
  if (op) {
    phi = ir_insert_before_op(irb, op, IR_OP_TY_PHI, var_ty);
  } else {
    phi = ir_alloc_op(irb, stmt);
    phi->ty = IR_OP_TY_PHI;
    phi->var_ty = var_ty;
  }

  phi->phi = (struct ir_op_phi){.num_values = 0, .values = NULL};

  return phi;
}

struct ir_basicblock *
ir_insert_before_basicblock(struct ir_func *irb,
                            struct ir_basicblock *insert_before) {
  DEBUG_ASSERT(insert_before, "invalid insertion point!");

  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  ir_initialise_basicblock(basicblock, irb->next_basicblock_id++);

  ir_move_before_basicblock(irb, basicblock, insert_before);

  return basicblock;
}

struct ir_basicblock *
ir_insert_after_basicblock(struct ir_func *irb,
                           struct ir_basicblock *insert_after) {
  DEBUG_ASSERT(insert_after, "invalid insertion point!");

  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  ir_initialise_basicblock(basicblock, irb->next_basicblock_id++);

  ir_move_after_basicblock(irb, basicblock, insert_after);

  return basicblock;
}

void ir_swap_ops_in_place(struct ir_func *irb, struct ir_op *left,
                          struct ir_op *right) {
  // WARN: known buggy!

  DEBUG_ASSERT(left->succ == right && right->pred == left,
               "can only swap in place ops that are adjacent");

  struct ir_op *tmp = arena_alloc(irb->arena, sizeof(*tmp));
  *tmp = *left;
  *left = *right;
  *right = *tmp;
}

void ir_swap_ops(struct ir_func *irb, struct ir_op *left, struct ir_op *right) {
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
    ir_detach_op(irb, left);
    ir_move_after_op(irb, left, right);
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

  ir_detach_op(irb, left);
  ir_detach_op(irb, right);

  ir_attach_op(irb, left, right_stmt, right_pred, right_succ);
  ir_attach_op(irb, right, left_stmt, left_pred, left_succ);
}

struct ir_basicblock *ir_alloc_basicblock(struct ir_func *irb) {
  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  if (!irb->first) {
    irb->first = basicblock;
  }

  irb->basicblock_count++;

  *basicblock = (struct ir_basicblock){
      .id = irb->next_basicblock_id++,
      .func = irb,
      .pred = irb->last,
      .succ = NULL,
      .first = NULL,
      .last = NULL,
      .metadata = NULL,
      .comment = NULL,

      .cg_basicblock = NULL,
      .preds = NULL,
      .num_preds = 0,
  };

  if (irb->last) {
    irb->last->succ = basicblock;
  }

  irb->last = basicblock;

  return basicblock;
}

struct ir_stmt *ir_alloc_stmt(struct ir_func *irb,
                              struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = arena_alloc(irb->arena, sizeof(*stmt));

  if (!basicblock->first) {
    basicblock->first = stmt;
  }

  irb->stmt_count++;

  stmt->id = irb->next_stmt_id++;
  stmt->basicblock = basicblock;
  stmt->pred = basicblock->last;
  stmt->flags = IR_STMT_FLAG_NONE;
  stmt->succ = NULL;
  stmt->first = NULL;
  stmt->last = NULL;
  stmt->comment = NULL;

  if (basicblock->last) {
    basicblock->last->succ = stmt;
  }

  basicblock->last = stmt;

  return stmt;
}

// TODO: this should call `initialise_ir_op`
struct ir_op *ir_alloc_op(struct ir_func *irb, struct ir_stmt *stmt) {
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

struct ir_op *ir_alloc_contained_op(struct ir_func *irb, struct ir_op *op,
                                    struct ir_op *consumer) {
  struct ir_op *contained =
      ir_insert_before_op(irb, consumer, op->ty, op->var_ty);

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

struct ir_op *ir_alloc_fixed_reg_dest_op(struct ir_func *irb, struct ir_op **op,
                                         struct ir_op *consumer,
                                         struct ir_reg reg) {
  struct ir_op *mov =
      ir_insert_before_op(irb, consumer, IR_OP_TY_MOV, (*op)->var_ty);

  mov->flags |= IR_OP_FLAG_FIXED_REG;
  mov->reg = reg;
  mov->mov = (struct ir_op_mov){.value = *op};

  *op = mov;

  return mov;
}

struct ir_op *ir_alloc_fixed_reg_source_op(struct ir_func *irb,
                                           struct ir_op *producer,
                                           struct ir_reg reg) {
  struct ir_op *mov =
      ir_insert_before_op(irb, producer, producer->ty, producer->var_ty);

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

void ir_mk_zero_constant(struct ir_unit *iru, struct ir_op *op,
                         struct ir_var_ty *var_ty) {
  switch (var_ty->ty) {
  case IR_VAR_TY_TY_PRIMITIVE:
    if (ir_primitive_ty_is_fp(var_ty->primitive)) {
      ir_mk_floating_zero_constant(iru, op, var_ty->primitive);
    } else {
      ir_mk_integral_constant(iru, op, var_ty->primitive, 0);
    }
    break;
  case IR_VAR_TY_TY_POINTER:
    ir_mk_pointer_constant(iru, op, 0);
    break;
  default:
    BUG("unsupported for func");
  }
}

void ir_mk_floating_zero_constant(UNUSED_ARG(struct ir_unit *iru),
                                  struct ir_op *op,
                                  enum ir_var_primitive_ty ty) {
  DEBUG_ASSERT(ir_primitive_ty_is_fp(ty), "expected fp ty");

  op->ty = IR_OP_TY_CNST;
  op->var_ty =
      (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE, .primitive = ty};
  op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_FLT, .int_value = 0};
}

void ir_mk_integral_constant(UNUSED_ARG(struct ir_unit *iru), struct ir_op *op,
                             enum ir_var_primitive_ty ty,
                             unsigned long long value) {
  DEBUG_ASSERT(ir_primitive_ty_is_integral(ty), "expected integral ty");

  op->ty = IR_OP_TY_CNST;
  op->var_ty =
      (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE, .primitive = ty};
  op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = value};
}

void ir_mk_pointer_constant(struct ir_unit *iru, struct ir_op *op,
                            unsigned long long value) {
  op->ty = IR_OP_TY_CNST;
  op->var_ty = ir_var_ty_for_pointer_size(iru);
  op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = value};
}

struct ir_op *ir_build_addr(struct ir_func *irb, struct ir_op *op) {
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
      ir_insert_before_op(irb, op, IR_OP_TY_ADDR, IR_VAR_TY_POINTER);
  res->addr = addr;
  return res;
}
}

struct ir_var_ty ir_var_ty_make_array(struct ir_unit *iru,
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

struct ir_var_ty ir_var_ty_for_pointer_size(struct ir_unit *iru) {
  // TODO: again, similar to parser:
  // either we need a pointer-sized int type or for `ir_func` to know the
  // native integer size
  return (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE,
                            .primitive = ir_var_ty_pointer_primitive_ty(iru)};
}

struct ir_op *alloc_integral_constant(struct ir_func *irb, struct ir_stmt *stmt,
                                      enum ir_var_primitive_ty primitive,
                                      unsigned long long value);

bool ir_valid_basicblock(struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;
  while (stmt) {
    struct ir_op *op = stmt->first;
    while (op) {
      if (ir_op_is_branch(op->ty)) {
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

void ir_add_pred_to_basicblock(struct ir_func *irb,
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

void ir_make_basicblock_split(struct ir_func *irb,
                              struct ir_basicblock *basicblock,
                              struct ir_basicblock *true_target,
                              struct ir_basicblock *false_target) {
  ir_remove_basicblock_successors(basicblock);

  basicblock->ty = IR_BASICBLOCK_TY_SPLIT;
  basicblock->split = (struct ir_basicblock_split){
      .true_target = true_target, .false_target = false_target};

  ir_add_pred_to_basicblock(irb, true_target, basicblock);
  ir_add_pred_to_basicblock(irb, false_target, basicblock);
}

void ir_make_basicblock_merge(struct ir_func *irb,
                              struct ir_basicblock *basicblock,
                              struct ir_basicblock *target) {
  ir_remove_basicblock_successors(basicblock);

  basicblock->ty = IR_BASICBLOCK_TY_MERGE;
  basicblock->merge = (struct ir_basicblock_merge){.target = target};

  ir_add_pred_to_basicblock(irb, target, basicblock);
}

void ir_make_basicblock_switch(struct ir_func *irb,
                               struct ir_basicblock *basicblock,
                               size_t num_cases, struct ir_split_case *cases,
                               struct ir_basicblock *default_target) {
  ir_remove_basicblock_successors(basicblock);

  basicblock->ty = IR_BASICBLOCK_TY_SWITCH;
  basicblock->switch_case = (struct ir_basicblock_switch){
      .cases = arena_alloc(irb->arena,
                           sizeof(*basicblock->switch_case.cases) * num_cases),
      .num_cases = num_cases,
      .default_target = default_target};

  memcpy(basicblock->switch_case.cases, cases,
         sizeof(*basicblock->switch_case.cases) * num_cases);

  for (size_t i = 0; i < num_cases; i++) {
    ir_add_pred_to_basicblock(irb, cases[i].target, basicblock);
  }

  ir_add_pred_to_basicblock(irb, default_target, basicblock);
}

struct ir_basicblock *
ir_insert_basicblocks_after_op(struct ir_func *irb, struct ir_op *insert_after,
                               struct ir_basicblock *first) {
  struct ir_basicblock *orig_bb = insert_after->stmt->basicblock;

  struct ir_basicblock *end_bb = ir_insert_after_basicblock(irb, orig_bb);
  struct ir_stmt *end_stmt = ir_alloc_stmt(irb, end_bb);

  struct ir_stmt *after = insert_after->stmt->succ;
  struct ir_stmt *last = insert_after->stmt->basicblock->last;

  // now move all later instructions to the end bb
  // first break up the stmt we are in
  end_stmt->first = insert_after->succ;
  if (!end_stmt->first) {
    end_stmt->last = NULL;
  }

  if (insert_after->succ) {
    insert_after->succ->pred = NULL;
  }

  insert_after->stmt->last = insert_after;
  insert_after->stmt->succ = NULL;
  insert_after->succ = NULL;

  orig_bb->last = insert_after->stmt;

  end_stmt->succ = after;
  if (after) {
    after->pred = end_stmt;
    end_bb->last = last;
  }

  struct ir_stmt *stmt = end_bb->first;
  struct ir_op *stmt_op = stmt->first;

  stmt->first = stmt_op;
  while (stmt_op) {
    stmt_op->stmt = stmt;
    stmt->last = stmt_op;
    stmt_op = stmt_op->succ;
  }

  while (stmt) {
    stmt->basicblock = end_bb;
    stmt = stmt->succ;
  }

  // forward block end
  end_bb->ty = orig_bb->ty;
  if (orig_bb->ty == IR_BASICBLOCK_TY_SPLIT) {
    ir_make_basicblock_split(irb, end_bb, orig_bb->split.true_target,
                             orig_bb->split.false_target);
  } else if (orig_bb->ty == IR_BASICBLOCK_TY_MERGE) {
    ir_make_basicblock_merge(irb, end_bb, orig_bb->merge.target);
  } else if (orig_bb->ty == IR_BASICBLOCK_TY_SWITCH) {
    TODO("inline switch");
  }

  ir_remove_basicblock_successors(orig_bb);

  struct ir_stmt *br_stmt = ir_alloc_stmt(irb, orig_bb);
  struct ir_op *br = ir_alloc_op(irb, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_VAR_TY_NONE;

  end_bb->pred = NULL;
  ir_insert_basicblock_chain(irb, first, orig_bb, end_bb);

  ir_make_basicblock_merge(irb, orig_bb, first);

  return end_bb;
}

struct ir_glb *ir_add_well_known_global(struct ir_unit *iru,
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
    params[2] = ir_var_ty_for_pointer_size(iru);

    struct ir_var_ty var_ty = {.ty = IR_VAR_TY_TY_FUNC,
                               .func = {.ret_ty = ptr,
                                        .num_params = num_params,
                                        .params = params,
                                        .flags = IR_VAR_FUNC_TY_FLAG_NONE}};

    struct ir_glb *memmove = ir_add_global(iru, IR_GLB_TY_FUNC, &var_ty,
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
    params[2] = ir_var_ty_for_pointer_size(iru);

    struct ir_var_ty var_ty = {.ty = IR_VAR_TY_TY_FUNC,
                               .func = {.ret_ty = ptr,
                                        .num_params = num_params,
                                        .params = params,
                                        .flags = IR_VAR_FUNC_TY_FLAG_NONE}};

    struct ir_glb *memcpy = ir_add_global(iru, IR_GLB_TY_FUNC, &var_ty,
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
    params[2] = ir_var_ty_for_pointer_size(iru);

    struct ir_var_ty var_ty = {.ty = IR_VAR_TY_TY_FUNC,
                               .func = {.ret_ty = ptr,
                                        .num_params = num_params,
                                        .params = params,
                                        .flags = IR_VAR_FUNC_TY_FLAG_NONE}};

    struct ir_glb *memset = ir_add_global(iru, IR_GLB_TY_FUNC, &var_ty,
                                          IR_GLB_DEF_TY_UNDEFINED, "memset");

    iru->well_known_glbs.memset = memset;
    return memset;
  }
  }
}

struct ir_glb *ir_add_global(struct ir_unit *iru, enum ir_glb_ty ty,
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

struct ir_lcl *ir_add_local(struct ir_func *irb,
                            const struct ir_var_ty *var_ty) {
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

void ir_detach_local(struct ir_func *irb, struct ir_lcl *lcl) {
  if (lcl->id == DETACHED_LCL) {
    return;
  }

  switch (lcl->alloc_ty) {
  case IR_LCL_ALLOC_TY_NONE:
    break;
  case IR_LCL_ALLOC_TY_NORMAL:
  case IR_LCL_ALLOC_TY_FIXED:
    // possible to detach normal alloc but painful as it involves removing a
    // "hole" possibly in the middle of the local block
    BUG("can't detach allocated lcl");
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

bool ir_var_ty_is_aggregate(const struct ir_var_ty *var_ty) {
  return var_ty->ty == IR_VAR_TY_TY_STRUCT || var_ty->ty == IR_VAR_TY_TY_UNION;
}

bool ir_var_ty_is_primitive(const struct ir_var_ty *var_ty,
                            enum ir_var_primitive_ty primitive) {
  return var_ty->ty == IR_VAR_TY_TY_PRIMITIVE && var_ty->primitive == primitive;
}

bool ir_primitive_ty_is_integral(enum ir_var_primitive_ty ty) {
  switch (ty) {
  case IR_VAR_PRIMITIVE_TY_I1:
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
  case IR_VAR_PRIMITIVE_TY_I64:
  case IR_VAR_PRIMITIVE_TY_I128:
    return true;
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    return false;
  }
}

bool ir_primitive_ty_is_fp(enum ir_var_primitive_ty ty) {
  switch (ty) {
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    return true;
  case IR_VAR_PRIMITIVE_TY_I1:
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
  case IR_VAR_PRIMITIVE_TY_I64:
  case IR_VAR_PRIMITIVE_TY_I128:
    return false;
  }
}

bool ir_var_ty_is_integral(const struct ir_var_ty *var_ty) {
  if (var_ty->ty == IR_VAR_TY_TY_POINTER) {
    return true;
  }

  if (var_ty->ty != IR_VAR_TY_TY_PRIMITIVE) {
    return false;
  }

  return ir_primitive_ty_is_integral(var_ty->primitive);
}

bool ir_var_ty_is_fp(const struct ir_var_ty *var_ty) {
  if (var_ty->ty != IR_VAR_TY_TY_PRIMITIVE) {
    return false;
  }

  return ir_primitive_ty_is_fp(var_ty->primitive);
}

static void flatten_add_fields(struct ir_unit *iru, const struct ir_var_ty *ty,
                               struct vector *fields, size_t *offset) {
  switch (ty->ty) {
  case IR_VAR_TY_TY_ARRAY: {
    for (size_t i = 0; i < ty->array.num_elements; i++) {
      flatten_add_fields(iru, ty->array.underlying, fields, offset);
    }
    break;
  }

  case IR_VAR_TY_TY_STRUCT: {
    struct ir_var_ty_info info = ir_var_ty_info(iru, ty);

    for (size_t i = 0; i < info.num_fields; i++) {
      size_t field_offset = *offset + info.offsets[i];
      flatten_add_fields(iru, &ty->aggregate.fields[i], fields, &field_offset);
    }

    *offset += info.size;
    break;
  }

  case IR_VAR_TY_TY_UNION: {
    struct ir_var_ty_info info = ir_var_ty_info(iru, ty);

    for (size_t i = 0; i < info.num_fields; i++) {
      size_t field_offset = *offset;
      flatten_add_fields(iru, &ty->aggregate.fields[i], fields, &field_offset);
    }

    *offset += info.size;
    break;
  }
  default: {
    struct ir_field_info info = {.offset = *offset, .var_ty = *ty};
    *offset += ir_var_ty_info(iru, ty).size;
    vector_push_back(fields, &info);
  }
  }
}

struct ir_var_ty_flattened ir_var_ty_info_flat(struct ir_unit *iru,
                                               const struct ir_var_ty *ty) {
  // TODO: this isn't efficient because it generates a huge array of offsets
  // for arrays

  struct vector *fields =
      vector_create_in_arena(sizeof(struct ir_field_info), iru->arena);
  size_t offset = 0;
  flatten_add_fields(iru, ty, fields, &offset);

  return (struct ir_var_ty_flattened){.fields = vector_head(fields),
                                      .num_fields = vector_length(fields)};
}

struct ir_var_ty_info ir_var_ty_info(struct ir_unit *iru,
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
    case IR_VAR_PRIMITIVE_TY_I1:
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
    case IR_VAR_PRIMITIVE_TY_I128:
      return (struct ir_var_ty_info){.size = 16, .alignment = 16};
    }
  case IR_VAR_TY_TY_ARRAY: {
    struct ir_var_ty_info element_info =
        ir_var_ty_info(iru, ty->array.underlying);
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
      struct ir_var_ty_info info = ir_var_ty_info(iru, field);
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
      struct ir_var_ty_info info = ir_var_ty_info(iru, field);
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

struct ir_op *ir_spill_op(struct ir_func *irb, struct ir_op *op) {
  DEBUG_ASSERT(!(op->flags & IR_OP_FLAG_FIXED_REG),
               "spilling fixed reg illegal (op %zu)", op->id);

  debug("spilling %zu\n", op->id);

  if (!op->lcl) {
    op->lcl = ir_add_local(irb, &op->var_ty);
  }

  if (op->flags & IR_OP_FLAG_SPILLED) {
    return NULL;
  }

  if (op->ty != IR_OP_TY_UNDF && !(op->flags & IR_OP_FLAG_SPILLED)) {
    op->flags |= IR_OP_FLAG_SPILLED;

    struct ir_op *store;
    if (op->ty == IR_OP_TY_PHI ||
        (op->ty == IR_OP_TY_MOV && (op->flags & IR_OP_FLAG_PARAM))) {
      struct ir_stmt *succ = op->stmt->succ;
      DEBUG_ASSERT(!succ || !succ->first || succ->first->ty != IR_OP_TY_PHI,
                   "expected all phi to be in same stmt");

      if (succ->first) {
        store = ir_insert_before_op(irb, succ->first, IR_OP_TY_STORE,
                                    IR_VAR_TY_NONE);
      } else {
        store = ir_alloc_op(irb, succ);
        store->ty = IR_OP_TY_STORE;
        store->var_ty = IR_VAR_TY_NONE;
      }
    } else {
      store = ir_insert_after_op(irb, op, IR_OP_TY_STORE, IR_VAR_TY_NONE);
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

static void build_op_uses_callback(struct ir_op **op, enum ir_op_use_ty use_ty,
                                   void *cb_metadata) {
  struct build_op_uses_callback_data *data = cb_metadata;

  struct ir_op_use use = {.ty = use_ty, .op = op, .consumer = data->op};

  DEBUG_ASSERT((*op)->id != DETACHED_OP, "detached op consumed by %zu",
               use.consumer->id);

  vector_push_back(data->use_data[(*op)->id].uses, &use);
}

struct ir_op_use_map ir_build_op_uses_map(struct ir_func *func) {
  ir_rebuild_func_ids(func);

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

      // because walk_op_uses can be out of order we need to create the
      // vectors in advance
      data.use_data[i++] = (struct op_use_data){
          .op = op, .uses = vector_create(sizeof(struct ir_op_use))};
    }
  }

  {
    size_t i = 0;
    struct ir_lcl *lcl = func->first_lcl;
    while (lcl) {
      DEBUG_ASSERT(lcl->id == i, "ids unordered");

      // because walk_op_uses can be out of order we need to create the
      // vectors in advance
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

        ir_walk_op_uses(op, build_op_uses_callback, &data);

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

size_t ir_unique_idx_for_reg(struct ir_reg reg) {
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
  return (struct ir_func_iter){
      .func = func,
      .flags = flags,
      .op = NULL,
  };
}

static struct ir_op *get_first(struct ir_func *func) {
  struct ir_basicblock *basicblock = func->first;

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;

      if (op) {
        return op;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  return NULL;
}

static struct ir_op *get_next(struct ir_op *op) {
  if (op->succ) {
    return op->succ;
  }

  struct ir_stmt *stmt = op->stmt->succ;
  struct ir_basicblock *basicblock = op->stmt->basicblock;

  while (!stmt || !stmt->first) {
    if (!stmt) {
      basicblock = basicblock->succ;

      if (!basicblock) {
        return NULL;
      }

      stmt = basicblock->first;
    } else {
      stmt = stmt->succ;
    }
  }

  return stmt->first;
}

bool ir_func_iter_next(struct ir_func_iter *iter, struct ir_op **op) {
  if (!iter->op) {
    iter->op = get_first(iter->func);
  } else {
    DEBUG_ASSERT(iter->op->id != DETACHED_OP, "op detached during iter");
    iter->op = get_next(iter->op);
  }

  if (iter->op) {
    DEBUG_ASSERT(iter->op->id != DETACHED_OP, "op detached during iter");
  }

  *op = iter->op;
  return *op;
}

struct ir_basicblock_succ_iter
ir_basicblock_succ_iter(struct ir_basicblock *basicblock) {
  return (struct ir_basicblock_succ_iter){.basicblock = basicblock, .idx = 0};
}

bool ir_basicblock_succ_iter_next(struct ir_basicblock_succ_iter *iter,
                                  struct ir_basicblock **basicblock) {
  switch (iter->basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    return false;
  case IR_BASICBLOCK_TY_SPLIT:
    switch (iter->idx) {
    case 0:
      *basicblock = iter->basicblock->split.true_target;
      break;
    case 1:
      *basicblock = iter->basicblock->split.false_target;
      break;
    default:
      return false;
    }

    iter->idx++;
    return true;
  case IR_BASICBLOCK_TY_MERGE:
    if (iter->idx) {
      return false;
    }
    *basicblock = iter->basicblock->merge.target;
    iter->idx++;
    return true;
  case IR_BASICBLOCK_TY_SWITCH:
    if (!iter->idx) {
      *basicblock = iter->basicblock->switch_case.default_target;
      iter->idx++;
      return true;
    }

    if (iter->idx <= iter->basicblock->switch_case.num_cases) {
      *basicblock = iter->basicblock->switch_case.cases[iter->idx - 1].target;
      iter->idx++;
      return true;
    }
    return false;
  }
}

static struct ir_basicblock *intersect(struct ir_basicblock **idoms,
                                       struct ir_basicblock *left,
                                       struct ir_basicblock *right,
                                       size_t *rpo) {
  while (left != right) {
    while (rpo[left->id] > rpo[right->id]) {
      left = idoms[left->id];
    }

    while (rpo[right->id] > rpo[left->id]) {
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

struct ir_idoms {
  size_t num_idoms;
  struct ir_basicblock **idoms;
};

static struct ir_idoms compute_idoms(struct ir_func *func) {
  struct ir_basicblock **idoms = arena_alloc(
      func->arena, func->basicblock_count * sizeof(struct ir_basicblock *));

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

  DEBUG_ASSERT(count == func->basicblock_count,
               "fewer bbs found in traversal than expected");

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

  return (struct ir_idoms){.num_idoms = func->basicblock_count, .idoms = idoms};
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

  struct vector **dom_trees = arena_alloc(
      func->arena, func->basicblock_count * sizeof(struct vector *));
  struct vector **children = arena_alloc(
      func->arena, func->basicblock_count * sizeof(struct vector *));

  struct ir_idoms idoms = compute_idoms(func);

  for (size_t i = 0; i < func->basicblock_count; i++) {
    domf[i] =
        vector_create_in_arena(sizeof(struct ir_basicblock *), func->arena);
    children[i] =
        vector_create_in_arena(sizeof(struct ir_basicblock *), func->arena);
    dom_trees[i] =
        vector_create_in_arena(sizeof(struct ir_basicblock *), func->arena);
  }

  struct ir_basicblock *entry = func->first;

  struct ir_basicblock *basicblock = entry->succ;
  while (basicblock) {
    struct ir_basicblock *idom = idoms.idoms[basicblock->id];

    vector_push_back(dom_trees[idom->id], &basicblock);

    vector_push_back(children[idoms.idoms[basicblock->id]->id], &basicblock);

    basicblock = basicblock->succ;
  }

  compute_df_recursive(entry, idoms.idoms, domf, children);

  return (struct ir_dominance_frontier){.idoms = idoms.idoms,
                                        .idom_children = children,
                                        .domfs = domf,
                                        .dom_trees = dom_trees};
}

void ir_alloc_locals(struct ir_func *func) {
  struct ir_lcl *lcl = func->first_lcl;
  while (lcl) {
    if (lcl->alloc_ty != IR_LCL_ALLOC_TY_NONE) {
      lcl = lcl->succ;
      continue;
    }

    struct ir_var_ty_info ty_info = ir_var_ty_info(func->unit, &lcl->var_ty);

    size_t lcl_pad =
        (ty_info.alignment - (func->total_locals_size % ty_info.alignment)) %
        ty_info.alignment;

    size_t lcl_size = ty_info.size;

    func->total_locals_size += lcl_pad;

    lcl->alloc_ty = IR_LCL_ALLOC_TY_NORMAL;
    lcl->alloc = (struct ir_lcl_alloc){.offset = func->total_locals_size,
                                       .padding = lcl_pad,
                                       .size = lcl_size};

    func->total_locals_size += lcl_size;

    lcl = lcl->succ;
  }
}

void ir_simplify_phis(struct ir_func *func) {
  // FIXME: phi gen is shit, we should do it better
  bool improved = true;

// FIXME: super unimaginably ineffecieitn because it generates a new use map
// after _every_ change
loop:
  while (improved) {
    // first remove duplicate entries
    struct ir_basicblock *basicblock = func->first;
    while (basicblock) {
      struct ir_stmt *stmt = basicblock->first;
      if (stmt && stmt->flags & IR_STMT_FLAG_PHI) {
        struct ir_op *op = stmt->first;
        while (op) {
          struct vector *new_entries =
              vector_create_in_arena(sizeof(struct ir_phi_entry), func->arena);
          vector_ensure_capacity(new_entries, op->phi.num_values);

          struct ir_op_phi *phi = &op->phi;
          for (size_t i = 0; i < phi->num_values; i++) {
            struct ir_op *l = phi->values[i].value;

            size_t num_other = vector_length(new_entries);
            bool found = false;
            for (size_t j = 0; j < num_other; j++) {
              struct ir_phi_entry *other = vector_get(new_entries, j);

              if (other->value == l) {
                found = true;
                break;
              }
            }

            if (found) {
              continue;
            }

            vector_push_back(new_entries, &phi->values[i]);
          }

          phi->num_values = vector_length(new_entries);
          phi->values = vector_head(new_entries);

          op = op->succ;
        }
      }

      basicblock = basicblock->succ;
    }

    struct ir_op_use_map use_map = ir_build_op_uses_map(func);

    ir_clear_metadata(func);

    improved = false;
    basicblock = func->first;
    while (basicblock) {
      struct ir_stmt *stmt = basicblock->first;
      if (stmt && stmt->flags & IR_STMT_FLAG_PHI) {
        struct ir_op *op = stmt->first;
        while (op) {
          if (op->metadata) {
            // going to be detached
            op = op->succ;
            continue;
          }

          struct ir_op_phi *phi = &op->phi;

          // given we expect small phis, i think doing n^2 double loop is
          // actually most efficient

          bool all_ops_phis = phi->num_values > 0;
          for (size_t i = 0; i < phi->num_values; i++) {
            if (phi->values[i].value->ty != IR_OP_TY_PHI) {
              all_ops_phis = false;
              break;
            }
          }

          // something in this logic is broken
          if (false && all_ops_phis) {
            struct vector *entries = vector_create_in_arena(
                sizeof(struct ir_phi_entry), func->arena);

            for (size_t i = 0; i < phi->num_values; i++) {
              struct ir_op_phi sub = phi->values[i].value->phi;

              size_t num_entries = vector_length(entries);
              for (size_t j = 0; j < sub.num_values; j++) {
                struct ir_op *v = sub.values[j].value;

                bool found = false;
                for (size_t k = 0; k < num_entries; k++) {
                  struct ir_phi_entry *entry = vector_get(entries, k);
                  if (entry->value == v) {
                    found = true;
                    break;
                  }
                }

                if (!found) {
                  struct ir_phi_entry new_entry = {
                      .basicblock = phi->values[i].basicblock, .value = v};
                  vector_push_back(entries, &new_entry);
                }
              }
            }

            phi->num_values = vector_length(entries);
            phi->values = vector_head(entries);

            op = op->succ;
            improved = true;
            continue;
          } else if (phi->num_values == 1) {
            struct ir_op_usage usage = use_map.op_use_datas[op->id];

            for (size_t i = 0; i < usage.num_uses; i++) {
              *usage.uses[i].op = phi->values[0].value;
            }

            ir_detach_op(func, op);
            improved = true;
            goto loop;
          } else if (phi->num_values == 2) {
            struct ir_op *other = NULL;

            if (phi->values[0].value == op) {
              other = phi->values[1].value;
            } else if (phi->values[1].value == op) {
              other = phi->values[0].value;
            }

            if (other) {
              struct ir_op_usage usage = use_map.op_use_datas[op->id];

              for (size_t i = 0; i < usage.num_uses; i++) {
                *usage.uses[i].op = other;
              }

              ir_detach_op(func, op);
              improved = true;
              goto loop;
            }
          }

          op = op->succ;
        }
      }

      basicblock = basicblock->succ;
    }
  }
}

bool ir_basicblock_is_pred(struct ir_basicblock *basicblock,
                           struct ir_basicblock *pred) {
  switch (pred->ty) {
  case IR_BASICBLOCK_TY_RET:
    return false;
  case IR_BASICBLOCK_TY_SPLIT:
    return pred->split.true_target == basicblock ||
           pred->split.false_target == basicblock;
  case IR_BASICBLOCK_TY_MERGE:
    return pred->merge.target == basicblock;
  case IR_BASICBLOCK_TY_SWITCH:
    for (size_t i = 0; i < pred->switch_case.num_cases; i++) {
      if (pred->switch_case.cases[i].target == basicblock) {
        return true;
      }
    }

    return pred->switch_case.default_target == basicblock;
  }

  BUG("bad bb ty");
}
