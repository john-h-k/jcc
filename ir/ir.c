#include "ir.h"

#include "../vector.h"
#include "../alloc.h"
#include "../log.h"
#include "var_refs.h"

#include <sys/stat.h>

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
    return false;
  }
}

bool op_has_side_effects(const struct ir_op *op) {
  if (op->flags & IR_OP_FLAG_SIDE_EFFECTS) {
    return true;
  }

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    bug("unknown op ty");
  case IR_OP_TY_PHI:
  case IR_OP_TY_UNDF:
  case IR_OP_TY_CNST:
  case IR_OP_TY_CAST_OP:
  case IR_OP_TY_LOAD_GLB:
  case IR_OP_TY_LOAD_LCL:
  case IR_OP_TY_LOAD_ADDR:
  case IR_OP_TY_ADDR:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_UNARY_OP:
    return false;
  case IR_OP_TY_MOV:
  case IR_OP_TY_CALL:
  case IR_OP_TY_STORE_GLB:
  case IR_OP_TY_STORE_LCL:
  case IR_OP_TY_STORE_ADDR:
  case IR_OP_TY_BR_COND:
  case IR_OP_TY_BR_SWITCH:
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
    return true;
  case IR_OP_TY_CUSTOM:
    bug("not well defined for IR_OP_TY_CUSTOM");
  }
}

bool op_produces_value(const struct ir_op *op) {
  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    bug("unknown op ty");
  case IR_OP_TY_PHI:
  case IR_OP_TY_UNDF:
  case IR_OP_TY_MOV:
  case IR_OP_TY_CNST:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_UNARY_OP:
  case IR_OP_TY_CAST_OP:
  case IR_OP_TY_LOAD_GLB:
  case IR_OP_TY_LOAD_LCL:
  case IR_OP_TY_LOAD_ADDR:
  case IR_OP_TY_ADDR:
    return true;
  case IR_OP_TY_CALL:
    return op->call.func_ty.func.ret_ty->ty != IR_OP_VAR_TY_TY_NONE;
  case IR_OP_TY_STORE_GLB:
  case IR_OP_TY_STORE_LCL:
  case IR_OP_TY_STORE_ADDR:
  case IR_OP_TY_BR_COND:
  case IR_OP_TY_BR_SWITCH:
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
    return false;
  case IR_OP_TY_CUSTOM:
    bug("`op_produces_value` not well defined for IR_OP_TY_CUSTOM");
  }
}

bool op_is_branch(enum ir_op_ty ty) {
  switch (ty) {
  case IR_OP_TY_UNKNOWN:
    bug("unknown op ty");
  case IR_OP_TY_BR_COND:
  case IR_OP_TY_BR_SWITCH:
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
    return true;
  // calls are NOT branches, because while they do leave, they guarantee return
  case IR_OP_TY_CALL:
  case IR_OP_TY_UNDF:
  case IR_OP_TY_PHI:
  case IR_OP_TY_MOV:
  case IR_OP_TY_CNST:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_UNARY_OP:
  case IR_OP_TY_CAST_OP:
  case IR_OP_TY_STORE_GLB:
  case IR_OP_TY_LOAD_GLB:
  case IR_OP_TY_STORE_LCL:
  case IR_OP_TY_LOAD_LCL:
  case IR_OP_TY_STORE_ADDR:
  case IR_OP_TY_LOAD_ADDR:
  case IR_OP_TY_ADDR:
    return false;
  case IR_OP_TY_CUSTOM:
    bug("`op_produces_value` not well defined for IR_OP_TY_CUSTOM");
  }
}

void walk_br_cond(struct ir_op_br_cond *br_cond, walk_op_callback *cb,
                  void *cb_metadata) {
  cb(&br_cond->cond, cb_metadata);
}

void walk_store_lcl(struct ir_op_store_lcl *store_lcl, walk_op_callback *cb,
                    void *cb_metadata) {
  cb(&store_lcl->value, cb_metadata);
}

void walk_load_lcl(struct ir_op_load_lcl *load_lcl, walk_op_callback *cb,
                   void *cb_metadata) {
  UNUSED_ARG(load_lcl);
  UNUSED_ARG(cb);
  UNUSED_ARG(cb_metadata);
  // nada
}

void walk_cnst(struct ir_op_cnst *cnst, walk_op_callback *cb,
               void *cb_metadata) {
  UNUSED_ARG(cnst);
  UNUSED_ARG(cb);
  UNUSED_ARG(cb_metadata);
  // nada
}

void walk_binary_op(struct ir_op_binary_op *binary_op, walk_op_callback *cb,
                    void *cb_metadata) {
  walk_op(binary_op->lhs, cb, cb_metadata);
  walk_op(binary_op->rhs, cb, cb_metadata);
}

void walk_unary_op(struct ir_op_unary_op *unary_op, walk_op_callback *cb,
                   void *cb_metadata) {
  walk_op(unary_op->value, cb, cb_metadata);
}

void walk_cast_op(struct ir_op_cast_op *cast_op, walk_op_callback *cb,
                  void *cb_metadata) {
  walk_op(cast_op->value, cb, cb_metadata);
}

void walk_ret(struct ir_op_ret *ret, walk_op_callback *cb, void *cb_metadata) {
  if (ret->value) {
    walk_op(ret->value, cb, cb_metadata);
  }
}

void walk_op_uses(struct ir_op *op, walk_op_callback *cb, void *cb_metadata) {
  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    bug("unknown op!");
  case IR_OP_TY_CUSTOM:
  case IR_OP_TY_UNDF:
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
      cb(&op->phi.values[i], cb_metadata);
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
  case IR_OP_TY_STORE_ADDR:
    cb(&op->store_addr.addr, cb_metadata);
    cb(&op->store_addr.value, cb_metadata);
    break;
  case IR_OP_TY_STORE_GLB:
    cb(&op->store_glb.value, cb_metadata);
    break;
  case IR_OP_TY_LOAD_GLB:
    break;
  case IR_OP_TY_STORE_LCL:
    cb(&op->store_lcl.value, cb_metadata);
    break;
  case IR_OP_TY_LOAD_LCL:
    break;
  case IR_OP_TY_LOAD_ADDR:
    cb(&op->load_addr.addr, cb_metadata);
    break;
  case IR_OP_TY_ADDR:
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
    if (op->mov.value) {
      cb(&op->mov.value, cb_metadata);
    }
    break;
  case IR_OP_TY_RET:
    if (op->ret.value) {
      cb(&op->ret.value, cb_metadata);
    }
    break;
  }
}

void walk_op(struct ir_op *op, walk_op_callback *cb, void *cb_metadata) {
  cb(&op, cb_metadata);

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    bug("unknown op!");
  case IR_OP_TY_CUSTOM:
    todo("walk custom");
  case IR_OP_TY_CALL:
    todo("walk call");
  case IR_OP_TY_PHI:
    todo("walk phi");
  case IR_OP_TY_MOV:
    todo("walk mov");
  case IR_OP_TY_LOAD_GLB:
    todo("walk load glb");
  case IR_OP_TY_STORE_GLB:
    todo("walk store glb");
  case IR_OP_TY_LOAD_ADDR:
    todo("walk load addr");
  case IR_OP_TY_STORE_ADDR:
    todo("walk store addr");
  case IR_OP_TY_ADDR:
    todo("walk addr");
  case IR_OP_TY_BR_SWITCH:
    todo("walk br.switch");
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
  case IR_OP_TY_STORE_LCL:
    walk_store_lcl(&op->store_lcl, cb, cb_metadata);
    break;
  case IR_OP_TY_LOAD_LCL:
    walk_load_lcl(&op->load_lcl, cb, cb_metadata);
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

const struct ir_op_var_ty IR_OP_VAR_TY_NONE = {.ty = IR_OP_VAR_TY_TY_NONE};
const struct ir_op_var_ty IR_OP_VAR_TY_I8 = {
    .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I8};
const struct ir_op_var_ty IR_OP_VAR_TY_I16 = {
    .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I16};
const struct ir_op_var_ty IR_OP_VAR_TY_I32 = {
    .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I32};
const struct ir_op_var_ty IR_OP_VAR_TY_I64 = {
    .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I64};
const struct ir_op_var_ty IR_OP_VAR_TY_F32 = {
    .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_F32};
const struct ir_op_var_ty IR_OP_VAR_TY_F64 = {
    .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_F64};
const struct ir_op_var_ty IR_OP_VAR_TY_VARIADIC = {
    .ty = IR_OP_VAR_TY_TY_VARIADIC};

bool is_func_variadic(const struct ir_op_var_func_ty *ty) {
  return ty->flags & IR_OP_VAR_FUNC_TY_FLAG_VARIADIC;
}

void initialise_ir_op(struct ir_op *op, size_t id, enum ir_op_ty ty,
                      struct ir_op_var_ty var_ty, struct ir_reg reg,
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
  op->metadata = NULL;
  op->comment = NULL;
}

void detach_ir_basicblock(struct ir_func *irb,
                          struct ir_basicblock *basicblock) {
  invariant_assert(irb->basicblock_count,
                   "`detach_ir_basicblock` would underflow basicblock count "
                   "for `ir_builder`");

  invariant_assert(!basicblock->num_preds, "trying to detach BB with preds");

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

  basicblock->irb = NULL;
}

void detach_ir_stmt(struct ir_func *irb, struct ir_stmt *stmt) {
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

  if (stmt->succ) {
    stmt->succ->pred = stmt->pred;
  } else {
    stmt->basicblock->last = stmt->pred;
  }

  stmt->basicblock = NULL;
}

void detach_ir_op(struct ir_func *irb, struct ir_op *op) {
  invariant_assert(irb->op_count,
                   "`detach_ir_op` would underflow op count for `ir_builder`");
  invariant_assert(op->stmt, "can't detach `op` not attached to stmt");

  irb->op_count--;

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

void prune_basicblocks(struct ir_func *irb) {
  // skip first BB as it has an implicit predecessor
  struct ir_basicblock *basicblock = irb->first ? irb->first->succ : NULL;

  while (basicblock) {
    prune_stmts(irb, basicblock);

    // save succ before we detach
    struct ir_basicblock *succ = basicblock->succ;

    // remove if it has no preds (if it has preds, it is needed as a target)
    if (!basicblock->num_preds) {
      detach_ir_basicblock(irb, basicblock);
    }

    basicblock = succ;
  }

  // means bb->id < bb_count for all bbs
  rebuild_ids(irb);
}

void prune_stmts(struct ir_func *irb, struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;

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
  UNUSED_ARG(irb);
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
  UNUSED_ARG(irb);
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
  UNUSED_ARG(irb);
  invariant_assert(basicblock->id != move_after->id,
                   "trying to move basicblock after itself!");

  if (basicblock->irb) {
    detach_ir_basicblock(irb, basicblock);
  }

  attach_ir_basicblock(irb, basicblock, move_after, move_after->succ);
}

void move_before_ir_basicblock(struct ir_func *irb,
                               struct ir_basicblock *basicblock,
                               struct ir_basicblock *move_before) {
  UNUSED_ARG(irb);
  invariant_assert(basicblock->id != move_before->id,
                   "trying to move basicblock before itself!");

  if (basicblock->irb) {
    detach_ir_basicblock(irb, basicblock);
  }

  attach_ir_basicblock(irb, basicblock, move_before->pred, move_before);
}

struct ir_op *replace_ir_op(struct ir_func *irb, struct ir_op *op,
                            enum ir_op_ty ty, struct ir_op_var_ty var_ty) {
  UNUSED_ARG(irb);
  debug_assert(op, "invalid replacement point!");

  op->ty = ty;
  op->var_ty = var_ty;

  return op;
}

struct ir_op *insert_before_ir_op(struct ir_func *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_op_var_ty var_ty) {
  debug_assert(insert_before, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->next_op_id++, ty, var_ty, NO_REG, NULL);

  move_before_ir_op(irb, op, insert_before);

  return op;
}

void initialise_ir_basicblock(struct ir_basicblock *basicblock, size_t id) {
  basicblock->id = id;
  basicblock->irb = NULL;
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
                                 struct ir_op_var_ty var_ty) {
  debug_assert(insert_after, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->next_op_id++, ty, var_ty, NO_REG, NULL);

  move_after_ir_op(irb, op, insert_after);

  return op;
}

struct ir_basicblock *
insert_before_ir_basicblock(struct ir_func *irb,
                            struct ir_basicblock *insert_before) {
  debug_assert(insert_before, "invalid insertion point!");

  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  initialise_ir_basicblock(basicblock, irb->next_basicblock_id++);

  move_before_ir_basicblock(irb, basicblock, insert_before);

  return basicblock;
}

struct ir_basicblock *
insert_after_ir_basicblock(struct ir_func *irb,
                           struct ir_basicblock *insert_after) {
  debug_assert(insert_after, "invalid insertion point!");

  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  initialise_ir_basicblock(basicblock, irb->next_basicblock_id++);

  move_after_ir_basicblock(irb, basicblock, insert_after);

  return basicblock;
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
  basicblock->irb = irb;
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
    todo("unsupported type for contained op");
  }

  return contained;
}

void make_integral_constant(struct ir_unit *iru, struct ir_op *op,
                            enum ir_op_var_primitive_ty ty,
                            unsigned long long value) {
  UNUSED_ARG(iru);

  op->ty = IR_OP_TY_CNST;
  op->var_ty =
      (struct ir_op_var_ty){.ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = ty};
  op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = value};
}

void make_pointer_constant(struct ir_unit *iru, struct ir_op *op,
                           unsigned long long value) {
  op->ty = IR_OP_TY_CNST;
  op->var_ty = var_ty_for_pointer_size(iru);
  op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = value};
}

struct ir_op_var_ty var_ty_get_underlying(const struct ir_op_var_ty *var_ty) {
  switch (var_ty->ty) {
  case IR_OP_VAR_TY_TY_POINTER:
    return *var_ty->pointer.underlying;
  case IR_OP_VAR_TY_TY_ARRAY:
    return *var_ty->array.underlying;
  default:
    bug("non pointer/array passed");
  }
}

struct ir_op_var_ty var_ty_make_pointer(struct ir_unit *iru,
                                        const struct ir_op_var_ty *underlying) {
  struct ir_op_var_ty *copied = arena_alloc(iru->arena, sizeof(*copied));

  *copied = *underlying;

  struct ir_op_var_ty var_ty;
  var_ty.ty = IR_OP_VAR_TY_TY_POINTER;
  var_ty.pointer = (struct ir_op_var_pointer_ty){.underlying = copied};

  return var_ty;
}

struct ir_op_var_ty var_ty_make_array(struct ir_unit *iru,
                                      const struct ir_op_var_ty *underlying,
                                      size_t num_elements) {
  struct ir_op_var_ty *copied = arena_alloc(iru->arena, sizeof(*copied));

  *copied = *underlying;

  struct ir_op_var_ty var_ty;
  var_ty.ty = IR_OP_VAR_TY_TY_ARRAY;
  var_ty.array = (struct ir_op_var_array_ty){.num_elements = num_elements,
                                             .underlying = copied};

  return var_ty;
}

struct ir_op_var_ty var_ty_for_pointer_size(struct ir_unit *iru) {
  UNUSED_ARG(iru);

  // TODO: again, similar to parser:
  // either we need a pointer-sized int type or for `ir_func` to know the
  // native integer size
  return (struct ir_op_var_ty){.ty = IR_OP_VAR_TY_TY_PRIMITIVE,
                               .primitive = IR_OP_VAR_PRIMITIVE_TY_I64};
}

struct ir_op *alloc_integral_constant(struct ir_func *irb, struct ir_stmt *stmt,
                                      enum ir_op_var_primitive_ty primitive,
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

struct ir_glb *add_global(struct ir_unit *iru, enum ir_glb_ty ty,
                          const struct ir_op_var_ty *var_ty,
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
struct ir_lcl *add_local(struct ir_func *irb, struct ir_op_var_ty *var_ty) {
  struct ir_lcl *lcl = arena_alloc(irb->arena, sizeof(*lcl));
  lcl->id = irb->num_locals++;

  struct ir_var_ty_info ty_info = var_ty_info(irb->unit, var_ty);

  size_t lcl_pad =
      (ty_info.alignment - (irb->total_locals_size % ty_info.alignment)) %
      ty_info.alignment;
  size_t lcl_size = ty_info.size;

  irb->total_locals_size += lcl_pad;

  lcl->var_ty = *var_ty;
  lcl->offset = irb->total_locals_size;
  lcl->store = NULL;
  lcl->pred = irb->last_local;
  lcl->succ = NULL;
  lcl->metadata = NULL;

  irb->total_locals_size += lcl_size;

  if (!irb->first_local) {
    irb->first_local = lcl;
  }

  if (irb->last_local) {
    irb->last_local->succ = lcl;
  }

  irb->last_local = lcl;

  return lcl;
}

bool var_ty_is_aggregate(const struct ir_op_var_ty *var_ty) {
  return var_ty->ty == IR_OP_VAR_TY_TY_STRUCT ||
         var_ty->ty == IR_OP_VAR_TY_TY_UNION;
}

bool var_ty_is_primitive(const struct ir_op_var_ty *var_ty,
                         enum ir_op_var_primitive_ty primitive) {
  return var_ty->ty == IR_OP_VAR_TY_TY_PRIMITIVE &&
         var_ty->primitive == primitive;
}

bool var_ty_is_integral(const struct ir_op_var_ty *var_ty) {
  if (var_ty->ty != IR_OP_VAR_TY_TY_PRIMITIVE) {
    return false;
  }

  switch (var_ty->primitive) {
  case IR_OP_VAR_PRIMITIVE_TY_I8:
  case IR_OP_VAR_PRIMITIVE_TY_I16:
  case IR_OP_VAR_PRIMITIVE_TY_I32:
  case IR_OP_VAR_PRIMITIVE_TY_I64:
    return true;
  case IR_OP_VAR_PRIMITIVE_TY_F16:
  case IR_OP_VAR_PRIMITIVE_TY_F32:
  case IR_OP_VAR_PRIMITIVE_TY_F64:
    return false;
  }
}

bool var_ty_is_fp(const struct ir_op_var_ty *var_ty) {
  if (var_ty->ty != IR_OP_VAR_TY_TY_PRIMITIVE) {
    return false;
  }

  switch (var_ty->primitive) {
  case IR_OP_VAR_PRIMITIVE_TY_I8:
  case IR_OP_VAR_PRIMITIVE_TY_I16:
  case IR_OP_VAR_PRIMITIVE_TY_I32:
  case IR_OP_VAR_PRIMITIVE_TY_I64:
    return false;
  case IR_OP_VAR_PRIMITIVE_TY_F16:
  case IR_OP_VAR_PRIMITIVE_TY_F32:
  case IR_OP_VAR_PRIMITIVE_TY_F64:
    return true;
  }
}

struct ir_var_ty_info var_ty_info(struct ir_unit *iru,
                                  const struct ir_op_var_ty *ty) {
  // FIXME: pointer size!
  UNUSED_ARG(iru);

  switch (ty->ty) {
  case IR_OP_VAR_TY_TY_NONE:
    bug("IR_OP_VAR_TY_TY_NONE has no size");
  case IR_OP_VAR_TY_TY_VARIADIC:
    bug("IR_OP_VAR_TY_TY_VARIADIC has no size");
  case IR_OP_VAR_TY_TY_FUNC:
  case IR_OP_VAR_TY_TY_POINTER:
    return (struct ir_var_ty_info){.size = 8, .alignment = 8};
  case IR_OP_VAR_TY_TY_PRIMITIVE:
    switch (ty->primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      return (struct ir_var_ty_info){.size = 1, .alignment = 1};
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      return (struct ir_var_ty_info){.size = 2, .alignment = 2};
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      return (struct ir_var_ty_info){.size = 4, .alignment = 4};
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      return (struct ir_var_ty_info){.size = 8, .alignment = 8};
    case IR_OP_VAR_PRIMITIVE_TY_F16:
      return (struct ir_var_ty_info){.size = 2, .alignment = 2};
    case IR_OP_VAR_PRIMITIVE_TY_F32:
      return (struct ir_var_ty_info){.size = 4, .alignment = 4};
    case IR_OP_VAR_PRIMITIVE_TY_F64:
      return (struct ir_var_ty_info){.size = 8, .alignment = 8};
    }
  case IR_OP_VAR_TY_TY_ARRAY: {
    struct ir_var_ty_info element_info = var_ty_info(iru, ty->array.underlying);
    size_t size = ty->array.num_elements * element_info.size;
    return (struct ir_var_ty_info){.size = size,
                                   .alignment = element_info.alignment};
  }
  case IR_OP_VAR_TY_TY_STRUCT: {
    size_t max_alignment = 0;
    size_t size = 0;
    size_t num_fields = ty->struct_ty.num_fields;
    size_t *offsets = arena_alloc(iru->arena, sizeof(*offsets) * num_fields);

    for (size_t i = 0; i < ty->struct_ty.num_fields; i++) {
      struct ir_op_var_ty *field = &ty->struct_ty.fields[i];
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
  case IR_OP_VAR_TY_TY_UNION: {
    size_t max_alignment = 0;
    size_t size = 0;
    size_t num_fields = ty->struct_ty.num_fields;

    for (size_t i = 0; i < ty->struct_ty.num_fields; i++) {
      struct ir_op_var_ty *field = &ty->struct_ty.fields[i];
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
  debug_assert(!(op->flags & IR_OP_FLAG_FIXED_REG),
               "spilling fixed reg illegal");

  debug("spilling %zu\n", op->id);

  if (!op->lcl) {
    op->lcl = add_local(irb, &op->var_ty);
  }

  if (op->ty != IR_OP_TY_PHI) {
    // storing undf makes no sense
    if (op->ty != IR_OP_TY_UNDF) {
      struct ir_op *store =
          insert_after_ir_op(irb, op, IR_OP_TY_STORE_LCL, IR_OP_VAR_TY_NONE);
      store->lcl = op->lcl;
      store->store_lcl.value = op;
      store->reg = NO_REG;
      store->flags |= IR_OP_FLAG_SPILL;

      op->lcl->store = store;
      return store;
    }

    return NULL;
  }

  // if phi is spilled, first ensure it doesn't already have a local via one of
  // its values then, once spilled, propogate its local to all of its dependents
  struct ir_lcl *lcl = NULL;
  for (size_t i = 0; i < op->phi.num_values; i++) {
    struct ir_op *value = op->phi.values[i];

    if (value->lcl) {
      lcl = value->lcl;
    }
  }

  if (!lcl) {
    lcl = add_local(irb, &op->var_ty);
  }

  op->lcl = lcl;
  for (size_t i = 0; i < op->phi.num_values; i++) {
    struct ir_op *value = op->phi.values[i];

    value->lcl = lcl;
  }

  return NULL;
}

struct build_op_uses_callback_data {
  struct ir_op *op;
  struct use_data *use_data;
};

struct use_data {
  struct ir_op *op;
  struct vector *uses;
};

static void build_op_uses_callback(struct ir_op **op, void *cb_metadata) {
  struct build_op_uses_callback_data *data = cb_metadata;

  vector_push_back(data->use_data[(*op)->id].uses, data->op);
}

struct ir_op_uses build_op_uses_map(struct ir_func *func) {
  rebuild_ids(func);

  struct build_op_uses_callback_data data = {
      .op = NULL,
      .use_data =
          arena_alloc(func->arena, sizeof(*data.use_data) * func->op_count)};

  for (size_t i = 0; i < func->op_count; i++) {
    // because walk_op_uses can be out of order we need to create the vectors in
    // advance
    data.use_data[i].uses = vector_create(sizeof(struct ir_op *));
  }

  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        data.op = op;
        data.use_data[op->id].op = op;

        walk_op_uses(op, build_op_uses_callback, &data);

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  struct ir_op_uses uses = {
      .num_use_datas = func->op_count,
      .use_datas =
          arena_alloc(func->arena, sizeof(*uses.use_datas) * func->op_count)};

  for (size_t i = 0; i < func->op_count; i++) {
    struct use_data *use_data = &data.use_data[i];

    debug_assert(i == use_data->op->id, "ops were not keyed");

    uses.use_datas[i] = (struct ir_op_use){
        .op = use_data->op,
        .num_uses = vector_length(use_data->uses),
        .uses = arena_alloc(func->arena, vector_byte_size(use_data->uses))};

    vector_copy_to(use_data->uses, uses.use_datas[i].uses);
  }

  return uses;
}
