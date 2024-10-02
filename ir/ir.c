#include "ir.h"

#include "var_refs.h"

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

bool op_produces_value(enum ir_op_ty ty) {
  switch (ty) {
  case IR_OP_TY_UNKNOWN:
    bug("unknown op ty");
  case IR_OP_TY_PHI:
  case IR_OP_TY_UNDF:
  case IR_OP_TY_MOV:
  case IR_OP_TY_CNST:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_UNARY_OP:
  case IR_OP_TY_CAST_OP:
  case IR_OP_TY_LOAD_LCL:
  case IR_OP_TY_LOAD_ADDR:
  case IR_OP_TY_ADDR:
  case IR_OP_TY_CALL:
  case IR_OP_TY_GLB_REF:
    return true;
  case IR_OP_TY_STORE_LCL:
  case IR_OP_TY_STORE_ADDR:
  case IR_OP_TY_BR_COND:
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
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
    return true;
  // calls are NOT branches, because while they do leave, they guarantee return
  case IR_OP_TY_CALL:
  case IR_OP_TY_UNDF:
  case IR_OP_TY_GLB_REF:
  case IR_OP_TY_PHI:
  case IR_OP_TY_MOV:
  case IR_OP_TY_CNST:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_UNARY_OP:
  case IR_OP_TY_CAST_OP:
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
  case IR_OP_TY_GLB_REF: {
    break;
  }
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
  case IR_OP_TY_STORE_LCL:
    cb(&op->store_lcl.value, cb_metadata);
    break;
  case IR_OP_TY_LOAD_LCL:
    break;
  case IR_OP_TY_LOAD_ADDR:
    cb(&op->load_addr.addr, cb_metadata);
    break;
  case IR_OP_TY_ADDR:
    // this sort-of has uses but not really...
    break;
  case IR_OP_TY_BR:
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
  case IR_OP_TY_GLB_REF:
    todo("walk global");
  case IR_OP_TY_CALL:
    todo("walk call");
  case IR_OP_TY_PHI:
    todo("walk phi");
  case IR_OP_TY_MOV:
    todo("walk mov");
  case IR_OP_TY_LOAD_ADDR:
    todo("walk load addr");
  case IR_OP_TY_STORE_ADDR:
    todo("walk store addr");
  case IR_OP_TY_ADDR:
    todo("walk addr");
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
const struct ir_op_var_ty IR_OP_VAR_TY_I8 = { .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I8 };
const struct ir_op_var_ty IR_OP_VAR_TY_I16 = { .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I16 };
const struct ir_op_var_ty IR_OP_VAR_TY_I32 = { .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I32 };
const struct ir_op_var_ty IR_OP_VAR_TY_I64 = { .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I64 };
const struct ir_op_var_ty IR_OP_VAR_TY_F32 = { .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_F32 };
const struct ir_op_var_ty IR_OP_VAR_TY_F64 = { .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_F64 };
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
  op->live_gp_regs = 0;
  op->live_fp_regs = 0;
  op->metadata = NULL;
}

void detach_ir_basicblock(struct ir_builder *irb,
                          struct ir_basicblock *basicblock) {
  invariant_assert(irb->basicblock_count,
                   "`detach_ir_basicblock` would underflow basicblock count "
                   "for `ir_builder`");

  irb->basicblock_count--;

  invariant_assert(!basicblock->num_preds, "trying to detach BB with preds");

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

void detach_ir_stmt(struct ir_builder *irb, struct ir_stmt *stmt) {
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

void detach_ir_op(struct ir_builder *irb, struct ir_op *op) {
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

  return !basicblock->first;
}

void prune_basicblocks(struct ir_builder *irb) {
  struct ir_basicblock *basicblock = irb->first;

  while (basicblock) {
    prune_stmts(irb, basicblock);

    // save succ before we detach
    struct ir_basicblock *succ = basicblock->succ;

    // remove if it has no preds (if it has preds, it is needed as a target)
    if (basicblock_is_empty(basicblock) && !basicblock->num_preds) {
      detach_ir_basicblock(irb, basicblock);
    }

    basicblock = succ;
  }
}

void prune_stmts(struct ir_builder *irb, struct ir_basicblock *basicblock) {
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

void clear_metadata(struct ir_builder *irb) {
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

void rebuild_ids(struct ir_builder *irb) {
  irb->next_id = 0;

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      struct ir_op *op = stmt->first;

      while (op) {
        op->id = irb->next_id++;

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}

void attach_ir_op(struct ir_builder *irb, struct ir_op *op,
                  struct ir_stmt *stmt, struct ir_op *pred,
                  struct ir_op *succ) {
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

void move_after_ir_op(struct ir_builder *irb, struct ir_op *op,
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

void move_before_ir_op(struct ir_builder *irb, struct ir_op *op,
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

struct ir_op *replace_ir_op(struct ir_builder *irb, struct ir_op *op,
                            enum ir_op_ty ty, struct ir_op_var_ty var_ty) {
  UNUSED_ARG(irb);
  debug_assert(op, "invalid replacement point!");

  op->ty = ty;
  op->var_ty = var_ty;

  return op;
}

struct ir_op *insert_before_ir_op(struct ir_builder *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_op_var_ty var_ty) {
  debug_assert(insert_before, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->next_id++, ty, var_ty, NO_REG, NULL);

  move_before_ir_op(irb, op, insert_before);

  return op;
}

struct ir_op *insert_after_ir_op(struct ir_builder *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_op_var_ty var_ty) {
  debug_assert(insert_after, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->next_id++, ty, var_ty, NO_REG, NULL);

  move_after_ir_op(irb, op, insert_after);

  return op;
}

void swap_ir_ops(struct ir_builder *irb, struct ir_op *left,
                 struct ir_op *right) {
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

// TODO: this should call `initialise_ir_op`
struct ir_op *alloc_ir_op(struct ir_builder *irb, struct ir_stmt *stmt) {
  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  if (!stmt->first) {
    stmt->first = op;
  }

  irb->op_count++;

  op->id = irb->next_id++;
  op->ty = IR_OP_TY_UNKNOWN;
  op->flags = IR_OP_FLAG_NONE;
  op->stmt = stmt;
  op->pred = stmt->last;
  op->succ = NULL;
  op->metadata = NULL;
  op->reg = NO_REG;
  op->live_gp_regs = 0;
  op->live_fp_regs = 0;
  op->lcl = NULL;

  if (stmt->last) {
    stmt->last->succ = op;
  }

  stmt->last = op;

  return op;
}

void make_integral_constant(struct ir_builder *irb, struct ir_op *op, enum ir_op_var_primitive_ty ty, unsigned long long value) {
  UNUSED_ARG(irb);

  op->ty = IR_OP_TY_CNST;
  op->var_ty = (struct ir_op_var_ty){
    .ty = IR_OP_VAR_TY_TY_PRIMITIVE,
    .primitive = ty
  };
  op->cnst = (struct ir_op_cnst){
    .ty = IR_OP_CNST_TY_INT,
    .int_value = value
  };
}

void make_pointer_constant(struct ir_builder *irb, struct ir_op *op, unsigned long long value) {
  op->ty = IR_OP_TY_CNST;
  op->var_ty = var_ty_for_pointer_size(irb);
  op->cnst = (struct ir_op_cnst){
    .ty = IR_OP_CNST_TY_INT,
    .int_value = value
  };
}

struct ir_op *alloc_integral_constant(struct ir_builder *irb, struct ir_stmt *stmt, enum ir_op_var_primitive_ty primitive, unsigned long long value);


struct ir_stmt *alloc_ir_stmt(struct ir_builder *irb,
                              struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = arena_alloc(irb->arena, sizeof(*stmt));

  if (!basicblock->first) {
    basicblock->first = stmt;
  }

  stmt->id = irb->stmt_count++;
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

void add_pred_to_basicblock(struct ir_builder *irb,
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

void get_basicblock_successors(struct ir_basicblock *basicblock,
                               struct ir_basicblock **first,
                               struct ir_basicblock **second) {
  *first = NULL;
  *second = NULL;

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    // returns don't have successors
    break;
  case IR_BASICBLOCK_TY_MERGE:
    *first = basicblock->merge.target;
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    *first = basicblock->split.true_target;
    *second = basicblock->split.false_target;
    break;
  }
}

struct ir_basicblock *alloc_ir_basicblock(struct ir_builder *irb) {
  struct ir_basicblock *basicblock =
      arena_alloc(irb->arena, sizeof(*basicblock));

  if (!irb->first) {
    irb->first = basicblock;
  }

  basicblock->id = irb->basicblock_count++;
  // disabled and checks are done manually now
  // because this broke stuff by writing to global var refs (bad!)
  // basicblock->var_refs = var_refs_create(irb->global_var_refs);
  basicblock->irb = irb;
  basicblock->pred = irb->last;
  basicblock->succ = NULL;
  basicblock->first = NULL;
  basicblock->last = NULL;
  basicblock->function_offset = irb->op_count;
  basicblock->metadata = NULL;
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

struct ir_basicblock *insert_basicblocks_after(struct ir_builder *irb,
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

struct ir_lcl *add_local(struct ir_builder *irb,
                         const struct ir_op_var_ty *var_ty) {
  struct ir_lcl *lcl = arena_alloc(irb->arena, sizeof(*lcl));
  lcl->id = irb->num_locals++;


  struct ir_var_ty_info ty_info = var_ty_info(irb, var_ty);

  size_t lcl_pad = ty_info.alignment - (irb->total_locals_size % ty_info.alignment);
  size_t lcl_size = ty_info.size;

  irb->total_locals_size += lcl_pad;

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

void make_sym_ref(struct ir_builder *irb, const char *sym_name,
                  struct ir_op *op, const struct ir_op_var_ty *var_ty) {
  struct ir_op_glb_ref *glb_ref = &op->glb_ref;

  op->ty = IR_OP_TY_GLB_REF;
  op->var_ty = *var_ty;
  op->reg = NO_REG;
  op->flags |= IR_OP_FLAG_DONT_GIVE_SLOT;

  glb_ref->ty = IR_OP_GLB_REF_TY_SYM;
  glb_ref->sym_name = sym_name;
  glb_ref->metadata = NULL;

  glb_ref->succ = irb->global_refs;
  irb->global_refs = glb_ref;
}

bool var_ty_is_simple(const struct ir_op_var_ty *var_ty) {
  // simple types - pointer, primitive
  return var_ty->ty == IR_OP_VAR_TY_TY_PRIMITIVE || var_ty->ty == IR_OP_VAR_TY_TY_POINTER;
}

bool var_ty_is_primitive(const struct ir_op_var_ty *var_ty, enum ir_op_var_primitive_ty primitive) {
  return var_ty->ty == IR_OP_VAR_TY_TY_PRIMITIVE && var_ty->primitive == primitive;
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
  case IR_OP_VAR_PRIMITIVE_TY_F32:
  case IR_OP_VAR_PRIMITIVE_TY_F64:
    return true;
  }
}


struct ir_var_ty_info var_ty_info(struct ir_builder *irb, const struct ir_op_var_ty *ty) {
  // FIXME: pointer size!
  UNUSED_ARG(irb);

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
    case IR_OP_VAR_PRIMITIVE_TY_F32:
      return (struct ir_var_ty_info){.size = 4, .alignment = 4};
    case IR_OP_VAR_PRIMITIVE_TY_F64:
      return (struct ir_var_ty_info){.size = 8, .alignment = 8};
    }
  case IR_OP_VAR_TY_TY_ARRAY: {
    struct ir_var_ty_info element_info = var_ty_info(irb, ty->array.underlying);
    size_t size = ty->array.num_elements * element_info.size;
    return (struct ir_var_ty_info){.size = size, .alignment = element_info.alignment};   
  }
  case IR_OP_VAR_TY_TY_STRUCT: {
    size_t max_alignment = 0;
    size_t size = 0;
    size_t num_fields = ty->struct_ty.num_fields;
    size_t *offsets = arena_alloc(irb->arena, sizeof(*offsets) * num_fields);

    for (size_t i = 0; i < ty->struct_ty.num_fields; i++) {
      struct ir_op_var_ty *field = &ty->struct_ty.fields[i];
      struct ir_var_ty_info info = var_ty_info(irb, field);
      max_alignment = MAX(max_alignment, info.alignment);

      size = ROUND_UP(size, info.alignment);

      offsets[i] = size;

      size += info.size;
    }

    return (struct ir_var_ty_info){.size = size, .alignment = max_alignment, .num_fields = num_fields, .offsets = offsets};
  }
  case IR_OP_VAR_TY_TY_UNION: {
    size_t max_alignment = 0;
    size_t size = 0;
    size_t num_fields = ty->struct_ty.num_fields;

    for (size_t i = 0; i < ty->struct_ty.num_fields; i++) {
      struct ir_op_var_ty *field = &ty->struct_ty.fields[i];
      struct ir_var_ty_info info = var_ty_info(irb, field);
      max_alignment = MAX(max_alignment, info.alignment);

      size = MAX(size, info.size);
    }

    return (struct ir_var_ty_info){.size = size, .alignment = max_alignment, .num_fields = num_fields, .offsets = NULL};
  }
  }
}

void spill_op(struct ir_builder *irb, struct ir_op *op) {
  debug("spilling %zu\n", op->id);

  if (op->reg.ty == IR_REG_TY_SPILLED) {
    debug_assert(op->lcl, "op was spilled but had no local");
    return;
  }

  op->reg = REG_SPILLED;

  if (op->ty != IR_OP_TY_PHI) {
    op->lcl = add_local(irb, &op->var_ty);   

    if (op->ty != IR_OP_TY_UNDF) {
      // storing undf makes no sense
      struct ir_op *store = insert_after_ir_op(irb, op, IR_OP_TY_STORE_LCL,
                                               IR_OP_VAR_TY_NONE);
      store->lcl = op->lcl;
      store->store_lcl.value = op;
      store->reg = NO_REG;
      store->flags |= IR_OP_FLAG_SPILL;

      op->lcl->store = store;
    }

    return;
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
}

