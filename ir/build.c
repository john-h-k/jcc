#include "build.h"
#include "../alloc.h"
#include "../compiler.h"
#include "../lex.h"
#include "../parse.h"
#include "../util.h"
#include "../var_table.h"
#include "../vector.h"
#include <math.h>

bool op_produces_value(enum ir_op_ty ty) {
  switch (ty) {
  case IR_OP_TY_PHI:
  case IR_OP_TY_MOV:
  case IR_OP_TY_CNST:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_LOAD_LCL:
    return true;
  case IR_OP_TY_STORE_LCL:
  case IR_OP_TY_BR_COND:
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
    return false;
  }
}

bool op_is_branch(enum ir_op_ty ty) {
  switch (ty) {
  case IR_OP_TY_BR_COND:
  case IR_OP_TY_RET:
  case IR_OP_TY_BR:
    return true;
  case IR_OP_TY_PHI:
  case IR_OP_TY_MOV:
  case IR_OP_TY_CNST:
  case IR_OP_TY_BINARY_OP:
  case IR_OP_TY_STORE_LCL:
  case IR_OP_TY_LOAD_LCL:
    return false;
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

void walk_ret(struct ir_op_ret *ret, walk_op_callback *cb, void *cb_metadata) {
  if (ret->value) {
    walk_op(ret->value, cb, cb_metadata);
  }
}

void walk_op_uses(struct ir_op *op, walk_op_callback *cb, void *cb_metadata) {
  switch (op->ty) {
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
  case IR_OP_TY_STORE_LCL:
    cb(&op->store_lcl.value, cb_metadata);
    break;
  case IR_OP_TY_LOAD_LCL:
    break;
  case IR_OP_TY_BR:
    break;
  case IR_OP_TY_BR_COND:
    cb(&op->br_cond.cond, cb_metadata);
    break;
  case IR_OP_TY_MOV:
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
  case IR_OP_TY_PHI:
    todo("walk phi");
  case IR_OP_TY_MOV:
    todo("walk mov");
  case IR_OP_TY_CNST:
    walk_cnst(&op->cnst, cb, cb_metadata);
    break;
  case IR_OP_TY_BINARY_OP:
    walk_binary_op(&op->binary_op, cb, cb_metadata);
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
  case IR_OP_BINARY_OP_TY_ADD:
  case IR_OP_BINARY_OP_TY_SUB:
  case IR_OP_BINARY_OP_TY_MUL:
    return IR_OP_SIGN_NA;
  case IR_OP_BINARY_OP_TY_SDIV:
  case IR_OP_BINARY_OP_TY_SQUOT:
    return IR_OP_SIGN_SIGNED;
  case IR_OP_BINARY_OP_TY_UDIV:
  case IR_OP_BINARY_OP_TY_UQUOT:
    return IR_OP_SIGN_UNSIGNED;
  }
}

const struct ir_op_var_ty IR_OP_VAR_TY_NONE = {.ty = IR_OP_VAR_TY_TY_NONE};

void initialise_ir_op(struct ir_op *op, size_t id, enum ir_op_ty ty,
                      struct ir_op_var_ty var_ty, struct ir_op *pred,
                      struct ir_op *succ, struct ir_stmt *stmt,
                      unsigned long reg, unsigned long lcl_idx) {
  op->id = id;
  op->ty = ty;
  op->var_ty = var_ty;
  op->pred = pred;
  op->succ = succ;
  op->stmt = stmt;
  op->reg = reg;
  op->lcl_idx = lcl_idx;
  op->metadata = NULL;
}

void detach_ir_op(struct ir_op *op) {
  // fix links on either side of op
  if (op->pred) {
    op->pred->succ = op->succ;
  }

  if (op->succ) {
    op->succ->pred = op->pred;
  }
}

void move_after_ir_op(struct ir_builder *irb, struct ir_op *op,
                      struct ir_op *move_after) {
  UNUSED_ARG(irb);
  invariant_assert(op->id != move_after->id, "trying to move op after itself!");
  invariant_assert(op->stmt == NULL || op->stmt == move_after->stmt,
                   "moving between ir_stmts not supported");

  detach_ir_op(op);

  op->stmt = move_after->stmt;

  debug("move after pred %zu, succ %zu",
        move_after->pred ? move_after->pred->id : SIZE_T_MAX,
        move_after->succ ? move_after->succ->id : SIZE_T_MAX);

  // now insert it
  op->pred = move_after;
  op->succ = move_after->succ;

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

void move_before_ir_op(struct ir_builder *irb, struct ir_op *op,
                       struct ir_op *move_before) {
  UNUSED_ARG(irb);
  invariant_assert(op->id != move_before->id,
                   "trying to move op before itself!");
  invariant_assert(op->stmt == NULL || op->stmt == move_before->stmt,
                   "moving between ir_stmts not supported");

  detach_ir_op(op);

  debug("move before pred %zu, succ %zu",
        move_before->pred ? move_before->pred->id : SIZE_T_MAX,
        move_before->succ ? move_before->succ->id : SIZE_T_MAX);

  op->stmt = move_before->stmt;

  // now insert it
  op->pred = move_before->pred;
  op->succ = move_before;

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

struct ir_op *insert_before_ir_op(struct ir_builder *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_op_var_ty var_ty) {
  debug_assert(insert_before, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->op_count++, ty, var_ty, NULL, NULL,
                   insert_before->stmt, NO_REG, NO_LCL);

  move_before_ir_op(irb, op, insert_before);

  return op;
}

struct ir_op *insert_after_ir_op(struct ir_builder *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_op_var_ty var_ty) {
  debug_assert(insert_after, "invalid insertion point!");

  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->op_count++, ty, var_ty, NULL, NULL,
                   insert_after->stmt, NO_REG, NO_LCL);

  move_after_ir_op(irb, op, insert_after);

  return op;
}

// TODO: this should call `initialise_ir_op`
struct ir_op *alloc_ir_op(struct ir_builder *irb, struct ir_stmt *stmt) {
  struct ir_op *op = arena_alloc(irb->arena, sizeof(*op));

  if (!stmt->first) {
    stmt->first = op;
  }

  op->id = irb->op_count++;
  op->stmt = stmt;
  op->pred = stmt->last;
  op->succ = NULL;

  if (stmt->last) {
    stmt->last->succ = op;
  }

  stmt->last = op;

  return op;
}

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
  basicblock->var_table = var_table_create(irb->parser);
  basicblock->pred = irb->last;
  basicblock->succ = NULL;
  basicblock->function_offset = irb->op_count;

  basicblock->preds = NULL;
  basicblock->num_preds = 0;

  if (irb->last) {
    irb->last->succ = basicblock;
  }

  irb->last = basicblock;

  return basicblock;
}

enum ir_op_var_primitive_ty ty_for_well_known_ty(enum well_known_ty wkt) {
  switch (wkt) {
  case WELL_KNOWN_TY_ASCII_CHAR:
  case WELL_KNOWN_TY_SIGNED_CHAR:
  case WELL_KNOWN_TY_UNSIGNED_CHAR:
    return IR_OP_VAR_PRIMITIVE_TY_I8;
  case WELL_KNOWN_TY_SIGNED_SHORT:
  case WELL_KNOWN_TY_UNSIGNED_SHORT:
    return IR_OP_VAR_PRIMITIVE_TY_I16;
  case WELL_KNOWN_TY_SIGNED_INT:
  case WELL_KNOWN_TY_UNSIGNED_INT:
    return IR_OP_VAR_PRIMITIVE_TY_I32;
  case WELL_KNOWN_TY_SIGNED_LONG:
  case WELL_KNOWN_TY_UNSIGNED_LONG:
    return IR_OP_VAR_PRIMITIVE_TY_I64;
  case WELL_KNOWN_TY_SIGNED_LONG_LONG:
  case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
    return IR_OP_VAR_PRIMITIVE_TY_I64;
  }
}

struct ir_op_var_ty ty_for_ast_tyref(const struct ast_tyref *ty_ref) {
  switch (ty_ref->ty) {
  case AST_TYREF_TY_UNKNOWN:
    bug("shouldn't reach IR gen with unresolved type");
  case AST_TYREF_TY_WELL_KNOWN: {
    struct ir_op_var_ty ty;
    ty.ty = IR_OP_VAR_TY_TY_PRIMITIVE;
    ty.primitive = ty_for_well_known_ty(ty_ref->well_known);
    return ty;
  }
  }
}

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_expr *expr);

struct ir_op *build_ir_for_binaryop(struct ir_builder *irb,
                                    struct ir_stmt *stmt,
                                    struct ast_binaryop *binary_op) {
  struct ir_op *lhs = build_ir_for_expr(irb, stmt, binary_op->lhs);
  struct ir_op *rhs = build_ir_for_expr(irb, stmt, binary_op->rhs);

  struct ir_op *op = alloc_ir_op(irb, stmt);
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = ty_for_ast_tyref(&binary_op->var_ty);

  struct ir_op_binary_op *b = &op->binary_op;

  b->lhs = lhs;
  b->rhs = rhs;

  invariant_assert(binary_op->var_ty.ty == AST_TYREF_TY_WELL_KNOWN,
                   "non primitives (/well-knowns) cannot be used in binary "
                   "expression by point IR is reached!");

  switch (binary_op->ty) {
  case AST_BINARY_OP_TY_ADD:
    b->ty = IR_OP_BINARY_OP_TY_ADD;
    break;
  case AST_BINARY_OP_TY_SUB:
    b->ty = IR_OP_BINARY_OP_TY_SUB;
    break;
  case AST_BINARY_OP_TY_MUL:
    b->ty = IR_OP_BINARY_OP_TY_MUL;
    break;
  case AST_BINARY_OP_TY_DIV:
    if (WKT_IS_SIGNED(binary_op->var_ty.well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SDIV;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UDIV;
    }
    break;
  case AST_BINARY_OP_TY_QUOT:
    if (WKT_IS_SIGNED(binary_op->var_ty.well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SQUOT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UQUOT;
    }
    break;
  }

  return op;
}

struct ir_op *build_ir_for_cnst(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_cnst *cnst) {
  struct ir_op *op = alloc_ir_op(irb, stmt);

  op->ty = IR_OP_TY_CNST;
  op->var_ty.ty = IR_OP_VAR_TY_TY_PRIMITIVE;
  op->var_ty.primitive = ty_for_well_known_ty(cnst->cnst_ty);
  op->cnst.value = cnst->value;

  return op;
}

struct ir_op *build_ir_for_assg(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_assg *assg) {
  switch (assg->lvalue.ty) {
  case AST_LVALUE_TY_VAR: {
    struct var_table_entry *entry =
        get_or_create_entry(&stmt->basicblock->var_table, &assg->lvalue.var);

    struct ir_op *expr = build_ir_for_expr(irb, stmt, assg->expr);

    debug_assert(expr, "null expr in assignment!");

    entry->value = expr;
    return expr;
  }
  }
}

struct ir_op *
build_ir_for_compoundexpr(struct ir_builder *irb, struct ir_stmt *stmt,
                          struct ast_compoundexpr *compound_expr) {
  for (size_t i = 0; i < compound_expr->num_exprs; i++) {
    build_ir_for_expr(irb, stmt, &compound_expr->exprs[i]);
  }

  return stmt->last;
}

struct ir_op *build_ir_for_rvalue(struct ir_builder *irb, struct ir_stmt *stmt,
                                  struct ast_rvalue *rvalue) {
  switch (rvalue->ty) {
  case AST_RVALUE_TY_CNST:
    return build_ir_for_cnst(irb, stmt, &rvalue->cnst);
  case AST_RVALUE_TY_ASSG:
    return build_ir_for_assg(irb, stmt, rvalue->assg);
  case AST_RVALUE_TY_BINARY_OP:
    return build_ir_for_binaryop(irb, stmt, &rvalue->binary_op);
  case AST_RVALUE_TY_COMPOUNDEXPR:
    return build_ir_for_compoundexpr(irb, stmt, &rvalue->compound_expr);
  }
}

struct ir_op *build_ir_for_var(struct ir_builder *irb, struct ir_stmt *stmt,
                               struct ast_var *var) {
  debug("build_ir_for_var name=%s, scope=%zu",
        identifier_str(irb->parser, &var->identifier), var->scope);
  UNUSED_ARG(stmt);

  // this is when we are _reading_ from the var
  struct var_table_entry *entry = get_entry(&stmt->basicblock->var_table, var);

  struct ir_op *expr = entry ? entry->value : NULL;

  if (expr) {
    debug("no phi!");
    return expr;
  }

  debug("phi");

  // we generate an empty phi and then after all blocks are built we insert the
  // correct values
  struct ir_op *phi = alloc_ir_op(irb, stmt);
  phi->ty = IR_OP_TY_PHI;
  phi->var_ty = IR_OP_VAR_TY_NONE;
  phi->phi.var = *var;
  warn("!!build_ir_for_var name=%s, scope=%zu",
       identifier_str(irb->parser, &phi->phi.var.identifier),
       phi->phi.var.scope);
  phi->phi.values = NULL;
  phi->phi.num_values = 0;

  return phi;
}

struct ir_op *build_ir_for_lvalue(struct ir_builder *irb, struct ir_stmt *stmt,
                                  struct ast_lvalue *lvalue) {
  switch (lvalue->ty) {
  case AST_LVALUE_TY_VAR: {
    return build_ir_for_var(irb, stmt, &lvalue->var);
  }
  }
}

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_expr *expr) {
  switch (expr->ty) {
  case AST_EXPR_TY_RVALUE:
    return build_ir_for_rvalue(irb, stmt, &expr->rvalue);
  case AST_EXPR_TY_LVALUE:
    return build_ir_for_lvalue(irb, stmt, &expr->lvalue);
  }
}

struct ir_basicblock *build_ir_for_stmt(struct ir_builder *irb,
                                        struct ir_basicblock *basicblock,
                                        struct ast_stmt *stmt);

struct ir_basicblock *
build_ir_for_compoundstmt(struct ir_builder *irb,
                          struct ir_basicblock *basicblock,
                          struct ast_compoundstmt *compound_stmt) {
  for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
    basicblock = build_ir_for_stmt(irb, basicblock, &compound_stmt->stmts[i]);
  }
  return basicblock;
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

void make_basicblock_split(struct ir_builder *irb,
                           struct ir_basicblock *basicblock,
                           struct ir_basicblock *true_target,
                           struct ir_basicblock *false_target) {
  basicblock->ty = IR_BASICBLOCK_TY_SPLIT;
  basicblock->split.true_target = true_target;
  basicblock->split.false_target = false_target;

  add_pred_to_basicblock(irb, true_target, basicblock);
  add_pred_to_basicblock(irb, false_target, basicblock);
}

void make_basicblock_merge(struct ir_builder *irb,
                           struct ir_basicblock *basicblock,
                           struct ir_basicblock *target) {
  basicblock->ty = IR_BASICBLOCK_TY_MERGE;
  basicblock->merge.target = target;

  add_pred_to_basicblock(irb, target, basicblock);
}

struct ir_basicblock *build_ir_for_if(struct ir_builder *irb,
                                      struct ir_basicblock *basicblock,
                                      struct ast_ifstmt *if_stmt) {
  struct ir_basicblock *pre_if_basicblock = basicblock;

  // basic block for if body
  struct ir_basicblock *if_basicblock = alloc_ir_basicblock(irb);

  // basic block for *after* if body
  struct ir_basicblock *after_if_basicblock;
  if (true || irb->last->first) {
    after_if_basicblock = alloc_ir_basicblock(irb);
  } else {
    // FIXME: does not currently work
    // existing BB is empty, we can use it
    // this makes nested if/else statements nicer as they all target the same
    // end BB rather than a series of empty ones
    after_if_basicblock = irb->last;
  }

  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb, pre_if_basicblock);
  struct ir_op *cond = build_ir_for_expr(irb, cond_stmt, &if_stmt->condition);
  struct ir_op *br_cond = alloc_ir_op(irb, cond_stmt);
  br_cond->ty = IR_OP_TY_BR_COND;
  br_cond->var_ty = IR_OP_VAR_TY_NONE;
  br_cond->br_cond.cond = cond;

  build_ir_for_stmt(irb, if_basicblock, if_stmt->body);

  if (!op_is_branch(if_basicblock->last->last->ty)) {
    // we add a redundant branch to keep the nice property that all BBs end in a
    // branch
    struct ir_op *br = alloc_ir_op(irb, if_basicblock->last);
    br->ty = IR_OP_TY_BR;
    br->var_ty = IR_OP_VAR_TY_NONE;
  }

  make_basicblock_split(irb, pre_if_basicblock, if_basicblock,
                        after_if_basicblock);
  make_basicblock_merge(irb, if_basicblock, after_if_basicblock);

  return after_if_basicblock;
}

struct ir_basicblock *build_ir_for_ifelse(struct ir_builder *irb,
                                          struct ir_basicblock *basicblock,
                                          struct ast_ifelsestmt *if_else_stmt) {
  struct ir_basicblock *pre_if_basicblock = basicblock;

  // basic block for if body
  struct ir_basicblock *if_basicblock = alloc_ir_basicblock(irb);
  build_ir_for_stmt(irb, if_basicblock, if_else_stmt->body);

  // basic block for else body
  struct ir_basicblock *else_basicblock = alloc_ir_basicblock(irb);
  build_ir_for_stmt(irb, else_basicblock, if_else_stmt->else_body);

  // basic block for *after* if-else
  struct ir_basicblock *after_if_else_basicblock;
  if (irb->last->first) {
    after_if_else_basicblock = alloc_ir_basicblock(irb);
  } else {
    // existing BB is empty, we can use it
    // this makes nested if/else statements nicer as they all target the same
    // end BB rather than a series of empty ones
    after_if_else_basicblock = irb->last;
  }

  make_basicblock_split(irb, pre_if_basicblock, if_basicblock, else_basicblock);

  struct ir_op *br_after_if = NULL;
  // branch to combined end, if the block itself doesn't already end in branch
  // TODO: does this work or does it incorrectly assume `op_is_branch` is
  // sufficient? could other branch types mess it up?
  if (!if_basicblock->last || !op_is_branch(if_basicblock->last->last->ty)) {
    br_after_if = alloc_ir_op(irb, if_basicblock->last);
    br_after_if->ty = IR_OP_TY_BR;
    br_after_if->var_ty = IR_OP_VAR_TY_NONE;
    make_basicblock_merge(irb, if_basicblock, after_if_else_basicblock);
  }

  struct ir_op *br_after_else = NULL;
  // branch to combined end, if the block itself doesn't already end in branch
  if (!else_basicblock->last ||
      !op_is_branch(else_basicblock->last->last->ty)) {
    br_after_else = alloc_ir_op(irb, else_basicblock->last);
    br_after_else->ty = IR_OP_TY_BR;
    br_after_else->var_ty = IR_OP_VAR_TY_NONE;
    make_basicblock_merge(irb, else_basicblock, after_if_else_basicblock);
  }

  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb, pre_if_basicblock);
  struct ir_op *cond = build_ir_for_expr(irb, cond_stmt, &if_else_stmt->condition);
  struct ir_op *br_cond = alloc_ir_op(irb, cond_stmt);
  br_cond->ty = IR_OP_TY_BR_COND;
  br_cond->var_ty = IR_OP_VAR_TY_NONE;
  br_cond->br_cond.cond = cond;

  return after_if_else_basicblock;
}

struct ir_basicblock *
build_ir_for_selectstmt(struct ir_builder *irb,
                        struct ir_basicblock *basicblock,
                        struct ast_selectstmt *select_stmt) {
  switch (select_stmt->ty) {
  case AST_SELECTSTMT_TY_IF: {
    return build_ir_for_if(irb, basicblock, &select_stmt->if_stmt);
  }
  case AST_SELECTSTMT_TY_IF_ELSE:
    return build_ir_for_ifelse(irb, basicblock, &select_stmt->if_else_stmt);
  case AST_SELECTSTMT_TY_SWITCH:
    todo("switch IR");
  }
}

struct ir_basicblock *build_ir_for_iterstmt(struct ir_builder *irb,
                                            struct ir_basicblock *basicblock,
                                            struct ast_iterstmt *iter_stmt) {
  switch (iter_stmt->ty) {
  case AST_ITERSTMT_TY_WHILE: {
    struct ir_basicblock *before_cond_basicblock = basicblock;
    struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb);
    struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb);
    struct ir_basicblock *after_body_basicblock = alloc_ir_basicblock(irb);

    make_basicblock_merge(irb, before_cond_basicblock, cond_basicblock);
    make_basicblock_merge(irb, body_basicblock, cond_basicblock);
    make_basicblock_split(irb, cond_basicblock, body_basicblock,
                          after_body_basicblock);

    struct ir_stmt *cond_stmt = alloc_ir_stmt(irb, cond_basicblock);
    struct ir_op *cond =
        build_ir_for_expr(irb, cond_stmt, &iter_stmt->while_stmt.condition);
    struct ir_op *cond_br = alloc_ir_op(irb, cond_stmt);
    cond_br->ty = IR_OP_TY_BR_COND;
    cond_br->var_ty = IR_OP_VAR_TY_NONE;
    cond_br->br_cond.cond = cond;

    struct ir_basicblock *body_stmt_basicblock =
        build_ir_for_stmt(irb, body_basicblock, iter_stmt->while_stmt.body);
    debug_assert(body_stmt_basicblock == body_basicblock, "stmt in wrong bb");

    struct ir_op *pre_cond_br = alloc_ir_op(irb, before_cond_basicblock->last);
    pre_cond_br->ty = IR_OP_TY_BR;
    pre_cond_br->var_ty = IR_OP_VAR_TY_NONE;

    struct ir_stmt *br_stmt = alloc_ir_stmt(irb, body_basicblock);
    struct ir_op *br = alloc_ir_op(irb, br_stmt);
    br->ty = IR_OP_TY_BR;
    br->var_ty = IR_OP_VAR_TY_NONE;

    return after_body_basicblock;
  }
  }
}

struct ir_basicblock *build_ir_for_jumpstmt(struct ir_builder *irb,
                                            struct ir_stmt *stmt,
                                            struct ast_jumpstmt *jump_stmt) {
  switch (jump_stmt->ty) {
  case AST_JUMPSTMT_TY_RETURN: {
    struct ir_op *expr_op = build_ir_for_expr(irb, stmt, &jump_stmt->ret_expr);

    struct ir_op *op = alloc_ir_op(irb, stmt);
    op->ty = IR_OP_TY_RET;
    op->ret.value = expr_op;

    op->stmt->basicblock->ty = IR_BASICBLOCK_TY_RET;

    struct ir_basicblock *after_ret_basicblock = alloc_ir_basicblock(irb);

    return after_ret_basicblock;
  }
  }
}

struct ir_op *build_ir_for_vardecllist(struct ir_builder *irb,
                                       struct ir_stmt *stmt,
                                       struct ast_vardecllist *var_decl_list) {
  for (size_t i = 0; i < var_decl_list->num_decls; i++) {
    struct ast_vardecl *decl = &var_decl_list->decls[i];

    // a decl is _always_ a new entry (it may shadow)
    struct var_table_entry *entry =
        create_entry(&stmt->basicblock->var_table, &decl->var);

    if (decl->ty == AST_VARDECL_TY_DECL) {
      continue;
    }

    struct ir_op *expr = build_ir_for_expr(irb, stmt, &decl->assg_expr);
    debug_assert(expr, "null expr in assignment!");

    entry->value = expr;
  }

  return stmt->last;
}

struct ir_basicblock *build_ir_for_stmt(struct ir_builder *irb,
                                        struct ir_basicblock *basicblock,
                                        struct ast_stmt *stmt) {

  switch (stmt->ty) {
  case AST_STMT_TY_VAR_DECL_LIST: {
    struct ir_stmt *ir_stmt = alloc_ir_stmt(irb, basicblock);
    build_ir_for_vardecllist(irb, ir_stmt, &stmt->var_decl_list);
    return basicblock;
  }
  case AST_STMT_TY_EXPR: {
    // TODO: ternaries
    struct ir_stmt *ir_stmt = alloc_ir_stmt(irb, basicblock);
    build_ir_for_expr(irb, ir_stmt, &stmt->expr);
    return basicblock;
  }
  case AST_STMT_TY_JUMP: {
    struct ir_stmt *ir_stmt = alloc_ir_stmt(irb, basicblock);
    return build_ir_for_jumpstmt(irb, ir_stmt, &stmt->jump);
  }
  case AST_STMT_TY_COMPOUND: {
    return build_ir_for_compoundstmt(irb, basicblock, &stmt->compound);
  }
  case AST_STMT_TY_SELECT: {
    return build_ir_for_selectstmt(irb, basicblock, &stmt->select);
  }
  case AST_STMT_TY_ITER: {
    return build_ir_for_iterstmt(irb, basicblock, &stmt->iter);
  }
  }
}

size_t var_ty_size(struct ir_builder *irb, struct ir_op_var_ty *ty) {
  UNUSED_ARG(irb);

  switch (ty->ty) {
  case IR_OP_VAR_TY_TY_NONE:
    bug("IR_OP_VAR_TY_TY_NONE has no size");
  case IR_OP_VAR_TY_TY_PRIMITIVE:
    switch (ty->primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      return 1;
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      return 2;
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      return 4;
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      return 8;
    }
  }
}

void walk_basicblock(struct ir_builder *irb, bool *basicblocks_visited,
                     struct ast_var *var, struct ir_basicblock *basicblock,
                     struct ir_op ***exprs, size_t *num_exprs) {
  if (!basicblock || basicblocks_visited[basicblock->id]) {
    return;
  }

  basicblocks_visited[basicblock->id] = true;
  debug("now walking %zu", basicblock->id);

  struct var_table_entry *entry = get_entry(&basicblock->var_table, var);

  if (entry && entry->value) {
    (*num_exprs)++;
    *exprs =
        arena_realloc(irb->arena, *exprs, sizeof(struct ir_op *) * *num_exprs);
    (*exprs)[*num_exprs - 1] = (struct ir_op *)entry->value;
    return;
  }

  debug("bb %zu has %zu preds", basicblock->id, basicblock->num_preds);
  for (size_t i = 0; i < basicblock->num_preds; i++) {
    walk_basicblock(irb, basicblocks_visited, var, basicblock->preds[i], exprs,
                    num_exprs);
  }
}

void find_phi_exprs(struct ir_builder *irb, struct ir_op *phi) {
  debug_assert(phi->ty == IR_OP_TY_PHI, "non-phi in `find_phi_exprs`");

  // walk predecessor basic blocks (splitting into seperate walks each time we
  // have multiple predecessors) until we
  // * A) find a write
  // * B) re-reach current bb
  // * or C) reach end (first bb)
  bool *basicblocks_visited = arena_alloc(
      irb->arena, sizeof(*basicblocks_visited) * irb->basicblock_count);
  memset(basicblocks_visited, 0,
         sizeof(*basicblocks_visited) * irb->basicblock_count);

  struct ir_op **exprs = NULL;
  size_t num_exprs = 0;

  for (size_t i = 0; i < phi->stmt->basicblock->num_preds; i++) {
    walk_basicblock(irb, basicblocks_visited, &phi->phi.var,
                    phi->stmt->basicblock->preds[i], &exprs, &num_exprs);
  }

  debug("find phi exprs for op %zu", phi->id);

  if (!num_exprs) {
    err("undefined behaviour - reading from unassigned variable '%s'",
        identifier_str(irb->parser, &phi->phi.var.identifier));
  }

  phi->var_ty = exprs[0]->var_ty;
  phi->phi.values = exprs;
  phi->phi.num_values = num_exprs;
}

struct ir_builder *build_ir_for_function(struct parser *parser,
                                         struct arena_allocator *arena,
                                         struct ast_funcdef *def) {
  struct ir_builder b = {.name = identifier_str(parser, &def->sig.name),
                         .parser = parser,
                         .arena = arena,
                         .first = NULL,
                         .last = NULL,
                         .op_count = 0,
                         .num_locals = 0,
                         .total_locals_size = 0};

  struct ir_builder *builder = arena_alloc(arena, sizeof(b));
  *builder = b;

  // needs at least one initial basic block
  alloc_ir_basicblock(builder);

  struct ir_basicblock *basicblock = builder->first;

  for (size_t i = 0; i < def->body.num_stmts; i++) {
    basicblock = build_ir_for_stmt(builder, basicblock, &def->body.stmts[i]);
  }

  debug_print_ir(builder, builder->first, NULL, NULL);

  // now we fix up phis
  debug("doing phi fixups");
  basicblock = builder->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        if (op->ty == IR_OP_TY_PHI) {
          find_phi_exprs(builder, op);
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  return builder;
}

const char *binary_op_string(enum ir_op_binary_op_ty ty) {
  switch (ty) {
  case IR_OP_BINARY_OP_TY_ADD:
    return "+";
  case IR_OP_BINARY_OP_TY_SUB:
    return "-";
  case IR_OP_BINARY_OP_TY_MUL:
    return "*";
  case IR_OP_BINARY_OP_TY_SDIV:
    return "s/";
  case IR_OP_BINARY_OP_TY_UDIV:
    return "u/";
  case IR_OP_BINARY_OP_TY_SQUOT:
    return "s%";
  case IR_OP_BINARY_OP_TY_UQUOT:
    return "u%";
  default:
    return "?unknown?";
  }
}

const char *var_ty_string(const struct ir_op_var_ty *var_ty) {
  switch (var_ty->ty) {
  case IR_OP_VAR_TY_TY_NONE: {
    return "<none>";
  }
  case IR_OP_VAR_TY_TY_PRIMITIVE: {
    switch (var_ty->primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      return "i8";
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      return "i16";
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      return "i32";
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      return "i64";
    }
  }
  }
}

const char *phi_string(struct ir_op_phi *phi) {
  // just assume we don't have more than 100,000 phi inputs
  char *buff = nonnull_malloc(phi->num_values * (6 + 2));
  char *head = buff;

  for (size_t i = 0; i < phi->num_values; i++) {
    head += sprintf(head, "%%%zu", phi->values[i]->id);

    if (i + 1 < phi->num_values) {
      head += sprintf(head, ", ");
    }
  }

  *head = '\0';
  return buff;
}

void debug_print_op(struct ir_op *ir) {
  FILE *file = stderr;

  switch (ir->ty) {
  case IR_OP_TY_PHI:
    fslogsl(file, "%%%zu (%s) = phi [ %s ]", ir->id, var_ty_string(&ir->var_ty),
            phi_string(&ir->phi));
    break;
  case IR_OP_TY_MOV:
    fslogsl(file, "%%%zu (%s) = %%%zu", ir->id, var_ty_string(&ir->var_ty),
            ir->mov.value->id);
    break;
  case IR_OP_TY_CNST:
    fslogsl(file, "%%%zu (%s) = %zu", ir->id, var_ty_string(&ir->var_ty),
            ir->cnst.value);
    break;
  case IR_OP_TY_BINARY_OP:
    fslogsl(file, "%%%zu (%s) = %%%zu %s %%%zu", ir->id,
            var_ty_string(&ir->var_ty), ir->binary_op.lhs->id,
            binary_op_string(ir->binary_op.ty), ir->binary_op.rhs->id);
    break;
  case IR_OP_TY_STORE_LCL:
    fslogsl(file, "%%%zu (%s) = storelcl LCL(%zu), %%%zu", ir->id,
            var_ty_string(&ir->var_ty), ir->store_lcl.lcl_idx,
            ir->store_lcl.value->id);
    break;
  case IR_OP_TY_LOAD_LCL:
    fslogsl(file, "%%%zu = loadlcl (%s) LCL(%zu)", ir->id,
            var_ty_string(&ir->var_ty), ir->load_lcl.lcl_idx);
    break;
  case IR_OP_TY_BR:
    invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_MERGE,
                     "found `br` but bb wasn't MERGE");
    fslogsl(file, "br @%zu", ir->stmt->basicblock->merge.target->id);
    break;
  case IR_OP_TY_BR_COND:
    invariant_assert(ir->stmt->basicblock->ty == IR_BASICBLOCK_TY_SPLIT,
                     "found `br.cond` but bb wasn't SPLIT");
    fslogsl(file, "br.cond %%%zu, TRUE(@%zu), FALSE(@%zu)",
            ir->br_cond.cond->id, ir->stmt->basicblock->split.true_target->id,
            ir->stmt->basicblock->split.false_target->id);
    break;
  case IR_OP_TY_RET:
    fslogsl(file, "return %%%zu", ir->ret.value->id);
    break;
  }
}

void debug_print_ir(struct ir_builder *irb, struct ir_basicblock *basicblock,
                    debug_print_op_callback *cb, void *cb_metadata) {
  debug("%zu statements", irb->stmt_count);

  int ctr_pad = (int)log10(irb->op_count) + 1;
  size_t ctr = 0;

  FILE *file = stderr;

  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    fslog(file, "\nBB @ %03zu", basicblock->id);

    while (stmt) {
      struct ir_op *ir = stmt->first;

      int op_pad = /* guess */ 50;

      while (ir) {
        fslogsl(file, "%0*zu: ", ctr_pad, ctr++);

        // HACK: this shouldn't rely on the fact `log` goes to `stderr`
        long pos = ftell(stderr);
        debug_print_op(ir);
        long width = ftell(stderr) - pos;
        long pad = op_pad - width;

        if (pad > 0) {
          fslogsl(file, "%*s", (int)pad, "");
        }

        if (cb) {
          fslogsl(file, " | ");
          cb(stderr, ir, cb_metadata);
        }
        fslogsl(file, "\n");

        ir = ir->succ;
      }

      stmt = stmt->succ;
    }
    basicblock = basicblock->succ;
  }
}
