#include "build.h"

#include "../alloc.h"
#include "../compiler.h"
#include "../lex.h"
#include "../parse.h"
#include "../util.h"
#include "../var_table.h"
#include "../vector.h"
#include "ir.h"
#include "prettyprint.h"

#include <math.h>

enum ir_op_var_primitive_ty ty_for_well_known_ty(enum well_known_ty wkt) {
  switch (wkt) {
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
                                    struct ast_binary_op *binary_op) {
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
  // all phis appear at the start of their bb as they execute ""
  struct ir_op *phi;
  if (stmt->basicblock->first->first) {
    phi = insert_before_ir_op(irb, stmt->basicblock->first->first, IR_OP_TY_PHI, IR_OP_VAR_TY_NONE);
  } else {
    phi = alloc_ir_op(irb, stmt->basicblock->first);
    phi->ty = IR_OP_TY_PHI;
    phi->var_ty = IR_OP_VAR_TY_NONE;
  }

  phi->phi.var = *var;
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
  struct ir_op *cond =
      build_ir_for_expr(irb, cond_stmt, &if_else_stmt->condition);
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

struct ir_basicblock *build_ir_for_whilestmt(struct ir_builder *irb,
                                            struct ir_basicblock *basicblock,
                                            struct ast_whilestmt *while_stmt) {
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
      build_ir_for_expr(irb, cond_stmt, &while_stmt->cond);
  struct ir_op *cond_br = alloc_ir_op(irb, cond_stmt);
  cond_br->ty = IR_OP_TY_BR_COND;
  cond_br->var_ty = IR_OP_VAR_TY_NONE;
  cond_br->br_cond.cond = cond;

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, while_stmt->body);
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

struct ir_basicblock *build_ir_for_dowhilestmt(struct ir_builder *irb,
                                            struct ir_basicblock *basicblock,
                                            struct ast_dowhilestmt *do_while_stmt) {
  struct ir_basicblock *before_body_basicblock = basicblock;
  struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb);
  struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb);
  struct ir_basicblock *after_cond_basicblock = alloc_ir_basicblock(irb);

  make_basicblock_merge(irb, before_body_basicblock, body_basicblock);
  make_basicblock_merge(irb, body_basicblock, cond_basicblock);
  make_basicblock_split(irb, cond_basicblock, body_basicblock,
                        after_cond_basicblock);

  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb, cond_basicblock);
  struct ir_op *cond =
      build_ir_for_expr(irb, cond_stmt, &do_while_stmt->cond);
  struct ir_op *cond_br = alloc_ir_op(irb, cond_stmt);
  cond_br->ty = IR_OP_TY_BR_COND;
  cond_br->var_ty = IR_OP_VAR_TY_NONE;
  cond_br->br_cond.cond = cond;

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, do_while_stmt->body);
  debug_assert(body_stmt_basicblock == body_basicblock, "stmt in wrong bb");

  struct ir_op *pre_body_br = alloc_ir_op(irb, before_body_basicblock->last);
  pre_body_br->ty = IR_OP_TY_BR;
  pre_body_br->var_ty = IR_OP_VAR_TY_NONE;

  struct ir_stmt *br_stmt = alloc_ir_stmt(irb, body_basicblock);
  struct ir_op *br = alloc_ir_op(irb, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_OP_VAR_TY_NONE;

  return after_cond_basicblock;
}

struct ir_op *build_ir_for_vardecllist(struct ir_builder *irb,
                                       struct ir_stmt *stmt,
                                       struct ast_vardecllist *var_decl_list);

struct ir_op *build_ir_for_declorexpr(struct ir_builder *irb, struct ir_stmt *stmt, struct ast_declorexpr *decl_or_expr) {
  if (decl_or_expr->decl) {
    return build_ir_for_vardecllist(irb, stmt, decl_or_expr->decl);
  } else if (decl_or_expr->expr) {
    return build_ir_for_expr(irb, stmt, decl_or_expr->expr);
  }

  return NULL;
}

struct ir_basicblock *build_ir_for_forstmt(struct ir_builder *irb,
                                            struct ir_basicblock *basicblock,
                                            struct ast_forstmt *for_stmt) {
  
  struct ir_basicblock *before_cond_basicblock = basicblock;
  struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb);
  struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb);
  struct ir_basicblock *after_body_basicblock = alloc_ir_basicblock(irb);

  make_basicblock_merge(irb, before_cond_basicblock, cond_basicblock);
  make_basicblock_merge(irb, body_basicblock, cond_basicblock);
  make_basicblock_split(irb, cond_basicblock, body_basicblock,
                        after_body_basicblock);

  if (for_stmt->init) {
    struct ir_stmt *init_stmt = alloc_ir_stmt(irb, before_cond_basicblock);
    build_ir_for_declorexpr(irb, init_stmt, for_stmt->init);
  }

  invariant_assert(for_stmt->cond, "for stmt without cond not yet supported");

  if (for_stmt->cond) {
    struct ir_stmt *cond_stmt = alloc_ir_stmt(irb, cond_basicblock);
    struct ir_op *cond =
        build_ir_for_expr(irb, cond_stmt, for_stmt->cond);
    struct ir_op *cond_br = alloc_ir_op(irb, cond_stmt);
    cond_br->ty = IR_OP_TY_BR_COND;
    cond_br->var_ty = IR_OP_VAR_TY_NONE;
    cond_br->br_cond.cond = cond;
  }

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, for_stmt->body);

  if (for_stmt->iter) {
    invariant_assert(body_basicblock->last, "attempting to add `for` loop iter without stmt present, needs fixing");
    build_ir_for_expr(irb, body_basicblock->last, for_stmt->iter);
  }

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

struct ir_basicblock *build_ir_for_iterstmt(struct ir_builder *irb,
                                            struct ir_basicblock *basicblock,
                                            struct ast_iterstmt *iter_stmt) {
  switch (iter_stmt->ty) {
  case AST_ITERSTMT_TY_WHILE:
      return build_ir_for_whilestmt(irb, basicblock, &iter_stmt->while_stmt);
  case AST_ITERSTMT_TY_DO_WHILE:
      return build_ir_for_dowhilestmt(irb, basicblock, &iter_stmt->do_while_stmt);
  case AST_ITERSTMT_TY_FOR:
      return build_ir_for_forstmt(irb, basicblock, &iter_stmt->for_stmt);
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
  case AST_STMT_TY_NULL: {
    return basicblock;   
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

  // we may generate empty basicblocks or statements, prune them here
  prune_basicblocks(builder);

  debug_print_ir(stderr, builder, NULL, NULL);

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
