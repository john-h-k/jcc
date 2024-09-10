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
#include "var_refs.h"

#include <math.h>

struct var_key get_var_key(struct parser *parser, const struct ast_var *var) {
  const char *name = identifier_str(parser, &var->identifier);
  return (struct var_key){name, var->scope};
}

bool var_ty_eq(const struct ir_op_var_ty *l, const struct ir_op_var_ty *r) {
  if (l->ty != r->ty) {
    return false;
  }

  switch (l->ty) {
  case IR_OP_VAR_TY_TY_NONE:
    bug("comparing none makes no sense");
    break;
  case IR_OP_VAR_TY_TY_PRIMITIVE:
    return l->primitive == r->primitive;
  case IR_OP_VAR_TY_TY_POINTER:
    return var_ty_eq(l->pointer.underlying, r->pointer.underlying);
  case IR_OP_VAR_TY_TY_FUNC:
    todo("cmp func tys");
    break;
  }

  unreachable("var_ty_eq");
}

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

struct ir_op_var_ty ty_for_ast_tyref(struct ir_builder *irb,
                                     const struct ast_tyref *ty_ref) {
  switch (ty_ref->ty) {
  case AST_TYREF_TY_UNKNOWN:
    bug("shouldn't reach IR gen with unresolved type");
  case AST_TYREF_TY_VOID:
    return IR_OP_VAR_TY_NONE;
  case AST_TYREF_TY_WELL_KNOWN: {
    struct ir_op_var_ty ty;
    ty.ty = IR_OP_VAR_TY_TY_PRIMITIVE;
    ty.primitive = ty_for_well_known_ty(ty_ref->well_known);
    return ty;
  }
  case AST_TYREF_TY_FUNC: {
    struct ir_op_var_ty ty;
    ty.ty = IR_OP_VAR_TY_TY_FUNC;
    ty.func.ret_ty = arena_alloc(irb->arena, sizeof(*ty.func.ret_ty));
    *ty.func.ret_ty = ty_for_ast_tyref(irb, ty_ref->func.ret_var_ty);

    ty.func.num_params = ty_ref->func.num_param_var_tys;
    ty.func.params =
        arena_alloc(irb->arena, sizeof(struct ir_op) * ty.func.num_params);

    for (size_t i = 0; i < ty.func.num_params; i++) {
      ty.func.params[i] = ty_for_ast_tyref(irb, &ty_ref->func.param_var_tys[i]);
    }

    return ty;
  }
  case AST_TYREF_TY_POINTER: {
    struct ir_op_var_ty *underlying =
        arena_alloc(irb->arena, sizeof(*underlying));
    *underlying = ty_for_ast_tyref(irb, ty_ref->pointer.underlying);

    struct ir_op_var_ty ty;
    ty.ty = IR_OP_VAR_TY_TY_POINTER;
    ty.pointer = (struct ir_op_var_pointer_ty){.underlying = underlying};

    return ty;
  }
  }
}

struct ir_op_var_ty return_ty_for_ast_tyref(struct ir_builder *irb,
                                            const struct ast_tyref *ty_ref) {
  invariant_assert(ty_ref->ty == AST_TYREF_TY_FUNC,
                   "passed non-func to `return_ty_for_ast_tyref`");

  struct ir_op_var_ty func_ty = ty_for_ast_tyref(irb, ty_ref);
  return *func_ty.func.ret_ty;
}

enum ir_op_cast_op_ty cast_ty_for_ast_tyref(struct ir_builder *irb,
                                            const struct ast_tyref *from,
                                            const struct ast_tyref *to) {
  struct ir_op_var_ty from_var_ty = ty_for_ast_tyref(irb, from);
  struct ir_op_var_ty to_var_ty = ty_for_ast_tyref(irb, to);

  if (from_var_ty.ty != IR_OP_VAR_TY_TY_PRIMITIVE ||
      to_var_ty.ty != IR_OP_VAR_TY_TY_PRIMITIVE) {
    todo("casts for non prims");
  }

  if (to_var_ty.primitive < from_var_ty.primitive) {
    return IR_OP_CAST_OP_TY_TRUNCATE;
  } else {
    invariant_assert(from_var_ty.primitive != to_var_ty.primitive,
                     "cast not needed for types of same size");

    if (WKT_IS_SIGNED(from->well_known)) {
      return IR_OP_CAST_OP_TY_SEXT;
    } else {
      return IR_OP_CAST_OP_TY_ZEXT;
    }
  }
}

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_expr *expr,
                                const struct ast_tyref *ast_tyref);

struct ir_op *insert_ir_for_cast(struct ir_builder *irb, struct ir_stmt *stmt,
                                 struct ir_op *op,
                                 const struct ir_op_var_ty *to,
                                 enum ir_op_cast_op_ty ty) {
  struct ir_op *cast = alloc_ir_op(irb, stmt);

  cast->ty = IR_OP_TY_CAST_OP;
  cast->var_ty = *to;
  cast->cast_op.ty = ty;
  cast->cast_op.value = op;

  return cast;
}

struct ir_op *build_ir_for_binaryop(struct ir_builder *irb,
                                    struct ir_stmt *stmt,
                                    struct ast_binary_op *binary_op) {
  struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, &binary_op->var_ty);

  struct ir_op *lhs =
      build_ir_for_expr(irb, stmt, binary_op->lhs, &binary_op->var_ty);
  struct ir_op *rhs =
      build_ir_for_expr(irb, stmt, binary_op->rhs, &binary_op->var_ty);

  struct ir_op *op = alloc_ir_op(irb, stmt);
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = var_ty;

  struct ir_op_binary_op *b = &op->binary_op;

  b->lhs = lhs;
  b->rhs = rhs;

  invariant_assert(binary_op->var_ty.ty == AST_TYREF_TY_WELL_KNOWN,
                   "non primitives (/well-knowns) cannot be used in binary "
                   "expression by point IR is reached!");

  switch (binary_op->ty) {
  case AST_BINARY_OP_TY_EQ:
    b->ty = IR_OP_BINARY_OP_TY_EQ;
    break;
  case AST_BINARY_OP_TY_NEQ:
    b->ty = IR_OP_BINARY_OP_TY_NEQ;
    break;
  case AST_BINARY_OP_TY_GT:
    if (WKT_IS_SIGNED(binary_op->var_ty.well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SGT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UGT;
    }
    break;
  case AST_BINARY_OP_TY_GTEQ:
    if (WKT_IS_SIGNED(binary_op->var_ty.well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SGTEQ;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UGTEQ;
    }
    break;
  case AST_BINARY_OP_TY_LT:
    if (WKT_IS_SIGNED(binary_op->var_ty.well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SLT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_ULT;
    }
    break;
  case AST_BINARY_OP_TY_LTEQ:
    if (WKT_IS_SIGNED(binary_op->var_ty.well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SLTEQ;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_ULTEQ;
    }
    break;
  case AST_BINARY_OP_TY_RSHIFT:
    if (WKT_IS_SIGNED(binary_op->var_ty.well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SRSHIFT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_URSHIFT;
    }
    break;
  case AST_BINARY_OP_TY_LSHIFT:
    b->ty = IR_OP_BINARY_OP_TY_LSHIFT;
    break;
  case AST_BINARY_OP_TY_AND:
    b->ty = IR_OP_BINARY_OP_TY_AND;
    break;
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
  op->var_ty = ty_for_ast_tyref(irb, &cnst->cnst_ty);

  if (cnst->cnst_ty.ty == AST_TYREF_TY_POINTER &&
      cnst->cnst_ty.pointer.underlying->ty == AST_TYREF_TY_WELL_KNOWN) {
    enum well_known_ty wkt = cnst->cnst_ty.pointer.underlying->well_known;
    invariant_assert(wkt == WELL_KNOWN_TY_SIGNED_CHAR ||
                         wkt == WELL_KNOWN_TY_UNSIGNED_CHAR,
                     "expected str type");

    op->cnst.ty = IR_OP_CNST_TY_STR;
    op->cnst.str_value = cnst->str_value;
  } else {
    op->cnst.ty = IR_OP_CNST_TY_INT;
    op->cnst.int_value = cnst->int_value;
  }

  return op;
}

struct ir_op *build_ir_for_compoundexpr(struct ir_builder *irb,
                                        struct ir_stmt *stmt,
                                        struct ast_compoundexpr *compound_expr,
                                        const struct ast_tyref *ast_tyref) {
  struct ir_op *op = NULL;
  for (size_t i = 0; i < compound_expr->num_exprs; i++) {
    op = build_ir_for_expr(irb, stmt, &compound_expr->exprs[i], ast_tyref);
  }

  return op;
}

struct ir_op *build_ir_for_assg(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_assg *assg);

struct ir_op *build_ir_for_var(struct ir_builder *irb, struct ir_stmt *stmt,
                               struct ast_var *var) {
  UNUSED_ARG(stmt);

  // this is when we are _reading_ from the var
  struct var_key key = get_var_key(irb->parser, var);

  struct var_ref *ref = var_refs_get(stmt->basicblock->var_refs, &key);
  if (!ref) {
    ref = var_refs_get(irb->global_var_refs, &key);
  }

  // invariant_assert((ref && ref->op) || !ref, "ref present but empty?");
  struct ir_op *expr = ref && ref->ty == VAR_REF_TY_LCL ? ref->lcl : NULL;

  if (expr) {
    return expr;
  }

  if (ref) {
    // non-local

    switch (ref->ty) {
      case VAR_REF_TY_LCL:
        unreachable("local cannot have come from the global var table");
      case VAR_REF_TY_GLB: {
        struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, &var->var_ty);
        struct ir_op *op = alloc_ir_op(irb, stmt);
        make_sym_ref(irb, key.name, op, &var_ty);

        return op;
      }
      case VAR_REF_TY_ENUM_CNST: {
        struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, &var->var_ty);
        struct ir_op *op = alloc_ir_op(irb, stmt);
        op->ty = IR_OP_TY_CNST;
        op->var_ty = var_ty;
        op->cnst = (struct ir_op_cnst){
          .ty = IR_OP_CNST_TY_INT,
          .int_value = ref->enum_cnst
        };

        return op;
      }
    }
  }

  struct ast_tyref var_tyref = var->var_ty;
  struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, &var_tyref);
  invariant_assert(var_tyref.ty != AST_TYREF_TY_UNKNOWN,
                   "can't have unknown tyref in phi lowering");

  // we generate an empty phi and then after all blocks are built we insert the
  // correct values
  // all phis appear at the start of their bb as they execute ""
  struct ir_op *phi;
  if (stmt->basicblock->first->first) {
    phi = insert_before_ir_op(irb, stmt->basicblock->first->first, IR_OP_TY_PHI,
                              var_ty);
  } else {
    phi = alloc_ir_op(irb, stmt->basicblock->first);
    phi->ty = IR_OP_TY_PHI;
    phi->var_ty = var_ty;
  }

  phi->phi.var = *var;
  phi->phi.values = NULL;
  phi->phi.num_values = 0;

  debug("creating phi %d for name=%s", phi->id,
        identifier_str(irb->parser, &var->identifier));

  key = get_var_key(irb->parser, var);
  struct var_ref *new_ref = var_refs_add(stmt->basicblock->var_refs, &key, VAR_REF_TY_LCL);
  new_ref->ty = VAR_REF_TY_LCL;
  new_ref->lcl = phi;

  return phi;
}

struct ir_op *build_ir_for_atom(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_atom *atom,
                                const struct ast_tyref *ast_tyref) {
  switch (atom->ty) {
  case AST_ATOM_TY_VAR:
    return build_ir_for_var(irb, stmt, &atom->var);
  case AST_ATOM_TY_CNST:
    return build_ir_for_cnst(irb, stmt, &atom->cnst);
  case AST_ATOM_TY_COMPOUNDEXPR:
    return build_ir_for_compoundexpr(irb, stmt, &atom->compound_expr,
                                     ast_tyref);
  }
}

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_expr *expr,
                                const struct ast_tyref *ast_tyref);

struct ir_op *build_ir_for_call(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_call *call,
                                const struct ast_tyref *ast_tyref) {
  // need to generate args and target IR first to keep IR in order

  struct ir_op **args =
      arena_alloc(irb->arena, sizeof(struct ir_op *) * call->arg_list.num_args);
  for (size_t i = 0; i < call->arg_list.num_args; i++) {
    args[i] = build_ir_for_expr(irb, stmt, &call->arg_list.args[i], ast_tyref);
  }

  struct ir_op *target = build_ir_for_atom(irb, stmt, call->target, ast_tyref);

  irb->flags |= IR_BUILDER_FLAG_MAKES_CALL;
  struct ir_op *op = alloc_ir_op(irb, stmt);

  op->ty = IR_OP_TY_CALL;
  op->var_ty = ty_for_ast_tyref(irb, &call->var_ty);
  op->call.target = target;
  op->call.num_args = call->arg_list.num_args;
  op->call.args = args;

  return op;
}

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_expr *expr,
                                const struct ast_tyref *ast_tyref) {
  struct ir_op *op;
  switch (expr->ty) {
  case AST_EXPR_TY_ATOM:
    op = build_ir_for_atom(irb, stmt, &expr->atom, ast_tyref);
    break;
  case AST_EXPR_TY_CALL:
    op = build_ir_for_call(irb, stmt, expr->call, ast_tyref);
    break;
  case AST_EXPR_TY_BINARY_OP:
    op = build_ir_for_binaryop(irb, stmt, &expr->binary_op);
    break;
  case AST_EXPR_TY_ASSG:
    op = build_ir_for_assg(irb, stmt, expr->assg);
    break;
  }

  if (ast_tyref) {
    struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, ast_tyref);

    if (!var_ty_eq(&op->var_ty, &var_ty)) {
      op = insert_ir_for_cast(
          irb, stmt, op, &var_ty,
          cast_ty_for_ast_tyref(irb, &expr->var_ty, ast_tyref));
    }
  }

  invariant_assert(op, "null op!");
  return op;
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
  struct ir_op *cond = build_ir_for_expr(irb, cond_stmt, &if_stmt->condition,
                                         &if_stmt->condition.var_ty);
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
      build_ir_for_expr(irb, cond_stmt, &if_else_stmt->condition,
                        &if_else_stmt->condition.var_ty);
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

  make_basicblock_merge(irb, before_cond_basicblock, cond_basicblock);

  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb, cond_basicblock);
  struct ir_op *cond = build_ir_for_expr(irb, cond_stmt, &while_stmt->cond,
                                         &while_stmt->cond.var_ty);
  struct ir_op *cond_br = alloc_ir_op(irb, cond_stmt);
  cond_br->ty = IR_OP_TY_BR_COND;
  cond_br->var_ty = IR_OP_VAR_TY_NONE;
  cond_br->br_cond.cond = cond;

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, while_stmt->body);
  UNUSED_ARG(body_stmt_basicblock);
  // debug_assert(body_stmt_basicblock == body_basicblock, "stmt in wrong bb
  // (while)");

  struct ir_basicblock *after_body_basicblock = alloc_ir_basicblock(irb);
  make_basicblock_split(irb, cond_basicblock, body_basicblock,
                        after_body_basicblock);

  struct ir_op *pre_cond_br = alloc_ir_op(irb, before_cond_basicblock->last);
  pre_cond_br->ty = IR_OP_TY_BR;
  pre_cond_br->var_ty = IR_OP_VAR_TY_NONE;

  make_basicblock_merge(irb, body_stmt_basicblock, cond_basicblock);
  // struct ir_stmt *br_stmt = alloc_ir_stmt(irb, body_basicblock);
  struct ir_stmt *br_stmt = alloc_ir_stmt(irb, body_stmt_basicblock);
  struct ir_op *br = alloc_ir_op(irb, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_OP_VAR_TY_NONE;

  // make_basicblock_merge(irb, body_basicblock, cond_basicblock);
  debug("body %zu, body_stmt %zu, after %zu\n", body_basicblock->id,
        body_stmt_basicblock->id, after_body_basicblock->id);

  return after_body_basicblock;
}

struct ir_basicblock *
build_ir_for_dowhilestmt(struct ir_builder *irb,
                         struct ir_basicblock *basicblock,
                         struct ast_dowhilestmt *do_while_stmt) {
  struct ir_basicblock *before_body_basicblock = basicblock;
  struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb);
  struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb);

  make_basicblock_merge(irb, before_body_basicblock, body_basicblock);

  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb, cond_basicblock);
  struct ir_op *cond = build_ir_for_expr(irb, cond_stmt, &do_while_stmt->cond,
                                         &do_while_stmt->cond.var_ty);
  struct ir_op *cond_br = alloc_ir_op(irb, cond_stmt);
  cond_br->ty = IR_OP_TY_BR_COND;
  cond_br->var_ty = IR_OP_VAR_TY_NONE;
  cond_br->br_cond.cond = cond;

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, do_while_stmt->body);
  // debug_assert(body_stmt_basicblock == body_basicblock, "stmt in wrong bb
  // (do-while)");

  struct ir_basicblock *after_cond_basicblock = alloc_ir_basicblock(irb);
  make_basicblock_split(irb, cond_basicblock, body_basicblock,
                        after_cond_basicblock);

  struct ir_op *pre_body_br = alloc_ir_op(irb, before_body_basicblock->last);
  pre_body_br->ty = IR_OP_TY_BR;
  pre_body_br->var_ty = IR_OP_VAR_TY_NONE;

  make_basicblock_merge(irb, body_stmt_basicblock, cond_basicblock);
  // struct ir_stmt *br_stmt = alloc_ir_stmt(irb, body_basicblock);
  struct ir_stmt *br_stmt = alloc_ir_stmt(irb, body_stmt_basicblock);
  struct ir_op *br = alloc_ir_op(irb, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_OP_VAR_TY_NONE;

  return after_cond_basicblock;
}

struct ir_op *build_ir_for_vardecllist(struct ir_builder *irb,
                                       struct ir_stmt *stmt,
                                       struct ast_vardecllist *var_decl_list);

struct ir_op *build_ir_for_declorexpr(struct ir_builder *irb,
                                      struct ir_stmt *stmt,
                                      struct ast_declorexpr *decl_or_expr) {
  if (decl_or_expr->decl) {
    return build_ir_for_vardecllist(irb, stmt, decl_or_expr->decl);
  } else if (decl_or_expr->expr) {
    return build_ir_for_expr(irb, stmt, decl_or_expr->expr,
                             &decl_or_expr->expr->var_ty);
  }

  return NULL;
}

struct ir_basicblock *build_ir_for_forstmt(struct ir_builder *irb,
                                           struct ir_basicblock *basicblock,
                                           struct ast_forstmt *for_stmt) {

  struct ir_basicblock *before_cond_basicblock = basicblock;
  struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb);
  struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb);

  make_basicblock_merge(irb, before_cond_basicblock, cond_basicblock);

  if (for_stmt->init) {
    struct ir_stmt *init_stmt = alloc_ir_stmt(irb, before_cond_basicblock);
    build_ir_for_declorexpr(irb, init_stmt, for_stmt->init);
  }

  invariant_assert(for_stmt->cond, "for stmt without cond not yet supported");

  if (for_stmt->cond) {
    struct ir_stmt *cond_stmt = alloc_ir_stmt(irb, cond_basicblock);
    struct ir_op *cond = build_ir_for_expr(irb, cond_stmt, for_stmt->cond,
                                           &for_stmt->cond->var_ty);
    struct ir_op *cond_br = alloc_ir_op(irb, cond_stmt);
    cond_br->ty = IR_OP_TY_BR_COND;
    cond_br->var_ty = IR_OP_VAR_TY_NONE;
    cond_br->br_cond.cond = cond;
  }

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, for_stmt->body);

  if (for_stmt->iter) {
    invariant_assert(
        body_basicblock->last,
        "attempting to add `for` loop iter without stmt present, needs fixing");
    build_ir_for_expr(irb, body_basicblock->last, for_stmt->iter,
                      &for_stmt->iter->var_ty);
  }

  struct ir_basicblock *after_body_basicblock = alloc_ir_basicblock(irb);
  make_basicblock_split(irb, cond_basicblock, body_basicblock,
                        after_body_basicblock);
  // debug_assert(body_stmt_basicblock == body_basicblock,
  //              "stmt in wrong bb (for)");

  struct ir_op *pre_cond_br = alloc_ir_op(irb, before_cond_basicblock->last);
  pre_cond_br->ty = IR_OP_TY_BR;
  pre_cond_br->var_ty = IR_OP_VAR_TY_NONE;

  make_basicblock_merge(irb, body_stmt_basicblock, cond_basicblock);
  // struct ir_stmt *br_stmt = alloc_ir_stmt(irb, body_basicblock);
  struct ir_stmt *br_stmt = alloc_ir_stmt(irb, body_stmt_basicblock);
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

/* Return stmt be null when this is used to add implicit returns not in code
 * (e.g at end of method) */
struct ir_basicblock *build_ir_for_ret(struct ir_builder *irb,
                                       struct ir_stmt *stmt,
                                       struct ast_returnstmt *return_stmt) {
  struct ir_op *expr_op;
  if (return_stmt && return_stmt->expr) {
    expr_op =
        build_ir_for_expr(irb, stmt, return_stmt->expr, &return_stmt->var_ty);
  } else {
    expr_op = NULL;
  }

  struct ir_op *op = alloc_ir_op(irb, stmt);
  op->ty = IR_OP_TY_RET;
  op->var_ty = return_stmt ? ty_for_ast_tyref(irb, &return_stmt->var_ty)
                           : IR_OP_VAR_TY_NONE;
  op->ret.value = expr_op;

  op->stmt->basicblock->ty = IR_BASICBLOCK_TY_RET;

  struct ir_basicblock *after_ret_basicblock = alloc_ir_basicblock(irb);

  return after_ret_basicblock;
}

struct ir_basicblock *build_ir_for_jumpstmt(struct ir_builder *irb,
                                            struct ir_stmt *stmt,
                                            struct ast_jumpstmt *jump_stmt) {
  switch (jump_stmt->ty) {
  case AST_JUMPSTMT_TY_RETURN:
    return build_ir_for_ret(irb, stmt, &jump_stmt->return_stmt);
  }
}

struct ir_op *var_assg(struct ir_builder *irb, struct ir_stmt *stmt,
                       struct ast_tyref *var_ty, struct ast_var *var,
                       struct ast_expr *expr) {
  struct ir_op *op = build_ir_for_expr(irb, stmt, expr, var_ty);
  debug_assert(op, "null expr in assignment!");

  struct var_key key = get_var_key(irb->parser, var);
  struct var_ref *ref = var_refs_get(stmt->basicblock->var_refs, &key);

  if (!ref) {
    ref = var_refs_add(stmt->basicblock->var_refs, &key, VAR_REF_TY_LCL);
  }

  if (ref->ty == VAR_REF_TY_LCL) {
    ref->lcl = op;
  } else {
    unreachable("assignment to global not yet suppported");
  }

  return op;
}

struct ir_op *build_ir_for_assg(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_assg *assg) {
  if (assg->assignee->ty == AST_EXPR_TY_ATOM &&
      assg->assignee->atom.ty == AST_ATOM_TY_VAR) {
    return var_assg(irb, stmt, &assg->assignee->var_ty,
                    &assg->assignee->atom.var, assg->expr);
  } else {
    todo("non var assignments");
  }
}

struct ir_op *build_ir_for_vardecllist(struct ir_builder *irb,
                                       struct ir_stmt *stmt,
                                       struct ast_vardecllist *var_decl_list) {
  for (size_t i = 0; i < var_decl_list->num_decls; i++) {
    struct ast_vardecl *decl = &var_decl_list->decls[i];

    if (decl->ty == AST_VARDECL_TY_DECL) {
      continue;
    }

    // FIXME: is this right
    var_assg(irb, stmt, &var_decl_list->var_ty, &decl->var, &decl->assg_expr);
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
    build_ir_for_expr(irb, ir_stmt, &stmt->expr, NULL);
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

// FIXME: pointer size!
size_t var_ty_size(struct ir_builder *irb, struct ir_op_var_ty *ty) {
  UNUSED_ARG(irb);

  switch (ty->ty) {
  case IR_OP_VAR_TY_TY_NONE:
    bug("IR_OP_VAR_TY_TY_NONE has no size");
  case IR_OP_VAR_TY_TY_FUNC:
    return 8;
  case IR_OP_VAR_TY_TY_POINTER:
    return 8;
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
                     struct ir_op *source_phi, struct ast_var *var,
                     struct ir_basicblock *basicblock, struct ir_op ***exprs,
                     size_t *num_exprs) {
  if (!basicblock || basicblocks_visited[basicblock->id]) {
    return;
  }

  basicblocks_visited[basicblock->id] = true;

  struct var_key key = get_var_key(irb->parser, var);
  struct var_ref *ref = var_refs_get(basicblock->var_refs, &key);

  if (!ref) {
    debug("bb %zu has %zu preds", basicblock->id, basicblock->num_preds);
    for (size_t i = 0; i < basicblock->num_preds; i++) {
      walk_basicblock(irb, basicblocks_visited, source_phi, var,
                      basicblock->preds[i], exprs, num_exprs);
    }
    return;
  }

  switch(ref->ty) {
    case VAR_REF_TY_GLB:
    case VAR_REF_TY_ENUM_CNST:
      unreachable("non-lcl var refs should already be handled and not generate phis");
    case VAR_REF_TY_LCL: {
      struct ir_op *entry_op = ref->lcl;
      // FIXME: this phi simplification is buggy and breaks `tests/do_while.c`
      if (false && entry_op->ty == IR_OP_TY_PHI && entry_op != source_phi) {
        // copy over the entries from that phi, to prevent deeply nested ones
        // which are confusing and also bad for codegen

        // FIXME: this is O(n^2), we need some sort of lookup instead
        for (size_t i = 0; i < entry_op->phi.num_values; i++) {
          struct ir_op *phi = entry_op->phi.values[i];

          bool seen = false;
          for (size_t j = 0; j < *num_exprs; j++) {
            if ((*exprs)[j]->id == phi->id) {
              seen = true;
              break;
            }
          }

          if (!seen) {
            (*num_exprs)++;
            *exprs = arena_realloc(irb->arena, *exprs,
                                   sizeof(struct ir_op *) * *num_exprs);
            (*exprs)[*num_exprs - 1] = phi;
          }
        }
      } else {
        (*num_exprs)++;
        *exprs = arena_realloc(irb->arena, *exprs,
                               sizeof(struct ir_op *) * *num_exprs);
        (*exprs)[*num_exprs - 1] = ref->lcl;
      }
    }
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
    struct ir_basicblock *pred = phi->stmt->basicblock->preds[i];
    walk_basicblock(irb, basicblocks_visited, phi, &phi->phi.var, pred, &exprs,
                    &num_exprs);
  }

  if (num_exprs && (exprs[0]->flags & IR_OP_FLAG_PARAM)) {
    return;
  }

  if (!num_exprs) {
    err("undefined behaviour - reading from unassigned variable '%s'",
        identifier_str(irb->parser, &phi->phi.var.identifier));
    return;
    phi->var_ty = IR_OP_VAR_TY_NONE;
    phi->phi.values = NULL;
    phi->phi.num_values = 0;
  }

  phi->var_ty = exprs[0]->var_ty;
  phi->phi.values = exprs;
  phi->phi.num_values = num_exprs;
}

void validate_op_tys_callback(struct ir_op **op, void *cb_metadata) {
  struct ir_op *consumer = cb_metadata;

  // TODO: validate CALL ops as well
  if (consumer->ty != IR_OP_TY_CAST_OP && consumer->ty != IR_OP_TY_CALL &&
      op_produces_value(consumer->ty)) {
    invariant_assert(var_ty_eq(&(*op)->var_ty, &consumer->var_ty),
                     "op %zu uses op %zu with different type!", consumer->id,
                     (*op)->id);
  }
}

struct ir_builder *build_ir_for_function(struct parser *parser,
                                         struct arena_allocator *arena,
                                         struct ast_funcdef *def,
                                         struct var_refs *global_var_refs) {
  struct ir_builder b = {.name = identifier_str(parser, &def->sig.name),
                         .parser = parser,
                         .global_var_refs = global_var_refs,
                         .global_refs = NULL,
                         .strings = NULL,
                         .arena = arena,
                         .flags = IR_BUILDER_FLAG_NONE,
                         .first = NULL,
                         .last = NULL,
                         .op_count = 0,
                         .nonvolatile_registers_used = 0,
                         .num_locals = 0,
                         .total_locals_size = 0,
                         .num_call_saves = 0,
                         .total_call_saves_size = 0,
                         .offset = 0};

  struct ir_builder *builder = arena_alloc(arena, sizeof(b));
  *builder = b;

  struct ir_op_var_ty ty = ty_for_ast_tyref(builder, &def->sig.var_ty);
  builder->func_ty = ty.func;

  // needs at least one initial basic block
  alloc_ir_basicblock(builder);
  struct ir_basicblock *basicblock = builder->first;
  struct ir_stmt *param_stmt = alloc_ir_stmt(builder, basicblock);

  // first statement is a bunch of magic MOV commands that explain to the rest
  // of the IR that these are params this is encoded as MOV NULL with the
  // IR_OP_FLAG_PARAM flag
  for (size_t i = 0; i < def->sig.param_list.num_params; i++) {
    const struct ast_param *param = &def->sig.param_list.params[i];

    struct var_key key = get_var_key(builder->parser, &param->var);
    struct var_ref *ref = var_refs_add(basicblock->var_refs, &key, VAR_REF_TY_LCL);

    struct ir_op *mov = alloc_ir_op(builder, param_stmt);
    mov->ty = IR_OP_TY_MOV;
    mov->var_ty = ty_for_ast_tyref(builder, &param->var_ty);
    mov->flags |= IR_OP_FLAG_PARAM;
    mov->mov.value = NULL;

    ref->lcl = mov;
  }

  for (size_t i = 0; i < def->body.num_stmts; i++) {
    basicblock = build_ir_for_stmt(builder, basicblock, &def->body.stmts[i]);
  }

  debug_print_ir(stderr, builder, NULL, NULL);

  // we may generate empty basicblocks or statements, prune them here
  prune_basicblocks(builder);

  // may not end in a return, but needs to to be well-formed IR
  struct ir_basicblock *last_bb = builder->last;
  if (!last_bb) {
    last_bb = alloc_ir_basicblock(builder);
  }

  struct ir_stmt *last_stmt = last_bb->last;
  if (!last_stmt) {
    last_stmt = alloc_ir_stmt(builder, last_bb);
  }
  
  struct ir_op *last_op = last_stmt->last;

  if (!last_op || last_op->ty != IR_OP_TY_RET) {
    struct ir_op *return_value = NULL;
    
    if (strcmp(builder->name, "main") == 0) {
      debug("adding implicit return 0");

      struct ir_op *cnst = alloc_ir_op(builder, last_stmt);
      cnst->ty = IR_OP_TY_CNST;
      cnst->var_ty = (struct ir_op_var_ty){
          .ty = IR_OP_VAR_TY_TY_PRIMITIVE,
          .primitive = IR_OP_VAR_PRIMITIVE_TY_I32,
      };
      cnst->cnst.int_value = 0;

      return_value = cnst;
    }

    basicblock = build_ir_for_ret(builder, last_stmt, NULL);
    debug_assert(last_stmt->last->ty == IR_OP_TY_RET, "expected ret after call to build ret");
    last_stmt->last->ret.value = return_value;
  }

  // do we need to prune again? i do not think so

  if (log_enabled()) {
    debug_print_ir(stderr, builder, NULL, NULL);
  }

  // now we fix up phis
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

  basicblock = builder->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        walk_op_uses(op, validate_op_tys_callback, op);

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  return builder;
}

struct ir_unit *build_ir_for_translationunit(
    /* needed for `associated_text */ struct parser *parser,
    struct arena_allocator *arena,
    struct ast_translationunit *translation_unit) {

  struct ir_unit u = {
      .funcs = arena_alloc(arena, sizeof(struct ir_builder *) *
                                      translation_unit->num_func_defs),
      .num_funcs = translation_unit->num_func_defs,
  };
  struct ir_unit *iru = arena_alloc(arena, sizeof(*iru));
  *iru = u;

  struct var_refs *global_var_refs = var_refs_create(NULL);
  // funcs do not necessarily have a seperate decl so we do it for defs too

  for (size_t i = 0; i < translation_unit->num_func_decls; i++) {
    struct ast_funcdecl *decl = &translation_unit->func_decls[i];
    struct var_key key = {.name = identifier_str(parser, &decl->sig.name),
                          .scope = SCOPE_GLOBAL};
    struct var_ref *ref = var_refs_add(global_var_refs, &key, VAR_REF_TY_GLB);
    UNUSED_ARG(ref);
  }

  for (size_t i = 0; i < translation_unit->num_func_defs; i++) {
    struct ast_funcdef *def = &translation_unit->func_defs[i];
    struct var_key key = {.name = identifier_str(parser, &def->sig.name),
                          .scope = SCOPE_GLOBAL};
    struct var_ref *ref = var_refs_add(global_var_refs, &key, VAR_REF_TY_GLB);
    UNUSED_ARG(ref);
  }

  for (size_t i = 0; i < translation_unit->num_enum_defs; i++) {
    struct ast_enumdef *def = &translation_unit->enum_defs[i];

    unsigned long long enum_value = 0;
    for (size_t j = 0; j < def->num_enum_cnsts; j++) {
      struct ast_enumcnst *enum_cnst = &def->enum_cnsts[j];

      if (enum_cnst->ty == AST_ENUMCNST_TY_EXPLICIT_VALUE) {
        enum_value = enum_cnst->value;
      }

      struct var_key key = {.name = identifier_str(parser, &enum_cnst->identifier),
                            .scope = SCOPE_GLOBAL};
      struct var_ref *ref = var_refs_add(global_var_refs, &key, VAR_REF_TY_ENUM_CNST);
      ref->enum_cnst = enum_value;

      // increment for the next implicit value
      enum_value++;
    }
  }
  

  for (size_t i = 0; i < translation_unit->num_func_defs; i++) {
    struct ast_funcdef *def = &translation_unit->func_defs[i];
    struct var_key key = {.name = identifier_str(parser, &def->sig.name),
                          .scope = SCOPE_GLOBAL};

    struct ir_builder *func =
        build_ir_for_function(parser, arena, def, global_var_refs);

    var_refs_get(global_var_refs, &key)->func = func;

    iru->funcs[i] = func;
  }

  return iru;
}
