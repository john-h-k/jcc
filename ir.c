#include "ir.h"
#include "alloc.h"
#include "compiler.h"
#include "lex.h"
#include "parse.h"
#include "util.h"
#include "vector.h"
#include <math.h>

void walk_store_lcl(struct ir_op_store_lcl *store_lcl, walk_op_callback *cb,
                    void *cb_metadata) {
  UNUSED_ARG(store_lcl);
  UNUSED_ARG(cb);
  UNUSED_ARG(cb_metadata);
  // nada
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
  case IR_OP_TY_PHI:
    todo("walk phi");
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

const struct ir_op_var_ty IR_OP_VAR_TY_NONE = { .ty = IR_OP_VAR_TY_TY_NONE };

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
}

struct ir_op *insert_before_ir_op(struct ir_builder *irb,
                                  struct ir_op *insert_before, enum ir_op_ty ty,
                                  struct ir_op_var_ty var_ty) {
  debug_assert(insert_before, "invalid insertion point!");

  struct ir_op *op = alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->op_count++, ty, var_ty, insert_before->pred,
                   insert_before, insert_before->stmt, NO_REG, NO_LCL);

  if (op->pred) {
    op->pred->succ = op;
  } else {
    op->stmt->first = op;
  }

  insert_before->pred = op;

  return op;
}

struct ir_op *insert_after_ir_op(struct ir_builder *irb,
                                 struct ir_op *insert_after, enum ir_op_ty ty,
                                 struct ir_op_var_ty var_ty) {
  debug_assert(insert_after, "invalid insertion point!");

  struct ir_op *op = alloc(irb->arena, sizeof(*op));

  initialise_ir_op(op, irb->op_count++, ty, var_ty, insert_after,
                   insert_after->succ, insert_after->stmt, NO_REG, NO_LCL);

  if (op->succ) {
    op->succ->pred = op;
  } else {
    op->stmt->last = op;
  }

  insert_after->succ = op;

  return op;
}

// TODO: this should call `initialise_ir_op`
struct ir_op *alloc_ir_op(struct ir_builder *irb, struct ir_stmt *stmt) {
  struct ir_op *op = alloc(irb->arena, sizeof(*op));

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

struct ir_stmt *alloc_ir_stmt(struct ir_builder *irb) {
  struct ir_stmt *stmt = alloc(irb->arena, sizeof(*stmt));

  if (!irb->first) {
    irb->first = stmt;
  }

  stmt->id = irb->stmt_count++;
  stmt->pred = irb->last;
  stmt->succ = NULL;

  if (irb->last) {
    irb->last->succ = stmt;
  }

  irb->last = stmt;

  return stmt;
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
        get_or_create_entry(&irb->var_table, &assg->lvalue.var);

    entry->value = build_ir_for_expr(irb, stmt, assg->expr);
    return entry->value;
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

struct ir_op *build_ir_for_lvalue(struct ir_builder *irb, struct ir_stmt *stmt,
                                  struct ast_lvalue *lvalue) {
  switch (lvalue->ty) {
  case AST_LVALUE_TY_VAR: {
    UNUSED_ARG(stmt);
    // this is when we are _reading_ from the var
    struct var_table_entry *entry =
        get_or_create_entry(&irb->var_table, &lvalue->var);

    if (!entry->value) {
      err("undefined behaviour - reading from unassigned variable '%s'",
          entry->name);
    }

    return entry->value;
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

struct ir_stmt *build_ir_for_stmt(struct ir_builder *irb,
                                  struct ast_stmt *stmt);

void build_ir_for_compoundstmt(struct ir_builder *irb,
                               struct ast_compoundstmt *compound_stmt) {
  for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
    build_ir_for_stmt(irb, &compound_stmt->stmts[i]);
  }
}

struct ir_op *build_ir_for_jumpstmt(struct ir_builder *irb,
                                    struct ir_stmt *stmt,
                                    struct ast_jumpstmt *jump_stmt) {
  switch (jump_stmt->ty) {
  case AST_JUMPSTMT_TY_RETURN: {
    struct ir_op *expr_op = build_ir_for_expr(irb, stmt, &jump_stmt->ret_expr);

    struct ir_op *op = alloc_ir_op(irb, stmt);
    op->ty = IR_OP_TY_RET;
    op->ret.value = expr_op;
    return op;
  }
  }
}

struct ir_op *build_ir_for_vardecllist(struct ir_builder *irb,
                                       struct ir_stmt *stmt,
                                       struct ast_vardecllist *var_decl_list) {
  for (size_t i = 0; i < var_decl_list->num_decls; i++) {
    struct ast_vardecl *decl = &var_decl_list->decls[i];

    // a decl is _always_ a new entry (it may shadow)
    struct var_table_entry *entry = create_entry(&irb->var_table, &decl->var);

    if (decl->ty == AST_VARDECL_TY_DECL) {
      continue;
    }

    entry->value = build_ir_for_expr(irb, stmt, &decl->assg_expr);
  }

  return stmt->last;
}

struct ir_stmt *build_ir_for_stmt(struct ir_builder *irb,
                                  struct ast_stmt *stmt) {
  struct ir_stmt *ir_stmt = alloc_ir_stmt(irb);

  switch (stmt->ty) {
  case AST_STMT_TY_VAR_DECL_LIST: {
    build_ir_for_vardecllist(irb, ir_stmt, &stmt->var_decl_list);
    break;
  }
  case AST_STMT_TY_EXPR: {
    build_ir_for_expr(irb, ir_stmt, &stmt->expr);
    break;
  }
  case AST_STMT_TY_COMPOUND: {
    build_ir_for_compoundstmt(irb, &stmt->compound);
    break;
  }
  case AST_STMT_TY_JUMP: {
    build_ir_for_jumpstmt(irb, ir_stmt, &stmt->jump);
    break;
  }
  case AST_STMT_TY_SELECT: {
    todo("IR for selects");
    break;
  }
  }

  return ir_stmt;
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

struct ir_builder *build_ir_for_function(struct parser *parser,
                                         struct arena_allocator *arena,
                                         struct ast_funcdef *def) {
  struct ir_builder b = {.name = identifier_str(parser, &def->sig.name),
                         .parser = parser,
                         .arena = arena,
                         .var_table = var_table_create(parser),
                         .first = NULL,
                         .last = NULL,
                         .op_count = 0,
                         .num_locals = 0,
                         .total_locals_size = 0};

  struct ir_builder *builder = alloc(arena, sizeof(b));
  *builder = b;

  for (size_t i = 0; i < def->body.num_stmts; i++) {
    build_ir_for_stmt(builder, &def->body.stmts[i]);
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

void debug_print_op(struct ir_op *ir) {
  switch (ir->ty) {
  case IR_OP_TY_PHI:
    todo("debug PHI");
    break;
  case IR_OP_TY_CNST:
    fprintf(stderr, "%%%zu (%s) = %zu", ir->id, var_ty_string(&ir->var_ty),
            ir->cnst.value);
    break;
  case IR_OP_TY_BINARY_OP:
    fprintf(stderr, "%%%zu (%s) = %%%zu %s %%%zu", ir->id,
            var_ty_string(&ir->var_ty), ir->binary_op.lhs->id,
            binary_op_string(ir->binary_op.ty), ir->binary_op.rhs->id);
    break;
  case IR_OP_TY_STORE_LCL:
    fprintf(stderr, "%%%zu (%s) = storelcl LCL(%zu), %%%zu",
            ir->id, var_ty_string(&ir->var_ty), ir->store_lcl.lcl_idx,
            ir->store_lcl.value->id);
    break;
  case IR_OP_TY_LOAD_LCL:
    fprintf(stderr, "%%%zu = loadlcl (%s) LCL(%zu)", ir->id,
            var_ty_string(&ir->var_ty), ir->load_lcl.lcl_idx);
    break;
  case IR_OP_TY_RET:
    fprintf(stderr, "return %%%zu", ir->ret.value->id);
    break;
  }
}

void debug_print_ir(struct ir_builder *irb, struct ir_stmt *stmt,
                    debug_print_op_callback *cb, void *cb_metadata) {
  debug("%zu statements", irb->stmt_count);

  int ctr_pad = (int)log10(irb->op_count) + 1;
  size_t ctr = 0;
  while (stmt) {
    struct ir_op *ir = stmt->first;

    int op_pad = /* guess */ 50;

    while (ir) {
      fprintf(stderr, "%0*zu: ", ctr_pad, ctr++);

      long pos = ftell(stderr);
      debug_print_op(ir);
      long width = ftell(stderr) - pos;
      long pad = op_pad - width;

      if (pad > 0) {
        fprintf(stderr, "%*s", (int)pad, "");
      }

      if (cb) {
        fprintf(stderr, " | ");
        cb(stderr, ir, cb_metadata);
      }
      fprintf(stderr, "\n");

      ir = ir->succ;
    }

    stmt = stmt->succ;
  }
}
