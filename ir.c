#include "ir.h"
#include "alloc.h"
#include "compiler.h"
#include "lex.h"
#include "parse.h"
#include "util.h"
#include "var_table.h"
#include "vector.h"

struct ir_builder {
  struct parser *parser;
  struct arena_allocator *arena;

  // `value` contains a `struct ir_op *` to the last op that wrote to this
  // variable or NULL if it is not yet written to
  struct var_table var_table;

  struct ir_op *first;
  struct ir_op *last;

  size_t last_id;
};

struct ir_op *alloc_ir_op(struct ir_builder *irb) {
  struct ir_op *op = alloc(irb->arena, sizeof(struct ir_op));

  if (!irb->first) {
    irb->first = op;
    irb->last = op;
  }

  op->id = irb->last_id++;
  op->pred = irb->last;
  irb->last->succ = op;
  irb->last = op;

  return op;
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

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ast_expr *expr);

struct ir_op *build_ir_for_binaryop(struct ir_builder *irb,
                                     struct ast_binaryop *binary_op) {
  struct ir_op *lhs = build_ir_for_expr(irb, binary_op->lhs);
  struct ir_op *rhs = build_ir_for_expr(irb, binary_op->rhs);

  struct ir_op *op = alloc_ir_op(irb);
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = ty_for_ast_tyref(&binary_op->var_ty);

  struct ir_op_binary_op *b = &op->binary_op;

  b->lhs = lhs;
  b->rhs = rhs;

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
    invariant_assert(binary_op->var_ty.ty == AST_TYREF_TY_WELL_KNOWN,
                     "non primitives (/well-knowns) cannot be used in binary "
                     "expression by point IR is reached!");
    if (WKT_IS_SIGNED(binary_op->var_ty.well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SDIV;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UDIV;
    }
    break;
  case AST_BINARY_OP_TY_QUOT:
    b->ty = IR_OP_BINARY_OP_TY_QUOT;
    break;
  }

  return op;
}

struct ir_op *build_ir_for_cnst(struct ir_builder *irb, struct ast_cnst *cnst) {
  struct ir_op *op = alloc_ir_op(irb);

  op->ty = IR_OP_TY_CNST;
  op->var_ty.ty = IR_OP_VAR_TY_TY_PRIMITIVE;
  op->var_ty.primitive = ty_for_well_known_ty(cnst->cnst_ty);
  op->cnst.value = cnst->value;

  return op;
}

struct ir_op *build_ir_for_assg(struct ir_builder *irb, struct ast_assg *assg) {
  switch (assg->lvalue.ty) {
  case AST_LVALUE_TY_VAR: {
    struct var_table_entry *entry =
        get_or_create_entry(&irb->var_table, &assg->lvalue.var);

    entry->value = build_ir_for_expr(irb, assg->expr);
    return entry->value;
  }
  }
}

struct ir_op *build_ir_for_compoundexpr(struct ir_builder *irb,
                                  struct ast_compoundexpr *compound_expr) {
  for (size_t i = 0; i < compound_expr->num_exprs; i++) {
    build_ir_for_expr(irb, &compound_expr->exprs[i]);
  }

  return irb->last;
}

struct ir_op *build_ir_for_rvalue(struct ir_builder *irb,
                                  struct ast_rvalue *rvalue) {
  switch (rvalue->ty) {
  case AST_RVALUE_TY_CNST:
    return build_ir_for_cnst(irb, &rvalue->cnst);
  case AST_RVALUE_TY_ASSG:
    return build_ir_for_assg(irb, rvalue->assg);
  case AST_RVALUE_TY_BINARY_OP:
    return build_ir_for_binaryop(irb, &rvalue->binary_op);
  case AST_RVALUE_TY_COMPOUNDEXPR:
    return build_ir_for_compoundexpr(irb, &rvalue->compound_expr);
  }
}

struct ir_op *build_ir_for_lvalue(struct ir_builder *irb,
                                  struct ast_lvalue *lvalue) {
  switch (lvalue->ty) {
  case AST_LVALUE_TY_VAR: {
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

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ast_expr *expr) {
  switch (expr->ty) {
  case AST_EXPR_TY_RVALUE:
    return build_ir_for_rvalue(irb, &expr->rvalue);
  case AST_EXPR_TY_LVALUE:
    return build_ir_for_lvalue(irb, &expr->lvalue);
  }
}

struct ir_op *build_ir_for_stmt(struct ir_builder *irb, struct ast_stmt *stmt);

struct ir_op *
build_ir_for_compoundstmt(struct ir_builder *irb,
                          struct ast_compoundstmt *compound_stmt) {
  struct ir_op *last;
  for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
    last = build_ir_for_stmt(irb, &compound_stmt->stmts[i]);
  }

  return last ? last : irb->last;
}

struct ir_op *build_ir_for_jumpstmt(struct ir_builder *irb,
                                    struct ast_jumpstmt *jump_stmt) {
  switch (jump_stmt->ty) {
  case AST_JUMPSTMT_TY_RETURN: {
    struct ir_op *expr_op = build_ir_for_expr(irb, &jump_stmt->ret_expr);

    struct ir_op *op = alloc_ir_op(irb);
    op->ty = IR_OP_TY_RET;
    op->ret.value = expr_op;
    return op;
  }
  }
}

struct ir_op *build_ir_for_vardecllist(struct ir_builder *irb,
                                       struct ast_vardecllist *var_decl_list) {
  for (size_t i = 0; i < var_decl_list->num_decls; i++) {
    struct ast_vardecl *decl = &var_decl_list->decls[i];

    // a decl is _always_ a new entry (it may shadow)
    struct var_table_entry *entry = create_entry(&irb->var_table, &decl->var);

    if (decl->ty == AST_VARDECL_TY_DECL) {
      continue;
    }

    entry->value = build_ir_for_expr(irb, &decl->assg_expr);
  }

  return irb->last;
}

struct ir_op *build_ir_for_stmt(struct ir_builder *irb, struct ast_stmt *stmt) {
  switch (stmt->ty) {
  case AST_STMT_TY_VAR_DECL_LIST: {
    return build_ir_for_vardecllist(irb, &stmt->var_decl_list);
  }
  case AST_STMT_TY_EXPR: {
    return build_ir_for_expr(irb, &stmt->expr);
  }
  case AST_STMT_TY_COMPOUND: {
    return build_ir_for_compoundstmt(irb, &stmt->compound);
  }
  case AST_STMT_TY_JUMP: {
    return build_ir_for_jumpstmt(irb, &stmt->jump);
  }
  }
}

struct ir_function build_ir_for_function(struct parser *parser,
                                         struct arena_allocator *arena,
                                         struct ast_funcdef *def) {
  struct ir_builder builder = {.parser = parser,
                               .arena = arena,
                               .var_table = var_table_create(parser),
                               .first = NULL,
                               .last = NULL,
                               .last_id = 0};

  for (size_t i = 0; i < def->body.num_stmts; i++) {
    build_ir_for_stmt(&builder, &def->body.stmts[i]);
  }

  struct ir_function func = {
      .name = identifier_str(parser, &def->sig.name),
      .start = builder.first,
      .end = builder.last,
      .op_count = builder.last ? builder.last->id : 0 // i think?
  };

  return func;
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
  case IR_OP_BINARY_OP_TY_UDIV:
    return "/";
  case IR_OP_BINARY_OP_TY_QUOT:
    return "%";
  default:
    return "?unknown?";
  }
}

const char *var_ty_string(const struct ir_op_var_ty *var_ty) {
  switch (var_ty->ty) {
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

void debug_print_ir(struct ir_op *ir) {
  while (ir) {
    switch (ir->ty) {
    case IR_OP_TY_PHI:
      todo("debug PHI");
      break;
    case IR_OP_TY_CNST:
      fprintf(stderr, "%%%zu (%s) = %zu\n", ir->id, var_ty_string(&ir->var_ty),
              ir->cnst.value);
      break;
    case IR_OP_TY_BINARY_OP:
      fprintf(stderr, "%%%zu (%s) = %%%zu %s %%%zu\n", ir->id,
              var_ty_string(&ir->var_ty), ir->binary_op.lhs->id,
              binary_op_string(ir->binary_op.ty), ir->binary_op.rhs->id);
      break;

    case IR_OP_TY_RET:
      fprintf(stderr, "return %%%zu\n", ir->ret.value->id);
      break;
    }

    ir = ir->succ;
  }
}
