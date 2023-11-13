#include "ir.h"
#include "alloc.h"
#include "lex.h"
#include "parse.h"
#include "vector.h"

struct var_table_entry {
  const char *name;
  int scope;
  struct ir_op *last_write;
};

struct var_table {
  // vector of `var_table_entry`
  // change to hash eventually?
  struct vector *entries;

  // needed for accessing AST text
  struct parser *parser;
};

struct var_table var_table_create(struct parser *parser) {
  // known memory leak here, no `free` function atm
  struct var_table var_table = {
      .entries = vector_create(sizeof(struct var_table_entry)),
      .parser = parser};

  return var_table;
}

struct var_table_entry *create_entry(struct var_table *var_table, const struct ast_var *var) {
  const char *name = identifier_str(var_table->parser, &var->identifier);
  struct var_table_entry entry = {
      .name = name, .scope = var->scope, .last_write = NULL};

  return vector_push_back(var_table->entries, &entry);
}

struct var_table_entry *get_or_create_entry(struct var_table *var_table,
                                            const struct ast_var *var) {
  // super inefficient, TODO: make efficient
  // does linear scan for entry at current scope, if that fails, tries at higher scope, until scope is global
  // then creates new entry

  const char *name = identifier_str(var_table->parser, &var->identifier);
  size_t num_vars = vector_length(var_table->entries);

  for (int scope = var->scope; scope >= SCOPE_GLOBAL; scope--) {
    trace("trying to find var '%s' at scope '%d' (var has scope '%d')", name, scope, var->scope);

    for (size_t i = 0; i < num_vars; i++) {
      struct var_table_entry *entry = vector_get(var_table->entries, i);

      if (entry->scope == scope && strcmp(entry->name, name) == 0) {
        trace("found var at scope '%d'", scope);
        return entry;
      }
    }

    if (scope != SCOPE_GLOBAL) {
      trace("failed! trying at next scope up");
    }
  }

  trace("couldn't find variable, creating new entry '%s' with scope '%d'", name, var->scope);
  
  return create_entry(var_table, var);
}

struct ir_builder {
  struct parser *parser;
  struct arena_allocator *arena;

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

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ast_expr *expr);

struct ir_op *build_ir_for_binary_op(struct ir_builder *irb,
                                     struct ast_binaryop *binary_op) {
  struct ir_op *lhs = build_ir_for_expr(irb, binary_op->lhs);
  struct ir_op *rhs = build_ir_for_expr(irb, binary_op->rhs);

  struct ir_op *op = alloc_ir_op(irb);
  op->ty = IR_OP_TY_BINARY_OP;

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
    b->ty = IR_OP_BINARY_OP_TY_DIV;
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
  op->cnst.value = cnst->value;

  return op;
}

struct ir_op *build_ir_for_assg(struct ir_builder *irb,
                                  struct ast_assg *assg) {
  switch (assg->lvalue.ty) {
    case AST_LVALUE_TY_VAR: {
      struct var_table_entry *entry =
          get_or_create_entry(&irb->var_table, &assg->lvalue.var);
    
      entry->last_write = build_ir_for_expr(irb, assg->expr);
      return entry->last_write;
    }
  }
}
  
struct ir_op *build_ir_for_rvalue(struct ir_builder *irb,
                                  struct ast_rvalue *rvalue) {
  switch (rvalue->ty) {
  case AST_RVALUE_TY_CNST:
    return build_ir_for_cnst(irb, &rvalue->cnst);
  case AST_RVALUE_TY_ASSG:
      return build_ir_for_assg(irb, rvalue->assg);
  case AST_RVALUE_TY_BINARY_OP:
    return build_ir_for_binary_op(irb, &rvalue->binary_op);
  default:
    todo("build ir for <thing>");
  }
}

struct ir_op *build_ir_for_lvalue(struct ir_builder *irb,
                                  struct ast_lvalue *lvalue) {
  switch (lvalue->ty) {
  case AST_LVALUE_TY_VAR: {
    // this is when we are _reading_ from the var
    struct var_table_entry *entry =
        get_or_create_entry(&irb->var_table, &lvalue->var);

    if (!entry->last_write) {
      err("undefined behaviour - reading from unassigned variable '%s'", entry->name);
    }
      
    return entry->last_write;
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
    struct var_table_entry *entry = create_entry(
        &irb->var_table, &decl->var);

    if (decl->ty == AST_VARDECL_TY_DECL) {
      continue;
    }

    entry->last_write = build_ir_for_expr(irb, &decl->assg_expr);
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
      .op_count = builder.last->id // i think?
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
  case IR_OP_BINARY_OP_TY_DIV:
    return "/";
  case IR_OP_BINARY_OP_TY_QUOT:
    return "%";
  default:
    return "?unknown?";
  }
}

void debug_print_ir(struct ir_op *ir) {
  while (ir) {
    switch (ir->ty) {
    case IR_OP_TY_PHI:
      todo("debug PHI");
      break;
    case IR_OP_TY_CNST:
      fprintf(stderr, "%%%zu = %zu\n", ir->id, ir->cnst.value);
      break;
    case IR_OP_TY_BINARY_OP:
      fprintf(stderr, "%%%zu = %%%zu %s %%%zu\n", ir->id, ir->binary_op.lhs->id,
              binary_op_string(ir->binary_op.ty), ir->binary_op.rhs->id);
      break;

    case IR_OP_TY_RET:
      fprintf(stderr, "return %%%zu\n", ir->ret.value->id);
      break;
    }

    ir = ir->succ;
  }
}
