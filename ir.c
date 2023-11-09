#include "ir.h"
#include "alloc.h"
#include "parse.h"
#include "vector.h"

struct ir_builder {
  struct arena_allocator *arena;

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
                                     struct ast_binary_op *binary_op) {
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

struct ir_op *build_ir_for_rvalue(struct ir_builder *irb, struct ast_rvalue *rvalue) {
  switch (rvalue->ty) {
  case AST_RVALUE_TY_CNST:
    return build_ir_for_cnst(irb, &rvalue->cnst);
  case AST_RVALUE_TY_BINARY_OP:
    return build_ir_for_binary_op(irb, &rvalue->binary_op);
  }
}

struct ir_op *build_ir_for_lvalue(struct ir_builder *irb, struct ast_lvalue *lvalue) {
  // switch (lvalue->ty) {
  // }
  UNUSED_ARG(irb);
  UNUSED_ARG(lvalue);
  todo("build_ir_for_lvalue");
}

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ast_expr *expr) {
  switch (expr->ty) {
  case AST_EXPR_TY_RVALUE:
    return build_ir_for_rvalue(irb, &expr->rvalue);
  case AST_EXPR_TY_LVALUE:
    return build_ir_for_lvalue(irb, &expr->lvalue);
  }
}

struct ir_op *build_ir_for_stmt(struct ir_builder *irb, struct ast_stmt *stmt) {
  switch (stmt->ty) {
  case AST_STMT_TY_RET: {
    struct ir_op *expr_op = build_ir_for_expr(irb, &stmt->ret);

    struct ir_op *op = alloc_ir_op(irb);
    op->ty = IR_OP_TY_RET;
    op->ret.value = expr_op;
    return op;
  }
  }
}

struct ir_function build_ir_for_function(struct arena_allocator *arena,
                                         struct ast_funcdef *def) {
  struct ir_builder builder = {
      .arena = arena, .first = NULL, .last = NULL, .last_id = 0};

  for (size_t i = 0; i < def->body.num_stmts; i++) {
    build_ir_for_stmt(&builder, &def->body.stmts[i]);
  }

  struct ir_function func = {
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
