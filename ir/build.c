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

struct var_key get_var_key(struct parser *parser, const struct ast_var *var, struct ir_basicblock *basicblock) {
  const char *name = identifier_str(parser, &var->identifier);
  return (struct var_key){name, var->scope, .basicblock = basicblock};
}

void get_var_ref(struct ir_builder *irb, struct ir_basicblock *basicblock, struct ast_var *var, struct var_key *key, struct var_ref **ref) {
  *ref = NULL;

  // this is when we are _reading_ from the var
  *key = get_var_key(irb->parser, var, basicblock);

  *ref = var_refs_get(irb->var_refs, key);

  if (*ref) {
    return;
  }

  *ref = var_refs_get(irb->var_refs, key);
  if (*ref && (*ref)->op->lcl) {
    return;
  }

  *ref = var_refs_get(irb->global_var_refs, key);
}


bool var_ty_eq(const struct ir_op_var_ty *l, const struct ir_op_var_ty *r) {
  if (l == r) {
    return true;
  }

  if (l->ty != r->ty) {
    return false;
  }

  switch (l->ty) {
  case IR_OP_VAR_TY_TY_NONE:
    return r->ty == IR_OP_VAR_TY_TY_NONE;
  case IR_OP_VAR_TY_TY_PRIMITIVE:
    return l->primitive == r->primitive;
  case IR_OP_VAR_TY_TY_VARIADIC:
    return r->ty == IR_OP_VAR_TY_TY_VARIADIC;
  case IR_OP_VAR_TY_TY_POINTER:
    return var_ty_eq(l->pointer.underlying, r->pointer.underlying);
  case IR_OP_VAR_TY_TY_ARRAY:
    return l->array.ty == r->array.ty &&
           (l->array.ty == IR_OP_VAR_ARRAY_TY_TY_SIZE_UNKNOWN ||
            l->array.num_elements == r->array.num_elements) &&
           var_ty_eq(l->array.underlying, r->array.underlying);
  case IR_OP_VAR_TY_TY_FUNC:
    if (!var_ty_eq(l->func.ret_ty, r->func.ret_ty)) {
      return false;
    }
    if (l->func.num_params != r->func.num_params) {
      return false;
    }
    for (size_t i = 0; i < l->func.num_params; i++) {
      if (!var_ty_eq(&l->func.params[i], &r->func.params[i])) {
        return false;
      }
    }

    return true;
  }

  unreachable("var_ty_eq");
}

bool var_ty_needs_cast_op(const struct ir_op_var_ty *l,
                          const struct ir_op_var_ty *r) {
  if (var_ty_eq(l, r)) {
    return false;
  }

  if ((l->ty == IR_OP_VAR_TY_TY_POINTER || l->ty == IR_OP_VAR_TY_TY_ARRAY) && (r->ty == IR_OP_VAR_TY_TY_POINTER || r->ty == IR_OP_VAR_TY_TY_ARRAY)) {
    // pointers/arrays need no cast instr
    return false;
  }

  // TODO: hardcodes pointer size
  if (l->ty == IR_OP_VAR_TY_TY_PRIMITIVE &&
      l->primitive == IR_OP_VAR_PRIMITIVE_TY_I64 &&
      r->ty == IR_OP_VAR_TY_TY_POINTER) {
    // same size int -> pointer needs no cast
    return false;
  }

  return true;
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

struct ir_op_var_ty
var_ty_get_pointer_underlying(const struct ir_op_var_ty *var_ty) {
  debug_assert(var_ty->ty == IR_OP_VAR_TY_TY_POINTER,
               "non pointer passed to `%s`", __func__);

  return *var_ty->pointer.underlying;
}

struct ir_op_var_ty var_ty_make_pointer(struct ir_builder *irb,
                                        const struct ir_op_var_ty *underlying) {
  struct ir_op_var_ty *copied = arena_alloc(irb->arena, sizeof(*copied));

  *copied = *underlying;

  struct ir_op_var_ty var_ty;
  var_ty.ty = IR_OP_VAR_TY_TY_POINTER;
  var_ty.pointer = (struct ir_op_var_pointer_ty){.underlying = copied};

  return var_ty;
}

struct ir_op_var_ty var_ty_make_array(struct ir_builder *irb,
                                      const struct ir_op_var_ty *underlying,
                                      enum ir_op_var_array_ty_ty ty,
                                      size_t num_elements) {
  struct ir_op_var_ty *copied = arena_alloc(irb->arena, sizeof(*copied));

  *copied = *underlying;

  struct ir_op_var_ty var_ty;
  var_ty.ty = IR_OP_VAR_TY_TY_ARRAY;
  var_ty.array = (struct ir_op_var_array_ty){
      .ty = ty, .num_elements = num_elements, .underlying = copied};

  return var_ty;
}

struct ir_op_var_ty ty_for_ast_tyref(struct ir_builder *irb,
                                     const struct ast_tyref *ty_ref) {
  switch (ty_ref->ty) {
  case AST_TYREF_TY_UNKNOWN:
    bug("shouldn't reach IR gen with unresolved type");
  case AST_TYREF_TY_VOID:
    return IR_OP_VAR_TY_NONE;
  case AST_TYREF_TY_VARIADIC:
    return IR_OP_VAR_TY_VARIADIC;
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
    struct ir_op_var_ty underlying =
        ty_for_ast_tyref(irb, ty_ref->pointer.underlying);
    return var_ty_make_pointer(irb, &underlying);
  }
  case AST_TYREF_TY_ARRAY: {
    struct ir_op_var_ty underlying =
        ty_for_ast_tyref(irb, ty_ref->array.element);

    enum ir_op_var_array_ty_ty ty;
    size_t num_elements;
    switch (ty_ref->array.ty) {
    case AST_TY_ARRAY_TY_UNKNOWN_SIZE:
      ty = IR_OP_VAR_ARRAY_TY_TY_SIZE_UNKNOWN;
      num_elements = 0;
      break;
    case AST_TY_ARRAY_TY_KNOWN_SIZE:
      ty = IR_OP_VAR_ARRAY_TY_TY_SIZE_KNOWN;
      num_elements = ty_ref->array.size;
      break;
    }

    return var_ty_make_array(irb, &underlying, ty, num_elements);
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

  if (from_var_ty.ty == IR_OP_VAR_TY_TY_POINTER &&
      to_var_ty.ty == IR_OP_VAR_TY_TY_POINTER) {
    bug("cast between pointer types is implicit");
  }

  if (from_var_ty.ty == IR_OP_VAR_TY_TY_PRIMITIVE &&
      to_var_ty.ty == IR_OP_VAR_TY_TY_POINTER) {
    // primitive -> pointer
    // TODO: hardcodes pointer size
    if (from_var_ty.primitive == IR_OP_VAR_PRIMITIVE_TY_I64) {
      bug("cast between primitive & pointer type of same size is implicit");
    }

    if (WKT_IS_SIGNED(from->well_known)) {
      return IR_OP_CAST_OP_TY_SEXT;
    } else {
      return IR_OP_CAST_OP_TY_ZEXT;
    }
  }

  if (from_var_ty.ty != IR_OP_VAR_TY_TY_PRIMITIVE ||
      to_var_ty.ty != IR_OP_VAR_TY_TY_PRIMITIVE) {
    todo("casts for non prims/pointers (from %d -> %d)", from_var_ty.ty,
         to_var_ty.ty);
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

struct ir_op *alloc_binaryop(struct ir_builder *irb, struct ir_stmt *stmt,
                             const struct ast_tyref *ty_ref,
                             enum ast_binary_op_ty ty, struct ir_op *lhs,
                             struct ir_op *rhs) {
  struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, ty_ref);

  struct ir_op *op = alloc_ir_op(irb, stmt);
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = var_ty;

  struct ir_op_binary_op *b = &op->binary_op;

  b->lhs = lhs;
  b->rhs = rhs;

  invariant_assert(
      ty_ref->ty == AST_TYREF_TY_WELL_KNOWN ||
          ty_ref->ty == AST_TYREF_TY_POINTER,
      "non primitives/well-knowns/pointers cannot be used in binary "
      "expression by point IR is reached!");

  switch (ty) {
  case AST_BINARY_OP_TY_EQ:
    b->ty = IR_OP_BINARY_OP_TY_EQ;
    break;
  case AST_BINARY_OP_TY_NEQ:
    b->ty = IR_OP_BINARY_OP_TY_NEQ;
    break;
  case AST_BINARY_OP_TY_GT:
    if (WKT_IS_SIGNED(ty_ref->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SGT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UGT;
    }
    break;
  case AST_BINARY_OP_TY_GTEQ:
    if (WKT_IS_SIGNED(ty_ref->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SGTEQ;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UGTEQ;
    }
    break;
  case AST_BINARY_OP_TY_LT:
    if (WKT_IS_SIGNED(ty_ref->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SLT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_ULT;
    }
    break;
  case AST_BINARY_OP_TY_LTEQ:
    if (WKT_IS_SIGNED(ty_ref->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SLTEQ;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_ULTEQ;
    }
    break;
  case AST_BINARY_OP_TY_RSHIFT:
    if (WKT_IS_SIGNED(ty_ref->well_known)) {
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
  case AST_BINARY_OP_TY_OR:
    b->ty = IR_OP_BINARY_OP_TY_OR;
    break;
  case AST_BINARY_OP_TY_XOR:
    b->ty = IR_OP_BINARY_OP_TY_XOR;
    break;
  case AST_BINARY_OP_TY_DIV:
    if (WKT_IS_SIGNED(ty_ref->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SDIV;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UDIV;
    }
    break;
  case AST_BINARY_OP_TY_QUOT:
    if (WKT_IS_SIGNED(ty_ref->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SQUOT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UQUOT;
    }
    break;
  }

  return op;
}

struct ir_op *build_ir_for_addressof(struct ir_builder *irb, struct ir_stmt *stmt,
                                   struct ast_expr *expr, const struct ast_tyref *underlying_ty) {
  // address of does not actually "read" its underlying expression
  // so we do not build the expression

  if (expr->ty != AST_EXPR_TY_ATOM || expr->atom.ty != AST_ATOM_TY_VAR) {
    todo("address of non-simple vars");
  }


  struct ast_tyref pointer_ty = tyref_make_pointer(irb->parser, underlying_ty);
  struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, &pointer_ty);

  struct var_key key;
  struct var_ref *ref;
  get_var_ref(irb, NULL, &expr->atom.var, &key, &ref);

  struct ir_lcl *lcl;
  switch (ref->ty) {
  case VAR_REF_TY_SSA:
    spill_op(irb, ref->op);
    lcl = ref->op->lcl;

    ref->ty = VAR_REF_TY_LCL;

    break;
  case VAR_REF_TY_LCL:
    lcl = ref->op->lcl;
    break;
  case VAR_REF_TY_GLB:
    todo("address of globals");
    break;
  case VAR_REF_TY_ENUM_CNST:
    bug("address of enum makes no sense");
    break;
  }

  struct ir_op *op = alloc_ir_op(irb, stmt);
  op->ty = IR_OP_TY_ADDR;
  op->var_ty = var_ty;
  op->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};
  
  return op;
}

struct ir_op *build_ir_for_unaryop(struct ir_builder *irb, struct ir_stmt *stmt,
                                   struct ast_unary_op *unary_op) {
  if (unary_op->ty == AST_UNARY_OP_TY_ADDRESSOF) {
    return build_ir_for_addressof(irb, stmt, unary_op->expr, &unary_op->expr->var_ty);
  }

  struct ir_op *expr =
      build_ir_for_expr(irb, stmt, unary_op->expr, &unary_op->expr->var_ty);
  struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, &unary_op->var_ty);

  if (unary_op->ty == AST_UNARY_OP_TY_INDIRECTION) {
    // does not generate a unary op instead generates a LOAD_ADDR
    struct ir_op *op = alloc_ir_op(irb, stmt);
    op->ty = IR_OP_TY_LOAD_ADDR;
    op->var_ty = var_ty;
    op->load_addr = (struct ir_op_load_addr){.addr = expr};

    return op;
  }

  switch (unary_op->ty) {
  case AST_UNARY_OP_TY_PREFIX_DEC:
  case AST_UNARY_OP_TY_PREFIX_INC: {
    enum ast_binary_op_ty binary_op_ty =
        unary_op->ty == AST_UNARY_OP_TY_PREFIX_INC ? AST_BINARY_OP_TY_ADD
                                                   : AST_BINARY_OP_TY_SUB;
    struct ir_op *one = alloc_ir_op(irb, stmt);
    one->ty = IR_OP_TY_CNST;
    one->var_ty = var_ty;
    one->cnst.ty = IR_OP_CNST_TY_INT;
    one->cnst.int_value = 1;

    return alloc_binaryop(irb, stmt, &unary_op->var_ty, binary_op_ty, expr,
                          one);
  }
  case AST_UNARY_OP_TY_POSTFIX_INC:
  case AST_UNARY_OP_TY_POSTFIX_DEC:
    todo("postfix inc/dec");
    break;
  case AST_UNARY_OP_TY_PLUS:
    // no work needed, build_expr will handle type conversion
    return expr;
  case AST_UNARY_OP_TY_SIZEOF:
  case AST_UNARY_OP_TY_ALIGNOF:
    todo("sizeof/alignof build (will need different node as they take types "
         "not exprs)");
    break;
  case AST_UNARY_OP_TY_CAST:
    if (var_ty_needs_cast_op(&var_ty, &expr->var_ty)) {
      return insert_ir_for_cast(irb, stmt, expr, &var_ty,
                                cast_ty_for_ast_tyref(irb,
                                                      &unary_op->expr->var_ty,
                                                      &unary_op->var_ty));
    } else {
      expr->var_ty = ty_for_ast_tyref(irb, &unary_op->var_ty);
      return expr;
    }
  default:
    break;
  }

  enum ir_op_unary_op_ty unary_op_ty;
  switch (unary_op->ty) {
  case AST_UNARY_OP_TY_MINUS:
    unary_op_ty = IR_OP_UNARY_OP_TY_NEG;
    break;
  case AST_UNARY_OP_TY_LOGICAL_NOT:
    unary_op_ty = IR_OP_UNARY_OP_TY_LOGICAL_NOT;
    break;
  case AST_UNARY_OP_TY_NOT:
    unary_op_ty = IR_OP_UNARY_OP_TY_NOT;
    break;
  default:
    bug("unexpected unary_op_ty in `%s`", __func__);
    break;
  }

  struct ir_op *op = alloc_ir_op(irb, stmt);
  op->ty = IR_OP_TY_UNARY_OP;
  op->var_ty = var_ty;
  op->unary_op.ty = unary_op_ty;
  op->unary_op.value = expr;

  return op;
}

struct ir_op *build_ir_for_binaryop(struct ir_builder *irb,
                                    struct ir_stmt *stmt,
                                    struct ast_binary_op *binary_op) {
  struct ir_op *lhs =
      build_ir_for_expr(irb, stmt, binary_op->lhs, &binary_op->var_ty);
  struct ir_op *rhs =
      build_ir_for_expr(irb, stmt, binary_op->rhs, &binary_op->var_ty);

  return alloc_binaryop(irb, stmt, &binary_op->var_ty, binary_op->ty, lhs, rhs);
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
  // this is when we are _reading_ from the var
  struct var_key key;
  struct var_ref *ref;
  get_var_ref(irb, stmt->basicblock, var, &key, &ref);

  struct ast_tyref var_tyref = var->var_ty;
  struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, &var_tyref);
  if (ref) {
    switch (ref->ty) {
    case VAR_REF_TY_SSA:
      return ref->op;
    case VAR_REF_TY_LCL: {
      struct ir_op *op = alloc_ir_op(irb, stmt);
      op->ty = IR_OP_TY_LOAD_LCL;
      op->var_ty = var_ty;
      op->load_lcl = (struct ir_op_load_lcl){
        .lcl = ref->op
      };

      return op;
    }
    case VAR_REF_TY_GLB: {
      struct ir_op *op = alloc_ir_op(irb, stmt);
      make_sym_ref(irb, key.name, op, &var_ty);

      return op;
    }
    case VAR_REF_TY_ENUM_CNST: {
      struct ir_op *op = alloc_ir_op(irb, stmt);
      op->ty = IR_OP_TY_CNST;
      op->var_ty = var_ty;
      op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                     .int_value = ref->enum_cnst};

      return op;
    }
    }
  }

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

  key = get_var_key(irb->parser, var, stmt->basicblock);
  struct var_ref *new_ref =
      var_refs_add(irb->var_refs, &key, VAR_REF_TY_SSA);
  new_ref->ty = VAR_REF_TY_SSA;
  new_ref->op = phi;

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

struct ir_op *build_ir_for_initlist(struct ir_builder *irb,
                                    struct ir_stmt *stmt,
                                    struct ast_initlist *init_list,
                                    const struct ast_tyref *ast_tyref) {
  // init list is fundamentally untyped, so it needs to know its target type in
  // order to be built

  debug_assert(ast_tyref->ty == AST_TYREF_TY_ARRAY,
               "init list only makes sense for arrays");
  UNUSED_ARG(irb);
  UNUSED_ARG(stmt);
  UNUSED_ARG(init_list);
  UNUSED_ARG(ast_tyref);
  todo(__func__);
}

struct ir_op *build_ir_for_call(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_call *call) {
  // need to generate args and target IR first to keep IR in order

  struct ir_op **args =
      arena_alloc(irb->arena, sizeof(struct ir_op *) * call->arg_list.num_args);

  size_t num_non_variadic_args = call->var_ty.func.num_param_var_tys
                                     ? call->var_ty.func.num_param_var_tys - 1
                                     : 0;
  for (size_t i = 0; i < call->arg_list.num_args; i++) {
    args[i] = build_ir_for_expr(irb, stmt, &call->arg_list.args[i],
                                &call->arg_list.args[i].var_ty);

    if (i >= num_non_variadic_args) {
      args[i]->flags |= IR_OP_FLAG_VARIADIC_PARAM;
    }
  }

  struct ir_op *target =
      build_ir_for_expr(irb, stmt, call->target, &call->target->var_ty);

  irb->flags |= IR_BUILDER_FLAG_MAKES_CALL;
  struct ir_op *op = alloc_ir_op(irb, stmt);

  op->ty = IR_OP_TY_CALL;
  op->var_ty = ty_for_ast_tyref(irb, &call->var_ty);
  op->call.target = target;
  op->call.num_args = call->arg_list.num_args;
  op->call.args = args;

  return op;
}

struct ir_op *build_ir_for_array_address(struct ir_builder *irb,
                                         struct ir_stmt *stmt,
                                         struct ast_expr *lhs_expr,
                                         struct ast_expr *rhs_expr,
                                         const struct ast_tyref *elem_ty);

struct ir_op *build_ir_for_expr(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_expr *expr,
                                const struct ast_tyref *ast_tyref) {
  struct ir_op *op;
  switch (expr->ty) {
  case AST_EXPR_TY_ATOM:
    op = build_ir_for_atom(irb, stmt, &expr->atom, ast_tyref);
    break;
  case AST_EXPR_TY_CALL:
    op = build_ir_for_call(irb, stmt, &expr->call);
    break;
  case AST_EXPR_TY_UNARY_OP:
    op = build_ir_for_unaryop(irb, stmt, &expr->unary_op);
    break;
  case AST_EXPR_TY_BINARY_OP:
    op = build_ir_for_binaryop(irb, stmt, &expr->binary_op);
    break;
  case AST_EXPR_TY_ARRAYACCESS: {
    struct ir_op *address =
        build_ir_for_array_address(irb, stmt, expr->array_access.lhs,
                                   expr->array_access.rhs, &expr->var_ty);

    op = alloc_ir_op(irb, stmt);
    op->ty = IR_OP_TY_LOAD_ADDR;
    op->var_ty = ty_for_ast_tyref(irb, &expr->var_ty);
    op->load_addr = (struct ir_op_load_addr){.addr = address};

    break;
  }
  case AST_EXPR_TY_MEMBERACCESS:
  case AST_EXPR_TY_POINTERACCESS:
    todo("access build");
    break;
  case AST_EXPR_TY_INIT_LIST:
    bug("init list only makes sense in decls and compound assignments");
  case AST_EXPR_TY_ASSG:
    op = build_ir_for_assg(irb, stmt, &expr->assg);
    break;
  }

  if (ast_tyref) {
    struct ir_op_var_ty var_ty = ty_for_ast_tyref(irb, ast_tyref);

    if (var_ty_needs_cast_op(&op->var_ty, &var_ty)) {
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
                       struct ir_op *op, struct ast_var *var) {
  debug_assert(op, "null expr in assignment!");

  struct var_key key = get_var_key(irb->parser, var, stmt->basicblock);
  struct var_ref *ref = var_refs_get(irb->var_refs, &key);

  if (!ref) {
    ref = var_refs_add(irb->var_refs, &key, VAR_REF_TY_SSA);
  }

  if (ref->ty == VAR_REF_TY_SSA) {
    ref->op = op;
  } else {
    unreachable("assignment to global not yet suppported");
  }

  return op;
}

struct ir_op *build_ir_for_array_address(struct ir_builder *irb,
                                         struct ir_stmt *stmt,
                                         struct ast_expr *lhs_expr,
                                         struct ast_expr *rhs_expr,
                                         const struct ast_tyref *elem_ty) {
  struct ast_tyref pointer_ty = tyref_make_pointer(irb->parser, elem_ty);

  struct ir_op *lhs;
  if (lhs_expr->var_ty.ty == AST_TYREF_TY_ARRAY) {
    // need to decay the type to pointer
    struct ast_tyref *underlying = lhs_expr->var_ty.array.element;
    lhs = build_ir_for_addressof(irb, stmt, lhs_expr, underlying);
  } else {
    lhs = build_ir_for_expr(irb, stmt, lhs_expr, &pointer_ty);
  }
  
  struct ir_op *rhs = build_ir_for_expr(irb, stmt, rhs_expr, &pointer_ty);

  return alloc_binaryop(irb, stmt, &pointer_ty, AST_BINARY_OP_TY_ADD, lhs, rhs);
}

struct ir_op *build_ir_for_assg(struct ir_builder *irb, struct ir_stmt *stmt,
                                struct ast_assg *assg) {
  struct ir_op *expr =
      build_ir_for_expr(irb, stmt, assg->expr, &assg->expr->var_ty);
  struct ir_op *value;

  switch (assg->ty) {
  case AST_ASSG_TY_SIMPLEASSG:
    value = expr;
    break;
  case AST_ASSG_TY_COMPOUNDASSG: {
    struct ir_op *assignee =
        build_ir_for_expr(irb, stmt, assg->assignee, &assg->assignee->var_ty);

    value = alloc_binaryop(irb, stmt, &assg->compound_assg.intermediate_var_ty,
                           assg->compound_assg.binary_op_ty, assignee, expr);
  }
  }

  if (assg->assignee->ty == AST_EXPR_TY_ATOM &&
      assg->assignee->atom.ty == AST_ATOM_TY_VAR) {
    return var_assg(irb, stmt, value, &assg->assignee->atom.var);
  } else if (assg->assignee->ty == AST_EXPR_TY_ARRAYACCESS) {
    struct ast_arrayaccess *access = &assg->assignee->array_access;
    struct ir_op *address = build_ir_for_array_address(
        irb, stmt, access->lhs, access->rhs, &assg->assignee->var_ty);

    struct ir_op *store = alloc_ir_op(irb, stmt);
    store->ty = IR_OP_TY_STORE_ADDR;
    store->var_ty = IR_OP_VAR_TY_NONE;
    store->store_addr = (struct ir_op_store_addr){
      .addr = address,
      .value = value
    };

    return store;
  } else {
    todo("non var assignments");
  }
}
struct ir_op *build_ir_for_vardecl_with_initlist(struct ir_builder *irb,
                                       struct ir_stmt *stmt,
                                       struct ast_vardecl *decl,
                                       struct ast_initlist *init_list
                                     ) {
  for (size_t i = 0; i < init_list->num_exprs; i++)
}

struct ir_op *build_ir_for_vardecllist(struct ir_builder *irb,
                                       struct ir_stmt *stmt,
                                       struct ast_vardecllist *var_decl_list) {
  for (size_t i = 0; i < var_decl_list->num_decls; i++) {
    struct ast_vardecl *decl = &var_decl_list->decls[i];

    struct ir_op *assignment;
    if (decl->ty == AST_VARDECL_TY_DECL_WITH_ASSG) {
      if (decl->assg_expr.ty == AST_EXPR_TY_INIT_LIST) {
        assignment = build_ir_for_vardecl_with_initlist(irb, stmt, decl, &decl->assg_expr.init_list);
      } else {
        assignment = build_ir_for_expr(irb, stmt, &decl->assg_expr, &decl->var.var_ty);
      }
    } else {
      assignment = alloc_ir_op(irb, stmt);
      assignment->ty = IR_OP_TY_UNDF;
      assignment->var_ty = ty_for_ast_tyref(irb, &decl->var.var_ty);
    }

    // FIXME: is this right
    var_assg(irb, stmt, assignment, &decl->var);
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

void walk_basicblock(struct ir_builder *irb, bool *basicblocks_visited,
                     struct ir_op *source_phi, struct ast_var *var,
                     struct ir_basicblock *basicblock, struct ir_op ***exprs,
                     size_t *num_exprs) {
  if (!basicblock || basicblocks_visited[basicblock->id]) {
    return;
  }

  basicblocks_visited[basicblock->id] = true;

  struct var_key key = get_var_key(irb->parser, var, basicblock);
  struct var_ref *ref = var_refs_get(irb->var_refs, &key);

  if (!ref) {
    debug("bb %zu has %zu preds", basicblock->id, basicblock->num_preds);
    for (size_t i = 0; i < basicblock->num_preds; i++) {
      walk_basicblock(irb, basicblocks_visited, source_phi, var,
                      basicblock->preds[i], exprs, num_exprs);
    }
    return;
  }

  switch (ref->ty) {
  case VAR_REF_TY_GLB:
  case VAR_REF_TY_LCL:
  case VAR_REF_TY_ENUM_CNST:
    unreachable(
        "non-lcl var refs should already be handled and not generate phis");
  case VAR_REF_TY_SSA: {
    struct ir_op *entry_op = ref->op;
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
    } else if (ref->op) {
      (*num_exprs)++;
      *exprs = arena_realloc(irb->arena, *exprs,
                             sizeof(struct ir_op *) * *num_exprs);
      (*exprs)[*num_exprs - 1] = ref->op;
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

struct validate_metadata {
  struct ir_builder *irb;
  struct ir_op *consumer;
};

void validate_op_tys_callback(struct ir_op **op, void *cb_metadata) {
  struct validate_metadata *metadata = cb_metadata;
  struct ir_op *consumer = metadata->consumer;

  struct ir_op_var_ty res_ty = (*op)->var_ty;

  // TODO: validate cast types
  switch (consumer->ty) {
  case IR_OP_TY_CAST_OP:
    res_ty = consumer->var_ty;
    break;
  case IR_OP_TY_ADDR:
    res_ty =
        var_ty_make_pointer((*op)->stmt->basicblock->irb, &(*op)->var_ty);
    break;
  default:
    break;
  }

  // TODO: validate CALL ops as well
  if (consumer->ty != IR_OP_TY_CALL && op_produces_value(consumer->ty)) {

    invariant_assert(!var_ty_needs_cast_op(&res_ty, &consumer->var_ty),
                     "op %zu uses op %zu with different type!", consumer->id,
                     (*op)->id);
  }
}

struct ir_builder *
build_ir_for_function(struct parser *parser, struct arena_allocator *arena,
                      struct ast_funcdef *def, struct var_refs *global_var_refs,
                      debug_print_custom_ir_op debug_print_custom_ir_op) {
  struct var_refs *var_refs = var_refs_create();
  struct ir_builder b = {.name = identifier_str(parser, &def->sig.name),
                         .parser = parser,
                         .var_refs = var_refs,
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
                         .offset = 0,
                         .debug_print_custom_ir_op = debug_print_custom_ir_op};

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

    struct var_key key = get_var_key(builder->parser, &param->var, basicblock);
    struct var_ref *ref =
        var_refs_add(builder->var_refs, &key, VAR_REF_TY_SSA);

    struct ir_op *mov = alloc_ir_op(builder, param_stmt);
    mov->ty = IR_OP_TY_MOV;
    mov->var_ty = ty_for_ast_tyref(builder, &param->var_ty);
    mov->flags |= IR_OP_FLAG_PARAM;
    mov->mov.value = NULL;

    ref->op = mov;
  }

  for (size_t i = 0; i < def->body.num_stmts; i++) {
    basicblock = build_ir_for_stmt(builder, basicblock, &def->body.stmts[i]);
  }

  // we may generate empty basicblocks or statements, prune them here
  prune_basicblocks(builder);

  // may not end in a return, but needs to to be well-formed IR
  struct ir_basicblock *last_bb = builder->last;
  if (!last_bb) {
    debug("adding bb to create ret");
    last_bb = alloc_ir_basicblock(builder);
  }

  struct ir_stmt *last_stmt = last_bb->last;
  if (!last_stmt) {
    debug("adding bb to create stmt");
    last_stmt = alloc_ir_stmt(builder, last_bb);
  }

  struct ir_op *last_op = last_stmt->last;

  if (!last_op || last_op->ty != IR_OP_TY_RET) {
    struct ir_op *return_value = NULL;

    if (strcmp(builder->name, "main") == 0) {
      debug("adding implicit return 0 to bb %zu", last_bb->id);

      struct ir_op *cnst = alloc_ir_op(builder, last_stmt);
      cnst->ty = IR_OP_TY_CNST;
      cnst->var_ty = (struct ir_op_var_ty){
          .ty = IR_OP_VAR_TY_TY_PRIMITIVE,
          .primitive = IR_OP_VAR_PRIMITIVE_TY_I32,
      };
      cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = 0};

      return_value = cnst;
    }

    basicblock = build_ir_for_ret(builder, last_stmt, NULL);
    debug_assert(last_stmt->last->ty == IR_OP_TY_RET,
                 "expected ret after call to build ret");
    last_stmt->last->ret.value = return_value;
  }

  // prune again, as inserting the ret can introduce an extraneous empty bb
  prune_basicblocks(builder);

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
        struct validate_metadata metadata = {.irb = builder, .consumer = op};

        walk_op_uses(op, validate_op_tys_callback, &metadata);

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
    struct arena_allocator *arena, struct ast_translationunit *translation_unit,
    debug_print_custom_ir_op debug_print_custom_ir_op) {

  struct ir_unit u = {
      .funcs = arena_alloc(arena, sizeof(struct ir_builder *) *
                                      translation_unit->num_func_defs),
      .num_funcs = translation_unit->num_func_defs,
  };
  struct ir_unit *iru = arena_alloc(arena, sizeof(*iru));
  *iru = u;

  struct var_refs *global_var_refs = var_refs_create();
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

      struct var_key key = {.name =
                                identifier_str(parser, &enum_cnst->identifier),
                            .scope = SCOPE_GLOBAL};
      struct var_ref *ref =
          var_refs_add(global_var_refs, &key, VAR_REF_TY_ENUM_CNST);
      ref->enum_cnst = enum_value;

      // increment for the next implicit value
      enum_value++;
    }
  }

  for (size_t i = 0; i < translation_unit->num_func_defs; i++) {
    struct ast_funcdef *def = &translation_unit->func_defs[i];
    struct var_key key = {.name = identifier_str(parser, &def->sig.name),
                          .scope = SCOPE_GLOBAL};

    struct ir_builder *func = build_ir_for_function(
        parser, arena, def, global_var_refs, debug_print_custom_ir_op);

    var_refs_get(global_var_refs, &key)->func = func;

    iru->funcs[i] = func;
  }

  return iru;
}
