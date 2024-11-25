#include "build.h"

#include "../alloc.h"
#include "../compiler.h"
#include "../lex.h"
#include "../typechk.h"
#include "../util.h"
#include "../var_table.h"
#include "../vector.h"
#include "ir.h"
#include "prettyprint.h"
#include "var_refs.h"

#include <math.h>

// break/continues will add an entry into the jumps vector
// and then at the end of the loop these will be traversed and fixed to point to
// the correct basicblock the special value IR_JUMP_TY_NEW_LOOP indicates the
// start of a loop
enum ir_jump_ty {
  IR_JUMP_TY_NEW_LOOP,

  IR_JUMP_TY_BREAK,
  IR_JUMP_TY_CONTINUE
};

struct ir_jump {
  enum ir_jump_ty ty;

  struct ir_basicblock *basicblock;
};

enum ir_case_ty { IR_CASE_TY_CASE, IR_CASE_TY_DEFAULT };

struct ir_case {
  enum ir_case_ty ty;

  struct ir_split_case split_case;
};

// linked list of label -> bb mappings
struct ir_label {
  const char *name;
  struct ir_basicblock *basicblock;

  struct ir_label *succ;
};

struct ir_func_builder {
  struct arena_allocator *arena;

  struct ir_unit *unit;
  struct ir_func *func;

  struct typechk *tchk;

  struct var_refs *var_refs;
  struct var_refs *global_var_refs;

  struct ir_label *labels;

  struct vector *jumps;
  struct vector *switch_cases;
};

static struct ir_label *add_label(struct ir_func_builder *irb, const char *name,
                                  struct ir_basicblock *basicblock) {
  struct ir_label *label = arena_alloc(irb->arena, sizeof(*label));

  label->name = name;
  label->basicblock = basicblock;
  label->succ = irb->labels;

  irb->labels = label;

  return label;
}

static struct var_key get_var_key(const struct td_var *var,
                                  struct ir_basicblock *basicblock) {
  return (struct var_key){var->identifier, var->scope,
                          .basicblock = basicblock};
}

static void get_var_ref(struct ir_func_builder *irb,
                        struct ir_basicblock *basicblock, struct td_var *var,
                        struct var_key *key, struct var_ref **ref) {
  // debug_assert(var->ty != TD_VAR_TY_ENUM_CNST,
  //              "can't get var ref for enum cnst");

  *ref = NULL;

  // this is when we are _reading_ from the var
  *key = get_var_key(var, basicblock);

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

static bool var_ty_eq(struct ir_func *irb, const struct ir_var_ty *l,
                      const struct ir_var_ty *r) {
  if (l == r) {
    return true;
  }

  if (l->ty != r->ty) {
    return false;
  }

  switch (l->ty) {
  case IR_VAR_TY_TY_NONE:
    return r->ty == IR_VAR_TY_TY_NONE;
  case IR_VAR_TY_TY_PRIMITIVE:
    return l->primitive == r->primitive;
  case IR_VAR_TY_TY_VARIADIC:
    return r->ty == IR_VAR_TY_TY_VARIADIC;
  case IR_VAR_TY_TY_POINTER:
    return true;
  case IR_VAR_TY_TY_ARRAY:
    return l->array.num_elements == r->array.num_elements &&
           var_ty_eq(irb, l->array.underlying, r->array.underlying);
  case IR_VAR_TY_TY_FUNC:
    if (!var_ty_eq(irb, l->func.ret_ty, r->func.ret_ty)) {
      return false;
    }
    if (l->func.num_params != r->func.num_params) {
      return false;
    }
    for (size_t i = 0; i < l->func.num_params; i++) {
      if (!var_ty_eq(irb, &l->func.params[i], &r->func.params[i])) {
        return false;
      }
    }

    return true;
  case IR_VAR_TY_TY_STRUCT: {
    if (l->struct_ty.num_fields != r->struct_ty.num_fields) {
      return false;
    }

    struct ir_var_ty_info l_info = var_ty_info(irb->unit, l);
    struct ir_var_ty_info r_info = var_ty_info(irb->unit, r);

    // currently we do not have custom alignment/size but it is possible
    if (l_info.size != r_info.size || l_info.alignment != r_info.alignment) {
      return false;
    }

    for (size_t i = 0; i < l->struct_ty.num_fields; i++) {
      if (!var_ty_eq(irb, &l->struct_ty.fields[i], &r->struct_ty.fields[i])) {
        return false;
      }
    }

    return true;
  }
  case IR_VAR_TY_TY_UNION: {
    if (l->union_ty.num_fields != r->union_ty.num_fields) {
      return false;
    }

    struct ir_var_ty_info l_info = var_ty_info(irb->unit, l);
    struct ir_var_ty_info r_info = var_ty_info(irb->unit, r);

    // currently we do not have custom alignment/size but it is possible
    if (l_info.size != r_info.size || l_info.alignment != r_info.alignment) {
      return false;
    }

    for (size_t i = 0; i < l->union_ty.num_fields; i++) {
      if (!var_ty_eq(irb, &l->union_ty.fields[i], &r->union_ty.fields[i])) {
        return false;
      }
    }

    return true;
  }
  }

  unreachable();
}

static bool var_ty_needs_cast_op(struct ir_func_builder *irb,
                                 const struct ir_var_ty *l,
                                 const struct ir_var_ty *r) {
  // note: `l` is TO, `r` is FROM, (as this is in the context of `l <- r`)

  if (l->ty == IR_VAR_TY_TY_NONE) {
    // void casts are nop
    return false;
  }

  if (var_ty_is_aggregate(l) && var_ty_is_aggregate(r)) {
    // casting between these could require conversion,
    // but never a cast op
    return false;
  }

  if (var_ty_eq(irb->func, l, r)) {
    return false;
  }

  if ((l->ty == IR_VAR_TY_TY_FUNC && r->ty == IR_VAR_TY_TY_POINTER) ||
      (r->ty == IR_VAR_TY_TY_FUNC && l->ty == IR_VAR_TY_TY_POINTER)) {
    return false;
  }

  if ((l->ty == IR_VAR_TY_TY_POINTER || l->ty == IR_VAR_TY_TY_ARRAY) &&
      (r->ty == IR_VAR_TY_TY_POINTER || r->ty == IR_VAR_TY_TY_ARRAY)) {
    // pointers/arrays need no cast instr
    return false;
  }

  // TODO: hardcodes pointer size
  if (((l->ty == IR_VAR_TY_TY_PRIMITIVE &&
        l->primitive == IR_VAR_PRIMITIVE_TY_I64) ||
       l->ty == IR_VAR_TY_TY_POINTER) &&
      ((r->ty == IR_VAR_TY_TY_PRIMITIVE &&
        r->primitive == IR_VAR_PRIMITIVE_TY_I64) ||
       r->ty == IR_VAR_TY_TY_POINTER)) {
    // same size int -> pointer needs no cast
    return false;
  }

  return true;
}

static enum ir_var_primitive_ty
var_ty_for_well_known_ty(enum well_known_ty wkt) {
  switch (wkt) {
  case WELL_KNOWN_TY_CHAR:
  case WELL_KNOWN_TY_SIGNED_CHAR:
  case WELL_KNOWN_TY_UNSIGNED_CHAR:
    return IR_VAR_PRIMITIVE_TY_I8;
  case WELL_KNOWN_TY_SIGNED_SHORT:
  case WELL_KNOWN_TY_UNSIGNED_SHORT:
    return IR_VAR_PRIMITIVE_TY_I16;
  case WELL_KNOWN_TY_SIGNED_INT:
  case WELL_KNOWN_TY_UNSIGNED_INT:
    return IR_VAR_PRIMITIVE_TY_I32;
  case WELL_KNOWN_TY_SIGNED_LONG:
  case WELL_KNOWN_TY_UNSIGNED_LONG:
    return IR_VAR_PRIMITIVE_TY_I64;
  case WELL_KNOWN_TY_SIGNED_LONG_LONG:
  case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
    return IR_VAR_PRIMITIVE_TY_I64;
  case WELL_KNOWN_TY_HALF:
    return IR_VAR_PRIMITIVE_TY_F16;
  case WELL_KNOWN_TY_FLOAT:
    return IR_VAR_PRIMITIVE_TY_F32;
  case WELL_KNOWN_TY_DOUBLE:
  case WELL_KNOWN_TY_LONG_DOUBLE:
    return IR_VAR_PRIMITIVE_TY_F64;
  }
}

static struct ir_var_ty var_ty_for_td_var_ty(struct ir_unit *iru,
                                             const struct td_var_ty *var_ty) {
  switch (var_ty->ty) {
  case TD_VAR_TY_TY_UNKNOWN:
  case TD_VAR_TY_TY_INCOMPLETE_AGGREGATE:
    bug("shouldn't reach IR gen with unresolved type");
  case TD_VAR_TY_TY_AGGREGATE: {
    struct td_ty_aggregate aggregate = var_ty->aggregate;

    struct ir_var_ty ty;
    switch (aggregate.ty) {
    case TD_TY_AGGREGATE_TY_STRUCT:
      ty.ty = IR_VAR_TY_TY_STRUCT;
      ty.struct_ty.num_fields = aggregate.num_fields;
      ty.struct_ty.fields = arena_alloc(
          iru->arena, sizeof(struct ir_var_ty) * ty.struct_ty.num_fields);

      for (size_t i = 0; i < ty.struct_ty.num_fields; i++) {
        // handle nested types

        ty.struct_ty.fields[i] =
            var_ty_for_td_var_ty(iru, &aggregate.fields[i].var_ty);
      }
      break;
    case TD_TY_AGGREGATE_TY_UNION:
      ty.ty = IR_VAR_TY_TY_UNION;
      ty.union_ty.num_fields = aggregate.num_fields;
      ty.union_ty.fields = arena_alloc(iru->arena, sizeof(struct ir_var_ty) *
                                                       ty.union_ty.num_fields);

      for (size_t i = 0; i < ty.union_ty.num_fields; i++) {
        // handle nested types

        ty.struct_ty.fields[i] =
            var_ty_for_td_var_ty(iru, &aggregate.fields[i].var_ty);
      }
      break;
    }

    return ty;
  }
  case TD_VAR_TY_TY_VOID:
    return IR_VAR_TY_NONE;
  case TD_VAR_TY_TY_VARIADIC:
    return IR_VAR_TY_VARIADIC;
  case TD_VAR_TY_TY_WELL_KNOWN: {
    struct ir_var_ty ty;
    ty.ty = IR_VAR_TY_TY_PRIMITIVE;
    ty.primitive = var_ty_for_well_known_ty(var_ty->well_known);
    return ty;
  }
  case TD_VAR_TY_TY_FUNC: {
    bool variadic = var_ty->func.ty == TD_TY_FUNC_TY_VARIADIC;

    struct ir_var_ty ty;
    ty.ty = IR_VAR_TY_TY_FUNC;
    ty.func.ret_ty = arena_alloc(iru->arena, sizeof(*ty.func.ret_ty));
    *ty.func.ret_ty = var_ty_for_td_var_ty(iru, var_ty->func.ret);

    // from IR onwards, variadic is no longer a param of the function but
    // instead a flag
    ty.func.num_params = var_ty->func.num_params;
    ty.func.params =
        arena_alloc(iru->arena, sizeof(struct ir_op) * ty.func.num_params);

    ty.func.flags = IR_VAR_FUNC_TY_FLAG_NONE;
    if (variadic) {
      ty.func.flags |= IR_VAR_FUNC_TY_FLAG_VARIADIC;
    }

    for (size_t i = 0; i < ty.func.num_params; i++) {
      ty.func.params[i] =
          var_ty_for_td_var_ty(iru, &var_ty->func.params[i].var_ty);
    }

    return ty;
  }
  case TD_VAR_TY_TY_POINTER: {
    return IR_VAR_TY_POINTER;
  }
  case TD_VAR_TY_TY_ARRAY: {
    struct ir_var_ty underlying =
        var_ty_for_td_var_ty(iru, var_ty->array.underlying);

    return var_ty_make_array(iru, &underlying, var_ty->array.size);
  }
  }
}

UNUSED struct ir_var_ty static var_ty_return_ty_for_td_var_ty(
    struct ir_func_builder *irb, const struct td_var_ty *ty_ref) {
  invariant_assert(ty_ref->ty == TD_VAR_TY_TY_FUNC,
                   "passed non-func to `return_ty_for_td_var_ty`");

  struct ir_var_ty func_ty = var_ty_for_td_var_ty(irb->unit, ty_ref);
  return *func_ty.func.ret_ty;
}

static enum ir_op_cast_op_ty cast_ty_for_td_var_ty(struct ir_func_builder *irb,
                                                   const struct td_var_ty *from,
                                                   const struct td_var_ty *to) {
  struct ir_var_ty from_var_ty = var_ty_for_td_var_ty(irb->unit, from);
  struct ir_var_ty to_var_ty = var_ty_for_td_var_ty(irb->unit, to);

  if (from_var_ty.ty == IR_VAR_TY_TY_POINTER &&
      to_var_ty.ty == IR_VAR_TY_TY_POINTER) {
    bug("cast between pointer types is implicit");
  }

  if (from_var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      to_var_ty.ty == IR_VAR_TY_TY_POINTER) {
    // primitive -> pointer
    // TODO: hardcodes pointer size
    if (from_var_ty.primitive == IR_VAR_PRIMITIVE_TY_I64) {
      bug("cast between primitive & pointer type of same size is implicit");
    }

    if (WKT_IS_SIGNED(to->well_known)) {
      return IR_OP_CAST_OP_TY_SEXT;
    } else {
      return IR_OP_CAST_OP_TY_ZEXT;
    }
  }

  if (from_var_ty.ty == IR_VAR_TY_TY_POINTER &&
      to_var_ty.ty == IR_VAR_TY_TY_PRIMITIVE) {
    return IR_OP_CAST_OP_TY_TRUNC;
  }

  if (from_var_ty.ty != IR_VAR_TY_TY_PRIMITIVE ||
      to_var_ty.ty != IR_VAR_TY_TY_PRIMITIVE) {
    todo("casts for non prims/pointers (from %d -> %d)", from_var_ty.ty,
         to_var_ty.ty);
  }

  if (is_fp_ty(from) && is_fp_ty(to)) {
    return IR_OP_CAST_OP_TY_CONV;
  }

  if (is_fp_ty(from) || is_fp_ty(to)) {
    // one (but not both) is fp
    // we need to generate `uconv`/`iconv` depending on the sign of the integral
    // type

    invariant_assert(from->ty == TD_VAR_TY_TY_WELL_KNOWN ||
                         to->ty == TD_VAR_TY_TY_WELL_KNOWN,
                     "other type must be an integer for float conversion");

    bool is_signed = is_fp_ty(from) ? WKT_IS_SIGNED(to->well_known)
                                    : WKT_IS_SIGNED(from->well_known);

    return is_signed ? IR_OP_CAST_OP_TY_SCONV : IR_OP_CAST_OP_TY_UCONV;
  }

  if (to_var_ty.primitive < from_var_ty.primitive) {
    return IR_OP_CAST_OP_TY_TRUNC;
  } else {
    invariant_assert(from_var_ty.primitive != to_var_ty.primitive,
                     "cast not needed for types of same size");
    if (WKT_IS_SIGNED(to->well_known)) {
      return IR_OP_CAST_OP_TY_SEXT;
    } else {
      return IR_OP_CAST_OP_TY_ZEXT;
    }
  }
}

static struct ir_op *build_ir_for_expr(struct ir_func_builder *irb,
                                       struct ir_stmt **stmt,
                                       struct td_expr *expr);

static struct ir_op *insert_ir_for_cast(struct ir_func_builder *irb,
                                        struct ir_stmt *stmt, struct ir_op *op,
                                        const struct ir_var_ty *to,
                                        enum ir_op_cast_op_ty ty) {
  struct ir_op *cast = alloc_ir_op(irb->func, stmt);

  cast->ty = IR_OP_TY_CAST_OP;
  cast->var_ty = *to;
  cast->cast_op.ty = ty;
  cast->cast_op.value = op;

  return cast;
}

struct ir_build_binaryop {
  enum td_binary_op_ty ty;
  struct td_var_ty result_ty;

  struct td_var_ty lhs_ty, rhs_ty;
  struct ir_op *lhs, *rhs;
};

static struct ir_op *alloc_binaryop(struct ir_func_builder *irb,
                                    struct ir_stmt *stmt,
                                    const struct ir_build_binaryop *args) {

  enum td_binary_op_ty ty = args->ty;
  struct td_var_ty lhs_ty = args->lhs_ty, rhs_ty = args->rhs_ty;
  struct ir_op *lhs = args->lhs, *rhs = args->rhs;
  const struct td_var_ty *td_var_ty = &args->result_ty;

  invariant_assert(lhs->var_ty.ty != IR_VAR_TY_TY_ARRAY ||
                       rhs->var_ty.ty != IR_VAR_TY_TY_ARRAY,
                   "array should have decayed to ptr");

  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, td_var_ty);

  if (!td_binary_op_is_comparison(ty) && (lhs_ty.ty == TD_VAR_TY_TY_POINTER ||
                                          rhs_ty.ty == TD_VAR_TY_TY_POINTER)) {
    if (td_var_ty->ty == TD_VAR_TY_TY_WELL_KNOWN) {
      struct td_var_ty *pointer_ty =
          lhs_ty.ty == TD_VAR_TY_TY_POINTER ? &lhs_ty : &rhs_ty;

      // need to multiply rhs by the element size
      struct ir_var_ty el_ty =
          var_ty_for_td_var_ty(irb->unit, pointer_ty->pointer.underlying);
      struct ir_var_ty_info el_info = var_ty_info(irb->unit, &el_ty);

      struct ir_op *el_size_op = alloc_ir_op(irb->func, stmt);
      make_pointer_constant(irb->unit, el_size_op, el_info.size);

      struct ir_op *diff = alloc_ir_op(irb->func, stmt);
      diff->ty = IR_OP_TY_BINARY_OP;
      diff->var_ty = var_ty;
      diff->binary_op.ty = IR_OP_BINARY_OP_TY_SUB;
      diff->binary_op.lhs = lhs;
      diff->binary_op.rhs = rhs;

      struct ir_op *op = alloc_ir_op(irb->func, stmt);
      op->ty = IR_OP_TY_BINARY_OP;
      op->var_ty = var_ty;
      op->binary_op.ty = IR_OP_BINARY_OP_TY_SDIV;
      op->binary_op.lhs = diff;
      op->binary_op.rhs = el_size_op;

      return op;
    } else {
      debug_assert(td_var_ty->ty == TD_VAR_TY_TY_POINTER, "non pointer");

      // need to multiply rhs by the element size
      struct ir_var_ty el_ty =
          var_ty_for_td_var_ty(irb->unit, td_var_ty->pointer.underlying);
      struct ir_var_ty_info el_info = var_ty_info(irb->unit, &el_ty);

      struct ir_op *el_size_op = alloc_ir_op(irb->func, stmt);
      make_pointer_constant(irb->unit, el_size_op, el_info.size);

      struct ir_op *rhs_mul = alloc_ir_op(irb->func, stmt);
      rhs_mul->ty = IR_OP_TY_BINARY_OP;
      rhs_mul->var_ty = var_ty;
      rhs_mul->binary_op.ty = IR_OP_BINARY_OP_TY_MUL;
      rhs_mul->binary_op.lhs = el_size_op;
      rhs_mul->binary_op.rhs = rhs;

      struct ir_op *op = alloc_ir_op(irb->func, stmt);
      op->ty = IR_OP_TY_BINARY_OP;
      op->var_ty = var_ty;
      op->binary_op.ty = ty == TD_BINARY_OP_TY_ADD ? IR_OP_BINARY_OP_TY_ADD
                                                   : IR_OP_BINARY_OP_TY_SUB;
      op->binary_op.lhs = lhs;
      op->binary_op.rhs = rhs_mul;

      return op;
    }
  }

  struct ir_op *op = alloc_ir_op(irb->func, stmt);
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = var_ty;

  struct ir_op_binary_op *b = &op->binary_op;

  b->lhs = lhs;
  b->rhs = rhs;

  bool is_fp = var_ty_is_fp(&op->binary_op.lhs->var_ty);
  debug_assert(is_fp == var_ty_is_fp(&op->binary_op.rhs->var_ty),
               "type mismatch between lhs/rhs");

  invariant_assert(
      td_var_ty->ty == TD_VAR_TY_TY_WELL_KNOWN ||
          td_var_ty->ty == TD_VAR_TY_TY_POINTER,
      "non primitives/well-knowns/pointers cannot be used in binary "
      "expression by point IR is reached!");

  switch (ty) {
  case TD_BINARY_OP_TY_LOGICAL_AND:
  case TD_BINARY_OP_TY_LOGICAL_OR:
    bug("logical and/or must be handled outside (as they need basicblock "
        "adjustment)");
  case TD_BINARY_OP_TY_EQ:
    b->ty = is_fp ? IR_OP_BINARY_OP_TY_FEQ : IR_OP_BINARY_OP_TY_EQ;
    break;
  case TD_BINARY_OP_TY_NEQ:
    b->ty = is_fp ? IR_OP_BINARY_OP_TY_FNEQ : IR_OP_BINARY_OP_TY_NEQ;
    break;
  case TD_BINARY_OP_TY_GT:
    if (is_fp) {
      b->ty = IR_OP_BINARY_OP_TY_FGT;
    } else if (WKT_IS_SIGNED(td_var_ty->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SGT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UGT;
    }
    break;
  case TD_BINARY_OP_TY_GTEQ:
    if (is_fp) {
      b->ty = IR_OP_BINARY_OP_TY_FGTEQ;
    } else if (WKT_IS_SIGNED(td_var_ty->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SGTEQ;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UGTEQ;
    }
    break;
  case TD_BINARY_OP_TY_LT:
    if (is_fp) {
      b->ty = IR_OP_BINARY_OP_TY_FLT;
    } else if (WKT_IS_SIGNED(td_var_ty->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SLT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_ULT;
    }
    break;
  case TD_BINARY_OP_TY_LTEQ:
    if (is_fp) {
      b->ty = IR_OP_BINARY_OP_TY_FLTEQ;
    } else if (WKT_IS_SIGNED(td_var_ty->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SLTEQ;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_ULTEQ;
    }
    break;
  case TD_BINARY_OP_TY_RSHIFT:
    if (WKT_IS_SIGNED(td_var_ty->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SRSHIFT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_URSHIFT;
    }
    break;
  case TD_BINARY_OP_TY_LSHIFT:
    b->ty = IR_OP_BINARY_OP_TY_LSHIFT;
    break;
  case TD_BINARY_OP_TY_AND:
    b->ty = IR_OP_BINARY_OP_TY_AND;
    break;
  case TD_BINARY_OP_TY_OR:
    b->ty = IR_OP_BINARY_OP_TY_OR;
    break;
  case TD_BINARY_OP_TY_XOR:
    b->ty = IR_OP_BINARY_OP_TY_XOR;
    break;
  case TD_BINARY_OP_TY_ADD:
    b->ty = is_fp ? IR_OP_BINARY_OP_TY_FADD : IR_OP_BINARY_OP_TY_ADD;
    break;
  case TD_BINARY_OP_TY_SUB:
    b->ty = is_fp ? IR_OP_BINARY_OP_TY_FSUB : IR_OP_BINARY_OP_TY_SUB;
    break;
  case TD_BINARY_OP_TY_MUL:
    b->ty = is_fp ? IR_OP_BINARY_OP_TY_FMUL : IR_OP_BINARY_OP_TY_MUL;
    break;
  case TD_BINARY_OP_TY_DIV:
    if (is_fp) {
      b->ty = IR_OP_BINARY_OP_TY_FDIV;
    } else if (WKT_IS_SIGNED(td_var_ty->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SDIV;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UDIV;
    }
    break;
  case TD_BINARY_OP_TY_QUOT:
    if (WKT_IS_SIGNED(td_var_ty->well_known)) {
      b->ty = IR_OP_BINARY_OP_TY_SQUOT;
    } else {
      b->ty = IR_OP_BINARY_OP_TY_UQUOT;
    }
    break;
  }

  return op;
}

static struct ir_op *build_ir_for_array_address(struct ir_func_builder *irb,
                                                struct ir_stmt **stmt,
                                                struct td_expr *lhs_expr,
                                                struct td_expr *rhs_expr);

static struct ir_op *build_ir_for_member_address(struct ir_func_builder *irb,
                                                 struct ir_stmt **stmt,
                                                 struct td_expr *lhs_expr,
                                                 const char *member_name);

static struct ir_op *build_ir_for_pointer_address(struct ir_func_builder *irb,
                                                  struct ir_stmt **stmt,
                                                  struct td_expr *lhs_expr,
                                                  const char *member_name);

static struct ir_op *build_ir_for_addressof_var(struct ir_func_builder *irb,
                                                struct ir_stmt **stmt,
                                                struct td_var *var) {
  struct var_key key;
  struct var_ref *ref;
  get_var_ref(irb, NULL, var, &key, &ref);

  struct ir_var_ty var_ty = IR_VAR_TY_POINTER;

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_ADDR;

  switch (ref->ty) {
  case VAR_REF_TY_SSA:
    ref->ty = VAR_REF_TY_LCL;

    if (ref->op) {
      spill_op(irb->func, ref->op);
      ref->lcl = ref->op->lcl;
    } else {
      ref->lcl = add_local(irb->func, &var_ty);
      op->lcl = ref->lcl;
    }

    op->var_ty = var_ty;
    op->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = ref->lcl};
    break;
  case VAR_REF_TY_LCL:
    if (!ref->lcl) {
      ref->lcl = add_local(irb->func, &var_ty);
    }

    op->var_ty = var_ty;
    op->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = ref->lcl};
    break;
  case VAR_REF_TY_GLB:
    op->var_ty = var_ty;
    op->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = ref->glb};
    break;
  }

  return op;
}

static struct ir_op *build_ir_for_addressof(struct ir_func_builder *irb,
                                            struct ir_stmt **stmt,
                                            struct td_expr *expr) {
  // address of does not actually "read" its underlying expression
  // so we do not build the expression

  switch (expr->ty) {
  case TD_EXPR_TY_ARRAYACCESS: {
    return build_ir_for_array_address(irb, stmt, expr->array_access.lhs,
                                      expr->array_access.rhs);
  }
  case TD_EXPR_TY_MEMBERACCESS: {
    return build_ir_for_member_address(irb, stmt, expr->member_access.lhs,
                                       expr->member_access.member);
  }
  case TD_EXPR_TY_POINTERACCESS: {
    return build_ir_for_pointer_address(irb, stmt, expr->pointer_access.lhs,
                                        expr->pointer_access.member);
  }
  case TD_EXPR_TY_COMPOUNDEXPR: {
    return build_ir_for_addressof(
        irb, stmt,
        &expr->compound_expr.exprs[expr->compound_expr.num_exprs - 1]);
  }
  default:
    break;
  }

  if (expr->ty == TD_EXPR_TY_UNARY_OP &&
      expr->unary_op.ty == TD_UNARY_OP_TY_INDIRECTION) {
    // &*, so cancel
    return build_ir_for_expr(irb, stmt, expr->unary_op.expr);
  }

  if (expr->ty != TD_EXPR_TY_VAR) {
    todo("unknown type for addressof");
  }

  return build_ir_for_addressof_var(irb, stmt, &expr->var);
}

static struct ir_op *build_ir_for_assg(struct ir_func_builder *irb,
                                       struct ir_stmt **stmt,
                                       struct td_expr *expr);

static struct ir_op *build_ir_for_unaryop(struct ir_func_builder *irb,
                                          struct ir_stmt **stmt,
                                          struct td_expr *expr) {
  struct td_unary_op *unary_op = &expr->unary_op;

  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &expr->var_ty);

  if (unary_op->ty == TD_UNARY_OP_TY_ADDRESSOF) {
    return build_ir_for_addressof(irb, stmt, unary_op->expr);
  }

  struct ir_op *ir_expr = build_ir_for_expr(irb, stmt, unary_op->expr);

  if (unary_op->ty == TD_UNARY_OP_TY_INDIRECTION) {
    // does not generate a unary op instead generates a LOAD_ADDR
    struct ir_op *op = alloc_ir_op(irb->func, *stmt);
    op->ty = IR_OP_TY_LOAD_ADDR;
    op->var_ty = var_ty;
    op->load_addr = (struct ir_op_load_addr){.addr = ir_expr};

    return op;
  }

  bool is_postfix;
  enum td_assg_ty assg_ty;

  switch (unary_op->ty) {
  case TD_UNARY_OP_TY_PREFIX_DEC:
  case TD_UNARY_OP_TY_PREFIX_INC:
    is_postfix = false;
    assg_ty = unary_op->ty == TD_UNARY_OP_TY_PREFIX_INC ? TD_ASSG_TY_ADD
                                                        : TD_ASSG_TY_SUB;

    goto inc_dec;
  case TD_UNARY_OP_TY_POSTFIX_INC:
  case TD_UNARY_OP_TY_POSTFIX_DEC:
    is_postfix = true;
    assg_ty = unary_op->ty == TD_UNARY_OP_TY_POSTFIX_INC ? TD_ASSG_TY_ADD
                                                         : TD_ASSG_TY_SUB;
    goto inc_dec;

  inc_dec : {
    // if we are decrementing a pointer/array, we need to make sure we don't
    // build an expr that is PTR - PTR as this will do a "pointer subtract"
    // rather than "pointer minus integer" so we give the constant a
    // pointer-sized-integer-type, rather than pointer type
    struct td_var_ty cnst_ty;
    if (unary_op->expr->var_ty.ty == TD_VAR_TY_TY_POINTER ||
        unary_op->expr->var_ty.ty == TD_VAR_TY_TY_ARRAY) {
      cnst_ty = td_var_ty_pointer_sized_int(irb->tchk, false);
    } else {
      cnst_ty = unary_op->expr->var_ty;
    }

    struct td_expr one = {
        .ty = TD_EXPR_TY_CNST,
        .var_ty = cnst_ty,
        .cnst = (struct td_cnst){.ty = TD_CNST_TY_SIGNED_INT, .int_value = 1}};

    struct td_assg td_assg = {
        .ty = assg_ty,
        .expr = &one,
        .assignee = unary_op->expr,
    };

    struct td_expr td_expr = {
        .ty = TD_EXPR_TY_ASSG, .var_ty = expr->var_ty, .assg = td_assg};

    struct ir_op *assg = build_ir_for_assg(irb, stmt, &td_expr);

    if (is_postfix) {
      return ir_expr;
    } else {
      return assg;
    }
  }
  case TD_UNARY_OP_TY_PLUS:
    // no work needed, build_expr will handle type conversion
    return ir_expr;
  case TD_UNARY_OP_TY_SIZEOF:
  case TD_UNARY_OP_TY_ALIGNOF:
    todo("sizeof/alignof build (will need different node as they take types "
         "not exprs)");
  case TD_UNARY_OP_TY_CAST:
    if (var_ty_needs_cast_op(irb, &var_ty, &ir_expr->var_ty)) {
      return insert_ir_for_cast(
          irb, *stmt, ir_expr, &var_ty,
          cast_ty_for_td_var_ty(irb, &unary_op->expr->var_ty, &expr->var_ty));
    } else {
      ir_expr->var_ty = var_ty_for_td_var_ty(irb->unit, &expr->var_ty);
      return ir_expr;
    }
  default:
    break;
  }

  enum ir_op_unary_op_ty unary_op_ty;
  switch (unary_op->ty) {
  case TD_UNARY_OP_TY_MINUS:
    unary_op_ty = is_fp_ty(&expr->var_ty) ? IR_OP_UNARY_OP_TY_FNEG
                                          : IR_OP_UNARY_OP_TY_NEG;
    break;
  case TD_UNARY_OP_TY_LOGICAL_NOT:
    unary_op_ty = IR_OP_UNARY_OP_TY_LOGICAL_NOT;
    break;
  case TD_UNARY_OP_TY_NOT:
    unary_op_ty = IR_OP_UNARY_OP_TY_NOT;
    break;
  default:
    bug("unexpected unary_op_ty in `%s`", __func__);
  }

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_UNARY_OP;
  op->var_ty = var_ty;
  op->unary_op.ty = unary_op_ty;
  op->unary_op.value = ir_expr;

  return op;
}

static struct ir_op *build_ir_for_binaryop(struct ir_func_builder *irb,
                                           struct ir_stmt **stmt,
                                           struct td_expr *expr) {
  struct td_binary_op *binary_op = &expr->binary_op;
  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &expr->var_ty);

  struct ir_op *lhs = build_ir_for_expr(irb, stmt, binary_op->lhs);

  if (binary_op->ty == TD_BINARY_OP_TY_LOGICAL_AND ||
      binary_op->ty == TD_BINARY_OP_TY_LOGICAL_OR) {
    struct ir_basicblock *entry_bb = (*stmt)->basicblock;
    struct ir_basicblock *rhs_bb = alloc_ir_basicblock(irb->func);
    struct ir_basicblock *end_bb = alloc_ir_basicblock(irb->func);

    if (binary_op->ty == TD_BINARY_OP_TY_LOGICAL_AND) {
      make_basicblock_split(irb->func, entry_bb, rhs_bb, end_bb);
    } else {
      make_basicblock_split(irb->func, entry_bb, end_bb, rhs_bb);
    }

    struct ir_stmt *entry_stmt = alloc_ir_stmt(irb->func, entry_bb);
    struct ir_op *lhs_br = alloc_ir_op(irb->func, entry_stmt);
    lhs_br->ty = IR_OP_TY_BR_COND;
    lhs_br->var_ty = IR_VAR_TY_NONE;
    lhs_br->br_cond = (struct ir_op_br_cond){.cond = lhs};

    struct ir_stmt *rhs_stmt = alloc_ir_stmt(irb->func, rhs_bb);
    struct ir_op *rhs = build_ir_for_expr(irb, &rhs_stmt, binary_op->rhs);

    struct ir_op *rhs_br = alloc_ir_op(irb->func, rhs_stmt);
    rhs_br->ty = IR_OP_TY_BR;
    rhs_br->var_ty = IR_VAR_TY_NONE;
    make_basicblock_merge(irb->func, rhs_bb, end_bb);

    struct ir_stmt *end_stmt = alloc_ir_stmt(irb->func, end_bb);
    struct ir_op *phi = alloc_ir_op(irb->func, end_stmt);

    phi->ty = IR_OP_TY_PHI;
    phi->var_ty = var_ty;
    phi->phi = (struct ir_op_phi){
        .num_values = 2,
        .values = arena_alloc(irb->arena, sizeof(struct ir_phi_entry *) * 2)};

    phi->phi.values[0] = (struct ir_phi_entry){
        .basicblock = lhs->stmt->basicblock, .value = lhs};
    phi->phi.values[1] = (struct ir_phi_entry){
        .basicblock = rhs->stmt->basicblock, .value = rhs};

    *stmt = phi->stmt;
    return phi;
  }

  struct ir_op *rhs = build_ir_for_expr(irb, stmt, binary_op->rhs);

  struct ir_build_binaryop args = {
      .ty = binary_op->ty,
      .result_ty = expr->var_ty,
      .lhs_ty = binary_op->lhs->var_ty,
      .rhs_ty = binary_op->rhs->var_ty,
      .lhs = lhs,
      .rhs = rhs,
  };

  return alloc_binaryop(irb, *stmt, &args);
}

static struct ir_op *build_ir_for_sizeof(struct ir_func_builder *irb,
                                         struct ir_stmt **stmt,
                                         struct td_expr *expr) {
  struct td_sizeof *size_of = &expr->size_of;
  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &expr->var_ty);

  struct ir_var_ty size_var_ty;
  switch (size_of->ty) {
  case TD_SIZEOF_TY_TYPE:
    size_var_ty = var_ty_for_td_var_ty(irb->unit, &size_of->var_ty);
    break;
  case TD_SIZEOF_TY_EXPR:
    size_var_ty = var_ty_for_td_var_ty(irb->unit, &size_of->expr->var_ty);
    break;
  }

  struct ir_var_ty_info info = var_ty_info(irb->unit, &size_var_ty);

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_CNST;
  op->var_ty = var_ty;
  op->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = info.size};

  return op;
}

static struct ir_op *build_ir_for_alignof(struct ir_func_builder *irb,
                                          struct ir_stmt **stmt,
                                          struct td_expr *expr) {
  struct td_alignof *align_of = &expr->align_of;
  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &expr->var_ty);

  struct ir_var_ty align_var_ty =
      var_ty_for_td_var_ty(irb->unit, &align_of->var_ty);
  struct ir_var_ty_info info = var_ty_info(irb->unit, &align_var_ty);

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_CNST;
  op->var_ty = var_ty;
  op->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = info.alignment};

  return op;
}

static struct ir_op *build_ir_for_cnst(struct ir_func_builder *irb,
                                       struct ir_stmt **stmt,
                                       struct ir_var_ty var_ty,
                                       struct td_cnst *cnst) {
  struct ir_op *op = alloc_ir_op(irb->func, *stmt);

  switch (cnst->ty) {
  case TD_CNST_TY_CHAR:
  case TD_CNST_TY_WIDE_CHAR:
  case TD_CNST_TY_SIGNED_INT:
  case TD_CNST_TY_UNSIGNED_INT:
  case TD_CNST_TY_SIGNED_LONG:
  case TD_CNST_TY_UNSIGNED_LONG:
  case TD_CNST_TY_SIGNED_LONG_LONG:
  case TD_CNST_TY_UNSIGNED_LONG_LONG:
    op->ty = IR_OP_TY_CNST;
    op->var_ty = var_ty;
    op->cnst.ty = IR_OP_CNST_TY_INT;
    op->cnst.int_value = cnst->int_value;
    break;
  case TD_CNST_TY_FLOAT:
  case TD_CNST_TY_DOUBLE:
  case TD_CNST_TY_LONG_DOUBLE:
    op->ty = IR_OP_TY_CNST;
    op->var_ty = var_ty;
    op->cnst.ty = IR_OP_CNST_TY_FLT;
    op->cnst.flt_value = cnst->flt_value;
    break;
  case TD_CNST_TY_STR_LITERAL:
  case TD_CNST_TY_WIDE_STR_LITERAL: {
    struct ir_glb *glb = add_global(irb->unit, IR_GLB_TY_DATA, &var_ty,
                                    IR_GLB_DEF_TY_DEFINED, NULL);
    glb->var = arena_alloc(irb->arena, sizeof(*glb->var));
    *glb->var = (struct ir_var){.ty = IR_VAR_TY_STRING_LITERAL,
                                .value = {.ty = IR_VAR_VALUE_TY_STR,
                                          .var_ty = var_ty,
                                          .str_value = cnst->str_value}};

    op->ty = IR_OP_TY_ADDR;
    op->var_ty = var_ty;
    op->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = glb};

    break;
  }
  }

  return op;
}

static struct ir_op *
build_ir_for_compoundexpr(struct ir_func_builder *irb, struct ir_stmt **stmt,
                          UNUSED_ARG(struct ir_var_ty var_ty),
                          struct td_compoundexpr *compound_expr) {
  struct ir_op *op = NULL;
  for (size_t i = 0; i < compound_expr->num_exprs; i++) {
    op = build_ir_for_expr(irb, stmt, &compound_expr->exprs[i]);
  }

  return op;
}

static struct ir_op *build_ir_for_ternary(struct ir_func_builder *irb,
                                          struct ir_stmt **stmt,
                                          struct ir_var_ty var_ty,
                                          struct td_ternary *ternary) {
  struct ir_op *cond = build_ir_for_expr(irb, stmt, ternary->cond);
  struct ir_op *br_cond = alloc_ir_op(irb->func, *stmt);
  br_cond->ty = IR_OP_TY_BR_COND;
  br_cond->var_ty = IR_VAR_TY_NONE;
  br_cond->br_cond = (struct ir_op_br_cond){.cond = cond};

  struct ir_basicblock *pre_cond_bb = (*stmt)->basicblock;
  struct ir_basicblock *true_bb = alloc_ir_basicblock(irb->func);
  struct ir_basicblock *false_bb = alloc_ir_basicblock(irb->func);
  struct ir_basicblock *end_bb = alloc_ir_basicblock(irb->func);

  make_basicblock_split(irb->func, pre_cond_bb, true_bb, false_bb);
  make_basicblock_merge(irb->func, true_bb, end_bb);
  make_basicblock_merge(irb->func, false_bb, end_bb);

  struct ir_stmt *true_stmt = alloc_ir_stmt(irb->func, true_bb);
  struct ir_op *true_op =
      build_ir_for_expr(irb, &true_stmt, ternary->true_expr);

  struct ir_op *true_br = alloc_ir_op(irb->func, true_stmt);
  true_br->ty = IR_OP_TY_BR;
  true_br->var_ty = IR_VAR_TY_NONE;

  struct ir_stmt *false_stmt = alloc_ir_stmt(irb->func, false_bb);
  struct ir_op *false_op =
      build_ir_for_expr(irb, &false_stmt, ternary->false_expr);
  struct ir_op *false_br = alloc_ir_op(irb->func, false_stmt);
  false_br->ty = IR_OP_TY_BR;
  false_br->var_ty = IR_VAR_TY_NONE;

  struct ir_stmt *end_stmt = alloc_ir_stmt(irb->func, end_bb);
  struct ir_op *phi = alloc_ir_op(irb->func, end_stmt);
  phi->ty = IR_OP_TY_PHI;
  phi->var_ty = var_ty;
  phi->phi = (struct ir_op_phi){
      .num_values = 2,
      .values = arena_alloc(irb->arena, sizeof(struct ir_op *) * 2),
  };

  phi->phi.values[0] =
      (struct ir_phi_entry){.basicblock = false_bb, .value = false_op};
  phi->phi.values[1] =
      (struct ir_phi_entry){.basicblock = true_bb, .value = true_op};

  *stmt = end_stmt;
  return phi;
}

static struct ir_op *build_ir_for_var(struct ir_func_builder *irb,
                                      struct ir_stmt **stmt,
                                      struct ir_var_ty var_ty,
                                      struct td_var *var) {
  // if `a` is an array/function, then reading `a` is actually `&a[0]`/&a
  // same with functions
  if (var_ty.ty == IR_VAR_TY_TY_ARRAY || var_ty.ty == IR_VAR_TY_TY_FUNC) {
    return build_ir_for_addressof_var(irb, stmt, var);
  }

  if (var->ty == TD_VAR_VAR_TY_ENUMERATOR) {
    struct ir_op *op = alloc_ir_op(irb->func, *stmt);
    op->ty = IR_OP_TY_CNST;
    op->var_ty = var_ty;
    op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                   .int_value = var->enumerator};

    return op;
  }

  struct var_key key;
  struct var_ref *ref;
  get_var_ref(irb, (*stmt)->basicblock, var, &key, &ref);

  switch (var->ty) {
  case TD_VAR_VAR_TY_ENUMERATOR:
    unreachable();

  case TD_VAR_VAR_TY_VAR: {
    // this is when we are _reading_ from the var

    if (ref) {
      switch (ref->ty) {
      case VAR_REF_TY_SSA:
        return ref->op;
      case VAR_REF_TY_LCL: {
        debug_assert(ref->lcl, "VAR_REF_TY_LCL but op %zu had no lcl",
                     ref->op->id);

        struct ir_op *op = alloc_ir_op(irb->func, *stmt);
        op->ty = IR_OP_TY_LOAD_LCL;

        if (var_ty.ty == IR_VAR_TY_TY_ARRAY) {
          // pointer decay
          op->var_ty = IR_VAR_TY_POINTER;
        } else {
          op->var_ty = var_ty;
        }
        op->load_lcl = (struct ir_op_load_lcl){.lcl = ref->lcl};

        return op;
      }
      case VAR_REF_TY_GLB: {
        struct ir_op *op = alloc_ir_op(irb->func, *stmt);
        op->ty = IR_OP_TY_LOAD_GLB;

        if (var_ty.ty == IR_VAR_TY_TY_ARRAY) {
          // pointer decay
          op->var_ty = IR_VAR_TY_POINTER;
        } else {
          op->var_ty = var_ty;
        }
        op->load_glb = (struct ir_op_load_glb){.glb = ref->glb};

        return op;
      }
      }
    }
  }
  }

  // invariant_assert(var_var_ty.ty != TD_VAR_TY_TY_UNKNOWN,
  //                  "can't have unknown tyref in phi lowering");

  // we generate an empty phi and then after all blocks are built we insert the
  // correct values
  // all phis appear at the start of their bb as they execute ""
  struct ir_op *phi = insert_phi(irb->func, (*stmt)->basicblock, var_ty);

  phi->metadata = arena_alloc(irb->arena, sizeof(struct td_var));
  *(struct td_var *)phi->metadata = *var;

  key = get_var_key(var, (*stmt)->basicblock);
  struct var_ref *new_ref = var_refs_add(irb->var_refs, &key, VAR_REF_TY_SSA);
  new_ref->ty = VAR_REF_TY_SSA;
  new_ref->op = phi;

  return phi;
}

static struct ir_op *build_ir_for_call(struct ir_func_builder *irb,
                                       struct ir_stmt **stmt,
                                       struct td_expr *expr) {
  // need to generate args and target IR first to keep IR in order
  struct td_call *call = &expr->call;

  struct ir_op **args =
      arena_alloc(irb->arena, sizeof(struct ir_op *) * call->arg_list.num_args);

  size_t num_non_variadic_args = call->target->var_ty.func.num_params;

  struct td_expr *target_expr = call->target;

  struct ir_var_ty func_ty;

  // one level deref can occur
  if (target_expr->var_ty.ty == TD_VAR_TY_TY_POINTER ||
      target_expr->var_ty.ty == TD_VAR_TY_TY_ARRAY) {
    struct td_var_ty underlying =
        td_var_ty_get_underlying(irb->tchk, &target_expr->var_ty);
    func_ty = var_ty_for_td_var_ty(irb->unit, &underlying);
  } else {
    func_ty = var_ty_for_td_var_ty(irb->unit, &target_expr->var_ty);
  }

  debug_assert(func_ty.ty == IR_VAR_TY_TY_FUNC,
               "expected target to be func ty");

  for (size_t i = 0; i < call->arg_list.num_args; i++) {
    args[i] = build_ir_for_expr(irb, stmt, &call->arg_list.args[i]);

    if (i >= num_non_variadic_args) {
      args[i]->flags |= IR_OP_FLAG_VARIADIC_PARAM;
    }
  }

  // if the target is a function name, we want to take address
  // else, we want to use value
  struct ir_op *target;
  if (call->target->var_ty.ty == TD_VAR_TY_TY_POINTER) {
    target = build_ir_for_expr(irb, stmt, call->target);
  } else {
    target = build_ir_for_addressof(irb, stmt, call->target);
  }

  irb->func->flags |= IR_FUNC_FLAG_MAKES_CALL;
  struct ir_op *op = alloc_ir_op(irb->func, *stmt);

  op->ty = IR_OP_TY_CALL;
  op->var_ty = *func_ty.func.ret_ty;

  op->call.func_ty = func_ty;
  op->call.target = target;
  op->call.num_args = call->arg_list.num_args;
  op->call.args = args;

  return op;
}

static void var_assg_glb(struct ir_func_builder *irb, struct ir_stmt *stmt,
                         struct ir_glb *glb, struct td_var *var) {

  debug_assert(glb, "null glb in assignment!");

  struct var_key key;
  struct var_ref *ref;
  get_var_ref(irb, stmt->basicblock, var, &key, &ref);

  if (!ref) {
    ref = var_refs_add(irb->var_refs, &key, VAR_REF_TY_GLB);
  }

  ref->glb = glb;
}

static struct ir_op *var_assg(struct ir_func_builder *irb, struct ir_stmt *stmt,
                              struct ir_op *op, struct td_var *var) {
  struct var_key key;
  struct var_ref *ref;
  get_var_ref(irb, stmt->basicblock, var, &key, &ref);

  if (!ref) {
    ref = var_refs_add(irb->var_refs, &key, VAR_REF_TY_SSA);
  }

  switch (ref->ty) {
  case VAR_REF_TY_SSA:
    ref->op = op;
    return op;
  case VAR_REF_TY_LCL: {
    // FIXME: is this right
    struct ir_op *ld = alloc_ir_op(irb->func, stmt);
    ld->ty = IR_OP_TY_STORE_LCL;
    ld->var_ty = op->var_ty;
    ld->store_lcl = (struct ir_op_store_lcl){.value = op};

    // its okay that we use the thing assigned to the global, rather than
    // reloading the global
    return op;
  }
  case VAR_REF_TY_GLB: {
    // FIXME: is this right
    struct ir_op *ld = alloc_ir_op(irb->func, stmt);
    ld->ty = IR_OP_TY_STORE_GLB;
    ld->var_ty = op->var_ty;
    ld->store_glb = (struct ir_op_store_glb){.glb = ref->glb, .value = op};

    // its okay that we use the thing assigned to the global, rather than
    // reloading the global
    return op;
  }
  }
}

static void get_member_info(struct ir_unit *iru,
                            const struct td_var_ty *aggregate,
                            const char *member_name,
                            struct ir_var_ty *member_ty, size_t *member_idx,
                            size_t *member_offset,
                            struct td_var_ty *td_member_ty) {
  debug_assert(aggregate->ty == TD_VAR_TY_TY_AGGREGATE, "expected aggregate");

  *member_ty = IR_VAR_TY_NONE;

  size_t idx;
  if (!member_idx) {
    member_idx = &idx;
  }

  *member_idx = 0;
  for (; *member_idx < aggregate->aggregate.num_fields; (*member_idx)++) {
    struct td_struct_field *field = &aggregate->aggregate.fields[*member_idx];
    if (strcmp(field->identifier, member_name) == 0) {
      if (td_member_ty) {
        *td_member_ty = field->var_ty;
      }

      *member_ty = var_ty_for_td_var_ty(iru, &field->var_ty);
      if (member_ty->ty == IR_VAR_TY_TY_ARRAY) {
        // pointer decay
        *member_ty = *member_ty->array.underlying;
      }
      break;
    }
  }

  struct ir_var_ty ir_struct_ty = var_ty_for_td_var_ty(iru, aggregate);
  struct ir_var_ty_info info = var_ty_info(iru, &ir_struct_ty);

  // offsets are null for a union
  *member_offset = info.offsets ? info.offsets[*member_idx] : 0;
}

static struct ir_op *build_ir_for_member_address_offset(
    struct ir_func_builder *irb, struct ir_stmt **stmt,
    const struct td_var_ty *struct_ty, const char *member_name,
    struct ir_var_ty *member_ty, struct td_var_ty *td_member_ty) {

  size_t member_offset;
  size_t idx;
  get_member_info(irb->unit, struct_ty, member_name, member_ty, &idx,
                  &member_offset, td_member_ty);

  struct ir_op *offset = alloc_ir_op(irb->func, *stmt);
  offset->ty = IR_OP_TY_CNST;
  offset->var_ty = (struct ir_var_ty){.ty = IR_VAR_TY_TY_PRIMITIVE,
                                      .primitive = IR_VAR_PRIMITIVE_TY_I64};
  offset->cnst =
      (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = member_offset};

  return offset;
}

static struct ir_op *build_ir_for_member_address(struct ir_func_builder *irb,
                                                 struct ir_stmt **stmt,
                                                 struct td_expr *lhs_expr,
                                                 const char *member_name) {
  struct ir_op *lhs = build_ir_for_addressof(irb, stmt, lhs_expr);

  struct ir_var_ty member_ty;
  struct ir_op *rhs = build_ir_for_member_address_offset(
      irb, stmt, &lhs_expr->var_ty, member_name, &member_ty, NULL);

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = IR_VAR_TY_POINTER;
  op->binary_op = (struct ir_op_binary_op){
      .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = lhs, .rhs = rhs};

  return op;
}

static struct ir_op *build_ir_for_pointer_address(struct ir_func_builder *irb,
                                                  struct ir_stmt **stmt,
                                                  struct td_expr *lhs_expr,
                                                  const char *member_name) {
  debug_assert(lhs_expr->var_ty.ty == TD_VAR_TY_TY_POINTER,
               "makes no sense except on LHS pointer");

  struct ir_op *lhs = build_ir_for_expr(irb, stmt, lhs_expr);

  struct ir_var_ty member_ty;
  struct ir_op *rhs = build_ir_for_member_address_offset(
      irb, stmt, lhs_expr->var_ty.pointer.underlying, member_name, &member_ty,
      NULL);

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_BINARY_OP;
  op->var_ty = IR_VAR_TY_POINTER;
  op->binary_op = (struct ir_op_binary_op){
      .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = lhs, .rhs = rhs};

  return op;
}

static bool is_integral_cnst(struct ir_func_builder *irb,
                             const struct td_expr *expr,
                             unsigned long long *value) {
  if (expr->ty == TD_EXPR_TY_UNARY_OP &&
      expr->unary_op.ty == TD_UNARY_OP_TY_CAST) {
    return is_integral_cnst(irb, expr->unary_op.expr, value);
  }

  if (expr->ty != TD_EXPR_TY_CNST) {
    return false;
  }

  switch (expr->cnst.ty) {
  case TD_CNST_TY_FLOAT:
  case TD_CNST_TY_DOUBLE:
  case TD_CNST_TY_LONG_DOUBLE:
  case TD_CNST_TY_CHAR:
  case TD_CNST_TY_WIDE_CHAR:
  case TD_CNST_TY_STR_LITERAL:
  case TD_CNST_TY_WIDE_STR_LITERAL:
    return false;
  default:
    break;
  }

  *value = expr->cnst.int_value;
  return true;
}

static struct ir_op *build_ir_for_array_address(struct ir_func_builder *irb,
                                                struct ir_stmt **stmt,
                                                struct td_expr *lhs_expr,
                                                struct td_expr *rhs_expr) {
  struct td_var_ty pointer_ty, lhs_ty;
  struct td_var_ty rhs_ty = rhs_expr->var_ty;

  struct ir_op *lhs;
  if (lhs_expr->var_ty.ty == TD_VAR_TY_TY_ARRAY) {
    // need to decay the type to pointer
    struct td_var_ty *underlying = lhs_expr->var_ty.array.underlying;
    lhs = build_ir_for_addressof(irb, stmt, lhs_expr);
    pointer_ty = td_var_ty_make_pointer(irb->tchk, underlying,
                                        TD_TYPE_QUALIFIER_FLAG_NONE);

    lhs_ty = pointer_ty;
  } else {
    lhs = build_ir_for_expr(irb, stmt, lhs_expr);
    pointer_ty = lhs_expr->var_ty;
    lhs_ty = lhs_expr->var_ty;
  }

  unsigned long long idx = 0;
  if (is_integral_cnst(irb, rhs_expr, &idx)) {
    // make IR clearer for constant indices even when optimisation is turned off

    if (idx == 0) {
      return lhs;
    }

    struct td_var_ty underlying = td_var_ty_get_underlying(irb->tchk, &lhs_ty);
    struct ir_var_ty el_ty = var_ty_for_td_var_ty(irb->unit, &underlying);
    struct ir_var_ty_info info = var_ty_info(irb->unit, &el_ty);
    unsigned long long offset_value = idx * info.size;

    struct ir_op *cnst = alloc_ir_op(irb->func, *stmt);
    cnst->ty = IR_OP_TY_CNST;
    cnst->var_ty = IR_VAR_TY_POINTER;
    cnst->cnst =
        (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = offset_value};

    struct ir_op *offset = alloc_ir_op(irb->func, *stmt);
    offset->ty = IR_OP_TY_BINARY_OP;
    offset->var_ty = IR_VAR_TY_POINTER;
    offset->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_ADD,
        .lhs = lhs,
        .rhs = cnst,
    };

    return offset;
  }

  // need to promote rhs to pointer size int
  debug_assert(rhs_expr->var_ty.ty == TD_VAR_TY_TY_WELL_KNOWN,
               "expected well-known ty rhs");

  struct ir_op *rhs = build_ir_for_expr(irb, stmt, rhs_expr);

  struct ir_build_binaryop args = {
      .ty = TD_BINARY_OP_TY_ADD,
      .result_ty = pointer_ty,
      .lhs_ty = lhs_ty,
      .rhs_ty = rhs_ty,
      .lhs = lhs,
      .rhs = rhs,
  };

  return alloc_binaryop(irb, *stmt, &args);
}

static struct ir_op *build_ir_for_assg(struct ir_func_builder *irb,
                                       struct ir_stmt **stmt,
                                       struct td_expr *expr) {
  struct td_assg *assg = &expr->assg;

  struct ir_op *value;
  enum td_binary_op_ty ty;
  switch (assg->ty) {
  case TD_ASSG_TY_BASIC:
    value = build_ir_for_expr(irb, stmt, assg->expr);
    break;

  case TD_ASSG_TY_ADD:
    ty = TD_BINARY_OP_TY_ADD;
    goto compound_assg;
  case TD_ASSG_TY_SUB:
    ty = TD_BINARY_OP_TY_SUB;
    goto compound_assg;
  case TD_ASSG_TY_MUL:
    ty = TD_BINARY_OP_TY_MUL;
    goto compound_assg;
  case TD_ASSG_TY_DIV:
    ty = TD_BINARY_OP_TY_DIV;
    goto compound_assg;
  case TD_ASSG_TY_QUOT:
    ty = TD_BINARY_OP_TY_QUOT;
    goto compound_assg;
  case TD_ASSG_TY_AND:
    ty = TD_BINARY_OP_TY_AND;
    goto compound_assg;
  case TD_ASSG_TY_OR:
    ty = TD_BINARY_OP_TY_OR;
    goto compound_assg;
  case TD_ASSG_TY_XOR:
    ty = TD_BINARY_OP_TY_XOR;
    goto compound_assg;
  case TD_ASSG_TY_LSHIFT:
    ty = TD_BINARY_OP_TY_LSHIFT;
    goto compound_assg;
  case TD_ASSG_TY_RSHIFT:
    ty = TD_BINARY_OP_TY_RSHIFT;
    goto compound_assg;

  compound_assg : {
    struct ir_op *assignee = build_ir_for_expr(irb, stmt, assg->assignee);

    struct ir_op *rhs = build_ir_for_expr(irb, stmt, assg->expr);

    struct ir_build_binaryop args = {
        .ty = ty,
        .result_ty = expr->var_ty,
        .lhs_ty = assg->assignee->var_ty,
        .rhs_ty = assg->expr->var_ty,
        .lhs = assignee,
        .rhs = rhs,
    };

    value = alloc_binaryop(irb, *stmt, &args);

    break;
  }
  }

  struct ir_op *address = NULL;
  switch (assg->assignee->ty) {
  case TD_EXPR_TY_VAR:
    return var_assg(irb, *stmt, value, &assg->assignee->var);
  case TD_EXPR_TY_ARRAYACCESS: {
    struct td_arrayaccess *access = &assg->assignee->array_access;
    address = build_ir_for_array_address(irb, stmt, access->lhs, access->rhs);
    break;
  }
  case TD_EXPR_TY_MEMBERACCESS: {
    struct td_memberaccess *access = &assg->assignee->member_access;
    address =
        build_ir_for_member_address(irb, stmt, access->lhs, access->member);

    break;
  }
  case TD_EXPR_TY_POINTERACCESS: {
    struct td_pointeraccess *access = &assg->assignee->pointer_access;
    address =
        build_ir_for_pointer_address(irb, stmt, access->lhs, access->member);
    break;
  }
  case TD_EXPR_TY_UNARY_OP: {
    if (assg->assignee->unary_op.ty == TD_UNARY_OP_TY_INDIRECTION) {
      address = build_ir_for_expr(irb, stmt, assg->assignee->unary_op.expr);
    }
    break;
  }
  default:
    todo("non var assignments");
  }

  if (!address) {
    todo("non var assignments");
  }

  struct ir_op *store = alloc_ir_op(irb->func, *stmt);
  store->ty = IR_OP_TY_STORE_ADDR;
  store->var_ty = IR_VAR_TY_NONE;
  store->store_addr =
      (struct ir_op_store_addr){.addr = address, .value = value};

  return value;
}

static struct ir_op *
build_ir_for_arrayaccess(struct ir_func_builder *irb, struct ir_stmt **stmt,
                         struct td_arrayaccess *array_access) {
  struct td_var_ty underlying =
      td_var_ty_get_underlying(irb->tchk, &array_access->lhs->var_ty);
  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &underlying);

  struct ir_op *address = build_ir_for_array_address(
      irb, stmt, array_access->lhs, array_access->rhs);

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_LOAD_ADDR;
  op->var_ty = var_ty;
  op->load_addr = (struct ir_op_load_addr){.addr = address};

  return op;
}

static struct ir_op *
build_ir_for_memberaccess(struct ir_func_builder *irb, struct ir_stmt **stmt,
                          struct td_memberaccess *member_access,
                          const struct td_var_ty *member_ty) {
  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, member_ty);

  struct ir_op *address = build_ir_for_member_address(
      irb, stmt, member_access->lhs, member_access->member);

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_LOAD_ADDR;
  op->var_ty = var_ty;
  op->load_addr = (struct ir_op_load_addr){.addr = address};

  return op;
}

static struct ir_op *
build_ir_for_pointeraccess(struct ir_func_builder *irb, struct ir_stmt **stmt,
                           struct td_pointeraccess *pointer_access,
                           const struct td_var_ty *member_ty) {
  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, member_ty);

  struct ir_op *address = build_ir_for_pointer_address(
      irb, stmt, pointer_access->lhs, pointer_access->member);

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_LOAD_ADDR;
  op->var_ty = var_ty;
  op->load_addr = (struct ir_op_load_addr){.addr = address};

  return op;
}

static struct ir_op *build_ir_for_expr(struct ir_func_builder *irb,
                                       struct ir_stmt **stmt,
                                       struct td_expr *expr) {
  struct ir_op *op;

  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &expr->var_ty);

  switch (expr->ty) {
  case TD_EXPR_TY_TERNARY:
    op = build_ir_for_ternary(irb, stmt, var_ty, &expr->ternary);
    break;
  case TD_EXPR_TY_VAR:
    op = build_ir_for_var(irb, stmt, var_ty, &expr->var);
    break;
  case TD_EXPR_TY_CNST:
    op = build_ir_for_cnst(irb, stmt, var_ty, &expr->cnst);
    break;
  case TD_EXPR_TY_COMPOUNDEXPR:
    op = build_ir_for_compoundexpr(irb, stmt, var_ty, &expr->compound_expr);
    break;
  case TD_EXPR_TY_CALL:
    op = build_ir_for_call(irb, stmt, expr);
    break;
  case TD_EXPR_TY_UNARY_OP:
    op = build_ir_for_unaryop(irb, stmt, expr);
    break;
  case TD_EXPR_TY_BINARY_OP:
    op = build_ir_for_binaryop(irb, stmt, expr);
    break;
  case TD_EXPR_TY_ARRAYACCESS:
    op = build_ir_for_arrayaccess(irb, stmt, &expr->array_access);
    break;
  case TD_EXPR_TY_MEMBERACCESS:
    op = build_ir_for_memberaccess(irb, stmt, &expr->member_access,
                                   &expr->var_ty);
    break;
  case TD_EXPR_TY_POINTERACCESS:
    op = build_ir_for_pointeraccess(irb, stmt, &expr->pointer_access,
                                    &expr->var_ty);
    break;
  case TD_EXPR_TY_ASSG:
    op = build_ir_for_assg(irb, stmt, expr);
    break;
  case TD_EXPR_TY_SIZEOF:
    op = build_ir_for_sizeof(irb, stmt, expr);
    break;
  case TD_EXPR_TY_ALIGNOF:
    op = build_ir_for_alignof(irb, stmt, expr);
    break;
  case TD_EXPR_TY_COMPOUND_LITERAL:
    todo("compound literals");
  }

  invariant_assert(op, "null op!");
  return op;
}

static struct ir_basicblock *build_ir_for_stmt(struct ir_func_builder *irb,
                                               struct ir_basicblock *basicblock,
                                               struct td_stmt *stmt);

static struct ir_basicblock *
build_ir_for_compoundstmt(struct ir_func_builder *irb,
                          struct ir_basicblock *basicblock,
                          struct td_compoundstmt *compound_stmt) {
  for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
    basicblock = build_ir_for_stmt(irb, basicblock, &compound_stmt->stmts[i]);
  }
  return basicblock;
}

static struct ir_basicblock *build_ir_for_if(struct ir_func_builder *irb,
                                             struct ir_basicblock *basicblock,
                                             struct td_ifstmt *if_stmt) {

  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, basicblock);
  struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, &if_stmt->cond);

  struct ir_basicblock *pre_if_basicblock = cond->stmt->basicblock;

  // basic block for if body
  struct ir_basicblock *if_start_basicblock = alloc_ir_basicblock(irb->func);

  struct ir_op *br_cond = alloc_ir_op(irb->func, cond_stmt);
  br_cond->ty = IR_OP_TY_BR_COND;
  br_cond->var_ty = IR_VAR_TY_NONE;
  br_cond->br_cond.cond = cond;

  struct ir_basicblock *if_end_basicblock =
      build_ir_for_stmt(irb, if_start_basicblock, if_stmt->body);

  // we add a redundant branch to keep the nice property that all BBs end in a
  // branch
  struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, if_end_basicblock);
  struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_VAR_TY_NONE;

  // basic block for *after* if body
  struct ir_basicblock *after_if_basicblock = alloc_ir_basicblock(irb->func);

  make_basicblock_split(irb->func, pre_if_basicblock, if_start_basicblock,
                        after_if_basicblock);

  make_basicblock_merge(irb->func, if_end_basicblock, after_if_basicblock);

  return after_if_basicblock;
}

static struct ir_basicblock *
build_ir_for_ifelse(struct ir_func_builder *irb,
                    struct ir_basicblock *basicblock,
                    struct td_ifelsestmt *if_else_stmt) {
  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, basicblock);
  struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, &if_else_stmt->cond);

  // basic block for if body
  struct ir_basicblock *if_basicblock = alloc_ir_basicblock(irb->func);
  struct ir_basicblock *after_if_bb =
      build_ir_for_stmt(irb, if_basicblock, if_else_stmt->body);

  // basic block for else body
  struct ir_basicblock *else_basicblock = alloc_ir_basicblock(irb->func);
  struct ir_basicblock *after_else_bb =
      build_ir_for_stmt(irb, else_basicblock, if_else_stmt->else_body);

  struct ir_basicblock *after_if_else_basicblock =
      alloc_ir_basicblock(irb->func);

  struct ir_basicblock *pre_if_basicblock = cond->stmt->basicblock;

  make_basicblock_split(irb->func, pre_if_basicblock, if_basicblock,
                        else_basicblock);

  struct ir_stmt *br_cond_stmt = alloc_ir_stmt(irb->func, pre_if_basicblock);
  struct ir_op *br_cond = alloc_ir_op(irb->func, br_cond_stmt);
  br_cond->ty = IR_OP_TY_BR_COND;
  br_cond->var_ty = IR_VAR_TY_NONE;
  br_cond->br_cond.cond = cond;

  struct ir_stmt *br_after_if_stmt = alloc_ir_stmt(irb->func, after_if_bb);
  struct ir_op *br_after_if = alloc_ir_op(irb->func, br_after_if_stmt);
  br_after_if->ty = IR_OP_TY_BR;
  br_after_if->var_ty = IR_VAR_TY_NONE;
  make_basicblock_merge(irb->func, after_if_bb, after_if_else_basicblock);

  struct ir_stmt *br_after_else_stmt = alloc_ir_stmt(irb->func, after_else_bb);
  struct ir_op *br_after_else = alloc_ir_op(irb->func, br_after_else_stmt);
  br_after_else->ty = IR_OP_TY_BR;
  br_after_else->var_ty = IR_VAR_TY_NONE;
  make_basicblock_merge(irb->func, after_else_bb, after_if_else_basicblock);

  return after_if_else_basicblock;
}

static struct ir_basicblock *
build_ir_for_switch(struct ir_func_builder *irb,
                    struct ir_basicblock *basicblock,
                    struct td_switchstmt *switch_stmt) {
  struct ir_jump new_loop = {.ty = IR_JUMP_TY_NEW_LOOP};
  vector_push_back(irb->jumps, &new_loop);

  struct ir_stmt *ctrl_stmt = alloc_ir_stmt(irb->func, basicblock);
  struct ir_op *ctrl_op =
      build_ir_for_expr(irb, &ctrl_stmt, &switch_stmt->ctrl_expr);

  struct ir_op *switch_op = alloc_ir_op(irb->func, ctrl_stmt);
  switch_op->ty = IR_OP_TY_BR_SWITCH;
  switch_op->var_ty = IR_VAR_TY_NONE;
  switch_op->br_switch = (struct ir_op_br_switch){.value = ctrl_op};

  struct ir_basicblock *body_bb = alloc_ir_basicblock(irb->func);
  struct ir_basicblock *end_bb =
      build_ir_for_stmt(irb, body_bb, switch_stmt->body);

  struct ir_basicblock *after_body_bb = alloc_ir_basicblock(irb->func);
  make_basicblock_merge(irb->func, end_bb, after_body_bb);
  struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, end_bb);
  struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_VAR_TY_NONE;

  struct ir_basicblock *default_block = NULL;

  struct vector *cases = vector_create(sizeof(struct ir_split_case));

  while (!vector_empty(irb->switch_cases)) {
    struct ir_case *switch_case = vector_pop(irb->switch_cases);

    switch (switch_case->ty) {
    case IR_CASE_TY_CASE: {
      vector_push_back(cases, &switch_case->split_case);
      break;
    }
    case IR_CASE_TY_DEFAULT:
      default_block = switch_case->split_case.target;
      break;
    }
  }

  if (!default_block) {
    default_block = after_body_bb;
  }

  make_basicblock_switch(irb->func, basicblock, vector_length(cases),
                         vector_head(cases), default_block);

  struct vector *continues = vector_create(sizeof(struct ir_jump));

  while (!vector_empty(irb->jumps)) {
    struct ir_jump *jump = vector_pop(irb->jumps);

    switch (jump->ty) {
    case IR_JUMP_TY_NEW_LOOP:
      // end
      return after_body_bb;
    case IR_JUMP_TY_BREAK: {
      make_basicblock_merge(irb->func, jump->basicblock, after_body_bb);
      struct ir_stmt *break_br_stmt =
          alloc_ir_stmt(irb->func, jump->basicblock);
      struct ir_op *break_br = alloc_ir_op(irb->func, break_br_stmt);
      break_br->ty = IR_OP_TY_BR;
      break_br->var_ty = IR_VAR_TY_NONE;
      break;
    }
    case IR_JUMP_TY_CONTINUE:
      vector_push_back(continues, jump);
      break;
    }
  }

  // propogate the `continue`s to the next level up
  vector_extend(irb->jumps, vector_head(continues), vector_length(continues));

  return after_body_bb;
}

static struct ir_basicblock *
build_ir_for_selectstmt(struct ir_func_builder *irb,
                        struct ir_basicblock *basicblock,
                        struct td_selectstmt *select_stmt) {
  switch (select_stmt->ty) {
  case TD_SELECTSTMT_TY_IF: {
    return build_ir_for_if(irb, basicblock, &select_stmt->if_stmt);
  }
  case TD_SELECTSTMT_TY_IF_ELSE:
    return build_ir_for_ifelse(irb, basicblock, &select_stmt->if_else_stmt);
  case TD_SELECTSTMT_TY_SWITCH:
    return build_ir_for_switch(irb, basicblock, &select_stmt->switch_stmt);
  }
}

static void build_ir_for_declaration(struct ir_func_builder *irb,
                                     struct ir_stmt **stmt,
                                     struct td_declaration *declaration);

static void
build_ir_for_declorexpr(struct ir_func_builder *irb, struct ir_stmt **stmt,
                        struct td_declaration_or_expr *decl_or_expr) {
  switch (decl_or_expr->ty) {
  case TD_DECLARATION_OR_EXPR_TY_DECL:
    build_ir_for_declaration(irb, stmt, &decl_or_expr->decl);
    break;
  case TD_DECLARATION_OR_EXPR_TY_EXPR:
    build_ir_for_expr(irb, stmt, &decl_or_expr->expr);
    break;
  }
}

struct ir_loop {
  // for CONTINUE
  struct ir_basicblock *entry;
  // for BREAK
  struct ir_basicblock *exit;
};

static struct ir_loop build_ir_for_whilestmt(struct ir_func_builder *irb,
                                             struct ir_basicblock *basicblock,
                                             struct td_whilestmt *while_stmt) {
  struct ir_basicblock *before_cond_basicblock = basicblock;
  struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb->func);
  struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb->func);

  make_basicblock_merge(irb->func, before_cond_basicblock, cond_basicblock);

  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, cond_basicblock);
  struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, &while_stmt->cond);
  struct ir_op *cond_br = alloc_ir_op(irb->func, cond_stmt);
  cond_br->ty = IR_OP_TY_BR_COND;
  cond_br->var_ty = IR_VAR_TY_NONE;
  cond_br->br_cond.cond = cond;

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, while_stmt->body);

  struct ir_basicblock *after_body_basicblock = alloc_ir_basicblock(irb->func);
  make_basicblock_split(irb->func, cond_basicblock, body_basicblock,
                        after_body_basicblock);

  struct ir_stmt *pre_cond_stmt =
      alloc_ir_stmt(irb->func, before_cond_basicblock);
  struct ir_op *pre_cond_br = alloc_ir_op(irb->func, pre_cond_stmt);
  pre_cond_br->ty = IR_OP_TY_BR;
  pre_cond_br->var_ty = IR_VAR_TY_NONE;

  make_basicblock_merge(irb->func, body_stmt_basicblock, cond_basicblock);
  struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, body_stmt_basicblock);
  struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_VAR_TY_NONE;

  return (struct ir_loop){.entry = cond_basicblock,
                          .exit = after_body_basicblock};
}

static struct ir_loop
build_ir_for_dowhilestmt(struct ir_func_builder *irb,
                         struct ir_basicblock *basicblock,
                         struct td_dowhilestmt *do_while_stmt) {
  struct ir_basicblock *before_body_basicblock = basicblock;
  struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb->func);
  struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb->func);

  make_basicblock_merge(irb->func, before_body_basicblock, body_basicblock);

  struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, cond_basicblock);
  struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, &do_while_stmt->cond);
  struct ir_op *cond_br = alloc_ir_op(irb->func, cond_stmt);
  cond_br->ty = IR_OP_TY_BR_COND;
  cond_br->var_ty = IR_VAR_TY_NONE;
  cond_br->br_cond.cond = cond;

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, do_while_stmt->body);

  struct ir_basicblock *after_cond_basicblock = alloc_ir_basicblock(irb->func);
  make_basicblock_split(irb->func, cond_basicblock, body_basicblock,
                        after_cond_basicblock);

  struct ir_stmt *pre_cond_stmt =
      alloc_ir_stmt(irb->func, before_body_basicblock);
  struct ir_op *pre_body_br = alloc_ir_op(irb->func, pre_cond_stmt);
  pre_body_br->ty = IR_OP_TY_BR;
  pre_body_br->var_ty = IR_VAR_TY_NONE;

  make_basicblock_merge(irb->func, body_stmt_basicblock, cond_basicblock);
  struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, body_stmt_basicblock);
  struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_VAR_TY_NONE;

  return (struct ir_loop){.entry = cond_basicblock,
                          .exit = after_cond_basicblock};
}

static struct ir_loop build_ir_for_forstmt(struct ir_func_builder *irb,
                                           struct ir_basicblock *basicblock,
                                           struct td_forstmt *for_stmt) {

  struct ir_basicblock *before_cond_basicblock = basicblock;
  struct ir_basicblock *before_body_basicblock = basicblock;

  if (for_stmt->init) {
    struct ir_stmt *init_stmt =
        alloc_ir_stmt(irb->func, before_cond_basicblock);
    build_ir_for_declorexpr(irb, &init_stmt, for_stmt->init);

    before_cond_basicblock = init_stmt->basicblock;
    before_body_basicblock = init_stmt->basicblock;
  }

  if (for_stmt->cond) {
    struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb->func);
    make_basicblock_merge(irb->func, before_cond_basicblock, cond_basicblock);

    struct ir_stmt *to_cond_stmt =
        alloc_ir_stmt(irb->func, before_cond_basicblock);
    struct ir_op *to_cond_br = alloc_ir_op(irb->func, to_cond_stmt);
    to_cond_br->ty = IR_OP_TY_BR;
    to_cond_br->var_ty = IR_VAR_TY_NONE;

    struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, cond_basicblock);
    struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, for_stmt->cond);

    struct ir_op *cond_br = alloc_ir_op(irb->func, cond_stmt);
    cond_br->ty = IR_OP_TY_BR_COND;
    cond_br->var_ty = IR_VAR_TY_NONE;
    cond_br->br_cond.cond = cond;

    before_body_basicblock = cond_stmt->basicblock;
  } else {
    struct ir_stmt *to_body_stmt =
        alloc_ir_stmt(irb->func, before_body_basicblock);
    struct ir_op *to_body_br = alloc_ir_op(irb->func, to_body_stmt);
    to_body_br->ty = IR_OP_TY_BR;
    to_body_br->var_ty = IR_VAR_TY_NONE;
  }

  struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb->func);
  make_basicblock_merge(irb->func, before_body_basicblock, body_basicblock);

  if (!for_stmt->cond) {
    before_body_basicblock = body_basicblock;
  }

  struct ir_basicblock *body_stmt_basicblock =
      build_ir_for_stmt(irb, body_basicblock, for_stmt->body);

  struct ir_basicblock *end_body_basicblock = body_stmt_basicblock;

  if (for_stmt->iter) {
    struct ir_basicblock *iter_basicblock = alloc_ir_basicblock(irb->func);
    make_basicblock_merge(irb->func, body_stmt_basicblock, iter_basicblock);

    struct ir_stmt *to_iter_stmt =
        alloc_ir_stmt(irb->func, body_stmt_basicblock);
    struct ir_op *to_iter_br = alloc_ir_op(irb->func, to_iter_stmt);
    to_iter_br->ty = IR_OP_TY_BR;
    to_iter_br->var_ty = IR_VAR_TY_NONE;

    struct ir_stmt *iter_stmt = alloc_ir_stmt(irb->func, iter_basicblock);
    build_ir_for_expr(irb, &iter_stmt, for_stmt->iter);

    end_body_basicblock = iter_stmt->basicblock;
  }

  struct ir_stmt *end_stmt = alloc_ir_stmt(irb->func, end_body_basicblock);
  struct ir_op *end_br = alloc_ir_op(irb->func, end_stmt);
  end_br->ty = IR_OP_TY_BR;
  end_br->var_ty = IR_VAR_TY_NONE;
  make_basicblock_merge(irb->func, end_body_basicblock, before_body_basicblock);

  struct ir_basicblock *after_body_basicblock = alloc_ir_basicblock(irb->func);

  if (for_stmt->cond) {
    make_basicblock_split(irb->func, before_body_basicblock, body_basicblock,
                          after_body_basicblock);
  }

  return (struct ir_loop){.entry = end_body_basicblock,
                          .exit = after_body_basicblock};
}

static struct ir_basicblock *
build_ir_for_iterstmt(struct ir_func_builder *irb,
                      struct ir_basicblock *basicblock,
                      struct td_iterstmt *iter_stmt) {
  struct ir_jump new_loop = {.ty = IR_JUMP_TY_NEW_LOOP};
  vector_push_back(irb->jumps, &new_loop);

  struct ir_loop loop;
  switch (iter_stmt->ty) {
  case TD_ITERSTMT_TY_WHILE:
    loop = build_ir_for_whilestmt(irb, basicblock, &iter_stmt->while_stmt);
    break;
  case TD_ITERSTMT_TY_DO_WHILE:
    loop = build_ir_for_dowhilestmt(irb, basicblock, &iter_stmt->do_while_stmt);
    break;
  case TD_ITERSTMT_TY_FOR:
    loop = build_ir_for_forstmt(irb, basicblock, &iter_stmt->for_stmt);
    break;
  }

  for (size_t i = vector_length(irb->jumps); i; i--) {
    struct ir_jump *jump = vector_pop(irb->jumps);

    switch (jump->ty) {
    case IR_JUMP_TY_NEW_LOOP:
      return loop.exit;
    case IR_JUMP_TY_BREAK:
      make_basicblock_merge(irb->func, jump->basicblock, loop.exit);
      break;
    case IR_JUMP_TY_CONTINUE:
      make_basicblock_merge(irb->func, jump->basicblock, loop.entry);
      break;
    }

    struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, jump->basicblock);
    struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
    br->ty = IR_OP_TY_BR;
    br->var_ty = IR_VAR_TY_NONE;
  }

  bug("should've found IR_JUMP_TY_NEW_LOOP in jump vector");
}

static struct ir_basicblock *build_ir_for_goto(struct ir_func_builder *irb,
                                               struct ir_stmt **stmt,
                                               struct td_gotostmt *goto_stmt) {
  struct ir_basicblock *before_goto_basicblock = (*stmt)->basicblock;

  struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, before_goto_basicblock);
  struct ir_op *br = alloc_ir_op(irb->func, br_stmt);

  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_VAR_TY_NONE;

  // put the label we target into metadata
  // copy it out to ignore const warnings
  size_t label_len = strlen(goto_stmt->label);
  br->metadata = arena_alloc(irb->arena, label_len + 1);
  memcpy(br->metadata, goto_stmt->label, label_len + 1);

  struct ir_basicblock *after_goto_basicblock = alloc_ir_basicblock(irb->func);
  return after_goto_basicblock;
}

/* Return stmt be null when this is used to add implicit returns not in code
 * (e.g at end of method) */
static struct ir_basicblock *
build_ir_for_ret(struct ir_func_builder *irb, struct ir_stmt **stmt,
                 struct td_returnstmt *return_stmt) {
  struct ir_op *expr_op;
  if (return_stmt && return_stmt->expr) {
    expr_op = build_ir_for_expr(irb, stmt, return_stmt->expr);
  } else {
    expr_op = NULL;
  }

  struct ir_op *op = alloc_ir_op(irb->func, *stmt);
  op->ty = IR_OP_TY_RET;
  op->var_ty = return_stmt && return_stmt->expr
                   ? var_ty_for_td_var_ty(irb->unit, &return_stmt->expr->var_ty)
                   : IR_VAR_TY_NONE;
  op->ret.value = expr_op;

  op->stmt->basicblock->ty = IR_BASICBLOCK_TY_RET;

  struct ir_basicblock *after_ret_basicblock = alloc_ir_basicblock(irb->func);

  return after_ret_basicblock;
}

static struct ir_basicblock *build_ir_for_break(struct ir_func_builder *irb,
                                                struct ir_stmt **stmt) {
  struct ir_jump jump = {.ty = IR_JUMP_TY_BREAK,
                         .basicblock = (*stmt)->basicblock};
  vector_push_back(irb->jumps, &jump);

  struct ir_basicblock *after_break_basicblock = alloc_ir_basicblock(irb->func);
  return after_break_basicblock;
}

static struct ir_basicblock *build_ir_for_continue(struct ir_func_builder *irb,
                                                   struct ir_stmt **stmt) {
  struct ir_jump jump = {.ty = IR_JUMP_TY_CONTINUE,
                         .basicblock = (*stmt)->basicblock};
  vector_push_back(irb->jumps, &jump);

  struct ir_basicblock *after_continue_basicblock =
      alloc_ir_basicblock(irb->func);
  return after_continue_basicblock;
}

static struct ir_basicblock *
build_ir_for_jumpstmt(struct ir_func_builder *irb,
                      struct ir_basicblock *basicblock,
                      struct td_jumpstmt *jump_stmt) {
  struct ir_stmt *stmt = alloc_ir_stmt(irb->func, basicblock);

  switch (jump_stmt->ty) {
  case TD_JUMPSTMT_TY_RETURN:
    return build_ir_for_ret(irb, &stmt, &jump_stmt->return_stmt);
  case TD_JUMPSTMT_TY_GOTO:
    return build_ir_for_goto(irb, &stmt, &jump_stmt->goto_stmt);
  case TD_JUMPSTMT_TY_BREAK:
    return build_ir_for_break(irb, &stmt);
  case TD_JUMPSTMT_TY_CONTINUE:
    return build_ir_for_continue(irb, &stmt);
  }
}

// describes a fully flattened init list
// init lists in functions then build `expr` to `ir_op`s, while global ones turn
// it into `ir_var`s
struct ir_build_init {
  size_t offset;
  struct td_expr *expr;
};

struct ir_build_init_list_layout {
  size_t num_inits;
  struct ir_build_init *inits;
};

static struct ir_build_init_list_layout
build_init_list_layout(struct ir_unit *iru,
                       const struct td_init_list *init_list);

struct init_range {
  size_t offset;
  size_t size;
};

static int sort_ranges_by_offset(const void *l, const void *r) {
  return (ssize_t)((const struct init_range *)l)->offset -
         (ssize_t)((const struct init_range *)r)->offset;
}

static void build_ir_zero_range(struct ir_func_builder *irb,
                                struct ir_stmt **stmt,
                                struct ir_op *insert_before,
                                struct ir_op *address, size_t base_offset,
                                size_t byte_size) {
  if (!byte_size) {
    return;
  }

  struct ir_var_ty cnst_ty;
  ssize_t chunk_size;
  if (byte_size >= 8) {
    cnst_ty = IR_VAR_TY_I64;
    chunk_size = 8;
  } else if (byte_size >= 4) {
    cnst_ty = IR_VAR_TY_I32;
    chunk_size = 4;
  } else if (byte_size >= 2) {
    cnst_ty = IR_VAR_TY_I16;
    chunk_size = 2;
  } else {
    cnst_ty = IR_VAR_TY_I8;
    chunk_size = 1;
  }

  struct ir_op *zero;
  if (insert_before) {
    zero = insert_before_ir_op(irb->func, insert_before, IR_OP_TY_CNST,
                               var_ty_for_pointer_size(irb->unit));
  } else {
    zero = alloc_ir_op(irb->func, *stmt);
    zero->ty = IR_OP_TY_CNST;
    zero->var_ty = cnst_ty;
  }

  zero->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = 0};

  ssize_t remaining = byte_size;
  size_t head = 0;

  struct ir_op *last = zero;

  while (remaining > 0) {
    size_t offset = remaining >= chunk_size ? head : byte_size - chunk_size;

    struct ir_op *offset_cnst = insert_after_ir_op(
        irb->func, last, IR_OP_TY_CNST, var_ty_for_pointer_size(irb->unit));
    offset_cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
                                            .int_value = base_offset + offset};

    struct ir_op *init_address = insert_after_ir_op(
        irb->func, offset_cnst, IR_OP_TY_BINARY_OP, IR_VAR_TY_POINTER);
    init_address->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = address, .rhs = offset_cnst};

    struct ir_op *store = insert_after_ir_op(
        irb->func, init_address, IR_OP_TY_STORE_ADDR, IR_VAR_TY_NONE);
    store->store_addr =
        (struct ir_op_store_addr){.addr = init_address, .value = zero};

    last = store;

    remaining -= chunk_size;
    head += chunk_size;
  }
}

static void build_ir_for_init_list(struct ir_func_builder *irb,
                                   struct ir_stmt **stmt, struct ir_op *address,
                                   struct td_init_list *init_list) {
  struct ir_build_init_list_layout layout =
      build_init_list_layout(irb->unit, init_list);

  struct vector *init_ranges = vector_create(sizeof(struct init_range));

  // add a "fake range" to cover the start of the struct
  struct init_range start_range = {.offset = 0, .size = 0};
  vector_push_back(init_ranges, &start_range);

  struct ir_op *first_init = NULL;

  for (size_t i = 0; i < layout.num_inits; i++) {
    struct ir_build_init *init = &layout.inits[i];

    struct ir_op *value = build_ir_for_expr(irb, stmt, init->expr);

    if (!first_init) {
      first_init = value;
    }

    struct ir_op *offset = alloc_ir_op(irb->func, *stmt);
    offset->ty = IR_OP_TY_CNST;
    offset->var_ty = var_ty_for_pointer_size(irb->unit);
    offset->cnst =
        (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = init->offset};

    struct ir_op *init_address = alloc_ir_op(irb->func, *stmt);
    init_address->ty = IR_OP_TY_BINARY_OP;
    init_address->var_ty = IR_VAR_TY_POINTER;
    init_address->binary_op = (struct ir_op_binary_op){
        .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = address, .rhs = offset};

    struct ir_op *store = alloc_ir_op(irb->func, *stmt);
    store->ty = IR_OP_TY_STORE_ADDR;
    store->var_ty = IR_VAR_TY_NONE;
    store->store_addr =
        (struct ir_op_store_addr){.addr = init_address, .value = value};

    struct ir_var_ty var_ty =
        var_ty_for_td_var_ty(irb->unit, &init->expr->var_ty);
    struct ir_var_ty_info info = var_ty_info(irb->unit, &var_ty);

    struct init_range range = {.offset = init->offset, .size = info.size};

    vector_push_back(init_ranges, &range);
  }

  qsort(vector_head(init_ranges), vector_length(init_ranges),
        vector_element_size(init_ranges), sort_ranges_by_offset);

  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &init_list->var_ty);
  struct ir_var_ty_info info = var_ty_info(irb->unit, &var_ty);

  // add a "fake range" to cover the end of the struct
  struct init_range end_range = {.offset = info.size, .size = 0};
  vector_push_back(init_ranges, &end_range);

  // TODO: can be more efficient, just does 8 byte blocks
  size_t num_offsets = vector_length(init_ranges);

  size_t head = 0;
  size_t end = info.size;
  for (size_t i = 0; i < num_offsets + 1; i++) {
    size_t new_head;
    size_t offset;

    if (i < num_offsets) {
      struct init_range *init_range = vector_get(init_ranges, i);
      new_head = init_range->offset + init_range->size;
      offset = init_range->offset;
    } else {
      new_head = head;
      offset = end;
    }

    if (i != 0) {
      ssize_t gap = (ssize_t)offset - (ssize_t)head;
      debug_assert(gap >= 0, "bad math");

      build_ir_zero_range(irb, stmt, first_init, address, head, gap);
    }

    head = new_head;
  }
}

static struct ir_op *build_ir_for_init(struct ir_func_builder *irb,
                                       struct ir_stmt **stmt,
                                       struct ir_op *start_address,
                                       struct td_init *init) {
  switch (init->ty) {
  case TD_INIT_TY_EXPR:
    return build_ir_for_expr(irb, stmt, &init->expr);
  case TD_INIT_TY_INIT_LIST:
    debug_assert(start_address,
                 "start_address required when building with init list");
    build_ir_for_init_list(irb, stmt, start_address, &init->init_list);
    return NULL;
  }
}

static void var_assg_glb(struct ir_func_builder *irb, struct ir_stmt *stmt,
                         struct ir_glb *glb, struct td_var *var);

static struct ir_var_value build_ir_for_var_value(struct ir_unit *iru,
                                                  struct td_init *init,
                                                  struct td_var_ty *var_ty);

static void
build_ir_for_global_var(struct ir_unit *iru, struct ir_func *func,
                        struct var_refs *var_refs,
                        enum td_storage_class_specifier storage_class,
                        struct td_var_declaration *decl) {

  struct ir_var_ty var_ty = var_ty_for_td_var_ty(iru, &decl->var_ty);

  const char *name = decl->var.identifier;
  const char *symbol_name;
  if (storage_class == TD_STORAGE_CLASS_SPECIFIER_STATIC) {
    // need to mangle the name as statics cannot interfere with others
    size_t base_len = strlen(name);

    size_t len = base_len + 2; // null char and leading "."

    size_t func_name_len = 0;
    if (func) {
      func_name_len = strlen(func->name);
      len += func_name_len;
      len++; // for "."
    }

    char *buff = arena_alloc(iru->arena, sizeof(*name) * len);
    size_t head = 0;

    buff[head++] = '.';

    if (func) {
      memcpy(&buff[head], func->name, func_name_len);
      head += func_name_len;
      buff[head++] = '.';
    }

    memcpy(&buff[head], name, base_len);
    head += base_len;
    buff[head++] = '\0';

    debug_assert(head == len, "string/buff length mismatch");

    symbol_name = buff;
  } else {
    symbol_name = name;
  }

  struct var_key key = {.name = name, .scope = decl->var.scope};

  enum ir_glb_ty ty;
  if (decl->var_ty.ty == TD_VAR_TY_TY_FUNC) {
    ty = IR_GLB_TY_FUNC;
  } else {
    ty = IR_GLB_TY_DATA;
  }

  struct var_ref *ref = var_refs_get(var_refs, &key);

  if (ref) {
    debug_assert(ref->glb, "ref but has no glb");
  }

  enum ir_linkage linkage;

  bool is_func = decl->var_ty.ty == TD_VAR_TY_TY_FUNC;
  bool is_extern = storage_class == TD_STORAGE_CLASS_SPECIFIER_EXTERN;
  bool is_static = storage_class == TD_STORAGE_CLASS_SPECIFIER_STATIC;
  bool is_file_scope = key.scope == SCOPE_GLOBAL;
  bool is_unspecified_storage =
      storage_class == TD_STORAGE_CLASS_SPECIFIER_NONE;

  if ((is_func && !is_static) || is_extern || (is_file_scope && !is_static)) {
    linkage = IR_LINKAGE_EXTERNAL;
  } else if (is_file_scope && is_static) {
    linkage = IR_LINKAGE_INTERNAL;
  } else {
    linkage = IR_LINKAGE_NONE;
  }

  enum ir_glb_def_ty def_ty;
  if (decl->init) {
    def_ty = IR_GLB_DEF_TY_DEFINED;
  } else if (is_file_scope && !is_func &&
             (is_unspecified_storage || is_static)) {
    def_ty = IR_GLB_DEF_TY_TENTATIVE;
  } else {
    def_ty = IR_GLB_DEF_TY_UNDEFINED;
  }

  if (ref && linkage == IR_LINKAGE_EXTERNAL &&
      ref->glb->linkage == IR_LINKAGE_INTERNAL) {
    // extern but prev was static, stays static
    linkage = IR_LINKAGE_INTERNAL;
  }

  if (!ref) {
    ref = var_refs_add(var_refs, &key, VAR_REF_TY_GLB);
  }

  if (!ref->glb) {
    ref->glb = add_global(iru, ty, &var_ty, def_ty, symbol_name);
  }

  ref->glb->def_ty = def_ty;
  ref->glb->linkage = linkage;

  if (ref && def_ty == IR_GLB_DEF_TY_TENTATIVE) {
    // already defined, and this is tentative, so do nothing
    return;
  }

  if (def_ty != IR_GLB_DEF_TY_DEFINED) {
    return;
  }

  struct ir_var_value value;
  if (decl->init) {
    value = build_ir_for_var_value(iru, decl->init, &decl->var_ty);
  } else {
    value = (struct ir_var_value){.var_ty = var_ty};
  }

  if (!ref->glb->var) {
    ref->glb->var = arena_alloc(iru->arena, sizeof(*ref->glb->var));
  }

  *ref->glb->var =
      (struct ir_var){.ty = IR_VAR_TY_DATA, .var_ty = var_ty, .value = value};
}

static void
build_ir_for_global_declaration(struct ir_unit *iru, struct ir_func *func,
                                struct var_refs *var_refs,
                                struct td_declaration *declaration) {
  for (size_t i = 0; i < declaration->num_var_declarations; i++) {
    build_ir_for_global_var(iru, func, var_refs,
                            declaration->storage_class_specifier,
                            &declaration->var_declarations[i]);
  }
}

static void build_ir_for_auto_var(struct ir_func_builder *irb,
                                  struct ir_stmt **stmt,
                                  struct td_var_declaration *decl) {
  struct ir_lcl *lcl;
  struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &decl->var_ty);

  if (decl->var_ty.ty == TD_VAR_TY_TY_AGGREGATE ||
      decl->var_ty.ty == TD_VAR_TY_TY_ARRAY) {
    // this is a new var, so we can safely create a new ref
    struct var_key key = get_var_key(&decl->var, (*stmt)->basicblock);
    struct var_ref *ref = var_refs_add(irb->var_refs, &key, VAR_REF_TY_LCL);
    ref->lcl = add_local(irb->func, &var_ty);

    // address = build_ir_for_addressof_var(irb, stmt, &decl->var);
    lcl = ref->lcl;
  } else {
    lcl = NULL;
  }

  struct ir_op *assignment = NULL;
  if (decl->init) {
    struct ir_op *address = NULL;

    if (lcl) {
      address = alloc_ir_op(irb->func, *stmt);
      address->ty = IR_OP_TY_ADDR;
      address->var_ty = IR_VAR_TY_POINTER;
      address->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};
    }

    assignment = build_ir_for_init(irb, stmt, address, decl->init);
  } else if (!lcl) {
    assignment = alloc_ir_op(irb->func, *stmt);
    assignment->ty = IR_OP_TY_UNDF;
    assignment->var_ty = var_ty_for_td_var_ty(irb->unit, &decl->var_ty);
  }

  if (lcl && assignment) {
    struct ir_op *str = alloc_ir_op(irb->func, *stmt);
    str->ty = IR_OP_TY_STORE_LCL;
    str->var_ty = IR_VAR_TY_NONE;
    str->lcl = lcl;
    str->store_lcl = (struct ir_op_store_lcl){.value = assignment};
  } else if (assignment) {
    var_assg(irb, *stmt, assignment, &decl->var);
  }
}

// this is called for decl lists WITHIN a function (i.e default is local
// storage)
static void build_ir_for_declaration(struct ir_func_builder *irb,
                                     struct ir_stmt **stmt,
                                     struct td_declaration *declaration) {
  if (declaration->storage_class_specifier ==
      TD_STORAGE_CLASS_SPECIFIER_TYPEDEF) {
    return;
  }

  for (size_t i = 0; i < declaration->num_var_declarations; i++) {
    struct td_var_declaration *decl = &declaration->var_declarations[i];

    if (decl->var_ty.ty == TD_VAR_TY_TY_FUNC) {
      // tentative definition! make global
      struct ir_var_ty var_ty = var_ty_for_td_var_ty(irb->unit, &decl->var_ty);

      struct ir_glb *glb =
          add_global(irb->unit, IR_GLB_TY_FUNC, &var_ty,
                     IR_GLB_DEF_TY_UNDEFINED, decl->var.identifier);

      glb->var = arena_alloc(irb->arena, sizeof(*glb->var));

      var_assg_glb(irb, *stmt, glb, &decl->var);
      continue;
    }

    if (declaration->storage_class_specifier ==
            TD_STORAGE_CLASS_SPECIFIER_NONE ||
        declaration->storage_class_specifier ==
            TD_STORAGE_CLASS_SPECIFIER_AUTO) {
      build_ir_for_auto_var(irb, stmt, decl);
    } else {
      build_ir_for_global_var(irb->unit, irb->func, irb->global_var_refs,
                              declaration->storage_class_specifier, decl);
    }
  }
}

static struct ir_basicblock *
build_ir_for_labeledstmt(struct ir_func_builder *irb,
                         struct ir_basicblock *basicblock,
                         struct td_labeledstmt *labeled_stmt) {
  struct ir_basicblock *next_bb = alloc_ir_basicblock(irb->func);
  make_basicblock_merge(irb->func, basicblock, next_bb);

  struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, basicblock);
  struct ir_op *br_op = alloc_ir_op(irb->func, br_stmt);
  br_op->ty = IR_OP_TY_BR;
  br_op->var_ty = IR_VAR_TY_NONE;

  switch (labeled_stmt->ty) {
  case TD_LABELEDSTMT_TY_LABEL: {
    add_label(irb, labeled_stmt->label, next_bb);
    break;
  }
  case TD_LABELEDSTMT_TY_CASE: {
    struct ir_case switch_case = {
        .ty = IR_CASE_TY_CASE,
        .split_case = {.target = next_bb, .value = labeled_stmt->cnst}};
    vector_push_back(irb->switch_cases, &switch_case);
    break;
  }
  case TD_LABELEDSTMT_TY_DEFAULT: {
    struct ir_case switch_case = {.ty = IR_CASE_TY_DEFAULT,
                                  .split_case = {.target = next_bb}};
    vector_push_back(irb->switch_cases, &switch_case);
    break;
  }
  }

  return build_ir_for_stmt(irb, next_bb, labeled_stmt->stmt);
}

static struct ir_basicblock *build_ir_for_stmt(struct ir_func_builder *irb,
                                               struct ir_basicblock *basicblock,
                                               struct td_stmt *stmt) {

  debug_assert(basicblock, "bb cannot be null");

  switch (stmt->ty) {
  case TD_STMT_TY_DECLARATION: {
    struct ir_stmt *ir_stmt = alloc_ir_stmt(irb->func, basicblock);
    build_ir_for_declaration(irb, &ir_stmt, &stmt->declaration);
    return ir_stmt->basicblock;
  }
  case TD_STMT_TY_EXPR: {
    // TODO: ternaries
    struct ir_stmt *ir_stmt = alloc_ir_stmt(irb->func, basicblock);
    build_ir_for_expr(irb, &ir_stmt, &stmt->expr);
    return ir_stmt->basicblock;
  }
  case TD_STMT_TY_JUMP: {
    return build_ir_for_jumpstmt(irb, basicblock, &stmt->jump);
  }
  case TD_STMT_TY_COMPOUND: {
    return build_ir_for_compoundstmt(irb, basicblock, &stmt->compound);
  }
  case TD_STMT_TY_SELECT: {
    return build_ir_for_selectstmt(irb, basicblock, &stmt->select);
  }
  case TD_STMT_TY_ITER: {
    return build_ir_for_iterstmt(irb, basicblock, &stmt->iter);
  }
  case TD_STMT_TY_LABELED: {
    return build_ir_for_labeledstmt(irb, basicblock, &stmt->labeled);
  }
  case TD_STMT_TY_NULL: {
    return basicblock;
  }
  }
}

struct ir_build_phi_build {
  struct ir_phi_entry *entry;
  struct ir_basicblock *pred;
};

static void gen_var_phis(struct ir_func_builder *irb,
                         struct ir_op **basicblock_ops_for_var,
                         struct vector *preds, struct td_var *var,
                         struct ir_var_ty *var_ty) {
  size_t head = 0;
  while (vector_length(preds) - head) {
    struct ir_build_phi_build *build = vector_get(preds, head++);

    struct ir_basicblock *basicblock = build->pred;

    struct ir_op *op = basicblock_ops_for_var[basicblock->id];
    if (!op) {
      struct var_key key = get_var_key(var, basicblock);
      struct var_ref *ref = var_refs_get(irb->var_refs, &key);

      if (ref) {
        debug_assert(ref->ty == VAR_REF_TY_SSA,
                     "non-ssa ref ty makes no sense for phi");

        op = ref->op;
      }
    }

    if (op) {
      *build->entry =
          (struct ir_phi_entry){.basicblock = basicblock, .value = op};

      basicblock_ops_for_var[basicblock->id] = op;
      continue;
    }

    // var is not in this bb, so gen phi
    struct ir_op *phi = insert_phi(irb->func, basicblock, *var_ty);

    phi->phi = (struct ir_op_phi){
        .num_values = basicblock->num_preds,
        .values = arena_alloc(irb->arena, sizeof(*phi->phi.values) *
                                              basicblock->num_preds)};
    *build->entry =
        (struct ir_phi_entry){.basicblock = basicblock, .value = phi};

    basicblock_ops_for_var[basicblock->id] = phi;

    for (size_t i = 0; i < basicblock->num_preds; i++) {
      struct ir_basicblock *pred = basicblock->preds[i];

      struct ir_build_phi_build pred_build = {.entry = &phi->phi.values[i],
                                              .pred = pred};

      printf("adding entry %zu from %zu\n", pred->id, basicblock->id);
      vector_push_back(preds, &pred_build);
    }
  }
}

static void find_phi_exprs(struct ir_func_builder *irb, struct ir_op *phi) {
  debug_assert(phi->ty == IR_OP_TY_PHI, "non-phi in `find_phi_exprs`");

  // walk predecessor basic blocks (splitting into seperate walks each time we
  // have multiple predecessors) until we
  // * A) find a write
  // * B) re-reach current bb
  // * or C) reach end (first bb)

  struct ir_basicblock *basicblock = phi->stmt->basicblock;

  struct ir_op **basicblock_ops_for_var = arena_alloc(
      irb->arena, sizeof(struct ir_op *) * irb->func->basicblock_count);
  memset(basicblock_ops_for_var, 0,
         sizeof(struct ir_op *) * irb->func->basicblock_count);
  basicblock_ops_for_var[basicblock->id] = phi;

  phi->phi = (struct ir_op_phi){
      .num_values = basicblock->num_preds,
      .values = arena_alloc(irb->arena,
                            sizeof(*phi->phi.values) * basicblock->num_preds)};

  struct vector *phi_builds = vector_create(sizeof(struct ir_build_phi_build));

  for (size_t i = 0; i < basicblock->num_preds; i++) {
    struct ir_build_phi_build build = {.entry = &phi->phi.values[i],
                                       .pred = basicblock->preds[i]};

    vector_push_back(phi_builds, &build);
  }

  gen_var_phis(irb, basicblock_ops_for_var, phi_builds, phi->metadata,
               &phi->var_ty);
}

struct validate_metadata {
  struct ir_func_builder *irb;
  struct ir_op *consumer;
};

static void validate_op_tys_callback(struct ir_op **op, void *cb_metadata) {
  struct validate_metadata *metadata = cb_metadata;
  struct ir_op *consumer = metadata->consumer;

  struct ir_var_ty res_ty = (*op)->var_ty;

  // TODO: validate cast types (make sure they are valid)
  switch (consumer->ty) {
  case IR_OP_TY_BINARY_OP:
    res_ty = consumer->var_ty;
    break;
  case IR_OP_TY_CALL:
    res_ty = *consumer->call.func_ty.func.ret_ty;
    break;
  case IR_OP_TY_CAST_OP:
    res_ty = consumer->var_ty;
    break;
  case IR_OP_TY_ADDR:
    res_ty = IR_VAR_TY_POINTER;
    break;
  case IR_OP_TY_LOAD_ADDR:
    // loads happen on opaque pointers so we can't check types
    return;
  default:
    break;
  }

  if (op_produces_value(consumer)) {
    invariant_assert(
        !var_ty_needs_cast_op(metadata->irb, &res_ty, &consumer->var_ty),
        "op %zu uses op %zu with different type!", consumer->id, (*op)->id);
  }
}

static struct ir_func_builder *
build_ir_for_function(struct ir_unit *unit, struct arena_allocator *arena,
                      struct td_funcdef *def,
                      struct var_refs *global_var_refs) {
  struct var_refs *var_refs = var_refs_create();
  struct ir_func b = {.unit = unit,
                      .name = def->var_declaration.var.identifier,
                      .arena = arena,
                      .flags = IR_FUNC_FLAG_NONE,
                      .first = NULL,
                      .last = NULL,
                      .op_count = 0,
                      .num_locals = 0,
                      .total_locals_size = 0};

  struct ir_func *f = arena_alloc(arena, sizeof(b));
  *f = b;

  struct ir_func_builder *builder = arena_alloc(arena, sizeof(b));
  *builder = (struct ir_func_builder){
      .arena = arena,
      .unit = unit,
      .func = f,
      .tchk = unit->tchk,
      .jumps = vector_create(sizeof(struct ir_jump)),
      .switch_cases = vector_create(sizeof(struct ir_case)),
      .var_refs = var_refs,
      .global_var_refs = global_var_refs};

  // needs at letd one initial basic block
  alloc_ir_basicblock(builder->func);
  struct ir_basicblock *basicblock = builder->func->first;
  struct ir_stmt *param_stmt = alloc_ir_stmt(builder->func, basicblock);

  // first statement is a bunch of magic MOV commands that explain to the rest
  // of the IR that these are params this is encoded as MOV NULL with the
  // IR_OP_FLAG_PARAM flag
  struct td_ty_func func_ty = def->var_declaration.var_ty.func;

  for (size_t i = 0; i < func_ty.num_params; i++) {
    const struct td_ty_param *param = &func_ty.params[i];

    if (param->var_ty.ty == TD_VAR_TY_TY_VARIADIC || !param->identifier) {
      continue;
    }

    // TODO: the whole decl code needs reworking
    struct td_var var = {
        .scope = SCOPE_PARAMS,
        .identifier = param->identifier,
    };

    struct var_key key = get_var_key(&var, basicblock);
    struct var_ref *ref = var_refs_add(builder->var_refs, &key, VAR_REF_TY_SSA);

    struct ir_var_ty var_ty =
        var_ty_for_td_var_ty(builder->unit, &param->var_ty);

    if (var_ty.ty == IR_VAR_TY_TY_ARRAY) {
      // arrays are actually pointers
      var_ty = IR_VAR_TY_POINTER;
    }

    struct ir_op *mov = alloc_ir_op(builder->func, param_stmt);
    mov->ty = IR_OP_TY_MOV;
    mov->var_ty = var_ty;
    mov->flags |= IR_OP_FLAG_PARAM;
    mov->mov.value = NULL;

    ref->op = mov;
  }

  basicblock = build_ir_for_stmt(builder, basicblock, &def->body);

  // now we have generated the IR we first need to fix up labels
  basicblock = builder->func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        if (op->ty == IR_OP_TY_BR && op->metadata) {
          // any BR with metadata is a label
          const char *name = op->metadata;

          struct ir_label *label = builder->labels;
          while (label) {
            if (strcmp(name, label->name) == 0) {
              make_basicblock_merge(builder->func, basicblock,
                                    label->basicblock);
            }

            label = label->succ;
          }
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  // we may generate empty basicblocks or statements, prune them here
  prune_basicblocks(builder->func);

  // may not end in a return, but needs to to be well-formed IR
  struct ir_basicblock *last_bb = builder->func->last;
  if (!last_bb || (last_bb->last && last_bb->last->last &&
                   op_is_branch(last_bb->last->last->ty))) {
    debug("adding bb to create ret");
    last_bb = alloc_ir_basicblock(builder->func);
  }

  struct ir_stmt *last_stmt = last_bb->last;
  if (!last_stmt) {
    debug("adding bb to create stmt");
    last_stmt = alloc_ir_stmt(builder->func, last_bb);
  }

  struct ir_op *last_op = last_stmt->last;

  if (!last_op || last_op->ty != IR_OP_TY_RET) {
    struct ir_op *return_value = NULL;

    if (strcmp(builder->func->name, "main") == 0) {
      debug("adding implicit return 0 to bb %zu", last_bb->id);

      struct ir_op *cnst = alloc_ir_op(builder->func, last_stmt);
      cnst->ty = IR_OP_TY_CNST;
      cnst->var_ty = (struct ir_var_ty){
          .ty = IR_VAR_TY_TY_PRIMITIVE,
          .primitive = IR_VAR_PRIMITIVE_TY_I32,
      };
      cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = 0};

      return_value = cnst;
    }

    basicblock = build_ir_for_ret(builder, &last_stmt, NULL);
    debug_assert(last_stmt->last->ty == IR_OP_TY_RET,
                 "expected ret after call to build ret");
    last_stmt->last->ret.value = return_value;
  }

  // prune again, as inserting the ret can introduce an extraneous empty bb
  prune_basicblocks(builder->func);

  if (log_enabled()) {
    debug_print_ir_func(stderr, builder->func, NULL, NULL);
  }

  // now we fix up phis
  basicblock = builder->func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        if (op->ty == IR_OP_TY_PHI && op->metadata) {
          find_phi_exprs(builder, op);
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  basicblock = builder->func->first;
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

// static struct ir_var_value build_ir_for_zero_var(struct ir_unit *iru,
//                                                  struct td_var_ty *var_ty)
//                                                  {
//   switch (var_ty->ty) {
//   case TD_VAR_TY_TY_UNKNOWN:
//   case TD_VAR_TY_TY_VOID:
//   case TD_VAR_TY_TY_VARIADIC:
//   case TD_VAR_TY_TY_FUNC:
//     bug("no sense");
//   case TD_VAR_TY_TY_WELL_KNOWN:
//   case TD_VAR_TY_TY_POINTER:
//   case TD_VAR_TY_TY_ARRAY:
//   case TD_VAR_TY_TY_INCOMPLETE_AGGREGATE:
//   case TD_VAR_TY_TY_AGGREGATE:
//     return (struct ir_var_value){.var_ty = var_ty_for_td_var_ty(iru,
//     var_ty)};
//   }
// }

static size_t get_member_index_offset(struct ir_unit *iru,
                                      const struct td_var_ty *var_ty,
                                      size_t member_index,
                                      struct td_var_ty *member_ty) {
  if (var_ty->ty == TD_VAR_TY_TY_ARRAY) {
    *member_ty = td_var_ty_get_underlying(iru->tchk, var_ty);
    struct ir_var_ty el_ty = var_ty_for_td_var_ty(iru, member_ty);
    struct ir_var_ty_info info = var_ty_info(iru, &el_ty);

    return info.size * member_index;
  } else {
    debug_assert(var_ty->ty == TD_VAR_TY_TY_AGGREGATE ||
                     var_ty->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE,
                 "bad type");

    const char *member_name = var_ty->aggregate.fields[member_index].identifier;
    struct ir_var_ty ir_member_ty;
    size_t member_offset;
    size_t idx;
    get_member_info(iru, var_ty, member_name, &ir_member_ty, &idx,
                    &member_offset, member_ty);

    return member_offset;
  }
}

static size_t get_designator_offset(struct ir_unit *iru,
                                    const struct td_var_ty *var_ty,
                                    struct td_designator_list *designator_list,
                                    size_t *member_index,
                                    struct td_var_ty *member_ty) {
  debug_assert(designator_list->num_designators,
               "not defined for 0 designators");

  size_t offset = 0;

  struct td_var_ty cur_var_ty = *var_ty;
  for (size_t i = 0; i < designator_list->num_designators; i++) {
    struct td_designator *designator = &designator_list->designators[i];

    switch (designator->ty) {
    case TD_DESIGNATOR_TY_FIELD: {
      const char *member_name = designator->field;
      struct ir_var_ty ir_member_ty;
      size_t member_offset;
      get_member_info(iru, &cur_var_ty, member_name, &ir_member_ty,
                      member_index, &member_offset, member_ty);

      offset += member_offset;
      break;
    }
    case TD_DESIGNATOR_TY_INDEX: {
      *member_ty = designator->var_ty;
      struct ir_var_ty el_var_ty = var_ty_for_td_var_ty(iru, member_ty);
      struct ir_var_ty_info info = var_ty_info(iru, &el_var_ty);

      offset += info.size * designator->index;
      *member_index = designator->index;
      break;
    }
    }

    cur_var_ty = designator->var_ty;
  }

  return offset;
}

static struct ir_var_value build_ir_for_var_value(struct ir_unit *iru,
                                                  struct td_init *init,
                                                  struct td_var_ty *var_ty);

// UNUSED static struct ir_var_value
// build_ir_value_for_struct_init_list(struct ir_unit *iru,
//                                     struct td_init_list *init_list,
//                                     const struct td_var_ty *var_ty) {

//   // debug_assert(var_ty->ty == TD_VAR_TY_TY_AGGREGATE &&
//   //                  var_ty->aggregate.ty == TD_TY__AGGREGATE_TY_STRUCT,
//   //              "non stuct init list");

//   size_t num_elements = var_ty->array.size;

//   if (!num_elements) {
//     bug("empty structs are GNU extension");
//   }

//   struct ir_var_value_list value_list = {
//       .num_values = init_list->num_inits,
//       .values = arena_alloc(iru->arena,
//                             sizeof(*value_list.values) *
//                             init_list->num_inits),
//       .offsets = arena_alloc(iru->arena, sizeof(*value_list.offsets) *
//                                              init_list->num_inits)};

//   size_t member_idx = 0;
//   for (size_t i = 0; i < num_elements; i++) {
//     debug_assert(i < num_elements, "too many items in struct init-list");

//     struct td_init_list_init *init = &init_list->inits[i];

//     size_t offset;
//     struct td_var_ty member_ty;
//     if (i < init_list->num_inits && init->designator_list->num_designators)
//     {
//       offset = get_designator_offset(iru, var_ty,
//                                      init_list->inits[i].designator_list,
//                                      &member_idx, &member_ty);
//     } else {
//       offset = get_member_index_offset(iru, var_ty, member_idx,
//       &member_ty);
//     }
//     member_idx++;

//     struct ir_var_value value;
//     if (i < init_list->num_inits) {
//       value = build_ir_for_var_value(iru, init->init, &member_ty);
//     } else {
//       value = build_ir_for_zero_var(iru, &member_ty);
//     }

//     value_list.values[i] = value;
//     value_list.offsets[i] = offset;
//   }

//   return (struct ir_var_value){.ty = IR_VAR_VALUE_TY_VALUE_LIST,
//                                .var_ty = var_ty_for_td_var_ty(iru, var_ty),
//                                .value_list = value_list};
// }

enum init_list_layout_ty {
  INIT_LIST_LAYOUT_TY_STRUCT,
  INIT_LIST_LAYOUT_TY_UNION,
  INIT_LIST_LAYOUT_TY_ARRAY,
};

static void build_init_list_layout_entry(struct ir_unit *iru,
                                         const struct td_init_list *init_list,
                                         const struct td_var_ty *var_ty,
                                         size_t offset, struct vector *inits) {

  enum init_list_layout_ty ty;
  struct td_var_ty el_ty;
  size_t el_size;
  switch (var_ty->ty) {
  case TD_VAR_TY_TY_AGGREGATE:
    ty = var_ty->aggregate.ty == TD_TY_AGGREGATE_TY_STRUCT
             ? INIT_LIST_LAYOUT_TY_STRUCT
             : INIT_LIST_LAYOUT_TY_UNION;
    break;
  case TD_VAR_TY_TY_ARRAY:
    ty = INIT_LIST_LAYOUT_TY_ARRAY;
    el_ty = *var_ty->array.underlying;
    struct ir_var_ty ir_el_ty = var_ty_for_td_var_ty(iru, &el_ty);
    el_size = var_ty_info(iru, &ir_el_ty).size;
    break;
  default:
    bug("bad type for init list");
  }

  size_t num_elements = init_list->num_inits;

  size_t member_idx = 0;
  for (size_t i = 0; i < num_elements; i++) {
    struct td_init_list_init *init = &init_list->inits[i];

    size_t init_offset = offset;
    struct td_var_ty member_ty;
    if (init->designator_list && init->designator_list->num_designators) {
      init_offset += get_designator_offset(iru, &init_list->var_ty,
                                           init_list->inits[i].designator_list,
                                           &member_idx, &member_ty);
    } else {
      switch (ty) {
      case INIT_LIST_LAYOUT_TY_STRUCT:
      case INIT_LIST_LAYOUT_TY_UNION:
        init_offset +=
            get_member_index_offset(iru, var_ty, member_idx, &member_ty);
        break;
      case INIT_LIST_LAYOUT_TY_ARRAY:
        member_ty = el_ty;
        init_offset += member_idx * el_size;
      }
    }

    member_idx++;

    switch (init->init->ty) {
    case TD_INIT_TY_EXPR: {
      struct ir_build_init build_init = {
          .offset = init_offset,
          .expr = &init->init->expr,
      };

      vector_push_back(inits, &build_init);
      break;
    }
    case TD_INIT_TY_INIT_LIST:
      build_init_list_layout_entry(iru, &init->init->init_list, &member_ty,
                                   init_offset, inits);
      break;
    }
  }
}

static struct ir_build_init_list_layout
build_init_list_layout(struct ir_unit *iru,
                       const struct td_init_list *init_list) {
  struct vector *inits = vector_create(sizeof(struct ir_build_init));

  build_init_list_layout_entry(iru, init_list, &init_list->var_ty, 0, inits);

  struct ir_build_init_list_layout layout = {
      .num_inits = vector_length(inits),
      .inits = arena_alloc(iru->arena, vector_byte_size(inits))};

  vector_copy_to(inits, layout.inits);

  return layout;
}

static struct ir_var_value
build_ir_for_var_value_expr(struct ir_unit *iru, struct td_expr *expr,
                            struct td_var_ty *var_ty) {
  switch (expr->ty) {
  case TD_EXPR_TY_UNARY_OP: {
    if (expr->unary_op.ty == TD_UNARY_OP_TY_CAST) {
      return build_ir_for_var_value_expr(iru, expr->unary_op.expr,
                                         &expr->unary_op.cast.var_ty);
    } else {
      todo("other unary ops");
    }
  }
  case TD_EXPR_TY_CNST: {
    struct td_cnst *cnst = &expr->cnst;
    if (is_integral_ty(&expr->var_ty)) {
      return (struct ir_var_value){.ty = IR_VAR_VALUE_TY_INT,
                                   .var_ty = var_ty_for_td_var_ty(iru, var_ty),
                                   .int_value = cnst->int_value};
    } else if (is_fp_ty(&expr->var_ty)) {
      return (struct ir_var_value){.ty = IR_VAR_VALUE_TY_FLT,
                                   .var_ty = var_ty_for_td_var_ty(iru, var_ty),
                                   .flt_value = cnst->flt_value};
    } else if (expr->var_ty.ty == TD_VAR_TY_TY_POINTER) {
      return (struct ir_var_value){.ty = IR_VAR_VALUE_TY_INT,
                                   .var_ty = var_ty_for_td_var_ty(iru, var_ty),
                                   .int_value = cnst->int_value};
    } else {
      todo("other types");
    }
  }
  default:
    todo("other expr tys");
  }
}

static struct ir_var_value build_ir_for_var_value(struct ir_unit *iru,
                                                  struct td_init *init,
                                                  struct td_var_ty *var_ty) {
  switch (init->ty) {
  case TD_INIT_TY_EXPR: {
    return build_ir_for_var_value_expr(iru, &init->expr, &init->expr.var_ty);
  }
  case TD_INIT_TY_INIT_LIST: {
    const struct td_init_list *init_list = &init->init_list;

    struct ir_build_init_list_layout layout =
        build_init_list_layout(iru, init_list);

    struct ir_var_value_list value_list = {
        .num_values = layout.num_inits,
        .values = arena_alloc(iru->arena,
                              sizeof(*value_list.values) * layout.num_inits),
        .offsets = arena_alloc(iru->arena,
                               sizeof(*value_list.offsets) * layout.num_inits),
    };

    for (size_t i = 0; i < layout.num_inits; i++) {
      struct ir_build_init *build_init = &layout.inits[i];

      value_list.values[i] = build_ir_for_var_value_expr(
          iru, build_init->expr, &build_init->expr->var_ty);
      value_list.offsets[i] = build_init->offset;
    }

    return (struct ir_var_value){.ty = IR_VAR_VALUE_TY_VALUE_LIST,
                                 .var_ty = var_ty_for_td_var_ty(iru, var_ty),
                                 .value_list = value_list};
  }
  }
}

struct ir_unit *
build_ir_for_translationunit(struct typechk *tchk,
                             struct arena_allocator *arena,
                             struct td_translationunit *translation_unit) {

  struct ir_unit *iru = arena_alloc(arena, sizeof(*iru));
  *iru = (struct ir_unit){.arena = arena,
                          .tchk = tchk,
                          .first_global = NULL,
                          .last_global = NULL,
                          .num_globals = 0};

  struct var_refs *global_var_refs = var_refs_create();
  // funcs do not necessarily have a seperate decl so we do it for defs too

  for (size_t i = 0; i < translation_unit->num_external_declarations; i++) {
    struct td_external_declaration *external_declaration =
        &translation_unit->external_declarations[i];

    switch (external_declaration->ty) {
    case TD_EXTERNAL_DECLARATION_TY_DECLARATION: {
      build_ir_for_global_declaration(iru, NULL, global_var_refs,
                                      &external_declaration->declaration);
      break;
    }
    case TD_EXTERNAL_DECLARATION_TY_FUNC_DEF: {
      struct td_funcdef *def = &external_declaration->func_def;

      build_ir_for_global_var(iru, NULL, global_var_refs,
                              def->storage_class_specifier,
                              &def->var_declaration);

      struct ir_func_builder *builder =
          build_ir_for_function(iru, arena, def, global_var_refs);

      struct var_key key = {.name = def->var_declaration.var.identifier,
                            .scope = SCOPE_GLOBAL};

      struct var_ref *ref = var_refs_get(global_var_refs, &key);
      ref->glb->def_ty = IR_GLB_DEF_TY_DEFINED;
      ref->glb->func = builder->func;

      break;
    }
    }
  }

  // finally, we need to convert tentative definitions to real ones
  struct ir_glb *glb = iru->first_global;
  while (glb) {
    if (glb->def_ty == IR_GLB_DEF_TY_TENTATIVE) {
      debug_assert(glb->ty == IR_GLB_TY_DATA, "tentative func makes no sense");
      glb->def_ty = IR_GLB_DEF_TY_DEFINED;
      glb->var = arena_alloc(iru->arena, sizeof(*glb->var));
      *glb->var = (struct ir_var){.ty = IR_VAR_TY_DATA,
                                  .var_ty = glb->var_ty,
                                  .value = {.ty = IR_VAR_VALUE_TY_ZERO}};
    }

    glb = glb->succ;
  }

  return iru;
}
