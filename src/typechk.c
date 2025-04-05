#include "typechk.h"

#include "alloc.h"
#include "ap_val.h"
#include "compiler.h"
#include "diagnostics.h"
#include "lex.h"
#include "log.h"
#include "parse.h"
#include "program.h"
#include "target.h"
#include "util.h"
#include "var_table.h"
#include "vector.h"

#include <ctype.h>

struct td_var_ty TD_VAR_TY_UNKNOWN = {.ty = TD_VAR_TY_TY_UNKNOWN};
struct td_var_ty TD_VAR_TY_VOID = {.ty = TD_VAR_TY_TY_VOID};
struct td_var_ty TD_VAR_TY_VOID_POINTER = {
    .ty = TD_VAR_TY_TY_POINTER, .pointer = {.underlying = &TD_VAR_TY_VOID}};
struct td_var_ty TD_VAR_TY_CHAR_POINTER = {
    .ty = TD_VAR_TY_TY_POINTER,
    .pointer = {.underlying = &TD_VAR_TY_WELL_KNOWN_CHAR}};
struct td_var_ty TD_VAR_TY_CONST_CHAR_POINTER = {
    .ty = TD_VAR_TY_TY_POINTER,
    .type_qualifiers = TD_TYPE_QUALIFIER_FLAG_CONST,
    .pointer = {.underlying = &TD_VAR_TY_WELL_KNOWN_CHAR}};

#define MAKE_WKT(name)                                                         \
  struct td_var_ty TD_VAR_TY_WELL_KNOWN_##name = {                             \
      .ty = TD_VAR_TY_TY_WELL_KNOWN, .well_known = WELL_KNOWN_TY_##name}

MAKE_WKT(CHAR);
MAKE_WKT(SIGNED_CHAR);
MAKE_WKT(UNSIGNED_CHAR);
MAKE_WKT(SIGNED_SHORT);
MAKE_WKT(UNSIGNED_SHORT);
MAKE_WKT(SIGNED_INT);
MAKE_WKT(UNSIGNED_INT);
MAKE_WKT(SIGNED_LONG);
MAKE_WKT(UNSIGNED_LONG);
MAKE_WKT(SIGNED_LONG_LONG);
MAKE_WKT(UNSIGNED_LONG_LONG);
MAKE_WKT(HALF);
MAKE_WKT(FLOAT);
MAKE_WKT(DOUBLE);
MAKE_WKT(LONG_DOUBLE);

#undef MAKE_WKT

struct typechk {
  struct arena_allocator *arena;
  struct parser *parser;
  const struct target *target;

  const struct compile_args *args;

  size_t next_anonymous_type_name_id;

  // `returns` need to know what they coerce to
  struct td_var_ty ret_ty;

  // `value` contains a `struct td_var_ty *` to the type of the variable
  // or NULL if the variable has been used without a declaration
  struct var_table var_table;

  // types (e.g declared structs)
  struct var_table ty_table;

  enum typechk_result_ty result_ty;
  struct compiler_diagnostics *diagnostics;
};

static struct td_var_ty
get_target_for_variadic(const struct td_var_ty *ty_ref) {
  // floats are promoted to doubles and types smaller than int are promoted to
  // int
  if (ty_ref->ty != TD_VAR_TY_TY_WELL_KNOWN) {
    return *ty_ref;
  }

  if (ty_ref->well_known == WELL_KNOWN_TY_FLOAT) {
    return TD_VAR_TY_WELL_KNOWN_DOUBLE;
  } else if (ty_ref->well_known < WELL_KNOWN_TY_SIGNED_INT) {
    return TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
  }

  return *ty_ref;
}
bool td_binary_op_is_comparison(enum td_binary_op_ty ty) {
  switch (ty) {
  case TD_BINARY_OP_TY_EQ:
  case TD_BINARY_OP_TY_NEQ:
  case TD_BINARY_OP_TY_GT:
  case TD_BINARY_OP_TY_GTEQ:
  case TD_BINARY_OP_TY_LT:
  case TD_BINARY_OP_TY_LTEQ:
  case TD_BINARY_OP_TY_LOGICAL_OR:
  case TD_BINARY_OP_TY_LOGICAL_AND:
    return true;
  case TD_BINARY_OP_TY_OR:
  case TD_BINARY_OP_TY_AND:
  case TD_BINARY_OP_TY_XOR:
  case TD_BINARY_OP_TY_LSHIFT:
  case TD_BINARY_OP_TY_RSHIFT:
  case TD_BINARY_OP_TY_ADD:
  case TD_BINARY_OP_TY_SUB:
  case TD_BINARY_OP_TY_MUL:
  case TD_BINARY_OP_TY_DIV:
  case TD_BINARY_OP_TY_MOD:
    return false;
  }
}

bool td_var_ty_is_fp_ty(const struct td_var_ty *ty) {
  if (ty->ty != TD_VAR_TY_TY_WELL_KNOWN) {
    return false;
  }

  switch (ty->well_known) {
  case WELL_KNOWN_TY_HALF:
  case WELL_KNOWN_TY_FLOAT:
  case WELL_KNOWN_TY_DOUBLE:
  case WELL_KNOWN_TY_LONG_DOUBLE:
    return true;

  case WELL_KNOWN_TY_BOOL:
  case WELL_KNOWN_TY_CHAR:
  case WELL_KNOWN_TY_SIGNED_CHAR:
  case WELL_KNOWN_TY_UNSIGNED_CHAR:
  case WELL_KNOWN_TY_SIGNED_SHORT:
  case WELL_KNOWN_TY_UNSIGNED_SHORT:
  case WELL_KNOWN_TY_SIGNED_INT:
  case WELL_KNOWN_TY_UNSIGNED_INT:
  case WELL_KNOWN_TY_SIGNED_LONG:
  case WELL_KNOWN_TY_UNSIGNED_LONG:
  case WELL_KNOWN_TY_SIGNED_LONG_LONG:
  case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
  case WELL_KNOWN_TY_UINT128:
  case WELL_KNOWN_TY_INT128:
    return false;
  }
}

static bool try_get_completed_aggregate(struct typechk *tchk,
                                        const struct td_var_ty *var_ty,
                                        struct td_var_ty *complete);

enum td_var_ty_compatible_flags {
  TD_VAR_TY_COMPATIBLE_FLAG_NONE = 0,
  TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS = 1 << 0,
  TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_RHS = 1 << 1,
  TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_ALL =
      TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS |
      TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_RHS
};

static struct td_var_ty
td_var_ty_lvalue_convert(struct typechk *tchk, const struct td_var_ty *var_ty) {
  // arrays and functions decay to pointers
  // const/volatile/restrict qualifiers stripped

  struct td_var_ty lvalue_cvt = *var_ty;
  lvalue_cvt.type_qualifiers = TD_TYPE_QUALIFIER_FLAG_NONE;

  if (lvalue_cvt.ty == TD_VAR_TY_TY_ARRAY) {
    lvalue_cvt = td_var_ty_make_pointer(tchk, lvalue_cvt.array.underlying,
                                        TD_TYPE_QUALIFIER_FLAG_NONE);
  } else if (lvalue_cvt.ty == TD_VAR_TY_TY_FUNC) {
    lvalue_cvt =
        td_var_ty_make_pointer(tchk, &lvalue_cvt, TD_TYPE_QUALIFIER_FLAG_NONE);
  }

  return lvalue_cvt;
}

static bool td_var_ty_compatible(struct typechk *tchk,
                                 const struct td_var_ty *l,
                                 const struct td_var_ty *r,
                                 enum td_var_ty_compatible_flags flags) {
  if (l->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    struct td_var_ty left = *l;
    if (try_get_completed_aggregate(tchk, l, &left)) {
      return td_var_ty_compatible(tchk, &left, r, flags);
    }
  }

  if (r->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    struct td_var_ty right;
    if (try_get_completed_aggregate(tchk, r, &right)) {
      return td_var_ty_compatible(tchk, l, &right, flags);
    }
  }

  if (flags & TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS) {
    struct td_var_ty left = td_var_ty_lvalue_convert(tchk, l);
    return td_var_ty_compatible(
        tchk, &left, r, flags & ~TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS);
  }

  if (flags & TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_RHS) {
    struct td_var_ty right = td_var_ty_lvalue_convert(tchk, r);
    return td_var_ty_compatible(
        tchk, l, &right, flags & ~TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_RHS);
  }

  if (l->type_qualifiers != r->type_qualifiers) {
    return false;
  }

  if (l->ty != r->ty) {
    // possible bug source: what if one is incomplete aggregate and one is
    // complete aggregate? this will return false but maybe shouldn't
    return false;
  }

  switch (l->ty) {
  case TD_VAR_TY_TY_UNKNOWN:
    return true;
  case TD_VAR_TY_TY_VOID:
    return true;
  case TD_VAR_TY_TY_VARIADIC:
    BUG("doesn't make sense");
  case TD_VAR_TY_TY_WELL_KNOWN:
    return l->well_known == r->well_known;
  case TD_VAR_TY_TY_FUNC: {
    struct td_ty_func l_func = l->func;
    struct td_ty_func r_func = r->func;

    if (!td_var_ty_compatible(tchk, l_func.ret, r_func.ret,
                              TD_VAR_TY_COMPATIBLE_FLAG_NONE)) {
      return false;
    }

    if (l_func.ty == TD_TY_FUNC_TY_UNKNOWN_ARGS ||
        r_func.ty == TD_TY_FUNC_TY_UNKNOWN_ARGS) {
      // TODO: absolute minefield. from C spec
      //
      //   * one is an old-style (parameter-less) definition, the other has a
      //   parameter list,
      //     the parameter list does not use an ellipsis and each parameter is
      //     compatible (after function parameter type adjustment) with the
      //     corresponding old-style parameter after default argument promotions
      //
      //   * one is an old-style (parameter-less) declaration, the other has a
      //   parameter list, the parameter list does not use an ellipsis,
      //     and all parameters (after function parameter type adjustment) are
      //     unaffected by default argument promotions

      return true;
    }

    if (l_func.ty != r_func.ty) {
      return false;
    }

    if (l_func.num_params != r_func.num_params) {
      return false;
    }

    size_t num_params = l_func.num_params;
    for (size_t i = 0; i < num_params; i++) {
      const struct td_var_ty *lp = &l_func.params[i].var_ty;
      const struct td_var_ty *rp = &r_func.params[i].var_ty;

      if (lp->ty == TD_VAR_TY_TY_VARIADIC || rp->ty == TD_VAR_TY_TY_VARIADIC) {
        continue;
      }

      if (!td_var_ty_compatible(
              tchk, lp, rp,
              TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS |
                  TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_RHS)) {
        return false;
      }
    }

    return true;
  }
  case TD_VAR_TY_TY_POINTER: {
    struct td_var_ty l_underlying = td_var_ty_get_underlying(tchk, l);
    struct td_var_ty r_underlying = td_var_ty_get_underlying(tchk, r);

    return td_var_ty_compatible(tchk, &l_underlying, &r_underlying,
                                TD_VAR_TY_COMPATIBLE_FLAG_NONE);
  }
  case TD_VAR_TY_TY_ARRAY: {
    // unknown size array type is turned to pointer, but we don't know how to
    // distinguish that from normal pointer, so this is wrong there as it should
    // do `if (one_is_pointer) { return true }`
    if (l->array.size != r->array.size) {
      return false;
    }

    struct td_var_ty l_underlying = td_var_ty_get_underlying(tchk, l);
    struct td_var_ty r_underlying = td_var_ty_get_underlying(tchk, r);
    return td_var_ty_compatible(tchk, &l_underlying, &r_underlying,
                                TD_VAR_TY_COMPATIBLE_FLAG_NONE);
  }
  case TD_VAR_TY_TY_AGGREGATE: {
    struct td_ty_aggregate l_agg = l->aggregate;
    struct td_ty_aggregate r_agg = r->aggregate;

    if (l_agg.ty != r_agg.ty) {
      return false;
    }

    // aggregate types are the same iff they have the same name or come from the
    // same declaration we give anonymous types a name per-declaration
    return szstreq(l_agg.name, r_agg.name);
  }
  case TD_VAR_TY_TY_INCOMPLETE_AGGREGATE: {
    // same logic as for aggregate
    struct td_ty_incomplete_aggregate l_agg = l->incomplete_aggregate;
    struct td_ty_incomplete_aggregate r_agg = r->incomplete_aggregate;

    if (l_agg.ty != r_agg.ty) {
      return false;
    }

    return szstreq(l_agg.name, r_agg.name);
  }
  }
}

// FIXME: consider qualifiers, and "compatible" types rather than just equal
// this method likely still needed, but most consumers of it care about
// "compatible"
bool td_var_ty_eq(struct typechk *tchk, const struct td_var_ty *l,
                  const struct td_var_ty *r) {
  if (l->ty != r->ty) {
    return false;
  }

  switch (l->ty) {
  case TD_VAR_TY_TY_UNKNOWN:
    BUG("comparing unknown types");
  case TD_VAR_TY_TY_VOID:
    return true;
  case TD_VAR_TY_TY_WELL_KNOWN:
    return l->well_known == r->well_known;
  case TD_VAR_TY_TY_FUNC: {
    struct td_ty_func l_func = l->func;
    struct td_ty_func r_func = r->func;

    if (!td_var_ty_eq(tchk, l_func.ret, r_func.ret)) {
      return false;
    }

    if (l_func.ty == TD_TY_FUNC_TY_UNKNOWN_ARGS ||
        r_func.ty == TD_TY_FUNC_TY_UNKNOWN_ARGS) {
      return true;
    }

    if (l_func.ty != r_func.ty) {
      return false;
    }

    if (l_func.num_params != r_func.num_params) {
      return false;
    }

    size_t num_params = l_func.num_params;
    for (size_t i = 0; i < num_params; i++) {
      if (!td_var_ty_eq(tchk, &l_func.params[i].var_ty,
                        &r_func.params[i].var_ty)) {
        return false;
      }
    }

    return true;
  }
  case TD_VAR_TY_TY_ARRAY:
    if (l->array.size != r->array.size) {
      return false;
    }

    // cast never needed for actual array?
    return true;
  case TD_VAR_TY_TY_POINTER: {
    // FIXME: why did i change this to this
    return r->ty == TD_VAR_TY_TY_POINTER;
    // struct td_var_ty l_underlying = td_var_ty_get_underlying(tchk, l);
    // struct td_var_ty r_underlying = td_var_ty_get_underlying(tchk, r);
    // return td_var_ty_eq(tchk, &l_underlying, &r_underlying);
  }

  case TD_VAR_TY_TY_VARIADIC:
    return true;
  case TD_VAR_TY_TY_AGGREGATE: {
    struct td_ty_aggregate l_agg = l->aggregate;
    struct td_ty_aggregate r_agg = r->aggregate;

    if (l_agg.ty != r_agg.ty) {
      return false;
    }

    // aggregate types are the same iff they have the same name or come from the
    // same declaration we give anonymous types a name per-declaration
    return szstreq(l_agg.name, r_agg.name);
  }
  case TD_VAR_TY_TY_INCOMPLETE_AGGREGATE: {
    struct td_ty_incomplete_aggregate l_agg = l->incomplete_aggregate;
    struct td_ty_incomplete_aggregate r_agg = r->incomplete_aggregate;

    if (l_agg.ty != r_agg.ty) {
      return false;
    }

    // aggregate types are the same iff they have the same name or come from the
    // same declaration we give anonymous types a name per-declaration
    return szstreq(l_agg.name, r_agg.name);
  }
  }
}

bool td_var_ty_is_integral_ty(const struct td_var_ty *ty) {
  if (ty->ty != TD_VAR_TY_TY_WELL_KNOWN) {
    return false;
  }

  switch (ty->well_known) {
  case WELL_KNOWN_TY_BOOL:
  case WELL_KNOWN_TY_CHAR:
  case WELL_KNOWN_TY_SIGNED_CHAR:
  case WELL_KNOWN_TY_UNSIGNED_CHAR:
  case WELL_KNOWN_TY_SIGNED_SHORT:
  case WELL_KNOWN_TY_UNSIGNED_SHORT:
  case WELL_KNOWN_TY_SIGNED_INT:
  case WELL_KNOWN_TY_UNSIGNED_INT:
  case WELL_KNOWN_TY_SIGNED_LONG:
  case WELL_KNOWN_TY_UNSIGNED_LONG:
  case WELL_KNOWN_TY_SIGNED_LONG_LONG:
  case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
  case WELL_KNOWN_TY_INT128:
  case WELL_KNOWN_TY_UINT128:
    return true;

  case WELL_KNOWN_TY_HALF:
  case WELL_KNOWN_TY_FLOAT:
  case WELL_KNOWN_TY_DOUBLE:
  case WELL_KNOWN_TY_LONG_DOUBLE:
    return false;
  }
}

bool td_var_ty_is_scalar_ty(const struct td_var_ty *ty) {
  // FIXME: also nullptr_t in C23
  return td_var_ty_is_integral_ty(ty) || td_var_ty_is_fp_ty(ty) ||
         ty->ty == TD_VAR_TY_TY_POINTER;
}

struct td_var_ty td_var_ty_pointer_sized_int(struct typechk *tchk,
                                             bool is_signed) {
  // returns the type for `size_t` effectively
  // TODO: generalise - we should have a special ptr-sized int type

  enum well_known_ty wkt;
  switch (tchk->target->lp_sz) {
  case TARGET_LP_SZ_LP32:
    wkt = is_signed ? WELL_KNOWN_TY_SIGNED_INT : WELL_KNOWN_TY_UNSIGNED_INT;
    break;
  case TARGET_LP_SZ_LP64:
    wkt = is_signed ? WELL_KNOWN_TY_SIGNED_LONG_LONG
                    : WELL_KNOWN_TY_UNSIGNED_LONG_LONG;
    break;
  }

  return (struct td_var_ty){.ty = TD_VAR_TY_TY_WELL_KNOWN, .well_known = wkt};
}

static char *tchk_type_name(struct typechk *tchk,
                            const struct td_var_ty *var_ty);

struct td_var_ty
td_var_ty_make_pointer(struct typechk *tchk, const struct td_var_ty *var_ty,
                       enum td_type_qualifier_flags qualifiers) {
  // we don't know lifetime of the other one so need to copy it
  // TODO: cache types
  struct td_var_ty *copied = arena_alloc(tchk->arena, sizeof(*copied));
  *copied = *var_ty;

  return (struct td_var_ty){.ty = TD_VAR_TY_TY_POINTER,
                            .type_qualifiers = qualifiers,
                            .pointer =
                                (struct td_ty_pointer){.underlying = copied}};
}

struct td_var_ty td_var_ty_get_underlying(UNUSED_ARG(struct typechk *tchk),
                                          const struct td_var_ty *ty_ref) {
  switch (ty_ref->ty) {
  case TD_VAR_TY_TY_UNKNOWN:
    return TD_VAR_TY_UNKNOWN;
  case TD_VAR_TY_TY_POINTER:
    return *ty_ref->pointer.underlying;
  case TD_VAR_TY_TY_ARRAY:
    return *ty_ref->array.underlying;
  default:
    BUG("non pointer/array/tagged passed (type %u)", ty_ref->ty);
  }
}

UNUSED static struct td_var_ty
td_var_ty_promote_integer(UNUSED_ARG(struct typechk *tchk),
                          const struct td_var_ty *ty_ref) {
  DEBUG_ASSERT(ty_ref->ty != TD_VAR_TY_TY_UNKNOWN, "unknown ty in call to `%s`",
               __func__);

  if (ty_ref->ty != TD_VAR_TY_TY_WELL_KNOWN ||
      ty_ref->well_known >= WELL_KNOWN_TY_SIGNED_INT) {
    return *ty_ref;
  }

  // all values smaller than int are promoted to int
  // FIXME: wrong on EEP, as unsigned short should promote to unsigned int due
  // to both being 16 bits
  return TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
}

static struct td_expr add_cast_expr(struct typechk *tchk, struct td_expr expr,
                                    struct td_var_ty target_ty) {
  struct td_expr td_expr = (struct td_expr){
      .ty = TD_EXPR_TY_UNARY_OP,
      .var_ty = target_ty,
      .unary_op =
          (struct td_unary_op){
              .ty = TD_UNARY_OP_TY_CAST,
              .expr = arena_alloc(tchk->arena, sizeof(*td_expr.unary_op.expr)),
              .cast = (struct td_cast){.var_ty = target_ty}},

      .span = expr.span};

  *td_expr.unary_op.expr = expr;
  return td_expr;
}

static char *tchk_base_type_name(struct typechk *tchk,
                                 const struct td_var_ty *var_ty) {
  const char *kw;
  struct sized_str name;

  switch (var_ty->ty) {
  case TD_VAR_TY_TY_UNKNOWN:
    return "<err-ty>";
  case TD_VAR_TY_TY_VOID:
    return "void";
  case TD_VAR_TY_TY_WELL_KNOWN:
    switch (var_ty->well_known) {
    case WELL_KNOWN_TY_CHAR:
      return "char";
    case WELL_KNOWN_TY_SIGNED_CHAR:
      return "signed char";
    case WELL_KNOWN_TY_UNSIGNED_CHAR:
      return "unsigned char";
    case WELL_KNOWN_TY_SIGNED_SHORT:
      return "short";
    case WELL_KNOWN_TY_UNSIGNED_SHORT:
      return "unsigned short";
    case WELL_KNOWN_TY_SIGNED_INT:
      return "int";
    case WELL_KNOWN_TY_UNSIGNED_INT:
      return "unsigned int";
    case WELL_KNOWN_TY_SIGNED_LONG:
      return "long";
    case WELL_KNOWN_TY_UNSIGNED_LONG:
      return "unsigned long";
    case WELL_KNOWN_TY_SIGNED_LONG_LONG:
      return "long long";
    case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
      return "unsigned long long";
    case WELL_KNOWN_TY_INT128:
      return "int128";
    case WELL_KNOWN_TY_UINT128:
      return "uint128";
    case WELL_KNOWN_TY_BOOL:
      return "bool";
    case WELL_KNOWN_TY_HALF:
      // hmm, we probably want to show `__Fp16` or what is actually used
      // (same for typedefs)
      return "half";
    case WELL_KNOWN_TY_FLOAT:
      return "float";
    case WELL_KNOWN_TY_DOUBLE:
      return "double";
    case WELL_KNOWN_TY_LONG_DOUBLE:
      return "long double";
    }
  case TD_VAR_TY_TY_FUNC: {
    // FIXME: this is wrong, e.g it will do `**(char bar())` instead of `char
    // (**bar)`
    const char *ret_name = tchk_type_name(tchk, var_ty->func.ret);

    char *base = arena_alloc_snprintf(tchk->arena, "%s (", ret_name);
    for (size_t i = 0; i < var_ty->func.num_params; i++) {
      if (i + 1 == var_ty->func.num_params) {
        base = arena_alloc_snprintf(
            tchk->arena, "%s)",
            tchk_type_name(tchk, &var_ty->func.params[i].var_ty));
      } else {
        base = arena_alloc_snprintf(
            tchk->arena, "%s, ",
            tchk_type_name(tchk, &var_ty->func.params[i].var_ty));
      }
    }

    return base;
  }
  case TD_VAR_TY_TY_POINTER: {
    const char *el_name = tchk_type_name(tchk, var_ty->array.underlying);
    return arena_alloc_snprintf(tchk->arena, "%s *", el_name);
  }
  case TD_VAR_TY_TY_ARRAY: {
    const char *el_name = tchk_type_name(tchk, var_ty->array.underlying);
    return arena_alloc_snprintf(tchk->arena, "%s[%zu]", el_name,
                                var_ty->array.size);
  }

  case TD_VAR_TY_TY_VARIADIC:
    return "...";
  case TD_VAR_TY_TY_AGGREGATE:
    switch (var_ty->aggregate.ty) {
    case TD_TY_AGGREGATE_TY_STRUCT:
      kw = "struct";
      break;
    case TD_TY_AGGREGATE_TY_UNION:
      kw = "union";
      break;
    }

    // FIXME: will print <anonymous> type name for anonymous types
    name = var_ty->aggregate.name;
    goto named;
  case TD_VAR_TY_TY_INCOMPLETE_AGGREGATE:
    switch (var_ty->incomplete_aggregate.ty) {
    case TD_TY_AGGREGATE_TY_STRUCT:
      kw = "struct";
      break;
    case TD_TY_AGGREGATE_TY_UNION:
      kw = "union";
      break;
    }
    name = var_ty->incomplete_aggregate.name;
    goto named;

  named: {
    return arena_alloc_snprintf(tchk->arena, "%s %.*s", kw, (int)name.len,
                                name.str);
  }
  }
}

// returns qualifiers _with a trailing space_
static char *tchk_qualifiers_name(struct typechk *tchk,
                                  enum td_type_qualifier_flags flags) {

  if (!flags) {
    return "";
  }

  size_t len = 0;
#define TQUAL(name, str)                                                       \
  if ((flags & (name))) {                                                      \
    len += strlen((str)) + 1;                                                  \
  }

  TQUAL(TD_TYPE_QUALIFIER_FLAG_CONST, "const")
  TQUAL(TD_TYPE_QUALIFIER_FLAG_VOLATILE, "volatile")
  TQUAL(TD_TYPE_QUALIFIER_FLAG_RESTRICT, "restrict")
  TQUAL(TD_TYPE_QUALIFIER_FLAG_NULLABLE, "_Optional")

#undef TQUAL

  char *buf = arena_alloc(tchk->arena, len + 1);
  size_t head = 0;

#define TQUAL(name, str)                                                       \
  if ((flags & (name))) {                                                      \
    strcpy(&buf[head], str " ");                                               \
    head += strlen((str)) + 1;                                                 \
  }

  TQUAL(TD_TYPE_QUALIFIER_FLAG_CONST, "const")
  TQUAL(TD_TYPE_QUALIFIER_FLAG_VOLATILE, "volatile")
  TQUAL(TD_TYPE_QUALIFIER_FLAG_RESTRICT, "restrict")
  TQUAL(TD_TYPE_QUALIFIER_FLAG_NULLABLE, "_Optional")

#undef TQUAL

  buf[head++] = '\0';
  DEBUG_ASSERT(head == len + 1,
               "str buf sizing went wrong (head %zu expected %zu)", head,
               len + 1);

  return buf;
}

static char *tchk_type_name(struct typechk *tchk,
                            const struct td_var_ty *var_ty) {
  return arena_alloc_snprintf(
      tchk->arena, "%s%s", tchk_qualifiers_name(tchk, var_ty->type_qualifiers),
      tchk_base_type_name(tchk, var_ty));
}

enum cast_ty { CAST_TY_NONE, CAST_TY_CAST, CAST_TY_ERR };

static enum cast_ty is_cast_needed(struct typechk *tchk,
                                   const struct td_var_ty *var_ty,
                                   const struct td_var_ty *target_ty) {
  if (target_ty->ty == TD_VAR_TY_TY_UNKNOWN ||
      var_ty->ty == TD_VAR_TY_TY_UNKNOWN) {
    return CAST_TY_NONE;
  }

  if (target_ty->ty == TD_VAR_TY_TY_VOID) {
    return CAST_TY_NONE;
  }

  if (td_var_ty_compatible(tchk, var_ty, target_ty,
                           TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS |
                               TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_RHS)) {
    return CAST_TY_NONE;
  }

  if (td_var_ty_is_integral_ty(target_ty)) {
    if (td_var_ty_is_integral_ty(var_ty) || td_var_ty_is_fp_ty(var_ty) ||
        var_ty->ty == TD_VAR_TY_TY_POINTER) {
      return CAST_TY_CAST;
    }

    return CAST_TY_ERR;
  }

  if (td_var_ty_is_fp_ty(target_ty)) {
    if (td_var_ty_is_integral_ty(var_ty) || td_var_ty_is_fp_ty(var_ty)) {
      return CAST_TY_CAST;
    }

    return CAST_TY_ERR;
  }

  if (target_ty->ty == TD_VAR_TY_TY_POINTER) {
    if (td_var_ty_is_integral_ty(var_ty) ||
        var_ty->ty == TD_VAR_TY_TY_POINTER ||
        var_ty->ty == TD_VAR_TY_TY_ARRAY || var_ty->ty == TD_VAR_TY_TY_FUNC) {
      return CAST_TY_CAST;
    }

    return CAST_TY_ERR;
  }

  return CAST_TY_ERR;
}

static struct td_expr add_cast_if_needed(struct typechk *tchk,
                                         struct text_span context,
                                         struct td_expr expr,
                                         struct td_var_ty target_ty) {
  if (expr.var_ty.ty == TD_VAR_TY_TY_FUNC) {
    // decay to ptr
    expr.var_ty =
        td_var_ty_make_pointer(tchk, &expr.var_ty, TD_TYPE_QUALIFIER_FLAG_NONE);
  }

  if (expr.ty == TD_EXPR_TY_CNST && expr.var_ty.ty == TD_VAR_TY_TY_POINTER &&
      target_ty.ty == TD_VAR_TY_TY_ARRAY) {
    // FIXME: should validate rhs here

    // HACK: change the string literal type to array so ir build knows to have
    // it inline
    // do we need to do this in explicit casts (`type_cast`) too?
    expr.var_ty = target_ty;
    return expr;
  }

  switch (is_cast_needed(tchk, &expr.var_ty, &target_ty)) {
  case CAST_TY_NONE:
    return expr;
  case CAST_TY_CAST:
    return add_cast_expr(tchk, expr, target_ty);
  case CAST_TY_ERR:
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            CAST, cast, context, MK_INVALID_TEXT_POS(0),
            arena_alloc_snprintf(tchk->arena,
                                 "cast is not legal (from '%s' to '%s')",
                                 tchk_type_name(tchk, &expr.var_ty),
                                 tchk_type_name(tchk, &target_ty))));

    return expr;
  }
}

// turns long long to long on LP64, and long to int on LP32
// for easier comparison
static enum well_known_ty canonicalize_wkt(struct typechk *tchk,
                                           enum well_known_ty wkt) {
  switch (tchk->target->lp_sz) {
  case TARGET_LP_SZ_LP32:
    switch (wkt) {
    case WELL_KNOWN_TY_SIGNED_LONG:
      return WELL_KNOWN_TY_SIGNED_INT;
    case WELL_KNOWN_TY_UNSIGNED_LONG:
      return WELL_KNOWN_TY_UNSIGNED_INT;
    default:
      return wkt;
    }
  case TARGET_LP_SZ_LP64:
    switch (wkt) {
    case WELL_KNOWN_TY_SIGNED_LONG_LONG:
      return WELL_KNOWN_TY_SIGNED_LONG;
    case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
      return WELL_KNOWN_TY_UNSIGNED_LONG;
    default:
      return wkt;
    }
  }
}

static struct td_expr perform_integer_promotion(struct typechk *tchk,
                                                struct td_expr expr) {
  if (expr.var_ty.ty == TD_VAR_TY_TY_WELL_KNOWN &&
      expr.var_ty.well_known < WELL_KNOWN_TY_SIGNED_INT) {
    struct td_var_ty target_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;

    return add_cast_expr(tchk, expr, target_ty);
  }

  return expr;
}

static struct td_var_ty resolve_usual_arithmetic_conversions(
    struct typechk *tchk, const struct td_var_ty *lhs_ty,
    const struct td_var_ty *rhs_ty, struct text_span context) {
  // it is expected integer promotion has already been performed

  if (lhs_ty->ty == TD_VAR_TY_TY_UNKNOWN ||
      rhs_ty->ty == TD_VAR_TY_TY_UNKNOWN) {
    return TD_VAR_TY_UNKNOWN;
  }

  if (lhs_ty->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE ||
      rhs_ty->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    return TD_VAR_TY_UNKNOWN;
  }

  if (lhs_ty->ty == TD_VAR_TY_TY_ARRAY) {
    struct td_var_ty lhs_ptr = td_var_ty_make_pointer(
        tchk, lhs_ty->array.underlying, TD_TYPE_QUALIFIER_FLAG_NONE);
    return resolve_usual_arithmetic_conversions(tchk, &lhs_ptr, rhs_ty,
                                                context);
  }

  if (lhs_ty->ty == TD_VAR_TY_TY_FUNC) {
    struct td_var_ty lhs_ptr =
        td_var_ty_make_pointer(tchk, lhs_ty, TD_TYPE_QUALIFIER_FLAG_NONE);
    return resolve_usual_arithmetic_conversions(tchk, &lhs_ptr, rhs_ty,
                                                context);
  }

  if (rhs_ty->ty == TD_VAR_TY_TY_ARRAY) {
    struct td_var_ty rhs_ptr = td_var_ty_make_pointer(
        tchk, rhs_ty->array.underlying, TD_TYPE_QUALIFIER_FLAG_NONE);
    return resolve_usual_arithmetic_conversions(tchk, lhs_ty, &rhs_ptr,
                                                context);
  }

  if (rhs_ty->ty == TD_VAR_TY_TY_FUNC) {
    struct td_var_ty rhs_ptr =
        td_var_ty_make_pointer(tchk, rhs_ty, TD_TYPE_QUALIFIER_FLAG_NONE);
    return resolve_usual_arithmetic_conversions(tchk, lhs_ty, &rhs_ptr,
                                                context);
  }

  if (lhs_ty->ty == TD_VAR_TY_TY_POINTER ||
      rhs_ty->ty == TD_VAR_TY_TY_POINTER) {
    if (lhs_ty->ty == TD_VAR_TY_TY_WELL_KNOWN) {
      return *rhs_ty;
    } else if (rhs_ty->ty == TD_VAR_TY_TY_WELL_KNOWN) {
      return *lhs_ty;
    }

    if ((lhs_ty->ty == TD_VAR_TY_TY_POINTER &&
         lhs_ty->pointer.underlying->ty == TD_VAR_TY_TY_VOID) ||
        td_var_ty_is_integral_ty(lhs_ty)) {
      return *rhs_ty;
    } else if ((rhs_ty->ty == TD_VAR_TY_TY_POINTER &&
                rhs_ty->pointer.underlying->ty == TD_VAR_TY_TY_VOID) ||
               td_var_ty_is_integral_ty(rhs_ty)) {
      return *lhs_ty;
    } else if (lhs_ty->ty == TD_VAR_TY_TY_POINTER &&
               rhs_ty->ty == TD_VAR_TY_TY_POINTER &&
               td_var_ty_compatible(
                   tchk, lhs_ty->pointer.underlying, rhs_ty->pointer.underlying,
                   TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS |
                       TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_RHS)) {
      return *lhs_ty;
    } else {
      // compiler_diagnostics_add(
      //     tchk->diagnostics,
      //     MK_SEMANTIC_DIAGNOSTIC(POINTER_TYPE_MISMATCH,
      //     pointer_type_mismatch,
      //                            context, MK_INVALID_TEXT_POS(0),
      //                            "pointer type mismatch"));
    }
  }

  if (lhs_ty->ty != TD_VAR_TY_TY_WELL_KNOWN ||
      rhs_ty->ty != TD_VAR_TY_TY_WELL_KNOWN) {
    return *lhs_ty;
    // TODO("`%s` for types other than well known", __func__);
  }

  DEBUG_ASSERT(lhs_ty->well_known >= WELL_KNOWN_TY_SIGNED_INT &&
                   rhs_ty->well_known >= WELL_KNOWN_TY_SIGNED_INT,
               "integer promotion should have occurred");

  struct td_var_ty result_ty;
  result_ty.ty = TD_VAR_TY_TY_WELL_KNOWN;

  if (lhs_ty->well_known == rhs_ty->well_known) {
    // they are the same type
    result_ty.well_known = lhs_ty->well_known;
  } else {
    enum well_known_ty signed_lhs = WKT_MAKE_SIGNED(lhs_ty->well_known);
    enum well_known_ty signed_rhs = WKT_MAKE_SIGNED(rhs_ty->well_known);

    /* Otherwise, if both operands have signed integer types or both have
    ** unsigned integer types, the operand with the type of lesser integer
    ** conversion rank is converted to the type of the operand with greater
    ** rank.
    */

    if (WKT_IS_SIGNED(lhs_ty->well_known) ==
        WKT_IS_SIGNED(rhs_ty->well_known)) {
      result_ty.well_known = MAX(lhs_ty->well_known, rhs_ty->well_known);

      /* Otherwise, if the operand that has unsigned integer type has rank
      ** greater or equal to the rank of the type of the other operand, then the
      ** operand with signed integer type is converted to the type of the
      ** operand with unsigned integer type.
      */
    } else if (!WKT_IS_SIGNED(lhs_ty->well_known) && signed_lhs >= signed_rhs) {
      result_ty.well_known = lhs_ty->well_known;
    } else if (!WKT_IS_SIGNED(rhs_ty->well_known) && signed_rhs >= signed_lhs) {
      result_ty.well_known = rhs_ty->well_known;

      /* Otherwise, if the type of the operand with signed integer type can
      ** represent all of the values of the type of the operand with unsigned
      ** integer type, then the operand with unsigned integer type is converted
      ** to the type of the operand with signed integer type.
      */

    } else if (WKT_IS_SIGNED(lhs_ty->well_known) &&
               (canonicalize_wkt(tchk, lhs_ty->well_known) >
                canonicalize_wkt(tchk, rhs_ty->well_known))) {
      // one is bigger than other
      // type of expression is simply the larger type
      // FIXME: is this correct
      result_ty.well_known =
          signed_lhs > signed_rhs ? lhs_ty->well_known : rhs_ty->well_known;
    }
    /* Otherwise, both operands are converted to the unsigned integer type
    ** corresponding to the type of the operand with signed integer type.
    */
    else {
      result_ty.well_known = WKT_IS_SIGNED(lhs_ty->well_known)
                                 ? WKT_MAKE_UNSIGNED(lhs_ty->well_known)
                                 : WKT_MAKE_UNSIGNED(rhs_ty->well_known);
    }
  }

  return result_ty;
}

struct td_binary_op_tys {
  struct td_var_ty lhs_op_ty;
  struct td_var_ty rhs_op_ty;
  struct td_var_ty result_ty;
};

static struct td_binary_op_tys
resolve_binary_op_types(struct typechk *tchk, const struct td_expr *lhs_expr,
                        const struct td_expr *rhs_expr, enum td_binary_op_ty ty,
                        struct text_span context) {
  // it is expected integer promotion has already been performed
  // TODO: resolve cmps as bool (because otherwise IR will generate many
  // pointless `trunc` ops)

  const struct td_var_ty *lhs = &lhs_expr->var_ty;
  const struct td_var_ty *rhs = &rhs_expr->var_ty;

  if (lhs->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    struct td_var_ty complete;
    if (try_get_completed_aggregate(tchk, lhs, &complete)) {
      lhs = &complete;
    }
  }

  if (rhs->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    struct td_var_ty complete;
    if (try_get_completed_aggregate(tchk, rhs, &complete)) {
      rhs = &complete;
    }
  }

  struct td_var_ty lhs_op_ty, rhs_op_ty, result_ty;

  if (ty == TD_BINARY_OP_TY_LOGICAL_OR || ty == TD_BINARY_OP_TY_LOGICAL_AND) {
    lhs_op_ty = *lhs;
    rhs_op_ty = *rhs;
    result_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
  } else if (td_binary_op_is_comparison(ty)) {
    lhs_op_ty = rhs_op_ty = *lhs;
    result_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
  } else if (lhs->ty == TD_VAR_TY_TY_POINTER &&
             rhs->ty == TD_VAR_TY_TY_POINTER) {
    if (ty == TD_BINARY_OP_TY_SUB) {
      if (!td_var_ty_compatible(
              tchk, lhs->pointer.underlying, rhs->pointer.underlying,
              TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS |
                  TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_RHS)) {

        // TODO: instead of setting this everywhere have a method that does it
        // all for us
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(
                POINTER_SUB_TYPES, pointer_sub_types, context,
                MK_INVALID_TEXT_POS(0),
                "subtraction on pointers of different kinds is forbidden"));

        lhs_op_ty = TD_VAR_TY_UNKNOWN;
        rhs_op_ty = TD_VAR_TY_UNKNOWN;
        result_ty = TD_VAR_TY_UNKNOWN;
      } else {
        lhs_op_ty = rhs_op_ty = *lhs;
        result_ty = td_var_ty_pointer_sized_int(tchk, true);
      }
    } else {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              POINTER_TYPES, pointer_types, context, MK_INVALID_TEXT_POS(0),
              "binary operations where both types are pointer only makes sense "
              "for subtraction or comparisons"));

      lhs_op_ty = TD_VAR_TY_UNKNOWN;
      rhs_op_ty = TD_VAR_TY_UNKNOWN;
      result_ty = TD_VAR_TY_UNKNOWN;
    }
  } else if (lhs->ty == TD_VAR_TY_TY_POINTER) {
    lhs_op_ty = *lhs;
    rhs_op_ty = *lhs;
    result_ty = *lhs;
  } else if (rhs->ty == TD_VAR_TY_TY_POINTER) {
    lhs_op_ty = *rhs;
    rhs_op_ty = *rhs;
    result_ty = *rhs;
  } else if (ty == TD_BINARY_OP_TY_LSHIFT || ty == TD_BINARY_OP_TY_RSHIFT) {
    lhs_op_ty = *lhs;
    rhs_op_ty = *rhs;
    result_ty = *lhs;
  } else {
    // BUG: does not work when compiled with JCC
    // lhs_op_ty = rhs_op_ty =
    //     resolve_usual_arithmetic_conversions(tchk, lhs, rhs, context);
    rhs_op_ty = resolve_usual_arithmetic_conversions(tchk, lhs, rhs, context);

    lhs_op_ty = rhs_op_ty;

    if (td_binary_op_is_comparison(ty)) {
      result_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
    } else {
      result_ty = lhs_op_ty;
    }
  }

  return (struct td_binary_op_tys){
      .lhs_op_ty = lhs_op_ty, .rhs_op_ty = rhs_op_ty, .result_ty = result_ty};
}

struct assg_ty_map {
  enum lex_token_ty token_ty;
  enum ast_assg_ty assg_ty;
  enum ast_binary_op_ty binary_op_ty;
};

struct td_specifiers {
  enum td_storage_class_specifier storage;
  enum td_function_specifier_flags function;

  struct td_var_ty type_specifier;
};

enum td_specifier_allow {
  TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS = 1 << 0,
  TD_SPECIFIER_ALLOW_STORAGE_CLASS_SPECIFIERS = 1 << 1,
  TD_SPECIFIER_ALLOW_FUNCTION_SPECIFIERS = 1 << 2,
  TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS = 1 << 3,
};

static unsigned long long
type_constant_integral_expr(struct typechk *tchk, const struct ast_expr *expr);

static struct ap_val type_constant_expr(struct typechk *tchk,
                                        const struct ast_expr *expr);

static struct td_expr type_static_init_expr(struct typechk *tchk,
                                            struct td_expr expr,
                                            struct td_var_ty target_ty,
                                            bool is_addr);

enum td_declarator_mode {
  TD_DECLARATOR_MODE_NORMAL,

  // allows initialisers, but must be static-init acceptable
  TD_DECLARATOR_MODE_STATIC,

  // allows bitfields and forbids initializers
  TD_DECLARATOR_MODE_STRUCT
};

static struct td_declaration
type_declaration(struct typechk *tchk,
                 const struct ast_declaration *declaration,
                 enum td_declarator_mode mode);

static struct td_declaration
type_struct_declaration(struct typechk *tchk,
                        const struct ast_declaration *declaration);

static struct td_specifiers
type_specifiers(struct typechk *tchk,
                const struct ast_declaration_specifier_list *list,
                enum td_specifier_allow allow);

enum sign_state { SIGN_STATE_NONE, SIGN_STATE_SIGNED, SIGN_STATE_UNSIGNED };

static bool is_anonymous_name(struct sized_str name) {
  return szstr_prefix(name, MK_SIZED("<anonymous>"));
}

static struct sized_str anonymous_name(struct typechk *tchk) {
  size_t id = tchk->next_anonymous_type_name_id++;
  size_t char_size = num_digits(id);
  size_t len_prefix = strlen("<anonymous>");
  size_t len = len_prefix + char_size;

  char *buff = arena_alloc(tchk->arena, sizeof(*buff) * len + 1);
  snprintf(buff, len, "<anonymous>%zu", id);
  buff[len] = '\0';

  return (struct sized_str){buff, len};
}

static struct td_var_ty
td_var_ty_for_enum(struct typechk *tchk,
                   const struct ast_enum_specifier *specifier) {

  struct sized_str name;
  if (specifier->identifier) {
    name = identifier_str(tchk->parser, specifier->identifier);
  } else {
    name = anonymous_name(tchk);
  }

  struct td_var ty_var = {.ty = TD_VAR_VAR_TY_VAR,
                          .identifier = name,
                          .scope = vt_cur_scope(&tchk->ty_table)};

  struct var_table_entry *ty_entry =
      vt_create_entry(&tchk->ty_table, VAR_TABLE_NS_ENUM, name);
  ty_entry->var = arena_alloc(tchk->arena, sizeof(*ty_entry->var));
  *ty_entry->var = ty_var;
  ty_entry->var_ty = arena_alloc(tchk->arena, sizeof(*ty_entry->var_ty));
  *ty_entry->var_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;

  if (!specifier->enumerator_list) {
    if (!specifier->identifier) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(ENUM_TYPE, enum_type, specifier->span,
                                 MK_INVALID_TEXT_POS(0),
                                 "enum must have values or an identifier"));
      return TD_VAR_TY_UNKNOWN;
    }
  } else {
    unsigned long last_value;
    for (size_t i = 0; i < specifier->enumerator_list->num_enumerators; i++) {
      struct ast_enumerator *enumerator =
          &specifier->enumerator_list->enumerators[i];

      unsigned long long value;
      if (enumerator->value) {
        value = type_constant_integral_expr(tchk, enumerator->value);
      } else {
        value = i ? last_value + 1 : 0;
      }

      struct sized_str enum_name =
          identifier_str(tchk->parser, &enumerator->identifier);
      struct td_var var = {.ty = TD_VAR_VAR_TY_ENUMERATOR,
                           .identifier = enum_name,
                           .scope = vt_cur_scope(&tchk->var_table),
                           .enumerator = value};

      struct var_table_entry *entry;
      // enums have same behaviour as types, but are in the var table
      // so if type table is at global level, insert enum there too
      if (vt_cur_scope(&tchk->ty_table) == SCOPE_GLOBAL) {
        entry = vt_create_top_level_entry(&tchk->var_table, VAR_TABLE_NS_NONE,
                                          enum_name);
      } else {
        entry = vt_create_entry(&tchk->var_table, VAR_TABLE_NS_NONE, enum_name);
      }
      entry->var = arena_alloc(tchk->arena, sizeof(*entry->var));
      *entry->var = var;
      entry->var_ty = arena_alloc(tchk->arena, sizeof(*entry->var_ty));
      *entry->var_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;

      last_value = value;
    }
  }

  return TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
}

// FIXME: i don't think ty table scope changes same as var table does

static void tchk_push_scope(struct typechk *tchk) {
  vt_push_scope(&tchk->var_table);
  vt_push_scope(&tchk->ty_table);
}

static void tchk_pop_scope(struct typechk *tchk) {
  vt_pop_scope(&tchk->var_table);
  vt_pop_scope(&tchk->ty_table);
}

static struct td_var_ty td_var_ty_for_struct_or_union(
    struct typechk *tchk,
    const struct ast_struct_or_union_specifier *specifier) {

  struct td_var_ty var_ty = {
      .ty = TD_VAR_TY_TY_AGGREGATE,
  };

  switch (specifier->ty) {
  case AST_STRUCT_OR_UNION_SPECIFIER_TY_STRUCT:
    var_ty.aggregate.ty = TD_TY_AGGREGATE_TY_STRUCT;
    break;
  case AST_STRUCT_OR_UNION_SPECIFIER_TY_UNION:
    var_ty.aggregate.ty = TD_TY_AGGREGATE_TY_UNION;
    break;
  }

  struct sized_str name;
  if (specifier->identifier) {
    name = identifier_str(tchk->parser, specifier->identifier);
  } else {
    name = anonymous_name(tchk);
  }

  enum var_table_ns ns = var_ty.aggregate.ty == TD_TY_AGGREGATE_TY_STRUCT
                             ? VAR_TABLE_NS_STRUCT
                             : VAR_TABLE_NS_UNION;
  struct var_table_entry *entry = vt_get_entry(&tchk->ty_table, ns, name);

  if (!specifier->decl_list) {
    if (!specifier->identifier) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              AGGREGATE_TYPE, aggregate_type, specifier->span,
              MK_INVALID_TEXT_POS(0),
              "struct/union must have an identifier or a decl list"));
      return TD_VAR_TY_UNKNOWN;
    }

    // FIXME: check scope too
    // if (entry && entry->scope == scope of this decl)
    if (entry) {
      return *entry->var_ty;
    } else {
      return (struct td_var_ty){
          .ty = TD_VAR_TY_TY_INCOMPLETE_AGGREGATE,
          .incomplete_aggregate = {.ty = var_ty.aggregate.ty, .name = name}};
    }
  }

  var_ty.aggregate.name = name;

  struct vector *var_decls = vector_create(sizeof(struct td_var_declaration));
  for (size_t i = 0; i < specifier->decl_list->num_declarations; i++) {
    const struct ast_declaration *declaration =
        &specifier->decl_list->declarations[i];

    vt_push_scope(&tchk->var_table);

    struct td_declaration td_decl = type_struct_declaration(tchk, declaration);

    if (!td_decl.num_var_declarations &&
        td_decl.base_ty.ty == TD_VAR_TY_TY_AGGREGATE) {
      if (!is_anonymous_name(td_decl.base_ty.aggregate.name)) {
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(NO_DECL, no_decl, declaration->span,
                                   MK_INVALID_TEXT_POS(0),
                                   "does not declare anything"));
      } else {
        // anonymous type

        struct td_var_declaration anon_decl = {
            .ty = TD_VAR_DECLARATION_TY_VAR,
            .var =
                {
                    .ty = TD_VAR_VAR_TY_VAR,
                    .identifier = MK_NULL_STR(),
                    .scope = vt_cur_scope(&tchk->var_table),
                },
            .var_ty = td_decl.base_ty,
            .init = NULL,
        };
        vector_push_back(var_decls, &anon_decl);
      }
    }

    vector_extend(var_decls, td_decl.var_declarations,
                  td_decl.num_var_declarations);

    vt_pop_scope(&tchk->var_table);
  }

  size_t num_var_decls = vector_length(var_decls);

  var_ty.aggregate.num_fields = num_var_decls;
  var_ty.aggregate.fields = arena_alloc(
      tchk->arena, sizeof(*var_ty.aggregate.fields) * num_var_decls);

  for (size_t i = 0; i < num_var_decls; i++) {
    struct td_var_declaration *var_decl = vector_get(var_decls, i);

    DEBUG_ASSERT(!var_decl->init,
                 "field decl with init should have been caught earlier");

    enum td_struct_field_flags flags = TD_STRUCT_FIELD_FLAG_NONE;
    unsigned long long bitfield_width = 0;
    if (var_decl->ty == TD_VAR_DECLARATION_TY_BITFIELD) {
      flags |= TD_STRUCT_FIELD_FLAG_BITFIELD;
      bitfield_width = var_decl->bitfield_width;
    }

    var_ty.aggregate.fields[i] =
        (struct td_struct_field){.identifier = var_decl->var.identifier,
                                 .var_ty = var_decl->var_ty,
                                 .flags = flags,
                                 .bitfield_width = bitfield_width};
  }

  vector_free(&var_decls);

  ns = var_ty.aggregate.ty == TD_TY_AGGREGATE_TY_STRUCT ? VAR_TABLE_NS_STRUCT
                                                        : VAR_TABLE_NS_UNION;
  struct td_var var = {.ty = TD_VAR_VAR_TY_VAR,
                       .identifier = name,
                       .scope = vt_cur_scope(&tchk->ty_table)};

  entry = vt_get_or_create_entry(&tchk->ty_table, ns, name);
  entry->var = arena_alloc(tchk->arena, sizeof(*entry->var));
  *entry->var = var;
  entry->var_ty = arena_alloc(tchk->arena, sizeof(*entry->var_ty));
  *entry->var_ty = var_ty;

  return var_ty;
}

static struct td_var_declaration
type_declarator(struct typechk *tchk, const struct td_specifiers *specifiers,
                const struct ast_declarator *declarator,
                const struct ast_init *init, enum td_declarator_mode mode);

static struct td_var_ty type_abstract_declarator(
    struct typechk *tchk, const struct td_specifiers *specifiers,
    const struct ast_abstract_declarator *abstract_declarator);

static size_t td_num_init_fields(const struct td_var_ty *var_ty) {
  if (var_ty->ty == TD_VAR_TY_TY_AGGREGATE &&
      var_ty->aggregate.ty == TD_TY_AGGREGATE_TY_UNION) {
    return 1;
  }

  if (var_ty->ty == TD_VAR_TY_TY_AGGREGATE) {
    size_t sum = 0;
    for (size_t i = 0; i < var_ty->aggregate.num_fields; i++) {
      sum += td_num_init_fields(&var_ty->aggregate.fields[i].var_ty);
    }
    return sum;
  }

  if (var_ty->ty == TD_VAR_TY_TY_ARRAY) {
    return var_ty->array.size * td_num_init_fields(var_ty->array.underlying);
  }

  return 1;
}

static void td_walk_init_list(struct td_var_ty *var_ty,
                              const struct ast_init_list *init_list,
                              size_t *idx) {
  if (var_ty->ty != TD_VAR_TY_TY_ARRAY &&
      (var_ty->ty != TD_VAR_TY_TY_AGGREGATE ||
       (var_ty->ty == TD_VAR_TY_TY_AGGREGATE &&
        var_ty->aggregate.ty == TD_TY_AGGREGATE_TY_UNION))) {
    (*idx)++;
  }

  if (var_ty->ty == TD_VAR_TY_TY_AGGREGATE) {
    for (size_t i = 0; i < var_ty->aggregate.num_fields; i++) {
      td_walk_init_list(&var_ty->aggregate.fields[i].var_ty, init_list, idx);
    }
  } else {
    for (size_t i = 0; i < var_ty->array.size; i++) {
      td_walk_init_list(var_ty->array.underlying, init_list, idx);
    }
  }
}

static struct td_var_ty
type_array_declarator(struct typechk *tchk, struct td_var_ty var_ty,
                      const struct ast_array_declarator *array_declarator,
                      const struct ast_init *init) {
  struct td_var_ty array_ty = {
      .ty = TD_VAR_TY_TY_ARRAY,
  };

  switch (array_declarator->ty) {
  case AST_ARRAY_DECLARATOR_TY_STAR: {
    if (var_ty.ty == TD_VAR_TY_TY_ARRAY) {
      TODO("star array VLAs");
    }

    struct td_var_ty pointer_ty = {
        .ty = TD_VAR_TY_TY_POINTER,
        .pointer = {.underlying = arena_alloc(
                        tchk->arena, sizeof(*array_ty.array.underlying))}};
    *pointer_ty.pointer.underlying = var_ty;

    return pointer_ty;
  }
  case AST_ARRAY_DECLARATOR_TY_STATIC_SIZED:
  case AST_ARRAY_DECLARATOR_TY_SIZED:
    array_ty.array.size =
        type_constant_integral_expr(tchk, array_declarator->size);
    break;
  case AST_ARRAY_DECLARATOR_TY_UNSIZED:
    if (!init) {
      // FIXME: must be a param, else we need to erro
      struct td_var_ty pointer_ty = {
          .ty = TD_VAR_TY_TY_POINTER,
          .pointer = {.underlying = arena_alloc(
                          tchk->arena, sizeof(*array_ty.array.underlying))}};
      *pointer_ty.pointer.underlying = var_ty;

      return pointer_ty;
    }

    switch (init->ty) {
    case AST_INIT_TY_EXPR: {
      // TODO: maybe this should be a helper func
      if (init->expr.ty != AST_EXPR_TY_CNST ||
          (init->expr.cnst.ty != AST_CNST_TY_STR_LITERAL &&
           init->expr.cnst.ty != AST_CNST_TY_WIDE_STR_LITERAL)) {

        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(ARRAY_INIT_TYPE, array_init_type, init->span,
                                   MK_INVALID_TEXT_POS(0),
                                   "cannot initialise unsized array except "
                                   "with normal string literal"));
        return TD_VAR_TY_UNKNOWN;
      }

      // change the constant type to an array, as it is currently a pointer
      const struct ast_cnst *cnst = &init->expr.cnst;

      switch (cnst->ty) {
      case AST_CNST_TY_STR_LITERAL:
        array_ty.array.size = cnst->str_value.ascii.len + 1;
        break;
      case AST_CNST_TY_WIDE_STR_LITERAL:
        array_ty.array.size = cnst->str_value.wide.len + 1;
        break;
      default:
        unreachable();
      }
      break;
    }
    case AST_INIT_TY_INIT_LIST: {
      const struct ast_init_list *init_list = &init->init_list;

      size_t idx = 0;
      size_t size = 0;
      size_t max_size = 0;

      while (idx < init_list->num_inits) {
        const struct ast_init_list_init *init_list_init =
            &init_list->inits[idx];

        if (init_list_init->designator_list &&
            init_list_init->designator_list->num_designators) {
          const struct ast_designator *designator =
              &init_list_init->designator_list->designators[0];

          switch (designator->ty) {
          case AST_DESIGNATOR_TY_FIELD:
            tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
            compiler_diagnostics_add(
                tchk->diagnostics,
                MK_SEMANTIC_DIAGNOSTIC(
                    ARRAY_INIT_FIELD_DESIGNATOR, array_init_field_designator,
                    designator->span, MK_INVALID_TEXT_POS(0),
                    "cannot have a field designator in array init"));
            return TD_VAR_TY_UNKNOWN;
          case AST_DESIGNATOR_TY_INDEX:
            size = type_constant_integral_expr(tchk, designator->index) + 1;
            idx = size;
            break;
          }
        } else if (init_list_init->init->ty != AST_INIT_TY_INIT_LIST) {
          td_walk_init_list(&var_ty, init_list, &idx);
        } else {
          idx++;
        }

        size++;

        max_size = MAX(max_size, size);
      }

      array_ty.array.size = max_size;
      break;
    }
    }
    break;
  }

  array_ty.array.underlying =
      arena_alloc(tchk->arena, sizeof(*array_ty.array.underlying));
  *array_ty.array.underlying = var_ty;

  if (array_ty.array.underlying->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    struct td_var_ty complete;
    (void)try_get_completed_aggregate(tchk, array_ty.array.underlying,
                                      &complete);
    *array_ty.array.underlying = complete;
  }

  return array_ty;
}

static struct td_var_ty
type_func_declarator(struct typechk *tchk, struct td_var_ty var_ty,
                     struct ast_func_declarator *func_declarator) {
  struct td_var_ty func_ty = {
      .ty = TD_VAR_TY_TY_FUNC,
  };

  func_ty.func.ret = arena_alloc(tchk->arena, sizeof(*func_ty.func.ret));
  *func_ty.func.ret = var_ty;

  // put prefix attributes on the func itself
  // but merge
  if (!func_ty.attrs.weak) {
    func_ty.attrs.weak = var_ty.attrs.weak;
  }
  if (!func_ty.attrs.format) {
    func_ty.attrs.format = var_ty.attrs.format;
  }
  func_ty.func.ret->attrs = (struct td_var_attrs){0};

  struct ast_paramlist *param_list = func_declarator->param_list;

  size_t num_params;
  if (param_list->num_params &&
      param_list->params[param_list->num_params - 1].ty ==
          AST_PARAM_TY_VARIADIC) {
    num_params = param_list->num_params - 1;
    func_ty.func.ty = TD_TY_FUNC_TY_VARIADIC;
  } else if (param_list->num_params &&
             param_list->params[0].ty == AST_PARAM_TY_VOID) {
    num_params = 0;
    func_ty.func.ty = TD_TY_FUNC_TY_KNOWN_ARGS;
  } else {
    num_params = param_list->num_params;
    func_ty.func.ty = param_list->num_params ? TD_TY_FUNC_TY_KNOWN_ARGS
                                             : TD_TY_FUNC_TY_UNKNOWN_ARGS;
  }

  func_ty.func.num_params = num_params;
  func_ty.func.params =
      arena_alloc(tchk->arena, sizeof(*func_ty.func.params) * num_params);

  for (size_t j = 0; j < num_params; j++) {
    struct ast_param *param = &param_list->params[j];
    struct td_ty_param *td_param = &func_ty.func.params[j];

    struct td_specifiers param_specifiers =
        type_specifiers(tchk, &param->specifier_list,
                        TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS |
                            TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS);

    switch (param->ty) {
    case AST_PARAM_TY_VOID:
    case AST_PARAM_TY_VARIADIC:
      continue;
    case AST_PARAM_TY_DECL: {
      struct td_var_declaration param_decl =
          type_declarator(tchk, &param_specifiers, &param->declarator, NULL,
                          TD_DECLARATOR_MODE_NORMAL);
      *td_param = (struct td_ty_param){.identifier = param_decl.var.identifier,
                                       .var_ty = param_decl.var_ty};
      break;
    }
    case AST_PARAM_TY_ABSTRACT_DECL: {
      struct td_var_ty param_var_ty = type_abstract_declarator(
          tchk, &param_specifiers, &param->abstract_declarator);

      *td_param = (struct td_ty_param){.identifier = MK_NULL_STR(),
                                       .var_ty = param_var_ty};
      break;
    }
    }

    if (td_param->var_ty.ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
      struct td_var_ty complete;
      (void)try_get_completed_aggregate(tchk, &td_param->var_ty, &complete);
      td_param->var_ty = complete;
    }
  }

  return func_ty;
}

static struct td_var_ty type_abstract_declarator_inner(
    struct typechk *tchk, const struct td_var_ty *outer_var_ty,
    const struct ast_abstract_declarator *abstract_declarator) {
  struct ast_pointer_list pointer_list = abstract_declarator->pointer_list;
  struct ast_direct_abstract_declarator_list decl_list =
      abstract_declarator->direct_abstract_declarator_list;

  struct td_var_ty var_ty = *outer_var_ty;

  for (size_t i = 0; i < pointer_list.num_pointers; i++) {
    struct ast_pointer *pointer = &pointer_list.pointers[i];
    struct td_specifiers ptr_specifiers = type_specifiers(
        tchk, &pointer->specifier_list, TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS);

    var_ty = td_var_ty_make_pointer(
        tchk, &var_ty, ptr_specifiers.type_specifier.type_qualifiers);
  }

  ssize_t inner_idx = -1;

  for (size_t i = decl_list.num_direct_abstract_declarators; i; i--) {
    struct ast_direct_abstract_declarator *direct_declarator =
        &decl_list.direct_abstract_declarators[i - 1];

    switch (direct_declarator->ty) {
    case AST_DIRECT_ABSTRACT_DECLARATOR_TY_PAREN_DECLARATOR:
      if (inner_idx != -1) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(DECL_MULTIPLE_SUB, decl_multiple_sub,
                                   direct_declarator->span,
                                   MK_INVALID_TEXT_POS(0),
                                   "declarator has multiple sub-declarators"));
        return TD_VAR_TY_UNKNOWN;
      }

      inner_idx = i - 1;
      break;
    case AST_DIRECT_ABSTRACT_DECLARATOR_TY_ARRAY_DECLARATOR: {
      var_ty = type_array_declarator(tchk, var_ty,
                                     direct_declarator->array_declarator, NULL);

      break;
    }
    case AST_DIRECT_ABSTRACT_DECLARATOR_TY_FUNC_DECLARATOR: {
      var_ty = type_func_declarator(tchk, var_ty,
                                    direct_declarator->func_declarator);
      break;
    }
    }
  }

  if (inner_idx != -1) {
    struct ast_direct_abstract_declarator *direct_declarator =
        &decl_list.direct_abstract_declarators[inner_idx];

    return type_abstract_declarator_inner(tchk, &var_ty,
                                          direct_declarator->paren_declarator);
  } else {
    return var_ty;
  }
}

static struct td_var_ty type_abstract_declarator(
    struct typechk *tchk, const struct td_specifiers *specifiers,
    const struct ast_abstract_declarator *abstract_declarator) {
  // TODO: handle storage class/qualifier/function specifiers
  return type_abstract_declarator_inner(tchk, &specifiers->type_specifier,
                                        abstract_declarator);
}

static struct td_var_ty
td_var_ty_for_typedef(struct typechk *tchk,
                      const struct lex_token *identifier) {
  struct var_table_entry *entry =
      vt_get_entry(&tchk->ty_table, VAR_TABLE_NS_TYPEDEF,
                   identifier_str(tchk->parser, identifier));

  if (!entry) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(BAD_TYPEDEF, bad_typedef, identifier->span,
                               MK_INVALID_TEXT_POS(0),
                               "typedef name does not exist"));
    return TD_VAR_TY_UNKNOWN;
  }

  if (entry->var_ty->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    struct td_var_ty complete;
    if (try_get_completed_aggregate(tchk, entry->var_ty, &complete)) {
      return complete;
    }
  }

  return *entry->var_ty;
}

struct td_declarator {
  struct td_var_ty var_ty;
  const char *identifier;
};

static struct td_var_declaration type_declarator_inner(
    struct typechk *tchk, const struct td_var_ty *outer_var_ty,
    const struct ast_declarator *declarator, const struct ast_init *init) {
  struct td_var_declaration var_decl;

  struct td_var_ty var_ty = *outer_var_ty;

  struct ast_pointer_list pointer_list = declarator->pointer_list;
  struct ast_direct_declarator_list decl_list =
      declarator->direct_declarator_list;

  for (size_t i = 0; i < pointer_list.num_pointers; i++) {
    struct ast_pointer *pointer = &pointer_list.pointers[i];
    struct td_specifiers ptr_specifiers = type_specifiers(
        tchk, &pointer->specifier_list, TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS);

    var_ty = td_var_ty_make_pointer(
        tchk, &var_ty, ptr_specifiers.type_specifier.type_qualifiers);
  }

  bool found_ident = false;
  ssize_t inner_idx = -1;

  // FIXME: we need to properly copy attributes from base type to inner types

  for (size_t i = decl_list.num_direct_declarators; i; i--) {
    struct ast_direct_declarator *direct_declarator =
        &decl_list.direct_declarators[i - 1];

    switch (direct_declarator->ty) {
    case AST_DIRECT_DECLARATOR_TY_IDENTIFIER:
      var_decl.var = (struct td_var){
          // FIXME: other fields
          .ty = TD_VAR_VAR_TY_VAR,
          .scope = vt_cur_scope(&tchk->var_table),
          .identifier =
              identifier_str(tchk->parser, &direct_declarator->identifier),
      };
      found_ident = true;
      break;
    case AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR: {
      if (inner_idx != -1) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(DECL_MULTIPLE_SUB, decl_multiple_sub,
                                   direct_declarator->span,
                                   MK_INVALID_TEXT_POS(0),
                                   "declarator has multiple sub-declarators"));
        var_ty = TD_VAR_TY_UNKNOWN;
      }

      inner_idx = i - 1;
      found_ident = true;
      break;
    }
    case AST_DIRECT_DECLARATOR_TY_ARRAY_DECLARATOR: {
      var_ty = type_array_declarator(tchk, var_ty,
                                     direct_declarator->array_declarator, init);

      break;
    }
    case AST_DIRECT_DECLARATOR_TY_FUNC_DECLARATOR: {
      var_ty = type_func_declarator(tchk, var_ty,
                                    direct_declarator->func_declarator);
      break;
    }
    }
  }

  invariant_assert(found_ident, "decl without identifier?");

  if (inner_idx != -1) {
    struct ast_direct_declarator *direct_declarator =
        &decl_list.direct_declarators[inner_idx];

    return type_declarator_inner(tchk, &var_ty,
                                 direct_declarator->paren_declarator, init);
  } else {
    var_decl.init = NULL;
    var_decl.var_ty = var_ty;

    return var_decl;
  }
}

static void
type_attribute_list(struct typechk *tchk,
                                const struct ast_attribute_list *attribute_list,
                                struct td_var_attrs *attrs);

static void
type_attribute_specifier_list(struct typechk *tchk,
                                const struct ast_attribute_specifier_list *attribute_specifier_list,
                                struct td_var_attrs *attrs) {
  for (size_t i = 0; i < attribute_specifier_list->num_attribute_specifiers; i++) {
    type_attribute_list(tchk, &attribute_specifier_list->attribute_specifiers[i].attribute_list, attrs);
  }
}

static struct td_var_declaration
type_declarator(struct typechk *tchk, const struct td_specifiers *specifiers,
                const struct ast_declarator *declarator,
                const struct ast_init *init, enum td_declarator_mode mode) {

  // TODO: handle storage class/qualifier/function specifiers
  struct td_var_declaration declaration = type_declarator_inner(
      tchk, &specifiers->type_specifier, declarator, init);

  type_attribute_specifier_list(tchk, &declarator->attribute_specifier_list, &declaration.var_ty.attrs);

  if (declarator->bitfield_size) {
    if (mode == TD_DECLARATOR_MODE_NORMAL) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              BAD_BITFIELD_CONTEXT, bad_bitfield_context, declarator->span,
              MK_INVALID_TEXT_POS(0),
              "subtraction on pointers of different kinds is forbidden"));
    }

    declaration.ty = TD_VAR_DECLARATION_TY_BITFIELD;
    declaration.bitfield_width =
        type_constant_integral_expr(tchk, declarator->bitfield_size);
  } else {
    declaration.ty = TD_VAR_DECLARATION_TY_VAR;
  }

  return declaration;
}

static struct sized_str normalize_attr_ident(struct sized_str name) {
  name = szstr_strip_prefix(name, MK_SIZED("__"));
  name = szstr_strip_suffix(name, MK_SIZED("__"));
  return name;
}

static struct td_attr_format
type_attr_format(struct typechk *tchk, const struct ast_attribute *attribute) {
  if (attribute->num_params != 3) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            BAD_PARAM_COUNT, bad_param_count, attribute->span,
            MK_INVALID_TEXT_POS(0),
            arena_alloc_snprintf(tchk->arena, "expected 3 params, found %zu",
                                 attribute->num_params)));

    return (struct td_attr_format){0};
  }

  struct ast_expr *archetype_expr = attribute->params[0].expr;
  if (archetype_expr->ty != AST_EXPR_TY_VAR) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            EXPECTED_VAR, expected_var, archetype_expr->span,
            MK_INVALID_TEXT_POS(0),
            "first argument to 'format' attribute should be an identifier"));

    return (struct td_attr_format){0};
  }

  struct sized_str archetype_name =
      identifier_str(tchk->parser, &archetype_expr->var.identifier);
  struct sized_str normalized = normalize_attr_ident(archetype_name);

  if (szstreq(normalized, MK_SIZED("scanf"))) {
    return (struct td_attr_format){0};
  } else if (!szstreq(normalized, MK_SIZED("printf"))) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            UNRECOGNISED_ATTR, unrecognised_attr, archetype_expr->span,
            MK_INVALID_TEXT_POS(0),
            arena_alloc_snprintf(
                tchk->arena,
                "unrecognised format type '%.*s', (expected 'printf')",
                (int)archetype_name.len, archetype_name.str)));

    return (struct td_attr_format){0};
  }

  enum td_attr_format_archetype archetype = TD_ATTR_FORMAT_ARCHETYPE_PRINTF;

  unsigned long long string_index_expr =
      type_constant_integral_expr(tchk, attribute->params[1].expr);

  // TODO: validate within bounds for function applied + `string_index` not zero

  unsigned long long first_to_check_expr =
      type_constant_integral_expr(tchk, attribute->params[2].expr);

  return (struct td_attr_format){.archetype = archetype,
                                 .string_index = string_index_expr,
                                 .first_to_check = first_to_check_expr};
}

// writes into vector as a single specifier list may contain many attribute
// lists so more efficient
static void
type_attribute_list(struct typechk *tchk,
                                const struct ast_attribute_list *attribute_list,
                                struct td_var_attrs *attrs) {
  for (size_t j = 0; j < attribute_list->num_attributes; j++) {
    struct ast_attribute *attr = &attribute_list->attributes[j];

    switch (attr->ty) {
    case AST_ATTRIBUTE_TY_EMPTY:
      continue;
    case AST_ATTRIBUTE_TY_NAMED:
    case AST_ATTRIBUTE_TY_PARAMETERIZED: {
      // TODO: when we support many more, default this to false
      bool silent_ignore = true;
      if (attr->prefix) {
        struct sized_str prefix = identifier_str(tchk->parser, attr->prefix);
        struct sized_str prefix_norm = normalize_attr_ident(prefix);

        // currently the prefix is not used, but we ignore unknown ones and
        // ignore unknown attributes from gcc/clang
        if (szstreq(prefix_norm, MK_SIZED("jcc"))) {
          silent_ignore = false;
        } else if (szstreq(prefix_norm, MK_SIZED("gnu"))) {
          silent_ignore = true;
        } else if (szstreq(prefix_norm, MK_SIZED("clang"))) {
          silent_ignore = true;
        } else {
          continue;
        }
      }

      struct sized_str name = identifier_str(tchk->parser, &attr->name);
      name = normalize_attr_ident(name);

      if (szstreq(name, MK_SIZED("weak")) ||
          szstreq(name, MK_SIZED("weak_import"))) {
        attrs->weak = true;
      } else if (szstreq(name, MK_SIZED("format"))) {
        attrs->format = arena_alloc(tchk->arena, sizeof(*attrs->format));
        *attrs->format = type_attr_format(tchk, attr);
      } else if (!silent_ignore) {
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(UNRECOGNISED_ATTR, unrecognised_attr,
                                   attr->span, MK_INVALID_TEXT_POS(0),
                                   arena_alloc_snprintf(tchk->arena, "unrecognised attribute '%.*s' ignored", (int)name.len, name.str)));
      }
      break;
    }
    }
  }
}

static struct td_var_ty
type_type_or_expr(struct typechk *tchk,
                  const struct ast_type_or_expr *type_or_expr);

static struct td_specifiers
type_specifiers(struct typechk *tchk,
                const struct ast_declaration_specifier_list *list,
                enum td_specifier_allow allow) {
  struct td_specifiers specifiers = {
      .storage = TD_STORAGE_CLASS_SPECIFIER_NONE,
      .function = TD_FUNCTION_SPECIFIER_FLAG_NONE,
      .type_specifier = TD_VAR_TY_UNKNOWN,
  };

  struct td_var_attrs attrs = {0};
  enum td_type_qualifier_flags type_qualifiers = TD_TYPE_QUALIFIER_FLAG_NONE;

  int long_count = 0, int_count = 0, signed_count = 0, unsigned_count = 0;
  int type_specifier_count = 0;

  struct ast_type_specifier last_specifier;

  for (size_t i = 0; i < list->num_decl_specifiers; i++) {
    struct ast_declaration_specifier specifier = list->decl_specifiers[i];

    switch (specifier.ty) {
    case AST_DECL_SPECIFIER_TY_ATTRIBUTE_SPECIFIER:
      type_attribute_list(
          tchk, &specifier.attribute_specifier.attribute_list,
          &attrs);
      break;
    case AST_DECL_SPECIFIER_TY_STORAGE_CLASS_SPECIFIER:
      if (!(allow & TD_SPECIFIER_ALLOW_STORAGE_CLASS_SPECIFIERS)) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(
                BAD_STORAGE_CONTEXT, bad_storage_context, specifier.span,
                MK_INVALID_TEXT_POS(0),
                "storage class specifier not valid in this context"));
      }

      if (specifiers.storage != TD_STORAGE_CLASS_SPECIFIER_NONE) {
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(
                STORAGE_CLASS_MULTIPLE, storage_class_multiple, specifier.span,
                MK_INVALID_TEXT_POS(0), "multiple storage specifiers"));
      }

      switch (specifier.storage_class_specifier) {
      case AST_STORAGE_CLASS_SPECIFIER_TYPEDEF:
        specifiers.storage = TD_STORAGE_CLASS_SPECIFIER_TYPEDEF;
        break;
      case AST_STORAGE_CLASS_SPECIFIER_EXTERN:
        specifiers.storage = TD_STORAGE_CLASS_SPECIFIER_EXTERN;
        break;
      case AST_STORAGE_CLASS_SPECIFIER_STATIC:
        specifiers.storage = TD_STORAGE_CLASS_SPECIFIER_STATIC;
        break;
      case AST_STORAGE_CLASS_SPECIFIER_AUTO:
        specifiers.storage = TD_STORAGE_CLASS_SPECIFIER_AUTO;
        break;
      case AST_STORAGE_CLASS_SPECIFIER_REGISTER:
        specifiers.storage = TD_STORAGE_CLASS_SPECIFIER_REGISTER;
        break;
      }

      break;
    case AST_DECL_SPECIFIER_TY_TYPE_QUALIFIER:
      if (!(allow & TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS)) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(BAD_TYPE_QUALIFIER_CONTEXT,
                                   bad_type_qualifier_context, specifier.span,
                                   MK_INVALID_TEXT_POS(0),
                                   "type qualifier not valid in this context"));
      }

      switch (specifier.type_qualifier) {
      case AST_TYPE_QUALIFIER_CONST:
        if (type_qualifiers & TD_TYPE_QUALIFIER_FLAG_CONST) {
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(TYPE_QUALIFIER_DUPLICATE,
                                     type_qualifier_duplicate, specifier.span,
                                     MK_INVALID_TEXT_POS(0),
                                     "duplicate const flag"));
        }

        type_qualifiers |= TD_TYPE_QUALIFIER_FLAG_CONST;
        break;
      case AST_TYPE_QUALIFIER_VOLATILE:
        if (type_qualifiers & TD_TYPE_QUALIFIER_FLAG_VOLATILE) {
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(TYPE_QUALIFIER_DUPLICATE,
                                     type_qualifier_duplicate, specifier.span,
                                     MK_INVALID_TEXT_POS(0),
                                     "duplicate volatile flag"));
        }

        type_qualifiers |= TD_TYPE_QUALIFIER_FLAG_VOLATILE;
        break;
      case AST_TYPE_QUALIFIER_RESTRICT:
        if (type_qualifiers & TD_TYPE_QUALIFIER_FLAG_RESTRICT) {
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(TYPE_QUALIFIER_DUPLICATE,
                                     type_qualifier_duplicate, specifier.span,
                                     MK_INVALID_TEXT_POS(0),
                                     "duplicate restrict flag"));
        }

        type_qualifiers |= TD_TYPE_QUALIFIER_FLAG_RESTRICT;
        break;
      case AST_TYPE_QUALIFIER_NONNULL:
        if (type_qualifiers & TD_TYPE_QUALIFIER_FLAG_NONNULL) {
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(TYPE_QUALIFIER_DUPLICATE,
                                     type_qualifier_duplicate, specifier.span,
                                     MK_INVALID_TEXT_POS(0),
                                     "duplicate nonnull flag"));
        }

        type_qualifiers |= TD_TYPE_QUALIFIER_FLAG_NONNULL;
        break;
      case AST_TYPE_QUALIFIER_NULLABLE:
        if (type_qualifiers & TD_TYPE_QUALIFIER_FLAG_NULLABLE) {
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(TYPE_QUALIFIER_DUPLICATE,
                                     type_qualifier_duplicate, specifier.span,
                                     MK_INVALID_TEXT_POS(0),
                                     "duplicate nullable flag"));
        }

        type_qualifiers |= TD_TYPE_QUALIFIER_FLAG_NULLABLE;
        break;
      }
      break;
    case AST_DECL_SPECIFIER_TY_FUNCTION_SPECIFIER:
      if (!(allow & TD_SPECIFIER_ALLOW_FUNCTION_SPECIFIERS)) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(
                BAD_FUNCTION_SPECIFIER_CONTEXT, bad_function_specifier_context,
                specifier.span, MK_INVALID_TEXT_POS(0),
                "function specifier not valid in this context"));
      }

      enum td_function_specifier_flags flag;
      switch (specifier.function_specifier) {
      case AST_FUNCTION_SPECIFIER_INLINE:
        flag = TD_FUNCTION_SPECIFIER_FLAG_INLINE;
        break;
      case AST_FUNCTION_SPECIFIER_NORETURN:
        flag = TD_FUNCTION_SPECIFIER_FLAG_NORETURN;
        break;
      }

      if (specifiers.function & flag) {
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(FUNCTION_SPECIFIER_MULTIPLE,
                                   function_specifier_multiple, specifier.span,
                                   MK_INVALID_TEXT_POS(0),
                                   "duplicate function specifiers"));
      }

      specifiers.function |= flag;
      break;
    case AST_DECL_SPECIFIER_TY_TYPE_SPECIFIER: {
      if (!(allow & TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS)) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(BAD_TYPE_SPECIFIER_CONTEXT,
                                   bad_type_specifier_context, specifier.span,
                                   MK_INVALID_TEXT_POS(0),
                                   "type specifier not valid in this context"));
      }

      type_specifier_count++;
      if (specifier.type_specifier.ty == AST_TYPE_SPECIFIER_TY_KW) {
        enum ast_type_specifier_kw kw =
            specifier.type_specifier.type_specifier_kw;
        if (kw == AST_TYPE_SPECIFIER_KW_INT) {
          if (int_count) {
            tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
            compiler_diagnostics_add(
                tchk->diagnostics,
                MK_SEMANTIC_DIAGNOSTIC(
                    BAD_TYPE_SPECIFIERS, bad_type_specifiers, specifier.span,
                    MK_INVALID_TEXT_POS(0),
                    "multiple 'int' type specifiers is invalid"));
          } else {
            int_count++;
          }
        } else if (kw == AST_TYPE_SPECIFIER_KW_LONG) {
          if (long_count >= 2) {
            tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
            compiler_diagnostics_add(
                tchk->diagnostics,
                MK_SEMANTIC_DIAGNOSTIC(
                    BAD_TYPE_SPECIFIERS, bad_type_specifiers, specifier.span,
                    MK_INVALID_TEXT_POS(0),
                    "more than two 'long' type specifiers is invalid"));
          } else {
            long_count++;
          }
        } else if (kw == AST_TYPE_SPECIFIER_KW_SIGNED) {
          if (signed_count) {
            tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
            compiler_diagnostics_add(
                tchk->diagnostics,
                MK_SEMANTIC_DIAGNOSTIC(
                    BAD_TYPE_SPECIFIERS, bad_type_specifiers, specifier.span,
                    MK_INVALID_TEXT_POS(0),
                    "multiple 'signed' type specifiers is invalid"));
          } else if (unsigned_count) {
            tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
            compiler_diagnostics_add(
                tchk->diagnostics,
                MK_SEMANTIC_DIAGNOSTIC(BAD_TYPE_SPECIFIERS, bad_type_specifiers,
                                       specifier.span, MK_INVALID_TEXT_POS(0),
                                       "'signed' type specifiers is invalid "
                                       "after an 'unsigned' type specifier"));
          } else {
            signed_count++;
          }
        } else if (kw == AST_TYPE_SPECIFIER_KW_UNSIGNED) {
          if (unsigned_count) {
            tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
            compiler_diagnostics_add(
                tchk->diagnostics,
                MK_SEMANTIC_DIAGNOSTIC(
                    BAD_TYPE_SPECIFIERS, bad_type_specifiers, specifier.span,
                    MK_INVALID_TEXT_POS(0),
                    "multiple 'unsigned' type specifiers is invalid"));
          } else if (signed_count) {
            tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
            compiler_diagnostics_add(
                tchk->diagnostics,
                MK_SEMANTIC_DIAGNOSTIC(BAD_TYPE_SPECIFIERS, bad_type_specifiers,
                                       specifier.span, MK_INVALID_TEXT_POS(0),
                                       "'unsigned' type specifiers is invalid "
                                       "after an 'signed' type specifier"));
          } else {

            unsigned_count++;
          }
        } else {
          last_specifier = specifier.type_specifier;
        }
      } else {
        last_specifier = specifier.type_specifier;
      }
    }
    }
  }

  DEBUG_ASSERT(int_count <= 1 && long_count <= 2 && signed_count <= 1 &&
                   unsigned_count <= 1 &&
                   (signed_count == 0 || unsigned_count == 0),
               "diagnostic handling should have prevented");

  int total_modifiers = int_count + long_count + signed_count + unsigned_count;
  int remaining = type_specifier_count - total_modifiers;

  if (remaining > 1) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(BAD_TYPE_SPECIFIERS, bad_type_specifiers,
                               list->span, MK_INVALID_TEXT_POS(0),
                               "multiple type specifiers did not make sense"));
  }

  // only uses signed/unsigned/int/long

  if (total_modifiers) {
    // default in case anything goes awry
    enum well_known_ty wk = WELL_KNOWN_TY_SIGNED_INT;
    if (!remaining) {
      switch (long_count) {
      case 0:
        wk = unsigned_count ? WELL_KNOWN_TY_UNSIGNED_INT
                            : WELL_KNOWN_TY_SIGNED_INT;
        break;
      case 1:
        wk = unsigned_count ? WELL_KNOWN_TY_UNSIGNED_LONG
                            : WELL_KNOWN_TY_SIGNED_LONG;
        break;
      case 2:
        wk = unsigned_count ? WELL_KNOWN_TY_UNSIGNED_LONG_LONG
                            : WELL_KNOWN_TY_SIGNED_LONG_LONG;
        break;
      }
    } else if (last_specifier.ty == AST_TYPE_SPECIFIER_TY_KW &&
               last_specifier.type_specifier_kw ==
                   AST_TYPE_SPECIFIER_KW_DOUBLE) {
      wk = WELL_KNOWN_TY_LONG_DOUBLE;
    } else if (last_specifier.ty == AST_TYPE_SPECIFIER_TY_KW &&
               last_specifier.type_specifier_kw == AST_TYPE_SPECIFIER_KW_CHAR) {
      wk = unsigned_count ? WELL_KNOWN_TY_UNSIGNED_CHAR
           : signed_count ? WELL_KNOWN_TY_SIGNED_CHAR
                          : WELL_KNOWN_TY_CHAR;
    } else if (last_specifier.ty == AST_TYPE_SPECIFIER_TY_KW &&
               last_specifier.type_specifier_kw ==
                   AST_TYPE_SPECIFIER_KW_SHORT) {
      wk = unsigned_count ? WELL_KNOWN_TY_UNSIGNED_SHORT
                          : WELL_KNOWN_TY_SIGNED_SHORT;
    }

    specifiers.type_specifier.ty = TD_VAR_TY_TY_WELL_KNOWN;
    specifiers.type_specifier.well_known = wk;
  } else if (remaining) {
    if (last_specifier.ty == AST_TYPE_SPECIFIER_TY_KW &&
        last_specifier.type_specifier_kw == AST_TYPE_SPECIFIER_KW_VOID) {
      specifiers.type_specifier.ty = TD_VAR_TY_TY_VOID;

    } else {
      switch (last_specifier.ty) {
      case AST_TYPE_SPECIFIER_TY_KW: {
        enum well_known_ty wk = WELL_KNOWN_TY_SIGNED_INT;

        switch (last_specifier.type_specifier_kw) {
        case AST_TYPE_SPECIFIER_KW_CHAR:
          wk = WELL_KNOWN_TY_CHAR;
          break;
        case AST_TYPE_SPECIFIER_KW_SHORT:
          wk = WELL_KNOWN_TY_SIGNED_SHORT;
          break;
        case AST_TYPE_SPECIFIER_KW_FLOAT:
          wk = WELL_KNOWN_TY_FLOAT;
          break;
        case AST_TYPE_SPECIFIER_KW_DOUBLE:
          wk = WELL_KNOWN_TY_DOUBLE;
          break;
        case AST_TYPE_SPECIFIER_KW_BOOL:
          wk = WELL_KNOWN_TY_BOOL;
          break;
        case AST_TYPE_SPECIFIER_KW_COMPLEX:
          TODO("complex");
          // wk = WELL_KNOWN_TY_COMPLEX;
        case AST_TYPE_SPECIFIER_KW_HALF:
          wk = WELL_KNOWN_TY_HALF;
          break;
        case AST_TYPE_SPECIFIER_KW_UINT128:
          wk = WELL_KNOWN_TY_UINT128;
          break;
        default:
          TODO("other type specifiers");
        }

        specifiers.type_specifier.ty = TD_VAR_TY_TY_WELL_KNOWN;
        specifiers.type_specifier.well_known = wk;
        break;
      }
      case AST_TYPE_SPECIFIER_STRUCT_OR_UNION:
        specifiers.type_specifier = td_var_ty_for_struct_or_union(
            tchk, &last_specifier.struct_or_union_specifier);
        break;
      case AST_TYPE_SPECIFIER_ENUM:
        specifiers.type_specifier =
            td_var_ty_for_enum(tchk, &last_specifier.enum_specifier);
        break;
      case AST_TYPE_SPECIFIER_TYPEDEF_NAME:
        specifiers.type_specifier =
            td_var_ty_for_typedef(tchk, &last_specifier.typedef_name);
        break;
      case AST_TYPE_SPECIFIER_TYPEOF: {
        specifiers.type_specifier =
            type_type_or_expr(tchk, &last_specifier.type_of);
        break;
      }
      case AST_TYPE_SPECIFIER_TYPEOF_UNQUAL: {
        specifiers.type_specifier =
            type_type_or_expr(tchk, &last_specifier.type_of_unqual);

        specifiers.type_specifier.type_qualifiers = TD_TYPE_QUALIFIER_FLAG_NONE;
        break;
      }
      }
    }
  }

  // TODO: merge with this in type_func_declarator as a general "merge attrs" fn
  if (!specifiers.type_specifier.attrs.weak) {
    specifiers.type_specifier.attrs.weak = attrs.weak;
  }

  if (!specifiers.type_specifier.attrs.format) {
    specifiers.type_specifier.attrs.format = attrs.format;
  }

  specifiers.type_specifier.type_qualifiers |= type_qualifiers;

  return specifiers;
}

enum type_expr_flags {
  TYPE_EXPR_FLAGS_NONE = 0,
  // negative flag because in most places arrays *do* decay
  TYPE_EXPR_FLAGS_ARRAYS_DONT_DECAY = 1,
};

static struct td_expr type_expr(struct typechk *tchk,
                                enum type_expr_flags flags,
                                const struct ast_expr *expr);

static struct td_expr type_ternary(struct typechk *tchk,
                                   const struct ast_ternary *ternary) {
  struct td_ternary td_ternary = {
      .cond = arena_alloc(tchk->arena, sizeof(*td_ternary.cond)),
      .true_expr = arena_alloc(tchk->arena, sizeof(*td_ternary.true_expr)),
      .false_expr = arena_alloc(tchk->arena, sizeof(*td_ternary.false_expr)),
  };

  *td_ternary.cond = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, ternary->cond);
  *td_ternary.true_expr = perform_integer_promotion(
      tchk, type_expr(tchk, TYPE_EXPR_FLAGS_NONE, ternary->true_expr));
  *td_ternary.false_expr = perform_integer_promotion(
      tchk, type_expr(tchk, TYPE_EXPR_FLAGS_NONE, ternary->false_expr));

  struct text_span context = ternary->span;
  struct td_var_ty result_ty = resolve_usual_arithmetic_conversions(
      tchk, &td_ternary.true_expr->var_ty, &td_ternary.false_expr->var_ty,
      context);

  *td_ternary.true_expr =
      add_cast_if_needed(tchk, ternary->span, *td_ternary.true_expr, result_ty);
  *td_ternary.false_expr = add_cast_if_needed(
      tchk, ternary->span, *td_ternary.false_expr, result_ty);

  return (struct td_expr){
      .ty = TD_EXPR_TY_TERNARY, .var_ty = result_ty, .ternary = td_ternary};
}

static struct td_var_ty type_type_name(struct typechk *tchk,
                                       const struct ast_type_name *type_name) {
  struct td_specifiers specifiers =
      type_specifiers(tchk, &type_name->specifier_list,
                      TD_SPECIFIER_ALLOW_FUNCTION_SPECIFIERS |
                          TD_SPECIFIER_ALLOW_STORAGE_CLASS_SPECIFIERS |
                          TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS |
                          TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS);

  struct td_var_ty var_ty = type_abstract_declarator(
      tchk, &specifiers, &type_name->abstract_declarator);

  if (var_ty.ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    struct td_var_ty complete;
    if (try_get_completed_aggregate(tchk, &var_ty, &complete)) {
      var_ty = complete;
    }
  }

  return var_ty;
}

static struct td_arglist type_arglist(struct typechk *tchk,
                                      const struct ast_arglist *arg_list) {
  struct td_arglist td_arg_list = (struct td_arglist){
      .args = arena_alloc(tchk->arena,
                          sizeof(*td_arg_list.args) * arg_list->num_args),
      .num_args = arg_list->num_args};

  for (size_t i = 0; i < arg_list->num_args; i++) {
    td_arg_list.args[i] =
        type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &arg_list->args[i]);
  }

  return td_arg_list;
}

enum printf_argspec_flags {
  PRINTF_ARGSPEC_FLAG_NONE = 0,
  PRINTF_ARGSPEC_FLAG_LJUST = 1 << 0,
  PRINTF_ARGSPEC_FLAG_SIGN = 1 << 1,
  PRINTF_ARGSPEC_FLAG_SPACE = 1 << 2,
  PRINTF_ARGSPEC_FLAG_ALT = 1 << 3,
  PRINTF_ARGSPEC_FLAG_LEADING_ZEROS = 1 << 4,
};

enum printf_argspec_value_ty {
  PRINTF_ARGSPEC_VALUE_TY_NONE,
  PRINTF_ARGSPEC_VALUE_TY_CNST,
  PRINTF_ARGSPEC_VALUE_TY_VAR,
};

struct printf_argspec_value {
  enum printf_argspec_value_ty ty;

  union {
    int cnst;
  };
};

enum printf_argspec_length_mod {
  PRINTF_ARGSPEC_LENGTH_MOD_NONE,
  PRINTF_ARGSPEC_LENGTH_MOD_HH,
  PRINTF_ARGSPEC_LENGTH_MOD_H,
  PRINTF_ARGSPEC_LENGTH_MOD_L,
  PRINTF_ARGSPEC_LENGTH_MOD_LL,
  PRINTF_ARGSPEC_LENGTH_MOD_J,
  PRINTF_ARGSPEC_LENGTH_MOD_Z,
  PRINTF_ARGSPEC_LENGTH_MOD_T,

  PRINTF_ARGSPEC_LENGTH_MOD_L_UP,
};

enum printf_argspec_specifier {
  PRINTF_ARGSPEC_SPECIFIER_INVALID,

  // %
  PRINTF_ARGSPEC_SPECIFIER_LITERAL_PERCENT,

  // c
  PRINTF_ARGSPEC_SPECIFIER_CHAR,

  // s
  PRINTF_ARGSPEC_SPECIFIER_STRING,

  // d/i
  PRINTF_ARGSPEC_SPECIFIER_SIGNED_INT,

  // o
  PRINTF_ARGSPEC_SPECIFIER_OCTAL_UNSIGNED_INT,

  // x/X
  PRINTF_ARGSPEC_SPECIFIER_HEX_UNSIGNED_INT,

  // u
  PRINTF_ARGSPEC_SPECIFIER_UNSIGNED_INT,

  // f/F
  PRINTF_ARGSPEC_SPECIFIER_FLOAT,

  // e/E
  PRINTF_ARGSPEC_SPECIFIER_EXP_FLOAT,

  // a/A
  PRINTF_ARGSPEC_SPECIFIER_HEX_EXP_FLOAT,

  // g/G
  PRINTF_ARGSPEC_SPECIFIER_VAR_FLOAT,

  // n
  PRINTF_ARGSPEC_SPECIFIER_NUM_WRITTEN,

  // p
  PRINTF_ARGSPEC_SPECIFIER_POINTER,
};

struct printf_argspec {
  size_t offset;
  size_t specifier_offset;
  size_t len;

  enum printf_argspec_flags flags;
  struct printf_argspec_value min_width;
  struct printf_argspec_value precision;
  enum printf_argspec_length_mod length_mod;
  enum printf_argspec_specifier specifier;
};

struct printf_args {
  struct printf_argspec *argspecs;
  size_t num_argspecs;
};

static enum printf_argspec_length_mod
parse_printf_argspec_length_mod(const char *str, size_t *idx, size_t len) {
  if (*idx >= len) {
    return PRINTF_ARGSPEC_LENGTH_MOD_NONE;
  }

  switch (str[*idx]) {
  case 'h': {
    (*idx)++;

    if (*idx + 1 < len && str[*idx + 1] == 'h') {
      (*idx)++;
      return PRINTF_ARGSPEC_LENGTH_MOD_HH;
    }
    return PRINTF_ARGSPEC_LENGTH_MOD_H;
  }
  case 'l': {
    (*idx)++;

    if (*idx + 1 < len && str[*idx + 1] == 'l') {
      (*idx)++;
      return PRINTF_ARGSPEC_LENGTH_MOD_LL;
    }
    return PRINTF_ARGSPEC_LENGTH_MOD_L;
  }
  case 'j': {
    (*idx)++;
    return PRINTF_ARGSPEC_LENGTH_MOD_J;
  }
  case 'z': {
    (*idx)++;
    return PRINTF_ARGSPEC_LENGTH_MOD_Z;
  }
  case 't': {
    (*idx)++;
    return PRINTF_ARGSPEC_LENGTH_MOD_T;
  }
  case 'L': {
    (*idx)++;
    return PRINTF_ARGSPEC_LENGTH_MOD_L_UP;
  }
  default:
    return PRINTF_ARGSPEC_LENGTH_MOD_NONE;
  }
}

static enum printf_argspec_specifier
parse_printf_argspec_specifier(const char *str, size_t idx, size_t len) {
  if (idx >= len) {
    return PRINTF_ARGSPEC_SPECIFIER_INVALID;
  }

  switch (str[idx]) {
  case 'c':
    return PRINTF_ARGSPEC_SPECIFIER_CHAR;
  case 's':
    return PRINTF_ARGSPEC_SPECIFIER_STRING;

  case 'd':
  case 'i':
    return PRINTF_ARGSPEC_SPECIFIER_SIGNED_INT;

  case 'o':
    return PRINTF_ARGSPEC_SPECIFIER_OCTAL_UNSIGNED_INT;
  case 'x':
  case 'X':
    return PRINTF_ARGSPEC_SPECIFIER_HEX_UNSIGNED_INT;
  case 'u':
    return PRINTF_ARGSPEC_SPECIFIER_UNSIGNED_INT;

  case 'f':
  case 'F':
    return PRINTF_ARGSPEC_SPECIFIER_FLOAT;
  case 'e':
  case 'E':
    return PRINTF_ARGSPEC_SPECIFIER_EXP_FLOAT;
  case 'a':
  case 'A':
    return PRINTF_ARGSPEC_SPECIFIER_HEX_EXP_FLOAT;
  case 'g':
  case 'G':
    return PRINTF_ARGSPEC_SPECIFIER_VAR_FLOAT;

  case 'n':
    return PRINTF_ARGSPEC_SPECIFIER_NUM_WRITTEN;

  case 'p':
    return PRINTF_ARGSPEC_SPECIFIER_POINTER;

  default:
    return PRINTF_ARGSPEC_SPECIFIER_INVALID;
  }
}
static enum printf_argspec_flags
parse_printf_argspec_flag(const char *str, size_t idx, size_t len) {
  if (idx >= len) {
    return PRINTF_ARGSPEC_FLAG_NONE;
  }

  switch (str[idx]) {
  case '-':
    return PRINTF_ARGSPEC_FLAG_LJUST;
  case '+':
    return PRINTF_ARGSPEC_FLAG_SIGN;
  case ' ':
    return PRINTF_ARGSPEC_FLAG_SPACE;
  case '#':
    return PRINTF_ARGSPEC_FLAG_ALT;
  case '0':
    return PRINTF_ARGSPEC_FLAG_LEADING_ZEROS;
  default:
    return PRINTF_ARGSPEC_FLAG_NONE;
  }
}

static struct printf_argspec_value
parse_printf_argspec_value(const char *str, size_t *idx, size_t len) {
  if (*idx >= len) {
    return (struct printf_argspec_value){.ty = PRINTF_ARGSPEC_VALUE_TY_NONE};
  }

  if (str[*idx] == '*') {
    (*idx)++;
    return (struct printf_argspec_value){.ty = PRINTF_ARGSPEC_VALUE_TY_VAR};
  } else if (isdigit(str[*idx])) {
    int cnst = str[(*idx)++] - '0';

    while (isdigit(str[*idx] && *idx < len)) {
      cnst *= 10;
      cnst += str[(*idx)] - '0';

      (*idx)++;
    }

    return (struct printf_argspec_value){.ty = PRINTF_ARGSPEC_VALUE_TY_CNST,
                                         .cnst = cnst};
  }

  return (struct printf_argspec_value){.ty = PRINTF_ARGSPEC_VALUE_TY_NONE};
}

static struct printf_args parse_printf_format(struct typechk *tchk,
                                              struct td_ascii_str fmt) {
  struct vector *argspecs =
      vector_create_in_arena(sizeof(struct printf_argspec), tchk->arena);

  for (size_t i = 0; i < fmt.len; i++) {
    unsigned char c = fmt.value[i];

    if (c >= 128 || c != '%') {
      continue;
    }

    struct printf_argspec argspec = {.offset = i};

    if (i + 1 >= fmt.len) {
      argspec.specifier = PRINTF_ARGSPEC_SPECIFIER_INVALID;
    } else {
      i++;

      while (i < fmt.len) {
        enum printf_argspec_flags flag =
            parse_printf_argspec_flag(fmt.value, i, fmt.len);

        if (flag == PRINTF_ARGSPEC_FLAG_NONE) {
          break;
        }

        argspec.flags |= flag;
        i++;
      }

      if (argspec.flags & PRINTF_ARGSPEC_FLAG_SIGN) {
        argspec.flags &= ~PRINTF_ARGSPEC_FLAG_SPACE;
      }

      if (argspec.flags & PRINTF_ARGSPEC_FLAG_LEADING_ZEROS) {
        argspec.flags &= ~PRINTF_ARGSPEC_FLAG_LJUST;
      }

      argspec.min_width = parse_printf_argspec_value(fmt.value, &i, fmt.len);

      if (i < fmt.len) {
        c = fmt.value[i];

        if (c == '.') {
            i++;
          argspec.precision =
              parse_printf_argspec_value(fmt.value, &i, fmt.len);
        }
      }

      argspec.length_mod =
          parse_printf_argspec_length_mod(fmt.value, &i, fmt.len);
      argspec.specifier = parse_printf_argspec_specifier(fmt.value, i, fmt.len);
    }

    argspec.len = i - argspec.offset + 1;
    vector_push_back(argspecs, &argspec);
  }

  return (struct printf_args){.argspecs = vector_head(argspecs),
                              .num_argspecs = vector_length(argspecs)};
}

static struct td_expr type_call(struct typechk *tchk,
                                const struct ast_call *call) {
  struct td_call td_call = {
      .target = arena_alloc(tchk->arena, sizeof(*td_call.target)),
      .arg_list = type_arglist(tchk, &call->arg_list)};

  *td_call.target = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, call->target);

  struct td_var_ty target_var_ty = td_call.target->var_ty;

  if (target_var_ty.ty == TD_VAR_TY_TY_POINTER) {
    // one level of implicit deref is allowed for functions
    // e.g directly calling a function pointer as `foo()`
    target_var_ty = *target_var_ty.pointer.underlying;
  }

  if (target_var_ty.ty != TD_VAR_TY_TY_FUNC) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(FN_NOT_CALLABLE, fn_not_callable, call->span,
                               MK_INVALID_TEXT_POS(0), "can't call non func"));

    return (struct td_expr){
        .ty = TD_EXPR_TY_CALL, .var_ty = TD_VAR_TY_UNKNOWN, .call = td_call};
  }

  size_t num_params = target_var_ty.func.num_params;
  if (target_var_ty.func.ty != TD_TY_FUNC_TY_UNKNOWN_ARGS &&
      call->arg_list.num_args < num_params) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(BAD_PARAM_COUNT, bad_param_count, call->span,
                               MK_INVALID_TEXT_POS(0), "too few params"));
  } else if (target_var_ty.func.ty == TD_TY_FUNC_TY_KNOWN_ARGS &&
             call->arg_list.num_args > num_params) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(BAD_PARAM_COUNT, bad_param_count, call->span,
                               MK_INVALID_TEXT_POS(0), "too many params"));
  }

  for (size_t i = 0; i < td_call.arg_list.num_args; i++) {
    struct td_var_ty param_ty;

    if (i < num_params) {
      param_ty = target_var_ty.func.params[i].var_ty;
    } else {
      param_ty = get_target_for_variadic(&td_call.arg_list.args[i].var_ty);
    }

    if (param_ty.ty != TD_VAR_TY_TY_ARRAY) {
      td_call.arg_list.args[i] =
          add_cast_if_needed(tchk, td_call.arg_list.args[i].span,
                             td_call.arg_list.args[i], param_ty);
    }
  }

  // FIXME: need a hashtbl or similar for good attr lookup
  const struct td_attr_format *format = target_var_ty.attrs.format;
  if (format) {
    switch (format->archetype) {
    case TD_ATTR_FORMAT_ARCHETYPE_PRINTF: {
      DEBUG_ASSERT(format->string_index, "string index should never be zero");

      if (format->string_index > td_call.arg_list.num_args) {
        break;
      }

      struct td_expr *format_str =
          &td_call.arg_list.args[format->string_index - 1];

      while (format_str->ty == TD_EXPR_TY_UNARY_OP &&
             format_str->unary_op.ty == TD_UNARY_OP_TY_CAST) {
        format_str = format_str->unary_op.expr;
      }

      // TODO: add warning opt for this
      bool warn_printf_non_literal = false;

      if (format_str->ty == TD_EXPR_TY_CNST &&
          format_str->cnst.ty == TD_CNST_TY_STRING) {
        if (format_str->cnst.str_value.ty == TD_CNST_STR_TY_WIDE) {
          if (warn_printf_non_literal) {
            BUG("diag for wide str as printf arg");
          }

          break;
        }
      } else {
        if (warn_printf_non_literal) {
          BUG("diag for non str as printf arg");
        }

        break;
      }

      struct td_ascii_str str = format_str->cnst.str_value.ascii;
      struct printf_args args = parse_printf_format(tchk, str);

      bool arg_diag = false;
      size_t arg_idx = format->first_to_check - 1;
      for (size_t i = 0; i < args.num_argspecs; i++) {
        const struct printf_argspec *arg_spec = &args.argspecs[i];

        struct text_span span = {
            .start = format_str->span.start,
            .end = format_str->span.end,
        };

        // get span in the thing
        // HACK: just add one to skip quote (won't work with wide strings, but the rest wont work either)

        span.start.col += arg_spec->offset + 1;
        span.start.idx += arg_spec->offset + 1;
        span.end.col = span.start.col + arg_spec->len;
        span.end.idx = span.start.idx + arg_spec->len;

        // we can do more validation, currently very basic

        struct td_var_ty exp_ty = TD_VAR_TY_UNKNOWN;
        size_t num_spec_args = 0;

        switch (arg_spec->specifier) {
        case PRINTF_ARGSPEC_SPECIFIER_INVALID:
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(BAD_PRINTF_SPECIFIER, bad_printf_specifier,
                                     span, span.end,
                                     "invalid printf specifier"));

          continue;
        case PRINTF_ARGSPEC_SPECIFIER_LITERAL_PERCENT:
          continue;

        // TODO: proper validation of combinations and flags
        case PRINTF_ARGSPEC_SPECIFIER_CHAR:
        case PRINTF_ARGSPEC_SPECIFIER_SIGNED_INT:
          exp_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
          num_spec_args = 1;
          break;
        case PRINTF_ARGSPEC_SPECIFIER_STRING:
          exp_ty = TD_VAR_TY_CHAR_POINTER;
          num_spec_args = 1;
          break;
        case PRINTF_ARGSPEC_SPECIFIER_OCTAL_UNSIGNED_INT:
        case PRINTF_ARGSPEC_SPECIFIER_HEX_UNSIGNED_INT:
        case PRINTF_ARGSPEC_SPECIFIER_UNSIGNED_INT:
          exp_ty = TD_VAR_TY_WELL_KNOWN_UNSIGNED_INT;
          num_spec_args = 1;
          break;
        case PRINTF_ARGSPEC_SPECIFIER_FLOAT:
        case PRINTF_ARGSPEC_SPECIFIER_EXP_FLOAT:
        case PRINTF_ARGSPEC_SPECIFIER_HEX_EXP_FLOAT:
        case PRINTF_ARGSPEC_SPECIFIER_VAR_FLOAT:
          exp_ty = TD_VAR_TY_WELL_KNOWN_UNSIGNED_INT;
          num_spec_args = 1;
          break;
        case PRINTF_ARGSPEC_SPECIFIER_NUM_WRITTEN:
          // exp_ty = TD_VAR_TY_WELL_KNOWN_UNSIGNED_INT;
          num_spec_args = 1;
          break;
        case PRINTF_ARGSPEC_SPECIFIER_POINTER:
          exp_ty = TD_VAR_TY_VOID_POINTER;
          num_spec_args = 1;
          break;
        }

        if (format->first_to_check) {
          if (arg_idx + num_spec_args > td_call.arg_list.num_args &&
              !arg_diag) {
            arg_diag = true;

            compiler_diagnostics_add(
                tchk->diagnostics,
                MK_SEMANTIC_DIAGNOSTIC(BAD_PRINTF_ARGS, bad_printf_args,
                                       call->arg_list.span,
                                       MK_INVALID_TEXT_POS(0),
                                       "printf format string specified more "
                                       "arguments than provided"));
          } else {
            const struct td_var_ty *arg_ty =
                &td_call.arg_list.args[arg_idx].var_ty;
            if (!td_var_ty_compatible(
                    tchk, arg_ty, &exp_ty,
                    TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_ALL)) {

              compiler_diagnostics_add(
                  tchk->diagnostics,
                  MK_SEMANTIC_DIAGNOSTIC(
                      BAD_PRINTF_ARGS, bad_printf_args, span, span.end,
                      arena_alloc_snprintf(tchk->arena,
                                           "printf specifier expects type '%s' "
                                           "but type '%s' was provided",
                                           tchk_type_name(tchk, &exp_ty),
                                           tchk_type_name(tchk, arg_ty))));
            }

            arg_idx += num_spec_args;
          }
        }
      }

      break;
    }
    }
  }

  struct td_var_ty var_ty = *target_var_ty.func.ret;

  return (struct td_expr){
      .ty = TD_EXPR_TY_CALL, .var_ty = var_ty, .call = td_call};
}

static bool td_expr_is_lvalue(UNUSED struct typechk *tchk,
                              const struct td_expr *expr) {
  switch (expr->ty) {
  case TD_EXPR_TY_UNARY_OP:
    return expr->unary_op.ty == TD_UNARY_OP_TY_INDIRECTION;
  case TD_EXPR_TY_ARRAYACCESS:
  case TD_EXPR_TY_INVALID:
  case TD_EXPR_TY_MEMBERACCESS:
  case TD_EXPR_TY_POINTERACCESS:
  case TD_EXPR_TY_ASSG:
  case TD_EXPR_TY_COMPOUNDEXPR:
  case TD_EXPR_TY_VAR:
    return true;
  case TD_EXPR_TY_CALL:
  case TD_EXPR_TY_TERNARY:
  case TD_EXPR_TY_BINARY_OP:
  case TD_EXPR_TY_CNST:
  case TD_EXPR_TY_SIZEOF:
  case TD_EXPR_TY_ALIGNOF:
  case TD_EXPR_TY_COMPOUND_LITERAL:
    return false;
  }
}

static struct td_expr type_unary_op(struct typechk *tchk,
                                    const struct ast_unary_op *unary_op) {
  struct td_var_ty result_ty;

  struct td_unary_op td_unary_op = {
      .expr = arena_alloc(tchk->arena, sizeof(*td_unary_op.expr)),
  };

  enum type_expr_flags flags;
  switch (unary_op->ty) {
  case AST_UNARY_OP_TY_ADDRESSOF:
    flags = TYPE_EXPR_FLAGS_ARRAYS_DONT_DECAY;
    break;
  default:
    flags = TYPE_EXPR_FLAGS_NONE;
    break;
  }

  struct td_expr expr = type_expr(tchk, flags, unary_op->expr);

  switch (unary_op->ty) {
  case AST_UNARY_OP_TY_PLUS:
    td_unary_op.ty = TD_UNARY_OP_TY_PLUS;
    goto promotion;
  case AST_UNARY_OP_TY_NOT:
    td_unary_op.ty = TD_UNARY_OP_TY_NOT;
    goto promotion;
  case AST_UNARY_OP_TY_MINUS:
    td_unary_op.ty = TD_UNARY_OP_TY_MINUS;
    goto promotion;

  promotion:
    // these undergo integer promotion
    expr = perform_integer_promotion(tchk, expr);
    result_ty = expr.var_ty;
    break;
  case AST_UNARY_OP_TY_PREFIX_INC:
    td_unary_op.ty = TD_UNARY_OP_TY_PREFIX_INC;
    goto inc_dec;
  case AST_UNARY_OP_TY_PREFIX_DEC:
    td_unary_op.ty = TD_UNARY_OP_TY_PREFIX_DEC;
    goto inc_dec;
  case AST_UNARY_OP_TY_POSTFIX_INC:
    td_unary_op.ty = TD_UNARY_OP_TY_POSTFIX_INC;
    goto inc_dec;
  case AST_UNARY_OP_TY_POSTFIX_DEC:
    td_unary_op.ty = TD_UNARY_OP_TY_POSTFIX_DEC;
    goto inc_dec;

  inc_dec:
    if (!td_expr_is_lvalue(tchk, &expr)) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              NOT_ASSIGNABLE, not_assignable, unary_op->expr->span,
              MK_INVALID_TEXT_POS(0),
              "not an assignable expression; cannot use ++/--"));

      return (struct td_expr){.ty = TD_EXPR_TY_INVALID,
                              .var_ty = TD_VAR_TY_UNKNOWN};
    }

    result_ty = expr.var_ty;
    break;
  case AST_UNARY_OP_TY_LOGICAL_NOT:
    td_unary_op.ty = TD_UNARY_OP_TY_LOGICAL_NOT;
    result_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
    break;
  case AST_UNARY_OP_TY_INDIRECTION:
    td_unary_op.ty = TD_UNARY_OP_TY_INDIRECTION;
    switch (expr.var_ty.ty) {
    case TD_VAR_TY_TY_POINTER:
      result_ty = *expr.var_ty.pointer.underlying;
      break;
    case TD_VAR_TY_TY_ARRAY:
      result_ty = *expr.var_ty.array.underlying;
      break;
    default:
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(BAD_DEREF, bad_deref, unary_op->expr->span,
                                 MK_INVALID_TEXT_POS(0),
                                 "cannot dereference a non pointer/array"));

      result_ty = TD_VAR_TY_UNKNOWN;
    }

    break;
  case AST_UNARY_OP_TY_ADDRESSOF:
    td_unary_op.ty = TD_UNARY_OP_TY_ADDRESSOF;
    result_ty =
        td_var_ty_make_pointer(tchk, &expr.var_ty, TD_TYPE_QUALIFIER_FLAG_NONE);
    break;
  case AST_UNARY_OP_TY_CAST: {
    td_unary_op.ty = TD_UNARY_OP_TY_CAST;

    struct td_var_ty target_ty =
        type_type_name(tchk, &unary_op->cast.type_name);

    switch (is_cast_needed(tchk, &expr.var_ty, &target_ty)) {
    case CAST_TY_NONE:
      return expr;
    case CAST_TY_CAST:
      return add_cast_expr(tchk, expr, target_ty);
    case CAST_TY_ERR:
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              CAST, cast, expr.span, MK_INVALID_TEXT_POS(0),
              arena_alloc_snprintf(tchk->arena,
                                   "cast is not legal (from '%s' to '%s')",
                                   tchk_type_name(tchk, &expr.var_ty),
                                   tchk_type_name(tchk, &target_ty))));

      return expr;
    }
  }
  }

  *td_unary_op.expr = expr;

  return (struct td_expr){
      .ty = TD_EXPR_TY_UNARY_OP, .var_ty = result_ty, .unary_op = td_unary_op};
}

static struct td_expr type_binary_op(struct typechk *tchk,
                                     const struct ast_binary_op *binary_op) {
  // all binary operations perform integer promotion
  struct td_expr lhs = perform_integer_promotion(
      tchk, type_expr(tchk, TYPE_EXPR_FLAGS_NONE, binary_op->lhs));
  struct td_expr rhs = perform_integer_promotion(
      tchk, type_expr(tchk, TYPE_EXPR_FLAGS_NONE, binary_op->rhs));

  enum td_binary_op_ty ty;
  switch (binary_op->ty) {
  case AST_BINARY_OP_TY_EQ:
    ty = TD_BINARY_OP_TY_EQ;
    break;
  case AST_BINARY_OP_TY_NEQ:
    ty = TD_BINARY_OP_TY_NEQ;
    break;
  case AST_BINARY_OP_TY_GT:
    ty = TD_BINARY_OP_TY_GT;
    break;
  case AST_BINARY_OP_TY_GTEQ:
    ty = TD_BINARY_OP_TY_GTEQ;
    break;
  case AST_BINARY_OP_TY_LT:
    ty = TD_BINARY_OP_TY_LT;
    break;
  case AST_BINARY_OP_TY_LTEQ:
    ty = TD_BINARY_OP_TY_LTEQ;
    break;
  case AST_BINARY_OP_TY_LOGICAL_OR:
    ty = TD_BINARY_OP_TY_LOGICAL_OR;
    break;
  case AST_BINARY_OP_TY_LOGICAL_AND:
    ty = TD_BINARY_OP_TY_LOGICAL_AND;
    break;
  case AST_BINARY_OP_TY_OR:
    ty = TD_BINARY_OP_TY_OR;
    break;
  case AST_BINARY_OP_TY_AND:
    ty = TD_BINARY_OP_TY_AND;
    break;
  case AST_BINARY_OP_TY_XOR:
    ty = TD_BINARY_OP_TY_XOR;
    break;
  case AST_BINARY_OP_TY_LSHIFT:
    ty = TD_BINARY_OP_TY_LSHIFT;
    break;
  case AST_BINARY_OP_TY_RSHIFT:
    ty = TD_BINARY_OP_TY_RSHIFT;
    break;
  case AST_BINARY_OP_TY_ADD:
    ty = TD_BINARY_OP_TY_ADD;
    break;
  case AST_BINARY_OP_TY_SUB:
    ty = TD_BINARY_OP_TY_SUB;
    break;
  case AST_BINARY_OP_TY_MUL:
    ty = TD_BINARY_OP_TY_MUL;
    break;
  case AST_BINARY_OP_TY_DIV:
    ty = TD_BINARY_OP_TY_DIV;
    break;
  case AST_BINARY_OP_TY_MOD:
    ty = TD_BINARY_OP_TY_MOD;
    break;
  }

  struct td_binary_op td_binary_op = {
      .ty = ty,
      .lhs = arena_alloc(tchk->arena, sizeof(*td_binary_op.lhs)),
      .rhs = arena_alloc(tchk->arena, sizeof(*td_binary_op.rhs))};

  *td_binary_op.lhs = lhs;
  *td_binary_op.rhs = rhs;

  struct td_binary_op_tys op_tys =
      resolve_binary_op_types(tchk, td_binary_op.lhs, td_binary_op.rhs,
                              td_binary_op.ty, binary_op->span);

  *td_binary_op.lhs = add_cast_if_needed(tchk, binary_op->span,
                                         *td_binary_op.lhs, op_tys.lhs_op_ty);
  *td_binary_op.rhs = add_cast_if_needed(tchk, binary_op->span,
                                         *td_binary_op.rhs, op_tys.rhs_op_ty);

  return (struct td_expr){.ty = TD_EXPR_TY_BINARY_OP,
                          .var_ty = op_tys.result_ty,
                          .binary_op = td_binary_op};
}

static struct td_expr
type_arrayaccess(struct typechk *tchk,
                 const struct ast_arrayaccess *arrayaccess) {
  struct td_arrayaccess td_arrayaccess = {
      .lhs = arena_alloc(tchk->arena, sizeof(*td_arrayaccess.lhs)),
      .rhs = arena_alloc(tchk->arena, sizeof(*td_arrayaccess.rhs)),
  };

  struct td_expr lhs = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, arrayaccess->lhs);
  struct td_expr rhs = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, arrayaccess->rhs);

  struct td_var_ty var_ty;

  if (lhs.var_ty.ty == TD_VAR_TY_TY_POINTER ||
      lhs.var_ty.ty == TD_VAR_TY_TY_ARRAY) {
    *td_arrayaccess.lhs = lhs;
    *td_arrayaccess.rhs = rhs;

    var_ty = td_var_ty_get_underlying(tchk, &td_arrayaccess.lhs->var_ty);
  } else if (rhs.var_ty.ty == TD_VAR_TY_TY_POINTER ||
             rhs.var_ty.ty == TD_VAR_TY_TY_ARRAY) {
    *td_arrayaccess.lhs = rhs;
    *td_arrayaccess.rhs = lhs;

    var_ty = td_var_ty_get_underlying(tchk, &td_arrayaccess.lhs->var_ty);
  } else {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            BAD_DEREF, bad_deref, arrayaccess->span, MK_INVALID_TEXT_POS(0),
            "array access should have at least one pointer type"));

    var_ty = TD_VAR_TY_UNKNOWN;
  }

  struct td_var_ty complete;
  if (var_ty.ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    if (!try_get_completed_aggregate(tchk, &var_ty, &complete)) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(BAD_DEREF, bad_deref, arrayaccess->span,
                                 MK_INVALID_TEXT_POS(0),
                                 "array access had incomplete type"));
    }

    var_ty = complete;

    switch (lhs.var_ty.ty) {
    case TD_VAR_TY_TY_POINTER:
      *lhs.var_ty.pointer.underlying = var_ty;
      break;
    case TD_VAR_TY_TY_ARRAY:
      *lhs.var_ty.array.underlying = var_ty;
      break;
    default:
      unreachable();
    }
  }

  if (!td_var_ty_is_integral_ty(&td_arrayaccess.rhs->var_ty)) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            POINTER_TYPES, pointer_types, arrayaccess->span,
            MK_INVALID_TEXT_POS(0),
            "array access should have at least one integral type"));

    *td_arrayaccess.rhs = *td_arrayaccess.rhs;
  } else {
    struct td_var_ty pointer_ty = td_var_ty_pointer_sized_int(tchk, false);

    *td_arrayaccess.rhs = add_cast_if_needed(tchk, arrayaccess->span,
                                             *td_arrayaccess.rhs, pointer_ty);
  }

  return (struct td_expr){.ty = TD_EXPR_TY_ARRAYACCESS,
                          .var_ty = var_ty,
                          .array_access = td_arrayaccess};
}

static bool try_get_completed_aggregate(struct typechk *tchk,
                                        const struct td_var_ty *var_ty,
                                        struct td_var_ty *complete) {
  if (var_ty->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    enum var_table_ns ns =
        var_ty->incomplete_aggregate.ty == TD_TY_AGGREGATE_TY_STRUCT
            ? VAR_TABLE_NS_STRUCT
            : VAR_TABLE_NS_UNION;
    struct var_table_entry *entry =
        vt_get_entry(&tchk->ty_table, ns, var_ty->incomplete_aggregate.name);

    // FIXME: ALSO needs to check scope
    // if (!entry || entry->var->scope != td_var_ty)
    if (!entry) {
      *complete = *var_ty;
      return false;
    }

    DEBUG_ASSERT(entry->var_ty->ty == TD_VAR_TY_TY_AGGREGATE, "non aggregate");
    *complete = *entry->var_ty;
    return true;
  } else {
    DEBUG_ASSERT(var_ty->ty == TD_VAR_TY_TY_AGGREGATE, "non aggregate");
    *complete = *var_ty;
    return true;
  }
}

static struct td_var_ty get_completed_aggregate(struct typechk *tchk,
                                                const struct td_var_ty *var_ty,
                                                struct text_span context) {
  struct td_var_ty complete;
  if (!try_get_completed_aggregate(tchk, var_ty, &complete)) {
    // TODO: need to allow extern / tentative decls here

    char *msg = arena_alloc_snprintf(
        tchk->arena, "incomplete type '%.*s' cannot be used here",
        (int)var_ty->incomplete_aggregate.name.len,
        var_ty->incomplete_aggregate.name.str);

    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(INCOMPLETE_TYPE, incomplete_type, context,
                               MK_INVALID_TEXT_POS(0), msg));
    return TD_VAR_TY_UNKNOWN;
  }

  return complete;
}

static bool try_resolve_member_access_ty(struct typechk *tchk,
                                         const struct td_var_ty *var_ty,
                                         struct sized_str member_name,
                                         struct td_var_ty *member_var_ty) {
  if (var_ty->ty == TD_VAR_TY_TY_UNKNOWN) {
    *member_var_ty = TD_VAR_TY_UNKNOWN;
    return true;
  }

  DEBUG_ASSERT(var_ty->ty == TD_VAR_TY_TY_AGGREGATE, "non aggregate");

  // FIXME: super slow hashtable needed
  for (size_t i = 0; i < var_ty->aggregate.num_fields; i++) {
    const struct td_struct_field *field = &var_ty->aggregate.fields[i];
    if (field->identifier.str == NULL) {
      if (try_resolve_member_access_ty(tchk, &field->var_ty, member_name,
                                       member_var_ty)) {
        return true;
      }
    } else if (szstreq(field->identifier, member_name)) {
      *member_var_ty = field->var_ty;
      return true;
    }
  }

  return false;
}

static bool try_get_member_idx(struct typechk *tchk,
                               const struct td_var_ty *td_var_ty,
                               struct sized_str member_name, size_t *member_idx,
                               struct text_span context) {

  const struct td_var_ty var_ty =
      get_completed_aggregate(tchk, td_var_ty, context);

  // FIXME: super slow hashtable needed
  for (size_t i = 0; i < var_ty.aggregate.num_fields; i++) {
    const struct td_struct_field *field = &var_ty.aggregate.fields[i];
    if (field->identifier.str == NULL) {
      if (try_get_member_idx(tchk, &field->var_ty, member_name, member_idx,
                             context)) {
        return true;
      }
    } else if (szstreq(field->identifier, member_name)) {
      *member_idx = i;
      return true;
    }
  }

  return false;
}

static bool try_resolve_member_access_ty_by_index(
    struct typechk *tchk, const struct td_var_ty *td_var_ty, size_t member_idx,
    struct td_var_ty *member_var_ty, struct text_span context) {

  if (td_var_ty->ty == TD_VAR_TY_TY_ARRAY) {
    if (member_idx >= td_var_ty->array.size) {
      return false;
    }

    *member_var_ty = td_var_ty_get_underlying(tchk, td_var_ty);
    return true;
  }

  const struct td_var_ty var_ty =
      get_completed_aggregate(tchk, td_var_ty, context);

  if (member_idx >= var_ty.aggregate.num_fields) {
    return false;
  }

  *member_var_ty = var_ty.aggregate.fields[member_idx].var_ty;
  return true;
}

static struct td_var_ty type_incomplete_var_ty(struct typechk *tchk,
                                               const struct td_var_ty *var_ty,
                                               struct text_span context) {
  if (var_ty->ty != TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    return *var_ty;
  }

  return get_completed_aggregate(tchk, var_ty, context);
}

static struct td_expr
type_memberaccess(struct typechk *tchk, enum type_expr_flags flags,
                  const struct ast_memberaccess *memberaccess) {
  struct td_memberaccess td_memberaccess = {
      .lhs = arena_alloc(tchk->arena, sizeof(*td_memberaccess.lhs)),
      .member = identifier_str(tchk->parser, &memberaccess->member)};

  *td_memberaccess.lhs =
      type_expr(tchk, TYPE_EXPR_FLAGS_NONE, memberaccess->lhs);

  td_memberaccess.lhs->var_ty = type_incomplete_var_ty(
      tchk, &td_memberaccess.lhs->var_ty, memberaccess->span);

  struct sized_str member_name =
      identifier_str(tchk->parser, &memberaccess->member);

  struct td_var_ty var_ty;
  if (!try_resolve_member_access_ty(tchk, &td_memberaccess.lhs->var_ty,
                                    member_name, &var_ty)) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(NO_MEMBER, no_member, memberaccess->span,
                               MK_INVALID_TEXT_POS(0), "unknown member"));
    var_ty = TD_VAR_TY_UNKNOWN;
  }

  struct td_expr expr = {.ty = TD_EXPR_TY_MEMBERACCESS,
                         .var_ty = var_ty,
                         .member_access = td_memberaccess};

  if (var_ty.ty == TD_VAR_TY_TY_ARRAY &&
      !(flags & TYPE_EXPR_FLAGS_ARRAYS_DONT_DECAY)) {
    // array member access
    // decay this to addressof
    struct td_unary_op addr = {
        .ty = TD_UNARY_OP_TY_ADDRESSOF,
        .expr = arena_alloc(tchk->arena, sizeof(*addr.expr))};

    *addr.expr = expr;

    return (struct td_expr){
        .ty = TD_EXPR_TY_UNARY_OP,
        .var_ty = td_var_ty_make_pointer(tchk, var_ty.array.underlying,
                                         var_ty.type_qualifiers),
        .unary_op = addr};
  }

  return expr;
}

static struct td_expr
type_pointeraccess(struct typechk *tchk, enum type_expr_flags flags,
                   const struct ast_pointeraccess *pointeraccess) {

  struct td_pointeraccess td_pointeraccess = {
      .lhs = arena_alloc(tchk->arena, sizeof(*td_pointeraccess.lhs)),
      .member = identifier_str(tchk->parser, &pointeraccess->member)};

  *td_pointeraccess.lhs =
      type_expr(tchk, TYPE_EXPR_FLAGS_NONE, pointeraccess->lhs);

  switch (td_pointeraccess.lhs->var_ty.ty) {
  case TD_VAR_TY_TY_POINTER:
    *td_pointeraccess.lhs->var_ty.pointer.underlying = type_incomplete_var_ty(
        tchk, td_pointeraccess.lhs->var_ty.pointer.underlying,
        pointeraccess->span);
    break;
  case TD_VAR_TY_TY_ARRAY:
    *td_pointeraccess.lhs->var_ty.array.underlying = type_incomplete_var_ty(
        tchk, td_pointeraccess.lhs->var_ty.array.underlying,
        pointeraccess->span);
    break;
  default:
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(BAD_DEREF, bad_deref, pointeraccess->span,
                               MK_INVALID_TEXT_POS(0),
                               "type doesn't make sense for pointer access"));
    td_pointeraccess.lhs->var_ty = TD_VAR_TY_UNKNOWN;
  }

  struct sized_str pointer_name =
      identifier_str(tchk->parser, &pointeraccess->member);

  struct td_var_ty underlying =
      td_var_ty_get_underlying(tchk, &td_pointeraccess.lhs->var_ty);

  struct td_var_ty var_ty;
  if (!try_resolve_member_access_ty(tchk, &underlying, pointer_name, &var_ty)) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(NO_MEMBER, no_member, pointeraccess->span,
                               MK_INVALID_TEXT_POS(0), "unknown member"));
    var_ty = TD_VAR_TY_UNKNOWN;
  }

  struct td_expr expr = {.ty = TD_EXPR_TY_POINTERACCESS,
                         .var_ty = var_ty,
                         .pointer_access = td_pointeraccess};

  if (var_ty.ty == TD_VAR_TY_TY_ARRAY &&
      !(flags & TYPE_EXPR_FLAGS_ARRAYS_DONT_DECAY)) {
    // array member access
    // decay this to addressof
    struct td_unary_op addr = {
        .ty = TD_UNARY_OP_TY_ADDRESSOF,
        .expr = arena_alloc(tchk->arena, sizeof(*addr.expr))};

    *addr.expr = expr;

    return (struct td_expr){
        .ty = TD_EXPR_TY_UNARY_OP,
        .var_ty = td_var_ty_make_pointer(tchk, var_ty.array.underlying,
                                         var_ty.type_qualifiers),
        .unary_op = addr};
  }

  return expr;
}

static struct td_expr type_assg(struct typechk *tchk,
                                const struct ast_assg *assg) {
  struct td_assg td_assg = {
      .assignee = arena_alloc(tchk->arena, sizeof(*td_assg.assignee)),
      .expr = arena_alloc(tchk->arena, sizeof(*td_assg.expr)),
  };

  enum td_binary_op_ty bin_op;

  switch (assg->ty) {
  case AST_ASSG_TY_BASIC:
    td_assg.ty = TD_ASSG_TY_BASIC;
    break;
  case AST_ASSG_TY_ADD:
    td_assg.ty = TD_ASSG_TY_ADD;
    bin_op = TD_BINARY_OP_TY_ADD;
    break;
  case AST_ASSG_TY_SUB:
    td_assg.ty = TD_ASSG_TY_SUB;
    bin_op = TD_BINARY_OP_TY_SUB;
    break;
  case AST_ASSG_TY_MUL:
    td_assg.ty = TD_ASSG_TY_MUL;
    bin_op = TD_BINARY_OP_TY_MUL;
    break;
  case AST_ASSG_TY_DIV:
    td_assg.ty = TD_ASSG_TY_DIV;
    bin_op = TD_BINARY_OP_TY_DIV;
    break;
  case AST_ASSG_TY_MOD:
    td_assg.ty = TD_ASSG_TY_MOD;
    bin_op = TD_BINARY_OP_TY_MOD;
    break;
  case AST_ASSG_TY_AND:
    td_assg.ty = TD_ASSG_TY_AND;
    bin_op = TD_BINARY_OP_TY_AND;
    break;
  case AST_ASSG_TY_OR:
    td_assg.ty = TD_ASSG_TY_OR;
    bin_op = TD_BINARY_OP_TY_OR;
    break;
  case AST_ASSG_TY_XOR:
    td_assg.ty = TD_ASSG_TY_XOR;
    bin_op = TD_BINARY_OP_TY_XOR;
    break;
  case AST_ASSG_TY_LSHIFT:
    td_assg.ty = TD_ASSG_TY_LSHIFT;
    bin_op = TD_BINARY_OP_TY_LSHIFT;
    break;
  case AST_ASSG_TY_RSHIFT:
    td_assg.ty = TD_ASSG_TY_RSHIFT;
    bin_op = TD_BINARY_OP_TY_RSHIFT;
    break;
  }

  *td_assg.expr = perform_integer_promotion(
      tchk, type_expr(tchk, TYPE_EXPR_FLAGS_NONE, assg->expr));
  *td_assg.assignee = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, assg->assignee);

  if (!td_expr_is_lvalue(tchk, td_assg.assignee)) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            NOT_ASSIGNABLE, not_assignable, assg->assignee->span,
            MK_INVALID_TEXT_POS(0),
            "not an assignable expression; cannot use ++/--"));

    return (struct td_expr){.ty = TD_EXPR_TY_INVALID,
                            .var_ty = TD_VAR_TY_UNKNOWN};
  }

  if (td_assg.ty != TD_ASSG_TY_BASIC) {
    struct td_expr promoted_assignee =
        perform_integer_promotion(tchk, *td_assg.assignee);
    struct td_binary_op_tys tys = resolve_binary_op_types(
        tchk, &promoted_assignee, td_assg.expr, bin_op, assg->span);

    *td_assg.expr =
        add_cast_if_needed(tchk, assg->span, *td_assg.expr, tys.rhs_op_ty);

    td_assg.assignee_var_ty = tys.lhs_op_ty;
    td_assg.result_var_ty = tys.result_ty;
    td_assg.cast_assignee =
        is_cast_needed(tchk, &td_assg.assignee->var_ty, &tys.lhs_op_ty);
    td_assg.cast_result =
        is_cast_needed(tchk, &tys.result_ty, &td_assg.assignee->var_ty);
  } else {
    *td_assg.expr = add_cast_if_needed(tchk, assg->span, *td_assg.expr,
                                       td_assg.assignee->var_ty);

    td_assg.assignee_var_ty = td_assg.assignee->var_ty;
    td_assg.result_var_ty = td_assg.expr->var_ty;
    td_assg.cast_assignee = false;
    td_assg.cast_result = false;
  }

  struct td_var_ty var_ty = td_assg.assignee->var_ty;

  return (struct td_expr){
      .ty = TD_EXPR_TY_ASSG, .var_ty = var_ty, .assg = td_assg};
}

static struct td_expr type_var(struct typechk *tchk,
                               const struct ast_var *var) {
  struct sized_str name = identifier_str(tchk->parser, &var->identifier);

  // turn __FUNCTION__ into __func__
  if (szstreq(name, MK_SIZED("__FUNCTION__"))) {
    name = MK_SIZED("__func__");
  }

  struct var_table_entry *entry =
      vt_get_entry(&tchk->var_table, VAR_TABLE_NS_NONE, name);

  struct td_var_ty var_ty;
  struct td_var td_var;
  if (entry) {
    var_ty = *entry->var_ty;
    td_var = *entry->var;
  } else {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(tchk->diagnostics,
                             MK_SEMANTIC_DIAGNOSTIC(NO_VAR, no_var, var->span,
                                                    MK_INVALID_TEXT_POS(0),
                                                    "variable does not exist"));

    var_ty = TD_VAR_TY_UNKNOWN;
    td_var = (struct td_var){.ty = TD_VAR_VAR_TY_VAR,
                             .identifier = name,
                             .scope = vt_cur_scope(&tchk->ty_table)};
  }

  return (struct td_expr){
      .ty = TD_EXPR_TY_VAR, .var_ty = var_ty, .var = td_var};
}

static struct td_expr type_cnst(UNUSED_ARG(struct typechk *tchk),
                                const struct ast_cnst *cnst) {
  struct td_cnst td_cnst;
  struct td_var_ty var_ty;

#define CNST_TY_ENTRY(name, jump)                                              \
  case AST_CNST_TY_##name:                                                     \
    var_ty = TD_VAR_TY_WELL_KNOWN_##name;                                      \
    jump

  switch (cnst->ty) {
    CNST_TY_ENTRY(SIGNED_INT, goto integral);
    CNST_TY_ENTRY(UNSIGNED_INT, goto integral);
    CNST_TY_ENTRY(SIGNED_LONG, goto integral);
    CNST_TY_ENTRY(UNSIGNED_LONG, goto integral);
    CNST_TY_ENTRY(SIGNED_LONG_LONG, goto integral);
    CNST_TY_ENTRY(UNSIGNED_LONG_LONG, goto integral);
    CNST_TY_ENTRY(CHAR, goto integral);

  case AST_CNST_TY_WIDE_CHAR:
    var_ty = TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
    goto integral;

  integral:
    td_cnst.ty = TD_CNST_TY_NUM;
    td_cnst.num_value = cnst->num_value;
    break;

    CNST_TY_ENTRY(FLOAT, goto floating_point);
    CNST_TY_ENTRY(DOUBLE, goto floating_point);
    CNST_TY_ENTRY(LONG_DOUBLE, goto floating_point);

  floating_point:
    td_cnst.ty = TD_CNST_TY_NUM;
    td_cnst.num_value = cnst->num_value;
    break;

  case AST_CNST_TY_STR_LITERAL:
    // FIXME: we have duplication, both the cnst ty and the enum ty encode
    // ascii vs wide
    DEBUG_ASSERT(cnst->str_value.ty == AST_CNST_STR_TY_ASCII, "expected ascii");

    // FIXME: lifetimes
    td_cnst.ty = TD_CNST_TY_STRING;
    td_cnst.str_value =
        (struct td_cnst_str){.ty = TD_CNST_STR_TY_ASCII,
                             .ascii = {
                                 .value = cnst->str_value.ascii.value,
                                 .len = cnst->str_value.ascii.len,
                             }};
    var_ty = TD_VAR_TY_CONST_CHAR_POINTER;
    break;
  case AST_CNST_TY_WIDE_STR_LITERAL:
    // FIXME: we have duplication, both the cnst ty and the enum ty encode
    // ascii vs wide
    DEBUG_ASSERT(cnst->str_value.ty == AST_CNST_STR_TY_WIDE, "expected wide");

    td_cnst.ty = TD_CNST_TY_STRING;
    td_cnst.str_value =
        (struct td_cnst_str){.ty = TD_CNST_STR_TY_WIDE,
                             .wide = {
                                 .value = cnst->str_value.wide.value,
                                 .len = cnst->str_value.wide.len,
                             }};
    var_ty = (struct td_var_ty){
        .ty = TD_VAR_TY_TY_POINTER,
        .type_qualifiers = TD_TYPE_QUALIFIER_FLAG_CONST,
        .pointer = {.underlying = &TD_VAR_TY_WELL_KNOWN_SIGNED_INT}};
    break;
  }

#undef CNST_TY_ENTRY

  return (struct td_expr){
      .ty = TD_EXPR_TY_CNST, .var_ty = var_ty, .cnst = td_cnst};
}

static struct td_var_ty
type_type_or_expr(struct typechk *tchk,
                  const struct ast_type_or_expr *type_or_expr) {
  switch (type_or_expr->ty) {
  case AST_TYPE_OR_EXPR_TY_TYPE:
    return type_type_name(tchk, type_or_expr->type_name);
  case AST_TYPE_OR_EXPR_TY_EXPR:
    return type_expr(tchk, TYPE_EXPR_FLAGS_ARRAYS_DONT_DECAY,
                     type_or_expr->expr)
        .var_ty;
  }
}

static struct td_expr type_sizeof(struct typechk *tchk,
                                  const struct ast_sizeof *size_of) {
  // TODO: here and alignof, generate diagnostic for incomplete type

  struct td_var_ty var_ty = type_type_or_expr(tchk, &size_of->type_or_expr);

  return (struct td_expr){.ty = TD_EXPR_TY_SIZEOF,
                          .var_ty = td_var_ty_pointer_sized_int(tchk, false),
                          .size_of = {.var_ty = var_ty}};
}

static struct td_expr type_alignof(struct typechk *tchk,
                                   const struct ast_alignof *align_of) {
  struct td_alignof td_align_of = {
      .var_ty = type_type_name(tchk, &align_of->type_name)};

  return (struct td_expr){.ty = TD_EXPR_TY_ALIGNOF,
                          .var_ty = td_var_ty_pointer_sized_int(tchk, false),
                          .align_of = td_align_of};
}

static struct td_expr
type_compoundexpr(struct typechk *tchk, enum type_expr_flags flags,
                  const struct ast_compoundexpr *compoundexpr) {
  struct td_compoundexpr td_compoundexpr = {
      .num_exprs = compoundexpr->num_exprs,
      .exprs = arena_alloc(tchk->arena, sizeof(*td_compoundexpr.exprs) *
                                            compoundexpr->num_exprs)};

  for (size_t i = 0; i < compoundexpr->num_exprs; i++) {
    td_compoundexpr.exprs[i] = type_expr(tchk, flags, &compoundexpr->exprs[i]);
  }

  DEBUG_ASSERT(td_compoundexpr.num_exprs,
               "compound expression must have at least one expression");
  struct td_var_ty var_ty =
      td_compoundexpr.exprs[td_compoundexpr.num_exprs - 1].var_ty;

  return (struct td_expr){.ty = TD_EXPR_TY_COMPOUNDEXPR,
                          .var_ty = var_ty,
                          .compound_expr = td_compoundexpr};
}

enum td_init_mode {
  TD_INIT_MODE_NORMAL,
  TD_INIT_MODE_CONSTANT_EXPRS,
};

static struct td_init_list type_init_list(struct typechk *tchk,
                                          const struct td_var_ty *var_ty,
                                          const struct ast_init_list *init_list,
                                          enum td_init_mode mode);

static struct td_expr
type_compound_literal(struct typechk *tchk,
                      const struct ast_compound_literal *compound_literal,
                      enum td_init_mode mode) {
  struct td_var_ty var_ty = type_type_name(tchk, &compound_literal->type_name);

  return (struct td_expr){
      .ty = TD_EXPR_TY_COMPOUND_LITERAL,
      .var_ty = var_ty,
      .compound_literal = {
          .var_ty = var_ty,
          .init_list = type_init_list(tchk, &var_ty,
                                      &compound_literal->init_list, mode)}};
}

static struct td_expr type_generic(struct typechk *tchk,
                                   const struct ast_generic *generic) {
  // NOTE: ctrl_expr is never evaluated, so gets discarded here

  struct td_expr ctrl =
      type_expr(tchk, TYPE_EXPR_FLAGS_NONE, generic->ctrl_expr);

  // do a first pass through to validate
  // TODO: also validate for duplicate types (not just duplicate `default`)

  struct ast_generic_association *selected_association = NULL;
  struct ast_generic_association *default_association = NULL;

  for (size_t i = 0; i < generic->num_associations; i++) {
    struct ast_generic_association *association = &generic->associations[i];

    switch (association->ty) {
    case AST_GENERIC_ASSOCIATION_TY_TYPE_NAME: {
      struct td_var_ty assoc_var_ty =
          type_type_name(tchk, &association->type_name);

      if (td_var_ty_compatible(tchk, &assoc_var_ty, &ctrl.var_ty,
                               TD_VAR_TY_COMPATIBLE_FLAG_LVALUE_CONVERT_LHS)) {
        if (selected_association) {
          tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(
                  COMPATIBLE_GENERIC_ASSOCIATIONS,
                  compatible_generic_associations, association->span,
                  MK_INVALID_TEXT_POS(0),
                  "multiple associations in '_Generic' matched"));

          return (struct td_expr){.ty = TD_EXPR_TY_INVALID,
                                  .var_ty = TD_VAR_TY_UNKNOWN};
        } else {
          selected_association = association;
        }
      }
      break;
    }
    case AST_GENERIC_ASSOCIATION_TY_DEFAULT:
      if (default_association) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(
                DUPLICATE_GENERIC_DEFAULT, duplicate_generic_default,
                association->span, MK_INVALID_TEXT_POS(0),
                "duplicate 'default' expression in '_Generic'"));

        return (struct td_expr){.ty = TD_EXPR_TY_INVALID,
                                .var_ty = TD_VAR_TY_UNKNOWN};
      } else {
        default_association = association;
      }
      break;
    }
  }

  if (!selected_association) {
    if (!default_association) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              DUPLICATE_GENERIC_DEFAULT, duplicate_generic_default,
              generic->span, MK_INVALID_TEXT_POS(0),
              "no association matched and no default found"));

      return (struct td_expr){.ty = TD_EXPR_TY_INVALID,
                              .var_ty = TD_VAR_TY_UNKNOWN};
    }

    selected_association = default_association;
  }

  return type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &selected_association->expr);
}

static struct td_expr type_expr(struct typechk *tchk,
                                enum type_expr_flags flags,
                                const struct ast_expr *expr) {
  struct td_expr td_expr;

  switch (expr->ty) {
  case AST_EXPR_TY_INVALID:
    BUG("INVALID expr type should not reach typechk (should trigger "
        "diagnostic "
        "-> trigger parse fail)");
    break;
  case AST_EXPR_TY_GENERIC:
    td_expr = type_generic(tchk, &expr->generic);
    break;
  case AST_EXPR_TY_TERNARY:
    td_expr = type_ternary(tchk, &expr->ternary);
    break;
  case AST_EXPR_TY_CALL:
    td_expr = type_call(tchk, &expr->call);
    break;
  case AST_EXPR_TY_UNARY_OP:
    td_expr = type_unary_op(tchk, &expr->unary_op);
    break;
  case AST_EXPR_TY_BINARY_OP:
    td_expr = type_binary_op(tchk, &expr->binary_op);
    break;
  case AST_EXPR_TY_ARRAYACCESS:
    td_expr = type_arrayaccess(tchk, &expr->array_access);
    break;
  case AST_EXPR_TY_MEMBERACCESS:
    td_expr = type_memberaccess(tchk, flags, &expr->member_access);
    break;
  case AST_EXPR_TY_POINTERACCESS:
    td_expr = type_pointeraccess(tchk, flags, &expr->pointer_access);
    break;
  case AST_EXPR_TY_ASSG:
    td_expr = type_assg(tchk, &expr->assg);
    break;
  case AST_EXPR_TY_VAR:
    td_expr = type_var(tchk, &expr->var);
    break;
  case AST_EXPR_TY_CNST:
    td_expr = type_cnst(tchk, &expr->cnst);
    break;
  case AST_EXPR_TY_COMPOUNDEXPR:
    td_expr = type_compoundexpr(tchk, flags, &expr->compound_expr);
    break;
  case AST_EXPR_TY_SIZEOF:
    td_expr = type_sizeof(tchk, &expr->size_of);
    break;
  case AST_EXPR_TY_ALIGNOF:
    td_expr = type_alignof(tchk, &expr->align_of);
    break;
  case AST_EXPR_TY_COMPOUND_LITERAL:
    td_expr = type_compound_literal(tchk, &expr->compound_literal,
                                    TD_INIT_MODE_NORMAL);
    break;
  }

  if (!(flags & TYPE_EXPR_FLAGS_ARRAYS_DONT_DECAY) &&
      td_expr.var_ty.ty == TD_VAR_TY_TY_ARRAY) {
    struct td_var_ty *el = td_expr.var_ty.array.underlying;

    td_expr.var_ty.ty = TD_VAR_TY_TY_POINTER;
    td_expr.var_ty.pointer = (struct td_ty_pointer){.underlying = el};
  }

  if (td_expr.var_ty.ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    struct td_var_ty complete;
    (void)try_get_completed_aggregate(tchk, &td_expr.var_ty, &complete);
    td_expr.var_ty = complete;
  }

  td_expr.span = expr->span;

  return td_expr;
}

// we need this logic for constant expressions that use sizeof/alignof
// it is basically ripped out of `ir/ir.c`
// this is ugly, i would love a better solution
struct td_var_ty_info {
  size_t size;
  size_t alignment;
  size_t *offsets;
  size_t num_offsets;
};

static struct td_var_ty_info td_var_ty_info(struct typechk *tchk,
                                            const struct td_var_ty *ty,
                                            const struct text_span *context) {
  switch (ty->ty) {
  case TD_VAR_TY_TY_UNKNOWN:
    // FIXME: should emit diagnostic
    return (struct td_var_ty_info){0};
  case TD_VAR_TY_TY_VARIADIC:
    BUG("TD_VAR_TY_TY_VARIADIC has no size");
  case TD_VAR_TY_TY_VOID:
    BUG("TD_VAR_TY_TY_VOID has no size");
  case TD_VAR_TY_TY_INCOMPLETE_AGGREGATE:
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            INCOMPLETE_TYPE, incomplete_type, *context, MK_INVALID_TEXT_POS(0),
            "cannot get size or alignment of incomplete type"));
    return (struct td_var_ty_info){0};
  case TD_VAR_TY_TY_FUNC:
  case TD_VAR_TY_TY_POINTER:
    switch (tchk->target->lp_sz) {
    case TARGET_LP_SZ_LP32:
      return (struct td_var_ty_info){.size = 4, .alignment = 4};
    case TARGET_LP_SZ_LP64:
      return (struct td_var_ty_info){.size = 8, .alignment = 8};
    }
  case TD_VAR_TY_TY_WELL_KNOWN:
    switch (ty->well_known) {
    case WELL_KNOWN_TY_BOOL:
    case WELL_KNOWN_TY_CHAR:
    case WELL_KNOWN_TY_SIGNED_CHAR:
    case WELL_KNOWN_TY_UNSIGNED_CHAR:
      return (struct td_var_ty_info){.size = 1, .alignment = 1};
    case WELL_KNOWN_TY_SIGNED_SHORT:
    case WELL_KNOWN_TY_UNSIGNED_SHORT:
    case WELL_KNOWN_TY_HALF:
      return (struct td_var_ty_info){.size = 2, .alignment = 2};
    case WELL_KNOWN_TY_SIGNED_INT:
    case WELL_KNOWN_TY_UNSIGNED_INT:
    case WELL_KNOWN_TY_FLOAT:
      return (struct td_var_ty_info){.size = 4, .alignment = 4};
    case WELL_KNOWN_TY_SIGNED_LONG:
    case WELL_KNOWN_TY_UNSIGNED_LONG:
      switch (tchk->target->lp_sz) {
      case TARGET_LP_SZ_LP32:
        return (struct td_var_ty_info){.size = 4, .alignment = 4};
      case TARGET_LP_SZ_LP64:
        return (struct td_var_ty_info){.size = 8, .alignment = 8};
      }
    case WELL_KNOWN_TY_SIGNED_LONG_LONG:
    case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
      return (struct td_var_ty_info){.size = 8, .alignment = 8};
    case WELL_KNOWN_TY_DOUBLE:
      return (struct td_var_ty_info){.size = 8, .alignment = 8};
    case WELL_KNOWN_TY_LONG_DOUBLE:
      return (struct td_var_ty_info){.size = 8, .alignment = 8};
    case WELL_KNOWN_TY_INT128:
    case WELL_KNOWN_TY_UINT128:
      return (struct td_var_ty_info){.size = 16, .alignment = 16};
    }
  case TD_VAR_TY_TY_ARRAY: {
    struct td_var_ty_info element_info =
        td_var_ty_info(tchk, ty->array.underlying, context);
    size_t size = ty->array.size * element_info.size;

    return (struct td_var_ty_info){.size = size,
                                   .alignment = element_info.alignment};
  }
  case TD_VAR_TY_TY_AGGREGATE: {
    switch (ty->aggregate.ty) {
    case TD_TY_AGGREGATE_TY_STRUCT: {
      size_t max_alignment = 0;
      size_t size = 0;
      size_t num_fields = ty->aggregate.num_fields;
      size_t *offsets = arena_alloc(tchk->arena, sizeof(*offsets) * num_fields);

      for (size_t i = 0; i < ty->aggregate.num_fields; i++) {
        struct td_struct_field *field = &ty->aggregate.fields[i];
        struct td_var_ty_info info =
            td_var_ty_info(tchk, &field->var_ty, context);
        max_alignment = MAX(max_alignment, info.alignment);

        size = ROUND_UP(size, info.alignment);

        offsets[i] = size;

        size += info.size;
      }

      return (struct td_var_ty_info){.size = size,
                                     .alignment = max_alignment,
                                     .offsets = offsets,
                                     .num_offsets = num_fields};
    }
    case TD_TY_AGGREGATE_TY_UNION: {
      size_t max_alignment = 0;
      size_t size = 0;

      for (size_t i = 0; i < ty->aggregate.num_fields; i++) {
        struct td_struct_field *field = &ty->aggregate.fields[i];
        struct td_var_ty_info info =
            td_var_ty_info(tchk, &field->var_ty, context);
        max_alignment = MAX(max_alignment, info.alignment);

        size = MAX(size, info.size);
      }

      return (struct td_var_ty_info){.size = size, .alignment = max_alignment};
    }
    }
  }
  }
}

enum eval_constant_integral_expr_flags {
  EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_NONE = 0,
  EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS = 1 << 0,
};

// temporary type while td_cnst does not use ap_val
struct td_val {
  struct td_var_ty var_ty;
  struct ap_val val;
};

static bool try_get_member_offset(struct typechk *tchk,
                                  const struct td_var_ty *aggregate,
                                  struct sized_str member_name,
                                  struct text_span *context, size_t *offset) {
  DEBUG_ASSERT(aggregate->ty == TD_VAR_TY_TY_AGGREGATE, "expected aggregate");

  *offset = 0;

  for (size_t member_idx = 0; member_idx < aggregate->aggregate.num_fields;
       member_idx++) {
    struct td_struct_field *field = &aggregate->aggregate.fields[member_idx];
    if (!field->identifier.len) {
      // anonymous field
      size_t anon_member_offset;

      if (!try_get_member_offset(tchk, &field->var_ty, member_name, context,
                                 &anon_member_offset)) {
        continue;
      }

      DEBUG_ASSERT(member_idx < aggregate->aggregate.num_fields,
                   "member_idx out of range");

      struct td_var_ty_info info = td_var_ty_info(tchk, aggregate, context);

      // offsets are null for a union
      *offset += anon_member_offset;
      *offset += info.offsets ? info.offsets[member_idx] : 0;
      return true;
    } else if (szstreq(field->identifier, member_name)) {
      DEBUG_ASSERT(member_idx < aggregate->aggregate.num_fields,
                   "member_idx out of range");

      struct td_var_ty_info info = td_var_ty_info(tchk, aggregate, context);

      // offsets are null for a union
      *offset += info.offsets ? info.offsets[member_idx] : 0;
      return true;
    }
  }

  return false;
}

static bool
eval_constant_integral_expr(struct typechk *tchk, const struct td_expr *expr,
                            enum eval_constant_integral_expr_flags flags,
                            struct td_val *value) {
  // for recurse calls make sure value stays well defined
  *value =
      (struct td_val){.var_ty = TD_VAR_TY_UNKNOWN, .val = MK_AP_VAL_INVALID()};

  if (expr->ty == TD_EXPR_TY_CNST) {
    switch (expr->cnst.ty) {
    case TD_CNST_TY_NUM:
      *value =
          (struct td_val){.var_ty = expr->var_ty, .val = expr->cnst.num_value};
      return true;
    case TD_CNST_TY_STRING:
      if (flags & EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(
                BAD_ENUM_INIT, bad_enum_init, expr->span,
                MK_INVALID_TEXT_POS(0),
                "only integer or floating-point constants are "
                "allowed in constant expressions, not string"));
      }

      return false;
    }
  }

  if (expr->ty == TD_EXPR_TY_SIZEOF) {
    struct td_var_ty_info info =
        td_var_ty_info(tchk, &expr->size_of.var_ty, &expr->span);

    *value = (struct td_val){.var_ty = expr->var_ty,
                             .val = ap_val_from_ull(info.size, 32)};
    return true;
  }

  if (expr->ty == TD_EXPR_TY_ALIGNOF) {
    struct td_var_ty_info info =
        td_var_ty_info(tchk, &expr->align_of.var_ty, &expr->span);

    *value = (struct td_val){.var_ty = expr->var_ty,
                             .val = ap_val_from_ull(info.alignment, 32)};
    return true;
  }

  if (expr->ty == TD_EXPR_TY_VAR) {
    if (expr->var.ty != TD_VAR_VAR_TY_ENUMERATOR) {
      // FIXME: this is actually more general "bad value in constant expr",
      // not just for enums

      if (flags & EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(
                BAD_ENUM_INIT, bad_enum_init, expr->span,
                MK_INVALID_TEXT_POS(0),
                "variables in constant "
                "expressions must be constants or enum values"));
      }

      return false;
    }

    *value = (struct td_val){.var_ty = expr->var_ty,
                             .val = ap_val_from_ull(expr->var.enumerator, 32)};
    return true;
  }

  if (expr->ty == TD_EXPR_TY_COMPOUNDEXPR) {
    DEBUG_ASSERT(expr->compound_expr.num_exprs, "empty compound expr");

    struct td_val expr_value = {.var_ty = TD_VAR_TY_UNKNOWN,
                                .val = MK_AP_VAL_INVALID()};
    for (size_t i = 0; i < expr->compound_expr.num_exprs; i++) {
      eval_constant_integral_expr(tchk, &expr->compound_expr.exprs[i], flags,
                                  &expr_value);
    }

    *value = expr_value;
    return true;
  }

  if (expr->ty == TD_EXPR_TY_TERNARY) {
    // i think evaluating both is the correct thing to do?
    struct td_val cond;
    if (!eval_constant_integral_expr(tchk, expr->ternary.cond, flags, &cond)) {
      return false;
    }

    struct td_val lhs;
    if (!eval_constant_integral_expr(tchk, expr->ternary.true_expr, flags,
                                     &lhs)) {
      return false;
    }

    struct td_val rhs;
    if (!eval_constant_integral_expr(tchk, expr->ternary.false_expr, flags,
                                     &rhs)) {
      return false;
    }

    *value = ap_val_nonzero(cond.val) ? lhs : rhs;
    return true;
  }

  if (expr->ty == TD_EXPR_TY_UNARY_OP) {
    if (expr->unary_op.ty == TD_UNARY_OP_TY_ADDRESSOF &&
        expr->unary_op.expr->ty == TD_EXPR_TY_POINTERACCESS) {
      // need to recognise `&(((Ty)cnst)->member)` as an offsetof idiom (for
      // `cnst + offsetof(Ty, member)`) explicit support is not needed, but
      // may as well in case programs do it explicitly rather than using
      // `offsetof` macro

      struct td_expr *pointer_access = expr->unary_op.expr;

      struct td_val lhs;
      if (!eval_constant_integral_expr(tchk, pointer_access->pointer_access.lhs,
                                       flags, &lhs)) {
        return false;
      }

      struct td_var_ty *agg_ty =
          pointer_access->pointer_access.lhs->var_ty.pointer.underlying;

      DEBUG_ASSERT(agg_ty->ty == TD_VAR_TY_TY_AGGREGATE,
                   "expected aggregate (or to be err'd already)");

      size_t offset = 0;
      if (!try_get_member_offset(tchk, agg_ty,
                                 pointer_access->pointer_access.member,
                                 &pointer_access->span, &offset)) {
        BUG("nonexistent member should have been caught already");
      }

      struct ap_val val =
          ap_val_add(lhs.val, ap_val_from_ull(offset, lhs.val.ap_int.num_bits));

      *value = (struct td_val){.var_ty = expr->var_ty, .val = val};

      return true;
    }

    struct td_val expr_value;
    if (!eval_constant_integral_expr(tchk, expr->unary_op.expr, flags,
                                     &expr_value)) {
      return false;
    }

    // FIXME: casts and round to real type so no overflow (+ bool casts)
    switch (expr->unary_op.ty) {
    case TD_UNARY_OP_TY_PLUS:
      *value = expr_value; // FIXME: this should promote type
      return true;
    case TD_UNARY_OP_TY_MINUS:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_negate(expr_value.val)};
      return true;
    case TD_UNARY_OP_TY_LOGICAL_NOT:
      *value = (struct td_val){
          .var_ty = expr->var_ty,
          .val = ap_val_from_ull(ap_val_iszero(expr_value.val), 32)};
      return true;
    case TD_UNARY_OP_TY_NOT:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_not(expr_value.val)};
      return true;
    case TD_UNARY_OP_TY_CAST:
      switch (expr->var_ty.ty) {
      case TD_VAR_TY_TY_POINTER:
        switch (expr_value.var_ty.ty) {
        case TD_VAR_TY_TY_POINTER:
          // nop
          *value =
              (struct td_val){.var_ty = expr->var_ty, .val = expr_value.val};
          return true;

        case TD_VAR_TY_TY_WELL_KNOWN:
          if (td_var_ty_is_fp_ty(&expr_value.var_ty)) {
            goto bad_cast;
          } else {
            td_var_ty_pointer_sized_int(tchk, false);
            DEBUG_ASSERT(expr_value.val.ty == AP_VAL_TY_INT, "expected int?");

            // TODO: trunc/sext/zext to ptr size
            *value =
                (struct td_val){.var_ty = expr->var_ty, .val = expr_value.val};
            return true;
          }
        default:
          goto bad_cast;
        }

      case TD_VAR_TY_TY_WELL_KNOWN:
        switch (expr_value.var_ty.ty) {
        case TD_VAR_TY_TY_POINTER:
        case TD_VAR_TY_TY_WELL_KNOWN: {
          // nop

          size_t num_bits;
          enum ap_float_ty float_ty;
          switch (expr->var_ty.well_known) {
          case WELL_KNOWN_TY_BOOL:
            *value = (struct td_val){
                .var_ty = expr->var_ty,
                .val = ap_val_from_ull(ap_val_nonzero(expr_value.val), 1)};
            return true;
          case WELL_KNOWN_TY_CHAR:
          case WELL_KNOWN_TY_SIGNED_CHAR:
          case WELL_KNOWN_TY_UNSIGNED_CHAR:
            num_bits = 8;
            goto to_int;
          case WELL_KNOWN_TY_SIGNED_SHORT:
          case WELL_KNOWN_TY_UNSIGNED_SHORT:
            num_bits = 16;
            goto to_int;
          case WELL_KNOWN_TY_SIGNED_INT:
          case WELL_KNOWN_TY_UNSIGNED_INT:
            num_bits = 32;
            goto to_int;
          case WELL_KNOWN_TY_SIGNED_LONG:
          case WELL_KNOWN_TY_UNSIGNED_LONG:
            switch (tchk->target->lp_sz) {
            case TARGET_LP_SZ_LP32:
              num_bits = 32;
              break;
            case TARGET_LP_SZ_LP64:
              num_bits = 64;
              break;
            }
            goto to_int;
          case WELL_KNOWN_TY_SIGNED_LONG_LONG:
          case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
            num_bits = 64;
            goto to_int;
          case WELL_KNOWN_TY_INT128:
          case WELL_KNOWN_TY_UINT128:
            num_bits = 128;
            goto to_int;

          to_int:
            *value =
                (struct td_val){.var_ty = expr->var_ty,
                                .val = ap_val_to_int(expr_value.val, num_bits)};
            return true;
          case WELL_KNOWN_TY_HALF:
            float_ty = AP_FLOAT_TY_F16;
            goto to_float;
          case WELL_KNOWN_TY_FLOAT:
            float_ty = AP_FLOAT_TY_F32;
            goto to_float;
          case WELL_KNOWN_TY_DOUBLE:
            float_ty = AP_FLOAT_TY_F64;
            goto to_float;
          case WELL_KNOWN_TY_LONG_DOUBLE:
            float_ty = AP_FLOAT_TY_F64;
            goto to_float;

          to_float:
            *value = (struct td_val){
                .var_ty = expr->var_ty,
                .val = ap_val_to_float(expr_value.val, float_ty)};
            return true;
          }
        default:
          goto bad_cast;
        }
        }

      case TD_VAR_TY_TY_UNKNOWN:
        return false;

      default:
        goto bad_cast;
      }

    bad_cast:
      if (flags & EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(
                CAST, cast, expr->span, MK_INVALID_TEXT_POS(0),
                arena_alloc_snprintf(
                    tchk->arena, "cast is not legal (from '%s' to '%s')",
                    tchk_type_name(tchk, &expr->var_ty),
                    tchk_type_name(tchk, &expr->unary_op.cast.var_ty))));
      }
      return false;

    case TD_UNARY_OP_TY_PREFIX_INC:
    case TD_UNARY_OP_TY_PREFIX_DEC:
    case TD_UNARY_OP_TY_POSTFIX_INC:
    case TD_UNARY_OP_TY_POSTFIX_DEC:
      // FIXME: this is actually more general "bad value in constant expr",
      // not just for enums
      if (flags & EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(BAD_ENUM_INIT, bad_enum_init, expr->span,
                                   MK_INVALID_TEXT_POS(0),
                                   "cannot use ++/-- in constant expr"));
      }
      return false;
    case TD_UNARY_OP_TY_INDIRECTION:
      if (flags & EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS) {
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(BAD_ENUM_INIT, bad_enum_init, expr->span,
                                   MK_INVALID_TEXT_POS(0),
                                   "cannot use & in constant expr"));
      }
      return false;
    case TD_UNARY_OP_TY_ADDRESSOF: {
      if (flags & EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS) {
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(BAD_ENUM_INIT, bad_enum_init, expr->span,
                                   MK_INVALID_TEXT_POS(0),
                                   "cannot use & in constant expr"));
      }
      return false;
    }
    }
  }

  if (expr->ty == TD_EXPR_TY_BINARY_OP) {
    struct td_val lhs;
    if (!eval_constant_integral_expr(tchk, expr->binary_op.lhs, flags, &lhs)) {
      return false;
    }

    struct td_val rhs;
    if (!eval_constant_integral_expr(tchk, expr->binary_op.rhs, flags, &rhs)) {
      return false;
    }

#define CHECK_INT_OPS()                                                        \
  do {                                                                         \
    if (lhs.val.ty == AP_VAL_TY_FLOAT || rhs.val.ty == AP_VAL_TY_FLOAT) {      \
      if (flags & EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS) {         \
        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;                           \
        compiler_diagnostics_add(                                              \
            tchk->diagnostics,                                                 \
            MK_SEMANTIC_DIAGNOSTIC(                                            \
                FP_BITWISE, fp_bitwise, expr->span, MK_INVALID_TEXT_POS(0),    \
                "cannot perform bitwise operations on floating-point types")); \
      }                                                                        \
                                                                               \
      *value = (struct td_val){.var_ty = TD_VAR_TY_UNKNOWN,                    \
                               .val = MK_AP_VAL_INVALID()};                    \
      return false;                                                            \
    }                                                                          \
  } while (0)

    // FIXME: maybe wrong wrt sizes, definitely wrong wrt to signs

    // TEMP: hack sizes
    if (lhs.val.ty == AP_VAL_TY_INT && rhs.val.ty == AP_VAL_TY_INT &&
        lhs.val.ap_int.num_bits != rhs.val.ap_int.num_bits) {
      size_t num_bits = MAX(lhs.val.ap_int.num_bits, rhs.val.ap_int.num_bits);

      lhs.val = ap_val_from_ull(ap_int_as_ull(lhs.val.ap_int), num_bits);
      rhs.val = ap_val_from_ull(ap_int_as_ull(rhs.val.ap_int), num_bits);
    }

    switch (expr->binary_op.ty) {
    case TD_BINARY_OP_TY_EQ:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_eq(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_NEQ:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_neq(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_GT:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_gt(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_GTEQ:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_gteq(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_LT:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_lt(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_LTEQ:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_lteq(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_LOGICAL_OR:
      *value = (struct td_val){
          .var_ty = expr->var_ty,
          .val = ap_val_from_ull(
              (int)ap_val_nonzero(lhs.val) | (int)ap_val_nonzero(rhs.val), 32)};
      return true;
    case TD_BINARY_OP_TY_LOGICAL_AND:
      *value = (struct td_val){
          .var_ty = expr->var_ty,
          .val = ap_val_from_ull(
              (int)ap_val_nonzero(lhs.val) & (int)ap_val_nonzero(rhs.val), 32)};
      return true;
    case TD_BINARY_OP_TY_OR:
      CHECK_INT_OPS();
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_or(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_AND:
      CHECK_INT_OPS();
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_and(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_XOR:
      CHECK_INT_OPS();
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_xor(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_LSHIFT:
      CHECK_INT_OPS();
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_lshift(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_RSHIFT:
      CHECK_INT_OPS();
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_rshift(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_ADD:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_add(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_SUB:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_sub(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_MUL:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_mul(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_DIV:
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_div(lhs.val, rhs.val)};
      return true;
    case TD_BINARY_OP_TY_MOD:
      CHECK_INT_OPS();
      *value = (struct td_val){.var_ty = expr->var_ty,
                               .val = ap_val_mod(lhs.val, rhs.val)};
      return true;
    }
  }

  if (flags & EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(BAD_INTEGRAL_CNST_EXPR, bad_integral_cnst_expr,
                               expr->span, MK_INVALID_TEXT_POS(0),
                               "constant integral expr was expected"));
  }

  return false;
}

static unsigned long long
type_constant_integral_expr(struct typechk *tchk, const struct ast_expr *expr) {
  struct td_val value;

  struct td_expr td_expr = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, expr);

  (void)eval_constant_integral_expr(
      tchk, &td_expr, EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS,
      &value);

  switch (value.val.ty) {
  case AP_VAL_TY_INVALID:
    // invalid, diagnostics already emitted
    return 0;
  case AP_VAL_TY_INT:
    return ap_int_as_ull(value.val.ap_int);
  case AP_VAL_TY_FLOAT:
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            BAD_INTEGRAL_CNST_EXPR, bad_integral_cnst_expr, expr->span,
            MK_INVALID_TEXT_POS(0),
            "integer expr was expected but found floating-point"));
    return 0;
  }
}

static struct ap_val type_constant_expr(struct typechk *tchk,
                                        const struct ast_expr *expr) {
  struct td_val value;

  struct td_expr td_expr = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, expr);

  (void)eval_constant_integral_expr(
      tchk, &td_expr, EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS,
      &value);

  return value.val;
}

static struct td_expr
type_static_init_expr(struct typechk *tchk, struct td_expr expr,
                      struct td_var_ty target_ty,
                      // whether we are in an addressing node and so array
                      // access + member access are legal
                      bool is_addr) {
  // FIXME: i think casting logic in here may be inadequate (ie need more
  // calls to add_cast_if_needed)

  if (expr.ty == TD_EXPR_TY_CNST && expr.cnst.ty == TD_CNST_TY_STRING) {
    return expr;
  }

  // FIXME: think this func should take a context span and use that
  expr = add_cast_if_needed(tchk, expr.span, expr, target_ty);

  struct td_val cnst_value;
  if (eval_constant_integral_expr(
          tchk, &expr, EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_NONE, &cnst_value)) {

    // FIXME: types are wrong here, it is hacked in because this returns
    // td_expr
    switch (cnst_value.val.ty) {
    case AP_VAL_TY_INVALID:
      return (struct td_expr){
          .ty = TD_EXPR_TY_INVALID,
          .var_ty = TD_VAR_TY_UNKNOWN,
          .span = expr.span,
      };
    case AP_VAL_TY_INT:
    case AP_VAL_TY_FLOAT:
      return (struct td_expr){
          .ty = TD_EXPR_TY_CNST,
          .var_ty = cnst_value.var_ty,
          .cnst = {.ty = TD_CNST_TY_NUM, .num_value = cnst_value.val},
          .span = expr.span,
      };
    }
  }

  switch (expr.ty) {
  case TD_EXPR_TY_CNST: {
    // string literals are allowed here but not in cnst expressions

    return expr;
  }
  case TD_EXPR_TY_VAR: {
    // important special case: `int *p = a` where a is an array
    // we pass flag DONT_DECAY so we can look at the returned value to check
    // it is an array

    // TODO: make this diagnostic work
    // if (expr.var_ty.ty != TD_VAR_TY_TY_ARRAY) {
    // tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    // compiler_diagnostics_add(
    //     tchk->diagnostics,
    //     MK_SEMANTIC_DIAGNOSTIC(BAD_STATIC_INIT_EXPR, bad_static_init_expr,
    //                            expr.span, MK_INVALID_TEXT_POS(0),
    //                            "expression not valid as static "
    //                            "initializer; assigment is only allowed when
    //                            " "rhs is an array decaying to a pointer"));

    // return (struct td_expr){.ty = TD_EXPR_TY_INVALID,
    //                         .var_ty = TD_VAR_TY_UNKNOWN};
    // }

    return expr;
  }

  case TD_EXPR_TY_COMPOUND_LITERAL: {
    return expr;
  }

  case TD_EXPR_TY_POINTERACCESS: {
    if (!is_addr) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(BAD_STATIC_INIT_EXPR, bad_static_init_expr,
                                 expr.span, MK_INVALID_TEXT_POS(0),
                                 "in a constant expression, the '->' "
                                 "operator can only be used in "
                                 "an address-of expression"));
      return (struct td_expr){
          .ty = TD_EXPR_TY_INVALID,
          .var_ty = TD_VAR_TY_UNKNOWN,
          .span = expr.span,
      };
    }

    struct td_expr res = {
        .ty = TD_EXPR_TY_POINTERACCESS,
        .var_ty = expr.var_ty,
        .pointer_access =
            (struct td_pointeraccess){
                .lhs =
                    arena_alloc(tchk->arena, sizeof(*res.pointer_access.lhs)),
                .member = expr.pointer_access.member,
            },
        .span = expr.span,
    };

    *res.pointer_access.lhs =
        type_static_init_expr(tchk, *expr.pointer_access.lhs,
                              expr.pointer_access.lhs->var_ty, is_addr);

    return res;
  }

  case TD_EXPR_TY_MEMBERACCESS: {
    if (!is_addr) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(BAD_STATIC_INIT_EXPR, bad_static_init_expr,
                                 expr.span, MK_INVALID_TEXT_POS(0),
                                 "in a constant expression, the '.' operator "
                                 "can only be used in "
                                 "an address-of expression"));
      return (struct td_expr){
          .ty = TD_EXPR_TY_INVALID,
          .var_ty = TD_VAR_TY_UNKNOWN,
          .span = expr.span,
      };
    }

    struct td_expr res = {
        .ty = TD_EXPR_TY_MEMBERACCESS,
        .var_ty = expr.var_ty,
        .member_access =
            (struct td_memberaccess){
                .lhs = arena_alloc(tchk->arena, sizeof(*res.member_access.lhs)),
                .member = expr.member_access.member,
            },
        .span = expr.span,
    };

    *res.member_access.lhs = type_static_init_expr(
        tchk, *expr.member_access.lhs, expr.member_access.lhs->var_ty, is_addr);
    return res;
  }

  case TD_EXPR_TY_ARRAYACCESS: {
    if (!is_addr) {
      goto generic_fail;
    }

    struct td_expr res = {
        .ty = TD_EXPR_TY_ARRAYACCESS,
        .var_ty = expr.var_ty,
        .array_access =
            (struct td_arrayaccess){
                .lhs = arena_alloc(tchk->arena, sizeof(*res.array_access.lhs)),
                .rhs = arena_alloc(tchk->arena, sizeof(*res.array_access.rhs)),
            },
        .span = expr.span,
    };

    *res.array_access.lhs = type_static_init_expr(
        tchk, *expr.array_access.lhs, expr.array_access.lhs->var_ty, is_addr);
    *res.array_access.rhs = type_static_init_expr(
        tchk, *expr.array_access.rhs, expr.array_access.rhs->var_ty, is_addr);
    return res;
  }
  case TD_EXPR_TY_UNARY_OP:
    // may be address-of
    switch (expr.unary_op.ty) {
    case TD_UNARY_OP_TY_ADDRESSOF: {
      struct td_expr addr = {
          .ty = TD_EXPR_TY_UNARY_OP,
          .var_ty = expr.var_ty,
          .unary_op = (struct td_unary_op){.ty = TD_UNARY_OP_TY_ADDRESSOF,
                                           .expr = arena_alloc(
                                               tchk->arena,
                                               sizeof(*addr.unary_op.expr))},
          .span = expr.span,
      };

      *addr.unary_op.expr = type_static_init_expr(
          tchk, *expr.unary_op.expr, expr.unary_op.expr->var_ty, true);
      return addr;
    }
    case TD_UNARY_OP_TY_CAST:
      if ((td_var_ty_is_integral_ty(&expr.var_ty) ||
           expr.var_ty.ty == TD_VAR_TY_TY_POINTER) &&
          (td_var_ty_is_integral_ty(&expr.unary_op.expr->var_ty) ||
           expr.unary_op.expr->var_ty.ty == TD_VAR_TY_TY_POINTER ||
           expr.unary_op.expr->var_ty.ty == TD_VAR_TY_TY_ARRAY ||
           expr.unary_op.expr->var_ty.ty == TD_VAR_TY_TY_FUNC)) {
        // pointer/int to pointer/int cast, fine
        return type_static_init_expr(tchk, *expr.unary_op.expr,
                                     expr.unary_op.expr->var_ty, is_addr);
      }

      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;

      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              CAST, cast, expr.span, MK_INVALID_TEXT_POS(0),
              arena_alloc_snprintf(
                  tchk->arena,
                  "cast is not legal in constant expression (from '%s' to "
                  "'%s')",
                  tchk_type_name(tchk, &expr.var_ty),
                  tchk_type_name(tchk, &expr.unary_op.expr->var_ty))));

      return (struct td_expr){
          .ty = TD_EXPR_TY_INVALID,
          .var_ty = TD_VAR_TY_UNKNOWN,
          .span = expr.span,
      };
    default:
      // invalid for some other reason
      break;
    }
    break;
  case TD_EXPR_TY_BINARY_OP: {
    // may be `ptr + cnst` or `cnst + ptr`
    struct td_expr lhs =
        type_static_init_expr(tchk, *expr.binary_op.lhs, expr.var_ty, is_addr);
    struct td_expr rhs =
        type_static_init_expr(tchk, *expr.binary_op.rhs, expr.var_ty, is_addr);

    struct td_val cnst_val;
    struct td_expr ptr, cnst;
    if (eval_constant_integral_expr(
            tchk, &lhs, EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_NONE, &cnst_val)) {
      ptr = rhs;
      cnst = lhs;
    } else if (eval_constant_integral_expr(
                   tchk, &rhs, EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_NONE,
                   &cnst_val)) {
      ptr = lhs;
      cnst = rhs;
    } else {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(BAD_STATIC_INIT_EXPR, bad_static_init_expr,
                                 expr.span, MK_INVALID_TEXT_POS(0),
                                 "expression not valid as static "
                                 "initializer; both sides are pointer-type"));

      return (struct td_expr){
          .ty = TD_EXPR_TY_INVALID,
          .var_ty = TD_VAR_TY_UNKNOWN,
          .span = expr.span,
      };
    }

    struct td_expr res = {
        .ty = TD_EXPR_TY_BINARY_OP,
        .var_ty = ptr.var_ty,
        .binary_op =
            (struct td_binary_op){
                .ty = expr.binary_op.ty,
                .lhs = arena_alloc(tchk->arena, sizeof(*res.binary_op.lhs)),
                .rhs = arena_alloc(tchk->arena, sizeof(*res.binary_op.rhs)),
            },
        .span = expr.span,
    };

    *res.binary_op.lhs = ptr;
    *res.binary_op.rhs = cnst;

    switch (res.binary_op.ty) {
    case TD_BINARY_OP_TY_ADD:
    case TD_BINARY_OP_TY_SUB:
      break;
    default:
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              BAD_STATIC_INIT_EXPR, bad_static_init_expr, expr.span,
              MK_INVALID_TEXT_POS(0),
              "expression not valid as static initializer; only add/sub are "
              "allowed for pointer arithmetic"));
    }

    return res;
  }

  default:
  generic_fail:
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(BAD_STATIC_INIT_EXPR, bad_static_init_expr,
                               expr.span, MK_INVALID_TEXT_POS(0),
                               "expression not valid as static initializer"));
    return (struct td_expr){
        .ty = TD_EXPR_TY_INVALID,
        .var_ty = TD_VAR_TY_UNKNOWN,
        .span = expr.span,
    };
  }

  // expr failed in constant eval, but was not a pointer/address expr we
  // expected so has some other issue rerun constant eval with diagnostics
  // flag to get good errors if it succeeds, then emit a generic error

  if (eval_constant_integral_expr(
          tchk, &expr, EVAL_CONSTANT_INTEGRAL_EXPR_FLAG_EMIT_DIAGNOSTICS,
          &cnst_value)) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(BAD_STATIC_INIT_EXPR, bad_static_init_expr,
                               expr.span, MK_INVALID_TEXT_POS(0),
                               "expression not valid as static initializer"));
  }

  return (struct td_expr){
      .ty = TD_EXPR_TY_INVALID,
      .var_ty = TD_VAR_TY_UNKNOWN,
      .span = expr.span,
  };
}

static struct td_stmt type_stmt(struct typechk *tchk,
                                const struct ast_stmt *stmt);

static struct td_labeledstmt
type_labeledstmt(struct typechk *tchk, const struct ast_labeledstmt *labeled) {
  struct td_labeledstmt td_labeled;

  switch (labeled->ty) {
  case AST_LABELEDSTMT_TY_LABEL:
    td_labeled.ty = TD_LABELEDSTMT_TY_LABEL;
    td_labeled.label = identifier_str(tchk->parser, &labeled->label);
    break;
  case AST_LABELEDSTMT_TY_CASE:
    td_labeled.ty = TD_LABELEDSTMT_TY_CASE;
    td_labeled.cnst = type_constant_integral_expr(tchk, &labeled->cnst);
    break;
  case AST_LABELEDSTMT_TY_DEFAULT:
    td_labeled.ty = TD_LABELEDSTMT_TY_DEFAULT;
    break;
  }

  td_labeled.stmt = arena_alloc(tchk->arena, sizeof(*td_labeled.stmt));
  *td_labeled.stmt = type_stmt(tchk, labeled->stmt);

  return td_labeled;
}

static struct td_declaration_or_expr
type_declaration_or_expr(struct typechk *tchk,
                         const struct ast_declaration_or_expr *decl_or_expr) {
  struct td_declaration_or_expr td_decl_or_expr;

  switch (decl_or_expr->ty) {
  case AST_DECLARATION_OR_EXPR_TY_DECL:
    td_decl_or_expr.ty = TD_DECLARATION_OR_EXPR_TY_DECL;
    td_decl_or_expr.decl =
        type_declaration(tchk, &decl_or_expr->decl, TD_DECLARATOR_MODE_NORMAL);
    break;
  case AST_DECLARATION_OR_EXPR_TY_EXPR:
    td_decl_or_expr.ty = TD_DECLARATION_OR_EXPR_TY_EXPR;
    td_decl_or_expr.expr =
        type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &decl_or_expr->expr);
    break;
  }

  return td_decl_or_expr;
}

static struct td_forstmt type_forstmt(struct typechk *tchk,
                                      const struct ast_forstmt *forstmt) {
  struct td_forstmt td_for = {
      .init = NULL, .cond = NULL, .body = NULL, .iter = NULL};

  if (forstmt->init) {
    td_for.init = arena_alloc(tchk->arena, sizeof(*td_for.init));
    *td_for.init = type_declaration_or_expr(tchk, forstmt->init);
  }

  td_for.body = arena_alloc(tchk->arena, sizeof(*td_for.body));
  *td_for.body = type_stmt(tchk, forstmt->body);

  if (forstmt->cond) {
    td_for.cond = arena_alloc(tchk->arena, sizeof(*td_for.cond));
    *td_for.cond = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, forstmt->cond);
  }

  if (forstmt->iter) {
    td_for.iter = arena_alloc(tchk->arena, sizeof(*td_for.iter));
    *td_for.iter = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, forstmt->iter);
  }

  return td_for;
}

static struct td_dowhilestmt
type_dowhilestmt(struct typechk *tchk,
                 const struct ast_dowhilestmt *dowhilestmt) {

  struct td_dowhilestmt td_dowhile = {
      .cond = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &dowhilestmt->cond),
  };

  td_dowhile.body = arena_alloc(tchk->arena, sizeof(*td_dowhile.body));
  *td_dowhile.body = type_stmt(tchk, dowhilestmt->body);

  return td_dowhile;
}

static struct td_whilestmt
type_whilestmt(struct typechk *tchk, const struct ast_whilestmt *whilestmt) {
  struct td_whilestmt td_while = {
      .cond = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &whilestmt->cond),
  };

  td_while.body = arena_alloc(tchk->arena, sizeof(*td_while.body));
  *td_while.body = type_stmt(tchk, whilestmt->body);

  return td_while;
}

static struct td_iterstmt type_iterstmt(struct typechk *tchk,
                                        const struct ast_iterstmt *iterstmt) {
  struct td_iterstmt td_iter;
  switch (iterstmt->ty) {
  case AST_ITERSTMT_TY_WHILE:
    td_iter.ty = TD_ITERSTMT_TY_WHILE;
    td_iter.while_stmt = type_whilestmt(tchk, &iterstmt->while_stmt);
    break;
  case AST_ITERSTMT_TY_DO_WHILE:
    td_iter.ty = TD_ITERSTMT_TY_DO_WHILE;
    td_iter.do_while_stmt = type_dowhilestmt(tchk, &iterstmt->do_while_stmt);
    break;
  case AST_ITERSTMT_TY_FOR:
    td_iter.ty = TD_ITERSTMT_TY_FOR;
    td_iter.for_stmt = type_forstmt(tchk, &iterstmt->for_stmt);
    break;
  }

  return td_iter;
}

static struct td_jumpstmt type_jumpstmt(struct typechk *tchk,
                                        const struct ast_jumpstmt *jumpstmt) {

  struct td_jumpstmt td_jump;
  switch (jumpstmt->ty) {
  case AST_JUMPSTMT_TY_BREAK:
    td_jump.ty = TD_JUMPSTMT_TY_BREAK;
    break;
  case AST_JUMPSTMT_TY_CONTINUE:
    td_jump.ty = TD_JUMPSTMT_TY_CONTINUE;
    break;
  case AST_JUMPSTMT_TY_GOTO:
    td_jump.ty = TD_JUMPSTMT_TY_GOTO;
    td_jump.goto_stmt = (struct td_gotostmt){
        .label = identifier_str(tchk->parser, &jumpstmt->goto_stmt.label)};
    break;
  case AST_JUMPSTMT_TY_RETURN:
    td_jump.ty = TD_JUMPSTMT_TY_RETURN;

    if (jumpstmt->return_stmt.expr) {
      td_jump.return_stmt = (struct td_returnstmt){
          .expr = arena_alloc(tchk->arena, sizeof(*td_jump.return_stmt.expr))};

      *td_jump.return_stmt.expr = add_cast_if_needed(
          tchk, jumpstmt->span,
          type_expr(tchk, TYPE_EXPR_FLAGS_NONE, jumpstmt->return_stmt.expr),
          tchk->ret_ty);
    } else {
      td_jump.return_stmt = (struct td_returnstmt){.expr = NULL};
    }

    break;
  }

  return td_jump;
}

static struct td_ifstmt type_ifstmt(struct typechk *tchk,
                                    const struct ast_ifstmt *ifstmt) {
  struct td_ifstmt td_if = {
      .cond = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &ifstmt->cond),
      .body = arena_alloc(tchk->arena, sizeof(*td_if.body))};

  *td_if.body = type_stmt(tchk, ifstmt->body);

  return td_if;
}

static struct td_ifelsestmt
type_ifelsestmt(struct typechk *tchk, const struct ast_ifelsestmt *ifelsestmt) {
  struct td_ifelsestmt td_if_else = {
      .cond = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &ifelsestmt->cond),
      .body = arena_alloc(tchk->arena, sizeof(*td_if_else.body)),
      .else_body = arena_alloc(tchk->arena, sizeof(*td_if_else.else_body))};

  *td_if_else.body = type_stmt(tchk, ifelsestmt->body);
  *td_if_else.else_body = type_stmt(tchk, ifelsestmt->else_body);

  return td_if_else;
}

static struct td_switchstmt
type_switchstmt(struct typechk *tchk, const struct ast_switchstmt *switchstmt) {
  struct td_switchstmt td_switch = {
      .ctrl_expr =
          type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &switchstmt->ctrl_expr),
      .body = arena_alloc(tchk->arena, sizeof(*td_switch.body)),
  };

  *td_switch.body = type_stmt(tchk, switchstmt->body);

  return td_switch;
}

static struct td_selectstmt
type_selectstmt(struct typechk *tchk, const struct ast_selectstmt *selectstmt) {
  struct td_selectstmt td_select;

  switch (selectstmt->ty) {
  case AST_SELECTSTMT_TY_IF:
    td_select.ty = TD_SELECTSTMT_TY_IF;
    td_select.if_stmt = type_ifstmt(tchk, &selectstmt->if_stmt);
    break;
  case AST_SELECTSTMT_TY_IF_ELSE:
    td_select.ty = TD_SELECTSTMT_TY_IF_ELSE;
    td_select.if_else_stmt = type_ifelsestmt(tchk, &selectstmt->if_else_stmt);
    break;
  case AST_SELECTSTMT_TY_SWITCH:
    td_select.ty = TD_SELECTSTMT_TY_SWITCH;
    td_select.switch_stmt = type_switchstmt(tchk, &selectstmt->switch_stmt);
    break;
  }

  return td_select;
}

static struct td_declaration
type_declaration(struct typechk *tchk,
                 const struct ast_declaration *declaration,
                 enum td_declarator_mode mode);

static struct td_compoundstmt
type_compoundstmt(struct typechk *tchk,
                  const struct ast_compoundstmt *compound_stmt);

static void type_staticassert(struct typechk *tchk,
                              const struct ast_staticassert *staticassert);

static struct td_stmt type_stmt(struct typechk *tchk,
                                const struct ast_stmt *stmt) {
  struct td_stmt td_stmt;

  switch (stmt->ty) {
  case AST_STMT_TY_NULL:
    td_stmt = (struct td_stmt){
        .ty = TD_STMT_TY_NULL,
    };
    break;
  case AST_STMT_TY_STATICASSERT:
    type_staticassert(tchk, &stmt->staticassert);
    td_stmt = (struct td_stmt){
        .ty = TD_STMT_TY_NULL,
    };
    break;
  case AST_STMT_TY_DECLARATION:
    td_stmt = (struct td_stmt){.ty = TD_STMT_TY_DECLARATION,
                               .declaration =
                                   type_declaration(tchk, &stmt->declaration,
                                                    TD_DECLARATOR_MODE_NORMAL)};
    break;
  case AST_STMT_TY_LABELED:
    td_stmt =
        (struct td_stmt){.ty = TD_STMT_TY_LABELED,
                         .labeled = type_labeledstmt(tchk, &stmt->labeled)};
    break;
  case AST_STMT_TY_EXPR:
    td_stmt = (struct td_stmt){
        .ty = TD_STMT_TY_EXPR,
        .expr = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &stmt->expr)};
    break;
  case AST_STMT_TY_COMPOUND:
    td_stmt =
        (struct td_stmt){.ty = TD_STMT_TY_COMPOUND,
                         .compound = type_compoundstmt(tchk, &stmt->compound)};
    break;
  case AST_STMT_TY_JUMP:
    td_stmt = (struct td_stmt){.ty = TD_STMT_TY_JUMP,
                               .jump = type_jumpstmt(tchk, &stmt->jump)};
    break;
  case AST_STMT_TY_ITER:
    td_stmt = (struct td_stmt){.ty = TD_STMT_TY_ITER,
                               .iter = type_iterstmt(tchk, &stmt->iter)};
    break;
  case AST_STMT_TY_SELECT:
    td_stmt = (struct td_stmt){.ty = TD_STMT_TY_SELECT,
                               .select = type_selectstmt(tchk, &stmt->select)};
  }

  return td_stmt;
}

static struct td_compoundstmt
type_compoundstmt(struct typechk *tchk,
                  const struct ast_compoundstmt *compound_stmt) {

  tchk_push_scope(tchk);

  struct td_compoundstmt td_cmpd = {
      .num_stmts = compound_stmt->num_stmts,
      .stmts = arena_alloc(tchk->arena,
                           sizeof(*td_cmpd.stmts) * compound_stmt->num_stmts)};

  for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
    td_cmpd.stmts[i] = type_stmt(tchk, &compound_stmt->stmts[i]);
  }

  tchk_pop_scope(tchk);

  return td_cmpd;
}

static struct td_funcdef type_funcdef(struct typechk *tchk,
                                      const struct ast_funcdef *func_def) {
  if (func_def->declaration_list.num_declarations) {
    BUG("old-style function arguments not currently supported");
  }

  // struct td_var_ty func_ty = func_def->declarator
  // struct td_funcdef td_func_def = {.func_ty = };

  struct td_specifiers specifiers =
      type_specifiers(tchk, &func_def->specifier_list,
                      TD_SPECIFIER_ALLOW_FUNCTION_SPECIFIERS |
                          TD_SPECIFIER_ALLOW_STORAGE_CLASS_SPECIFIERS |
                          TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS |
                          TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS);

  struct td_var_declaration declaration =
      type_declarator(tchk, &specifiers, &func_def->declarator, NULL,
                      TD_DECLARATOR_MODE_NORMAL);

  struct var_table_entry *func_entry = vt_create_entry(
      &tchk->var_table, VAR_TABLE_NS_NONE, declaration.var.identifier);
  func_entry->var = arena_alloc(tchk->arena, sizeof(*func_entry->var));
  *func_entry->var = declaration.var;
  func_entry->var_ty = arena_alloc(tchk->arena, sizeof(*func_entry->var_ty));
  *func_entry->var_ty = declaration.var_ty;

  // param scope
  tchk_push_scope(tchk);

  tchk->ret_ty = *declaration.var_ty.func.ret;

  for (size_t i = 0; i < declaration.var_ty.func.num_params; i++) {
    struct td_ty_param *param = &declaration.var_ty.func.params[i];

    struct sized_str identifier = param->identifier;

    if (!identifier.len) {
      continue;
    }

    struct td_var var = {
        .ty = TD_VAR_VAR_TY_VAR,
        .identifier = identifier,
        .scope = SCOPE_PARAMS,
    };

    struct var_table_entry *entry =
        vt_create_entry(&tchk->var_table, VAR_TABLE_NS_NONE, identifier);
    entry->var = arena_alloc(tchk->arena, sizeof(*entry->var));
    *entry->var = var;
    entry->var_ty = arena_alloc(tchk->arena, sizeof(*entry->var_ty));
    *entry->var_ty = param->var_ty;

    // decay array params to pointers here
    if (entry->var_ty->ty == TD_VAR_TY_TY_ARRAY) {
      struct td_var_ty underlying =
          td_var_ty_get_underlying(tchk, entry->var_ty);
      *entry->var_ty = td_var_ty_make_pointer(tchk, &underlying,
                                              TD_TYPE_QUALIFIER_FLAG_NONE);
    }
  }

  // push the magic "__func__" variable
  struct td_var var = {
      .ty = TD_VAR_VAR_TY_VAR,
      .identifier = MK_SIZED("__func__"),
      .scope = SCOPE_PARAMS,
  };

  struct var_table_entry *entry =
      vt_create_entry(&tchk->var_table, VAR_TABLE_NS_NONE, var.identifier);
  entry->var = arena_alloc(tchk->arena, sizeof(*entry->var));
  *entry->var = var;
  entry->var_ty = arena_alloc(tchk->arena, sizeof(*entry->var_ty));
  *entry->var_ty = TD_VAR_TY_CONST_CHAR_POINTER;

  struct td_funcdef td_funcdef = {
      .storage_class_specifier = specifiers.storage,
      .function_specifier_flags = specifiers.function,
      .var_declaration = declaration,
      .body = {.ty = TD_STMT_TY_COMPOUND,
               .compound = type_compoundstmt(tchk, &func_def->body)},
      .span = func_def->span};

  // param scope
  tchk_pop_scope(tchk);

  return td_funcdef;
}

static struct td_init type_init(struct typechk *tchk, struct text_span context,
                                struct td_var_ty *var_ty,
                                const struct ast_init *init,
                                enum td_init_mode mode);

static struct td_designator
type_designator(struct typechk *tchk, const struct td_var_ty *var_ty,
                const struct ast_designator *designator) {
  switch (designator->ty) {
  case AST_DESIGNATOR_TY_FIELD: {
    struct sized_str field = identifier_str(tchk->parser, &designator->field);

    struct td_var_ty field_ty;
    if (!try_resolve_member_access_ty(tchk, var_ty, field, &field_ty)) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(NO_MEMBER, no_member, designator->span,
                                 MK_INVALID_TEXT_POS(0),
                                 "unknown member in designator"));
      field_ty = TD_VAR_TY_UNKNOWN;
    }

    return (struct td_designator){
        .ty = TD_DESIGNATOR_TY_FIELD, .var_ty = field_ty, .field = field};
  }
  case AST_DESIGNATOR_TY_INDEX: {
    struct td_var_ty el_ty = td_var_ty_get_underlying(tchk, var_ty);

    return (struct td_designator){
        .ty = TD_DESIGNATOR_TY_INDEX,
        .var_ty = el_ty,
        .index = type_constant_integral_expr(tchk, designator->index)};
  }
  }
}

static struct td_designator_list
type_designator_list(struct typechk *tchk, const struct td_var_ty *var_ty,
                     const struct ast_designator_list *designator_list) {
  struct td_designator_list td_designator_list = {
      .num_designators = designator_list->num_designators,
      .designators =
          arena_alloc(tchk->arena, sizeof(*td_designator_list.designators) *
                                       designator_list->num_designators)};

  struct td_var_ty cur_var_ty = *var_ty;

  for (size_t i = 0; i < designator_list->num_designators; i++) {
    td_designator_list.designators[i] =
        type_designator(tchk, &cur_var_ty, &designator_list->designators[i]);

    cur_var_ty = td_designator_list.designators[i].var_ty;
  }

  td_designator_list.var_ty = cur_var_ty;

  return td_designator_list;
}

static struct td_init_list_init
type_init_list_init(struct typechk *tchk, const struct td_var_ty *var_ty,
                    const struct td_var_ty *next_field_var_ty,
                    const struct ast_init_list_init *init_list_init,
                    enum td_init_mode mode) {
  struct td_init_list_init td_init_list_init = {
      .designator_list = NULL,
      .init = arena_alloc(tchk->arena, sizeof(*td_init_list_init.init))};

  struct td_var_ty init_list_var_ty;
  if (init_list_init->designator_list) {
    td_init_list_init.designator_list =
        arena_alloc(tchk->arena, sizeof(*td_init_list_init.designator_list));
    *td_init_list_init.designator_list =
        type_designator_list(tchk, var_ty, init_list_init->designator_list);

    init_list_var_ty = td_init_list_init.designator_list->var_ty;
  } else {
    init_list_var_ty = *next_field_var_ty;
  }

  *td_init_list_init.init =
      type_init(tchk, init_list_init->span, &init_list_var_ty,
                init_list_init->init, mode);

  return td_init_list_init;
}

TODO_FUNC(static struct td_expr type_zero_expr(struct typechk *tchk,
                                               const struct td_var_ty *var_ty))

static struct td_expr
type_init_list_for_scalar(struct typechk *tchk, const struct td_var_ty *var_ty,
                          const struct ast_init_list *init_list,
                          enum td_init_mode mode) {
  struct td_var_ty res_var_ty = *var_ty;

  if (init_list->num_inits == 0) {
    // zero-init
    return type_zero_expr(tchk, var_ty);
  }

  if (init_list->num_inits > 1) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(SCALAR_INIT_MULTIPLE_VALUES,
                               scalar_init_multiple_values, init_list->span,
                               MK_INVALID_TEXT_POS(0),
                               "trying to initialise scalar with init list "
                               "with multiple values"));

    res_var_ty = TD_VAR_TY_UNKNOWN;
  }

  const struct ast_init_list_init *init_list_init = &init_list->inits[0];

  if (init_list_init->designator_list) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(
            SCALAR_DESIGNATOR, scalar_designator, init_list->span,
            MK_INVALID_TEXT_POS(0),
            "cannot use designator list in scalar initialization"));

    res_var_ty = TD_VAR_TY_UNKNOWN;
  }

  const struct ast_init *init = init_list_init->init;
  switch (init->ty) {
  case AST_INIT_TY_EXPR:
    return add_cast_if_needed(
        tchk, init_list_init->span,
        type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &init->expr), res_var_ty);
  case AST_INIT_TY_INIT_LIST:
    return type_init_list_for_scalar(tchk, &res_var_ty, &init->init_list, mode);
  }
}

static struct td_init_list type_init_list_for_aggregate_or_array(
    struct typechk *tchk, const struct td_var_ty *var_ty,
    const struct ast_init_list *init_list, enum td_init_mode mode,
    size_t start_idx, bool top, size_t *inits_used) {

  size_t d = 0;
  if (!inits_used) {
    DEBUG_ASSERT(top, "expected to be top");
    inits_used = &d;
  }

  struct vector *inits = vector_create(sizeof(struct td_init_list_init));

  size_t num_inits =
      top ? init_list->num_inits
          : MIN(init_list->num_inits - start_idx, td_num_init_fields(var_ty));

  size_t field_index = 0;
  for (size_t i = start_idx; i < start_idx + num_inits; i++) {
    const struct ast_init_list_init *init = &init_list->inits[i];

    struct td_var_ty member_var_ty;
    if (init->designator_list && init->designator_list->num_designators) {
      const struct ast_designator *designator =
          &init->designator_list->designators[0];

      // modify the top level position
      switch (designator->ty) {
      case AST_DESIGNATOR_TY_INDEX:
        field_index = type_constant_integral_expr(tchk, designator->index);
        break;
      case AST_DESIGNATOR_TY_FIELD: {
        if (!try_get_member_idx(
                tchk, var_ty, identifier_str(tchk->parser, &designator->field),
                &field_index, designator->span)) {
          tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(NO_MEMBER, no_member, designator->span,
                                     MK_INVALID_TEXT_POS(0),
                                     "unknown designator for init list"));
        }

        break;
      }
      }
    } else {
      size_t cur_field_index = field_index;

      if (!try_resolve_member_access_ty_by_index(tchk, var_ty, cur_field_index,
                                                 &member_var_ty, init->span)) {
        member_var_ty = TD_VAR_TY_UNKNOWN;

        tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
        compiler_diagnostics_add(
            tchk->diagnostics,
            MK_SEMANTIC_DIAGNOSTIC(NO_MEMBER, no_member, init->span,
                                   MK_INVALID_TEXT_POS(0),
                                   "unknown member for init list"));
      }
    }

    struct td_init_list_init td_init_list_init;
    if (init->init->ty == AST_INIT_TY_EXPR && !init->designator_list &&
        (member_var_ty.ty == TD_VAR_TY_TY_AGGREGATE ||
         member_var_ty.ty == TD_VAR_TY_TY_ARRAY)) {
      // if the next value is the same type as the element, it initialises the
      // whole thing
      // FIXME: we are double typing this expression because it gets retyped
      // in the branches
      struct td_expr typed =
          type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &init->init->expr);

      if (td_var_ty_compatible(tchk, &member_var_ty, &typed.var_ty,
                               TD_VAR_TY_COMPATIBLE_FLAG_NONE)) {
        td_init_list_init =
            type_init_list_init(tchk, var_ty, &member_var_ty, init, mode);

        (*inits_used)++;
      } else {
        td_init_list_init = (struct td_init_list_init){
            .designator_list = NULL,
            .init = arena_alloc(tchk->arena, sizeof(*td_init_list_init.init))};

        size_t sub_inits_used = 0;
        *td_init_list_init.init =
            (struct td_init){.ty = TD_INIT_TY_INIT_LIST,
                             .init_list = type_init_list_for_aggregate_or_array(
                                 tchk, &member_var_ty, init_list, mode, i,
                                 false, &sub_inits_used)};

        i += sub_inits_used - 1;
        *inits_used += sub_inits_used;
      }
    } else {
      td_init_list_init =
          type_init_list_init(tchk, var_ty, &member_var_ty, init, mode);

      (*inits_used)++;
    }

    vector_push_back(inits, &td_init_list_init);

    field_index++;
  }

  struct td_init_list td_init_list = {
      .var_ty = *var_ty,
      .num_inits = vector_length(inits),
      .inits = arena_alloc(tchk->arena, vector_byte_size(inits))};

  vector_copy_to(inits, td_init_list.inits);
  vector_free(&inits);

  return td_init_list;
}

static struct td_init_list type_init_list(struct typechk *tchk,
                                          const struct td_var_ty *var_ty,
                                          const struct ast_init_list *init_list,
                                          enum td_init_mode mode) {
  if (init_list->num_inits == 0) {
    if (tchk->args->c_standard < COMPILE_C_STANDARD_C23) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(EMPTY_INIT_C23, empty_init_c23,
                                 init_list->span, MK_INVALID_TEXT_POS(0),
                                 "empy initializer is a c23 feature"));
      return (struct td_init_list){.var_ty = TD_VAR_TY_UNKNOWN};
    }
  }

  struct td_var_ty complete;
  if (var_ty->ty == TD_VAR_TY_TY_INCOMPLETE_AGGREGATE) {
    (void)try_get_completed_aggregate(tchk, var_ty, &complete);
  } else {
    complete = *var_ty;
  }

  if (complete.ty == TD_VAR_TY_TY_ARRAY ||
      complete.ty == TD_VAR_TY_TY_AGGREGATE) {
    return type_init_list_for_aggregate_or_array(tchk, &complete, init_list,
                                                 mode, 0, true, NULL);
  }

  if (init_list->num_inits > 1) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(TOO_MANY_INITS, too_many_inits, init_list->span,
                               MK_INVALID_TEXT_POS(0),
                               "scalar initialiser should have 1 or 0 values"));
    return (struct td_init_list){.var_ty = TD_VAR_TY_UNKNOWN};
  }

  if (init_list->inits[0].designator_list) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(NO_MEMBER, no_member, init_list->span,
                               MK_INVALID_TEXT_POS(0),
                               "scalar init list should not have designator"));
    return (struct td_init_list){.var_ty = TD_VAR_TY_UNKNOWN};
  }

  switch (init_list->inits[0].init->ty) {
  case AST_INIT_TY_EXPR: {
    struct td_init_list scalar = {
        .var_ty = *var_ty,
        .num_inits = 1,
        .inits = arena_alloc(tchk->arena, sizeof(*scalar.inits))};

    scalar.inits[0] = (struct td_init_list_init){
        .designator_list = NULL,
        .init = arena_alloc(tchk->arena, sizeof(*scalar.inits[0].init))};

    struct td_expr expr = type_expr(tchk, TYPE_EXPR_FLAGS_ARRAYS_DONT_DECAY,
                                    &init_list->inits[0].init->expr);

    expr =
        add_cast_if_needed(tchk, init_list->inits[0].init->span, expr, *var_ty);

    *scalar.inits[0].init = (struct td_init){
        .ty = TD_INIT_TY_EXPR,
        .expr = expr,
    };

    return scalar;
  }
  case AST_INIT_TY_INIT_LIST:
    return type_init_list(tchk, var_ty, &init_list->inits[0].init->init_list,
                          mode);
  }
}

static struct td_init type_init(struct typechk *tchk, struct text_span context,
                                struct td_var_ty *var_ty,
                                const struct ast_init *init,
                                enum td_init_mode mode) {
  struct td_init td_init;

  switch (init->ty) {
  case AST_INIT_TY_EXPR:
    td_init.ty = TD_INIT_TY_EXPR;

    switch (mode) {
    case TD_INIT_MODE_NORMAL:
      td_init.expr = type_expr(tchk, TYPE_EXPR_FLAGS_NONE, &init->expr);
      break;
    case TD_INIT_MODE_CONSTANT_EXPRS:
      // FIXME: i think this might be wrong, as it needs to pass mode in case
      // this is a compound literal
      td_init.expr = type_static_init_expr(
          tchk, type_expr(tchk, TYPE_EXPR_FLAGS_ARRAYS_DONT_DECAY, &init->expr),
          *var_ty, false);
      break;
    }

    if (td_init.expr.ty == TD_EXPR_TY_CNST &&
        td_init.expr.cnst.ty == TD_CNST_TY_STRING) {

      if (var_ty->ty == TD_VAR_TY_TY_WELL_KNOWN) {
        if ((td_init.expr.cnst.str_value.ty == TD_CNST_STR_TY_ASCII &&
             (var_ty->well_known == WELL_KNOWN_TY_CHAR ||
              var_ty->well_known == WELL_KNOWN_TY_SIGNED_CHAR ||
              var_ty->well_known == WELL_KNOWN_TY_UNSIGNED_CHAR)) ||
            (td_init.expr.cnst.str_value.ty == TD_CNST_STR_TY_WIDE &&
             (var_ty->well_known == WELL_KNOWN_TY_SIGNED_INT ||
              var_ty->well_known == WELL_KNOWN_TY_UNSIGNED_INT))) {
          // FIXME: mutating existing
          struct td_cnst_str str = td_init.expr.cnst.str_value;
          td_init.expr.var_ty = (struct td_var_ty){
              .ty = TD_VAR_TY_TY_ARRAY,
              .array = {.size = str.ty == TD_CNST_STR_TY_ASCII ? str.ascii.len
                                                               : str.wide.len,
                        .underlying = arena_alloc(
                            tchk->arena,
                            sizeof(*td_init.expr.var_ty.array.underlying))}};

          *td_init.expr.var_ty.array.underlying =
              var_ty->ty == TD_VAR_TY_TY_ARRAY ? *var_ty->array.underlying
                                               : *var_ty;
        } else {

          tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
          compiler_diagnostics_add(
              tchk->diagnostics,
              MK_SEMANTIC_DIAGNOSTIC(
                  CAST, cast, context, MK_INVALID_TEXT_POS(0),
                  arena_alloc_snprintf(
                      tchk->arena, "cast is not legal (from '%s' to '%s')",
                      tchk_type_name(tchk, &td_init.expr.var_ty),
                      tchk_type_name(tchk, var_ty))));
        }

        break;
      }
    }

    td_init.expr = add_cast_if_needed(tchk, context, td_init.expr, *var_ty);

    break;
  case AST_INIT_TY_INIT_LIST:
    if (td_var_ty_is_scalar_ty(var_ty)) {
      td_init.ty = TD_INIT_TY_EXPR;
      td_init.expr =
          type_init_list_for_scalar(tchk, var_ty, &init->init_list, mode);
    } else {
      td_init.ty = TD_INIT_TY_INIT_LIST;
      td_init.init_list = type_init_list(tchk, var_ty, &init->init_list, mode);
    }
    break;
  }

  return td_init;
}

static struct td_var_declaration
type_init_declarator(struct typechk *tchk, struct text_span context,
                     const struct td_specifiers *specifiers,
                     const struct ast_init_declarator *init_declarator,
                     enum td_declarator_mode mode) {
  struct td_var_declaration td_var_decl =
      type_declarator(tchk, specifiers, &init_declarator->declarator,
                      init_declarator->init, mode);

  struct var_table_entry *entry;
  if (specifiers->storage == TD_STORAGE_CLASS_SPECIFIER_TYPEDEF) {
    entry = vt_create_entry(&tchk->ty_table, VAR_TABLE_NS_TYPEDEF,
                            td_var_decl.var.identifier);
  } else {
    entry = vt_create_entry(&tchk->var_table, VAR_TABLE_NS_NONE,
                            td_var_decl.var.identifier);

    if (specifiers->storage != TD_STORAGE_CLASS_SPECIFIER_EXTERN) {
      td_var_decl.var_ty =
          type_incomplete_var_ty(tchk, &td_var_decl.var_ty, context);
    }
  }

  entry->var = arena_alloc(tchk->arena, sizeof(*entry->var));
  *entry->var = td_var_decl.var;
  entry->var_ty = arena_alloc(tchk->arena, sizeof(*entry->var_ty));
  *entry->var_ty = td_var_decl.var_ty;

  if (init_declarator->init) {
    if (mode == TD_DECLARATOR_MODE_STRUCT) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(INIT_IN_AGGREGATE, init_in_aggregate,
                                 init_declarator->init->span,
                                 MK_INVALID_TEXT_POS(0),
                                 "struct/union cannot have an initializer"));
    } else if (specifiers->storage == TD_STORAGE_CLASS_SPECIFIER_EXTERN) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(INIT_IN_EXTERN, init_in_extern,
                                 init_declarator->init->span,
                                 MK_INVALID_TEXT_POS(0),
                                 "extern decl cannot have an initializer"));
    }

    enum td_init_mode init_mode = mode == TD_DECLARATOR_MODE_STATIC
                                      ? TD_INIT_MODE_CONSTANT_EXPRS
                                      : TD_INIT_MODE_NORMAL;

    struct td_init init = type_init(tchk, context, &td_var_decl.var_ty,
                                    init_declarator->init, init_mode);

    td_var_decl.init = arena_alloc(tchk->arena, sizeof(*td_var_decl.init));
    *td_var_decl.init = init;
  }

  return td_var_decl;
}

static struct td_declaration type_init_declarator_list(
    struct typechk *tchk, struct text_span context,
    const struct td_specifiers *specifiers,
    const struct ast_init_declarator_list *declarator_list,
    enum td_declarator_mode mode) {
  struct td_declaration td_declaration = {
      .storage_class_specifier = specifiers->storage,
      .function_specifier_flags = specifiers->function,
      .base_ty = specifiers->type_specifier,
      .num_var_declarations = declarator_list->num_init_declarators,
      .var_declarations =
          arena_alloc(tchk->arena, sizeof(*td_declaration.var_declarations) *
                                       declarator_list->num_init_declarators),
      .span = declarator_list->span};

  for (size_t i = 0; i < declarator_list->num_init_declarators; i++) {
    td_declaration.var_declarations[i] = type_init_declarator(
        tchk, context, specifiers, &declarator_list->init_declarators[i], mode);
  }

  return td_declaration;
}

static struct td_declaration
type_declaration(struct typechk *tchk,
                 const struct ast_declaration *declaration,
                 enum td_declarator_mode mode) {
  struct td_specifiers specifiers = type_specifiers(
      tchk, &declaration->specifier_list,
      TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS | TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS |
          TD_SPECIFIER_ALLOW_STORAGE_CLASS_SPECIFIERS |
          TD_SPECIFIER_ALLOW_FUNCTION_SPECIFIERS);

  if (mode == TD_DECLARATOR_MODE_NORMAL) {
    // if it is a normal declarator, e.g in a function, it may actually be
    // static i.e when we call `type_declaration` from the top level, it
    // passes `MODE_STATIC` because all file-scope variables are
    // static-initialized
    //
    // however, this local
    // ```c
    //   static int a = 10;
    // ```
    //
    // is _also_ static-initialized, but will be called with normal
    // so we need to promote based on specifiers

    // this sort of duplicates some logic done in ir build, maybe we could
    // unify

    if (specifiers.storage ==
        TD_STORAGE_CLASS_SPECIFIER_STATIC /* or thread_local */) {
      mode = TD_DECLARATOR_MODE_STATIC;
    }
  }

  // TODO: if one declarator (e.g `float a = bar`), highlight whole thing
  // else highlight just the failure (currently we are always highlighting
  // all)

  return type_init_declarator_list(tchk, declaration->span, &specifiers,
                                   &declaration->declarator_list, mode);
}

static struct td_declaration
type_struct_declaration(struct typechk *tchk,
                        const struct ast_declaration *declaration) {
  struct td_specifiers specifiers = type_specifiers(
      tchk, &declaration->specifier_list,
      TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS | TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS);

  return type_init_declarator_list(tchk, declaration->span, &specifiers,
                                   &declaration->declarator_list,
                                   TD_DECLARATOR_MODE_STRUCT);
}

static struct td_external_declaration type_external_declaration(
    struct typechk *tchk,
    const struct ast_external_declaration *external_declaration) {
  switch (external_declaration->ty) {
  case AST_EXTERNAL_DECLARATION_TY_DECLARATION:
    return (struct td_external_declaration){
        .ty = TD_EXTERNAL_DECLARATION_TY_DECLARATION,
        .declaration =
            type_declaration(tchk, &external_declaration->declaration,
                             TD_DECLARATOR_MODE_STATIC)};
  case AST_EXTERNAL_DECLARATION_TY_FUNC_DEF:
    return (struct td_external_declaration){
        .ty = TD_EXTERNAL_DECLARATION_TY_FUNC_DEF,
        .func_def = type_funcdef(tchk, &external_declaration->func_def)};
  default:
    // static assert handled outside (as it does not generate a decl)
    unreachable();
  }
}

enum typechk_create_result
typechk_create(const struct target *target, const struct compile_args *args,
               struct parser *parser, struct compiler_diagnostics *diagnostics,
               struct typechk **tchk) {
  struct typechk *t = nonnull_malloc(sizeof(*t));

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  *t = (struct typechk){.arena = arena,
                        .args = args,
                        .parser = parser,
                        .target = target,
                        .next_anonymous_type_name_id = 0,
                        .ty_table = vt_create(arena),
                        .var_table = vt_create(arena),
                        .result_ty = TYPECHK_RESULT_TY_SUCCESS,
                        .diagnostics = diagnostics};

  *tchk = t;

  return TYPECHK_CREATE_RESULT_SUCCESS;
}

void typechk_free(struct typechk **tchk) {
  vt_free(&(*tchk)->ty_table);
  vt_free(&(*tchk)->var_table);

  arena_allocator_free(&(*tchk)->arena);
  (*tchk)->arena = NULL;
  free(*tchk);

  *tchk = NULL;
}

static void type_staticassert(struct typechk *tchk,
                              const struct ast_staticassert *staticassert) {
  // FIXME: should also allow floating point!
  struct ap_val cond = type_constant_expr(tchk, &staticassert->cond);

  const char *message;
  if (staticassert->message) {
    if (staticassert->message->ty != AST_EXPR_TY_CNST ||
        staticassert->message->cnst.ty != AST_CNST_TY_STR_LITERAL) {
      tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          tchk->diagnostics,
          MK_SEMANTIC_DIAGNOSTIC(
              STATIC_ASSERT_MESSAGE, static_assert_message,
              staticassert->message->span, MK_INVALID_TEXT_POS(0),
              "'static_assert' message must be a string literal"));

      return;
    }

#ifdef __JCC__
    // BUG jcc doesn't support va_list
    message = "static_assert failed";
#else
    struct ast_cnst_str cnst = staticassert->message->cnst.str_value;
    message = arena_alloc_snprintf(tchk->arena, "static_assert failed: %.*s",
                                   (int)cnst.ascii.len, cnst.ascii.value);
#endif
  } else {
    message = "static_assert failed";
  }

  if (ap_val_iszero(cond)) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        tchk->diagnostics,
        MK_SEMANTIC_DIAGNOSTIC(STATIC_ASSERT, static_assert, staticassert->span,
                               MK_INVALID_TEXT_POS(0), message));
  }
}

struct typechk_result td_typechk(struct typechk *tchk,
                                 struct ast_translationunit *translation_unit) {
  struct td_translationunit td_translation_unit = {
      .num_external_declarations = translation_unit->num_external_declarations,
      .external_declarations = arena_alloc(
          tchk->arena, sizeof(*td_translation_unit.external_declarations) *
                           translation_unit->num_external_declarations)};

  size_t head = 0;
  for (size_t i = 0; i < translation_unit->num_external_declarations; i++) {
    if (translation_unit->external_declarations[i].ty ==
        AST_EXTERNAL_DECLARATION_TY_STATIC_ASSERT) {
      type_staticassert(
          tchk, &translation_unit->external_declarations[i].staticassert);
    } else {
      td_translation_unit.external_declarations[head++] =
          type_external_declaration(
              tchk, &translation_unit->external_declarations[i]);
    }
  }

  // slightly overallocates (bc it allocates for static_asserts) but shouldn't
  // matter
  td_translation_unit.num_external_declarations = head;

  if (compiler_diagnostics_err(tchk->diagnostics)) {
    tchk->result_ty = TYPECHK_RESULT_TY_FAILURE;
  }

  return (struct typechk_result){.ty = tchk->result_ty,
                                 .translation_unit = td_translation_unit};
}

struct td_printstate {
  struct typechk *tchk;
  int indent;
};

#define DEBUG_FUNC_ENUM(ty, name)                                              \
  static void typechk_debug_print_##ty(struct td_printstate *state,            \
                                       enum td_##ty *name)

#define DEBUG_FUNC(ty, name)                                                   \
  static void typechk_debug_print_##ty(struct td_printstate *state,            \
                                       struct td_##ty *name)
#define DEBUG_CALL(ty, val) typechk_debug_print_##ty(state, val)

#define TD_PRINT_SAMELINE_Z_NOINDENT(fmt) slogsl(fmt)
#define TD_PRINT_SAMELINE_NOINDENT(fmt, ...) slogsl(fmt, __VA_ARGS__)

#define TD_PRINT_SAMELINE_Z(fmt)                                               \
  do {                                                                         \
    slogsl("%*s" fmt, state->indent * 4, "");                                  \
  } while (0)

#define TD_PRINT_SAMELINE(fmt, ...)                                            \
  do {                                                                         \
    slogsl("%*s" fmt, state->indent * 4, "", __VA_ARGS__);                     \
  } while (0)

#define TD_PRINTZ(fmt) TD_PRINT_SAMELINE_Z(fmt "\n")
#define TD_PRINT(fmt, ...) TD_PRINT_SAMELINE(fmt "\n", __VA_ARGS__)

#define INDENT() state->indent++
#define UNINDENT()                                                             \
  do {                                                                         \
    state->indent--;                                                           \
    DEBUG_ASSERT(state->indent >= 0, "indent negative!");                      \
  } while (0)

#define PUSH_INDENT()                                                          \
  int tmp_indent = state->indent;                                              \
  state->indent = 0;
#define POP_INDENT() state->indent = tmp_indent;

DEBUG_FUNC_ENUM(storage_class_specifier, storage_class_specifier) {
  switch (*storage_class_specifier) {
  case TD_STORAGE_CLASS_SPECIFIER_NONE:
    TD_PRINTZ("NONE");
    break;
  case TD_STORAGE_CLASS_SPECIFIER_TYPEDEF:
    TD_PRINTZ("TYPEDEF");
    break;
  case TD_STORAGE_CLASS_SPECIFIER_EXTERN:
    TD_PRINTZ("EXTERN");
    break;
  case TD_STORAGE_CLASS_SPECIFIER_STATIC:
    TD_PRINTZ("STATIC");
    break;
  case TD_STORAGE_CLASS_SPECIFIER_AUTO:
    TD_PRINTZ("AUTO");
    break;
  case TD_STORAGE_CLASS_SPECIFIER_REGISTER:
    TD_PRINTZ("REGISTER");
    break;
  }
}

DEBUG_FUNC_ENUM(type_qualifier_flags, type_qualifier_flags) {
  if (*type_qualifier_flags & TD_TYPE_QUALIFIER_FLAG_CONST) {
    TD_PRINTZ("CONST");
  }

  if (*type_qualifier_flags & TD_TYPE_QUALIFIER_FLAG_VOLATILE) {
    TD_PRINTZ("VOLATILE");
  }

  if (*type_qualifier_flags & TD_TYPE_QUALIFIER_FLAG_RESTRICT) {
    TD_PRINTZ("RESTRICT");
  }
}

#define TD_PRINT_STR_SAMELINE(s)                                               \
  TD_PRINT_SAMELINE_NOINDENT("'%.*s'\n", (int)(s).len, (s).str)

#define TD_PRINT_STR(s) TD_PRINT("'%.*s'\n", (int)(s).len, (s).str)

DEBUG_FUNC(var_ty, var_ty) {
  DEBUG_CALL(type_qualifier_flags, &var_ty->type_qualifiers);

  switch (var_ty->ty) {
  case TD_VAR_TY_TY_UNKNOWN:
    TD_PRINTZ("<unresolved type>");
    break;
  case TD_VAR_TY_TY_INCOMPLETE_AGGREGATE:
    switch (var_ty->aggregate.ty) {
    case TD_TY_AGGREGATE_TY_STRUCT:
      TD_PRINT_SAMELINE_Z("INCOMPLETE STRUCT ");
      TD_PRINT_STR_SAMELINE(var_ty->aggregate.name);
      break;
    case TD_TY_AGGREGATE_TY_UNION:
      TD_PRINT_SAMELINE_Z("INCOMPLETE UNION ");
      TD_PRINT_STR_SAMELINE(var_ty->aggregate.name);
      break;
    }
    break;
  case TD_VAR_TY_TY_AGGREGATE:
    switch (var_ty->aggregate.ty) {
    case TD_TY_AGGREGATE_TY_STRUCT:
      TD_PRINT_SAMELINE_Z("STRUCT ");
      TD_PRINT_STR_SAMELINE(var_ty->aggregate.name);
      break;
    case TD_TY_AGGREGATE_TY_UNION:
      TD_PRINT_SAMELINE_Z("UNION ");
      TD_PRINT_STR_SAMELINE(var_ty->aggregate.name);
      break;
    }

    INDENT();
    // TEMP: reenable once we don't recursively print types (stack overflow)
    // for (size_t i = 0; i < var_ty->aggregate.num_fields; i++) {
    //   TD_PRINT("FIELD %s", var_ty->aggregate.fields[i].identifier);
    //   INDENT();
    //   DEBUG_CALL(var_ty, &var_ty->aggregate.fields[i].var_ty);
    //   UNINDENT();
    // }
    UNINDENT();
    break;
  case TD_VAR_TY_TY_VOID:
    TD_PRINTZ("VOID");
    break;
  case TD_VAR_TY_TY_VARIADIC:
    TD_PRINTZ("VARIADIC");
    break;
  case TD_VAR_TY_TY_ARRAY:
    TD_PRINT("ARRAY SIZE %zu OF", var_ty->array.size);
    INDENT();
    DEBUG_CALL(var_ty, var_ty->array.underlying);
    UNINDENT();
    break;
  case TD_VAR_TY_TY_POINTER:
    TD_PRINTZ("POINTER TO");
    INDENT();
    DEBUG_CALL(var_ty, var_ty->pointer.underlying);
    UNINDENT();
    break;
  case TD_VAR_TY_TY_FUNC:
    switch (var_ty->func.ty) {
    case TD_TY_FUNC_TY_UNKNOWN_ARGS:
      TD_PRINTZ("UNSPECIFIED FUNC (");
      break;
    case TD_TY_FUNC_TY_KNOWN_ARGS:
      TD_PRINTZ("FUNC (");
      break;
    case TD_TY_FUNC_TY_VARIADIC:
      TD_PRINTZ("VARIADIC FUNC (");
      break;
    }
    INDENT();
    for (size_t i = 0; i < var_ty->func.num_params; i++) {
      DEBUG_CALL(var_ty, &var_ty->func.params[i].var_ty);
    }
    UNINDENT();

    TD_PRINTZ(") RETURNS");

    INDENT();
    DEBUG_CALL(var_ty, var_ty->func.ret);
    UNINDENT();

    break;
  case TD_VAR_TY_TY_WELL_KNOWN:
    switch (var_ty->well_known) {
    case WELL_KNOWN_TY_BOOL:
      TD_PRINTZ("bool");
      break;
    case WELL_KNOWN_TY_CHAR:
      TD_PRINTZ("char");
      break;
    case WELL_KNOWN_TY_SIGNED_CHAR:
      TD_PRINTZ("signed char");
      break;
    case WELL_KNOWN_TY_UNSIGNED_CHAR:
      TD_PRINTZ("unsigned char");
      break;
    case WELL_KNOWN_TY_SIGNED_SHORT:
      TD_PRINTZ("short");
      break;
    case WELL_KNOWN_TY_UNSIGNED_SHORT:
      TD_PRINTZ("unsigned short");
      break;
    case WELL_KNOWN_TY_SIGNED_INT:
      TD_PRINTZ("int");
      break;
    case WELL_KNOWN_TY_UNSIGNED_INT:
      TD_PRINTZ("unsigned int");
      break;
    case WELL_KNOWN_TY_SIGNED_LONG:
      TD_PRINTZ("long");
      break;
    case WELL_KNOWN_TY_UNSIGNED_LONG:
      TD_PRINTZ("unsigned long");
      break;
    case WELL_KNOWN_TY_SIGNED_LONG_LONG:
      TD_PRINTZ("long long");
      break;
    case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
      TD_PRINTZ("unsigned long long");
      break;
    case WELL_KNOWN_TY_HALF:
      TD_PRINTZ("half");
      break;
    case WELL_KNOWN_TY_FLOAT:
      TD_PRINTZ("float");
      break;
    case WELL_KNOWN_TY_DOUBLE:
      TD_PRINTZ("double");
      break;
    case WELL_KNOWN_TY_LONG_DOUBLE:
      TD_PRINTZ("long double");
      break;
    case WELL_KNOWN_TY_INT128:
      TD_PRINTZ("__int128_t");
      break;
    case WELL_KNOWN_TY_UINT128:
      TD_PRINTZ("__uint128_t");
      break;
    }

    break;
    // case TD_VAR_TY_TY_TYPEDEF_NAME:
    //   <#code#>
    //   break;
    // case TD_VAR_TY_TY_STRUCT:
    //   <#code#>
    //   break;
    // case TD_VAR_TY_TY_UNION:
    //   <#code#>
    //   break;
    // case TD_VAR_TY_TY_ENUM:
    //   <#code#>
    //   break;
  }
}

DEBUG_FUNC(compoundstmt, compound_stmt);
DEBUG_FUNC(expr, expr);

DEBUG_FUNC(var, var) {
  TD_PRINT_SAMELINE_Z("VARIABLE ");
  TD_PRINT_STR_SAMELINE(var->identifier);
  switch (var->scope) {
  case SCOPE_GLOBAL:
    TD_PRINTZ("SCOPE GLOBAL");
    break;
  case SCOPE_PARAMS:
    TD_PRINTZ("SCOPE PARAMS");
    break;
  default:
    TD_PRINT("SCOPE %d", var->scope);
    break;
  }

  switch (var->ty) {
  case TD_VAR_VAR_TY_VAR:
    break;
  case TD_VAR_VAR_TY_ENUMERATOR:
    TD_PRINT("VALUE '%d'", var->enumerator);
  }
}

DEBUG_FUNC(cnst, cnst) {
  switch (cnst->ty) {
  case TD_CNST_TY_NUM:
    TD_PRINT_SAMELINE_Z("CONSTANT ");
    ap_val_fprintf(stderr, cnst->num_value);
    TD_PRINTZ("");
    break;
  case TD_CNST_TY_STRING:
    TD_PRINT_SAMELINE_Z("CONSTANT ");

    switch (cnst->str_value.ty) {
    case TD_CNST_STR_TY_ASCII:
      fprint_str(stderr, cnst->str_value.ascii.value,
                 cnst->str_value.ascii.len);
      break;
    case TD_CNST_STR_TY_WIDE:
      fprint_wstr(stderr, cnst->str_value.wide.value, cnst->str_value.wide.len);
      break;
    }
    fprintf(stderr, "\n");
  }
}

DEBUG_FUNC(compoundexpr, compound_expr);

DEBUG_FUNC(unary_op, unary_op) {
  switch (unary_op->ty) {
  case TD_UNARY_OP_TY_PREFIX_INC:
    TD_PRINTZ("PREFIX INC");
    break;
  case TD_UNARY_OP_TY_PREFIX_DEC:
    TD_PRINTZ("PREFIX DEC");
    break;
  case TD_UNARY_OP_TY_POSTFIX_INC:
    TD_PRINTZ("POSTFIX INC");
    break;
  case TD_UNARY_OP_TY_POSTFIX_DEC:
    TD_PRINTZ("POSTFIX DEC");
    break;
  case TD_UNARY_OP_TY_PLUS:
    TD_PRINTZ("PLUS");
    break;
  case TD_UNARY_OP_TY_MINUS:
    TD_PRINTZ("MINUS");
    break;
  case TD_UNARY_OP_TY_LOGICAL_NOT:
    TD_PRINTZ("LOGICAL NOT");
    break;
  case TD_UNARY_OP_TY_NOT:
    TD_PRINTZ("NOT");
    break;
  case TD_UNARY_OP_TY_INDIRECTION:
    TD_PRINTZ("INDIRECTION");
    break;
  case TD_UNARY_OP_TY_ADDRESSOF:
    TD_PRINTZ("ADDRESSOF");
    break;
  case TD_UNARY_OP_TY_CAST:
    TD_PRINTZ("CAST");
    INDENT();
    DEBUG_CALL(var_ty, &unary_op->expr->var_ty);
    TD_PRINTZ("TO");
    DEBUG_CALL(var_ty, &unary_op->cast.var_ty);
    UNINDENT();
    break;
  }

  INDENT();
  DEBUG_CALL(expr, unary_op->expr);
  UNINDENT();
}

DEBUG_FUNC(binary_op, binary_op) {
  INDENT();
  switch (binary_op->ty) {
  case TD_BINARY_OP_TY_EQ:
    TD_PRINTZ("EQ");
    break;
  case TD_BINARY_OP_TY_NEQ:
    TD_PRINTZ("NEQ");
    break;
  case TD_BINARY_OP_TY_LT:
    TD_PRINTZ("LT");
    break;
  case TD_BINARY_OP_TY_LTEQ:
    TD_PRINTZ("LTEQ");
    break;
  case TD_BINARY_OP_TY_GT:
    TD_PRINTZ("GT");
    break;
  case TD_BINARY_OP_TY_GTEQ:
    TD_PRINTZ("GTEQ");
    break;
  case TD_BINARY_OP_TY_LSHIFT:
    TD_PRINTZ("LSHIFT");
    break;
  case TD_BINARY_OP_TY_RSHIFT:
    TD_PRINTZ("RSHIFT");
    break;
  case TD_BINARY_OP_TY_OR:
    TD_PRINTZ("OR");
    break;
  case TD_BINARY_OP_TY_XOR:
    TD_PRINTZ("XOR");
    break;
  case TD_BINARY_OP_TY_AND:
    TD_PRINTZ("AND");
    break;
  case TD_BINARY_OP_TY_ADD:
    TD_PRINTZ("ADD");
    break;
  case TD_BINARY_OP_TY_SUB:
    TD_PRINTZ("SUB");
    break;
  case TD_BINARY_OP_TY_MUL:
    TD_PRINTZ("MUL");
    break;
  case TD_BINARY_OP_TY_DIV:
    TD_PRINTZ("DIV");
    break;
  case TD_BINARY_OP_TY_MOD:
    TD_PRINTZ("MOD");
    break;
  case TD_BINARY_OP_TY_LOGICAL_OR:
    TD_PRINTZ("LOGICAL OR");
    break;
  case TD_BINARY_OP_TY_LOGICAL_AND:
    TD_PRINTZ("LOGICAL AND");
    break;
  }

  TD_PRINTZ("LHS: ");
  INDENT();
  DEBUG_CALL(expr, binary_op->lhs);
  UNINDENT();

  TD_PRINTZ("RHS: ");
  INDENT();
  DEBUG_CALL(expr, binary_op->rhs);
  UNINDENT();

  UNINDENT();
}

DEBUG_FUNC(compoundexpr, compound_expr) {
  TD_PRINTZ("COMPOUND EXPRESSION: ");

  INDENT();

  for (size_t i = 0; i < compound_expr->num_exprs; i++) {
    DEBUG_CALL(expr, &compound_expr->exprs[i]);
  }

  UNINDENT();
}

DEBUG_FUNC(assg, assg) {
  TD_PRINTZ("ASSIGNMENT");
  INDENT();
  DEBUG_CALL(expr, assg->assignee);

  INDENT();
  switch (assg->ty) {
  case TD_ASSG_TY_BASIC:
    TD_PRINTZ("=");
    break;
  case TD_ASSG_TY_ADD:
    TD_PRINTZ("+=");
    break;
  case TD_ASSG_TY_SUB:
    TD_PRINTZ("-=");
    break;
  case TD_ASSG_TY_MUL:
    TD_PRINTZ("*=");
    break;
  case TD_ASSG_TY_DIV:
    TD_PRINTZ("/=");
    break;
  case TD_ASSG_TY_MOD:
    TD_PRINTZ("%%=");
    break;
  case TD_ASSG_TY_LSHIFT:
    TD_PRINTZ("<<=");
    break;
  case TD_ASSG_TY_RSHIFT:
    TD_PRINTZ(">>=");
    break;
  case TD_ASSG_TY_AND:
    TD_PRINTZ("&=");
    break;
  case TD_ASSG_TY_OR:
    TD_PRINTZ("|=");
    break;
  case TD_ASSG_TY_XOR:
    TD_PRINTZ("^=");
    break;
  }

  DEBUG_CALL(expr, assg->expr);
  UNINDENT();

  UNINDENT();
}

DEBUG_FUNC(arglist, arg_list) {
  TD_PRINTZ("ARGLIST:");
  INDENT();

  for (size_t i = 0; i < arg_list->num_args; i++) {
    DEBUG_CALL(expr, &arg_list->args[i]);
  }

  UNINDENT();
}

DEBUG_FUNC(call, call) {
  TD_PRINTZ("CALL");
  INDENT();
  TD_PRINTZ("TARGET");
  DEBUG_CALL(expr, call->target);

  DEBUG_CALL(arglist, &call->arg_list);

  UNINDENT();
}

DEBUG_FUNC(pointeraccess, pointer_access) {
  TD_PRINTZ("POINTER_ACCESS");

  INDENT();
  DEBUG_CALL(expr, pointer_access->lhs);
  UNINDENT();

  TD_PRINTZ("MEMBER");

  INDENT();
  TD_PRINT_STR(pointer_access->member);
  UNINDENT();
}

DEBUG_FUNC(memberaccess, member_access) {
  TD_PRINTZ("MEMBER_ACCESS");

  INDENT();
  DEBUG_CALL(expr, member_access->lhs);
  UNINDENT();

  TD_PRINTZ("MEMBER");

  INDENT();
  TD_PRINT_STR(member_access->member);
  UNINDENT();
}

DEBUG_FUNC(arrayaccess, array_access) {
  TD_PRINTZ("ARRAY_ACCESS");

  INDENT();
  DEBUG_CALL(expr, array_access->lhs);
  UNINDENT();

  TD_PRINTZ("OFFSET");

  INDENT();
  DEBUG_CALL(expr, array_access->rhs);
  UNINDENT();
}

DEBUG_FUNC(designator, designator) {
  TD_PRINTZ("DESIGNATOR");

  DEBUG_CALL(var_ty, &designator->var_ty);

  INDENT();
  switch (designator->ty) {
  case TD_DESIGNATOR_TY_FIELD:
    TD_PRINT_SAMELINE_Z("FIELD ");
    TD_PRINT_STR_SAMELINE(designator->field);
    break;
  case AST_DESIGNATOR_TY_INDEX:
    TD_PRINT("INDEX '%llu'", designator->index);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(designator_list, designator_list) {
  TD_PRINTZ("DESIGNATOR LIST");

  DEBUG_CALL(var_ty, &designator_list->var_ty);

  INDENT();
  for (size_t i = 0; i < designator_list->num_designators; i++) {
    DEBUG_CALL(designator, &designator_list->designators[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(init_list, init_list);

DEBUG_FUNC(init, init) {
  TD_PRINTZ("INIT");
  INDENT();
  switch (init->ty) {
  case TD_INIT_TY_EXPR:
    DEBUG_CALL(expr, &init->expr);
    break;
  case TD_INIT_TY_INIT_LIST:
    DEBUG_CALL(init_list, &init->init_list);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(init_list_init, init) {
  TD_PRINTZ("INIT LIST INIT");

  if (init->designator_list) {
    DEBUG_CALL(designator_list, init->designator_list);
  }

  TD_PRINTZ("init");

  INDENT();
  DEBUG_CALL(init, init->init);
  UNINDENT();
}

DEBUG_FUNC(init_list, init_list) {
  TD_PRINTZ("INIT LIST");

  DEBUG_CALL(var_ty, &init_list->var_ty);

  INDENT();
  for (size_t i = 0; i < init_list->num_inits; i++) {
    DEBUG_CALL(init_list_init, &init_list->inits[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(sizeof, size_of) {
  TD_PRINTZ("SIZEOF");

  INDENT();
  DEBUG_CALL(var_ty, &size_of->var_ty);
  UNINDENT();
}

DEBUG_FUNC(alignof, align_of) {
  TD_PRINTZ("ALIGNOF");

  INDENT();
  TD_PRINTZ("TYPE");
  DEBUG_CALL(var_ty, &align_of->var_ty);
  UNINDENT();
}

DEBUG_FUNC(ternary, ternary) {
  TD_PRINTZ("TERNARY");

  INDENT();
  TD_PRINTZ("COND");
  DEBUG_CALL(expr, ternary->cond);
  UNINDENT();

  INDENT();
  TD_PRINTZ("TRUE");
  DEBUG_CALL(expr, ternary->true_expr);
  UNINDENT();

  INDENT();
  TD_PRINTZ("FALSE");
  DEBUG_CALL(expr, ternary->false_expr);
  UNINDENT();
}

DEBUG_FUNC(compound_literal, compound_literal) {
  TD_PRINTZ("COMPOUND LITERAL");
  INDENT();
  DEBUG_CALL(var_ty, &compound_literal->var_ty);
  DEBUG_CALL(init_list, &compound_literal->init_list);
  UNINDENT();
}

DEBUG_FUNC(expr, expr) {
  TD_PRINTZ("EXPRESSION");

  INDENT();
  DEBUG_CALL(var_ty, &expr->var_ty);
  switch (expr->ty) {
  case TD_EXPR_TY_INVALID:
    TD_PRINTZ("INVALID");
    break;
  case TD_EXPR_TY_TERNARY:
    DEBUG_CALL(ternary, &expr->ternary);
    break;
  case TD_EXPR_TY_VAR:
    DEBUG_CALL(var, &expr->var);
    break;
  case TD_EXPR_TY_CNST:
    DEBUG_CALL(cnst, &expr->cnst);
    break;
  case TD_EXPR_TY_COMPOUNDEXPR:
    DEBUG_CALL(compoundexpr, &expr->compound_expr);
    break;
  case TD_EXPR_TY_CALL:
    DEBUG_CALL(call, &expr->call);
    break;
  case TD_EXPR_TY_UNARY_OP:
    DEBUG_CALL(unary_op, &expr->unary_op);
    break;
  case TD_EXPR_TY_BINARY_OP:
    DEBUG_CALL(binary_op, &expr->binary_op);
    break;
  case TD_EXPR_TY_ARRAYACCESS:
    DEBUG_CALL(arrayaccess, &expr->array_access);
    break;
  case TD_EXPR_TY_MEMBERACCESS:
    DEBUG_CALL(memberaccess, &expr->member_access);
    break;
  case TD_EXPR_TY_POINTERACCESS:
    DEBUG_CALL(pointeraccess, &expr->pointer_access);
    break;
  case TD_EXPR_TY_ASSG:
    DEBUG_CALL(assg, &expr->assg);
    break;
  case TD_EXPR_TY_SIZEOF:
    DEBUG_CALL(sizeof, &expr->size_of);
    break;
  case TD_EXPR_TY_ALIGNOF:
    DEBUG_CALL(alignof, &expr->align_of);
    break;
  case TD_EXPR_TY_COMPOUND_LITERAL:
    DEBUG_CALL(compound_literal, &expr->compound_literal);
  }
  UNINDENT();
}

DEBUG_FUNC(var_declaration, var_declaration);

DEBUG_FUNC(declaration, declaration) {
  TD_PRINTZ("DECLARATION");

  TD_PRINTZ("STORAGE CLASS SPECIFIER");
  INDENT();
  DEBUG_CALL(storage_class_specifier, &declaration->storage_class_specifier);
  UNINDENT();

  TD_PRINTZ("VARS");
  INDENT();
  for (size_t i = 0; i < declaration->num_var_declarations; i++) {
    DEBUG_CALL(var_declaration, &declaration->var_declarations[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(jumpstmt, jump_stmt) {
  switch (jump_stmt->ty) {
  case TD_JUMPSTMT_TY_RETURN:
    TD_PRINTZ("RETURN");
    INDENT();
    if (jump_stmt->return_stmt.expr) {
      DEBUG_CALL(expr, jump_stmt->return_stmt.expr);
    }
    UNINDENT();
    break;
  case TD_JUMPSTMT_TY_GOTO:
    TD_PRINT_SAMELINE_Z("GOTO ");
    TD_PRINT_STR_SAMELINE(jump_stmt->goto_stmt.label);
    break;
  case TD_JUMPSTMT_TY_BREAK:
    TD_PRINTZ("BREAK");
    break;
  case TD_JUMPSTMT_TY_CONTINUE:
    TD_PRINTZ("CONTINUE");
    break;
  }
}

DEBUG_FUNC(stmt, stmt);

DEBUG_FUNC(switchstmt, switch_stmt) {
  TD_PRINTZ("SWITCH");
  TD_PRINTZ("CONTROL EXPRESSION");
  INDENT();
  DEBUG_CALL(expr, &switch_stmt->ctrl_expr);
  UNINDENT();

  TD_PRINTZ("BODY");
  DEBUG_CALL(stmt, switch_stmt->body);
}

DEBUG_FUNC(stmt, if_stmt);

DEBUG_FUNC(ifstmt, if_stmt) {
  TD_PRINTZ("IF");
  TD_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &if_stmt->cond);
  UNINDENT();

  TD_PRINTZ("BODY");
  DEBUG_CALL(stmt, if_stmt->body);
}

DEBUG_FUNC(ifelsestmt, if_else_stmt) {
  TD_PRINTZ("IF");
  TD_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &if_else_stmt->cond);
  UNINDENT();

  TD_PRINTZ("BODY");
  DEBUG_CALL(stmt, if_else_stmt->body);

  TD_PRINTZ("ELSE");
  DEBUG_CALL(stmt, if_else_stmt->else_body);
}

DEBUG_FUNC(selectstmt, select_stmt) {
  switch (select_stmt->ty) {
  case TD_SELECTSTMT_TY_IF:
    DEBUG_CALL(ifstmt, &select_stmt->if_stmt);
    break;
  case TD_SELECTSTMT_TY_IF_ELSE:
    DEBUG_CALL(ifelsestmt, &select_stmt->if_else_stmt);
    break;
  case TD_SELECTSTMT_TY_SWITCH:
    DEBUG_CALL(switchstmt, &select_stmt->switch_stmt);
    break;
  }
}

DEBUG_FUNC(whilestmt, while_stmt) {
  TD_PRINTZ("WHILE");
  TD_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &while_stmt->cond);
  UNINDENT();

  TD_PRINTZ("BODY");
  DEBUG_CALL(stmt, while_stmt->body);
}

DEBUG_FUNC(dowhilestmt, do_while_stmt) {
  TD_PRINTZ("DO");
  TD_PRINTZ("BODY");
  DEBUG_CALL(stmt, do_while_stmt->body);

  TD_PRINTZ("WHILE");
  TD_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &do_while_stmt->cond);
  UNINDENT();
}

DEBUG_FUNC(declaration_or_expr, decl_or_expr) {
  switch (decl_or_expr->ty) {
  case TD_DECLARATION_OR_EXPR_TY_DECL:
    DEBUG_CALL(declaration, &decl_or_expr->decl);
    break;
  case TD_DECLARATION_OR_EXPR_TY_EXPR:
    DEBUG_CALL(expr, &decl_or_expr->expr);
    break;
  }
}

DEBUG_FUNC(forstmt, for_stmt) {
  TD_PRINTZ("FOR");
  if (for_stmt->init) {
    TD_PRINTZ("INIT");
    INDENT();
    DEBUG_CALL(declaration_or_expr, for_stmt->init);
    UNINDENT();
  }
  TD_PRINTZ("COND");
  INDENT();
  if (for_stmt->cond) {
    DEBUG_CALL(expr, for_stmt->cond);
  }
  UNINDENT();
  TD_PRINTZ("ITER");
  INDENT();
  if (for_stmt->iter) {
    DEBUG_CALL(expr, for_stmt->iter);
  }
  UNINDENT();

  TD_PRINTZ("BODY");
  DEBUG_CALL(stmt, for_stmt->body);
}

DEBUG_FUNC(iterstmt, iter_stmt) {
  switch (iter_stmt->ty) {
  case TD_ITERSTMT_TY_WHILE:
    DEBUG_CALL(whilestmt, &iter_stmt->while_stmt);
    break;
  case TD_ITERSTMT_TY_DO_WHILE:
    DEBUG_CALL(dowhilestmt, &iter_stmt->do_while_stmt);
    break;
  case TD_ITERSTMT_TY_FOR:
    DEBUG_CALL(forstmt, &iter_stmt->for_stmt);
    break;
  }
}

DEBUG_FUNC(labeledstmt, labeled_stmt) {
  switch (labeled_stmt->ty) {
  case TD_LABELEDSTMT_TY_LABEL:
    TD_PRINT_SAMELINE_Z("LABEL ");
    TD_PRINT_STR_SAMELINE(labeled_stmt->label);
    break;
  case TD_LABELEDSTMT_TY_CASE:
    TD_PRINT("CASE %llu", labeled_stmt->cnst);
    break;
  case TD_LABELEDSTMT_TY_DEFAULT:
    TD_PRINTZ("DEFAULT");
    break;
  }
  TD_PRINTZ("STATEMENT");
  INDENT();
  DEBUG_CALL(stmt, labeled_stmt->stmt);
  UNINDENT();
}

DEBUG_FUNC(stmt, stmt) {
  INDENT();

  switch (stmt->ty) {
  case TD_STMT_TY_NULL:
    break;
  case TD_STMT_TY_DECLARATION:
    DEBUG_CALL(declaration, &stmt->declaration);
    break;
  case TD_STMT_TY_EXPR:
    DEBUG_CALL(expr, &stmt->expr);
    break;
  case TD_STMT_TY_COMPOUND:
    DEBUG_CALL(compoundstmt, &stmt->compound);
    break;
  case TD_STMT_TY_JUMP:
    DEBUG_CALL(jumpstmt, &stmt->jump);
    break;
  case TD_STMT_TY_SELECT:
    DEBUG_CALL(selectstmt, &stmt->select);
    break;
  case TD_STMT_TY_ITER:
    DEBUG_CALL(iterstmt, &stmt->iter);
    break;
  case TD_STMT_TY_LABELED:
    DEBUG_CALL(labeledstmt, &stmt->labeled);
    break;
  }

  UNINDENT();
}

DEBUG_FUNC(compoundstmt, compound_stmt) {
  TD_PRINTZ("COMPOUND STATEMENT: ");
  INDENT();

  for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
    DEBUG_CALL(stmt, &compound_stmt->stmts[i]);
  }

  UNINDENT();
}

DEBUG_FUNC(param, param) {
  TD_PRINT_SAMELINE_Z("PARAM ");
  DEBUG_CALL(var_ty, &param->var_ty);
  TD_PRINT_STR(param->name);
}

UNUSED DEBUG_FUNC(paramlist, param_list) {
  for (size_t i = 0; i < param_list->num_params; i++) {
    DEBUG_CALL(param, &param_list->params[i]);
  }
}

DEBUG_FUNC(var_declaration, var_declaration) {
  TD_PRINTZ("VAR DECLARATION");
  INDENT();

  DEBUG_CALL(var, &var_declaration->var);

  UNINDENT();
  TD_PRINTZ("TYPE");
  DEBUG_CALL(var_ty, &var_declaration->var_ty);
  INDENT();

  if (var_declaration->init) {
    UNINDENT();
    TD_PRINTZ("INDENT");
    DEBUG_CALL(init, var_declaration->init);
    INDENT();
  }

  UNINDENT();
}

DEBUG_FUNC(funcdef, func_def) {
  TD_PRINTZ("FUNCTION DEFINITION ");

  TD_PRINTZ("STORAGE CLASS");
  INDENT();
  DEBUG_CALL(storage_class_specifier, &func_def->storage_class_specifier);
  UNINDENT();

  DEBUG_CALL(var_declaration, &func_def->var_declaration);

  TD_PRINTZ("BODY");
  DEBUG_CALL(stmt, &func_def->body);
}

DEBUG_FUNC(external_declaration, external_declaration) {
  switch (external_declaration->ty) {
  case AST_EXTERNAL_DECLARATION_TY_DECLARATION:
    DEBUG_CALL(declaration, &external_declaration->declaration);
    break;
  case AST_EXTERNAL_DECLARATION_TY_FUNC_DEF:
    DEBUG_CALL(funcdef, &external_declaration->func_def);
    break;
  }
}

void debug_print_td(struct typechk *tchk, struct hashtbl *log_symbols,
                    struct td_translationunit *translation_unit) {
  struct td_printstate state_ = {.indent = 0, .tchk = tchk};

  struct td_printstate *state = &state_;

  TD_PRINTZ("PRINTING TD");

  for (size_t i = 0; i < translation_unit->num_external_declarations; i++) {
    struct td_external_declaration *decl =
        &translation_unit->external_declarations[i];

    if (log_symbols) {
      // if we have `size_t a, b, c` and `--log-sym a`, it is okay to print
      // them all
      switch (decl->ty) {
      case TD_EXTERNAL_DECLARATION_TY_DECLARATION: {
        bool found = false;
        for (size_t j = 0; j < decl->declaration.num_var_declarations; j++) {
          if (hashtbl_lookup(
                  log_symbols,
                  &decl->declaration.var_declarations[j].var.identifier.str)) {
            found = true;
            break;
          }
        }

        if (!found) {
          continue;
        }
        break;
      }
      case TD_EXTERNAL_DECLARATION_TY_FUNC_DEF:
        if (!hashtbl_lookup(
                log_symbols,
                &decl->func_def.var_declaration.var.identifier.str)) {
          continue;
        }
        break;
      }
    }

    DEBUG_CALL(external_declaration, decl);
  }

  TD_PRINTZ("");
}
