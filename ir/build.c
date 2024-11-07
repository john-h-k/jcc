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

// #include <math.h>

// // break/continues will add an entry into the jumps vector
// // and then at the end of the loop these will be traversed and fixed to point to
// // the correct basicblock the special value IR_JUMP_TY_NEW_LOOP indicates the
// // start of a loop
// enum ir_jump_ty {
//   IR_JUMP_TY_NEW_LOOP,

//   IR_JUMP_TY_BREAK,
//   IR_JUMP_TY_CONTINUE
// };

// struct ir_jump {
//   enum ir_jump_ty ty;

//   struct ir_basicblock *basicblock;
// };

// enum ir_case_ty { IR_CASE_TY_CASE, IR_CASE_TY_DEFAULT };

// struct ir_case {
//   enum ir_case_ty ty;

//   struct ir_split_case split_case;
// };

// // linked list of label -> bb mappings
// struct ir_label {
//   const char *name;
//   struct ir_basicblock *basicblock;

//   struct ir_label *succ;
// };

// struct ir_func_builder {
//   struct parser *parser;

//   struct var_refs *var_refs;
//   struct var_refs *global_var_refs;

//   struct ir_label *labels;
//   struct ir_func *func;

//   struct vector *jumps;
//   struct vector *switch_cases;
// };

// struct ir_label *add_label(struct ir_func_builder *irb, const char *name,
//                            struct ir_basicblock *basicblock) {
//   struct ir_label *label = arena_alloc(irb->func->arena, sizeof(*label));

//   label->name = name;
//   label->basicblock = basicblock;
//   label->succ = irb->labels;

//   irb->labels = label;

//   return label;
// }

// struct var_key get_var_key(struct parser *parser, const struct td_var *var,
//                            struct ir_basicblock *basicblock) {
//   const char *name = identifier_str(parser, &var->identifier);
//   return (struct var_key){name, var->scope, .basicblock = basicblock};
// }

// void get_var_ref(struct ir_func_builder *irb, struct ir_basicblock *basicblock,
//                  struct td_var *var, struct var_key *key,
//                  struct var_ref **ref) {
//   debug_assert(var->ty != TD_VAR_TY_ENUM_CNST,
//                "can't get var ref for enum cnst");

//   *ref = NULL;

//   // this is when we are _reading_ from the var
//   *key = get_var_key(irb->parser, var, basicblock);

//   *ref = var_refs_get(irb->var_refs, key);

//   if (*ref) {
//     return;
//   }

//   *ref = var_refs_get(irb->var_refs, key);
//   if (*ref && (*ref)->op->lcl) {
//     return;
//   }

//   *ref = var_refs_get(irb->global_var_refs, key);
// }

// bool var_ty_eq(struct ir_func *irb, const struct ir_op_var_ty *l,
//                const struct ir_op_var_ty *r) {
//   if (l == r) {
//     return true;
//   }

//   if (l->ty != r->ty) {
//     return false;
//   }

//   switch (l->ty) {
//   case IR_OP_VAR_TY_TY_NONE:
//     return r->ty == IR_OP_VAR_TY_TY_NONE;
//   case IR_OP_VAR_TY_TY_PRIMITIVE:
//     return l->primitive == r->primitive;
//   case IR_OP_VAR_TY_TY_VARIADIC:
//     return r->ty == IR_OP_VAR_TY_TY_VARIADIC;
//   case IR_OP_VAR_TY_TY_POINTER:
//     return var_ty_eq(irb, l->pointer.underlying, r->pointer.underlying);
//   case IR_OP_VAR_TY_TY_ARRAY:
//     return l->array.num_elements == r->array.num_elements &&
//            var_ty_eq(irb, l->array.underlying, r->array.underlying);
//   case IR_OP_VAR_TY_TY_FUNC:
//     if (!var_ty_eq(irb, l->func.ret_ty, r->func.ret_ty)) {
//       return false;
//     }
//     if (l->func.num_params != r->func.num_params) {
//       return false;
//     }
//     for (size_t i = 0; i < l->func.num_params; i++) {
//       if (!var_ty_eq(irb, &l->func.params[i], &r->func.params[i])) {
//         return false;
//       }
//     }

//     return true;
//   case IR_OP_VAR_TY_TY_STRUCT: {
//     if (l->struct_ty.num_fields != r->struct_ty.num_fields) {
//       return false;
//     }

//     struct ir_var_ty_info l_info = var_ty_info(irb->unit, l);
//     struct ir_var_ty_info r_info = var_ty_info(irb->unit, r);

//     // currently we do not have custom alignment/size but it is possible
//     if (l_info.size != r_info.size || l_info.alignment != r_info.alignment) {
//       return false;
//     }

//     for (size_t i = 0; i < l->struct_ty.num_fields; i++) {
//       if (!var_ty_eq(irb, &l->struct_ty.fields[i], &r->struct_ty.fields[i])) {
//         return false;
//       }
//     }

//     return true;
//   }
//   case IR_OP_VAR_TY_TY_UNION: {
//     if (l->union_ty.num_fields != r->union_ty.num_fields) {
//       return false;
//     }

//     struct ir_var_ty_info l_info = var_ty_info(irb->unit, l);
//     struct ir_var_ty_info r_info = var_ty_info(irb->unit, r);

//     // currently we do not have custom alignment/size but it is possible
//     if (l_info.size != r_info.size || l_info.alignment != r_info.alignment) {
//       return false;
//     }

//     for (size_t i = 0; i < l->union_ty.num_fields; i++) {
//       if (!var_ty_eq(irb, &l->union_ty.fields[i], &r->union_ty.fields[i])) {
//         return false;
//       }
//     }

//     return true;
//   }
//   }

//   unreachable("var_ty_eq");
// }

// bool var_ty_needs_cast_op(struct ir_func_builder *irb,
//                           const struct ir_op_var_ty *l,
//                           const struct ir_op_var_ty *r) {
//   // note: `l` is TO, `r` is FROM, (as this is in the context of `l <- r`)

//   if (l->ty == IR_OP_VAR_TY_TY_NONE) {
//     // void casts are nop
//     return false;
//   }

//   if (var_ty_is_aggregate(l) && var_ty_is_aggregate(r)) {
//     // casting between these could require conversion,
//     // but never a cast op
//     return false;
//   }

//   if (var_ty_eq(irb->func, l, r)) {
//     return false;
//   }

//   if ((l->ty == IR_OP_VAR_TY_TY_FUNC && r->ty == IR_OP_VAR_TY_TY_POINTER) ||
//       (r->ty == IR_OP_VAR_TY_TY_FUNC && l->ty == IR_OP_VAR_TY_TY_POINTER)) {
//     return false;
//   }

//   if ((l->ty == IR_OP_VAR_TY_TY_POINTER || l->ty == IR_OP_VAR_TY_TY_ARRAY) &&
//       (r->ty == IR_OP_VAR_TY_TY_POINTER || r->ty == IR_OP_VAR_TY_TY_ARRAY)) {
//     // pointers/arrays need no cast instr
//     return false;
//   }

//   // TODO: hardcodes pointer size
//   if (((l->ty == IR_OP_VAR_TY_TY_PRIMITIVE &&
//         l->primitive == IR_OP_VAR_PRIMITIVE_TY_I64) ||
//        l->ty == IR_OP_VAR_TY_TY_POINTER) &&
//       ((r->ty == IR_OP_VAR_TY_TY_PRIMITIVE &&
//         r->primitive == IR_OP_VAR_PRIMITIVE_TY_I64) ||
//        r->ty == IR_OP_VAR_TY_TY_POINTER)) {
//     // same size int -> pointer needs no cast
//     return false;
//   }

//   return true;
// }

// enum ir_op_var_primitive_ty var_ty_for_well_known_ty(enum well_known_ty wkt) {
//   switch (wkt) {
//   case WELL_KNOWN_TY_SIGNED_CHAR:
//   case WELL_KNOWN_TY_UNSIGNED_CHAR:
//     return IR_OP_VAR_PRIMITIVE_TY_I8;
//   case WELL_KNOWN_TY_SIGNED_SHORT:
//   case WELL_KNOWN_TY_UNSIGNED_SHORT:
//     return IR_OP_VAR_PRIMITIVE_TY_I16;
//   case WELL_KNOWN_TY_SIGNED_INT:
//   case WELL_KNOWN_TY_UNSIGNED_INT:
//     return IR_OP_VAR_PRIMITIVE_TY_I32;
//   case WELL_KNOWN_TY_SIGNED_LONG:
//   case WELL_KNOWN_TY_UNSIGNED_LONG:
//     return IR_OP_VAR_PRIMITIVE_TY_I64;
//   case WELL_KNOWN_TY_SIGNED_LONG_LONG:
//   case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
//     return IR_OP_VAR_PRIMITIVE_TY_I64;
//   case WELL_KNOWN_TY_HALF:
//     return IR_OP_VAR_PRIMITIVE_TY_F16;
//   case WELL_KNOWN_TY_FLOAT:
//     return IR_OP_VAR_PRIMITIVE_TY_F32;
//   case WELL_KNOWN_TY_DOUBLE:
//   case WELL_KNOWN_TY_LONG_DOUBLE:
//     return IR_OP_VAR_PRIMITIVE_TY_F64;
//   }
// }

// struct ir_op_var_ty var_ty_for_td_var_ty(struct ir_unit *iru,
//                                          const struct td_var_ty *var_ty) {
//   switch (var_ty->ty) {
//   case TD_VAR_TY_TY_UNKNOWN:
//     bug("shouldn't reach IR gen with unresolved type");
//   case TD_VAR_TY_TY_TAGGED: {
//     return var_ty_for_td_var_ty(iru, var_ty);
//   }
//   case TD_VAR_TY_TY_AGGREGATE: {
//     struct td_ty_aggregate aggregate = var_ty->aggregate;

//     struct ir_op_var_ty ty;
//     switch (aggregate.ty) {
//     case TD_TY_AGGREGATE_TY_STRUCT:
//       ty.ty = IR_OP_VAR_TY_TY_STRUCT;
//       ty.struct_ty.num_fields = aggregate.num_field_var_tys;
//       ty.struct_ty.fields = arena_alloc(
//           iru->arena, sizeof(struct ir_op_var_ty) * ty.struct_ty.num_fields);

//       for (size_t i = 0; i < ty.struct_ty.num_fields; i++) {
//         // handle nested types

//         ty.struct_ty.fields[i] =
//             var_ty_for_td_var_ty(iru, aggregate.field_var_tys[i].var_ty);
//       }
//       break;
//     case TD_TY_AGGREGATE_TY_UNION:
//       ty.ty = IR_OP_VAR_TY_TY_UNION;
//       ty.union_ty.num_fields = aggregate.num_field_var_tys;
//       ty.union_ty.fields = arena_alloc(iru->arena, sizeof(struct ir_op_var_ty) *
//                                                        ty.union_ty.num_fields);

//       for (size_t i = 0; i < ty.union_ty.num_fields; i++) {
//         // handle nested types

//         ty.struct_ty.fields[i] =
//             var_ty_for_td_var_ty(iru, aggregate.field_var_tys[i].var_ty);
//       }
//       break;
//     }

//     return ty;
//   }
//   case TD_VAR_TY_TY_VOID:
//     return IR_OP_VAR_TY_NONE;
//   case TD_VAR_TY_TY_VARIADIC:
//     return IR_OP_VAR_TY_VARIADIC;
//   case TD_VAR_TY_TY_WELL_KNOWN: {
//     struct ir_op_var_ty ty;
//     ty.ty = IR_OP_VAR_TY_TY_PRIMITIVE;
//     ty.primitive = var_ty_for_well_known_ty(var_ty->well_known);
//     return ty;
//   }
//   case TD_VAR_TY_TY_FUNC: {
//     bool variadic =
//         var_ty->func.num_params &&
//         var_ty->func.param_var_tys[var_ty->func.num_params - 1].ty ==
//             TD_VAR_TY_TY_VARIADIC;

//     struct ir_op_var_ty ty;
//     ty.ty = IR_OP_VAR_TY_TY_FUNC;
//     ty.func.ret_ty = arena_alloc(iru->arena, sizeof(*ty.func.ret_ty));
//     *ty.func.ret_ty = var_ty_for_td_var_ty(iru, var_ty->func.ret_var_ty);

//     // from IR onwards, variadic is no longer a param of the function but
//     // instead a flag
//     ty.func.num_params =
//         variadic ? var_ty->func.num_params - 1 : var_ty->func.num_params;
//     ty.func.params =
//         arena_alloc(iru->arena, sizeof(struct ir_op) * ty.func.num_params);

//     ty.func.flags = IR_OP_VAR_FUNC_TY_FLAG_NONE;
//     if (variadic) {
//       ty.func.flags |= IR_OP_VAR_FUNC_TY_FLAG_VARIADIC;
//     }

//     for (size_t i = 0; i < ty.func.num_params; i++) {
//       ty.func.params[i] =
//           var_ty_for_td_var_ty(iru, &var_ty->func.param_var_tys[i]);
//     }

//     return ty;
//   }
//   case TD_VAR_TY_TY_POINTER: {
//     struct ir_op_var_ty underlying;
//     if (var_ty->pointer.underlying->ty == TD_VAR_TY_TY_TAGGED) {
//       underlying = IR_OP_VAR_TY_NONE;
//     } else {
//       underlying = var_ty_for_td_var_ty(iru, var_ty->pointer.underlying);
//     }

//     return var_ty_make_pointer(iru, &underlying);
//   }
//   case TD_VAR_TY_TY_ARRAY: {
//     struct ir_op_var_ty underlying =
//         var_ty_for_td_var_ty(iru, var_ty->array.underlying);

//     return var_ty_make_array(iru, &underlying, var_ty->array.size);
//   }
//   }
// }

// struct ir_op_var_ty
// var_ty_return_ty_for_td_var_ty(struct ir_func_builder *irb,
//                                const struct td_var_ty *ty_ref) {
//   invariant_assert(ty_ref->ty == TD_VAR_TY_TY_FUNC,
//                    "passed non-func to `return_ty_for_td_var_ty`");

//   struct ir_op_var_ty func_ty = var_ty_for_td_var_ty(irb->func->unit, ty_ref);
//   return *func_ty.func.ret_ty;
// }

// enum ir_op_cast_op_ty cast_ty_for_td_var_ty(struct ir_func_builder *irb,
//                                             const struct td_var_ty *from,
//                                             const struct td_var_ty *to) {
//   struct ir_op_var_ty from_var_ty = var_ty_for_td_var_ty(irb->func->unit, from);
//   struct ir_op_var_ty to_var_ty = var_ty_for_td_var_ty(irb->func->unit, to);

//   if (from_var_ty.ty == IR_OP_VAR_TY_TY_POINTER &&
//       to_var_ty.ty == IR_OP_VAR_TY_TY_POINTER) {
//     bug("cast between pointer types is implicit");
//   }

//   if (from_var_ty.ty == IR_OP_VAR_TY_TY_PRIMITIVE &&
//       to_var_ty.ty == IR_OP_VAR_TY_TY_POINTER) {
//     // primitive -> pointer
//     // TODO: hardcodes pointer size
//     if (from_var_ty.primitive == IR_OP_VAR_PRIMITIVE_TY_I64) {
//       bug("cast between primitive & pointer type of same size is implicit");
//     }

//     if (WKT_IS_SIGNED(from->well_known)) {
//       return IR_OP_CAST_OP_TY_SEXT;
//     } else {
//       return IR_OP_CAST_OP_TY_ZEXT;
//     }
//   }

//   if (from_var_ty.ty != IR_OP_VAR_TY_TY_PRIMITIVE ||
//       to_var_ty.ty != IR_OP_VAR_TY_TY_PRIMITIVE) {
//     todo("casts for non prims/pointers (from %d -> %d)", from_var_ty.ty,
//          to_var_ty.ty);
//   }

//   if (is_fp_ty(from) && is_fp_ty(to)) {
//     return IR_OP_CAST_OP_TY_CONV;
//   }

//   if (is_fp_ty(from) || is_fp_ty(to)) {
//     // one (but not both) is fp
//     // we need to generate `uconv`/`iconv` depending on the sign of the integral
//     // type

//     invariant_assert(from->ty == TD_VAR_TY_TY_WELL_KNOWN ||
//                          to->ty == TD_VAR_TY_TY_WELL_KNOWN,
//                      "other type must be an integer for float conversion");

//     bool is_signed = is_fp_ty(from) ? WKT_IS_SIGNED(to->well_known)
//                                     : WKT_IS_SIGNED(from->well_known);

//     return is_signed ? IR_OP_CAST_OP_TY_SCONV : IR_OP_CAST_OP_TY_UCONV;
//   }

//   if (to_var_ty.primitive < from_var_ty.primitive) {
//     return IR_OP_CAST_OP_TY_TRUNC;
//   } else {
//     invariant_assert(from_var_ty.primitive != to_var_ty.primitive,
//                      "cast not needed for types of same size");
//     if (WKT_IS_SIGNED(from->well_known)) {
//       return IR_OP_CAST_OP_TY_SEXT;
//     } else {
//       return IR_OP_CAST_OP_TY_ZEXT;
//     }
//   }
// }

// struct ir_op *build_ir_for_expr(struct ir_func_builder *irb,
//                                 struct ir_stmt **stmt, struct td_expr *expr,
//                                 const struct td_var_ty *td_var_ty);

// struct ir_op *insert_ir_for_cast(struct ir_func_builder *irb,
//                                  struct ir_stmt *stmt, struct ir_op *op,
//                                  const struct ir_op_var_ty *to,
//                                  enum ir_op_cast_op_ty ty) {
//   struct ir_op *cast = alloc_ir_op(irb->func, stmt);

//   cast->ty = IR_OP_TY_CAST_OP;
//   cast->var_ty = *to;
//   cast->cast_op.ty = ty;
//   cast->cast_op.value = op;

//   return cast;
// }

// struct ir_op *alloc_binaryop(struct ir_func_builder *irb, struct ir_stmt *stmt,
//                              const struct td_var_ty *ty_ref,
//                              enum td_binary_op_ty ty, struct ir_op *lhs,
//                              struct ir_op *rhs) {
//   invariant_assert(lhs->var_ty.ty != IR_OP_VAR_TY_TY_ARRAY ||
//                        rhs->var_ty.ty != IR_OP_VAR_TY_TY_ARRAY,
//                    "array should have decayed to ptr");

//   struct ir_op_var_ty var_ty = var_ty_for_td_var_ty(irb->func->unit, ty_ref);

//   if (!td_binary_op_is_comparison(ty) &&
//       (lhs->var_ty.ty == IR_OP_VAR_TY_TY_POINTER ||
//        rhs->var_ty.ty == IR_OP_VAR_TY_TY_POINTER)) {
//     if (ty_ref->ty == TD_VAR_TY_TY_WELL_KNOWN) {
//       struct ir_op_var_ty *pointer_ty =
//           lhs->var_ty.ty == IR_OP_VAR_TY_TY_POINTER ? &lhs->var_ty
//                                                     : &rhs->var_ty;

//       // need to multiply rhs by the element size
//       struct ir_var_ty_info el_info =
//           var_ty_info(irb->func->unit, pointer_ty->pointer.underlying);

//       struct ir_op *el_size_op = alloc_ir_op(irb->func, stmt);
//       make_pointer_constant(irb->func->unit, el_size_op, el_info.size);

//       struct ir_op *diff = alloc_ir_op(irb->func, stmt);
//       diff->ty = IR_OP_TY_BINARY_OP;
//       diff->var_ty = var_ty;
//       diff->binary_op.ty = IR_OP_BINARY_OP_TY_SUB;
//       diff->binary_op.lhs = lhs;
//       diff->binary_op.rhs = rhs;

//       struct ir_op *op = alloc_ir_op(irb->func, stmt);
//       op->ty = IR_OP_TY_BINARY_OP;
//       op->var_ty = var_ty;
//       op->binary_op.ty = IR_OP_BINARY_OP_TY_SDIV;
//       op->binary_op.lhs = diff;
//       op->binary_op.rhs = el_size_op;

//       return op;
//     } else {
//       struct ir_op_var_ty pointer_ty =
//           var_ty_for_td_var_ty(irb->func->unit, ty_ref);

//       // need to multiply rhs by the element size
//       struct ir_var_ty_info el_info =
//           var_ty_info(irb->func->unit, pointer_ty.pointer.underlying);

//       struct ir_op *el_size_op = alloc_ir_op(irb->func, stmt);
//       make_pointer_constant(irb->func->unit, el_size_op, el_info.size);

//       struct ir_op *rhs_mul = alloc_ir_op(irb->func, stmt);
//       rhs_mul->ty = IR_OP_TY_BINARY_OP;
//       rhs_mul->var_ty = var_ty;
//       rhs_mul->binary_op.ty = IR_OP_BINARY_OP_TY_MUL;
//       rhs_mul->binary_op.lhs = el_size_op;
//       rhs_mul->binary_op.rhs = rhs;

//       struct ir_op *op = alloc_ir_op(irb->func, stmt);
//       op->ty = IR_OP_TY_BINARY_OP;
//       op->var_ty = var_ty;
//       op->binary_op.ty = ty == TD_BINARY_OP_TY_ADD ? IR_OP_BINARY_OP_TY_ADD
//                                                     : IR_OP_BINARY_OP_TY_SUB;
//       op->binary_op.lhs = lhs;
//       op->binary_op.rhs = rhs_mul;

//       return op;
//     }
//   }

//   struct ir_op *op = alloc_ir_op(irb->func, stmt);
//   op->ty = IR_OP_TY_BINARY_OP;
//   op->var_ty = var_ty;

//   struct ir_op_binary_op *b = &op->binary_op;

//   b->lhs = lhs;
//   b->rhs = rhs;

//   bool is_fp = var_ty_is_fp(&op->binary_op.lhs->var_ty);
//   debug_assert(is_fp == var_ty_is_fp(&op->binary_op.rhs->var_ty),
//                "type mismatch between lhs/rhs");

//   invariant_assert(
//       ty_ref->ty == TD_VAR_TY_TY_WELL_KNOWN ||
//           ty_ref->ty == TD_VAR_TY_TY_POINTER,
//       "non primitives/well-knowns/pointers cannot be used in binary "
//       "expression by point IR is reached!");

//   switch (ty) {
//   case TD_BINARY_OP_TY_LOGICAL_AND:
//   case TD_BINARY_OP_TY_LOGICAL_OR:
//     bug("logical and/or must be handled outside (as they need basicblock "
//         "adjustment)");
//     break;
//   case TD_BINARY_OP_TY_EQ:
//     b->ty = is_fp ? IR_OP_BINARY_OP_TY_FEQ : IR_OP_BINARY_OP_TY_EQ;
//     break;
//   case TD_BINARY_OP_TY_NEQ:
//     b->ty = is_fp ? IR_OP_BINARY_OP_TY_FNEQ : IR_OP_BINARY_OP_TY_NEQ;
//     break;
//   case TD_BINARY_OP_TY_GT:
//     if (is_fp) {
//       b->ty = IR_OP_BINARY_OP_TY_FGT;
//     } else if (WKT_IS_SIGNED(ty_ref->well_known)) {
//       b->ty = IR_OP_BINARY_OP_TY_SGT;
//     } else {
//       b->ty = IR_OP_BINARY_OP_TY_UGT;
//     }
//     break;
//   case TD_BINARY_OP_TY_GTEQ:
//     if (is_fp) {
//       b->ty = IR_OP_BINARY_OP_TY_FGTEQ;
//     } else if (WKT_IS_SIGNED(ty_ref->well_known)) {
//       b->ty = IR_OP_BINARY_OP_TY_SGTEQ;
//     } else {
//       b->ty = IR_OP_BINARY_OP_TY_UGTEQ;
//     }
//     break;
//   case TD_BINARY_OP_TY_LT:
//     if (is_fp) {
//       b->ty = IR_OP_BINARY_OP_TY_FLT;
//     } else if (WKT_IS_SIGNED(ty_ref->well_known)) {
//       b->ty = IR_OP_BINARY_OP_TY_SLT;
//     } else {
//       b->ty = IR_OP_BINARY_OP_TY_ULT;
//     }
//     break;
//   case TD_BINARY_OP_TY_LTEQ:
//     if (is_fp) {
//       b->ty = IR_OP_BINARY_OP_TY_FLTEQ;
//     } else if (WKT_IS_SIGNED(ty_ref->well_known)) {
//       b->ty = IR_OP_BINARY_OP_TY_SLTEQ;
//     } else {
//       b->ty = IR_OP_BINARY_OP_TY_ULTEQ;
//     }
//     break;
//   case TD_BINARY_OP_TY_RSHIFT:
//     if (WKT_IS_SIGNED(ty_ref->well_known)) {
//       b->ty = IR_OP_BINARY_OP_TY_SRSHIFT;
//     } else {
//       b->ty = IR_OP_BINARY_OP_TY_URSHIFT;
//     }
//     break;
//   case TD_BINARY_OP_TY_LSHIFT:
//     b->ty = IR_OP_BINARY_OP_TY_LSHIFT;
//     break;
//   case TD_BINARY_OP_TY_AND:
//     b->ty = IR_OP_BINARY_OP_TY_AND;
//     break;
//   case TD_BINARY_OP_TY_OR:
//     b->ty = IR_OP_BINARY_OP_TY_OR;
//     break;
//   case TD_BINARY_OP_TY_XOR:
//     b->ty = IR_OP_BINARY_OP_TY_XOR;
//     break;
//   case TD_BINARY_OP_TY_ADD:
//     b->ty = is_fp ? IR_OP_BINARY_OP_TY_FADD : IR_OP_BINARY_OP_TY_ADD;
//     break;
//   case TD_BINARY_OP_TY_SUB:
//     b->ty = is_fp ? IR_OP_BINARY_OP_TY_FSUB : IR_OP_BINARY_OP_TY_SUB;
//     break;
//   case TD_BINARY_OP_TY_MUL:
//     b->ty = is_fp ? IR_OP_BINARY_OP_TY_FMUL : IR_OP_BINARY_OP_TY_MUL;
//     break;
//   case TD_BINARY_OP_TY_DIV:
//     if (is_fp) {
//       b->ty = IR_OP_BINARY_OP_TY_FDIV;
//     } else if (WKT_IS_SIGNED(ty_ref->well_known)) {
//       b->ty = IR_OP_BINARY_OP_TY_SDIV;
//     } else {
//       b->ty = IR_OP_BINARY_OP_TY_UDIV;
//     }
//     break;
//   case TD_BINARY_OP_TY_QUOT:
//     if (WKT_IS_SIGNED(ty_ref->well_known)) {
//       b->ty = IR_OP_BINARY_OP_TY_SQUOT;
//     } else {
//       b->ty = IR_OP_BINARY_OP_TY_UQUOT;
//     }
//     break;
//   }

//   return op;
// }

// struct ir_op *build_ir_for_array_address(struct ir_func_builder *irb,
//                                          struct ir_stmt **stmt,
//                                          struct td_expr *lhs_expr,
//                                          struct td_expr *rhs_expr);

// struct ir_op *build_ir_for_member_address(struct ir_func_builder *irb,
//                                           struct ir_stmt **stmt,
//                                           struct td_expr *lhs_expr,
//                                           const struct token *member);

// struct ir_op *build_ir_for_pointer_address(struct ir_func_builder *irb,
//                                            struct ir_stmt **stmt,
//                                            struct td_expr *lhs_expr,
//                                            const struct token *member);

// struct ir_op *build_ir_for_addressof_var(struct ir_func_builder *irb,
//                                          struct ir_stmt **stmt,
//                                          struct td_var *var) {
//   struct var_key key;
//   struct var_ref *ref;
//   get_var_ref(irb, NULL, var, &key, &ref);

//   struct ir_op_var_ty underlying_var_ty;
//   switch (ref->ty) {
//   case VAR_REF_TY_SSA:
//     underlying_var_ty = ref->op->var_ty;
//     break;
//   case VAR_REF_TY_LCL:
//     underlying_var_ty = ref->op->var_ty;
//     break;
//   case VAR_REF_TY_GLB:
//     underlying_var_ty = ref->glb->var_ty;
//     break;
//   }

//   struct ir_op_var_ty var_ty;
//   if (underlying_var_ty.ty == IR_OP_VAR_TY_TY_ARRAY) {
//     // decay T[] to T* (rather than to T[]*)
//     var_ty = var_ty_make_pointer(irb->func->unit,
//                                  underlying_var_ty.array.underlying);
//   } else if (underlying_var_ty.ty == IR_OP_VAR_TY_TY_FUNC) {
//     // `func_ptr = func` is legal without address
//     var_ty = var_ty_make_pointer(irb->func->unit, &underlying_var_ty);
//   } else {
//     var_ty = var_ty_make_pointer(irb->func->unit, &underlying_var_ty);
//   }

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_ADDR;

//   switch (ref->ty) {
//   case VAR_REF_TY_SSA:
//     spill_op(irb->func, ref->op);
//     ref->ty = VAR_REF_TY_LCL;

//     op->var_ty = var_ty;
//     op->addr =
//         (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = ref->op->lcl};
//     break;
//   case VAR_REF_TY_LCL:
//     op->var_ty = var_ty;
//     op->addr =
//         (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = ref->op->lcl};
//     break;
//   case VAR_REF_TY_GLB:
//     op->var_ty = var_ty;
//     op->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = ref->glb};
//     break;
//   }

//   return op;
// }

// struct ir_op *build_ir_for_addressof(struct ir_func_builder *irb,
//                                      struct ir_stmt **stmt,
//                                      struct td_expr *expr) {
//   // address of does not actually "read" its underlying expression
//   // so we do not build the expression

//   switch (expr->ty) {
//   case TD_EXPR_TY_ARRAYACCESS: {
//     return build_ir_for_array_address(irb, stmt, expr->array_access.lhs,
//                                       expr->array_access.rhs);
//   }
//   case TD_EXPR_TY_MEMBERACCESS: {
//     return build_ir_for_member_address(irb, stmt, expr->member_access.lhs,
//                                        &expr->member_access.member);
//   }
//   case TD_EXPR_TY_POINTERACCESS: {
//     return build_ir_for_pointer_address(irb, stmt, expr->pointer_access.lhs,
//                                         &expr->pointer_access.member);
//   }
//   case TD_EXPR_TY_COMPOUNDEXPR: {
//     return build_ir_for_addressof(
//         irb, stmt,
//         &expr->compound_expr.exprs[expr->compound_expr.num_exprs - 1]);
//   }
//   default:
//     break;
//   }

//   if (expr->ty == TD_EXPR_TY_UNARY_OP &&
//       expr->unary_op.ty == TD_UNARY_OP_TY_INDIRECTION) {
//     // &*, so cancel
//     return build_ir_for_expr(irb, stmt, expr->unary_op.expr, &expr->var_ty);
//   }

//   if (expr->ty != TD_EXPR_TY_VAR) {
//     todo("unknown type for addressof");
//   }

//   return build_ir_for_addressof_var(irb, stmt, &expr->var);
// }

// struct ir_op *build_ir_for_assg(struct ir_func_builder *irb,
//                                 struct ir_stmt **stmt, struct td_assg *assg);

// struct ir_op *build_ir_for_unaryop(struct ir_func_builder *irb,
//                                    struct ir_stmt **stmt,
//                                    struct td_unary_op *unary_op) {
//   if (unary_op->ty == TD_UNARY_OP_TY_ADDRESSOF) {
//     return build_ir_for_addressof(irb, stmt, unary_op->expr);
//   }

//   struct ir_op *expr =
//       build_ir_for_expr(irb, stmt, unary_op->expr, &unary_op->expr->var_ty);
//   struct ir_op_var_ty var_ty =
//       var_ty_for_td_var_ty(irb->func->unit, &unary_op->var_ty);

//   if (unary_op->ty == TD_UNARY_OP_TY_INDIRECTION) {
//     // does not generate a unary op instead generates a LOAD_ADDR
//     struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//     op->ty = IR_OP_TY_LOAD_ADDR;
//     op->var_ty = var_ty;
//     op->load_addr = (struct ir_op_load_addr){.addr = expr};

//     return op;
//   }

//   switch (unary_op->ty) {
//   case TD_UNARY_OP_TY_PREFIX_DEC:
//   case TD_UNARY_OP_TY_PREFIX_INC: {
//     enum td_binary_op_ty binary_op_ty =
//         unary_op->ty == TD_UNARY_OP_TY_PREFIX_INC ? TD_BINARY_OP_TY_ADD
//                                                    : TD_BINARY_OP_TY_SUB;
//     struct td_var_ty cnst_ty = (struct td_var_ty){
//         .ty = TD_VAR_TY_TY_WELL_KNOWN, .well_known = WELL_KNOWN_TY_SIGNED_INT};

//     struct td_expr one = {
//         .ty = TD_EXPR_TY_CNST,
//         .var_ty = cnst_ty,
//         .cnst = (struct td_cnst){.cnst_ty = cnst_ty, .int_value = 1}};

//     struct td_assg td_assg = {
//         .ty = td_ASSG_TY_COMPOUNDASSG,
//         .var_ty = unary_op->var_ty,
//         .expr = &one,
//         .assignee = unary_op->expr,
//         .compound_assg = (struct td_assg_compound_assg){
//             .binary_op_ty = binary_op_ty,
//             .intermediate_var_ty = unary_op->var_ty}};

//     return build_ir_for_assg(irb, stmt, &td_assg);
//   }
//   case TD_UNARY_OP_TY_POSTFIX_INC:
//   case TD_UNARY_OP_TY_POSTFIX_DEC: {
//     enum td_binary_op_ty binary_op_ty =
//         unary_op->ty == TD_UNARY_OP_TY_POSTFIX_INC ? TD_BINARY_OP_TY_ADD
//                                                     : TD_BINARY_OP_TY_SUB;
//     struct td_var_ty cnst_ty = (struct td_var_ty){
//         .ty = TD_VAR_TY_TY_WELL_KNOWN, .well_known = WELL_KNOWN_TY_SIGNED_INT};

//     struct td_expr one = {
//         .ty = TD_EXPR_TY_CNST,
//         .var_ty = cnst_ty,
//         .cnst = (struct td_cnst){.cnst_ty = cnst_ty, .int_value = 1}};

//     struct td_assg td_assg = {
//         .ty = td_ASSG_TY_COMPOUNDASSG,
//         .var_ty = unary_op->var_ty,
//         .expr = &one,
//         .assignee = unary_op->expr,
//         .compound_assg = (struct td_assg_compound_assg){
//             .binary_op_ty = binary_op_ty,
//             .intermediate_var_ty = unary_op->var_ty}};

//     build_ir_for_assg(irb, stmt, &td_assg);

//     return expr;
//   }
//   case TD_UNARY_OP_TY_PLUS:
//     // no work needed, build_expr will handle type conversion
//     return expr;
//   case TD_UNARY_OP_TY_SIZEOF:
//   case TD_UNARY_OP_TY_ALIGNOF:
//     todo("sizeof/alignof build (will need different node as they take types "
//          "not exprs)");
//     break;
//   case TD_UNARY_OP_TY_CAST:
//     if (var_ty_needs_cast_op(irb, &var_ty, &expr->var_ty)) {
//       return insert_ir_for_cast(irb, *stmt, expr, &var_ty,
//                                 cast_ty_for_td_var_ty(irb,
//                                                       &unary_op->expr->var_ty,
//                                                       &unary_op->var_ty));
//     } else {
//       expr->var_ty = var_ty_for_td_var_ty(irb->func->unit, &unary_op->var_ty);
//       return expr;
//     }
//   default:
//     break;
//   }

//   enum ir_op_unary_op_ty unary_op_ty;
//   switch (unary_op->ty) {
//   case TD_UNARY_OP_TY_MINUS:
//     unary_op_ty = IR_OP_UNARY_OP_TY_NEG;
//     break;
//   case TD_UNARY_OP_TY_LOGICAL_NOT:
//     unary_op_ty = IR_OP_UNARY_OP_TY_LOGICAL_NOT;
//     break;
//   case TD_UNARY_OP_TY_NOT:
//     unary_op_ty = IR_OP_UNARY_OP_TY_NOT;
//     break;
//   default:
//     bug("unexpected unary_op_ty in `%s`", __func__);
//     break;
//   }

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_UNARY_OP;
//   op->var_ty = var_ty;
//   op->unary_op.ty = unary_op_ty;
//   op->unary_op.value = expr;

//   return op;
// }

// struct ir_op *build_ir_for_binaryop(struct ir_func_builder *irb,
//                                     struct ir_stmt **stmt,
//                                     struct td_binary_op *binary_op,
//                                     struct td_var_ty *ty_ref) {
//   struct ir_op *lhs = build_ir_for_expr(irb, stmt, binary_op->lhs,
//                                         &binary_op->intermediate_var_ty);

//   if (binary_op->ty == TD_BINARY_OP_TY_LOGICAL_AND ||
//       binary_op->ty == TD_BINARY_OP_TY_LOGICAL_OR) {
//     struct ir_basicblock *entry_bb = (*stmt)->basicblock;
//     struct ir_basicblock *rhs_bb = alloc_ir_basicblock(irb->func);
//     struct ir_basicblock *end_bb = alloc_ir_basicblock(irb->func);

//     if (binary_op->ty == TD_BINARY_OP_TY_LOGICAL_AND) {
//       make_basicblock_split(irb->func, entry_bb, rhs_bb, end_bb);
//     } else {
//       make_basicblock_split(irb->func, entry_bb, end_bb, rhs_bb);
//     }

//     struct ir_stmt *entry_stmt = alloc_ir_stmt(irb->func, entry_bb);
//     struct ir_op *lhs_br = alloc_ir_op(irb->func, entry_stmt);
//     lhs_br->ty = IR_OP_TY_BR_COND;
//     lhs_br->var_ty = IR_OP_VAR_TY_NONE;
//     lhs_br->br_cond = (struct ir_op_br_cond){.cond = lhs};

//     struct ir_stmt *rhs_stmt = alloc_ir_stmt(irb->func, rhs_bb);
//     struct ir_op *rhs = build_ir_for_expr(irb, &rhs_stmt, binary_op->rhs,
//                                           &binary_op->intermediate_var_ty);

//     struct ir_op *rhs_br = alloc_ir_op(irb->func, rhs_stmt);
//     rhs_br->ty = IR_OP_TY_BR;
//     rhs_br->var_ty = IR_OP_VAR_TY_NONE;
//     make_basicblock_merge(irb->func, rhs_bb, end_bb);

//     struct ir_stmt *end_stmt = alloc_ir_stmt(irb->func, end_bb);
//     struct ir_op *phi = alloc_ir_op(irb->func, end_stmt);

//     phi->ty = IR_OP_TY_PHI;
//     phi->var_ty =
//         var_ty_for_td_var_ty(irb->func->unit, &binary_op->intermediate_var_ty);
//     phi->phi = (struct ir_op_phi){
//         .num_values = 2,
//         .values = arena_alloc(irb->func->arena, sizeof(struct ir_op *) * 2),
//         .var = NULL};

//     phi->phi.values[0] = lhs;
//     phi->phi.values[1] = rhs;

//     *stmt = phi->stmt;
//     return phi;
//   }

//   struct ir_op *rhs = build_ir_for_expr(irb, stmt, binary_op->rhs,
//                                         &binary_op->intermediate_var_ty);

//   return alloc_binaryop(irb, *stmt, ty_ref, binary_op->ty, lhs, rhs);
// }

// struct ir_op *build_ir_for_sizeof(struct ir_func_builder *irb,
//                                   struct ir_stmt **stmt,
//                                   struct td_sizeof *size_of) {
//   struct ir_op_var_ty var_ty;
//   switch (size_of->ty) {
//   case td_SIZEOF_TY_TYPE:
//     var_ty = var_ty_for_td_var_ty(irb->func->unit, &size_of->ty_ref);
//     break;
//   case td_SIZEOF_TY_EXPR:
//     var_ty = var_ty_for_td_var_ty(irb->func->unit, &size_of->expr->var_ty);
//     break;
//   }

//   struct ir_var_ty_info info = var_ty_info(irb->func->unit, &var_ty);

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_CNST;
//   op->var_ty = var_ty_for_pointer_size(irb->func->unit);
//   op->cnst =
//       (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = info.size};

//   return op;
// }

// struct ir_op *build_ir_for_alignof(struct ir_func_builder *irb,
//                                    struct ir_stmt **stmt,
//                                    struct td_alignof *align_of) {
//   struct ir_op_var_ty var_ty =
//       var_ty_for_td_var_ty(irb->func->unit, &align_of->ty_ref);

//   struct ir_var_ty_info info = var_ty_info(irb->func->unit, &var_ty);

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_CNST;
//   op->var_ty = var_ty_for_pointer_size(irb->func->unit);
//   op->cnst =
//       (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = info.alignment};

//   return op;
// }

// struct ir_op *build_ir_for_cnst(struct ir_func_builder *irb,
//                                 struct ir_stmt **stmt, struct td_cnst *cnst) {
//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);

//   if (cnst->cnst_ty.ty == TD_VAR_TY_TY_POINTER &&
//       cnst->cnst_ty.pointer.underlying->ty == TD_VAR_TY_TY_WELL_KNOWN) {
//     // either a string or array
//     // these are promoted to globals

//     struct ir_op_var_ty var_ty =
//         var_ty_for_td_var_ty(irb->func->unit, &cnst->cnst_ty);
//     struct ir_glb *glb = add_global(irb->func->unit, IR_GLB_TY_DATA, &var_ty,
//                                     IR_GLB_DEF_TY_DEFINED, NULL);
//     glb->var = arena_alloc(irb->func->arena, sizeof(*glb->var));
//     *glb->var = (struct ir_var){
//         .ty = IR_VAR_TY_STRING_LITERAL,
//         .value = {.var_ty = var_ty, .str_value = cnst->str_value}};

//     op->ty = IR_OP_TY_ADDR;
//     op->var_ty = var_ty;
//     op->addr = (struct ir_op_addr){.ty = IR_OP_ADDR_TY_GLB, .glb = glb};
//   } else if (is_fp_ty(&cnst->cnst_ty)) {
//     op->ty = IR_OP_TY_CNST;
//     op->var_ty = var_ty_for_td_var_ty(irb->func->unit, &cnst->cnst_ty);
//     op->cnst.ty = IR_OP_CNST_TY_FLT;
//     op->cnst.flt_value = cnst->flt_value;
//   } else if (is_integral_ty(&cnst->cnst_ty)) {
//     op->ty = IR_OP_TY_CNST;
//     op->var_ty = var_ty_for_td_var_ty(irb->func->unit, &cnst->cnst_ty);
//     op->cnst.ty = IR_OP_CNST_TY_INT;
//     op->cnst.int_value = cnst->int_value;
//   } else {
//     bug("unrecognised ty for cnst");
//   }

//   return op;
// }

// struct ir_op *build_ir_for_compoundexpr(struct ir_func_builder *irb,
//                                         struct ir_stmt **stmt,
//                                         struct td_compoundexpr *compound_expr,
//                                         const struct td_var_ty *td_var_ty) {
//   struct ir_op *op = NULL;
//   for (size_t i = 0; i < compound_expr->num_exprs; i++) {
//     op = build_ir_for_expr(irb, stmt, &compound_expr->exprs[i], td_var_ty);
//   }

//   return op;
// }

// struct ir_op *build_ir_for_ternary(struct ir_func_builder *irb,
//                                    struct ir_stmt **stmt,
//                                    struct td_ternary *ternary) {
//   struct ir_op *cond =
//       build_ir_for_expr(irb, stmt, ternary->cond, &ternary->cond->var_ty);
//   struct ir_op *br_cond = alloc_ir_op(irb->func, *stmt);
//   br_cond->ty = IR_OP_TY_BR_COND;
//   br_cond->var_ty = IR_OP_VAR_TY_NONE;
//   br_cond->br_cond = (struct ir_op_br_cond){.cond = cond};

//   struct ir_basicblock *pre_cond_bb = (*stmt)->basicblock;
//   struct ir_basicblock *true_bb = alloc_ir_basicblock(irb->func);
//   struct ir_basicblock *false_bb = alloc_ir_basicblock(irb->func);
//   struct ir_basicblock *end_bb = alloc_ir_basicblock(irb->func);

//   make_basicblock_split(irb->func, pre_cond_bb, true_bb, false_bb);
//   make_basicblock_merge(irb->func, true_bb, end_bb);
//   make_basicblock_merge(irb->func, false_bb, end_bb);

//   struct ir_stmt *true_stmt = alloc_ir_stmt(irb->func, true_bb);
//   struct ir_op *true_op =
//       build_ir_for_expr(irb, &true_stmt, ternary->true_expr, &ternary->var_ty);
//   struct ir_op *true_br = alloc_ir_op(irb->func, true_stmt);
//   true_br->ty = IR_OP_TY_BR;
//   true_br->var_ty = IR_OP_VAR_TY_NONE;

//   struct ir_stmt *false_stmt = alloc_ir_stmt(irb->func, false_bb);
//   struct ir_op *false_op = build_ir_for_expr(
//       irb, &false_stmt, ternary->false_expr, &ternary->var_ty);
//   struct ir_op *false_br = alloc_ir_op(irb->func, false_stmt);
//   false_br->ty = IR_OP_TY_BR;
//   false_br->var_ty = IR_OP_VAR_TY_NONE;

//   struct ir_stmt *end_stmt = alloc_ir_stmt(irb->func, end_bb);
//   struct ir_op *phi = alloc_ir_op(irb->func, end_stmt);
//   phi->ty = IR_OP_TY_PHI;
//   phi->var_ty = var_ty_for_td_var_ty(irb->func->unit, &ternary->var_ty);
//   phi->phi = (struct ir_op_phi){
//       .num_values = 2,
//       .values = arena_alloc(irb->func->arena, sizeof(struct ir_op *) * 2),
//       .var = NULL};

//   phi->phi.values[0] = true_op;
//   phi->phi.values[1] = false_op;

//   *stmt = end_stmt;
//   return phi;
// }

// struct ir_op *build_ir_for_var(struct ir_func_builder *irb,
//                                struct ir_stmt **stmt, struct td_var *var) {
//   // if `a` is an array, then reading `a` is actually `&a[0]`
//   // same with functions
//   if (var->var_ty.ty == TD_VAR_TY_TY_ARRAY ||
//       var->var_ty.ty == TD_VAR_TY_TY_FUNC) {
//     return build_ir_for_addressof_var(irb, stmt, var);
//   }

//   if (var->ty == TD_VAR_TY_ENUM_CNST) {
//     struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//     op->ty = IR_OP_TY_CNST;
//     op->var_ty = var_ty_for_td_var_ty(irb->func->unit, &var->var_ty);
//     op->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT,
//                                    .int_value = var->enum_cnst};

//     return op;
//   }

//   // this is when we are _reading_ from the var
//   struct var_key key;
//   struct var_ref *ref;
//   get_var_ref(irb, (*stmt)->basicblock, var, &key, &ref);

//   struct td_var_ty var_var_ty = var->var_ty;
//   struct ir_op_var_ty var_ty =
//       var_ty_for_td_var_ty(irb->func->unit, &var_var_ty);
//   if (ref) {
//     switch (ref->ty) {
//     case VAR_REF_TY_SSA:
//       return ref->op;
//     case VAR_REF_TY_LCL: {
//       debug_assert(ref->op->lcl, "VAR_REF_TY_LCL but op %zu had no lcl",
//                    ref->op->id);

//       struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//       op->ty = IR_OP_TY_LOAD_LCL;

//       if (var_ty.ty == IR_OP_VAR_TY_TY_ARRAY) {
//         // pointer decay
//         op->var_ty =
//             var_ty_make_pointer(irb->func->unit, var_ty.array.underlying);
//       } else {
//         op->var_ty = var_ty;
//       }
//       op->load_lcl = (struct ir_op_load_lcl){.lcl = ref->op->lcl};

//       return op;
//     }
//     case VAR_REF_TY_GLB: {
//       struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//       op->ty = IR_OP_TY_LOAD_GLB;

//       if (var_ty.ty == IR_OP_VAR_TY_TY_ARRAY) {
//         // pointer decay
//         op->var_ty =
//             var_ty_make_pointer(irb->func->unit, var_ty.array.underlying);
//       } else {
//         op->var_ty = var_ty;
//       }
//       op->load_glb = (struct ir_op_load_glb){.glb = ref->glb};

//       return op;
//     }
//     }
//   }

//   invariant_assert(var_var_ty.ty != TD_VAR_TY_TY_UNKNOWN,
//                    "can't have unknown tyref in phi lowering");

//   // we generate an empty phi and then after all blocks are built we insert the
//   // correct values
//   // all phis appear at the start of their bb as they execute ""
//   struct ir_op *phi;
//   struct ir_stmt *s = *stmt;
//   if (s->basicblock->first->first) {
//     phi = insert_before_ir_op(irb->func, s->basicblock->first->first,
//                               IR_OP_TY_PHI, var_ty);
//   } else {
//     phi = alloc_ir_op(irb->func, s->basicblock->first);
//     phi->ty = IR_OP_TY_PHI;
//     phi->var_ty = var_ty;
//   }

//   phi->phi.var = arena_alloc(irb->func->arena, sizeof(*phi->phi.var));
//   *phi->phi.var = *var;
//   phi->phi.values = NULL;
//   phi->phi.num_values = 0;

//   debug("creating phi %d for name=%s", phi->id,
//         identifier_str(irb->parser, &var->identifier));

//   key = get_var_key(irb->parser, var, (*stmt)->basicblock);
//   struct var_ref *new_ref = var_refs_add(irb->var_refs, &key, VAR_REF_TY_SSA);
//   new_ref->ty = VAR_REF_TY_SSA;
//   new_ref->op = phi;

//   return phi;
// }

// struct ir_op *build_ir_for_expr(struct ir_func_builder *irb,
//                                 struct ir_stmt **stmt, struct td_expr *expr,
//                                 const struct td_var_ty *td_var_ty);

// struct ir_op *build_ir_for_initlist(struct ir_func_builder *irb,
//                                     struct ir_stmt *stmt,
//                                     struct td_initlist *init_list,
//                                     const struct td_var_ty *td_var_ty) {
//   // init list is fundamentally untyped, so it needs to know its target type in
//   // order to be built

//   debug_assert(td_var_ty->ty == TD_VAR_TY_TY_ARRAY ||
//                    td_var_ty->ty == TD_VAR_TY_TY_AGGREGATE ||
//                    td_var_ty->ty == TD_VAR_TY_TY_TAGGED,
//                "init list only makes sense for arrays/structs/unions");
//   UNUSED_ARG(irb);
//   UNUSED_ARG(stmt);
//   UNUSED_ARG(init_list);
//   UNUSED_ARG(td_var_ty);
//   todo(__func__);
// }

// struct td_var_ty get_target_for_variadic(const struct td_var_ty *ty_ref) {
//   // we could do this in parsing, which would be more "elegant" (letting parser
//   // deal with all C typing concerns) but for now we do it here

//   // floats are promoted to doubles and types smaller than int are promoted to
//   // int
//   if (ty_ref->ty != TD_VAR_TY_TY_WELL_KNOWN) {
//     return *ty_ref;
//   }

//   if (ty_ref->well_known == WELL_KNOWN_TY_FLOAT) {
//     return (struct td_var_ty){
//         .ty = TD_VAR_TY_TY_WELL_KNOWN,
//         .well_known = WELL_KNOWN_TY_DOUBLE,
//     };
//   } else if (ty_ref->well_known < WELL_KNOWN_TY_SIGNED_INT) {
//     return (struct td_var_ty){
//         .ty = TD_VAR_TY_TY_WELL_KNOWN,
//         .well_known = WELL_KNOWN_TY_SIGNED_INT,
//     };
//   }

//   return *ty_ref;
// }

// struct ir_op *build_ir_for_call(struct ir_func_builder *irb,
//                                 struct ir_stmt **stmt, struct td_call *call) {
//   // need to generate args and target IR first to keep IR in order

//   struct ir_op **args = arena_alloc(
//       irb->func->arena, sizeof(struct ir_op *) * call->arg_list.num_args);

//   size_t num_non_variadic_args = call->var_ty.func.num_params;

//   struct ir_op_var_ty func_ty =
//       var_ty_for_td_var_ty(irb->func->unit, &call->target->var_ty);

//   // one level deref can occur
//   if (func_ty.ty == IR_OP_VAR_TY_TY_POINTER) {
//     func_ty = *func_ty.pointer.underlying;
//   }

//   debug_assert(func_ty.ty == IR_OP_VAR_TY_TY_FUNC,
//                "expected target to be func ty");

//   for (size_t i = 0; i < call->arg_list.num_args; i++) {
//     struct td_var_ty param_target_ty;
//     if (func_ty.func.flags & IR_OP_VAR_FUNC_TY_FLAG_VARIADIC) {
//       param_target_ty = get_target_for_variadic(&call->arg_list.args[i].var_ty);
//     } else {
//       param_target_ty = call->arg_list.args[i].var_ty;
//     }

//     args[i] =
//         build_ir_for_expr(irb, stmt, &call->arg_list.args[i], &param_target_ty);

//     if (i >= num_non_variadic_args) {
//       args[i]->flags |= IR_OP_FLAG_VARIADIC_PARAM;
//     }
//   }

//   // if the target is a function name, we want to take address
//   // else, we want to use value
//   struct ir_op *target;
//   if (call->target->var_ty.ty == TD_VAR_TY_TY_POINTER) {
//     target = build_ir_for_expr(irb, stmt, call->target, &call->target->var_ty);
//   } else {
//     target = build_ir_for_addressof(irb, stmt, call->target);
//   }

//   irb->func->flags |= IR_FUNC_FLAG_MAKES_CALL;
//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);

//   op->ty = IR_OP_TY_CALL;
//   op->var_ty = var_ty_for_td_var_ty(irb->func->unit, &call->var_ty);

//   op->call.func_ty = func_ty;
//   op->call.target = target;
//   op->call.num_args = call->arg_list.num_args;
//   op->call.args = args;

//   return op;
// }

// void var_assg_glb(struct ir_func_builder *irb, struct ir_stmt *stmt,
//                   struct ir_glb *glb, struct td_var *var) {

//   debug_assert(glb, "null glb in assignment!");

//   struct var_key key;
//   struct var_ref *ref;
//   get_var_ref(irb, stmt->basicblock, var, &key, &ref);

//   if (!ref) {
//     ref = var_refs_add(irb->var_refs, &key, VAR_REF_TY_GLB);
//   }

//   ref->glb = glb;
// }

// struct ir_op *var_assg(struct ir_func_builder *irb, struct ir_stmt *stmt,
//                        struct ir_op *op, struct td_var *var) {
//   debug_assert(op, "null expr in assignment!");

//   struct var_key key;
//   struct var_ref *ref;
//   get_var_ref(irb, stmt->basicblock, var, &key, &ref);

//   if (!ref) {
//     ref = var_refs_add(irb->var_refs, &key, VAR_REF_TY_SSA);
//   }

//   if (ref->ty == VAR_REF_TY_SSA) {
//     ref->op = op;
//     return op;
//   } else {
//     struct ir_op *ld = alloc_ir_op(irb->func, stmt);
//     ld->ty = IR_OP_TY_STORE_GLB;
//     ld->var_ty = var_ty_for_td_var_ty(irb->func->unit, &var->var_ty);
//     ld->store_glb = (struct ir_op_store_glb){.glb = ref->glb, .value = op};

//     // its okay that we use the thing assigned to the global, rather than
//     // reloading the global
//     return op;
//   }
// }

// void get_member_info(struct ir_unit *iru, const struct td_var_ty *ty_ref,
//                      const char *member_name, struct ir_op_var_ty *member_ty,
//                      size_t *member_idx, size_t *member_offset,
//                      struct td_var_ty *member_ty_ref) {
//   struct td_var_ty aggregate;
//   if (ty_ref->ty == TD_VAR_TY_TY_TAGGED) {
//     aggregate = tyref_get_defined(iru->parser, ty_ref);
//   } else {
//     aggregate = *ty_ref;
//   }

//   debug_assert(aggregate.ty == TD_VAR_TY_TY_AGGREGATE, "expected aggregate");

//   *member_ty = IR_OP_VAR_TY_NONE;

//   size_t idx;
//   if (!member_idx) {
//     member_idx = &idx;
//   }

//   *member_idx = 0;
//   for (; *member_idx < aggregate.aggregate.num_field_var_tys; (*member_idx)++) {
//     struct td_struct_field *field =
//         &aggregate.aggregate.field_var_tys[*member_idx];
//     if (strcmp(field->name, member_name) == 0) {
//       if (member_ty_ref) {
//         *member_ty_ref = *field->var_ty;
//       }

//       *member_ty = var_ty_for_td_var_ty(iru, field->var_ty);
//       if (member_ty->ty == IR_OP_VAR_TY_TY_ARRAY) {
//         // pointer decay
//         *member_ty = *member_ty->array.underlying;
//       }
//       break;
//     }
//   }

//   struct ir_op_var_ty ir_struct_ty = var_ty_for_td_var_ty(iru, &aggregate);
//   struct ir_var_ty_info info = var_ty_info(iru, &ir_struct_ty);

//   // offsets are null for a union
//   *member_offset = info.offsets ? info.offsets[*member_idx] : 0;
// }

// struct ir_op *build_ir_for_member_address_offset(
//     struct ir_func_builder *irb, struct ir_stmt **stmt,
//     const struct td_var_ty *struct_ty, const char *member_name,
//     struct ir_op_var_ty *member_ty) {

//   size_t member_offset;
//   size_t idx;
//   get_member_info(irb->func->unit, struct_ty, member_name, member_ty, &idx,
//                   &member_offset, NULL);

//   struct ir_op *offset = alloc_ir_op(irb->func, *stmt);
//   offset->ty = IR_OP_TY_CNST;
//   offset->var_ty = (struct ir_op_var_ty){
//       .ty = IR_OP_VAR_TY_TY_PRIMITIVE, .primitive = IR_OP_VAR_PRIMITIVE_TY_I64};
//   offset->cnst =
//       (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = member_offset};

//   return offset;
// }

// struct ir_op *build_ir_for_member_address(struct ir_func_builder *irb,
//                                           struct ir_stmt **stmt,
//                                           struct td_expr *lhs_expr,
//                                           const struct token *member) {
//   const char *member_name = identifier_str(irb->parser, member);

//   struct ir_op *lhs = build_ir_for_addressof(irb, stmt, lhs_expr);

//   struct ir_op_var_ty member_ty;
//   struct ir_op *rhs = build_ir_for_member_address_offset(
//       irb, stmt, &lhs_expr->var_ty, member_name, &member_ty);
//   struct ir_op_var_ty pointer_ty =
//       var_ty_make_pointer(irb->func->unit, &member_ty);

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_BINARY_OP;
//   op->var_ty = pointer_ty;
//   op->binary_op = (struct ir_op_binary_op){
//       .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = lhs, .rhs = rhs};

//   return op;
// }

// struct ir_op *build_ir_for_pointer_address(struct ir_func_builder *irb,
//                                            struct ir_stmt **stmt,
//                                            struct td_expr *lhs_expr,
//                                            const struct token *member) {
//   debug_assert(lhs_expr->var_ty.ty == TD_VAR_TY_TY_POINTER,
//                "makes no sense except on LHS pointer");
//   const char *member_name = identifier_str(irb->parser, member);

//   struct ir_op *lhs = build_ir_for_expr(irb, stmt, lhs_expr, NULL);

//   struct ir_op_var_ty member_ty;
//   struct ir_op *rhs = build_ir_for_member_address_offset(
//       irb, stmt, lhs_expr->var_ty.pointer.underlying, member_name, &member_ty);
//   struct ir_op_var_ty pointer_ty =
//       var_ty_make_pointer(irb->func->unit, &member_ty);

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_BINARY_OP;
//   op->var_ty = pointer_ty;
//   op->binary_op = (struct ir_op_binary_op){
//       .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = lhs, .rhs = rhs};

//   return op;
// }

// struct ir_op *build_ir_for_array_address(struct ir_func_builder *irb,
//                                          struct ir_stmt **stmt,
//                                          struct td_expr *lhs_expr,
//                                          struct td_expr *rhs_expr) {
//   struct td_var_ty pointer_ty;
//   struct ir_op *lhs;
//   if (lhs_expr->var_ty.ty == TD_VAR_TY_TY_ARRAY) {
//     // need to decay the type to pointer
//     struct td_var_ty *underlying = lhs_expr->var_ty.array.element;
//     lhs = build_ir_for_addressof(irb, stmt, lhs_expr);
//     pointer_ty = tyref_make_pointer(irb->parser, underlying);
//   } else {
//     lhs = build_ir_for_expr(irb, stmt, lhs_expr, &lhs_expr->var_ty);
//     pointer_ty = lhs_expr->var_ty;
//   }

//   // need to promote rhs to pointer size int
//   debug_assert(rhs_expr->var_ty.ty == TD_VAR_TY_TY_WELL_KNOWN,
//                "expected well-known ty rhs");
//   struct td_var_ty pointer_size_int =
//       tyref_pointer_sized_int(irb->parser, false);
//   struct ir_op *rhs = build_ir_for_expr(irb, stmt, rhs_expr, &pointer_size_int);

//   return alloc_binaryop(irb, *stmt, &pointer_ty, TD_BINARY_OP_TY_ADD, lhs,
//                         rhs);
// }

// struct ir_op *build_ir_for_assg(struct ir_func_builder *irb,
//                                 struct ir_stmt **stmt, struct td_assg *assg) {

//   struct ir_op *value;
//   switch (assg->ty) {
//   case td_ASSG_TY_SIMPLEASSG:
//     value = build_ir_for_expr(irb, stmt, assg->expr, &assg->var_ty);
//     break;
//   case td_ASSG_TY_COMPOUNDASSG: {
//     struct ir_op *assignee = build_ir_for_expr(
//         irb, stmt, assg->assignee, &assg->compound_assg.intermediate_var_ty);

//     struct ir_op *rhs = build_ir_for_expr(
//         irb, stmt, assg->expr, &assg->compound_assg.intermediate_var_ty);

//     struct ir_op *result =
//         alloc_binaryop(irb, *stmt, &assg->compound_assg.intermediate_var_ty,
//                        assg->compound_assg.binary_op_ty, assignee, rhs);

//     struct ir_op_var_ty assg_var_ty =
//         var_ty_for_td_var_ty(irb->func->unit, &assg->var_ty);
//     if (var_ty_needs_cast_op(irb, &result->var_ty, &assg_var_ty)) {
//       value = insert_ir_for_cast(
//           irb, *stmt, result, &assg_var_ty,
//           cast_ty_for_td_var_ty(irb, &assg->compound_assg.intermediate_var_ty,
//                                 &assg->var_ty));
//     } else {
//       value = result;
//     }
//   }
//   }

//   struct ir_op *address = NULL;
//   switch (assg->assignee->ty) {
//   case TD_EXPR_TY_VAR:
//     return var_assg(irb, *stmt, value, &assg->assignee->var);
//   case TD_EXPR_TY_ARRAYACCESS: {
//     struct td_arrayaccess *access = &assg->assignee->array_access;
//     address = build_ir_for_array_address(irb, stmt, access->lhs, access->rhs);
//     break;
//   }
//   case TD_EXPR_TY_MEMBERACCESS: {
//     struct td_memberaccess *access = &assg->assignee->member_access;
//     address =
//         build_ir_for_member_address(irb, stmt, access->lhs, &access->member);

//     break;
//   }
//   case TD_EXPR_TY_POINTERACCESS: {
//     struct td_pointeraccess *access = &assg->assignee->pointer_access;
//     address =
//         build_ir_for_pointer_address(irb, stmt, access->lhs, &access->member);
//     break;
//   }
//   case TD_EXPR_TY_UNARY_OP: {
//     if (assg->assignee->unary_op.ty == TD_UNARY_OP_TY_INDIRECTION) {
//       address = build_ir_for_expr(irb, stmt, assg->assignee->unary_op.expr,
//                                   &assg->assignee->unary_op.expr->var_ty);
//     }
//     break;
//   }
//   default:
//     todo("non var assignments");
//   }

//   if (!address) {
//     todo("non var assignments");
//   }

//   struct ir_op *store = alloc_ir_op(irb->func, *stmt);
//   store->ty = IR_OP_TY_STORE_ADDR;
//   store->var_ty = IR_OP_VAR_TY_NONE;
//   store->store_addr =
//       (struct ir_op_store_addr){.addr = address, .value = value};

//   return value;
// }

// struct ir_op *build_ir_for_arrayaccess(struct ir_func_builder *irb,
//                                        struct ir_stmt **stmt,
//                                        struct td_arrayaccess *array_access) {
//   struct ir_op_var_ty var_ty =
//       var_ty_for_td_var_ty(irb->func->unit, &array_access->lhs->var_ty);

//   struct ir_op *address = build_ir_for_array_address(
//       irb, stmt, array_access->lhs, array_access->rhs);

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_LOAD_ADDR;
//   op->var_ty = var_ty_get_underlying(&var_ty);
//   op->load_addr = (struct ir_op_load_addr){.addr = address};

//   return op;
// }

// struct ir_op *
// build_ir_for_memberaccess(struct ir_func_builder *irb, struct ir_stmt **stmt,
//                           struct td_memberaccess *member_access) {
//   struct ir_op *address = build_ir_for_member_address(
//       irb, stmt, member_access->lhs, &member_access->member);

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_LOAD_ADDR;
//   op->var_ty = var_ty_get_underlying(&address->var_ty);
//   op->load_addr = (struct ir_op_load_addr){.addr = address};

//   return op;
// }

// struct ir_op *
// build_ir_for_pointeraccess(struct ir_func_builder *irb, struct ir_stmt **stmt,
//                            struct td_pointeraccess *pointer_access) {
//   struct ir_op *address = build_ir_for_pointer_address(
//       irb, stmt, pointer_access->lhs, &pointer_access->member);

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_LOAD_ADDR;
//   op->var_ty = var_ty_get_underlying(&address->var_ty);
//   op->load_addr = (struct ir_op_load_addr){.addr = address};

//   return op;
// }

// struct ir_op *build_ir_for_expr(struct ir_func_builder *irb,
//                                 struct ir_stmt **stmt, struct td_expr *expr,
//                                 const struct td_var_ty *td_var_ty) {
//   struct ir_op *op;
//   switch (expr->ty) {
//   case TD_EXPR_TY_TERNARY:
//     op = build_ir_for_ternary(irb, stmt, &expr->ternary);
//     break;
//   case TD_EXPR_TY_VAR:
//     op = build_ir_for_var(irb, stmt, &expr->var);
//     break;
//   case TD_EXPR_TY_CNST:
//     op = build_ir_for_cnst(irb, stmt, &expr->cnst);
//     break;
//   case TD_EXPR_TY_COMPOUNDEXPR:
//     op = build_ir_for_compoundexpr(irb, stmt, &expr->compound_expr, td_var_ty);
//     break;
//   case TD_EXPR_TY_CALL:
//     op = build_ir_for_call(irb, stmt, &expr->call);
//     break;
//   case TD_EXPR_TY_UNARY_OP:
//     op = build_ir_for_unaryop(irb, stmt, &expr->unary_op);
//     break;
//   case TD_EXPR_TY_BINARY_OP:
//     op = build_ir_for_binaryop(irb, stmt, &expr->binary_op, &expr->var_ty);
//     break;
//   case TD_EXPR_TY_ARRAYACCESS:
//     op = build_ir_for_arrayaccess(irb, stmt, &expr->array_access);
//     break;
//   case TD_EXPR_TY_MEMBERACCESS:
//     op = build_ir_for_memberaccess(irb, stmt, &expr->member_access);
//     break;
//   case TD_EXPR_TY_POINTERACCESS:
//     op = build_ir_for_pointeraccess(irb, stmt, &expr->pointer_access);
//     break;
//   case TD_EXPR_TY_INIT_LIST:
//     bug("init list only makes sense in decls and compound assignments");
//   case TD_EXPR_TY_ASSG:
//     op = build_ir_for_assg(irb, stmt, &expr->assg);
//     break;
//   case TD_EXPR_TY_SIZEOF:
//     op = build_ir_for_sizeof(irb, stmt, &expr->size_of);
//     break;
//   case TD_EXPR_TY_ALIGNOF:
//     op = build_ir_for_alignof(irb, stmt, &expr->align_of);
//     break;
//   }

//   if (td_var_ty) {
//     struct ir_op_var_ty var_ty =
//         var_ty_for_td_var_ty(irb->func->unit, td_var_ty);

//     if (var_ty_needs_cast_op(irb, &op->var_ty, &var_ty)) {
//       op = insert_ir_for_cast(
//           irb, *stmt, op, &var_ty,
//           cast_ty_for_td_var_ty(irb, &expr->var_ty, td_var_ty));
//     }
//   }

//   invariant_assert(op, "null op!");
//   return op;
// }

// struct ir_basicblock *build_ir_for_stmt(struct ir_func_builder *irb,
//                                         struct ir_basicblock *basicblock,
//                                         struct td_stmt *stmt);

// struct ir_basicblock *
// build_ir_for_compoundstmt(struct ir_func_builder *irb,
//                           struct ir_basicblock *basicblock,
//                           struct td_compoundstmt *compound_stmt) {
//   for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
//     basicblock = build_ir_for_stmt(irb, basicblock, &compound_stmt->stmts[i]);
//   }
//   return basicblock;
// }

// struct ir_basicblock *build_ir_for_if(struct ir_func_builder *irb,
//                                       struct ir_basicblock *basicblock,
//                                       struct td_ifstmt *if_stmt) {

//   struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, basicblock);
//   struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, &if_stmt->condition,
//                                          &if_stmt->condition.var_ty);

//   struct ir_basicblock *pre_if_basicblock = cond->stmt->basicblock;

//   // basic block for if body
//   struct ir_basicblock *if_start_basicblock = alloc_ir_basicblock(irb->func);

//   struct ir_op *br_cond = alloc_ir_op(irb->func, cond_stmt);
//   br_cond->ty = IR_OP_TY_BR_COND;
//   br_cond->var_ty = IR_OP_VAR_TY_NONE;
//   br_cond->br_cond.cond = cond;

//   struct ir_basicblock *if_end_basicblock =
//       build_ir_for_stmt(irb, if_start_basicblock, if_stmt->body);

//   // we add a redundant branch to keep the nice property that all BBs end in a
//   // branch
//   struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, if_end_basicblock);
//   struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
//   br->ty = IR_OP_TY_BR;
//   br->var_ty = IR_OP_VAR_TY_NONE;

//   // basic block for *after* if body
//   struct ir_basicblock *after_if_basicblock = alloc_ir_basicblock(irb->func);

//   make_basicblock_split(irb->func, pre_if_basicblock, if_start_basicblock,
//                         after_if_basicblock);

//   make_basicblock_merge(irb->func, if_end_basicblock, after_if_basicblock);

//   return after_if_basicblock;
// }

// struct ir_basicblock *build_ir_for_ifelse(struct ir_func_builder *irb,
//                                           struct ir_basicblock *basicblock,
//                                           struct td_ifelsestmt *if_else_stmt) {
//   struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, basicblock);
//   struct ir_op *cond =
//       build_ir_for_expr(irb, &cond_stmt, &if_else_stmt->condition,
//                         &if_else_stmt->condition.var_ty);

//   // basic block for if body
//   struct ir_basicblock *if_basicblock = alloc_ir_basicblock(irb->func);
//   struct ir_basicblock *after_if_bb =
//       build_ir_for_stmt(irb, if_basicblock, if_else_stmt->body);

//   // basic block for else body
//   struct ir_basicblock *else_basicblock = alloc_ir_basicblock(irb->func);
//   struct ir_basicblock *after_else_bb =
//       build_ir_for_stmt(irb, else_basicblock, if_else_stmt->else_body);

//   struct ir_basicblock *after_if_else_basicblock =
//       alloc_ir_basicblock(irb->func);

//   struct ir_basicblock *pre_if_basicblock = cond->stmt->basicblock;

//   make_basicblock_split(irb->func, pre_if_basicblock, if_basicblock,
//                         else_basicblock);

//   struct ir_stmt *br_cond_stmt = alloc_ir_stmt(irb->func, pre_if_basicblock);
//   struct ir_op *br_cond = alloc_ir_op(irb->func, br_cond_stmt);
//   br_cond->ty = IR_OP_TY_BR_COND;
//   br_cond->var_ty = IR_OP_VAR_TY_NONE;
//   br_cond->br_cond.cond = cond;

//   struct ir_stmt *br_after_if_stmt = alloc_ir_stmt(irb->func, after_if_bb);
//   struct ir_op *br_after_if = alloc_ir_op(irb->func, br_after_if_stmt);
//   br_after_if->ty = IR_OP_TY_BR;
//   br_after_if->var_ty = IR_OP_VAR_TY_NONE;
//   make_basicblock_merge(irb->func, after_if_bb, after_if_else_basicblock);

//   struct ir_stmt *br_after_else_stmt = alloc_ir_stmt(irb->func, after_else_bb);
//   struct ir_op *br_after_else = alloc_ir_op(irb->func, br_after_else_stmt);
//   br_after_else->ty = IR_OP_TY_BR;
//   br_after_else->var_ty = IR_OP_VAR_TY_NONE;
//   make_basicblock_merge(irb->func, after_else_bb, after_if_else_basicblock);

//   return after_if_else_basicblock;
// }

// struct ir_basicblock *build_ir_for_switch(struct ir_func_builder *irb,
//                                           struct ir_basicblock *basicblock,
//                                           struct td_switchstmt *switch_stmt) {
//   struct ir_jump new_loop = {.ty = IR_JUMP_TY_NEW_LOOP};
//   vector_push_back(irb->jumps, &new_loop);

//   struct ir_stmt *ctrl_stmt = alloc_ir_stmt(irb->func, basicblock);
//   // TODO: this should be coerced to integer type
//   struct ir_op *ctrl_op =
//       build_ir_for_expr(irb, &ctrl_stmt, &switch_stmt->ctrl_expr, NULL);

//   struct ir_op *switch_op = alloc_ir_op(irb->func, ctrl_stmt);
//   switch_op->ty = IR_OP_TY_BR_SWITCH;
//   switch_op->var_ty = IR_OP_VAR_TY_NONE;
//   switch_op->br_switch = (struct ir_op_br_switch){.value = ctrl_op};

//   struct ir_basicblock *body_bb = alloc_ir_basicblock(irb->func);
//   struct ir_basicblock *end_bb =
//       build_ir_for_stmt(irb, body_bb, switch_stmt->body);

//   struct ir_basicblock *after_body_bb = alloc_ir_basicblock(irb->func);
//   make_basicblock_merge(irb->func, end_bb, after_body_bb);
//   struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, end_bb);
//   struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
//   br->ty = IR_OP_TY_BR;
//   br->var_ty = IR_OP_VAR_TY_NONE;

//   struct ir_basicblock *default_block = NULL;

//   struct vector *cases = vector_create(sizeof(struct ir_split_case));

//   while (!vector_empty(irb->switch_cases)) {
//     struct ir_case *switch_case = vector_pop(irb->switch_cases);

//     switch (switch_case->ty) {
//     case IR_CASE_TY_CASE: {
//       vector_push_back(cases, &switch_case->split_case);
//       break;
//     }
//     case IR_CASE_TY_DEFAULT:
//       default_block = switch_case->split_case.target;
//       break;
//     }
//   }

//   if (!default_block) {
//     default_block = after_body_bb;
//   }

//   make_basicblock_switch(irb->func, basicblock, vector_length(cases),
//                          vector_head(cases), default_block);

//   struct vector *continues = vector_create(sizeof(struct ir_jump));

//   while (!vector_empty(irb->jumps)) {
//     struct ir_jump *jump = vector_pop(irb->jumps);

//     switch (jump->ty) {
//     case IR_JUMP_TY_NEW_LOOP:
//       // end
//       return after_body_bb;
//     case IR_JUMP_TY_BREAK: {
//       make_basicblock_merge(irb->func, jump->basicblock, after_body_bb);
//       struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, jump->basicblock);
//       struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
//       br->ty = IR_OP_TY_BR;
//       br->var_ty = IR_OP_VAR_TY_NONE;
//       break;
//     }
//     case IR_JUMP_TY_CONTINUE:
//       vector_push_back(continues, jump);
//       break;
//     }
//   }

//   // propogate the `continue`s to the next level up
//   vector_extend(irb->jumps, vector_head(continues), vector_length(continues));

//   return after_body_bb;
// }

// struct ir_basicblock *
// build_ir_for_selectstmt(struct ir_func_builder *irb,
//                         struct ir_basicblock *basicblock,
//                         struct td_selectstmt *select_stmt) {
//   switch (select_stmt->ty) {
//   case td_SELECTSTMT_TY_IF: {
//     return build_ir_for_if(irb, basicblock, &select_stmt->if_stmt);
//   }
//   case td_SELECTSTMT_TY_IF_ELSE:
//     return build_ir_for_ifelse(irb, basicblock, &select_stmt->if_else_stmt);
//   case td_SELECTSTMT_TY_SWITCH:
//     return build_ir_for_switch(irb, basicblock, &select_stmt->switch_stmt);
//   }
// }

// struct ir_op *build_ir_for_decllist(struct ir_func_builder *irb,
//                                     struct ir_stmt **stmt,
//                                     struct td_decllist *decl_list);

// struct ir_op *build_ir_for_declorexpr(struct ir_func_builder *irb,
//                                       struct ir_stmt **stmt,
//                                       struct td_declorexpr *decl_or_expr) {
//   if (decl_or_expr->decl) {
//     return build_ir_for_decllist(irb, stmt, decl_or_expr->decl);
//   } else if (decl_or_expr->expr) {
//     return build_ir_for_expr(irb, stmt, decl_or_expr->expr,
//                              &decl_or_expr->expr->var_ty);
//   }

//   return NULL;
// }

// struct ir_loop {
//   // for CONTINUE
//   struct ir_basicblock *entry;
//   // for BREAK
//   struct ir_basicblock *exit;
// };

// struct ir_loop build_ir_for_whilestmt(struct ir_func_builder *irb,
//                                       struct ir_basicblock *basicblock,
//                                       struct td_whilestmt *while_stmt) {
//   struct ir_basicblock *before_cond_basicblock = basicblock;
//   struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb->func);
//   struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb->func);

//   make_basicblock_merge(irb->func, before_cond_basicblock, cond_basicblock);

//   struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, cond_basicblock);
//   struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, &while_stmt->cond,
//                                          &while_stmt->cond.var_ty);
//   struct ir_op *cond_br = alloc_ir_op(irb->func, cond_stmt);
//   cond_br->ty = IR_OP_TY_BR_COND;
//   cond_br->var_ty = IR_OP_VAR_TY_NONE;
//   cond_br->br_cond.cond = cond;

//   struct ir_basicblock *body_stmt_basicblock =
//       build_ir_for_stmt(irb, body_basicblock, while_stmt->body);
//   UNUSED_ARG(body_stmt_basicblock);

//   struct ir_basicblock *after_body_basicblock = alloc_ir_basicblock(irb->func);
//   make_basicblock_split(irb->func, cond_basicblock, body_basicblock,
//                         after_body_basicblock);

//   struct ir_stmt *pre_cond_stmt =
//       alloc_ir_stmt(irb->func, before_cond_basicblock);
//   struct ir_op *pre_cond_br = alloc_ir_op(irb->func, pre_cond_stmt);
//   pre_cond_br->ty = IR_OP_TY_BR;
//   pre_cond_br->var_ty = IR_OP_VAR_TY_NONE;

//   make_basicblock_merge(irb->func, body_stmt_basicblock, cond_basicblock);
//   struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, body_stmt_basicblock);
//   struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
//   br->ty = IR_OP_TY_BR;
//   br->var_ty = IR_OP_VAR_TY_NONE;

//   return (struct ir_loop){.entry = cond_basicblock,
//                           .exit = after_body_basicblock};
// }

// struct ir_loop build_ir_for_dowhilestmt(struct ir_func_builder *irb,
//                                         struct ir_basicblock *basicblock,
//                                         struct td_dowhilestmt *do_while_stmt) {
//   struct ir_basicblock *before_body_basicblock = basicblock;
//   struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb->func);
//   struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb->func);

//   make_basicblock_merge(irb->func, before_body_basicblock, body_basicblock);

//   struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, cond_basicblock);
//   struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, &do_while_stmt->cond,
//                                          &do_while_stmt->cond.var_ty);
//   struct ir_op *cond_br = alloc_ir_op(irb->func, cond_stmt);
//   cond_br->ty = IR_OP_TY_BR_COND;
//   cond_br->var_ty = IR_OP_VAR_TY_NONE;
//   cond_br->br_cond.cond = cond;

//   struct ir_basicblock *body_stmt_basicblock =
//       build_ir_for_stmt(irb, body_basicblock, do_while_stmt->body);

//   struct ir_basicblock *after_cond_basicblock = alloc_ir_basicblock(irb->func);
//   make_basicblock_split(irb->func, cond_basicblock, body_basicblock,
//                         after_cond_basicblock);

//   struct ir_stmt *pre_cond_stmt =
//       alloc_ir_stmt(irb->func, before_body_basicblock);
//   struct ir_op *pre_body_br = alloc_ir_op(irb->func, pre_cond_stmt);
//   pre_body_br->ty = IR_OP_TY_BR;
//   pre_body_br->var_ty = IR_OP_VAR_TY_NONE;

//   make_basicblock_merge(irb->func, body_stmt_basicblock, cond_basicblock);
//   struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, body_stmt_basicblock);
//   struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
//   br->ty = IR_OP_TY_BR;
//   br->var_ty = IR_OP_VAR_TY_NONE;

//   return (struct ir_loop){.entry = cond_basicblock,
//                           .exit = after_cond_basicblock};
// }

// struct ir_loop build_ir_for_forstmt(struct ir_func_builder *irb,
//                                     struct ir_basicblock *basicblock,
//                                     struct td_forstmt *for_stmt) {

//   struct ir_basicblock *before_cond_basicblock = basicblock;
//   struct ir_basicblock *before_body_basicblock = basicblock;

//   if (for_stmt->init) {
//     struct ir_stmt *init_stmt =
//         alloc_ir_stmt(irb->func, before_cond_basicblock);
//     build_ir_for_declorexpr(irb, &init_stmt, for_stmt->init);

//     before_cond_basicblock = init_stmt->basicblock;
//     before_body_basicblock = init_stmt->basicblock;
//   }

//   if (for_stmt->cond) {
//     struct ir_basicblock *cond_basicblock = alloc_ir_basicblock(irb->func);
//     make_basicblock_merge(irb->func, before_cond_basicblock, cond_basicblock);

//     struct ir_stmt *to_cond_stmt =
//         alloc_ir_stmt(irb->func, before_cond_basicblock);
//     struct ir_op *to_cond_br = alloc_ir_op(irb->func, to_cond_stmt);
//     to_cond_br->ty = IR_OP_TY_BR;
//     to_cond_br->var_ty = IR_OP_VAR_TY_NONE;

//     struct ir_stmt *cond_stmt = alloc_ir_stmt(irb->func, cond_basicblock);
//     struct ir_op *cond = build_ir_for_expr(irb, &cond_stmt, for_stmt->cond,
//                                            &for_stmt->cond->var_ty);

//     struct ir_op *cond_br = alloc_ir_op(irb->func, cond_stmt);
//     cond_br->ty = IR_OP_TY_BR_COND;
//     cond_br->var_ty = IR_OP_VAR_TY_NONE;
//     cond_br->br_cond.cond = cond;

//     before_body_basicblock = cond_stmt->basicblock;
//   } else {
//     struct ir_stmt *to_body_stmt =
//         alloc_ir_stmt(irb->func, before_body_basicblock);
//     struct ir_op *to_body_br = alloc_ir_op(irb->func, to_body_stmt);
//     to_body_br->ty = IR_OP_TY_BR;
//     to_body_br->var_ty = IR_OP_VAR_TY_NONE;
//   }

//   struct ir_basicblock *body_basicblock = alloc_ir_basicblock(irb->func);
//   make_basicblock_merge(irb->func, before_body_basicblock, body_basicblock);

//   struct ir_basicblock *body_stmt_basicblock =
//       build_ir_for_stmt(irb, body_basicblock, for_stmt->body);

//   struct ir_basicblock *end_body_basicblock = body_stmt_basicblock;

//   if (for_stmt->iter) {
//     struct ir_basicblock *iter_basicblock = alloc_ir_basicblock(irb->func);
//     make_basicblock_merge(irb->func, body_stmt_basicblock, iter_basicblock);

//     struct ir_stmt *to_iter_stmt =
//         alloc_ir_stmt(irb->func, body_stmt_basicblock);
//     struct ir_op *to_iter_br = alloc_ir_op(irb->func, to_iter_stmt);
//     to_iter_br->ty = IR_OP_TY_BR;
//     to_iter_br->var_ty = IR_OP_VAR_TY_NONE;

//     struct ir_stmt *iter_stmt = alloc_ir_stmt(irb->func, iter_basicblock);
//     build_ir_for_expr(irb, &iter_stmt, for_stmt->iter, &for_stmt->iter->var_ty);

//     end_body_basicblock = iter_stmt->basicblock;
//   }

//   struct ir_stmt *end_stmt = alloc_ir_stmt(irb->func, end_body_basicblock);
//   struct ir_op *end_br = alloc_ir_op(irb->func, end_stmt);
//   end_br->ty = IR_OP_TY_BR;
//   end_br->var_ty = IR_OP_VAR_TY_NONE;
//   make_basicblock_merge(irb->func, end_body_basicblock, before_body_basicblock);

//   struct ir_basicblock *after_body_basicblock = alloc_ir_basicblock(irb->func);

//   if (for_stmt->cond) {
//     make_basicblock_split(irb->func, before_body_basicblock, body_basicblock,
//                           after_body_basicblock);
//   } else {
//     make_basicblock_merge(irb->func, before_body_basicblock, body_basicblock);
//   }

//   return (struct ir_loop){.entry = end_body_basicblock,
//                           .exit = after_body_basicblock};
// }

// struct ir_basicblock *build_ir_for_iterstmt(struct ir_func_builder *irb,
//                                             struct ir_basicblock *basicblock,
//                                             struct td_iterstmt *iter_stmt) {
//   struct ir_jump new_loop = {.ty = IR_JUMP_TY_NEW_LOOP};
//   vector_push_back(irb->jumps, &new_loop);

//   struct ir_loop loop;
//   switch (iter_stmt->ty) {
//   case td_ITERSTMT_TY_WHILE:
//     loop = build_ir_for_whilestmt(irb, basicblock, &iter_stmt->while_stmt);
//     break;
//   case td_ITERSTMT_TY_DO_WHILE:
//     loop = build_ir_for_dowhilestmt(irb, basicblock, &iter_stmt->do_while_stmt);
//     break;
//   case td_ITERSTMT_TY_FOR:
//     loop = build_ir_for_forstmt(irb, basicblock, &iter_stmt->for_stmt);
//     break;
//   }

//   for (size_t i = vector_length(irb->jumps); i; i--) {
//     struct ir_jump *jump = vector_pop(irb->jumps);

//     switch (jump->ty) {
//     case IR_JUMP_TY_NEW_LOOP:
//       return loop.exit;
//     case IR_JUMP_TY_BREAK:
//       make_basicblock_merge(irb->func, jump->basicblock, loop.exit);
//       break;
//     case IR_JUMP_TY_CONTINUE:
//       make_basicblock_merge(irb->func, jump->basicblock, loop.entry);
//       break;
//     }

//     struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, jump->basicblock);
//     struct ir_op *br = alloc_ir_op(irb->func, br_stmt);
//     br->ty = IR_OP_TY_BR;
//     br->var_ty = IR_OP_VAR_TY_NONE;
//   }

//   bug("should've found IR_JUMP_TY_NEW_LOOP in jump vector");
// }

// struct ir_basicblock *build_ir_for_goto(struct ir_func_builder *irb,
//                                         struct ir_stmt **stmt,
//                                         struct td_gotostmt *goto_stmt) {
//   struct ir_basicblock *before_goto_basicblock = (*stmt)->basicblock;

//   struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, before_goto_basicblock);
//   struct ir_op *br = alloc_ir_op(irb->func, br_stmt);

//   br->ty = IR_OP_TY_BR;
//   br->var_ty = IR_OP_VAR_TY_NONE;

//   // put the label we target into metadata
//   br->metadata = (void *)identifier_str(irb->parser, &goto_stmt->label);

//   struct ir_basicblock *after_goto_basicblock = alloc_ir_basicblock(irb->func);
//   return after_goto_basicblock;
// }

// /* Return stmt be null when this is used to add implicit returns not in code
//  * (e.g at end of method) */
// struct ir_basicblock *build_ir_for_ret(struct ir_func_builder *irb,
//                                        struct ir_stmt **stmt,
//                                        struct td_returnstmt *return_stmt) {
//   struct ir_op *expr_op;
//   if (return_stmt && return_stmt->expr) {
//     expr_op =
//         build_ir_for_expr(irb, stmt, return_stmt->expr, &return_stmt->var_ty);
//   } else {
//     expr_op = NULL;
//   }

//   struct ir_op *op = alloc_ir_op(irb->func, *stmt);
//   op->ty = IR_OP_TY_RET;
//   op->var_ty = return_stmt
//                    ? var_ty_for_td_var_ty(irb->func->unit, &return_stmt->var_ty)
//                    : IR_OP_VAR_TY_NONE;
//   op->ret.value = expr_op;

//   op->stmt->basicblock->ty = IR_BASICBLOCK_TY_RET;

//   struct ir_basicblock *after_ret_basicblock = alloc_ir_basicblock(irb->func);

//   return after_ret_basicblock;
// }

// struct ir_basicblock *build_ir_for_break(struct ir_func_builder *irb,
//                                          struct ir_stmt **stmt) {
//   struct ir_jump jump = {.ty = IR_JUMP_TY_BREAK,
//                          .basicblock = (*stmt)->basicblock};
//   vector_push_back(irb->jumps, &jump);

//   struct ir_basicblock *after_break_basicblock = alloc_ir_basicblock(irb->func);
//   return after_break_basicblock;
// }

// struct ir_basicblock *build_ir_for_continue(struct ir_func_builder *irb,
//                                             struct ir_stmt **stmt) {
//   struct ir_jump jump = {.ty = IR_JUMP_TY_CONTINUE,
//                          .basicblock = (*stmt)->basicblock};
//   vector_push_back(irb->jumps, &jump);

//   struct ir_basicblock *after_continue_basicblock =
//       alloc_ir_basicblock(irb->func);
//   return after_continue_basicblock;
// }

// struct ir_basicblock *build_ir_for_jumpstmt(struct ir_func_builder *irb,
//                                             struct ir_basicblock *basicblock,
//                                             struct td_jumpstmt *jump_stmt) {
//   struct ir_stmt *stmt = alloc_ir_stmt(irb->func, basicblock);

//   switch (jump_stmt->ty) {
//   case td_JUMPSTMT_TY_RETURN:
//     return build_ir_for_ret(irb, &stmt, &jump_stmt->return_stmt);
//   case td_JUMPSTMT_TY_GOTO:
//     return build_ir_for_goto(irb, &stmt, &jump_stmt->goto_stmt);
//   case td_JUMPSTMT_TY_BREAK:
//     return build_ir_for_break(irb, &stmt);
//   case td_JUMPSTMT_TY_CONTINUE:
//     return build_ir_for_continue(irb, &stmt);
//   }
// }

// struct ir_op *build_ir_for_zero_init(struct ir_func_builder *irb,
//                                      struct ir_stmt **stmt,
//                                      const struct td_var_ty *var_ty) {
//   if (var_ty->ty != TD_VAR_TY_TY_WELL_KNOWN) {
//     todo("non well-known");
//   }

//   enum ir_op_var_primitive_ty ty = var_ty_for_well_known_ty(var_ty->well_known);

//   struct ir_op *value = alloc_ir_op(irb->func, *stmt);
//   make_integral_constant(irb->func->unit, value, ty, 0);
//   return value;
// }

// struct ir_op *build_ir_for_array_initlist(struct ir_func_builder *irb,
//                                           struct ir_stmt **stmt,
//                                           struct td_decl *decl,
//                                           struct td_initlist *init_list,
//                                           const struct td_var_ty *var_ty) {
//   debug_assert(var_ty->ty == TD_VAR_TY_TY_ARRAY, "non array init list");

//   struct td_var_ty *el_ty = var_ty->array.element;
//   size_t num_elements = var_ty->array.ty == TD_TY__ARRAY_TY_KNOWN_SIZE
//                             ? var_ty->array.size
//                             : init_list->num_inits;

//   if (!num_elements) {
//     bug("empty arrays are GNU extension");
//   }

//   struct td_expr decl_expr = {
//       .ty = TD_EXPR_TY_VAR, .var_ty = *var_ty, .var = decl->var};

//   struct ir_op *ltd;

//   struct ir_op *start_address = build_ir_for_addressof(irb, stmt, &decl_expr);

//   struct ir_op *zero_init = NULL;

//   struct ir_op_var_ty ir_el_ty = var_ty_for_td_var_ty(irb->func->unit, el_ty);
//   size_t el_size = var_ty_info(irb->func->unit, &ir_el_ty).size;

//   for (size_t i = 0; i < num_elements; i++) {
//     struct ir_op *expr;

//     if (i < init_list->num_inits) {
//       expr = build_ir_for_expr(irb, stmt, init_list->inits[i].expr, el_ty);
//     } else {
//       // can reuse zero init as array is always homogenous type
//       if (!zero_init) {
//         zero_init = build_ir_for_zero_init(irb, stmt, el_ty);
//       }
//       expr = zero_init;
//     }

//     struct ir_op *offset = alloc_ir_op(irb->func, *stmt);
//     make_pointer_constant(irb->func->unit, offset, i * el_size);

//     struct ir_op *address = alloc_ir_op(irb->func, *stmt);
//     address->ty = IR_OP_TY_BINARY_OP;
//     address->var_ty = start_address->var_ty;
//     address->binary_op = (struct ir_op_binary_op){
//         .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = start_address, .rhs = offset};

//     struct ir_op *store = alloc_ir_op(irb->func, *stmt);
//     store->ty = IR_OP_TY_STORE_ADDR;
//     store->var_ty = IR_OP_VAR_TY_NONE;
//     store->store_addr =
//         (struct ir_op_store_addr){.addr = address, .value = expr};

//     ltd = store;
//   }

//   return ltd;
// }

// struct ir_op *build_ir_for_struct_initlist(struct ir_func_builder *irb,
//                                            struct ir_stmt **stmt,
//                                            struct td_decl *decl,
//                                            struct td_initlist *init_list,
//                                            const struct td_var_ty *var_ty) {

//   debug_assert(var_ty->ty == TD_VAR_TY_TY_AGGREGATE &&
//                    var_ty->aggregate.ty == TD_TY__AGGREGATE_TY_STRUCT,
//                "non struct init list");

//   size_t num_elements = var_ty->aggregate.num_field_var_tys;

//   if (!num_elements) {
//     bug("empty structs are GNU extension");
//   }

//   struct td_expr decl_expr = {
//       .ty = TD_EXPR_TY_VAR, .var_ty = *var_ty, .var = decl->var};

//   struct ir_op *ltd;

//   struct ir_op *start_address = build_ir_for_addressof(irb, stmt, &decl_expr);

//   for (size_t i = 0; i < num_elements; i++) {
//     struct ir_op *expr;

//     debug_assert(i < var_ty->aggregate.num_field_var_tys,
//                  "too many items in struct init-list");
//     struct td_struct_field *field = &var_ty->aggregate.field_var_tys[i];

//     if (i < init_list->num_inits) {
//       expr =
//           build_ir_for_expr(irb, stmt, init_list->inits[i].expr, field->var_ty);
//     } else {
//       expr = build_ir_for_zero_init(irb, stmt, field->var_ty);
//     }

//     struct ir_op_var_ty member_ty;
//     struct ir_op *offset = build_ir_for_member_address_offset(
//         irb, stmt, var_ty, field->name, &member_ty);

//     struct ir_op *address = alloc_ir_op(irb->func, *stmt);
//     address->ty = IR_OP_TY_BINARY_OP;
//     address->var_ty = start_address->var_ty;
//     address->binary_op = (struct ir_op_binary_op){
//         .ty = IR_OP_BINARY_OP_TY_ADD, .lhs = start_address, .rhs = offset};

//     struct ir_op *store = alloc_ir_op(irb->func, *stmt);
//     store->ty = IR_OP_TY_STORE_ADDR;
//     store->var_ty = IR_OP_VAR_TY_NONE;
//     store->store_addr =
//         (struct ir_op_store_addr){.addr = address, .value = expr};

//     ltd = store;
//   }

//   return ltd;
// }

// struct ir_op *build_ir_for_union_initlist(struct ir_func_builder *irb,
//                                           struct ir_stmt **stmt,
//                                           struct td_decl *decl,
//                                           struct td_initlist *init_list,
//                                           const struct td_var_ty *var_ty) {
//   debug_assert(var_ty->ty == TD_VAR_TY_TY_AGGREGATE &&
//                    var_ty->aggregate.ty == TD_TY__AGGREGATE_TY_UNION,
//                "non union init list");

//   invariant_assert(init_list->num_inits <= 1,
//                    "cannot have more than 1 element in union init-list");

//   struct td_expr decl_expr = {
//       .ty = TD_EXPR_TY_VAR, .var_ty = *var_ty, .var = decl->var};
//   struct ir_op *address = build_ir_for_addressof(irb, stmt, &decl_expr);

//   debug_assert(var_ty->aggregate.num_field_var_tys,
//                "empty union is GNU extension");

//   struct ir_op *expr;
//   if (init_list->num_inits) {
//     struct td_struct_field *field = &var_ty->aggregate.field_var_tys[0];
//     expr =
//         build_ir_for_expr(irb, stmt, init_list->inits[0].expr, field->var_ty);
//   } else {
//     expr = build_ir_for_zero_init(irb, stmt, var_ty);
//   }

//   struct ir_op *store = alloc_ir_op(irb->func, *stmt);
//   store->ty = IR_OP_TY_STORE_ADDR;
//   store->var_ty = IR_OP_VAR_TY_NONE;
//   store->store_addr = (struct ir_op_store_addr){.addr = address, .value = expr};

//   return store;
// }

// struct ir_op *
// build_ir_for_vardecl_with_initlist(struct ir_func_builder *irb,
//                                    struct ir_stmt **stmt, struct td_decl *decl,
//                                    struct td_initlist *init_list) {

//   struct td_var_ty var_ty = decl->var.var_ty;
//   if (var_ty.ty == TD_VAR_TY_TY_TAGGED) {
//     var_ty = tyref_get_defined(irb->parser, &var_ty);
//   }
//   // TODO: non array init lists

//   switch (var_ty.ty) {
//   case TD_VAR_TY_TY_ARRAY:
//     return build_ir_for_array_initlist(irb, stmt, decl, init_list, &var_ty);
//   case TD_VAR_TY_TY_AGGREGATE:
//     switch (var_ty.aggregate.ty) {
//     case TD_TY__AGGREGATE_TY_STRUCT:
//       return build_ir_for_struct_initlist(irb, stmt, decl, init_list, &var_ty);
//     case TD_TY__AGGREGATE_TY_UNION:
//       return build_ir_for_union_initlist(irb, stmt, decl, init_list, &var_ty);
//     }
//   default:
//     bug("initlist only makes sense for array/struct/union");
//   }
// }

// void var_assg_glb(struct ir_func_builder *irb, struct ir_stmt *stmt,
//                   struct ir_glb *glb, struct td_var *var);

// struct ir_var_value build_ir_for_var_value(struct ir_unit *iru,
//                                            struct td_expr *expr,
//                                            struct td_var_ty *var_ty);

// void build_ir_for_non_auto_var(
//     struct ir_unit *iru, struct ir_func *func, struct var_refs *var_refs,
//     enum td_storage_class_specifier_flags storage_class,
//     struct td_decl *decl) {

//   struct ir_op_var_ty var_ty = var_ty_for_td_var_ty(iru, &decl->var.var_ty);

//   const char *name = identifier_str(iru->parser, &decl->var.identifier);
//   const char *symbol_name;
//   if (storage_class & td_STORAGE_CLASS_SPECIFIER_FLAG_STATIC) {
//     // need to mangle the name as statics cannot interfere with others
//     size_t base_len = strlen(name);

//     size_t len = base_len + 2; // null char and leading "."

//     size_t func_name_len = 0;
//     if (func) {
//       func_name_len = strlen(func->name);
//       len += func_name_len;
//       len++; // for "."
//     }

//     char *buff = arena_alloc(iru->arena, sizeof(*name) * len);
//     size_t head = 0;

//     buff[head++] = '.';

//     if (func) {
//       memcpy(&buff[head], func->name, func_name_len);
//       head += func_name_len;
//       buff[head++] = '.';
//     }

//     memcpy(&buff[head], name, base_len);
//     head += base_len;
//     buff[head++] = '\0';

//     debug_assert(head == len, "string/buff length mismatch");

//     symbol_name = buff;
//   } else {
//     symbol_name = name;
//   }

//   struct var_key key = {.name = name, .scope = decl->var.scope};

//   enum ir_glb_ty ty;
//   if (decl->var.var_ty.ty == TD_VAR_TY_TY_FUNC) {
//     ty = IR_GLB_TY_FUNC;
//   } else {
//     ty = IR_GLB_TY_DATA;
//   }

//   struct var_ref *ref = var_refs_get(var_refs, &key);

//   if (ref) {
//     debug_assert(ref->glb, "ref but has no glb");
//   }

//   enum ir_linkage linkage;

//   bool is_func = decl->var.var_ty.ty == TD_VAR_TY_TY_FUNC;
//   bool is_extern = storage_class & td_STORAGE_CLASS_SPECIFIER_FLAG_EXTERN;
//   bool is_static = storage_class & td_STORAGE_CLASS_SPECIFIER_FLAG_STATIC;
//   bool is_file_scope = key.scope == SCOPE_GLOBAL;
//   bool is_unspecified_storage =
//       storage_class == td_STORAGE_CLASS_SPECIFIER_FLAG_NONE;

//   if ((is_func && !is_static) || is_extern || (is_file_scope && !is_static)) {
//     linkage = IR_LINKAGE_EXTERNAL;
//   } else if (is_file_scope && is_static) {
//     linkage = IR_LINKAGE_INTERNAL;
//   } else {
//     linkage = IR_LINKAGE_NONE;
//   }

//   enum ir_glb_def_ty def_ty;
//   if (decl->ty == td_DECL_TY_DECL_WITH_ASSG) {
//     def_ty = IR_GLB_DEF_TY_DEFINED;
//   } else if (is_file_scope && !is_func && (is_unspecified_storage || is_static)) {
//     def_ty = IR_GLB_DEF_TY_TENTATIVE;
//   } else {
//     def_ty = IR_GLB_DEF_TY_UNDEFINED;
//   }

//   if (ref && def_ty == IR_GLB_DEF_TY_TENTATIVE) {
//     // already defined, and this is tentative, so do nothing
//     return;
//   }

//   if (ref && linkage == IR_LINKAGE_EXTERNAL &&
//       ref->glb->linkage == IR_LINKAGE_INTERNAL) {
//     // extern but prev was static, stays static
//     linkage = IR_LINKAGE_INTERNAL;
//   }

//   if (!ref) {
//     ref = var_refs_add(var_refs, &key, VAR_REF_TY_GLB);
//   }

//   if (!ref->glb) {
//     ref->glb = add_global(iru, ty, &var_ty, def_ty, symbol_name);
//   }

//   ref->glb->def_ty = def_ty;
//   ref->glb->linkage = linkage;

//   if (def_ty != IR_GLB_DEF_TY_DEFINED) {
//     return;
//   }

//   struct ir_var_value value;
//   if (decl->ty == td_DECL_TY_DECL_WITH_ASSG) {
//     value = build_ir_for_var_value(iru, &decl->assg_expr, &decl->var.var_ty);
//   } else {
//     value = (struct ir_var_value){.var_ty = var_ty};
//   }

//   if (!ref->glb->var) {
//     ref->glb->var = arena_alloc(iru->arena, sizeof(*ref->glb->var));
//   }

//   *ref->glb->var =
//       (struct ir_var){.ty = IR_VAR_TY_DATA, .var_ty = var_ty, .value = value};
// }

// void build_ir_for_auto_var(struct ir_func_builder *irb, struct ir_stmt **stmt,
//                            struct td_decl *decl) {
//   struct ir_op *assignment;
//   if (decl->ty == td_DECL_TY_DECL_WITH_ASSG &&
//       decl->assg_expr.ty != TD_EXPR_TY_INIT_LIST) {
//     assignment =
//         build_ir_for_expr(irb, stmt, &decl->assg_expr, &decl->var.var_ty);
//   } else {
//     assignment = alloc_ir_op(irb->func, *stmt);
//     assignment->ty = IR_OP_TY_UNDF;
//     assignment->var_ty =
//         var_ty_for_td_var_ty(irb->func->unit, &decl->var.var_ty);
//   }

//   var_assg(irb, *stmt, assignment, &decl->var);

//   // init lists are not true assignments
//   // they are a lot of stores into locals
//   // so must be built after the variable exists
//   if (decl->ty == td_DECL_TY_DECL_WITH_ASSG &&
//       decl->assg_expr.ty == TD_EXPR_TY_INIT_LIST) {
//     build_ir_for_vardecl_with_initlist(irb, stmt, decl,
//                                        &decl->assg_expr.init_list);
//   }
// }

// // this is called for decl lists WITHIN a function (i.e default is local
// // storage)
// struct ir_op *build_ir_for_decllist(struct ir_func_builder *irb,
//                                     struct ir_stmt **stmt,
//                                     struct td_decllist *decl_list) {
//   if (decl_list->storage_class_specifiers &
//       td_STORAGE_CLASS_SPECIFIER_FLAG_TYPEDEF) {
//     return NULL;
//   }

//   for (size_t i = 0; i < decl_list->num_decls; i++) {
//     struct td_decl *decl = &decl_list->decls[i];

//     if (decl->var.var_ty.ty == TD_VAR_TY_TY_FUNC) {
//       // tentative definition! make global
//       struct ir_op_var_ty var_ty =
//           var_ty_for_td_var_ty(irb->func->unit, &decl->var.var_ty);

//       struct ir_glb *glb = add_global(
//           irb->func->unit, IR_GLB_TY_FUNC, &var_ty, IR_GLB_DEF_TY_UNDEFINED,
//           identifier_str(irb->parser, &decl->var.identifier));

//       glb->var = arena_alloc(irb->func->arena, sizeof(*glb->var));

//       var_assg_glb(irb, *stmt, glb, &decl->var);
//       continue;
//     }

//     if (decl_list->storage_class_specifiers ==
//             td_STORAGE_CLASS_SPECIFIER_FLAG_NONE ||
//         decl_list->storage_class_specifiers ==
//             td_STORAGE_CLASS_SPECIFIER_FLAG_AUTO) {
//       build_ir_for_auto_var(irb, stmt, decl);
//     } else {
//       build_ir_for_non_auto_var(irb->func->unit, irb->func,
//                                 irb->global_var_refs,
//                                 decl_list->storage_class_specifiers, decl);
//     }
//   }

//   return (*stmt)->ltd;
// }

// struct ir_basicblock *
// build_ir_for_labeledstmt(struct ir_func_builder *irb,
//                          struct ir_basicblock *basicblock,
//                          struct td_labeledstmt *labeled_stmt) {
//   struct ir_basicblock *next_bb = alloc_ir_basicblock(irb->func);
//   make_basicblock_merge(irb->func, basicblock, next_bb);

//   struct ir_stmt *br_stmt = alloc_ir_stmt(irb->func, basicblock);
//   struct ir_op *br_op = alloc_ir_op(irb->func, br_stmt);
//   br_op->ty = IR_OP_TY_BR;
//   br_op->var_ty = IR_OP_VAR_TY_NONE;

//   switch (labeled_stmt->ty) {
//   case td_LABELEDSTMT_TY_LABEL: {
//     const char *name = identifier_str(irb->parser, &labeled_stmt->label);
//     add_label(irb, name, next_bb);
//     break;
//   }
//   case td_LABELEDSTMT_TY_CASE: {
//     struct ir_case switch_case = {
//         .ty = IR_CASE_TY_CASE,
//         .split_case = {.target = next_bb, .value = labeled_stmt->cnst}};
//     vector_push_back(irb->switch_cases, &switch_case);
//     break;
//   }
//   case td_LABELEDSTMT_TY_DEFAULT: {
//     struct ir_case switch_case = {.ty = IR_CASE_TY_DEFAULT,
//                                   .split_case = {.target = next_bb}};
//     vector_push_back(irb->switch_cases, &switch_case);
//     break;
//   }
//   }

//   return build_ir_for_stmt(irb, next_bb, labeled_stmt->stmt);
// }

// struct ir_basicblock *build_ir_for_stmt(struct ir_func_builder *irb,
//                                         struct ir_basicblock *basicblock,
//                                         struct td_stmt *stmt) {

//   debug_assert(basicblock, "bb cannot be null");

//   switch (stmt->ty) {
//   case td_STMT_TY_DECL_LIST: {
//     struct ir_stmt *ir_stmt = alloc_ir_stmt(irb->func, basicblock);
//     build_ir_for_decllist(irb, &ir_stmt, &stmt->decl_list);
//     return ir_stmt->basicblock;
//   }
//   case td_STMT_TY_EXPR: {
//     // TODO: ternaries
//     struct ir_stmt *ir_stmt = alloc_ir_stmt(irb->func, basicblock);
//     build_ir_for_expr(irb, &ir_stmt, &stmt->expr, NULL);
//     return ir_stmt->basicblock;
//   }
//   case td_STMT_TY_JUMP: {
//     return build_ir_for_jumpstmt(irb, basicblock, &stmt->jump);
//   }
//   case td_STMT_TY_COMPOUND: {
//     return build_ir_for_compoundstmt(irb, basicblock, &stmt->compound);
//   }
//   case td_STMT_TY_SELECT: {
//     return build_ir_for_selectstmt(irb, basicblock, &stmt->select);
//   }
//   case td_STMT_TY_ITER: {
//     return build_ir_for_iterstmt(irb, basicblock, &stmt->iter);
//   }
//   case td_STMT_TY_LABELED: {
//     return build_ir_for_labeledstmt(irb, basicblock, &stmt->labeled);
//   }
//   case td_STMT_TY_NULL: {
//     return basicblock;
//   }
//   }
// }

// void walk_basicblock(struct ir_func_builder *irb, bool *basicblocks_visited,
//                      struct ir_op *source_phi, struct td_var *var,
//                      struct ir_basicblock *basicblock, struct ir_op ***exprs,
//                      size_t *num_exprs) {
//   if (!basicblock || basicblocks_visited[basicblock->id]) {
//     return;
//   }

//   basicblocks_visited[basicblock->id] = true;

//   struct var_key key = get_var_key(irb->parser, var, basicblock);
//   struct var_ref *ref = var_refs_get(irb->var_refs, &key);

//   if (!ref) {
//     debug("bb %zu has %zu preds", basicblock->id, basicblock->num_preds);
//     for (size_t i = 0; i < basicblock->num_preds; i++) {
//       walk_basicblock(irb, basicblocks_visited, source_phi, var,
//                       basicblock->preds[i], exprs, num_exprs);
//     }
//     return;
//   }

//   switch (ref->ty) {
//   case VAR_REF_TY_GLB:
//   case VAR_REF_TY_LCL:
//   case VAR_REF_TY_SSA: {
//     if (ref->op) {
//       (*num_exprs)++;
//       *exprs = arena_realloc(irb->func->arena, *exprs,
//                              sizeof(struct ir_op *) * *num_exprs);
//       (*exprs)[*num_exprs - 1] = ref->op;
//     }
//   }
//   }
// }

// void find_phi_exprs(struct ir_func_builder *irb, struct ir_op *phi) {
//   debug_assert(phi->ty == IR_OP_TY_PHI, "non-phi in `find_phi_exprs`");

//   // walk predecessor basic blocks (splitting into seperate walks each time we
//   // have multiple predecessors) until we
//   // * A) find a write
//   // * B) re-reach current bb
//   // * or C) reach end (first bb)
//   bool *basicblocks_visited =
//       arena_alloc(irb->func->arena,
//                   sizeof(*basicblocks_visited) * irb->func->basicblock_count);
//   memset(basicblocks_visited, 0,
//          sizeof(*basicblocks_visited) * irb->func->basicblock_count);

//   struct ir_op **exprs = NULL;
//   size_t num_exprs = 0;

//   for (size_t i = 0; i < phi->stmt->basicblock->num_preds; i++) {
//     struct ir_basicblock *pred = phi->stmt->basicblock->preds[i];
//     walk_basicblock(irb, basicblocks_visited, phi, phi->phi.var, pred, &exprs,
//                     &num_exprs);
//   }

//   // if (num_exprs && (exprs[0]->flags & IR_OP_FLAG_PARAM)) {
//   //   return;
//   // }

//   if (!num_exprs) {
//     err("undefined behaviour - reading from unassigned variable '%s'",
//         identifier_str(irb->parser, &phi->phi.var->identifier));
//     return;
//     phi->var_ty = IR_OP_VAR_TY_NONE;
//     phi->phi.values = NULL;
//     phi->phi.num_values = 0;
//   }

//   phi->var_ty = exprs[0]->var_ty;
//   phi->phi.values = exprs;
//   phi->phi.num_values = num_exprs;
// }

// struct validate_metadata {
//   struct ir_func_builder *irb;
//   struct ir_op *consumer;
// };

// void validate_op_tys_callback(struct ir_op **op, void *cb_metadata) {
//   struct validate_metadata *metadata = cb_metadata;
//   struct ir_op *consumer = metadata->consumer;

//   struct ir_op_var_ty res_ty = (*op)->var_ty;

//   // TODO: validate cast types (make sure they are valid)
//   switch (consumer->ty) {
//   case IR_OP_TY_BINARY_OP:
//     res_ty = consumer->var_ty;
//     break;
//   case IR_OP_TY_CALL:
//     res_ty = *consumer->call.func_ty.func.ret_ty;
//     break;
//   case IR_OP_TY_CAST_OP:
//     res_ty = consumer->var_ty;
//     break;
//   case IR_OP_TY_ADDR:
//     res_ty = var_ty_make_pointer(metadata->irb->func->unit, &(*op)->var_ty);
//     break;
//   case IR_OP_TY_LOAD_ADDR:
//     res_ty = var_ty_get_underlying(&(*op)->var_ty);
//     break;
//   default:
//     break;
//   }

//   if (op_produces_value(consumer)) {
//     invariant_assert(
//         !var_ty_needs_cast_op(metadata->irb, &res_ty, &consumer->var_ty),
//         "op %zu uses op %zu with different type!", consumer->id, (*op)->id);
//   }
// }

// struct ir_func_builder *
// build_ir_for_function(struct ir_unit *unit, struct arena_allocator *arena,
//                       struct td_funcdef *def,
//                       struct var_refs *global_var_refs) {
//   struct var_refs *var_refs = var_refs_create();
//   struct ir_func b = {.unit = unit,
//                       .name = identifier_str(unit->parser, &def->identifier),
//                       .arena = arena,
//                       .flags = IR_FUNC_FLAG_NONE,
//                       .first = NULL,
//                       .ltd = NULL,
//                       .op_count = 0,
//                       .num_locals = 0,
//                       .total_locals_size = 0};

//   struct ir_func *f = arena_alloc(arena, sizeof(b));
//   *f = b;

//   struct ir_func_builder *builder = arena_alloc(arena, sizeof(b));
//   *builder = (struct ir_func_builder){
//       .func = f,
//       .jumps = vector_create(sizeof(struct ir_jump)),
//       .switch_cases = vector_create(sizeof(struct ir_case)),
//       .parser = unit->parser,
//       .var_refs = var_refs,
//       .global_var_refs = global_var_refs};

//   // needs at letd one initial basic block
//   alloc_ir_basicblock(builder->func);
//   struct ir_basicblock *basicblock = builder->func->first;
//   struct ir_stmt *param_stmt = alloc_ir_stmt(builder->func, basicblock);

//   // first statement is a bunch of magic MOV commands that explain to the rest
//   // of the IR that these are params this is encoded as MOV NULL with the
//   // IR_OP_FLAG_PARAM flag
//   for (size_t i = 0; i < def->var_ty.func.num_params; i++) {
//     const struct token *param_identifier =
//         def->var_ty.func.param_identifiers[i];
//     const struct td_var_ty *param_var_ty = &def->var_ty.func.param_var_tys[i];

//     if (param_var_ty->ty == TD_VAR_TY_TY_VARIADIC || !param_identifier) {
//       continue;
//     }

//     // TODO: the whole decl code needs reworking
//     struct td_var var = {
//         .scope = SCOPE_PARAMS,
//         .identifier = *param_identifier,
//     };

//     struct var_key key = get_var_key(builder->parser, &var, basicblock);
//     struct var_ref *ref = var_refs_add(builder->var_refs, &key, VAR_REF_TY_SSA);

//     struct ir_op_var_ty var_ty =
//         var_ty_for_td_var_ty(builder->func->unit, param_var_ty);
//     if (var_ty.ty == IR_OP_VAR_TY_TY_ARRAY) {
//       // arrays are actually pointers
//       struct ir_op_var_ty array_ty = var_ty;
//       var_ty.ty = IR_OP_VAR_TY_TY_POINTER;
//       var_ty.pointer.underlying =
//           arena_alloc(builder->func->arena, sizeof(*var_ty.pointer.underlying));
//       *var_ty.pointer.underlying = array_ty;
//     }

//     struct ir_op *mov = alloc_ir_op(builder->func, param_stmt);
//     mov->ty = IR_OP_TY_MOV;
//     mov->var_ty = var_ty;
//     mov->flags |= IR_OP_FLAG_PARAM;
//     mov->mov.value = NULL;

//     ref->op = mov;
//   }

//   for (size_t i = 0; i < def->body.num_stmts; i++) {
//     basicblock = build_ir_for_stmt(builder, basicblock, &def->body.stmts[i]);
//     debug_assert(basicblock, "stmt #%zu generated a null basicblock", i);
//   }

//   // now we have generated the IR we first need to fix up labels
//   basicblock = builder->func->first;
//   while (basicblock) {
//     struct ir_stmt *stmt = basicblock->first;
//     while (stmt) {
//       struct ir_op *op = stmt->first;
//       while (op) {
//         if (op->ty == IR_OP_TY_BR && op->metadata) {
//           // any BR with metadata is a label
//           const char *name = op->metadata;

//           struct ir_label *label = builder->labels;
//           while (label) {
//             if (strcmp(name, label->name) == 0) {
//               make_basicblock_merge(builder->func, basicblock,
//                                     label->basicblock);
//             }

//             label = label->succ;
//           }
//         }

//         op = op->succ;
//       }

//       stmt = stmt->succ;
//     }

//     basicblock = basicblock->succ;
//   }

//   // we may generate empty basicblocks or statements, prune them here
//   prune_basicblocks(builder->func);

//   // may not end in a return, but needs to to be well-formed IR
//   struct ir_basicblock *ltd_bb = builder->func->ltd;
//   if (!ltd_bb) {
//     debug("adding bb to create ret");
//     ltd_bb = alloc_ir_basicblock(builder->func);
//   }

//   struct ir_stmt *ltd_stmt = ltd_bb->ltd;
//   if (!ltd_stmt) {
//     debug("adding bb to create stmt");
//     ltd_stmt = alloc_ir_stmt(builder->func, ltd_bb);
//   }

//   struct ir_op *ltd_op = ltd_stmt->ltd;

//   if (!ltd_op || ltd_op->ty != IR_OP_TY_RET) {
//     struct ir_op *return_value = NULL;

//     if (strcmp(builder->func->name, "main") == 0) {
//       debug("adding implicit return 0 to bb %zu", ltd_bb->id);

//       struct ir_op *cnst = alloc_ir_op(builder->func, ltd_stmt);
//       cnst->ty = IR_OP_TY_CNST;
//       cnst->var_ty = (struct ir_op_var_ty){
//           .ty = IR_OP_VAR_TY_TY_PRIMITIVE,
//           .primitive = IR_OP_VAR_PRIMITIVE_TY_I32,
//       };
//       cnst->cnst = (struct ir_op_cnst){.ty = IR_OP_CNST_TY_INT, .int_value = 0};

//       return_value = cnst;
//     }

//     basicblock = build_ir_for_ret(builder, &ltd_stmt, NULL);
//     debug_assert(ltd_stmt->ltd->ty == IR_OP_TY_RET,
//                  "expected ret after call to build ret");
//     ltd_stmt->ltd->ret.value = return_value;
//   }

//   // prune again, as inserting the ret can introduce an extraneous empty bb
//   prune_basicblocks(builder->func);

//   if (log_enabled()) {
//     debug_print_ir_func(stderr, builder->func, NULL, NULL);
//   }

//   // now we fix up phis
//   basicblock = builder->func->first;
//   while (basicblock) {
//     struct ir_stmt *stmt = basicblock->first;
//     while (stmt) {
//       struct ir_op *op = stmt->first;
//       while (op) {
//         if (op->ty == IR_OP_TY_PHI && op->phi.var) {
//           find_phi_exprs(builder, op);
//         }

//         op = op->succ;
//       }

//       stmt = stmt->succ;
//     }

//     basicblock = basicblock->succ;
//   }

//   basicblock = builder->func->first;
//   while (basicblock) {
//     struct ir_stmt *stmt = basicblock->first;
//     while (stmt) {
//       struct ir_op *op = stmt->first;
//       while (op) {
//         struct validate_metadata metadata = {.irb = builder, .consumer = op};

//         walk_op_uses(op, validate_op_tys_callback, &metadata);

//         op = op->succ;
//       }

//       stmt = stmt->succ;
//     }

//     basicblock = basicblock->succ;
//   }

//   return builder;
// }

// struct ir_var_value build_ir_for_zero_var(struct ir_unit *iru,
//                                           struct td_var_ty *var_ty) {
//   switch (var_ty->ty) {
//   case TD_VAR_TY_TY_UNKNOWN:
//   case TD_VAR_TY_TY_VOID:
//   case TD_VAR_TY_TY_VARIADIC:
//   case TD_VAR_TY_TY_FUNC:
//     bug("no sense");
//   case TD_VAR_TY_TY_WELL_KNOWN:
//   case TD_VAR_TY_TY_POINTER:
//   case TD_VAR_TY_TY_ARRAY:
//   case TD_VAR_TY_TY_TAGGED:
//   case TD_VAR_TY_TY_AGGREGATE:
//     return (struct ir_var_value){.var_ty = var_ty_for_td_var_ty(iru, var_ty)};
//   }
// }

// size_t get_member_index_offset(struct ir_unit *iru,
//                                const struct td_var_ty *var_ty,
//                                size_t member_index,
//                                struct td_var_ty *member_ty) {
//   if (var_ty->ty == TD_VAR_TY_TY_ARRAY) {
//     *member_ty = tyref_get_underlying(iru->parser, var_ty);
//     struct ir_op_var_ty el_ty = var_ty_for_td_var_ty(iru, member_ty);
//     struct ir_var_ty_info info = var_ty_info(iru, &el_ty);

//     return info.size * member_index;
//   } else {
//     debug_assert(var_ty->ty == TD_VAR_TY_TY_AGGREGATE ||
//                      var_ty->ty == TD_VAR_TY_TY_TAGGED,
//                  "bad type");

//     const char *member_name =
//         var_ty->aggregate.field_var_tys[member_index].name;
//     struct ir_op_var_ty ir_member_ty;
//     size_t member_offset;
//     size_t idx;
//     get_member_info(iru, var_ty, member_name, &ir_member_ty, &idx,
//                     &member_offset, member_ty);

//     return member_offset;
//   }
// }

// size_t get_designator_offset(struct ir_unit *iru,
//                              const struct td_var_ty *var_ty,
//                              struct td_designator *designator,
//                              size_t *member_index,
//                              struct td_var_ty *member_ty) {
//   size_t offset;

//   switch (designator->ty) {
//   case td_DESIGNATOR_TY_FIELD: {
//     const char *member_name = identifier_str(iru->parser, &designator->field);
//     struct ir_op_var_ty ir_member_ty;
//     size_t member_offset;
//     get_member_info(iru, var_ty, member_name, &ir_member_ty, member_index,
//                     &member_offset, member_ty);

//     offset = member_offset;
//     break;
//   }
//   case td_DESIGNATOR_TY_INDEX: {
//     *member_ty = tyref_get_underlying(iru->parser, var_ty);
//     struct ir_op_var_ty el_var_ty = var_ty_for_td_var_ty(iru, member_ty);
//     struct ir_var_ty_info info = var_ty_info(iru, &el_var_ty);

//     offset = info.size * designator->index;
//     *member_index = designator->index;
//     break;
//   default:
//   }
//   }

//   if (designator->next) {
//     size_t idx;
//     struct td_var_ty sub_member_ty;
//     offset += get_designator_offset(iru, member_ty, designator->next, &idx,
//                                     &sub_member_ty);
//   }

//   return offset;
// }

// struct ir_var_value build_ir_for_var_value(struct ir_unit *iru,
//                                            struct td_expr *expr,
//                                            struct td_var_ty *var_ty);
// struct ir_var_value
// build_ir_value_for_struct_initlist(struct ir_unit *iru,
//                                    struct td_initlist *init_list,
//                                    const struct td_var_ty *var_ty) {

//   // debug_assert(var_ty->ty == TD_VAR_TY_TY_AGGREGATE &&
//   //                  var_ty->aggregate.ty == TD_TY__AGGREGATE_TY_STRUCT,
//   //              "non stuct init list");

//   size_t num_elements = var_ty->ty == TD_VAR_TY_TY_ARRAY
//                             ? var_ty->array.size
//                             : var_ty->aggregate.num_field_var_tys;

//   if (!num_elements) {
//     bug("empty structs are GNU extension");
//   }

//   struct ir_var_value_list value_list = {
//       .num_values = init_list->num_inits,
//       .values = arena_alloc(iru->arena,
//                             sizeof(*value_list.values) * init_list->num_inits),
//       .offsets = arena_alloc(iru->arena, sizeof(*value_list.offsets) *
//                                              init_list->num_inits)};

//   size_t member_idx = 0;
//   for (size_t i = 0; i < num_elements; i++) {
//     debug_assert(i < num_elements, "too many items in struct init-list");

//     struct td_init *init = &init_list->inits[i];

//     size_t offset;
//     struct td_var_ty member_ty;
//     if (i < init_list->num_inits && init->designator) {
//       offset = get_designator_offset(
//           iru, var_ty, init_list->inits[i].designator, &member_idx, &member_ty);
//     } else {
//       offset = get_member_index_offset(iru, var_ty, member_idx, &member_ty);
//     }
//     member_idx++;

//     struct ir_var_value value;
//     if (i < init_list->num_inits) {
//       value = build_ir_for_var_value(iru, init_list->inits[i].expr, &member_ty);
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

// struct ir_var_value build_ir_for_var_value(struct ir_unit *iru,
//                                            struct td_expr *expr,
//                                            struct td_var_ty *var_ty) {
//   struct td_var_ty def_var_ty = *var_ty;
//   if (def_var_ty.ty == TD_VAR_TY_TY_TAGGED) {
//     def_var_ty = tyref_get_defined(iru->parser, var_ty);
//   }

//   switch (expr->ty) {
//   case TD_EXPR_TY_CNST: {
//     struct td_cnst *cnst = &expr->cnst;
//     if (is_integral_ty(&cnst->cnst_ty)) {
//       return (struct ir_var_value){
//           .ty = IR_VAR_VALUE_TY_INT,
//           .var_ty = var_ty_for_td_var_ty(iru, &cnst->cnst_ty),
//           .int_value = expr->cnst.int_value};
//     } else if (is_fp_ty(&cnst->cnst_ty)) {
//       return (struct ir_var_value){
//           .ty = IR_VAR_VALUE_TY_FLT,
//           .var_ty = var_ty_for_td_var_ty(iru, &cnst->cnst_ty),
//           .flt_value = expr->cnst.flt_value};
//     } else if (cnst->cnst_ty.ty == TD_VAR_TY_TY_POINTER) {
//       todo("ptr");
//       return (struct ir_var_value){
//           .ty = IR_VAR_VALUE_TY_FLT,
//           .var_ty = var_ty_for_td_var_ty(iru, &cnst->cnst_ty),
//           .str_value = expr->cnst.str_value};
//     } else {
//       bug("bad var ty");
//     }
//   }
//   case TD_EXPR_TY_INIT_LIST: {
//     return build_ir_value_for_struct_initlist(iru, &expr->init_list,
//                                               &def_var_ty);
//   }
//   default:
//     todo("other expr tys");
//   }
// }

// struct ir_unit *build_ir_for_translationunit(
//     /* needed for `associated_text */ struct parser *parser,
//     struct arena_allocator *arena,
//     struct td_translationunit *translation_unit) {

//   struct ir_unit *iru = arena_alloc(arena, sizeof(*iru));
//   *iru = (struct ir_unit){.arena = arena,
//                           .parser = parser,
//                           .first_global = NULL,
//                           .ltd_global = NULL,
//                           .num_globals = 0};

//   struct var_refs *global_var_refs = var_refs_create();
//   // funcs do not necessarily have a seperate decl so we do it for defs too

//   for (size_t i = 0; i < translation_unit->num_decl_lists; i++) {
//     struct td_decllist *decl_list = &translation_unit->decl_lists[i];

//     if (decl_list->storage_class_specifiers &
//         td_STORAGE_CLASS_SPECIFIER_FLAG_TYPEDEF) {
//       continue;
//     }

//     for (size_t j = 0; j < decl_list->num_decls; j++) {
//       struct td_decl *decl = &decl_list->decls[j];

//       build_ir_for_non_auto_var(iru, NULL, global_var_refs,
//                                 decl_list->storage_class_specifiers, decl);
//     }
//   }

//   for (size_t i = 0; i < translation_unit->num_func_defs; i++) {
//     struct td_funcdef *def = &translation_unit->func_defs[i];

//     struct td_decl decl = {.ty = td_DECL_TY_DECL,
//                             .var = {.ty = TD_VAR_TY_VAR,
//                                     .identifier = def->identifier,
//                                     .scope = SCOPE_GLOBAL,
//                                     .var_ty = def->var_ty}};
//     build_ir_for_non_auto_var(iru, NULL, global_var_refs,
//                               def->storage_class_specifiers, &decl);

//     struct ir_func_builder *builder =
//         build_ir_for_function(iru, arena, def, global_var_refs);

//     struct var_key key = {.name = identifier_str(parser, &def->identifier),
//                           .scope = SCOPE_GLOBAL};
//     struct var_ref *ref = var_refs_get(global_var_refs, &key);
//     ref->glb->def_ty = IR_GLB_DEF_TY_DEFINED;
//     ref->glb->func = builder->func;
//   }

//   // finally, we need to convert tentative definitions to real ones
//   struct ir_glb *glb = iru->first_global;
//   while (glb) {
//     if (glb->def_ty == IR_GLB_DEF_TY_TENTATIVE) {
//       debug_assert(glb->ty == IR_GLB_TY_DATA, "tentative func makes no sense");
//       glb->def_ty = IR_GLB_DEF_TY_DEFINED;
//       glb->var = arena_alloc(iru->arena, sizeof(*glb->var));
//       *glb->var = (struct ir_var){.ty = IR_VAR_TY_DATA,
//                                   .var_ty = glb->var_ty,
//                                   .value = {.ty = IR_VAR_VALUE_TY_ZERO}};
//     }

//     glb = glb->succ;
//   }

//   return iru;
// }


TODO_FUNC(struct ir_unit *build_ir_for_translationunit(
    /* needed for `associated_text */ struct parser *parser,
    struct arena_allocator *arena,
    struct ast_translationunit *translation_unit))
