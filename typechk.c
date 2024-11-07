#include "typechk.h"

#include "alloc.h"
#include "parse.h"
#include "var_table.h"

#include <mach/kern_return.h>

struct typechk {
  struct arena_allocator *arena;

  // `value` contains a `struct td_var_ty *` to the type of the variable
  // or NULL if the variable has been used without a declaration
  struct var_table var_table;

  // types (e.g declared structs)
  struct var_table ty_table;
};

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
  case TD_BINARY_OP_TY_QUOT:
    return false;
  }
}
bool is_fp_ty(const struct td_var_ty *ty) {
  if (ty->ty != TD_VAR_TY_TY_WELL_KNOWN) {
    return false;
  }

  switch (ty->well_known) {
  case WELL_KNOWN_TY_HALF:
  case WELL_KNOWN_TY_FLOAT:
  case WELL_KNOWN_TY_DOUBLE:
  case WELL_KNOWN_TY_LONG_DOUBLE:
    return true;

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
    return false;
  }
}

bool is_integral_ty(const struct td_var_ty *ty) {
  if (ty->ty != TD_VAR_TY_TY_WELL_KNOWN) {
    return false;
  }

  switch (ty->well_known) {
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
    return true;

  case WELL_KNOWN_TY_HALF:
  case WELL_KNOWN_TY_FLOAT:
  case WELL_KNOWN_TY_DOUBLE:
  case WELL_KNOWN_TY_LONG_DOUBLE:
    return false;
  }
}

struct td_var_ty td_var_ty_pointer_sized_int(struct typechk *tchk,
                                             bool is_signed) {
  UNUSED_ARG(tchk);

  // returns the type for `size_t` effectively
  // TODO: generalise - either we should have a special ptr-sized int type, or
  // tchk should have a field for ptr size
  return (struct td_var_ty){.ty = TD_VAR_TY_TY_WELL_KNOWN,
                            .well_known =
                                is_signed ? WELL_KNOWN_TY_SIGNED_LONG_LONG
                                          : WELL_KNOWN_TY_UNSIGNED_LONG_LONG};
}

struct td_var_ty td_var_ty_make_pointer(struct typechk *tchk,
                                        const struct td_var_ty *var_ty) {
  UNUSED_ARG(tchk);

  // we don't know lifetime of the other one so need to copy it
  // TODO: cache types
  struct td_var_ty *copied = arena_alloc(tchk->arena, sizeof(*copied));
  *copied = *var_ty;

  return (struct td_var_ty){.ty = TD_VAR_TY_TY_POINTER,
                            .pointer =
                                (struct td_ty_pointer){.underlying = copied}};
}

struct td_var_ty td_var_ty_get_underlying(struct typechk *tchk,
                                          const struct td_var_ty *ty_ref) {
  UNUSED_ARG(tchk);

  switch (ty_ref->ty) {
  case TD_VAR_TY_TY_POINTER:
    return *ty_ref->pointer.underlying;
  case TD_VAR_TY_TY_ARRAY:
    return *ty_ref->array.underlying;
  default:
    bug("non pointer/array/tagged passed (type %d)", ty_ref->ty);
  }
}

struct td_var_ty td_var_ty_promote_integer(struct typechk *tchk,
                                           const struct td_var_ty *ty_ref) {
  UNUSED_ARG(tchk);

  debug_assert(ty_ref->ty != TD_VAR_TY_TY_UNKNOWN, "unknown ty in call to `%s`",
               __func__);

  if (ty_ref->ty != TD_VAR_TY_TY_WELL_KNOWN ||
      ty_ref->well_known >= WELL_KNOWN_TY_SIGNED_INT) {
    return *ty_ref;
  }

  // all values smaller than int are promoted to int
  return (struct td_var_ty){.ty = TD_VAR_TY_TY_WELL_KNOWN,
                            .well_known = WELL_KNOWN_TY_SIGNED_INT};
}

struct td_var_ty resolve_unary_op_types(struct typechk *tchk,
                                        enum td_unary_op_ty ty,
                                        const struct td_var_ty *var_ty,
                                        const struct td_cast *cast) {
  switch (ty) {
  case TD_UNARY_OP_TY_PLUS:
  case TD_UNARY_OP_TY_MINUS:
  case TD_UNARY_OP_TY_NOT:
    // these undergo promotion
    return td_var_ty_promote_integer(tchk, var_ty);
    break;
  case TD_UNARY_OP_TY_LOGICAL_NOT:
    // logical not always results in `int`
    return (struct td_var_ty){.ty = TD_VAR_TY_TY_WELL_KNOWN,
                              .well_known = WELL_KNOWN_TY_SIGNED_INT};
    break;
  case TD_UNARY_OP_TY_INDIRECTION:
    return td_var_ty_get_underlying(tchk, var_ty);
  case TD_UNARY_OP_TY_SIZEOF:
  case TD_UNARY_OP_TY_ALIGNOF:
    todo("type of sizeof/alignof. should be size_t, which is varying per-arch "
         "(pointer-sized int)");
    break;
  case TD_UNARY_OP_TY_ADDRESSOF:
    return td_var_ty_make_pointer(tchk, var_ty);
  case TD_UNARY_OP_TY_CAST:
    debug_assert(cast, "no cast provided but unary op ty was cast in `%s`",
                 __func__);
    return cast->var_ty;
  case TD_UNARY_OP_TY_PREFIX_INC:
  case TD_UNARY_OP_TY_PREFIX_DEC:
  case TD_UNARY_OP_TY_POSTFIX_INC:
  case TD_UNARY_OP_TY_POSTFIX_DEC:
    // these do not change type
    return *var_ty;
  }
}

struct td_var_ty resolve_binary_op_intermediate_types(
    struct typechk *tchk, enum td_binary_op_ty ty, const struct td_var_ty *lhs,
    const struct td_var_ty *rhs) {
  debug_assert(lhs->ty != TD_VAR_TY_TY_UNKNOWN &&
                   rhs->ty != TD_VAR_TY_TY_UNKNOWN,
               "unknown ty in call to `%s`", __func__);

  if (lhs->ty == TD_VAR_TY_TY_POINTER || rhs->ty == TD_VAR_TY_TY_POINTER) {
    const struct td_var_ty *pointer_ty =
        lhs->ty == TD_VAR_TY_TY_POINTER ? lhs : rhs;

    if (td_binary_op_is_comparison(ty)) {
      return td_var_ty_pointer_sized_int(tchk, false);
    }

    switch (ty) {
    case AST_BINARY_OP_TY_ADD:
      return *pointer_ty;
    case AST_BINARY_OP_TY_SUB:
      // ptrdiff is signed
      return (lhs->ty == TD_VAR_TY_TY_POINTER &&
              rhs->ty == TD_VAR_TY_TY_POINTER)
                 ? td_var_ty_pointer_sized_int(tchk, true)
                 : *pointer_ty;
    default:
      bug("bad op for poiner op");
    }
  }

  if (lhs->ty != TD_VAR_TY_TY_WELL_KNOWN ||
      rhs->ty != TD_VAR_TY_TY_WELL_KNOWN) {
    todo("`%s` for types other than well known", __func__);
  }

  struct td_var_ty result_ty;
  result_ty.ty = TD_VAR_TY_TY_WELL_KNOWN;

  struct td_var_ty lhs_ty = td_var_ty_promote_integer(tchk, lhs);
  struct td_var_ty rhs_ty = td_var_ty_promote_integer(tchk, rhs);

  if (ty == AST_BINARY_OP_TY_LSHIFT || ty == AST_BINARY_OP_TY_RSHIFT) {
    // these do not undergo "Usual arithmetic conversions"
    // and are just typed by the lhs
    return lhs_ty;
  }

  if (lhs_ty.well_known == rhs_ty.well_known) {
    // they are the same type
    result_ty.well_known = lhs_ty.well_known;
  } else {
    enum well_known_ty signed_lhs = WKT_MAKE_SIGNED(lhs_ty.well_known);
    enum well_known_ty signed_rhs = WKT_MAKE_SIGNED(rhs_ty.well_known);

    if (signed_lhs != signed_rhs) {
      // one is bigger than other
      // type of expression is simply the larger type
      result_ty.well_known = MAX(signed_lhs, signed_rhs);
    } else {
      // they are the same size
      // the unsigned one is chosen (C spec dictates)
      result_ty.well_known = WKT_MAKE_UNSIGNED(signed_lhs);
    }
  }

  return result_ty;
}

// bool parse_wkt(struct parser *parser, struct td_var_ty *ty_ref) {
//   struct text_pos pos = get_position(parser->lexer);

//   struct token token;
//   peek_token(parser->lexer, &token);

//   if (token.ty == LEX_TOKEN_TY_KW_VOID) {
//     consume_token(parser->lexer, token);

//     ty_ref->ty = TD_VAR_TY_TY_VOID;
//     return true;
//   }

//   bool seen_unsigned = false;
//   bool seen_signed = false;
//   if (token.ty == LEX_TOKEN_TY_KW_SIGNED) {
//     seen_signed = true;

//     consume_token(parser->lexer, token);
//   } else if (token.ty == LEX_TOKEN_TY_KW_UNSIGNED) {
//     seen_unsigned = true;

//     consume_token(parser->lexer, token);
//   }

//   bool enough_type_info =
//       seen_signed ||
//       seen_unsigned; // `signed` or `unsigned` is a type in itself

//   enum well_known_ty wkt;
//   if (!parse_wkt_item(parser, &wkt)) {
//     if (enough_type_info) {
//       wkt = seen_signed ? WELL_KNOWN_TY_SIGNED_INT :
//       WELL_KNOWN_TY_UNSIGNED_INT;
//     } else {
//       backtrack(parser->lexer, pos);
//       return false;
//     }
//   } else {
//     if (seen_unsigned) {
//       wkt = WKT_MAKE_UNSIGNED(wkt);
//     } else if (!seen_signed && !seen_unsigned &&
//                wkt == WELL_KNOWN_TY_SIGNED_CHAR) {
//       wkt = WELL_KNOWN_TY_SIGNED_CHAR;
//     }
//   }

//   ty_ref->ty = TD_VAR_TY_TY_WELL_KNOWN;
//   ty_ref->well_known = wkt;

//   return true;
// }

struct assg_ty_map {
  enum lex_token_ty token_ty;
  enum ast_assg_ty assg_ty;
  enum ast_binary_op_ty binary_op_ty;
};

// const struct assg_ty_map ASSG_TOKENS[11] = {
//     {LEX_TOKEN_TY_OP_ASSG, AST_ASSG_TY_SIMPLEASSG, 0},
//     {LEX_TOKEN_TY_OP_ADD_ASSG, AST_ASSG_TY_COMPOUNDASSG,
//     AST_BINARY_OP_TY_ADD}, {LEX_TOKEN_TY_OP_DIV_ASSG,
//     AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_DIV},
//     {LEX_TOKEN_TY_OP_MUL_ASSG, AST_ASSG_TY_COMPOUNDASSG,
//     AST_BINARY_OP_TY_MUL}, {LEX_TOKEN_TY_OP_SUB_ASSG,
//     AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_SUB},
//     {LEX_TOKEN_TY_OP_QUOT_ASSG, AST_ASSG_TY_COMPOUNDASSG,
//      AST_BINARY_OP_TY_QUOT},

//     {LEX_TOKEN_TY_OP_LSHIFT_ASSG, AST_ASSG_TY_COMPOUNDASSG,
//      AST_BINARY_OP_TY_LSHIFT},
//     {LEX_TOKEN_TY_OP_RSHIFT_ASSG, AST_ASSG_TY_COMPOUNDASSG,
//      AST_BINARY_OP_TY_RSHIFT},
//     {LEX_TOKEN_TY_OP_AND_ASSG, AST_ASSG_TY_COMPOUNDASSG,
//     AST_BINARY_OP_TY_AND}, {LEX_TOKEN_TY_OP_OR_ASSG,
//     AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_OR},
//     {LEX_TOKEN_TY_OP_XOR_ASSG, AST_ASSG_TY_COMPOUNDASSG,
//     AST_BINARY_OP_TY_XOR},
// };

// bool try_resolve_member_access_ty(struct td_var_ty base_ty,
//                                   const char *member_name,
//                                   struct td_var_ty *ty_ref) {
//   // TODO: super slow hashtable needed
//   for (size_t i = 0; i < base_ty.aggregate.num_fields; i++) {
//     const struct ast_struct_field *field = &base_ty.aggregate.fields[i];
//     if (field->name == NULL) {
//       if (try_resolve_member_access_ty(*field->var_ty, member_name, ty_ref))
//       {
//         return true;
//       }
//     }

//     if (strcmp(field->name, member_name) == 0) {
//       *ty_ref = *field->var_ty;
//       return true;
//     }
//   }

//   return false;
// }

// struct td_var_ty resolve_member_access_ty(struct parser *parser,
//                                           struct td_var_ty *var_ty,
//                                           const struct token *member) {
//   invariant_assert(var_ty->ty == TD_VAR_TY_TY_AGGREGATE ||
//                        var_ty->ty == TD_VAR_TY_TY_TAGGED,
//                    "non struct/union in member access");

//   struct td_var_ty base_ty;

//   // incomplete type, look it up now it is defined
//   if (var_ty->ty == TD_VAR_TY_TY_TAGGED) {
//     base_ty = td_var_ty_get_defined(parser, var_ty);
//   } else {
//     base_ty = *var_ty;
//   }

//   *var_ty = base_ty;

//   const char *member_name = identifier_str(parser, member);

//   struct td_var_ty access_ty;
//   if (try_resolve_member_access_ty(base_ty, member_name, &access_ty)) {
//     return access_ty;
//   }

//   todo("member '%s' does not exist", member_name);
// }

// struct td_var_ty resolve_pointer_access_ty(struct parser *parser,
//                                            const struct td_var_ty *var_ty,
//                                            const struct token *member) {

//   struct td_var_ty underlying_var_ty = td_var_ty_get_underlying(parser,
//   var_ty); struct td_var_ty base_ty =
//       underlying_var_ty.ty == TD_VAR_TY_TY_TAGGED
//           ? td_var_ty_get_defined(parser, &underlying_var_ty)
//           : underlying_var_ty;
//   return resolve_member_access_ty(parser, &base_ty, member);
// }

// struct td_var_ty resolve_array_access_ty(struct parser *parser,
//                                          const struct ast_expr *lhs,
//                                          const struct ast_expr *rhs,
//                                          bool *lhs_is_pointer) {
//   if (lhs->var_ty.ty == TD_VAR_TY_TY_POINTER ||
//       lhs->var_ty.ty == TD_VAR_TY_TY_ARRAY) {
//     *lhs_is_pointer = true;
//     return td_var_ty_get_underlying(parser, &lhs->var_ty);
//   } else {
//     *lhs_is_pointer = false;
//     return td_var_ty_get_underlying(parser, &rhs->var_ty);
//   }
// }

// void decay_array_params(struct ast_param *param) {
//   if (param->var_ty.ty == TD_VAR_TY_TY_ARRAY) {
//     param->var_ty.ty = TD_VAR_TY_TY_POINTER;
//     param->var_ty.pointer.underlying = param->var_ty.array.element;
//   }
// }

struct td_specifiers {
  enum td_storage_class_specifier storage;
  enum td_function_specifier function;
  enum td_type_qualifier_flags qualifier_flags;
  struct td_var_ty type_specifier;
};

enum td_specifier_allow {
  TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS = 1,
  TD_SPECIFIER_ALLOW_STORAGE_CLASS_SPECIFIERS = 2,
  TD_SPECIFIER_ALLOW_FUNCTION_SPECIFIERS = 4,
  TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS = 4,
};

static struct td_specifiers
condense_specifiers(struct typechk *tchk,
                    const struct ast_declaration_specifier_list *list,
                    enum td_specifier_allow allow);

// TODO:
#define WARN(s) bug("%s", s)

enum sign_state { SIGN_STATE_NONE, SIGN_STATE_SIGNED, SIGN_STATE_UNSIGNED };

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

  for (size_t i = 0; i < specifier->struct_decl_list.num_struct_declarations;
       i++) {
    // const struct ast_struct_declaration_list *decl_list =
    // &specifier->struct_decl_list.struct_declarations[i];

    // for (size_t j = 0; j <
    // specifier->struct_decl_list.num_struct_declarations; j++) {

    // }
  }
}

static struct td_var_ty td_var_ty_for_typedef(struct typechk *tchk,
                                              const struct token *identifier) {
  UNUSED_ARG(tchk);
  UNUSED_ARG(identifier);
  todo("typedefs");
}

// represents an in-process build of a var_ty
// e.g in `int (**foo)`, the processing of `(**foo)` leads to a partial ptr ty
// where `int` will be written to `underlying`
struct td_partial_var_ty {
  struct td_var_ty var_ty;
  struct td_var_ty *underlying;
};

static struct td_var_declaration
type_declarator(struct typechk *tchk, struct td_var_ty declaration,
                struct ast_declarator *declarator) {
  struct td_var_declaration var_decl;

  struct td_var_ty var_ty = declaration;
  struct td_var_ty *inner = &var_ty;

  struct ast_pointer_list pointer_list = declarator->pointer_list;
  struct ast_direct_declarator_list decl_list =
      declarator->direct_declarator_list;

  for (size_t i = 0; i < pointer_list.num_pointers; i++) {
    struct ast_pointer *pointer = &pointer_list.pointers[i];
    struct td_specifiers specifiers = condense_specifiers(
        tchk, &pointer->specifier_list, TD_SPECIFIER_ALLOW_TYPE_QUALIFIERS);

    var_ty = td_var_ty_make_pointer(tchk, &var_ty, specifiers.qualifier_flags);
  }

  bool found_ident = false;

  for (size_t i = decl_list.num_direct_declarators; i; i--) {
    struct ast_direct_declarator *direct_declarator =
        &decl_list.direct_declarators[i - 1];

    switch (direct_declarator->ty) {
    case AST_DIRECT_DECLARATOR_TY_IDENTIFIER:
      var_decl.identifier = direct_declarator->identifier;
      found_ident = true;
      break;
    case AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR:
      break;
    case AST_DIRECT_DECLARATOR_TY_ARRAY_DECLARATOR:
      break;
    case AST_DIRECT_DECLARATOR_TY_FUNC_DECLARATOR: {
      struct td_var_ty func_ty = {
          .ty = TD_VAR_TY_TY_FUNC,
      };
      func_ty.func.num_params =
          direct_declarator->func_declarator->param_list->num_params;
      // func_ty.func.param_var_tys =
      // direct_declarator->func_declarator->param_list->params;
      break;
    }
    }
  }

  debug_assert(found_ident, "decl without identifier?");
}

static struct td_specifiers
condense_specifiers(struct typechk *tchk,
                    const struct ast_declaration_specifier_list *list,
                    enum td_specifier_allow allow) {
  struct td_specifiers specifiers = {
      .storage = TD_STORAGE_CLASS_SPECIFIER_NONE,
      .function = TD_FUNCTION_SPECIFIER_NONE,
      .qualifier_flags = TD_TYPE_QUALIFIER_FLAG_NONE,
  };

  int long_count = 0, int_count = 0, signed_count = 0, unsigned_count = 0;
  int type_specifier_count = 0;

  struct ast_type_specifier last_specifier;

  for (size_t i = 0; i < list->num_decl_specifiers; i++) {
    struct ast_declaration_specifier specifier = list->decl_specifiers[i];

    switch (specifier.ty) {
    case AST_DECL_SPECIFIER_TY_STORAGE_CLASS_SPECIFIER:
      if (!(allow & TD_SPECIFIER_ALLOW_STORAGE_CLASS_SPECIFIERS)) {
        WARN("storage class specifier not valid");
      }

      if (specifiers.storage != TD_STORAGE_CLASS_SPECIFIER_NONE) {
        WARN("multiple storage specifiers");
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
        WARN("type qualifier not valid");
      }

      switch (specifier.type_qualifier) {
      case AST_TYPE_QUALIFIER_CONST:
        if (specifiers.qualifier_flags & TD_TYPE_QUALIFIER_FLAG_CONST) {
          WARN("duplicate const flag");
        }

        specifiers.qualifier_flags |= TD_TYPE_QUALIFIER_FLAG_CONST;
        break;
      case AST_TYPE_QUALIFIER_VOLATILE:
        if (specifiers.qualifier_flags & TD_TYPE_QUALIFIER_FLAG_VOLATILE) {
          WARN("duplicate volatile flag");
        }

        specifiers.qualifier_flags |= TD_TYPE_QUALIFIER_FLAG_VOLATILE;
        break;
      }
      break;
    case AST_DECL_SPECIFIER_TY_FUNCTION_SPECIFIER:
      if (!(allow & TD_SPECIFIER_ALLOW_FUNCTION_SPECIFIERS)) {
        WARN("function specifier not valid");
      }

      if (specifiers.function != TD_FUNCTION_SPECIFIER_NONE) {
        WARN("multiple function specifiers");
      }

      switch (specifier.function_specifier) {
      case AST_FUNCTION_SPECIFIER_INLINE:
        specifiers.function = TD_FUNCTION_SPECIFIER_INLINE;
        break;
      }
    case AST_DECL_SPECIFIER_TY_TYPE_SPECIFIER: {
      if (!(allow & TD_SPECIFIER_ALLOW_TYPE_SPECIFIERS)) {
        WARN("type specifier not valid");
      }

      if (specifier.type_specifier.ty == AST_TYPE_SPECIFIER_TY_KW) {
        enum ast_type_specifier_kw kw =
            specifier.type_specifier.type_specifier_kw;
        if (kw == AST_TYPE_SPECIFIER_KW_INT) {
          int_count++;
        } else if (kw == AST_TYPE_SPECIFIER_KW_LONG) {
          long_count++;
        } else if (kw == AST_TYPE_SPECIFIER_KW_SIGNED) {
          signed_count++;
        } else if (kw == AST_TYPE_SPECIFIER_KW_UNSIGNED) {
          unsigned_count++;
        } else {
          last_specifier = specifier.type_specifier;
          type_specifier_count++;
        }
      } else {
        last_specifier = specifier.type_specifier;
        type_specifier_count++;
      }
    }
    }
  }

  if (int_count > 1) {
    WARN("multiple int");
  }

  if (signed_count > 1) {
    WARN("multiple signed");
  }

  if (unsigned_count > 1) {
    WARN("multiple unsigned");
  }

  if (signed_count && unsigned_count) {
    WARN("signed and unsigned");
  }

  int total_modifiers = int_count + long_count + signed_count + unsigned_count;
  int remaining = type_specifier_count - total_modifiers;

  if (remaining > 1) {
    WARN("multiple type specifiers did not make sense");
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
               last_specifier.ty == AST_TYPE_SPECIFIER_KW_DOUBLE) {
      wk = WELL_KNOWN_TY_LONG_DOUBLE;
    } else if (last_specifier.ty == AST_TYPE_SPECIFIER_TY_KW &&
               last_specifier.ty == AST_TYPE_SPECIFIER_KW_CHAR) {
      wk = unsigned_count ? WELL_KNOWN_TY_UNSIGNED_CHAR
                          : WELL_KNOWN_TY_SIGNED_CHAR;
    } else if (last_specifier.ty == AST_TYPE_SPECIFIER_TY_KW &&
               last_specifier.ty == AST_TYPE_SPECIFIER_KW_SHORT) {
      wk = unsigned_count ? WELL_KNOWN_TY_UNSIGNED_SHORT
                          : WELL_KNOWN_TY_SIGNED_SHORT;
    }

    specifiers.type_specifier =
        (struct td_var_ty){.ty = TD_VAR_TY_TY_WELL_KNOWN, .well_known = wk};
  } else {
    if (last_specifier.ty == AST_TYPE_SPECIFIER_TY_KW &&
        last_specifier.type_specifier_kw == AST_TYPE_SPECIFIER_KW_VOID) {
      specifiers.type_specifier = (struct td_var_ty){.ty = TD_VAR_TY_TY_VOID};

    } else {
      switch (last_specifier.ty) {
      case AST_TYPE_SPECIFIER_TY_KW: {
        enum well_known_ty wk = WELL_KNOWN_TY_SIGNED_INT;

        switch (last_specifier.type_specifier_kw) {
        case AST_TYPE_SPECIFIER_KW_CHAR:
          wk = WELL_KNOWN_TY_SIGNED_CHAR;
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
          todo("bool");
          // wk = WELL_KNOWN_TY_BOOL;
          break;
        case AST_TYPE_SPECIFIER_KW_COMPLEX:
          todo("complex");
          // wk = WELL_KNOWN_TY_COMPLEX;
          break;
        case AST_TYPE_SPECIFIER_KW_HALF:
          wk = WELL_KNOWN_TY_HALF;
          break;
        default:
          unreachable("already handled");
        }

        specifiers.type_specifier =
            (struct td_var_ty){.ty = TD_VAR_TY_TY_WELL_KNOWN, .well_known = wk};
        break;
      }
      case AST_TYPE_SPECIFIER_STRUCT_OR_UNION:
        specifiers.type_specifier = td_var_ty_for_struct_or_union(
            tchk, &last_specifier.struct_or_union_specifier);
        break;
      case AST_TYPE_SPECIFIER_ENUM:
        specifiers.type_specifier =
            (struct td_var_ty){.ty = TD_VAR_TY_TY_WELL_KNOWN,
                               .well_known = WELL_KNOWN_TY_SIGNED_INT};
        break;
      case AST_TYPE_SPECIFIER_TYPEDEF_NAME:
        specifiers.type_specifier =
            td_var_ty_for_typedef(tchk, &last_specifier.typedef_name);
        break;
      }
    }
  }

  return specifiers;
}

struct td_var_ty resolve_ternary_ty(struct parser *parser,
                                    struct td_var_ty *lhs,
                                    struct td_var_ty *rhs) {
  UNUSED_ARG(parser);
  UNUSED_ARG(rhs);

  // FIXME: do logic
  return *lhs;
}

void tchk_push_scope(struct typechk *tchk) {
  push_scope(&tchk->var_table);
  push_scope(&tchk->ty_table);
}

void tchk_pop_scope(struct typechk *tchk) {
  pop_scope(&tchk->var_table);
  pop_scope(&tchk->ty_table);
}

struct td_stmt type_stmt(struct typechk *tchk, const struct ast_stmt *stmt) {
  switch (stmt->ty) {}
}

struct td_compoundstmt
type_compoundstmt(struct typechk *tchk,
                  const struct ast_compoundstmt *compound_stmt) {
  struct td_compoundstmt td_cmpd = {
      .num_stmts = compound_stmt->num_stmts,
      .stmts = arena_alloc(tchk->arena,
                           sizeof(*td_cmpd.stms) * compound_stmt->num_stmts)};

  for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
    td_cmpd.stmts[i] = type_stmt(tchk, &compound_stmt->stmts[i]);
  }

  return td_cmpd;
}

struct td_funcdef type_funcdef(struct typechk *tchk,
                               const struct ast_funcdef *func_def) {
  if (func_def->declaration_list.num_declarations) {
    bug("old-style function arguments not currently supported");
  }

  struct td_funcdef td_func_def = {.func_ty = };

  type_compoundstmt(tchk, &func_def->body);
}

struct td_declaration
type_declaration(struct typechk *tchk,
                 const struct ast_declaration *declaration) {}

struct td_external_declaration type_external_declaration(
    struct typechk *tchk,
    const struct ast_external_declaration *external_declaration) {
  switch (external_declaration->ty) {
  case AST_EXTERNAL_DECLARATION_TY_DECLARATION:
    return (struct td_external_declaration){
        .ty = TD_EXTERNAL_DECLARATION_TY_DECLARATION,
        .declaration =
            type_declaration(tchk, &external_declaration->declaration)};
  case AST_EXTERNAL_DECLARATION_TY_FUNC_DEF:
    return (struct td_external_declaration){
        .ty = TD_EXTERNAL_DECLARATION_TY_DECLARATION,
        .declaration = type_funcdef(tchk, &external_declaration->func_def)};
  }
}

struct td_translationunit
td_typchk(struct ast_translationunit *translation_unit) {
  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct typechk tchk = {
      .arena = arena,
      .ty_table = var_table_create(arena),
      .var_table = var_table_create(arena),
  };

  struct td_translationunit td_translation_unit = {
      .num_external_declarations = translation_unit->num_external_declarations,
      .external_declarations = arena_alloc(
          tchk.arena, sizeof(*td_translation_unit.external_declarations) *
                          translation_unit->num_external_declarations)};

  for (size_t i = 0; i < translation_unit->num_external_declarations; i++) {
    td_translation_unit.external_declarations[i] = type_external_declaration(
        &tchk, &translation_unit->external_declarations[i]);
  }

  return td_translation_unit;
}
