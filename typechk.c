#include "typechk.h"

void ast_typchk(struct ast_translationunit *translation_unit) {
  UNUSED_ARG(translation_unit);
}


bool is_fp_ty(const struct ast_tyref *ty) {
  if (ty->ty != AST_TYREF_TY_WELL_KNOWN) {
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

bool is_integral_ty(const struct ast_tyref *ty) {
  if (ty->ty != AST_TYREF_TY_WELL_KNOWN) {
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


struct ast_tyref tyref_pointer_sized_int(struct parser *parser,
                                         bool is_signed) {
  UNUSED_ARG(parser);

  // returns the type for `size_t` effectively
  // TODO: generalise - either we should have a special ptr-sized int type, or
  // parser should have a field for ptr size
  return (struct ast_tyref){.ty = AST_TYREF_TY_WELL_KNOWN,
                            .well_known =
                                is_signed ? WELL_KNOWN_TY_SIGNED_LONG_LONG
                                          : WELL_KNOWN_TY_UNSIGNED_LONG_LONG};
}

struct ast_tyref tyref_make_pointer(struct parser *parser,
                                    const struct ast_tyref *var_ty) {
  UNUSED_ARG(parser);

  // we don't know lifetime of the other one so need to copy it
  // TODO: cache types
  struct ast_tyref *copied = arena_alloc(parser->arena, sizeof(*copied));
  *copied = *var_ty;

  return (struct ast_tyref){.ty = AST_TYREF_TY_POINTER,
                            .pointer =
                                (struct ast_ty_pointer){.underlying = copied}};
}

struct ast_tyref tyref_get_defined(struct parser *parser,
                                   const struct ast_tyref *ty_ref) {
  UNUSED_ARG(parser);

  debug_assert(ty_ref->ty == AST_TYREF_TY_TAGGED, "non tagged");

  if (ty_ref->tagged.underlying) {
    return *ty_ref->tagged.underlying;
  }

  struct var_table_entry *entry =
      get_entry(&parser->ty_table, ty_ref->tagged.name);
  invariant_assert(entry, "expected aggregate to be defined");
  return *entry->value;
}

struct ast_tyref tyref_get_underlying(struct parser *parser,
                                      const struct ast_tyref *ty_ref) {
  UNUSED_ARG(parser);

  switch (ty_ref->ty) {
  case AST_TYREF_TY_POINTER:
    return *ty_ref->pointer.underlying;
  case AST_TYREF_TY_ARRAY:
    return *ty_ref->array.element;
  default:
    bug("non pointer/array/tagged passed (type %d)", ty_ref->ty);
  }
}

struct ast_tyref tyref_promote_integer(struct parser *parser,
                                       const struct ast_tyref *ty_ref) {
  UNUSED_ARG(parser);

  debug_assert(ty_ref->ty != AST_TYREF_TY_UNKNOWN, "unknown ty in call to `%s`",
               __func__);

  if (ty_ref->ty != AST_TYREF_TY_WELL_KNOWN ||
      ty_ref->well_known >= WELL_KNOWN_TY_SIGNED_INT) {
    return *ty_ref;
  }

  // all values smaller than int are promoted to int
  return (struct ast_tyref){.ty = AST_TYREF_TY_WELL_KNOWN,
                            .well_known = WELL_KNOWN_TY_SIGNED_INT};
}

struct ast_tyref resolve_unary_op_types(struct parser *parser,
                                        enum ast_unary_op_ty ty,
                                        const struct ast_tyref *var_ty,
                                        const struct ast_cast *cast) {
  switch (ty) {
  case AST_UNARY_OP_TY_PLUS:
  case AST_UNARY_OP_TY_MINUS:
  case AST_UNARY_OP_TY_NOT:
    // these undergo promotion
    return tyref_promote_integer(parser, var_ty);
    break;
  case AST_UNARY_OP_TY_LOGICAL_NOT:
    // logical not always results in `int`
    return (struct ast_tyref){.ty = AST_TYREF_TY_WELL_KNOWN,
                              .well_known = WELL_KNOWN_TY_SIGNED_INT};
    break;
  case AST_UNARY_OP_TY_INDIRECTION:
    return tyref_get_underlying(parser, var_ty);
  case AST_UNARY_OP_TY_SIZEOF:
  case AST_UNARY_OP_TY_ALIGNOF:
    todo("type of sizeof/alignof. should be size_t, which is varying per-arch "
         "(pointer-sized int)");
    break;
  case AST_UNARY_OP_TY_ADDRESSOF:
    return tyref_make_pointer(parser, var_ty);
  case AST_UNARY_OP_TY_CAST:
    debug_assert(cast, "no cast provided but unary op ty was cast in `%s`",
                 __func__);
    return cast->cast_ty;
  case AST_UNARY_OP_TY_PREFIX_INC:
  case AST_UNARY_OP_TY_PREFIX_DEC:
  case AST_UNARY_OP_TY_POSTFIX_INC:
  case AST_UNARY_OP_TY_POSTFIX_DEC:
    // these do not change type
    return *var_ty;
  }
}


struct ast_tyref resolve_binary_op_intermediate_types(
    struct parser *parser, enum ast_binary_op_ty ty,
    const struct ast_tyref *lhs, const struct ast_tyref *rhs) {
  debug_assert(lhs->ty != AST_TYREF_TY_UNKNOWN &&
                   rhs->ty != AST_TYREF_TY_UNKNOWN,
               "unknown ty in call to `%s`", __func__);

  if (lhs->ty == AST_TYREF_TY_POINTER || rhs->ty == AST_TYREF_TY_POINTER) {
    const struct ast_tyref *pointer_ty =
        lhs->ty == AST_TYREF_TY_POINTER ? lhs : rhs;

    if (ast_binary_op_is_comparison(ty)) {
      return tyref_pointer_sized_int(parser, false);
    }

    switch (ty) {
    case AST_BINARY_OP_TY_ADD:
      return *pointer_ty;
    case AST_BINARY_OP_TY_SUB:
      // ptrdiff is signed
      return (lhs->ty == AST_TYREF_TY_POINTER &&
              rhs->ty == AST_TYREF_TY_POINTER)
                 ? tyref_pointer_sized_int(parser, true)
                 : *pointer_ty;
    default:
      bug("bad op for poiner op");
    }
  }

  if (lhs->ty != AST_TYREF_TY_WELL_KNOWN ||
      rhs->ty != AST_TYREF_TY_WELL_KNOWN) {
    todo("`%s` for types other than well known", __func__);
  }

  struct ast_tyref result_ty;
  result_ty.ty = AST_TYREF_TY_WELL_KNOWN;

  struct ast_tyref lhs_ty = tyref_promote_integer(parser, lhs);
  struct ast_tyref rhs_ty = tyref_promote_integer(parser, rhs);

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


bool parse_wkt(struct parser *parser, struct ast_tyref *ty_ref) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TY_KW_VOID) {
    consume_token(parser->lexer, token);

    ty_ref->ty = AST_TYREF_TY_VOID;
    return true;
  }

  bool seen_unsigned = false;
  bool seen_signed = false;
  if (token.ty == LEX_TOKEN_TY_KW_SIGNED) {
    seen_signed = true;

    consume_token(parser->lexer, token);
  } else if (token.ty == LEX_TOKEN_TY_KW_UNSIGNED) {
    seen_unsigned = true;

    consume_token(parser->lexer, token);
  }

  bool enough_type_info =
      seen_signed ||
      seen_unsigned; // `signed` or `unsigned` is a type in itself

  enum well_known_ty wkt;
  if (!parse_wkt_item(parser, &wkt)) {
    if (enough_type_info) {
      wkt = seen_signed ? WELL_KNOWN_TY_SIGNED_INT : WELL_KNOWN_TY_UNSIGNED_INT;
    } else {
      backtrack(parser->lexer, pos);
      return false;
    }
  } else {
    if (seen_unsigned) {
      wkt = WKT_MAKE_UNSIGNED(wkt);
    } else if (!seen_signed && !seen_unsigned &&
               wkt == WELL_KNOWN_TY_SIGNED_CHAR) {
      wkt = WELL_KNOWN_TY_SIGNED_CHAR;
    }
  }

  ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
  ty_ref->well_known = wkt;

  return true;
}

struct assg_ty_map {
  enum lex_token_ty token_ty;
  enum ast_assg_ty assg_ty;
  enum ast_binary_op_ty binary_op_ty;
};

const struct assg_ty_map ASSG_TOKENS[11] = {
    {LEX_TOKEN_TY_OP_ASSG, AST_ASSG_TY_SIMPLEASSG, 0},
    {LEX_TOKEN_TY_OP_ADD_ASSG, AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_ADD},
    {LEX_TOKEN_TY_OP_DIV_ASSG, AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_DIV},
    {LEX_TOKEN_TY_OP_MUL_ASSG, AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_MUL},
    {LEX_TOKEN_TY_OP_SUB_ASSG, AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_SUB},
    {LEX_TOKEN_TY_OP_QUOT_ASSG, AST_ASSG_TY_COMPOUNDASSG,
     AST_BINARY_OP_TY_QUOT},

    {LEX_TOKEN_TY_OP_LSHIFT_ASSG, AST_ASSG_TY_COMPOUNDASSG,
     AST_BINARY_OP_TY_LSHIFT},
    {LEX_TOKEN_TY_OP_RSHIFT_ASSG, AST_ASSG_TY_COMPOUNDASSG,
     AST_BINARY_OP_TY_RSHIFT},
    {LEX_TOKEN_TY_OP_AND_ASSG, AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_AND},
    {LEX_TOKEN_TY_OP_OR_ASSG, AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_OR},
    {LEX_TOKEN_TY_OP_XOR_ASSG, AST_ASSG_TY_COMPOUNDASSG, AST_BINARY_OP_TY_XOR},
};


bool try_resolve_member_access_ty(struct ast_tyref base_ty,
                                  const char *member_name,
                                  struct ast_tyref *ty_ref) {
  // TODO: super slow hashtable needed
  for (size_t i = 0; i < base_ty.aggregate.num_field_var_tys; i++) {
    const struct ast_struct_field *field = &base_ty.aggregate.field_var_tys[i];
    if (field->name == NULL) {
      if (try_resolve_member_access_ty(*field->var_ty, member_name, ty_ref)) {
        return true;
      }
    }

    if (strcmp(field->name, member_name) == 0) {
      *ty_ref = *field->var_ty;
      return true;
    }
  }

  return false;
}

struct ast_tyref resolve_member_access_ty(struct parser *parser,
                                          struct ast_tyref *var_ty,
                                          const struct token *member) {
  invariant_assert(var_ty->ty == AST_TYREF_TY_AGGREGATE ||
                       var_ty->ty == AST_TYREF_TY_TAGGED,
                   "non struct/union in member access");

  struct ast_tyref base_ty;

  // incomplete type, look it up now it is defined
  if (var_ty->ty == AST_TYREF_TY_TAGGED) {
    base_ty = tyref_get_defined(parser, var_ty);
  } else {
    base_ty = *var_ty;
  }

  *var_ty = base_ty;

  const char *member_name = identifier_str(parser, member);

  struct ast_tyref access_ty;
  if (try_resolve_member_access_ty(base_ty, member_name, &access_ty)) {
    return access_ty;
  }

  todo("member '%s' does not exist", member_name);
}


struct ast_tyref resolve_pointer_access_ty(struct parser *parser,
                                           const struct ast_tyref *var_ty,
                                           const struct token *member) {

  struct ast_tyref underlying_var_ty = tyref_get_underlying(parser, var_ty);
  struct ast_tyref base_ty = underlying_var_ty.ty == AST_TYREF_TY_TAGGED
                                 ? tyref_get_defined(parser, &underlying_var_ty)
                                 : underlying_var_ty;
  return resolve_member_access_ty(parser, &base_ty, member);
}

struct ast_tyref resolve_array_access_ty(struct parser *parser,
                                         const struct ast_expr *lhs,
                                         const struct ast_expr *rhs,
                                         bool *lhs_is_pointer) {
  if (lhs->var_ty.ty == AST_TYREF_TY_POINTER ||
      lhs->var_ty.ty == AST_TYREF_TY_ARRAY) {
    *lhs_is_pointer = true;
    return tyref_get_underlying(parser, &lhs->var_ty);
  } else {
    *lhs_is_pointer = false;
    return tyref_get_underlying(parser, &rhs->var_ty);
  }
}


void decay_array_params(struct ast_param *param) {
  if (param->var_ty.ty == AST_TYREF_TY_ARRAY) {
    param->var_ty.ty = AST_TYREF_TY_POINTER;
    param->var_ty.pointer.underlying = param->var_ty.array.element;
  }
}

struct ast_tyref resolve_ternary_ty(struct parser *parser,
                                    struct ast_tyref *lhs,
                                    struct ast_tyref *rhs) {
  UNUSED_ARG(parser);
  UNUSED_ARG(rhs);

  // FIXME: do logic
  return *lhs;
}

void parser_push_scope(struct parser *parser) {
  push_scope(&parser->var_table);
  push_scope(&parser->ty_table);
}

void parser_pop_scope(struct parser *parser) {
  pop_scope(&parser->var_table);
  pop_scope(&parser->ty_table);
}
