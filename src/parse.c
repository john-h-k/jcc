#include "parse.h"

#include "alloc.h"
#include "ap_val.h"
#include "diagnostics.h"
#include "lex.h"
#include "log.h"
#include "program.h"
#include "util.h"
#include "var_table.h"
#include "vector.h"

#include <string.h>

enum parser_state {
  PARSER_STATE_NORMAL,
  PARSER_STATE_ERR,
};

struct parser {
  struct arena_allocator *arena;
  struct lexer *lexer;

  struct var_table ty_table;

  enum parser_state state;

  enum parse_result_ty result_ty;
  struct compiler_diagnostics *diagnostics;
};

// root error is given back to the first call that enters error state
// this prevents sub-errors clearing the error flag
typedef enum {
  PARSER_ERR_NONE,
  PARSER_ERR_ROOT,
  PARSER_ERR_REENTRANT
} err_state;

enum parser_create_result parser_create(
    struct program program, struct preproc *preproc,
    enum compile_c_standard c_standard, enum compile_preproc_mode mode,
    struct compiler_diagnostics *diagnostics, struct parser **parser) {
  struct parser *p = nonnull_malloc(sizeof(*p));

  arena_allocator_create("parser", &p->arena);
  if (lexer_create(program, preproc, c_standard, mode, &p->lexer) !=
      LEX_CREATE_RESULT_SUCCESS) {
    err("failed to create lexer");
    return PARSER_CREATE_RESULT_FAILURE;
  }

  p->result_ty = PARSE_RESULT_TY_SUCCESS;
  p->ty_table = vt_create(p->arena);
  p->diagnostics = diagnostics;
  p->state = PARSER_STATE_NORMAL;

  *parser = p;

  return PARSER_CREATE_RESULT_SUCCESS;
}

static err_state parser_err(struct parser *parser) {
  if (parser->state == PARSER_STATE_ERR) {
    return PARSER_ERR_REENTRANT;
  }

  parser->state = PARSER_STATE_ERR;
  return PARSER_ERR_ROOT;
}

static void parser_clear_err(struct parser *parser, err_state state) {
  if (state == PARSER_ERR_NONE) {
    return;
  }

  DEBUG_ASSERT(parser->state == PARSER_STATE_ERR,
               "called when parser not in err state");

  if (state == PARSER_ERR_ROOT) {
    parser->state = PARSER_STATE_NORMAL;
  }
}

void parser_free(struct parser **parser) {
  lexer_free(&(*parser)->lexer);
  vt_free(&(*parser)->ty_table);

  arena_allocator_free(&(*parser)->arena);
  (*parser)->arena = NULL;
  free(*parser);

  *parser = NULL;
}

enum ast_associativity {
  AST_ASSOCIATIVITY_NONE,
  AST_ASSOCIATIVITY_LEFT,
  AST_ASSOCIATIVITY_RIGHT,
};

struct ast_op_info {
  enum ast_binary_op_ty ty;
  enum ast_associativity associativity;
  unsigned precedence;
};

static struct ast_op_info op_info(enum ast_binary_op_ty ty) {
  struct ast_op_info info = {.ty = ty};

  switch (ty) {
    // ternary
  case AST_BINARY_OP_TY_LOGICAL_OR:
    info.precedence = 1;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_LOGICAL_AND:
    info.precedence = 2;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_OR:
    info.precedence = 3;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_XOR:
    info.precedence = 4;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_AND:
    info.precedence = 5;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_EQ:
  case AST_BINARY_OP_TY_NEQ:
    info.precedence = 6;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_GT:
  case AST_BINARY_OP_TY_GTEQ:
  case AST_BINARY_OP_TY_LT:
  case AST_BINARY_OP_TY_LTEQ:
    info.precedence = 7;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_LSHIFT:
  case AST_BINARY_OP_TY_RSHIFT:
    info.precedence = 8;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_ADD:
  case AST_BINARY_OP_TY_SUB:
    info.precedence = 9;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_MUL:
  case AST_BINARY_OP_TY_DIV:
  case AST_BINARY_OP_TY_MOD:
    info.precedence = 10;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  }

  return info;
}

static bool op_info_for_token(const struct lex_token *token,
                              struct ast_op_info *info) {
  switch (token->ty) {
  case LEX_TOKEN_TY_OP_EQ:
    *info = op_info(AST_BINARY_OP_TY_EQ);
    return true;
  case LEX_TOKEN_TY_OP_NEQ:
    *info = op_info(AST_BINARY_OP_TY_NEQ);
    return true;
  case LEX_TOKEN_TY_OP_GT:
    *info = op_info(AST_BINARY_OP_TY_GT);
    return true;
  case LEX_TOKEN_TY_OP_GTEQ:
    *info = op_info(AST_BINARY_OP_TY_GTEQ);
    return true;
  case LEX_TOKEN_TY_OP_LT:
    *info = op_info(AST_BINARY_OP_TY_LT);
    return true;
  case LEX_TOKEN_TY_OP_LTEQ:
    *info = op_info(AST_BINARY_OP_TY_LTEQ);
    return true;
  case LEX_TOKEN_TY_OP_LSHIFT:
    *info = op_info(AST_BINARY_OP_TY_LSHIFT);
    return true;
  case LEX_TOKEN_TY_OP_RSHIFT:
    *info = op_info(AST_BINARY_OP_TY_RSHIFT);
    return true;
  case LEX_TOKEN_TY_OP_LOGICAL_AND:
    *info = op_info(AST_BINARY_OP_TY_LOGICAL_AND);
    return true;
  case LEX_TOKEN_TY_OP_LOGICAL_OR:
    *info = op_info(AST_BINARY_OP_TY_LOGICAL_OR);
    return true;
  case LEX_TOKEN_TY_OP_AND:
    *info = op_info(AST_BINARY_OP_TY_AND);
    return true;
  case LEX_TOKEN_TY_OP_OR:
    *info = op_info(AST_BINARY_OP_TY_OR);
    return true;
  case LEX_TOKEN_TY_OP_XOR:
    *info = op_info(AST_BINARY_OP_TY_XOR);
    return true;
  case LEX_TOKEN_TY_OP_ADD:
    *info = op_info(AST_BINARY_OP_TY_ADD);
    return true;
  case LEX_TOKEN_TY_OP_SUB:
    *info = op_info(AST_BINARY_OP_TY_SUB);
    return true;
  case LEX_TOKEN_TY_OP_MUL:
    *info = op_info(AST_BINARY_OP_TY_MUL);
    return true;
  case LEX_TOKEN_TY_OP_DIV:
    *info = op_info(AST_BINARY_OP_TY_DIV);
    return true;
  case LEX_TOKEN_TY_OP_MOD:
    *info = op_info(AST_BINARY_OP_TY_MOD);
    return true;
  default:
    // not an op
    return false;
  }
}

// unlike `parse_token`, always ""succeeds"" (because we have enough context to
// know what token is next, so we must know what to do after) but generates a
// diagnostic if it fails to find it
static bool parse_expected_token(struct parser *parser, enum lex_token_ty ty,
                                 struct text_pos start, const char *err,
                                 struct text_span *span) {
  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  if (span) {
    *span = token.span;
  }

  if (token.ty == ty) {
    lex_consume_token(parser->lexer, token);

    return true;
  }

  struct text_pos end = token.span.end;

  const char *prefix = "expected token ";
  size_t pref_len = strlen(prefix);
  size_t err_len = strlen(err);
  char *msg = arena_alloc(parser->arena, pref_len + err_len + 1);
  memcpy(msg, prefix, pref_len);
  memcpy(msg + pref_len, err, err_len);
  msg[pref_len + err_len] = '\0';

  parser->result_ty = PARSE_RESULT_TY_FAILURE;
  compiler_diagnostics_add(parser->diagnostics,
                           MK_PARSER_DIAGNOSTIC(EXPECTED_TOKEN, expected_token,
                                                MK_TEXT_SPAN(start, end), end,
                                                msg));

  if (span) {
    *span = MK_TEXT_SPAN(end, end);
  }

  return false;
}

static bool parse_expected_identifier(struct parser *parser,
                                      struct lex_token *token,
                                      struct text_pos start, const char *err) {
  lex_peek_token(parser->lexer, token);
  if (token->ty == LEX_TOKEN_TY_IDENTIFIER) {
    lex_consume_token(parser->lexer, *token);
    return true;
  }

  parser->result_ty = PARSE_RESULT_TY_FAILURE;
  compiler_diagnostics_add(
      parser->diagnostics,
      MK_PARSER_DIAGNOSTIC(EXPECTED_TOKEN, expected_token,
                           MK_TEXT_SPAN(start, token->span.end),
                           token->span.end, err));

  return false;
}

static bool parse_expr(struct parser *parser, struct ast_expr *expr);

static bool parse_expected_expr(struct parser *parser, struct ast_expr *expr,
                                const char *err) {
  if (parse_expr(parser, expr)) {
    return true;
  }

  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  parser->result_ty = PARSE_RESULT_TY_FAILURE;
  compiler_diagnostics_add(parser->diagnostics,
                           MK_PARSER_DIAGNOSTIC(EXPECTED_EXPR, expected_expr,
                                                token.span, token.span.start,
                                                err));

  return false;
}
static bool parse_token(struct parser *parser, enum lex_token_ty ty,
                        struct text_span *span) {
  struct lex_token token;
  lex_peek_token(parser->lexer, &token);
  if (token.ty == ty) {
    lex_consume_token(parser->lexer, token);

    if (span) {
      *span = token.span;
    }
    return true;
  }

  return false;
}

static bool parse_identifier(struct parser *parser, struct lex_token *token) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  lex_peek_token(parser->lexer, token);

  if (token->ty == LEX_TOKEN_TY_IDENTIFIER) {
    lex_consume_token(parser->lexer, *token);

    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static bool parse_type_qualifier(struct parser *parser,
                                 enum ast_type_qualifier *qualifier,
                                 struct text_span *span) {
  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  *span = token.span;

  if (token.ty == LEX_TOKEN_TY_KW_CONST) {
    *qualifier = AST_TYPE_QUALIFIER_CONST;
  } else if (token.ty == LEX_TOKEN_TY_KW_VOLATILE) {
    *qualifier = AST_TYPE_QUALIFIER_VOLATILE;
  } else if (token.ty == LEX_TOKEN_TY_KW_RESTRICT) {
    *qualifier = AST_TYPE_QUALIFIER_RESTRICT;
  } else if (token.ty == LEX_TOKEN_TY_KW_NONNULL) {
    *qualifier = AST_TYPE_QUALIFIER_NONNULL;
  } else if (token.ty == LEX_TOKEN_TY_KW_NULLABLE) {
    *qualifier = AST_TYPE_QUALIFIER_NULLABLE;
  } else {
    return false;
  }

  lex_consume_token(parser->lexer, token);
  return true;
}

static bool parse_function_specifier(struct parser *parser,
                                     enum ast_function_specifier *specifier,
                                     struct text_span *span) {
  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  *span = token.span;

  if (token.ty == LEX_TOKEN_TY_KW_INLINE) {
    *specifier = AST_FUNCTION_SPECIFIER_INLINE;
  } else if (token.ty == LEX_TOKEN_TY_KW_NORETURN) {
    *specifier = AST_FUNCTION_SPECIFIER_NORETURN;
  } else {
    return false;
  }

  lex_consume_token(parser->lexer, token);
  return true;
}

static bool
parse_storage_class_specifier(struct parser *parser,
                              enum ast_storage_class_specifier *specifier,
                              struct text_span *span) {
  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  *span = token.span;

  if (token.ty == LEX_TOKEN_TY_KW_TYPEDEF) {
    *specifier = AST_STORAGE_CLASS_SPECIFIER_TYPEDEF;
  } else if (token.ty == LEX_TOKEN_TY_KW_EXTERN) {
    *specifier = AST_STORAGE_CLASS_SPECIFIER_EXTERN;
  } else if (token.ty == LEX_TOKEN_TY_KW_STATIC) {
    *specifier = AST_STORAGE_CLASS_SPECIFIER_STATIC;
  } else if (token.ty == LEX_TOKEN_TY_KW_AUTO) {
    *specifier = AST_STORAGE_CLASS_SPECIFIER_AUTO;
  } else if (token.ty == LEX_TOKEN_TY_KW_REGISTER) {
    *specifier = AST_STORAGE_CLASS_SPECIFIER_REGISTER;
  } else {
    return false;
  }

  lex_consume_token(parser->lexer, token);
  return true;
}

static bool parse_type_specifier_kw(struct parser *parser,
                                    enum ast_type_specifier_kw *wkt,
                                    struct text_span *span) {
  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  *span = token.span;

  switch (token.ty) {
  case LEX_TOKEN_TY_KW_VOID:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_VOID;
    return true;
  case LEX_TOKEN_TY_KW_CHAR:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_CHAR;
    return true;
  case LEX_TOKEN_TY_KW_SHORT:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_SHORT;
    return true;
  case LEX_TOKEN_TY_KW_INT:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_INT;
    return true;
  case LEX_TOKEN_TY_KW_BOOL:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_BOOL;
    return true;
  case LEX_TOKEN_TY_KW_UINT128:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_UINT128;
    return true;
  case LEX_TOKEN_TY_KW_LONG:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_LONG;
    return true;
  case LEX_TOKEN_TY_KW_FLOAT:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_FLOAT;
    return true;
  case LEX_TOKEN_TY_KW_DOUBLE:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_DOUBLE;
    return true;
  case LEX_TOKEN_TY_KW_SIGNED:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_SIGNED;
    return true;
  case LEX_TOKEN_TY_KW_UNSIGNED:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_UNSIGNED;
    return true;
  // case LEX_TOKEN_TY_KW_COMPLEX:
  //   consume_token(parser->lexer, token);
  //   *wkt = AST_TYPE_SPECIFIER_KW_COMPLEX;
  //   return true;
  case LEX_TOKEN_TY_KW_HALF:
    lex_consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_HALF;
    return true;
  default:
    return false;
  }
}

static bool parse_compoundexpr_raw(struct parser *parser,
                                   struct ast_compoundexpr *compound_expr);

static bool parse_compoundexpr_as_expr(struct parser *parser,
                                       struct ast_expr *expr);

static bool parse_attribute(struct parser *parser,
                            struct ast_attribute *attribute) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct lex_token identifier;
  lex_peek_token(parser->lexer, &identifier);

  if (identifier.ty == LEX_TOKEN_TY_COMMA ||
      identifier.ty == LEX_TOKEN_TY_CLOSE_BRACKET) {
    attribute->ty = AST_ATTRIBUTE_TY_EMPTY;

    // technically points to wrong char (the one ahead of it) but is zero length
    // so shouldn't matter?
    attribute->span =
        MK_TEXT_SPAN(identifier.span.start, identifier.span.start);
    return true;
  }

  parse_expected_identifier(parser, &identifier, identifier.span.start,
                            "expected attribute to start with identifier");

  attribute->prefix = NULL;

  struct lex_token next;
  lex_peek_token(parser->lexer, &next);
  if (next.ty == LEX_TOKEN_TY_COLON) {
    lex_consume_token(parser->lexer, next);
    parse_expected_token(parser, LEX_TOKEN_TY_COLON, identifier.span.start,
                         "'::' not ':' after attribute prefix", NULL);

    attribute->prefix = arena_alloc(parser->arena, sizeof(*attribute->prefix));
    *attribute->prefix = identifier;

    parse_expected_identifier(parser, &identifier, identifier.span.start,
                              "expected attribute to start with identifier");
  }

  struct lex_token open;
  lex_peek_token(parser->lexer, &open);
  if (open.ty != LEX_TOKEN_TY_OPEN_BRACKET) {
    attribute->ty = AST_ATTRIBUTE_TY_NAMED;
    attribute->name = identifier;
    attribute->span = identifier.span;
    return true;
  }

  lex_consume_token(parser->lexer, open);

  // reuse compoundexpr parsing, but intentionally a different type so we can
  // support other attributes (e.g types, `__attribute__((foo(char *)))`)

  struct ast_compoundexpr compoundexpr;
  if (!parse_compoundexpr_raw(parser, &compoundexpr)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  attribute->ty = AST_ATTRIBUTE_TY_PARAMETERIZED;
  attribute->name = identifier;
  attribute->params = arena_alloc(parser->arena, sizeof(*attribute->params) *
                                                     compoundexpr.num_exprs);
  attribute->num_params = compoundexpr.num_exprs;

  for (size_t i = 0; i < compoundexpr.num_exprs; i++) {
    attribute->params[i] =
        (struct ast_attribute_param){.expr = &compoundexpr.exprs[i]};
  }

  struct text_span close;
  parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET,
                       identifier.span.start, "`)` after attribute params",
                       &close);

  attribute->span = MK_TEXT_SPAN(identifier.span.start, close.end);
  return true;
}

static bool parse_attribute_list(struct parser *parser,
                                 struct ast_attribute_list *attribute_list) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  // copied from parse_compoundexpr. maybe we should unify

  struct vector *attributes =
      vector_create_in_arena(sizeof(struct ast_attribute), parser->arena);

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct lex_token token;
  struct ast_attribute sub_attribute;
  do {
    if (!parse_attribute(parser, &sub_attribute)) {
      lex_backtrack(parser->lexer, pos);
      *attribute_list = (struct ast_attribute_list){0};
      return false;
    }

    if (first) {
      first = false;
      start = sub_attribute.span.start;
    }

    vector_push_back(attributes, &sub_attribute);
    end = sub_attribute.span.end;

    lex_peek_token(parser->lexer, &token);
  } while (token.ty == LEX_TOKEN_TY_COMMA &&
           /* hacky */ (lex_consume_token(parser->lexer, token), true));

  attribute_list->attributes = vector_head(attributes);
  attribute_list->num_attributes = vector_length(attributes);

  attribute_list->span = MK_TEXT_SPAN(start, end);
  return true;
}

static bool
parse_attribute_specifier(struct parser *parser,
                          struct ast_attribute_specifier *attribute_specifier) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span kw_span;

  struct text_pos start = kw_span.start;
  struct text_span end;

  if (parse_token(parser, LEX_TOKEN_TY_KW_ATTRIBUTE, &kw_span)) {
    parse_expected_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, start,
                         "`((` after `__attribute__` keyword", NULL);
    parse_expected_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, start,
                         "`((` after `__attribute__` keyword", NULL);

    parse_attribute_list(parser, &attribute_specifier->attribute_list);

    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, start,
                         "`))` after attribute list", NULL);

    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, start,
                         "`))` after attribute list", &end);
  } else if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET, &kw_span) &&
             parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET, NULL)) {

    parse_attribute_list(parser, &attribute_specifier->attribute_list);

    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET, start,
                         "`]]` after attribute list", NULL);

    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET, start,
                         "`]]` after attribute list", &end);

  } else {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  attribute_specifier->span = MK_TEXT_SPAN(kw_span.start, end.end);
  return true;
}

static bool parse_enumerator(struct parser *parser,
                             struct ast_enumerator *enumerator) {
  if (!parse_identifier(parser, &enumerator->identifier)) {
    return false;
  }

  struct text_pos end;
  if (parse_token(parser, LEX_TOKEN_TY_OP_ASSG, NULL)) {
    enumerator->value = arena_alloc(parser->arena, sizeof(*enumerator->value));

    parse_expected_expr(parser, enumerator->value,
                        "expected expression after = in enum body");

    end = enumerator->value->span.end;
  } else {
    enumerator->value = NULL;
    end = enumerator->identifier.span.end;
  }

  struct text_span comma;
  (void)parse_token(parser, LEX_TOKEN_TY_COMMA, &comma);

  enumerator->span = MK_TEXT_SPAN(enumerator->identifier.span.start, end);
  return true;
}

static void parse_enumerator_list(struct parser *parser,
                                  struct ast_enumerator_list *enumerator_list) {

  struct vector *list = vector_create_in_arena(
      sizeof(*enumerator_list->enumerators), parser->arena);

  struct ast_enumerator enumerator;

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  while (parse_enumerator(parser, &enumerator)) {
    if (first) {
      first = false;
      start = enumerator.span.start;
    }

    end = enumerator.span.end;

    struct text_span comma;
    if (parse_token(parser, LEX_TOKEN_TY_COMMA, &comma)) {
      end = comma.end;
    }

    vector_push_back(list, &enumerator);
  }

  struct text_span comma;
  if (parse_token(parser, LEX_TOKEN_TY_COMMA, &comma)) {
    end = comma.end;
  }

  enumerator_list->enumerators = vector_head(list);
  enumerator_list->num_enumerators = vector_length(list);
  enumerator_list->span = MK_TEXT_SPAN(start, end);
}

static bool parse_enum_specifier(struct parser *parser,
                                 struct ast_enum_specifier *enum_specifier) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span enum_span;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_ENUM, &enum_span)) {
    return false;
  }

  struct text_pos start = enum_span.start;
  struct text_pos end = enum_span.end;

  struct lex_token identifier;
  if (parse_identifier(parser, &identifier)) {
    end = identifier.span.end;

    enum_specifier->identifier =
        arena_alloc(parser->arena, sizeof(*enum_specifier->identifier));
    *enum_specifier->identifier = identifier;
  } else {
    enum_specifier->identifier = NULL;
  }

  struct ast_enumerator_list enumerator_list;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE, NULL)) {
    parse_enumerator_list(parser, &enumerator_list);

    struct text_span span;
    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACE, enum_span.start,
                         "`}` after enumerator body", &span);
    end = span.end;

    enum_specifier->enumerator_list =
        arena_alloc(parser->arena, sizeof(*enum_specifier->enumerator_list));
    *enum_specifier->enumerator_list = enumerator_list;
  } else {
    enum_specifier->enumerator_list = NULL;
  }

  if (!enum_specifier->identifier && !enum_specifier->enumerator_list) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  enum_specifier->span = MK_TEXT_SPAN(start, end);
  return true;
}

enum type_specifier_mode {
  TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS,
  TYPE_SPECIFIER_MODE_DISALLOW_TYPEDEFS
};

static void parse_declaration_specifier_list(
    struct parser *parser, enum type_specifier_mode mode,
    struct ast_declaration_specifier_list *specifier_list);

static bool parse_declarator(struct parser *parser,
                             struct ast_declarator *declarator);
static void
parse_declaration_list(struct parser *parser,
                       struct ast_declaration_list *declaration_list);

static bool parse_struct_or_union_specifier(
    struct parser *parser,
    struct ast_struct_or_union_specifier *struct_or_union_specifier) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_pos end;

  enum ast_struct_or_union_specifier_ty ty;
  struct text_span start_span;
  if (parse_token(parser, LEX_TOKEN_TY_KW_STRUCT, &start_span)) {
    ty = AST_STRUCT_OR_UNION_SPECIFIER_TY_STRUCT;
    end = start_span.end;
  } else if (parse_token(parser, LEX_TOKEN_TY_KW_UNION, &start_span)) {
    ty = AST_STRUCT_OR_UNION_SPECIFIER_TY_UNION;
    end = start_span.end;
  } else {
    return false;
  }

  struct lex_token identifier;
  if (parse_identifier(parser, &identifier)) {
    struct_or_union_specifier->identifier = arena_alloc(
        parser->arena, sizeof(*struct_or_union_specifier->identifier));
    *struct_or_union_specifier->identifier = identifier;

    end = identifier.span.end;
  } else {
    struct_or_union_specifier->identifier = NULL;
  }

  struct_or_union_specifier->ty = ty;
  struct_or_union_specifier->decl_list = NULL;

  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE, NULL)) {
    struct_or_union_specifier->decl_list = arena_alloc(
        parser->arena, sizeof(*struct_or_union_specifier->decl_list));
    parse_declaration_list(parser, struct_or_union_specifier->decl_list);

    struct text_span brace;
    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACE, start_span.start,
                         "`}` after struct/union body", &brace);

    end = brace.end;
  }

  if (!struct_or_union_specifier->identifier &&
      !struct_or_union_specifier->decl_list) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct_or_union_specifier->span = MK_TEXT_SPAN(start_span.start, end);
  return true;
}

static bool parse_typedef_name(struct parser *parser,
                               struct lex_token *typedef_name) {
  struct lex_token identifier;
  lex_peek_token(parser->lexer, &identifier);

  if (identifier.ty != LEX_TOKEN_TY_IDENTIFIER) {
    return false;
  }

  ustr_t name = identifier_str(parser, &identifier);

  struct var_table_entry *entry =
      vt_get_entry(&parser->ty_table, VAR_TABLE_NS_TYPEDEF, name);
  if (!entry) {
    return false;
  }

  *typedef_name = identifier;

  lex_consume_token(parser->lexer, identifier);
  typedef_name->span = identifier.span;
  return true;
}

enum parse_type_or_expr_mode {
  PARSE_TYPE_OR_EXPR_MODE_NORMAL,
  PARSE_TYPE_OR_EXPR_MODE_EXPR_NEEDS_PARENS,
};

static void parse_type_or_expr(struct parser *parser, struct text_span context,
                               enum parse_type_or_expr_mode mode,
                               struct ast_type_or_expr *type_or_expr);

static bool parse_typeof_specifier(struct parser *parser,
                                   struct ast_type_specifier *type_specifier) {

  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  // spec says its either `type ( expr )` or `typeof ( type ) `
  // i assume `type` means `type-name`?
  switch (token.ty) {
  case LEX_TOKEN_TY_KW_TYPEOF:
    type_specifier->ty = AST_TYPE_SPECIFIER_TYPEOF;

    lex_consume_token(parser->lexer, token);
    parse_type_or_expr(parser, token.span,
                       PARSE_TYPE_OR_EXPR_MODE_EXPR_NEEDS_PARENS,
                       &type_specifier->type_of);

    type_specifier->span = type_specifier->type_of_unqual.span;
    return true;
  case LEX_TOKEN_TY_KW_TYPEOF_UNQUAL:
    type_specifier->ty = AST_TYPE_SPECIFIER_TYPEOF_UNQUAL;

    lex_consume_token(parser->lexer, token);
    parse_type_or_expr(parser, token.span,
                       PARSE_TYPE_OR_EXPR_MODE_EXPR_NEEDS_PARENS,
                       &type_specifier->type_of_unqual);

    type_specifier->span = type_specifier->type_of_unqual.span;
    return true;
  default:
    return false;
  }
}

static bool parse_type_specifier(struct parser *parser,
                                 struct ast_type_specifier *type_specifier,
                                 enum type_specifier_mode mode) {

  if (parse_type_specifier_kw(parser, &type_specifier->type_specifier_kw,
                              &type_specifier->span)) {
    type_specifier->ty = AST_TYPE_SPECIFIER_TY_KW;
    return true;
  }

  if (parse_typeof_specifier(parser, type_specifier)) {
    return true;
  }

  if (parse_struct_or_union_specifier(
          parser, &type_specifier->struct_or_union_specifier)) {
    type_specifier->ty = AST_TYPE_SPECIFIER_STRUCT_OR_UNION;
    type_specifier->span = type_specifier->struct_or_union_specifier.span;
    return true;
  }

  if (parse_enum_specifier(parser, &type_specifier->enum_specifier)) {
    type_specifier->ty = AST_TYPE_SPECIFIER_ENUM;
    type_specifier->span = type_specifier->enum_specifier.span;
    return true;
  }

  if (mode == TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS &&
      parse_typedef_name(parser, &type_specifier->typedef_name)) {
    type_specifier->ty = AST_TYPE_SPECIFIER_TYPEDEF_NAME;
    type_specifier->span = type_specifier->typedef_name.span;
    return true;
  }

  return false;
}

static bool parse_decl_specifier(struct parser *parser,
                                 struct ast_declaration_specifier *specifier,
                                 enum type_specifier_mode mode) {
  if (parse_storage_class_specifier(parser, &specifier->storage_class_specifier,
                                    &specifier->span)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_STORAGE_CLASS_SPECIFIER;
    return true;
  }

  if (parse_function_specifier(parser, &specifier->function_specifier,
                               &specifier->span)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_FUNCTION_SPECIFIER;
    return true;
  }

  if (parse_type_qualifier(parser, &specifier->type_qualifier,
                           &specifier->span)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_TYPE_QUALIFIER;
    return true;
  }

  if (parse_type_specifier(parser, &specifier->type_specifier, mode)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_TYPE_SPECIFIER;
    specifier->span = specifier->type_specifier.span;
    return true;
  }

  if (parse_attribute_specifier(parser, &specifier->attribute_specifier)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_ATTRIBUTE_SPECIFIER;
    specifier->span = specifier->attribute_specifier.span;
    return true;
  }

  return false;
}

static void parse_declaration_specifier_list(
    struct parser *parser, enum type_specifier_mode mode,
    struct ast_declaration_specifier_list *specifier_list) {
  struct vector *list = vector_create_in_arena(
      sizeof(*specifier_list->decl_specifiers), parser->arena);

  // code like this will parse wrong
  // ```
  //   typedef struct s s;
  //
  //   struct s {
  //   	struct s1 { } s;
  //   } s2;
  // ```
  // because it will take `struct s1 { }` as a type qualifier, and then `s` as a
  // typedef name type qualifier so we do a hack if we have seen _any_ type
  // specifiers so far, we do not look for typedef names anymore

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct ast_declaration_specifier specifier;
  while (parse_decl_specifier(parser, &specifier, mode)) {
    if (first) {
      first = false;
      start = specifier.span.start;
    }

    end = specifier.span.end;

    if (specifier.ty == AST_DECL_SPECIFIER_TY_TYPE_SPECIFIER) {
      mode = TYPE_SPECIFIER_MODE_DISALLOW_TYPEDEFS;
    }

    vector_push_back(list, &specifier);
  }

  specifier_list->decl_specifiers = vector_head(list);
  specifier_list->num_decl_specifiers = vector_length(list);
  specifier_list->span = MK_TEXT_SPAN(start, end);
}

static bool parse_designator(struct parser *parser,
                             struct ast_designator *designator) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span start;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET, &start)) {
    struct ast_expr expr;
    parse_expected_expr(parser, &expr, "expression after [ in designator");

    struct text_span close;
    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET, start.start,
                         "`]` after expression in designator", &close);

    designator->ty = AST_DESIGNATOR_TY_INDEX;
    designator->index = arena_alloc(parser->arena, sizeof(*designator->index));
    *designator->index = expr;
    designator->span = MK_TEXT_SPAN(start.start, close.end);

    return true;
  } else if (parse_token(parser, LEX_TOKEN_TY_DOT, &start)) {
    struct lex_token identifier;
    parse_expected_identifier(parser, &identifier, start.start,
                              "identifier after dot in designator");

    designator->ty = AST_DESIGNATOR_TY_FIELD;
    designator->field = identifier;
    designator->span = MK_TEXT_SPAN(start.start, identifier.span.end);

    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static bool parse_designator_list(struct parser *parser,
                                  struct ast_designator_list *designator_list) {

  struct vector *list = vector_create_in_arena(
      sizeof(*designator_list->designators), parser->arena);

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct ast_designator designator;
  while (parse_designator(parser, &designator)) {
    if (first) {
      first = false;
      start = designator.span.start;
    }

    end = designator.span.end;

    vector_push_back(list, &designator);
  }

  if (vector_empty(list)) {
    return false;
  }

  designator_list->designators = vector_head(list);
  designator_list->num_designators = vector_length(list);
  designator_list->span = MK_TEXT_SPAN(start, end);

  return true;
}

static bool parse_init_list(struct parser *parser,
                            struct ast_init_list *init_list);

static bool parse_init(struct parser *parser, struct ast_init *init) {
  if (parse_init_list(parser, &init->init_list)) {
    init->ty = AST_INIT_TY_INIT_LIST;
    init->span = init->init_list.span;
    return true;
  } else if (parse_expr(parser, &init->expr)) {
    init->ty = AST_INIT_TY_EXPR;
    init->span = init->expr.span;
    return true;
  }

  return false;
}

static bool parse_init_list_init(struct parser *parser,
                                 struct ast_init_list_init *init_list) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  *init_list =
      (struct ast_init_list_init){.designator_list = NULL, .init = NULL};

  struct text_pos start;

  struct ast_designator_list designator_list;
  struct ast_init init;
  if (parse_designator_list(parser, &designator_list)) {
    start = designator_list.span.start;

    parse_expected_token(parser, LEX_TOKEN_TY_OP_ASSG,
                         designator_list.span.start,
                         "`=` after designator in init list", NULL);

    init_list->designator_list =
        arena_alloc(parser->arena, sizeof(*init_list->designator_list));
    *init_list->designator_list = designator_list;

    if (!parse_init(parser, &init)) {
      // use next token as the end
      // kinda hacky
      struct lex_token peek;
      lex_peek_token(parser->lexer, &peek);

      parser->result_ty = PARSE_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          parser->diagnostics,
          MK_PARSER_DIAGNOSTIC(
              EXPECTED_INIT, expected_init,
              MK_TEXT_SPAN(designator_list.span.start, peek.span.start),
              peek.span.start,
              "expected expression or init-list after designator"));

      // parse the comma after to allow parsing to continue
      parse_token(parser, LEX_TOKEN_TY_COMMA, NULL);

      init_list->init = NULL;
      init_list->span =
          MK_TEXT_SPAN(designator_list.span.start, peek.span.start);

      return true;
    }
  } else {
    if (!parse_init(parser, &init)) {
      lex_backtrack(parser->lexer, pos);
      return false;
    }

    start = init.span.start;
  }

  struct text_pos end = init.span.end;

  struct text_span comma;
  if (parse_token(parser, LEX_TOKEN_TY_COMMA, &comma)) {
    end = comma.end;
  }

  init_list->init = arena_alloc(parser->arena, sizeof(*init_list->init));
  *init_list->init = init;
  init_list->span = MK_TEXT_SPAN(start, end);
  return true;
}

static bool parse_init_list(struct parser *parser,
                            struct ast_init_list *init_list) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span brace;
  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE, &brace)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct vector *inits =
      vector_create_in_arena(sizeof(struct ast_init_list_init), parser->arena);

  bool first = true;
  struct text_pos start = brace.start;
  struct text_pos end = start;

  struct ast_init_list_init init;
  while (parse_init_list_init(parser, &init)) {
    if (first) {
      first = false;
      start = init.span.start;
    }

    vector_push_back(inits, &init);
  }

  parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACE, brace.start,
                       "`}` at end of init list", &brace);

  end = brace.end;

  init_list->inits = vector_head(inits);
  init_list->num_inits = vector_length(inits);
  init_list->span = MK_TEXT_SPAN(start, end);

  return true;
}

static bool parse_pointer(struct parser *parser, struct ast_pointer *pointer) {
  struct text_span op;
  if (!parse_token(parser, LEX_TOKEN_TY_OP_MUL, &op)) {
    return false;
  }

  parse_declaration_specifier_list(
      parser, TYPE_SPECIFIER_MODE_DISALLOW_TYPEDEFS, &pointer->specifier_list);
  pointer->span = MK_TEXT_SPAN(op.start, pointer->specifier_list.span.end);
  return true;
}

static void parse_pointer_list(struct parser *parser,
                               struct ast_pointer_list *pointer_list) {
  struct vector *list =
      vector_create_in_arena(sizeof(*pointer_list->pointers), parser->arena);

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct ast_pointer pointer;
  while (parse_pointer(parser, &pointer)) {
    if (first) {
      first = false;
      start = pointer.span.start;
    }

    end = pointer.span.end;

    vector_push_back(list, &pointer);
  }

  pointer_list->span = MK_TEXT_SPAN(start, end);
  pointer_list->pointers = vector_head(list);
  pointer_list->num_pointers = vector_length(list);
}

static bool
parse_ast_array_declarator(struct parser *parser,
                           struct ast_array_declarator *array_declarator) {
  struct text_span open;
  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET, &open)) {
    return false;
  }

  struct text_pos start = open.start;

  parse_declaration_specifier_list(parser, TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS,
                                   &array_declarator->specifier_list);

  enum ast_array_declarator_ty ty;
  if (parse_token(parser, LEX_TOKEN_TY_OP_MUL, NULL)) {
    ty = AST_ARRAY_DECLARATOR_TY_STAR;
  } else {
    bool is_static = parse_token(parser, LEX_TOKEN_TY_KW_STATIC, NULL);

    is_static = is_static || parse_token(parser, LEX_TOKEN_TY_KW_STATIC, NULL);

    struct lex_token next;
    lex_peek_token(parser->lexer, &next);

    struct ast_expr size;
    if (is_static) {
      parse_expected_expr(parser, &size, "expected expr in static array type");
      ty = AST_ARRAY_DECLARATOR_TY_STATIC_SIZED;
      array_declarator->size =
          arena_alloc(parser->arena, sizeof(*array_declarator->size));
      *array_declarator->size = size;
    } else if (next.ty == LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET) {
      ty = AST_ARRAY_DECLARATOR_TY_UNSIZED;
    } else {
      parse_expected_expr(parser, &size, "expected expr in array type");
      ty = AST_ARRAY_DECLARATOR_TY_SIZED;
      array_declarator->size =
          arena_alloc(parser->arena, sizeof(*array_declarator->size));
      *array_declarator->size = size;
    }
  }

  struct text_span close;
  parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET, open.start,
                       "`]` after array declarator", &close);

  struct text_pos end = close.end;

  array_declarator->ty = ty;
  array_declarator->span = MK_TEXT_SPAN(start, end);
  return true;
}

static bool parse_paramlist(struct parser *parser,
                            struct ast_paramlist *param_list);

static bool
parse_ast_func_declarator(struct parser *parser,
                          struct ast_func_declarator *func_declarator) {

  struct ast_paramlist param_list;
  if (!parse_paramlist(parser, &param_list)) {
    return false;
  }

  func_declarator->param_list =
      arena_alloc(parser->arena, sizeof(*func_declarator->param_list));
  *func_declarator->param_list = param_list;
  func_declarator->span = param_list.span;
  return true;
}

static bool
parse_abstract_declarator(struct parser *parser,
                          struct ast_abstract_declarator *abstract_declarator);

static bool parse_direct_abstract_declarator(
    struct parser *parser,
    struct ast_direct_abstract_declarator *direct_abstract_declarator) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct ast_array_declarator array_declarator;
  if (parse_ast_array_declarator(parser, &array_declarator)) {
    direct_abstract_declarator->ty =
        AST_DIRECT_ABSTRACT_DECLARATOR_TY_ARRAY_DECLARATOR;
    direct_abstract_declarator->array_declarator = arena_alloc(
        parser->arena, sizeof(*direct_abstract_declarator->array_declarator));
    *direct_abstract_declarator->array_declarator = array_declarator;

    direct_abstract_declarator->span = array_declarator.span;
    return true;
  }

  struct ast_func_declarator func_declarator;
  if (parse_ast_func_declarator(parser, &func_declarator)) {
    direct_abstract_declarator->ty =
        AST_DIRECT_ABSTRACT_DECLARATOR_TY_FUNC_DECLARATOR;
    direct_abstract_declarator->func_declarator = arena_alloc(
        parser->arena, sizeof(*direct_abstract_declarator->func_declarator));
    *direct_abstract_declarator->func_declarator = func_declarator;

    direct_abstract_declarator->span = func_declarator.span;
    return true;
  }

  struct text_span start, end;

  struct ast_abstract_declarator abstract_declarator;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, &start) &&
      parse_abstract_declarator(parser, &abstract_declarator) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, NULL)) {
    direct_abstract_declarator->ty =
        AST_DIRECT_ABSTRACT_DECLARATOR_TY_PAREN_DECLARATOR;
    direct_abstract_declarator->paren_declarator = arena_alloc(
        parser->arena, sizeof(*direct_abstract_declarator->paren_declarator));
    *direct_abstract_declarator->paren_declarator = abstract_declarator;

    direct_abstract_declarator->span = MK_TEXT_SPAN(start.start, end.end);
    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static void
parse_direct_abstract_declarator_list(struct parser *parser,
                                      struct ast_direct_abstract_declarator_list
                                          *direct_abstract_declarator_list) {
  struct vector *list = vector_create_in_arena(
      sizeof(*direct_abstract_declarator_list->direct_abstract_declarators),
      parser->arena);

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct ast_direct_abstract_declarator direct_abstract_declarator;
  while (
      parse_direct_abstract_declarator(parser, &direct_abstract_declarator)) {
    if (first) {
      start = direct_abstract_declarator.span.start;
      first = false;
    }

    end = direct_abstract_declarator.span.end;

    vector_push_back(list, &direct_abstract_declarator);
  }

  direct_abstract_declarator_list->direct_abstract_declarators =
      vector_head(list);
  direct_abstract_declarator_list->num_direct_abstract_declarators =
      vector_length(list);
  direct_abstract_declarator_list->span = MK_TEXT_SPAN(start, end);
}

static bool
parse_abstract_declarator(struct parser *parser,
                          struct ast_abstract_declarator *abstract_declarator) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  parse_pointer_list(parser, &abstract_declarator->pointer_list);
  parse_direct_abstract_declarator_list(
      parser, &abstract_declarator->direct_abstract_declarator_list);

  if (!abstract_declarator->pointer_list.num_pointers &&
      !abstract_declarator->direct_abstract_declarator_list
           .num_direct_abstract_declarators) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  abstract_declarator->span = MK_TEXT_SPAN(
      abstract_declarator->pointer_list.span.start,
      abstract_declarator->direct_abstract_declarator_list.span.end);
  return true;
}

static bool
parse_direct_declarator(struct parser *parser,
                        struct ast_direct_declarator *direct_declarator) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  if (parse_identifier(parser, &direct_declarator->identifier)) {
    direct_declarator->ty = AST_DIRECT_DECLARATOR_TY_IDENTIFIER;
    direct_declarator->span = direct_declarator->identifier.span;
    return true;
  }

  struct ast_array_declarator array_declarator;
  if (parse_ast_array_declarator(parser, &array_declarator)) {
    direct_declarator->ty = AST_DIRECT_DECLARATOR_TY_ARRAY_DECLARATOR;
    direct_declarator->array_declarator = arena_alloc(
        parser->arena, sizeof(*direct_declarator->array_declarator));
    *direct_declarator->array_declarator = array_declarator;
    direct_declarator->span = direct_declarator->array_declarator->span;
    return true;
  }

  struct ast_func_declarator func_declarator;
  if (parse_ast_func_declarator(parser, &func_declarator)) {
    direct_declarator->ty = AST_DIRECT_DECLARATOR_TY_FUNC_DECLARATOR;
    direct_declarator->func_declarator =
        arena_alloc(parser->arena, sizeof(*direct_declarator->func_declarator));
    *direct_declarator->func_declarator = func_declarator;
    direct_declarator->span = direct_declarator->func_declarator->span;
    return true;
  }

  struct text_span start, end;

  struct ast_declarator declarator;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, &start) &&
      parse_declarator(parser, &declarator)) {
    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, start.start,
                         "')' after declarator", NULL);

    direct_declarator->ty = AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR;
    direct_declarator->paren_declarator = arena_alloc(
        parser->arena, sizeof(*direct_declarator->paren_declarator));
    *direct_declarator->paren_declarator = declarator;
    direct_declarator->span = MK_TEXT_SPAN(start.start, end.end);
    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

// TODO: both this and abstract_declarator_list should really be a recursive
// type, rather than list because you have to do hacks like check if you haven't
// yet seen a sub decl
static void parse_direct_declarator_list(
    struct parser *parser,
    struct ast_direct_declarator_list *direct_declarator_list) {
  struct vector *list = vector_create_in_arena(
      sizeof(*direct_declarator_list->direct_declarators), parser->arena);

  struct ast_direct_declarator direct_declarator;

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  while (parse_direct_declarator(parser, &direct_declarator)) {
    if (first) {
      first = false;
      start = direct_declarator.span.start;
    }

    end = direct_declarator.span.end;

    if (vector_empty(list)) {
      // first must be identifier or sub decl
      if (direct_declarator.ty != AST_DIRECT_DECLARATOR_TY_IDENTIFIER &&
          direct_declarator.ty != AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR) {
        break;
      }
    }

    vector_push_back(list, &direct_declarator);
  }

  direct_declarator_list->direct_declarators = vector_head(list);
  direct_declarator_list->num_direct_declarators = vector_length(list);
  direct_declarator_list->span = MK_TEXT_SPAN(start, end);
}

static void parse_attribute_specifier_list(
    struct parser *parser,
    struct ast_attribute_specifier_list *attribute_specifier_list) {
  struct vector *list = vector_create_in_arena(
      sizeof(*attribute_specifier_list->attribute_specifiers), parser->arena);

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct ast_attribute_specifier attribute_specifier;
  while (parse_attribute_specifier(parser, &attribute_specifier)) {
    if (first) {
      first = false;
      start = attribute_specifier.span.start;
    }

    end = attribute_specifier.span.end;

    vector_push_back(list, &attribute_specifier);
  }

  attribute_specifier_list->attribute_specifiers = vector_head(list);
  attribute_specifier_list->num_attribute_specifiers = vector_length(list);
  attribute_specifier_list->span = MK_TEXT_SPAN(start, end);
}

static bool parse_declarator(struct parser *parser,
                             struct ast_declarator *declarator) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  parse_pointer_list(parser, &declarator->pointer_list);
  parse_direct_declarator_list(parser, &declarator->direct_declarator_list);

  parse_attribute_specifier_list(parser, &declarator->attribute_specifier_list);

  bool has_declarator =
      declarator->direct_declarator_list.num_direct_declarators;

  struct text_pos end = declarator->attribute_specifier_list.span.end;

  struct text_span asm_kw;
  if (parse_token(parser, LEX_TOKEN_TY_KW_ASM, &asm_kw)) {
    parse_expected_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, asm_kw.start,
                         "'(' after asm keyword", NULL);

    struct ast_expr expr;
    parse_expected_expr(parser, &expr, "expected expr after asm keyword");

    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, asm_kw.start,
                         "')' after asm", NULL);

    declarator->declarator_label =
        arena_alloc(parser->arena, sizeof(*declarator->declarator_label));
    *declarator->declarator_label = (struct ast_declarator_label){
        .label = arena_alloc(parser->arena,
                             sizeof(*declarator->declarator_label->label))};

    *declarator->declarator_label->label = expr;
    end = expr.span.end;
  } else {
    declarator->declarator_label = NULL;
  }

  struct lex_pos end_of_declarator = lex_get_position(parser->lexer);

  struct ast_expr expr;
  if (parse_token(parser, LEX_TOKEN_TY_COLON, NULL) &&
      parse_expr(parser, &expr)) {
    declarator->bitfield_size =
        arena_alloc(parser->arena, sizeof(*declarator->bitfield_size));
    *declarator->bitfield_size = expr;

    end = expr.span.end;
  } else {
    declarator->bitfield_size = NULL;
    lex_backtrack(parser->lexer, end_of_declarator);
  }

  if (!has_declarator && !declarator->bitfield_size) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  declarator->span = MK_TEXT_SPAN(declarator->pointer_list.span.start, end);
  return true;
}

static bool parse_init_declarator(struct parser *parser,
                                  struct ast_init_declarator *init_declarator) {
  if (!parse_declarator(parser, &init_declarator->declarator)) {
    return false;
  }

  struct lex_pos pre_init_pos = lex_get_position(parser->lexer);

  struct text_pos end;
  struct ast_init init;
  if (parse_token(parser, LEX_TOKEN_TY_OP_ASSG, NULL) &&
      parse_init(parser, &init)) {
    init_declarator->init =
        arena_alloc(parser->arena, sizeof(*init_declarator->init));
    *init_declarator->init = init;

    end = init.span.end;
  } else {
    lex_backtrack(parser->lexer, pre_init_pos);
    init_declarator->init = NULL;

    end = init_declarator->declarator.span.end;
  }

  init_declarator->span =
      MK_TEXT_SPAN(init_declarator->declarator.span.start, end);
  return true;
}

static void parse_init_declarator_list(
    struct parser *parser,
    struct ast_init_declarator_list *init_declarator_list) {
  struct vector *list = vector_create_in_arena(
      sizeof(*init_declarator_list->init_declarators), parser->arena);

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct ast_init_declarator init_declarator;
  while (parse_init_declarator(parser, &init_declarator)) {
    if (first) {
      first = false;
      start = init_declarator.span.start;
    }

    end = init_declarator.span.end;

    vector_push_back(list, &init_declarator);

    parse_token(parser, LEX_TOKEN_TY_COMMA, NULL);
  }

  init_declarator_list->init_declarators = vector_head(list);
  init_declarator_list->num_init_declarators = vector_length(list);
  init_declarator_list->span = MK_TEXT_SPAN(start, end);
}

static bool parse_type_name(struct parser *parser,
                            struct ast_type_name *type_name) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  parse_declaration_specifier_list(parser, TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS,
                                   &type_name->specifier_list);
  if (!type_name->specifier_list.num_decl_specifiers) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  parse_abstract_declarator(parser, &type_name->abstract_declarator);

  type_name->span = MK_TEXT_SPAN(type_name->specifier_list.span.start,
                                 type_name->abstract_declarator.span.end);
  return true;
}

static bool parse_var(struct parser *parser, struct ast_var *var) {
  if (parse_identifier(parser, &var->identifier)) {
    var->span = var->identifier.span;
    return true;
  }

  return false;
}

static bool parse_float_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct lex_token token;

  lex_peek_token(parser->lexer, &token);

  enum ast_cnst_ty ty;
  enum ap_float_ty float_ty;
  switch (token.ty) {
  case LEX_TOKEN_TY_FLOAT_LITERAL:
    ty = AST_CNST_TY_FLOAT;
    float_ty = AP_FLOAT_TY_F32;
    break;
  case LEX_TOKEN_TY_DOUBLE_LITERAL:
    ty = AST_CNST_TY_DOUBLE;
    float_ty = AP_FLOAT_TY_F64;
    break;
  case LEX_TOKEN_TY_LONG_DOUBLE_LITERAL:
    ty = AST_CNST_TY_LONG_DOUBLE;
    // FIXME: long double types
    float_ty = AP_FLOAT_TY_F64;
    break;
  default:
    return false;
  }

  ustr_t literal = lex_associated_text(parser->lexer, &token);

  DEBUG_ASSERT(literal.len, "literal_len was 0");

  cnst->ty = ty;
  cnst->span = token.span;

  if (!ap_val_try_parse_float(parser->arena, float_ty, literal,
                              &cnst->num_value)) {
    parser->result_ty = PARSE_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        parser->diagnostics,
        MK_PARSER_DIAGNOSTIC(INVALID_FLOATING_POINT_LITERAL,
                             invalid_floating_point_literal, cnst->span,
                             MK_INVALID_TEXT_POS(0),
                             "invalid floating-point literal"));
  }

  lex_consume_token(parser->lexer, token);
  return true;
}

static bool parse_char_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct lex_token token;

  lex_peek_token(parser->lexer, &token);

  unsigned long long int_value;

  enum ast_cnst_ty ty;
  size_t num_bits;
  switch (token.ty) {
  case LEX_TOKEN_TY_ASCII_CHAR_LITERAL: {
    ty = AST_CNST_TY_CHAR;
    num_bits = 8;

    ustr_t literal =
        lex_strlike_associated_text(parser->lexer, &token);
    DEBUG_ASSERT(literal.len, "literal_len was 0");
    int_value = (unsigned long long)literal.str[0];
    break;
  }
  case LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL: {
    ty = AST_CNST_TY_SIGNED_INT;
    num_bits = 32;

    ustr_t literal =
        lex_strlike_associated_text(parser->lexer, &token);
    DEBUG_ASSERT(literal.len, "literal_len was 0");

    wchar_t wchar;
    mbtowc(&wchar, literal.str, literal.len);
    int_value = (unsigned long long)wchar;
    break;
  }
  default:
    return false;
  }

  lex_consume_token(parser->lexer, token);

  cnst->ty = ty;
  cnst->num_value = ap_val_from_ull(int_value, num_bits);

  cnst->span = token.span;
  return true;
}

static bool parse_int_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct lex_token token;

  lex_peek_token(parser->lexer, &token);

  enum ast_cnst_ty ty;
  switch (token.ty) {
  case LEX_TOKEN_TY_SIGNED_INT_LITERAL:
    ty = AST_CNST_TY_SIGNED_INT;
    break;
  case LEX_TOKEN_TY_UNSIGNED_INT_LITERAL:
    ty = AST_CNST_TY_UNSIGNED_INT;
    break;
  case LEX_TOKEN_TY_SIGNED_LONG_LITERAL:
    ty = AST_CNST_TY_SIGNED_LONG;
    break;
  case LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL:
    ty = AST_CNST_TY_UNSIGNED_LONG;
    break;
  case LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL:
    ty = AST_CNST_TY_SIGNED_LONG_LONG;
    break;
  case LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL:
    ty = AST_CNST_TY_UNSIGNED_LONG_LONG;
    break;
  default:
    return false;
  }

  ustr_t literal = lex_associated_text(parser->lexer, &token);
  DEBUG_ASSERT(literal.len, "literal_len was 0");

  lex_consume_token(parser->lexer, token);

  cnst->ty = ty;
  cnst->span = token.span;

  if (!ap_val_try_parse_int(parser->arena, 64, literal, &cnst->num_value)) {
    parser->result_ty = PARSE_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        parser->diagnostics,
        MK_PARSER_DIAGNOSTIC(INVALID_INT_LITERAL, invalid_int_literal,
                             cnst->span, MK_INVALID_TEXT_POS(0),
                             "invalid int literal"));
    return true;
  }

  return true;
}

static bool parse_str_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct lex_token token;

  struct vector *strings = vector_create_in_arena(sizeof(char), parser->arena);

  lex_peek_token(parser->lexer, &token);

  struct text_pos start = token.span.start;
  struct text_pos end = token.span.end;

  // must be at least one string component (but it could be empty)
  bool is_string = false;
  while (token.ty == LEX_TOKEN_TY_ASCII_STR_LITERAL ||
         token.ty == LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL) {

    is_string = true;

    if (token.ty == LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL) {
      cnst->ty = AST_CNST_TY_WIDE_STR_LITERAL;
    } else {
      cnst->ty = AST_CNST_TY_STR_LITERAL;
    }

    ustr_t str = lex_strlike_associated_text(parser->lexer, &token);
    vector_extend(strings, str.str, str.len);

    end = token.span.end;

    lex_consume_token(parser->lexer, token);
    lex_peek_token(parser->lexer, &token);
  }

  if (!is_string) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  size_t len = vector_length(strings);

  char null = 0;
  vector_push_back(strings, &null);
  if (cnst->ty == AST_CNST_TY_WIDE_STR_LITERAL) {
    // so its a full `int` 0
    vector_push_back(strings, &null);
    vector_push_back(strings, &null);
    vector_push_back(strings, &null);

    DEBUG_ASSERT(len % 4 == 0, "expected wide str to be length multiple of 4");

    cnst->str_value = (struct ast_cnst_str){
        .ty = AST_CNST_STR_TY_WIDE,
        .wide = {.value = vector_head(strings), .len = len / 4}};
  } else {
    cnst->str_value = (struct ast_cnst_str){
        .ty = AST_CNST_STR_TY_ASCII,
        .ascii = {.value = vector_head(strings), .len = len}};
  }

  cnst->span = MK_TEXT_SPAN(start, end);
  return true;
}

static bool parse_bool_cnst(struct parser *parser,
                            struct lex_token *token, struct ast_cnst *cnst) {
  switch (token->ty) {
  case LEX_TOKEN_TY_KW_TRUE:
    cnst->ty = AST_CNST_TY_BOOL;
    cnst->num_value = ap_val_from_ull(1, 1);
    cnst->span = token->span;
    lex_consume_token(parser->lexer, *token);
    return true;
  case LEX_TOKEN_TY_KW_FALSE:
    cnst->ty = AST_CNST_TY_BOOL;
    cnst->num_value = ap_val_from_ull(0, 1);
    cnst->span = token->span;
    lex_consume_token(parser->lexer, *token);
    return true;
  default:
    return false;
  }
}

static bool parse_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  switch (token.ty) {
  case LEX_TOKEN_TY_SIGNED_INT_LITERAL:
  case LEX_TOKEN_TY_UNSIGNED_INT_LITERAL:
  case LEX_TOKEN_TY_SIGNED_LONG_LITERAL:
  case LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL:
  case LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL:
  case LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL:
    return parse_int_cnst(parser, cnst);
  case LEX_TOKEN_TY_ASCII_CHAR_LITERAL:
  case LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL:
    return parse_char_cnst(parser, cnst);
  case LEX_TOKEN_TY_FLOAT_LITERAL:
  case LEX_TOKEN_TY_DOUBLE_LITERAL:
  case LEX_TOKEN_TY_LONG_DOUBLE_LITERAL:
    return parse_float_cnst(parser, cnst);
  case LEX_TOKEN_TY_KW_TRUE:
  case LEX_TOKEN_TY_KW_FALSE:
    return parse_bool_cnst(parser, &token, cnst);
  case LEX_TOKEN_TY_ASCII_STR_LITERAL:
  case LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL:
    return parse_str_cnst(parser, cnst);
  default:
    return false;
  }
}

static bool parse_expr(struct parser *parser, struct ast_expr *expr);
static bool parse_atom_0(struct parser *parser, struct ast_expr *expr);
static bool parse_atom_1(struct parser *parser, struct ast_expr *expr);
static bool parse_atom_2(struct parser *parser, struct ast_expr *expr);
static bool parse_atom_3(struct parser *parser, struct ast_expr *expr);

static bool parse_assg(struct parser *parser, const struct ast_expr *assignee,
                       struct ast_assg *assg) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  enum ast_assg_ty ty;
  switch (token.ty) {
  case LEX_TOKEN_TY_OP_ASSG:
    ty = AST_ASSG_TY_BASIC;
    break;
  case LEX_TOKEN_TY_OP_AND_ASSG:
    ty = AST_ASSG_TY_AND;
    break;
  case LEX_TOKEN_TY_OP_OR_ASSG:
    ty = AST_ASSG_TY_OR;
    break;
  case LEX_TOKEN_TY_OP_XOR_ASSG:
    ty = AST_ASSG_TY_XOR;
    break;
  case LEX_TOKEN_TY_OP_RSHIFT_ASSG:
    ty = AST_ASSG_TY_RSHIFT;
    break;
  case LEX_TOKEN_TY_OP_LSHIFT_ASSG:
    ty = AST_ASSG_TY_LSHIFT;
    break;
  case LEX_TOKEN_TY_OP_ADD_ASSG:
    ty = AST_ASSG_TY_ADD;
    break;
  case LEX_TOKEN_TY_OP_SUB_ASSG:
    ty = AST_ASSG_TY_SUB;
    break;
  case LEX_TOKEN_TY_OP_MUL_ASSG:
    ty = AST_ASSG_TY_MUL;
    break;
  case LEX_TOKEN_TY_OP_DIV_ASSG:
    ty = AST_ASSG_TY_DIV;
    break;
  case LEX_TOKEN_TY_OP_MOD_ASSG:
    ty = AST_ASSG_TY_MOD;
    break;
  default:
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  lex_consume_token(parser->lexer, token);

  struct ast_expr expr;
  parse_expected_expr(parser, &expr, "expected expr after assignment token");

  assg->ty = ty;
  assg->assignee = arena_alloc(parser->arena, sizeof(*assg->assignee));
  *assg->assignee = *assignee;
  assg->expr = arena_alloc(parser->arena, sizeof(*assg->expr));
  *assg->expr = expr;

  assg->span = MK_TEXT_SPAN(assg->assignee->span.start, assg->expr->span.end);
  return true;
}

static bool parse_arglist(struct parser *parser, struct ast_arglist *arg_list) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct ast_compoundexpr compound_expr;

  struct text_span start, end;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, &start) &&
      parse_compoundexpr_raw(parser, &compound_expr) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, &end)) {
    arg_list->args = compound_expr.exprs;
    arg_list->num_args = compound_expr.num_exprs;

    arg_list->span = MK_TEXT_SPAN(start.start, end.end);
    return true;
  }

  lex_backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, &start) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, &end)) {
    arg_list->args = NULL;
    arg_list->num_args = 0;

    arg_list->span = MK_TEXT_SPAN(start.start, end.end);
    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static bool
parse_generic_association(struct parser *parser,
                          struct ast_generic_association *generic_association) {
  struct text_span start;

  // need to parse kw first, because `parse_type_name` will parse `default` as
  // an invalid type name successfully

  if (parse_token(parser, LEX_TOKEN_TY_KW_DEFAULT, &start)) {
    generic_association->ty = AST_GENERIC_ASSOCIATION_TY_DEFAULT;
  } else {
    generic_association->ty = AST_GENERIC_ASSOCIATION_TY_TYPE_NAME;

    if (!parse_type_name(parser, &generic_association->type_name)) {
      // HACK: get next token for err
      // we need a more general way to say "next token" for diagnostic
      struct lex_token err_tok;
      lex_peek_token(parser->lexer, &err_tok);

      parser->result_ty = PARSE_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          parser->diagnostics,
          MK_PARSER_DIAGNOSTIC(
              EXPECTED_TYPE_NAME, expected_type_name, err_tok.span,
              err_tok.span.start,
              "expected type-name to begin generic association"));

      // 0 init so not in an invalid state
      generic_association->type_name = (struct ast_type_name){0};
      generic_association->span = MK_TEXT_SPAN(start.start, err_tok.span.start);
      return true;
    }

    start = generic_association->type_name.span;
  }

  parse_expected_token(parser, LEX_TOKEN_TY_COLON, start.start,
                       "':' after generic association list type name", NULL);

  parse_expected_expr(parser, &generic_association->expr,
                      "expression after ':' in generic association list");

  generic_association->span =
      MK_TEXT_SPAN(start.start, generic_association->expr.span.end);
  return true;
}

static bool parse_generic(struct parser *parser, struct ast_generic *generic) {
  struct text_span start;

  if (!parse_token(parser, LEX_TOKEN_TY_KW_GENERIC, &start)) {
    return false;
  }

  parse_expected_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, start.start,
                       "'(' after '_Generic' keyword", NULL);

  generic->ctrl_expr = arena_alloc(parser->arena, sizeof(*generic->ctrl_expr));
  parse_expected_expr(parser, generic->ctrl_expr,
                      "expression after '_Generic' keyword");

  parse_expected_token(parser, LEX_TOKEN_TY_COMMA, start.start,
                       "',' after '_Generic' controlling expression", NULL);

  struct lex_pos pos = lex_get_position(parser->lexer);

  // this could be made recursive instead

  struct vector *associations = vector_create_in_arena(
      sizeof(struct ast_generic_association), parser->arena);

  struct lex_token token;
  struct ast_generic_association association;
  do {
    if (!parse_generic_association(parser, &association)) {
      lex_backtrack(parser->lexer, pos);
      break;
    }

    vector_push_back(associations, &association);

    lex_peek_token(parser->lexer, &token);
  } while (token.ty == LEX_TOKEN_TY_COMMA &&
           /* hacky */ (lex_consume_token(parser->lexer, token), true));

  struct text_span end;
  parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, start.start,
                       "')' after '_Generic' association list", &end);

  generic->associations = vector_head(associations);
  generic->num_associations = vector_length(associations);
  generic->span = MK_TEXT_SPAN(start.start, end.end);
  return true;
}

// parses highest precedence (literals, vars, constants)
static bool parse_atom_0(struct parser *parser, struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  struct ast_generic generic;
  if (parse_generic(parser, &generic)) {
    expr->ty = AST_EXPR_TY_GENERIC;
    expr->generic = generic;
    expr->span = generic.span;
    return true;
  }

  // parenthesised expression
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, NULL) &&
      parse_compoundexpr_as_expr(parser, expr) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, NULL)) {
    return true;
  }

  lex_backtrack(parser->lexer, pos);

  if (parse_cnst(parser, &expr->cnst)) {
    expr->ty = AST_EXPR_TY_CNST;
    expr->span = expr->cnst.span;
    return true;
  }

  if (parse_var(parser, &expr->var)) {
    expr->ty = AST_EXPR_TY_VAR;
    expr->span = expr->var.span;
    return true;
  }

  return false;
}

static bool
parse_compound_literal(struct parser *parser,
                       struct ast_compound_literal *compound_literal) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct ast_type_name type_name;
  struct ast_init_list init_list;
  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, NULL) ||
      !parse_type_name(parser, &type_name) ||
      !parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, NULL) ||
      !parse_init_list(parser, &init_list)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  compound_literal->type_name = type_name;
  compound_literal->init_list = init_list;
  compound_literal->span =
      MK_TEXT_SPAN(type_name.span.start, init_list.span.end);

  return true;
}

// parses precedence level 0:
// vars
static bool parse_atom_1(struct parser *parser, struct ast_expr *expr) {
  if (parse_compound_literal(parser, &expr->compound_literal)) {
    expr->ty = AST_EXPR_TY_COMPOUND_LITERAL;
    expr->span = expr->compound_literal.span;
    return true;
  }

  if (parse_atom_0(parser, expr)) {
    return true;
  }

  return false;
}

static bool parse_call(struct parser *parser, struct ast_expr *sub_expr,
                       struct ast_expr *expr) {
  struct ast_arglist arg_list;
  if (!parse_arglist(parser, &arg_list)) {
    return false;
  }

  expr->ty = AST_EXPR_TY_CALL;
  expr->call.target = arena_alloc(parser->arena, sizeof(*expr->call.target));
  expr->call.target = sub_expr;
  expr->call.arg_list = arg_list;
  expr->call.span = MK_TEXT_SPAN(sub_expr->span.start, arg_list.span.end);

  expr->span = expr->call.span;
  return true;
}

static bool parse_array_access(struct parser *parser, struct ast_expr *lhs,
                               struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span access;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET, &access)) {
    struct ast_expr *rhs = arena_alloc(parser->arena, sizeof(*rhs));

    parse_expected_expr(parser, rhs,
                        "expression after [ in expression context");
    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET,
                         access.start, "`]` after array access", NULL);

    expr->ty = AST_EXPR_TY_ARRAYACCESS;
    expr->array_access.lhs = lhs;
    expr->array_access.rhs = rhs;
    expr->array_access.span = MK_TEXT_SPAN(lhs->span.start, rhs->span.end);

    expr->span = expr->array_access.span;
    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static bool parse_member_access(struct parser *parser,
                                struct ast_expr *sub_expr,
                                struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span dot;
  if (!parse_token(parser, LEX_TOKEN_TY_DOT, &dot)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct lex_token token;
  parse_expected_identifier(parser, &token, sub_expr->span.start,
                            "identifier after . in member access");

  expr->ty = AST_EXPR_TY_MEMBERACCESS;
  expr->member_access = (struct ast_memberaccess){
      .lhs = sub_expr,
      .member = token,
      .span = MK_TEXT_SPAN(sub_expr->span.start, token.span.end)};

  expr->span = expr->member_access.span;
  return true;
}

static bool parse_pointer_access(struct parser *parser,
                                 struct ast_expr *sub_expr,
                                 struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_ARROW, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct lex_token token;
  parse_expected_identifier(parser, &token, sub_expr->span.start,
                            "identifier after -> in pointer access");

  expr->ty = AST_EXPR_TY_POINTERACCESS;
  expr->pointer_access = (struct ast_pointeraccess){
      .lhs = sub_expr,
      .member = token,
      .span = MK_TEXT_SPAN(sub_expr->span.start, token.span.end)};

  expr->span = expr->pointer_access.span;
  return true;
}

static bool parse_unary_postfix_op(struct parser *parser,
                                   struct ast_expr *sub_expr,
                                   struct ast_expr *expr) {
  bool has_unary_postfix = false;
  enum ast_unary_op_ty unary_postfix_ty;
  struct text_span end;
  if (parse_token(parser, LEX_TOKEN_TY_OP_INC, &end)) {
    has_unary_postfix = true;
    unary_postfix_ty = AST_UNARY_OP_TY_POSTFIX_INC;
  } else if (parse_token(parser, LEX_TOKEN_TY_OP_DEC, &end)) {
    has_unary_postfix = true;
    unary_postfix_ty = AST_UNARY_OP_TY_POSTFIX_DEC;
  }

  if (!has_unary_postfix) {
    return false;
  }

  expr->ty = AST_EXPR_TY_UNARY_OP;
  expr->unary_op = (struct ast_unary_op){
      .ty = unary_postfix_ty,
      .expr = sub_expr,
  };

  expr->span = MK_TEXT_SPAN(sub_expr->span.start, end.end);
  return true;
}

// parses precedence level 1:
// postfix ++, postfix --, (), [], ., ->, (type){list}
static bool parse_atom_2(struct parser *parser, struct ast_expr *expr) {
  if (!parse_atom_1(parser, expr)) {
    return false;
  }

  while (true) {
    struct ast_expr *new_expr = arena_alloc(parser->arena, sizeof(*new_expr));
    struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*new_expr));
    *sub_expr = *expr;

    if (parse_unary_postfix_op(parser, sub_expr, new_expr) ||
        parse_call(parser, sub_expr, new_expr) ||
        parse_array_access(parser, sub_expr, new_expr) ||
        parse_member_access(parser, sub_expr, new_expr) ||
        parse_pointer_access(parser, sub_expr, new_expr)) {

      *expr = *new_expr;
      continue;
    }

    return true;
  }
}

static bool parse_cast(struct parser *parser, struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span start;
  struct ast_type_name type_name;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, &start) &&
      parse_type_name(parser, &type_name)) {

    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, start.start,
                         "`)` after type in cast", NULL);

    struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*sub_expr));
    if (!parse_atom_3(parser, sub_expr)) {
      lex_backtrack(parser->lexer, pos);
      return false;
    }

    expr->ty = AST_EXPR_TY_UNARY_OP;
    expr->unary_op = (struct ast_unary_op){
        .ty = AST_UNARY_OP_TY_CAST,
        .expr = sub_expr,
        // TODO: this is redundant
        .cast = (struct ast_cast){.type_name = type_name}};

    expr->span = MK_TEXT_SPAN(start.start, sub_expr->span.end);
    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static bool parse_unary_prefix_op(struct parser *parser,
                                  struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct lex_token token;
  lex_peek_token(parser->lexer, &token);

  bool has_unary_prefix = true;
  enum ast_unary_op_ty unary_prefix_ty;
  switch (token.ty) {
  case LEX_TOKEN_TY_OP_INC:
    unary_prefix_ty = AST_UNARY_OP_TY_PREFIX_INC;
    break;
  case LEX_TOKEN_TY_OP_DEC:
    unary_prefix_ty = AST_UNARY_OP_TY_PREFIX_DEC;
    break;
  case LEX_TOKEN_TY_OP_ADD:
    unary_prefix_ty = AST_UNARY_OP_TY_PLUS;
    break;
  case LEX_TOKEN_TY_OP_SUB:
    unary_prefix_ty = AST_UNARY_OP_TY_MINUS;
    break;
  case LEX_TOKEN_TY_OP_LOGICAL_NOT:
    unary_prefix_ty = AST_UNARY_OP_TY_LOGICAL_NOT;
    break;
  case LEX_TOKEN_TY_OP_NOT:
    unary_prefix_ty = AST_UNARY_OP_TY_NOT;
    break;
  case LEX_TOKEN_TY_OP_MUL:
    unary_prefix_ty = AST_UNARY_OP_TY_INDIRECTION;
    break;
  case LEX_TOKEN_TY_OP_AND:
    unary_prefix_ty = AST_UNARY_OP_TY_ADDRESSOF;
    break;
  case LEX_TOKEN_TY_OP_LOGICAL_AND:
    parser->result_ty = PARSE_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        parser->diagnostics,
        MK_PARSER_DIAGNOSTIC(ADDR_LABEL, addr_label, token.span,
                             token.span.start,
                             "address of label not supported"));
    break;
  default:
    // just pure expr
    has_unary_prefix = false;
  }

  if (!has_unary_prefix) {
    return false;
  }

  lex_consume_token(parser->lexer, token);

  struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*sub_expr));
  if (!parse_atom_3(parser, sub_expr)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  expr->ty = AST_EXPR_TY_UNARY_OP;
  expr->unary_op = (struct ast_unary_op){
      .ty = unary_prefix_ty,
      .expr = sub_expr,
  };

  expr->span = MK_TEXT_SPAN(token.span.start, sub_expr->span.end);
  return true;
}

static void parse_type_or_expr(struct parser *parser, struct text_span context,
                               enum parse_type_or_expr_mode mode,
                               struct ast_type_or_expr *type_or_expr) {
  // because of how sizeof works, we need to try and parse `sizeof(<ty_ref>)`
  // first else, something like `sizeof(char) + sizeof(short)` will be
  // resolves as `sizeof( (char) + sizeof(short) )` that is, the size of
  // `+sizeof(short)` cast to `char`

  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span end;
  struct ast_type_name type_name;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, NULL) &&
      parse_type_name(parser, &type_name) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, &end)) {
    type_or_expr->ty = AST_TYPE_OR_EXPR_TY_TYPE;
    type_or_expr->type_name =
        arena_alloc(parser->arena, sizeof(*type_or_expr->type_name));
    *type_or_expr->type_name = type_name;
    type_or_expr->span = MK_TEXT_SPAN(context.start, end.end);
    return;
  }

  lex_backtrack(parser->lexer, pos);

  struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*sub_expr));

  if (mode == PARSE_TYPE_OR_EXPR_MODE_EXPR_NEEDS_PARENS) {
    // FIXME: provide what keyword is causing the err in the message
    parse_expected_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, context.start,
                         "`(` for expr", NULL);
  }

  if (!parse_atom_3(parser, sub_expr)) {
    sub_expr->ty = AST_EXPR_TY_INVALID;
    sub_expr->span = context;

    parser->result_ty = PARSE_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        parser->diagnostics,
        MK_PARSER_DIAGNOSTIC(EXPECTED_EXPR, expected_expr, context, context.end,
                             "expected expression or type-name"));
  }

  type_or_expr->ty = AST_TYPE_OR_EXPR_TY_EXPR;
  type_or_expr->expr = sub_expr;

  type_or_expr->span = MK_TEXT_SPAN(context.start, sub_expr->span.end);

  if (mode == PARSE_TYPE_OR_EXPR_MODE_EXPR_NEEDS_PARENS) {
    // FIXME: provide what keyword is causing the err in the message
    parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, sub_expr->span.end,
                         "`)` after expr", NULL);
  }
}

static bool parse_sizeof(struct parser *parser, struct ast_expr *expr) {
  struct text_span start;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_SIZEOF, &start)) {
    return false;
  }

  parse_type_or_expr(parser, start, PARSE_TYPE_OR_EXPR_MODE_NORMAL,
                     &expr->size_of.type_or_expr);

  expr->ty = AST_EXPR_TY_SIZEOF;
  expr->size_of.span = expr->size_of.type_or_expr.span;
  expr->span = expr->size_of.span;

  return true;
}

static bool parse_alignof(struct parser *parser, struct ast_expr *expr) {
  struct text_span start;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_ALIGNOF, &start)) {
    return false;
  }

  parse_expected_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, start.start,
                       "`(` after alignof", NULL);
  if (!parse_type_name(parser, &expr->align_of.type_name)) {
    struct text_pos end = lex_cur_pos(parser->lexer);

    parser->result_ty = PARSE_RESULT_TY_FAILURE;
    compiler_diagnostics_add(
        parser->diagnostics,
        MK_PARSER_DIAGNOSTIC(EXPECTED_TYPE_NAME, expected_type_name,
                             MK_TEXT_SPAN(start.start, end), end,
                             "expected type-name after align keyword"));

    expr->ty = AST_EXPR_TY_INVALID;
    expr->span = MK_TEXT_SPAN(start.start, end);
    return true;
  }

  parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, start.start,
                       "`)` after alignof keyword", NULL);

  expr->ty = AST_EXPR_TY_ALIGNOF;
  expr->span = MK_TEXT_SPAN(start.start, expr->align_of.type_name.span.end);
  return true;
}

// parses precedence level 2:
// prefix ++, prefix --, unary +, unary -, !, ~, (type), *, &, sizeof,
// _Alignof
static bool parse_atom_3(struct parser *parser, struct ast_expr *expr) {
  if (!parse_unary_prefix_op(parser, expr) && !parse_cast(parser, expr) &&
      !parse_sizeof(parser, expr) && !parse_alignof(parser, expr) &&
      !parse_atom_2(parser, expr)) {
    return false;
  }

  return true;
}

static bool parse_expr_precedence_aware(struct parser *parser,
                                        unsigned min_precedence,
                                        const struct ast_expr *atom,
                                        struct ast_expr *expr) {
  if (atom) {
    *expr = *atom;
  } else if (!parse_atom_3(parser, expr)) {
    return false;
  }

  // TODO: make iterative
  while (true) {
    struct lex_token lookahead;
    lex_peek_token(parser->lexer, &lookahead);
    debug("looked ahead to %s", lex_token_name(parser->lexer, &lookahead));
    struct ast_op_info info;

    if (!op_info_for_token(&lookahead, &info) ||
        info.precedence < min_precedence) {
      return true;
    }

    lex_consume_token(parser->lexer, lookahead);

    DEBUG_ASSERT(info.associativity != AST_ASSOCIATIVITY_NONE,
                 "only operators with associativity should reach here!");
    unsigned next_min_precedence;
    if (info.associativity == AST_ASSOCIATIVITY_LEFT) {
      next_min_precedence = info.precedence + 1;
    } else {
      next_min_precedence = info.precedence;
    }

    struct ast_expr rhs;
    if (!parse_expr_precedence_aware(parser, next_min_precedence, NULL, &rhs)) {
      parser->result_ty = PARSE_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          parser->diagnostics,
          MK_PARSER_DIAGNOSTIC(
              EXPECTED_EXPR, expected_expr,
              MK_TEXT_SPAN(expr->span.start, lookahead.span.end),
              lookahead.span.end, "expected expression after binary operator"));
    }

    // slightly odd design where `expr` now contains lhs and `rhs` contains
    // `rhs` so we need to in-place modify `expr`
    struct ast_expr lhs = *expr;

    expr->ty = AST_EXPR_TY_BINARY_OP;

    struct ast_binary_op *binary_op = &expr->binary_op;
    binary_op->ty = info.ty;

    binary_op->lhs = arena_alloc(parser->arena, sizeof(*binary_op->lhs));
    *binary_op->lhs = lhs;

    binary_op->rhs = arena_alloc(parser->arena, sizeof(*binary_op->rhs));
    *binary_op->rhs = rhs;

    binary_op->span = MK_TEXT_SPAN(lhs.span.start, rhs.span.end);
    expr->span = binary_op->span;
  }
}

static bool parse_ternary(struct parser *parser, const struct ast_expr *cond,
                          struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct ast_expr true_expr, false_expr;
  if (!parse_token(parser, LEX_TOKEN_TY_QMARK, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  // in `cond ? true : false`, `true` is parsed as if it were parenthesised
  if (!parse_compoundexpr_as_expr(parser, &true_expr)) {
    return false;
  }

  parse_expected_token(parser, LEX_TOKEN_TY_COLON, true_expr.span.start,
                       "expected ':' after ternary true expression", NULL);
  parse_expected_expr(parser, &false_expr, "expected expr after ':'");

  expr->ty = AST_EXPR_TY_TERNARY;
  expr->ternary = (struct ast_ternary){
      .cond = arena_alloc(parser->arena, sizeof(*expr->ternary.cond)),
      .true_expr = arena_alloc(parser->arena, sizeof(*expr->ternary.true_expr)),
      .false_expr =
          arena_alloc(parser->arena, sizeof(*expr->ternary.false_expr)),
  };

  *expr->ternary.cond = *cond;
  *expr->ternary.true_expr = true_expr;
  *expr->ternary.false_expr = false_expr;
  expr->ternary.span = MK_TEXT_SPAN(cond->span.start, false_expr.span.end);
  expr->span = expr->ternary.span;
  return true;
}

static bool parse_constant_expr(struct parser *parser, struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct ast_expr atom;
  if (!parse_atom_3(parser, &atom)) {
    return false;
  }

  struct ast_assg assg;
  if (parse_assg(parser, &atom, &assg)) {
    expr->ty = AST_EXPR_TY_ASSG;
    expr->assg = assg;
    expr->span = assg.span;
    return true;
  }

  // all non-assignment expressions
  struct ast_expr cond;
  if (!parse_expr_precedence_aware(parser, 0, &atom, &cond)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_ternary(parser, &cond, expr)) {
    *expr = cond;
  }

  return true;
}

// parse a non-compound expression
static bool parse_expr(struct parser *parser, struct ast_expr *expr) {
  // historically needed, now useless (assg will be rejected in typechk)
  return parse_constant_expr(parser, expr);
}

// there are only two places you can have compound expressions
// * at top level of a statement (e.g `a = 1, b = 2;`)
// * within braces (e.g `(a = 1, b = 2)`)
// so only those places call this method for that purpose.
// `parse_call` calls this method as a helper but doesn't actually parse it as
// a compound expr
static bool parse_compoundexpr_raw(struct parser *parser,
                                   struct ast_compoundexpr *compound_expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  // this could be made recursive instead

  struct vector *exprs =
      vector_create_in_arena(sizeof(struct ast_expr), parser->arena);

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct lex_token token;
  struct ast_expr sub_expr;
  do {
    if (!parse_expr(parser, &sub_expr)) {
      lex_backtrack(parser->lexer, pos);
      break;
    }

    if (first) {
      first = false;
      start = sub_expr.span.start;
    }

    end = sub_expr.span.end;

    vector_push_back(exprs, &sub_expr);

    lex_peek_token(parser->lexer, &token);
  } while (token.ty == LEX_TOKEN_TY_COMMA &&
           /* hacky */ (lex_consume_token(parser->lexer, token), true));

  if (vector_empty(exprs)) {
    return false;
  }

  compound_expr->exprs = vector_head(exprs);
  compound_expr->num_exprs = vector_length(exprs);
  compound_expr->span = MK_TEXT_SPAN(start, end);

  return true;
}

static bool parse_compoundexpr_as_expr(struct parser *parser,
                                       struct ast_expr *expr) {
  struct ast_compoundexpr compound_expr;
  if (!parse_compoundexpr_raw(parser, &compound_expr)) {
    return false;
  }

  // if its one elem, promote it to an expr (as compound expr must have >1
  // expressions)

  if (compound_expr.num_exprs == 1) {
    *expr = compound_expr.exprs[0];
  } else {
    expr->ty = AST_EXPR_TY_COMPOUNDEXPR;
    expr->compound_expr = compound_expr;
    expr->span = compound_expr.span;
  }

  return true;
}

// parse a non-compound expression, ending with a semicolon
static bool parse_compoundexpr_with_semicolon(struct parser *parser,
                                              struct ast_expr *expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  if (parse_compoundexpr_as_expr(parser, expr) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON, NULL)) {
    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static void add_typedefs_to_table(
    struct parser *parser,
    const struct ast_direct_declarator_list *direct_decl_list) {
  for (size_t i = 0; i < direct_decl_list->num_direct_declarators; i++) {

    const struct ast_direct_declarator *direct_decl =
        &direct_decl_list->direct_declarators[i];

    if (direct_decl->ty == AST_DIRECT_DECLARATOR_TY_IDENTIFIER) {
      ustr_t name = identifier_str(parser, &direct_decl->identifier);

      vt_create_entry(&parser->ty_table, VAR_TABLE_NS_TYPEDEF, name);
    } else if (direct_decl->ty == AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR) {
      add_typedefs_to_table(
          parser, &direct_decl->paren_declarator->direct_declarator_list);
    }
  }
}

static bool parse_declaration(struct parser *parser,
                              struct ast_declaration *declaration) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  parse_declaration_specifier_list(parser, TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS,
                                   &declaration->specifier_list);

  if (!declaration->specifier_list.num_decl_specifiers) {
    // need to back out early else parser will try and parse things like
    // function calls as types

    lex_backtrack(parser->lexer, pos);
    return false;
  }

  parse_init_declarator_list(parser, &declaration->declarator_list);

  parse_expected_token(parser, LEX_TOKEN_TY_SEMICOLON,
                       declaration->specifier_list.span.start,
                       "';' after declaration", NULL);

  // FIXME: inefficient
  bool is_typedef = false;
  for (size_t i = 0; i < declaration->specifier_list.num_decl_specifiers; i++) {
    struct ast_declaration_specifier *spec =
        &declaration->specifier_list.decl_specifiers[i];

    if (spec->ty == AST_DECL_SPECIFIER_TY_STORAGE_CLASS_SPECIFIER &&
        spec->storage_class_specifier == AST_STORAGE_CLASS_SPECIFIER_TYPEDEF) {
      is_typedef = true;
      break;
    }
  }

  if (is_typedef) {
    for (size_t i = 0; i < declaration->declarator_list.num_init_declarators;
         i++) {
      struct ast_init_declarator *decl =
          &declaration->declarator_list.init_declarators[i];

      add_typedefs_to_table(parser, &decl->declarator.direct_declarator_list);
    }
  }

  declaration->span = MK_TEXT_SPAN(declaration->specifier_list.span.start,
                                   declaration->declarator_list.span.end);
  return true;
}

static void
parse_declaration_list(struct parser *parser,
                       struct ast_declaration_list *declaration_list) {
  struct vector *list = vector_create_in_arena(
      sizeof(*declaration_list->declarations), parser->arena);

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct ast_declaration declaration;
  while (parse_declaration(parser, &declaration)) {
    if (first) {
      first = false;
      start = declaration.span.start;
    }

    end = declaration.span.end;

    vector_push_back(list, &declaration);
  }

  declaration_list->declarations = vector_head(list);
  declaration_list->num_declarations = vector_length(list);
  declaration_list->span = MK_TEXT_SPAN(start, end);
}

static bool parse_jumpstmt(struct parser *parser,
                           struct ast_jumpstmt *jump_stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span start;
  struct ast_expr expr;
  if (parse_token(parser, LEX_TOKEN_TY_KW_RETURN, &start) &&
      parse_expr(parser, &expr)) {
    parse_expected_token(parser, LEX_TOKEN_TY_SEMICOLON, start.start,
                         "`;` after return expression", NULL);

    jump_stmt->ty = AST_JUMPSTMT_TY_RETURN;
    jump_stmt->return_stmt.expr =
        arena_alloc(parser->arena, sizeof(*jump_stmt->return_stmt.expr));
    *jump_stmt->return_stmt.expr = expr;

    jump_stmt->return_stmt.span = MK_TEXT_SPAN(start.start, expr.span.end);
    ;
    jump_stmt->span = jump_stmt->return_stmt.span;

    return true;
  }

  lex_backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_RETURN, &start)) {
    parse_expected_token(parser, LEX_TOKEN_TY_SEMICOLON, start.start,
                         "`;` or expression after return keyword", NULL);

    jump_stmt->ty = AST_JUMPSTMT_TY_RETURN;
    jump_stmt->return_stmt.expr = NULL;

    jump_stmt->return_stmt.span = start;
    jump_stmt->span = jump_stmt->return_stmt.span;

    return true;
  }

  lex_backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_BREAK, &start)) {
    parse_expected_token(parser, LEX_TOKEN_TY_SEMICOLON, start.start,
                         "`;` after break keyword", NULL);

    jump_stmt->ty = AST_JUMPSTMT_TY_BREAK;
    jump_stmt->span = start;

    return true;
  }

  lex_backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_CONTINUE, &start)) {
    parse_expected_token(parser, LEX_TOKEN_TY_SEMICOLON, start.start,
                         "`;` after continue keyword", NULL);

    jump_stmt->ty = AST_JUMPSTMT_TY_CONTINUE;
    jump_stmt->span = start;

    return true;
  }

  lex_backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_GOTO, &start)) {
    struct lex_token label;
    lex_peek_token(parser->lexer, &label);

    if (label.ty == LEX_TOKEN_TY_IDENTIFIER) {
      lex_consume_token(parser->lexer, label);

      parse_expected_token(parser, LEX_TOKEN_TY_SEMICOLON, start.start,
                           "`;` after goto label", NULL);
      jump_stmt->ty = AST_JUMPSTMT_TY_GOTO;
      jump_stmt->goto_stmt.label = label;
      jump_stmt->goto_stmt.span = MK_TEXT_SPAN(start.start, label.span.end);
      jump_stmt->span = jump_stmt->goto_stmt.span;

      return true;
    }
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

enum parse_stmt_mode {
  PARSE_STMT_MODE_DECL_OR_STMT,
  PARSE_STMT_MODE_NO_DECL,
};

static bool parse_stmt(struct parser *parser, enum parse_stmt_mode mode,
                       struct ast_stmt *stmt);

static bool parse_labeledstmt(struct parser *parser,
                              struct ast_labeledstmt *labeled_stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct lex_token label;
  lex_peek_token(parser->lexer, &label);
  lex_consume_token(parser->lexer, label);

  struct ast_expr expr;

  if (label.ty == LEX_TOKEN_TY_KW_DEFAULT) {
    labeled_stmt->ty = AST_LABELEDSTMT_TY_DEFAULT;
  } else if (label.ty == LEX_TOKEN_TY_KW_CASE && parse_expr(parser, &expr)) {
    labeled_stmt->ty = AST_LABELEDSTMT_TY_CASE;
    labeled_stmt->cnst = expr;
  } else if (label.ty == LEX_TOKEN_TY_IDENTIFIER) {
    labeled_stmt->ty = AST_LABELEDSTMT_TY_LABEL;
    labeled_stmt->label = label;
  } else {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_token(parser, LEX_TOKEN_TY_COLON, NULL) ||
      !parse_stmt(parser, PARSE_STMT_MODE_DECL_OR_STMT, &stmt)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  labeled_stmt->stmt = arena_alloc(parser->arena, sizeof(*labeled_stmt->stmt));
  *labeled_stmt->stmt = stmt;
  labeled_stmt->span = MK_TEXT_SPAN(label.span.start, stmt.span.end);
  return true;
}

static bool parse_ifstmt(struct parser *parser, struct ast_ifstmt *if_stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span kw;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_IF, &kw)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  err_state st = PARSER_ERR_NONE;
  if (!parse_expected_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, kw.start,
                            "'(' as condition must be wrapped in brackets",
                            NULL)) {
    st = parser_err(parser);
  }

  struct ast_expr expr;
  parse_compoundexpr_as_expr(parser, &expr);

  parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, kw.start,
                       "')' as condition must be wrapped in brackets", NULL);

  struct ast_stmt stmt;
  parse_stmt(parser, PARSE_STMT_MODE_NO_DECL, &stmt);

  if (stmt.ty == AST_STMT_TY_DECLARATION) {
  }

  if_stmt->cond = expr;
  if_stmt->body = arena_alloc(parser->arena, sizeof(*if_stmt->body));
  *if_stmt->body = stmt;
  if_stmt->span = MK_TEXT_SPAN(kw.start, stmt.span.end);

  parser_clear_err(parser, st);

  return true;
}

static bool parse_ifelsestmt(struct parser *parser,
                             const struct ast_ifstmt *if_stmt,
                             struct ast_ifelsestmt *if_else_stmt) {
  // parse `if {}`, then try parse `else`
  // not perfectly efficient but more elegant

  struct lex_pos pos = lex_get_position(parser->lexer);

  struct ast_stmt else_stmt;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_ELSE, NULL) ||
      !parse_stmt(parser, PARSE_STMT_MODE_NO_DECL, &else_stmt)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  if_else_stmt->cond = if_stmt->cond;
  if_else_stmt->body = arena_alloc(parser->arena, sizeof(*if_else_stmt->body));
  if_else_stmt->body = if_stmt->body;
  if_else_stmt->else_body =
      arena_alloc(parser->arena, sizeof(*if_else_stmt->else_body));
  *if_else_stmt->else_body = else_stmt;
  if_else_stmt->span = MK_TEXT_SPAN(if_stmt->span.start, else_stmt.span.end);

  return true;
}

static bool parse_switchstmt(struct parser *parser,
                             struct ast_switchstmt *switch_stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  // TODO: some places here can use `parse_expected_token` instead to get better
  // errors

  struct ast_expr ctrl_expr;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_SWITCH, NULL) ||
      !parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, NULL) ||
      !parse_compoundexpr_as_expr(parser, &ctrl_expr) ||
      !parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_stmt(parser, PARSE_STMT_MODE_NO_DECL, &stmt)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  switch_stmt->ctrl_expr = ctrl_expr;
  switch_stmt->body = arena_alloc(parser->arena, sizeof(*switch_stmt->body));
  *switch_stmt->body = stmt;
  switch_stmt->span = MK_TEXT_SPAN(ctrl_expr.span.start, stmt.span.end);

  return true;
}

static bool parse_whilestmt(struct parser *parser,
                            struct ast_whilestmt *while_stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span kw;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_WHILE, &kw) ||
      !parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_expr expr;
  if (!parse_compoundexpr_as_expr(parser, &expr)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_stmt(parser, PARSE_STMT_MODE_NO_DECL, &stmt)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  while_stmt->cond = expr;
  while_stmt->body = arena_alloc(parser->arena, sizeof(*while_stmt->body));
  *while_stmt->body = stmt;
  while_stmt->span = MK_TEXT_SPAN(kw.start, stmt.span.end);

  return true;
}

static bool parse_dowhilestmt(struct parser *parser,
                              struct ast_dowhilestmt *do_while_stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span kw;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_DO, &kw)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_stmt(parser, PARSE_STMT_MODE_NO_DECL, &stmt)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_KW_WHILE, NULL) ||
      !parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_expr expr;
  if (!parse_compoundexpr_as_expr(parser, &expr)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  do_while_stmt->cond = expr;
  do_while_stmt->body =
      arena_alloc(parser->arena, sizeof(*do_while_stmt->body));
  *do_while_stmt->body = stmt;
  do_while_stmt->span = MK_TEXT_SPAN(kw.start, stmt.span.end);

  return true;
}

static bool
parse_declaration_or_expr(struct parser *parser,
                          struct ast_declaration_or_expr *decl_or_expr) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  if (parse_declaration(parser, &decl_or_expr->decl)) {
    decl_or_expr->ty = AST_DECLARATION_OR_EXPR_TY_DECL;
    decl_or_expr->span = decl_or_expr->decl.span;
    return true;
  }

  if (parse_compoundexpr_as_expr(parser, &decl_or_expr->expr)) {
    parse_expected_token(parser, LEX_TOKEN_TY_SEMICOLON,
                         decl_or_expr->expr.span.start,
                         "`;` after expr in for loop initializer", NULL);
    decl_or_expr->ty = AST_DECLARATION_OR_EXPR_TY_EXPR;
    decl_or_expr->span = decl_or_expr->expr.span;
    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static bool parse_forstmt(struct parser *parser, struct ast_forstmt *for_stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  // first parse the `for (`
  struct text_span kw;
  if (!(parse_token(parser, LEX_TOKEN_TY_KW_FOR, &kw) &&
        parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, NULL))) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  // then, we look for a vardecllist or an expression, or neither
  struct ast_declaration_or_expr decl_or_expr;
  if (parse_declaration_or_expr(parser, &decl_or_expr)) {
    for_stmt->init = arena_alloc(parser->arena, sizeof(*for_stmt->init));
    *for_stmt->init = decl_or_expr;
  } else if (parse_token(parser, LEX_TOKEN_TY_SEMICOLON, NULL)) {
    for_stmt->init = NULL;
  } else {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  // parse the condition if present, else a semicolon
  struct ast_expr cond;
  if (parse_compoundexpr_as_expr(parser, &cond)) {
    for_stmt->cond = arena_alloc(parser->arena, sizeof(*for_stmt->cond));
    *for_stmt->cond = cond;
  } else {
    for_stmt->cond = NULL;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_SEMICOLON, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  // parse the iteration statement if present, else nothing
  struct ast_expr iter;
  if (parse_compoundexpr_as_expr(parser, &iter)) {
    for_stmt->iter = arena_alloc(parser->arena, sizeof(*for_stmt->iter));
    // FIXME: there are more places where compound expressions are legal
    // rework expression parsing to handle them better
    *for_stmt->iter = iter;
  } else {
    for_stmt->iter = NULL;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt body;
  if (!parse_stmt(parser, PARSE_STMT_MODE_NO_DECL, &body)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  for_stmt->body = arena_alloc(parser->arena, sizeof(*for_stmt->body));
  *for_stmt->body = body;
  for_stmt->span = MK_TEXT_SPAN(kw.start, body.span.end);

  return true;
}

static bool parse_iterstmt(struct parser *parser,
                           struct ast_iterstmt *iter_stmt) {
  struct ast_whilestmt while_stmt;
  if (parse_whilestmt(parser, &while_stmt)) {
    iter_stmt->ty = AST_ITERSTMT_TY_WHILE;
    iter_stmt->while_stmt = while_stmt;
    iter_stmt->span = while_stmt.span;
    return true;
  }

  struct ast_dowhilestmt do_while_stmt;
  if (parse_dowhilestmt(parser, &do_while_stmt)) {
    iter_stmt->ty = AST_ITERSTMT_TY_DO_WHILE;
    iter_stmt->do_while_stmt = do_while_stmt;
    iter_stmt->span = do_while_stmt.span;
    return true;
  }

  struct ast_forstmt for_stmt;
  if (parse_forstmt(parser, &for_stmt)) {
    iter_stmt->ty = AST_ITERSTMT_TY_FOR;
    iter_stmt->for_stmt = for_stmt;
    iter_stmt->span = for_stmt.span;
    return true;
  }

  return false;
}

static bool parse_selectstmt(struct parser *parser,
                             struct ast_selectstmt *select_stmt) {

  struct ast_ifstmt if_stmt;
  if (parse_ifstmt(parser, &if_stmt)) {
    struct ast_ifelsestmt if_else_stmt;

    if (parse_ifelsestmt(parser, &if_stmt, &if_else_stmt)) {
      select_stmt->ty = AST_SELECTSTMT_TY_IF_ELSE;
      select_stmt->if_else_stmt = if_else_stmt;
      select_stmt->span = if_else_stmt.span;
      return true;
    }

    select_stmt->ty = AST_SELECTSTMT_TY_IF;
    select_stmt->if_stmt = if_stmt;
    select_stmt->span = if_stmt.span;
    return true;
  }

  struct ast_switchstmt switch_stmt;
  if (parse_switchstmt(parser, &switch_stmt)) {
    select_stmt->ty = AST_SELECTSTMT_TY_SWITCH;
    select_stmt->switch_stmt = switch_stmt;
    select_stmt->span = switch_stmt.span;
    return true;
  }

  return false;
}

static bool parse_staticassert(struct parser *parser,
                               struct ast_staticassert *staticassert) {
  struct text_span start;

  if (!parse_token(parser, LEX_TOKEN_TY_KW_STATICASSERT, &start)) {
    return false;
  }

  parse_expected_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, start.start,
                       "'(' after 'static_assert' keyword", NULL);

  parse_expected_expr(parser, &staticassert->cond,
                      "expr after 'static_assert'");

  if (parse_token(parser, LEX_TOKEN_TY_COMMA, NULL)) {
    staticassert->message =
        arena_alloc(parser->arena, sizeof(*staticassert->message));

    parse_expected_expr(parser, staticassert->message,
                        "message after 'static_assert' expr");
  } else {
    staticassert->message = NULL;
  }

  struct text_span end;
  parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, start.start,
                       "'(' after 'static_assert' keyword", &end);

  staticassert->span = MK_TEXT_SPAN(start.start, end.end);

  return true;
}

static bool parse_compoundstmt(struct parser *parser,
                               struct ast_compoundstmt *compound_stmt);

static bool parse_stmt(struct parser *parser, enum parse_stmt_mode mode,
                       struct ast_stmt *stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span null_span;
  if (parse_token(parser, LEX_TOKEN_TY_SEMICOLON, &null_span)) {
    stmt->ty = AST_STMT_TY_NULL;
    stmt->span = null_span;
    return true;
  }

  // must parse labels first in case the label name is also a declaration
  struct ast_labeledstmt labeled_stmt;
  if (parse_labeledstmt(parser, &labeled_stmt)) {
    stmt->ty = AST_STMT_TY_LABELED;
    stmt->labeled = labeled_stmt;
    stmt->span = labeled_stmt.span;
    return true;
  }

  struct ast_declaration declaration;
  if (parse_declaration(parser, &declaration)) {
    stmt->ty = AST_STMT_TY_DECLARATION;
    stmt->declaration = declaration;
    stmt->span = declaration.span;

    if (mode == PARSE_STMT_MODE_NO_DECL) {
      parser->result_ty = PARSE_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          parser->diagnostics,
          MK_PARSER_DIAGNOSTIC(
              SYNTAX_ERR, syntax_err, declaration.span, declaration.span.start,
              "declaration statement not legal here (wrap it in '{}')"));
    }

    return true;
  }

  struct ast_jumpstmt jump_stmt;
  if (parse_jumpstmt(parser, &jump_stmt)) {
    stmt->ty = AST_STMT_TY_JUMP;
    stmt->jump = jump_stmt;
    stmt->span = jump_stmt.span;
    return true;
  }

  struct ast_selectstmt select_stmt;
  if (parse_selectstmt(parser, &select_stmt)) {
    stmt->ty = AST_STMT_TY_SELECT;
    stmt->select = select_stmt;
    stmt->span = select_stmt.span;
    return true;
  }

  struct ast_iterstmt iter_stmt;
  if (parse_iterstmt(parser, &iter_stmt)) {
    stmt->ty = AST_STMT_TY_ITER;
    stmt->iter = iter_stmt;
    stmt->span = iter_stmt.span;
    return true;
  }

  struct ast_compoundstmt compound_stmt;
  if (parse_compoundstmt(parser, &compound_stmt)) {
    stmt->ty = AST_STMT_TY_COMPOUND;
    stmt->compound = compound_stmt;
    stmt->span = compound_stmt.span;
    return true;
  }

  struct ast_expr compound_expr;
  if (parse_compoundexpr_with_semicolon(parser, &compound_expr)) {
    stmt->ty = AST_STMT_TY_EXPR;
    stmt->expr = compound_expr;
    stmt->span = compound_expr.span;

    return true;
  }

  struct ast_staticassert staticassert;
  if (parse_staticassert(parser, &staticassert)) {
    stmt->ty = AST_STMT_TY_STATICASSERT;
    stmt->staticassert = staticassert;
    stmt->span = staticassert.span;
    return true;
  }

  lex_backtrack(parser->lexer, pos);
  return false;
}

static bool parse_compoundstmt(struct parser *parser,
                               struct ast_compoundstmt *compound_stmt) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  vt_push_scope(&parser->ty_table);

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE, NULL)) {
    lex_backtrack(parser->lexer, pos);
    vt_pop_scope(&parser->ty_table);
    return false;
  }

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct vector *stmts =
      vector_create_in_arena(sizeof(struct ast_stmt), parser->arena);
  {
    struct ast_stmt stmt;
    while (parse_stmt(parser, PARSE_STMT_MODE_DECL_OR_STMT, &stmt)) {
      if (first) {
        first = false;
        start = stmt.span.start;
      }

      end = stmt.span.end;

      vector_push_back(stmts, &stmt);
    }
  }

  compound_stmt->stmts = vector_head(stmts);
  compound_stmt->num_stmts = vector_length(stmts);

  parse_expected_token(parser, LEX_TOKEN_TY_CLOSE_BRACE, end,
                       "`}` at end of compound stmt", NULL);

  vt_pop_scope(&parser->ty_table);
  compound_stmt->span = MK_TEXT_SPAN(start, end);
  return true;
}

static bool parse_param(struct parser *parser, struct ast_param *param) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct text_span kw;
  if (parse_token(parser, LEX_TOKEN_TY_ELLIPSIS, &kw)) {
    param->ty = AST_PARAM_TY_VARIADIC;
    param->span = kw;
    return true;
  } else if (parse_token(parser, LEX_TOKEN_TY_KW_VOID, &kw)) {
    struct lex_token token;
    lex_peek_token(parser->lexer, &token);
    if (token.ty == LEX_TOKEN_TY_CLOSE_BRACKET) {
      param->ty = AST_PARAM_TY_VOID;

      // a few places have spans that dont cover all punctuation. is this
      // preferable?
      param->span = kw;
      return true;
    }

    lex_backtrack(parser->lexer, pos);
  }

  parse_declaration_specifier_list(parser, TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS,
                                   &param->specifier_list);

  if (!param->specifier_list.num_decl_specifiers) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  if (parse_declarator(parser, &param->declarator)) {
    param->ty = AST_PARAM_TY_DECL;
    param->span = MK_TEXT_SPAN(kw.start, param->declarator.span.end);
    return true;
  } else if (parse_abstract_declarator(parser, &param->abstract_declarator)) {
    param->ty = AST_PARAM_TY_ABSTRACT_DECL;
    param->span = MK_TEXT_SPAN(kw.start, param->abstract_declarator.span.end);
    return true;
  } else {
    param->ty = AST_PARAM_TY_ABSTRACT_DECL;
    param->abstract_declarator = (struct ast_abstract_declarator){
        .direct_abstract_declarator_list = {.direct_abstract_declarators = NULL,
                                            .num_direct_abstract_declarators =
                                                0},
        .pointer_list = {.pointers = NULL, .num_pointers = 0}};

    param->span = MK_TEXT_SPAN(kw.start, kw.end);
    return true;
  }
}

static bool parse_paramlist(struct parser *parser,
                            struct ast_paramlist *param_list) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  struct vector *params =
      vector_create_in_arena(sizeof(struct ast_param), parser->arena);

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  param_list->params = NULL;
  param_list->num_params = 0;

  bool first = true;
  struct text_pos start = lex_cur_pos(parser->lexer);
  struct text_pos end = start;

  struct ast_param param;
  while (parse_param(parser, &param)) {
    if (first) {
      first = false;
      start = param.span.start;
    }

    end = param.span.end;

    vector_push_back(params, &param);

    if (!parse_token(parser, LEX_TOKEN_TY_COMMA, NULL)) {
      break;
    }
  }

  // allow trailing comma
  parse_token(parser, LEX_TOKEN_TY_COMMA, NULL);

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET, NULL)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  param_list->params = vector_head(params);
  param_list->num_params = vector_length(params);
  param_list->span = MK_TEXT_SPAN(start, end);

  return true;
}

static bool parse_funcdef(struct parser *parser, struct ast_funcdef *func_def) {
  struct lex_pos pos = lex_get_position(parser->lexer);

  parse_declaration_specifier_list(parser, TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS,
                                   &func_def->specifier_list);

  if (!parse_declarator(parser, &func_def->declarator)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  parse_declaration_list(parser, &func_def->declaration_list);

  if (!parse_compoundstmt(parser, &func_def->body)) {
    lex_backtrack(parser->lexer, pos);
    return false;
  }

  func_def->span = MK_TEXT_SPAN(func_def->specifier_list.span.start,
                                func_def->body.span.end);
  return true;
}

#define TOKEN_FMT(lexer, token)                                                \
  text_pos_len((token).start, (token).end),                                    \
      text_pos_len((token).start, (token).end), lexer->text[(token).start.idx]

ustr_t identifier_str(struct parser *parser,
                                const struct lex_token *token) {
  return lex_associated_text(parser->lexer, token);
}

static bool parse_external_declaration(
    struct parser *parser,
    struct ast_external_declaration *external_declaration) {
  if (parse_funcdef(parser, &external_declaration->func_def)) {
    external_declaration->ty = AST_EXTERNAL_DECLARATION_TY_FUNC_DEF;
    external_declaration->span = external_declaration->func_def.span;
    return true;
  }

  if (parse_declaration(parser, &external_declaration->declaration)) {
    external_declaration->ty = AST_EXTERNAL_DECLARATION_TY_DECLARATION;
    external_declaration->span = external_declaration->declaration.span;
    return true;
  }

  if (parse_staticassert(parser, &external_declaration->staticassert)) {
    external_declaration->ty = AST_EXTERNAL_DECLARATION_TY_STATIC_ASSERT;
    external_declaration->span = external_declaration->staticassert.span;
    return true;
  }

  return false;
}

struct parse_result parse(struct parser *parser) {
  struct lexer *lexer = parser->lexer;

  struct vector *declarations = vector_create_in_arena(
      sizeof(struct ast_external_declaration), parser->arena);

  struct text_span last = {0};
  while (true) {
    if (lexer_at_eof(lexer)) {
      info("EOF reached by lexer");
      break;
    }

    struct text_span null_span;
    if (parse_token(parser, LEX_TOKEN_TY_SEMICOLON, &null_span)) {
      continue;
    }

    struct ast_external_declaration external_declaration;
    if (parse_external_declaration(parser, &external_declaration)) {
      vector_push_back(declarations, &external_declaration);
      last = external_declaration.span;
      continue;
    }

    if (!lexer_at_eof(lexer)) {
      // parser failed
      struct text_pos pos = lex_cur_pos(lexer);
      err("parser finished at position %zu, line=%zu, col=%zu", pos.idx,
          pos.line, pos.col);

      struct lex_token fail;
      lex_peek_token(lexer, &fail);

      parser->result_ty = PARSE_RESULT_TY_FAILURE;
      compiler_diagnostics_add(
          parser->diagnostics,
          MK_PARSER_DIAGNOSTIC(SYNTAX_ERR, syntax_err,
                               MK_TEXT_SPAN(last.end, fail.span.end), pos,
                               "syntax error"));
      break;
    }

    BUG("parser hit nothing");
  }

  struct ast_translationunit translation_unit;

  if (compiler_diagnostics_err(parser->diagnostics)) {
    parser->result_ty = PARSE_RESULT_TY_FAILURE;
  }

  translation_unit.external_declarations = vector_head(declarations);
  translation_unit.num_external_declarations = vector_length(declarations);

  struct parse_result result = {.ty = parser->result_ty,
                                .translation_unit = translation_unit};

  return result;
}

struct ast_printstate {
  struct parser *parser;
  int indent;

  struct graphwriter *gwr;
};

#define DEBUG_FUNC_ENUM(ty, name)                                              \
  START_NO_UNUSED_ARGS                                                         \
  static void parse_debug_print_##ty(struct ast_printstate *state,             \
                                     enum ast_##ty *name) END_NO_UNUSED_ARGS

#define DEBUG_FUNC(ty, name)                                                   \
  START_NO_UNUSED_ARGS                                                         \
  static void parse_debug_print_##ty(struct ast_printstate *state,             \
                                     struct ast_##ty *name) END_NO_UNUSED_ARGS

#define DEBUG_CALL(ty, val) parse_debug_print_##ty(state, val)

#define AST_PRINT_SAMELINE_Z_NOINDENT(fmt) slogsl(fmt)
#define AST_PRINT_SAMELINE_NOINDENT(fmt, ...) slogsl(fmt, __VA_ARGS__)

#define AST_PRINT_SAMELINE_Z(fmt) slogsl("%*s" fmt, state->indent * 4, "")
#define AST_PRINT_SAMELINE(fmt, ...)                                           \
  slogsl("%*s" fmt, state->indent * 4, "", __VA_ARGS__)

#define AST_PRINTZ(fmt) AST_PRINT_SAMELINE_Z(fmt "\n")
#define AST_PRINT(fmt, ...) AST_PRINT_SAMELINE(fmt "\n", __VA_ARGS__)

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

DEBUG_FUNC_ENUM(function_specifier, function_specifier) {
  switch (*function_specifier) {
  case AST_FUNCTION_SPECIFIER_INLINE:
    AST_PRINTZ("INLINE");
    break;
  case AST_FUNCTION_SPECIFIER_NORETURN:
    AST_PRINTZ("NORETURN");
    break;
  }
}

DEBUG_FUNC_ENUM(storage_class_specifier, storage_class_specifier) {
  switch (*storage_class_specifier) {
  case AST_STORAGE_CLASS_SPECIFIER_AUTO:
    AST_PRINTZ("AUTO");
    break;
  case AST_STORAGE_CLASS_SPECIFIER_STATIC:
    AST_PRINTZ("STATIC");
    break;
  case AST_STORAGE_CLASS_SPECIFIER_REGISTER:
    AST_PRINTZ("REGISTER");
    break;
  case AST_STORAGE_CLASS_SPECIFIER_EXTERN:
    AST_PRINTZ("EXTERN");
    break;
  case AST_STORAGE_CLASS_SPECIFIER_TYPEDEF:
    AST_PRINTZ("TYPEDEF");
    break;
  }
}

DEBUG_FUNC_ENUM(type_qualifier, type_qualifier) {
  switch (*type_qualifier) {
  case AST_TYPE_QUALIFIER_CONST:
    AST_PRINTZ("CONST");
    break;
  case AST_TYPE_QUALIFIER_VOLATILE:
    AST_PRINTZ("VOLATILE");
    break;
  case AST_TYPE_QUALIFIER_RESTRICT:
    AST_PRINTZ("RESTRICT");
    break;
  case AST_TYPE_QUALIFIER_NONNULL:
    AST_PRINTZ("NONNULL");
    break;
  case AST_TYPE_QUALIFIER_NULLABLE:
    AST_PRINTZ("NULLABLE");
    break;
  }
}

DEBUG_FUNC_ENUM(type_specifier_kw, type_specifier_kw) {
  switch (*type_specifier_kw) {
  case AST_TYPE_SPECIFIER_KW_VOID:
    AST_PRINTZ("VOID");
    break;
  case AST_TYPE_SPECIFIER_KW_CHAR:
    AST_PRINTZ("CHAR");
    break;
  case AST_TYPE_SPECIFIER_KW_SHORT:
    AST_PRINTZ("SHORT");
    break;
  case AST_TYPE_SPECIFIER_KW_INT:
    AST_PRINTZ("INT");
    break;
  case AST_TYPE_SPECIFIER_KW_LONG:
    AST_PRINTZ("LONG");
    break;
  case AST_TYPE_SPECIFIER_KW_FLOAT:
    AST_PRINTZ("FLOAT");
    break;
  case AST_TYPE_SPECIFIER_KW_DOUBLE:
    AST_PRINTZ("DOUBLE");
    break;
  case AST_TYPE_SPECIFIER_KW_SIGNED:
    AST_PRINTZ("SIGNED");
    break;
  case AST_TYPE_SPECIFIER_KW_UNSIGNED:
    AST_PRINTZ("UNSIGNED");
    break;
  case AST_TYPE_SPECIFIER_KW_BOOL:
    AST_PRINTZ("BOOL");
    break;
  case AST_TYPE_SPECIFIER_KW_COMPLEX:
    AST_PRINTZ("COMPLEX");
    break;
  case AST_TYPE_SPECIFIER_KW_HALF:
    AST_PRINTZ("HALF");
    break;
  case AST_TYPE_SPECIFIER_KW_UINT128:
    AST_PRINTZ("UINT128");
    break;
  }
}

DEBUG_FUNC(declarator, declarator);
DEBUG_FUNC(declaration, declaration);
DEBUG_FUNC(declaration_specifier_list, specifier_list);
DEBUG_FUNC(expr, expr);

DEBUG_FUNC(declaration_list, declaration_list) {
  for (size_t i = 0; i < declaration_list->num_declarations; i++) {
    DEBUG_CALL(declaration, &declaration_list->declarations[i]);
  }
}

#define AST_PRINT_IDENTIFIER(identifier)                                       \
  do {                                                                         \
    ustr_t str = identifier_str(state->parser, identifier);          \
    AST_PRINT("'%.*s'", (int)str.len, str.str);                                \
    AST_PRINTZ("");                                                            \
  } while (0)

#define AST_PRINT_IDENTIFIER_SAMELINE(identifier)                              \
  do {                                                                         \
    ustr_t str = identifier_str(state->parser, identifier);          \
    AST_PRINT_SAMELINE_NOINDENT("'%.*s'", (int)str.len, str.str);              \
    AST_PRINTZ("");                                                            \
  } while (0)

DEBUG_FUNC(struct_or_union_specifier, struct_or_union_specifier) {
  switch (struct_or_union_specifier->ty) {
  case AST_STRUCT_OR_UNION_SPECIFIER_TY_STRUCT:
    if (struct_or_union_specifier->identifier) {
      AST_PRINT_SAMELINE_Z("STRUCT ");
      AST_PRINT_IDENTIFIER_SAMELINE(struct_or_union_specifier->identifier);

    } else {
      AST_PRINTZ("STRUCT");
    }
    break;
  case AST_STRUCT_OR_UNION_SPECIFIER_TY_UNION:
    if (struct_or_union_specifier->identifier) {
      AST_PRINT_SAMELINE_Z("UNION ");
      AST_PRINT_IDENTIFIER_SAMELINE(struct_or_union_specifier->identifier);
    } else {
      AST_PRINTZ("UNION");
    }
    break;
  }

  if (struct_or_union_specifier->decl_list) {
    DEBUG_CALL(declaration_list, struct_or_union_specifier->decl_list);
  }
}

DEBUG_FUNC(enumerator, enumerator) {
  AST_PRINT_SAMELINE_Z("ENUMERATOR ");
  AST_PRINT_IDENTIFIER_SAMELINE(&enumerator->identifier);

  if (enumerator->value) {
    AST_PRINTZ("VALUE");
    INDENT();
    DEBUG_CALL(expr, enumerator->value);
    UNINDENT();
  }
}

DEBUG_FUNC(enumerator_list, enumerator_list) {
  for (size_t i = 0; i < enumerator_list->num_enumerators; i++) {
    DEBUG_CALL(enumerator, &enumerator_list->enumerators[i]);
  }
}

DEBUG_FUNC(enum_specifier, enum_specifier) {
  if (enum_specifier->identifier) {
    AST_PRINT_SAMELINE_Z("ENUM ");
    AST_PRINT_IDENTIFIER_SAMELINE(enum_specifier->identifier);
  } else {
    AST_PRINTZ("ENUM");
  }

  if (enum_specifier->enumerator_list) {
    DEBUG_CALL(enumerator_list, enum_specifier->enumerator_list);
  }
}

DEBUG_FUNC(type_or_expr, type_or_expr);

DEBUG_FUNC(type_specifier, type_specifier) {
  AST_PRINTZ("TYPE SPECIFIER");
  INDENT();
  switch (type_specifier->ty) {
  case AST_TYPE_SPECIFIER_TY_KW:
    DEBUG_CALL(type_specifier_kw, &type_specifier->type_specifier_kw);
    break;
  case AST_TYPE_SPECIFIER_STRUCT_OR_UNION:
    DEBUG_CALL(struct_or_union_specifier,
               &type_specifier->struct_or_union_specifier);
    break;
  case AST_TYPE_SPECIFIER_ENUM:
    DEBUG_CALL(enum_specifier, &type_specifier->enum_specifier);
    break;
  case AST_TYPE_SPECIFIER_TYPEDEF_NAME:
    AST_PRINT_SAMELINE_Z("TYPEDEF ");
    AST_PRINT_IDENTIFIER_SAMELINE(&type_specifier->typedef_name);
    break;
  case AST_TYPE_SPECIFIER_TYPEOF:
    AST_PRINTZ("TYPEOF");
    DEBUG_CALL(type_or_expr, &type_specifier->type_of);
    break;
  case AST_TYPE_SPECIFIER_TYPEOF_UNQUAL:
    AST_PRINTZ("TYPEOF_UNQUAL");
    DEBUG_CALL(type_or_expr, &type_specifier->type_of_unqual);
  }
  UNINDENT();
}

DEBUG_FUNC(compoundstmt, compound_stmt);

DEBUG_FUNC(var, var) {
  AST_PRINT_SAMELINE_Z("VARIABLE ");
  AST_PRINT_IDENTIFIER_SAMELINE(&var->identifier);
}

DEBUG_FUNC(cnst, cnst) {
  switch (cnst->ty) {
  case AST_CNST_TY_BOOL:
    AST_PRINT("CONSTANT %s", ap_val_nonzero(cnst->num_value) ? "true" : "false");
    break;
  case AST_CNST_TY_SIGNED_INT:
  case AST_CNST_TY_UNSIGNED_INT:
  case AST_CNST_TY_SIGNED_LONG:
  case AST_CNST_TY_UNSIGNED_LONG:
  case AST_CNST_TY_SIGNED_LONG_LONG:
  case AST_CNST_TY_UNSIGNED_LONG_LONG:
  case AST_CNST_TY_FLOAT:
  case AST_CNST_TY_DOUBLE:
  case AST_CNST_TY_LONG_DOUBLE:
    AST_PRINT_SAMELINE_Z("CONSTANT ");
    ap_val_fprintf(stderr, cnst->num_value);
    AST_PRINTZ("");
    break;
  case AST_CNST_TY_CHAR:
    switch (cnst->num_value.ty) {
    case AP_VAL_TY_INVALID:
      AST_PRINT_SAMELINE_Z("CONSTANT ");
      ap_val_fprintf(stderr, cnst->num_value);
      AST_PRINTZ("");
      break;
    case AP_VAL_TY_INT:
      AST_PRINT("CONSTANT '%c'", (char)ap_int_as_ull(cnst->num_value.ap_int));
      break;
    case AP_VAL_TY_FLOAT:
      unreachable();
    }
    break;
  case AST_CNST_TY_WIDE_CHAR:
    AST_PRINT_SAMELINE_Z("CONSTANT (wide char) ");
    ap_val_fprintf(stderr, cnst->num_value);
    AST_PRINTZ("");
    break;
  case AST_CNST_TY_STR_LITERAL:
    AST_PRINT_SAMELINE_Z("CONSTANT ");
    fprint_str(stderr, cnst->str_value.ascii.value, cnst->str_value.ascii.len);
    fprintf(stderr, "\n");
    break;
  case AST_CNST_TY_WIDE_STR_LITERAL:
    AST_PRINT_SAMELINE_Z("CONSTANT ");
    fprint_wstr(stderr, cnst->str_value.wide.value, cnst->str_value.wide.len);
    fprintf(stderr, "\n");
    break;
  }
}

DEBUG_FUNC(compoundexpr, compound_expr);

DEBUG_FUNC(pointer, pointer) {
  AST_PRINTZ("POINTER");
  DEBUG_CALL(declaration_specifier_list, &pointer->specifier_list);
}

DEBUG_FUNC(pointer_list, pointer_list) {
  if (!pointer_list->num_pointers) {
    return;
  }

  AST_PRINTZ("POINTER LIST");
  INDENT();
  for (size_t i = 0; i < pointer_list->num_pointers; i++) {
    DEBUG_CALL(pointer, &pointer_list->pointers[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(abstract_declarator, abstract_declarator);

DEBUG_FUNC(array_declarator, array_declarator) {
  AST_PRINTZ("ARRAY DECLARATOR");

  INDENT();
  switch (array_declarator->ty) {
  case AST_ARRAY_DECLARATOR_TY_STAR:
    AST_PRINTZ("STAR");
    break;
  case AST_ARRAY_DECLARATOR_TY_STATIC_SIZED:
    AST_PRINTZ("STATIC");
    goto sized;
  sized:
  case AST_ARRAY_DECLARATOR_TY_SIZED:
    AST_PRINTZ("SIZED");
    DEBUG_CALL(expr, array_declarator->size);
    break;
  case AST_ARRAY_DECLARATOR_TY_UNSIZED:
    AST_PRINTZ("UNSIZED");
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(paramlist, param_list);

DEBUG_FUNC(func_declarator, func_declarator) {
  AST_PRINTZ("FUNC DECLARATOR");

  INDENT();
  DEBUG_CALL(paramlist, func_declarator->param_list);
  UNINDENT();
}

DEBUG_FUNC(direct_abstract_declarator, direct_abstract_declarator) {
  AST_PRINTZ("DIRECT ABSTRACT DECLARATOR");

  INDENT();
  switch (direct_abstract_declarator->ty) {
  case AST_DIRECT_ABSTRACT_DECLARATOR_TY_PAREN_DECLARATOR:
    DEBUG_CALL(abstract_declarator,
               direct_abstract_declarator->paren_declarator);
    break;
  case AST_DIRECT_ABSTRACT_DECLARATOR_TY_ARRAY_DECLARATOR:
    DEBUG_CALL(array_declarator, direct_abstract_declarator->array_declarator);
    break;
  case AST_DIRECT_ABSTRACT_DECLARATOR_TY_FUNC_DECLARATOR:
    DEBUG_CALL(func_declarator, direct_abstract_declarator->func_declarator);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(direct_abstract_declarator_list, direct_abstract_declarator_list) {
  AST_PRINTZ("DIRECT ABSTRACT DECLARATOR LIST");
  INDENT();
  for (size_t i = 0;
       i < direct_abstract_declarator_list->num_direct_abstract_declarators;
       i++) {
    DEBUG_CALL(
        direct_abstract_declarator,
        &direct_abstract_declarator_list->direct_abstract_declarators[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(abstract_declarator, abstract_declarator) {
  AST_PRINTZ("ABSTRACT DECLARATOR");
  INDENT();
  DEBUG_CALL(pointer_list, &abstract_declarator->pointer_list);
  DEBUG_CALL(direct_abstract_declarator_list,
             &abstract_declarator->direct_abstract_declarator_list);
  UNINDENT();
}

DEBUG_FUNC(direct_declarator, direct_declarator) {
  AST_PRINTZ("DIRECT DECLARATOR");

  INDENT();
  switch (direct_declarator->ty) {
  case AST_DIRECT_DECLARATOR_TY_IDENTIFIER:
    AST_PRINT_SAMELINE_Z("IDENTIFIER ");
    AST_PRINT_IDENTIFIER_SAMELINE(&direct_declarator->identifier);
    break;
  case AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR:
    DEBUG_CALL(declarator, direct_declarator->paren_declarator);
    break;
  case AST_DIRECT_DECLARATOR_TY_ARRAY_DECLARATOR:
    DEBUG_CALL(array_declarator, direct_declarator->array_declarator);
    break;
  case AST_DIRECT_DECLARATOR_TY_FUNC_DECLARATOR:
    DEBUG_CALL(func_declarator, direct_declarator->func_declarator);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(direct_declarator_list, direct_declarator_list) {
  AST_PRINTZ("DIRECT DECLARATOR LIST");
  INDENT();
  for (size_t i = 0; i < direct_declarator_list->num_direct_declarators; i++) {
    DEBUG_CALL(direct_declarator,
               &direct_declarator_list->direct_declarators[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(attribute_specifier, attribute_specifier);

DEBUG_FUNC(attribute_specifier_list, attribute_specifier_list) {
  for (size_t i = 0; i < attribute_specifier_list->num_attribute_specifiers;
       i++) {
    DEBUG_CALL(attribute_specifier,
               &attribute_specifier_list->attribute_specifiers[i]);
  }
}

DEBUG_FUNC(declarator, declarator) {
  AST_PRINTZ("DECLARATOR");
  INDENT();
  DEBUG_CALL(pointer_list, &declarator->pointer_list);
  DEBUG_CALL(direct_declarator_list, &declarator->direct_declarator_list);
  DEBUG_CALL(attribute_specifier_list, &declarator->attribute_specifier_list);
  UNINDENT();
}

DEBUG_FUNC(attribute_param, attribute_param) {
  DEBUG_CALL(expr, attribute_param->expr);
}

DEBUG_FUNC(attribute, attribute) {
  switch (attribute->ty) {
  case AST_ATTRIBUTE_TY_EMPTY:
    AST_PRINTZ("EMPTY");
    break;
  case AST_ATTRIBUTE_TY_NAMED:
    AST_PRINT_IDENTIFIER(&attribute->name);
    break;
  case AST_ATTRIBUTE_TY_PARAMETERIZED:
    AST_PRINT_IDENTIFIER(&attribute->name);
    AST_PRINTZ("ATTRIBUTE PARAMS");
    INDENT();
    for (size_t i = 0; i < attribute->num_params; i++) {
      DEBUG_CALL(attribute_param, &attribute->params[i]);
    }
    UNINDENT();
    break;
  }
}

DEBUG_FUNC(attribute_list, attribute_list) {
  AST_PRINTZ("ATTRIBUTE LIST");
  INDENT();
  for (size_t i = 0; i < attribute_list->num_attributes; i++) {
    DEBUG_CALL(attribute, &attribute_list->attributes[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(attribute_specifier, attribute_specifier) {
  DEBUG_CALL(attribute_list, &attribute_specifier->attribute_list);
}

DEBUG_FUNC(declaration_specifier, specifier) {
  AST_PRINTZ("DECLARATION SPECIFIER");
  INDENT();

  switch (specifier->ty) {
  case AST_DECL_SPECIFIER_TY_STORAGE_CLASS_SPECIFIER:
    DEBUG_CALL(storage_class_specifier, &specifier->storage_class_specifier);
    break;
  case AST_DECL_SPECIFIER_TY_TYPE_SPECIFIER:
    DEBUG_CALL(type_specifier, &specifier->type_specifier);
    break;
  case AST_DECL_SPECIFIER_TY_TYPE_QUALIFIER:
    DEBUG_CALL(type_qualifier, &specifier->type_qualifier);
    break;
  case AST_DECL_SPECIFIER_TY_FUNCTION_SPECIFIER:
    DEBUG_CALL(function_specifier, &specifier->function_specifier);
    break;
  case AST_DECL_SPECIFIER_TY_ATTRIBUTE_SPECIFIER:
    DEBUG_CALL(attribute_specifier, &specifier->attribute_specifier);
    break;
  }

  UNINDENT();
}

DEBUG_FUNC(declaration_specifier_list, specifier_list) {
  AST_PRINTZ("DECLARATION SPECIFIER LIST");
  INDENT();
  for (size_t i = 0; i < specifier_list->num_decl_specifiers; i++) {
    DEBUG_CALL(declaration_specifier, &specifier_list->decl_specifiers[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(type_name, type_name) {
  AST_PRINTZ("TYPE NAME");
  INDENT();
  DEBUG_CALL(declaration_specifier_list, &type_name->specifier_list);
  DEBUG_CALL(abstract_declarator, &type_name->abstract_declarator);
  UNINDENT();
}

DEBUG_FUNC(unary_op, unary_op) {
  switch (unary_op->ty) {
  case AST_UNARY_OP_TY_PREFIX_INC:
    AST_PRINTZ("PREFIX INC");
    break;
  case AST_UNARY_OP_TY_PREFIX_DEC:
    AST_PRINTZ("PREFIX DEC");
    break;
  case AST_UNARY_OP_TY_POSTFIX_INC:
    AST_PRINTZ("POSTFIX INC");
    break;
  case AST_UNARY_OP_TY_POSTFIX_DEC:
    AST_PRINTZ("POSTFIX DEC");
    break;
  case AST_UNARY_OP_TY_PLUS:
    AST_PRINTZ("PLUS");
    break;
  case AST_UNARY_OP_TY_MINUS:
    AST_PRINTZ("MINUS");
    break;
  case AST_UNARY_OP_TY_LOGICAL_NOT:
    AST_PRINTZ("LOGICAL NOT");
    break;
  case AST_UNARY_OP_TY_NOT:
    AST_PRINTZ("NOT");
    break;
  case AST_UNARY_OP_TY_INDIRECTION:
    AST_PRINTZ("INDIRECTION");
    break;
  case AST_UNARY_OP_TY_ADDRESSOF:
    AST_PRINTZ("ADDRESSOF");
    break;
  case AST_UNARY_OP_TY_CAST:
    AST_PRINTZ("CAST");
    INDENT();
    AST_PRINTZ("TO");
    DEBUG_CALL(type_name, &unary_op->cast.type_name);
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
  case AST_BINARY_OP_TY_EQ:
    AST_PRINTZ("EQ");
    break;
  case AST_BINARY_OP_TY_NEQ:
    AST_PRINTZ("NEQ");
    break;
  case AST_BINARY_OP_TY_LT:
    AST_PRINTZ("LT");
    break;
  case AST_BINARY_OP_TY_LTEQ:
    AST_PRINTZ("LTEQ");
    break;
  case AST_BINARY_OP_TY_GT:
    AST_PRINTZ("GT");
    break;
  case AST_BINARY_OP_TY_GTEQ:
    AST_PRINTZ("GTEQ");
    break;
  case AST_BINARY_OP_TY_LSHIFT:
    AST_PRINTZ("LSHIFT");
    break;
  case AST_BINARY_OP_TY_RSHIFT:
    AST_PRINTZ("RSHIFT");
    break;
  case AST_BINARY_OP_TY_OR:
    AST_PRINTZ("OR");
    break;
  case AST_BINARY_OP_TY_XOR:
    AST_PRINTZ("XOR");
    break;
  case AST_BINARY_OP_TY_AND:
    AST_PRINTZ("AND");
    break;
  case AST_BINARY_OP_TY_ADD:
    AST_PRINTZ("ADD");
    break;
  case AST_BINARY_OP_TY_SUB:
    AST_PRINTZ("SUB");
    break;
  case AST_BINARY_OP_TY_MUL:
    AST_PRINTZ("MUL");
    break;
  case AST_BINARY_OP_TY_DIV:
    AST_PRINTZ("DIV");
    break;
  case AST_BINARY_OP_TY_MOD:
    AST_PRINTZ("MOD");
    break;
  case AST_BINARY_OP_TY_LOGICAL_OR:
    AST_PRINTZ("LOGICAL OR");
    break;
  case AST_BINARY_OP_TY_LOGICAL_AND:
    AST_PRINTZ("LOGICAL AND");
    break;
  }

  AST_PRINTZ("LHS: ");
  INDENT();
  DEBUG_CALL(expr, binary_op->lhs);
  UNINDENT();

  AST_PRINTZ("RHS: ");
  INDENT();
  DEBUG_CALL(expr, binary_op->rhs);
  UNINDENT();

  UNINDENT();
}

DEBUG_FUNC(compoundexpr, compound_expr) {
  AST_PRINTZ("COMPOUND EXPRESSION: ");

  INDENT();

  for (size_t i = 0; i < compound_expr->num_exprs; i++) {
    DEBUG_CALL(expr, &compound_expr->exprs[i]);
  }

  UNINDENT();
}

DEBUG_FUNC(assg, assg) {
  AST_PRINTZ("ASSIGNMENT");
  INDENT();
  DEBUG_CALL(expr, assg->assignee);

  INDENT();
  switch (assg->ty) {
  case AST_ASSG_TY_BASIC:
    AST_PRINTZ("=");
    break;
  case AST_ASSG_TY_ADD:
    AST_PRINTZ("+=");
    break;
  case AST_ASSG_TY_SUB:
    AST_PRINTZ("-=");
    break;
  case AST_ASSG_TY_MUL:
    AST_PRINTZ("*=");
    break;
  case AST_ASSG_TY_DIV:
    AST_PRINTZ("/=");
    break;
  case AST_ASSG_TY_MOD:
    AST_PRINTZ("%%=");
    break;
  case AST_ASSG_TY_LSHIFT:
    AST_PRINTZ("<<=");
    break;
  case AST_ASSG_TY_RSHIFT:
    AST_PRINTZ(">>=");
    break;
  case AST_ASSG_TY_AND:
    AST_PRINTZ("&=");
    break;
  case AST_ASSG_TY_OR:
    AST_PRINTZ("|=");
    break;
  case AST_ASSG_TY_XOR:
    AST_PRINTZ("^=");
    break;
  }

  DEBUG_CALL(expr, assg->expr);
  UNINDENT();

  UNINDENT();
}

DEBUG_FUNC(arglist, arg_list) {
  AST_PRINTZ("ARGLIST:");
  INDENT();

  for (size_t i = 0; i < arg_list->num_args; i++) {
    DEBUG_CALL(expr, &arg_list->args[i]);
  }

  UNINDENT();
}

DEBUG_FUNC(call, call) {
  AST_PRINTZ("CALL");
  INDENT();
  DEBUG_CALL(expr, call->target);

  DEBUG_CALL(arglist, &call->arg_list);

  UNINDENT();
}

DEBUG_FUNC(pointeraccess, pointer_access) {
  AST_PRINTZ("POINTER_ACCESS");

  INDENT();
  DEBUG_CALL(expr, pointer_access->lhs);
  UNINDENT();

  AST_PRINTZ("MEMBER");

  INDENT();
  AST_PRINT_IDENTIFIER(&pointer_access->member);
  UNINDENT();
}

DEBUG_FUNC(memberaccess, member_access) {
  AST_PRINTZ("MEMBER_ACCESS");

  INDENT();
  DEBUG_CALL(expr, member_access->lhs);
  UNINDENT();

  AST_PRINTZ("MEMBER");

  INDENT();
  AST_PRINT_IDENTIFIER(&member_access->member);
  UNINDENT();
}

DEBUG_FUNC(arrayaccess, array_access) {
  AST_PRINTZ("ARRAY_ACCESS");

  INDENT();
  DEBUG_CALL(expr, array_access->lhs);
  UNINDENT();

  AST_PRINTZ("OFFSET");

  INDENT();
  DEBUG_CALL(expr, array_access->rhs);
  UNINDENT();
}

DEBUG_FUNC(designator, designator) {
  AST_PRINTZ("DESIGNATOR");

  INDENT();
  switch (designator->ty) {
  case AST_DESIGNATOR_TY_FIELD:
    AST_PRINT_SAMELINE_Z("FIELD ");
    AST_PRINT_IDENTIFIER_SAMELINE(&designator->field);
    break;
  case AST_DESIGNATOR_TY_INDEX:
    AST_PRINTZ("INDEX");
    DEBUG_CALL(expr, designator->index);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(designator_list, designator_list) {
  AST_PRINTZ("DESIGNATOR LIST");
  INDENT();
  for (size_t i = 0; i < designator_list->num_designators; i++) {
    DEBUG_CALL(designator, &designator_list->designators[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(init_list, init_list);

DEBUG_FUNC(init, init) {
  AST_PRINTZ("INIT");
  INDENT();
  switch (init->ty) {
  case AST_INIT_TY_EXPR:
    DEBUG_CALL(expr, &init->expr);
    break;
  case AST_INIT_TY_INIT_LIST:
    DEBUG_CALL(init_list, &init->init_list);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(init_list_init, init) {
  AST_PRINTZ("INIT LIST INIT");

  if (init->designator_list) {
    DEBUG_CALL(designator_list, init->designator_list);
  }

  AST_PRINTZ("init");

  INDENT();
  DEBUG_CALL(init, init->init);
  UNINDENT();
}

DEBUG_FUNC(init_list, init_list) {
  AST_PRINTZ("INIT LIST");

  INDENT();
  for (size_t i = 0; i < init_list->num_inits; i++) {
    DEBUG_CALL(init_list_init, &init_list->inits[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(type_or_expr, type_or_expr) {
  switch (type_or_expr->ty) {
  case AST_TYPE_OR_EXPR_TY_TYPE:
    DEBUG_CALL(type_name, type_or_expr->type_name);
    break;
  case AST_TYPE_OR_EXPR_TY_EXPR:
    DEBUG_CALL(expr, type_or_expr->expr);
    break;
  }
}

DEBUG_FUNC(sizeof, size_of) {
  AST_PRINTZ("SIZEOF");

  INDENT();
  DEBUG_CALL(type_or_expr, &size_of->type_or_expr);
  UNINDENT();
}

DEBUG_FUNC(alignof, align_of) {
  AST_PRINTZ("ALIGNOF");

  INDENT();
  AST_PRINTZ("TYPE");
  DEBUG_CALL(type_name, &align_of->type_name);
  UNINDENT();
}

DEBUG_FUNC(ternary, ternary) {
  AST_PRINTZ("TERNARY");

  INDENT();
  AST_PRINTZ("COND");
  DEBUG_CALL(expr, ternary->cond);
  UNINDENT();

  INDENT();
  AST_PRINTZ("TRUE");
  DEBUG_CALL(expr, ternary->true_expr);
  UNINDENT();

  INDENT();
  AST_PRINTZ("FALSE");
  DEBUG_CALL(expr, ternary->false_expr);
  UNINDENT();
}

DEBUG_FUNC(compound_literal, compound_literal) {
  AST_PRINTZ("COMPOUND LITERAL");
  INDENT();
  DEBUG_CALL(type_name, &compound_literal->type_name);
  DEBUG_CALL(init_list, &compound_literal->init_list);
  UNINDENT();
}

DEBUG_FUNC(generic_association, generic_association) {
  switch (generic_association->ty) {
  case AST_GENERIC_ASSOCIATION_TY_TYPE_NAME:
    AST_PRINTZ("TYPE NAME");
    INDENT();
    DEBUG_CALL(type_name, &generic_association->type_name);
    UNINDENT();
    break;
  case AST_GENERIC_ASSOCIATION_TY_DEFAULT:
    AST_PRINTZ("DEFAULT");
    break;
  }

  AST_PRINTZ("EXPR");
  INDENT();
  DEBUG_CALL(expr, &generic_association->expr);
  UNINDENT();
}

DEBUG_FUNC(generic, generic) {
  AST_PRINTZ("GENERIC");
  INDENT();

  AST_PRINTZ("CTRL");
  INDENT();
  DEBUG_CALL(expr, generic->ctrl_expr);
  UNINDENT();

  AST_PRINTZ("ASSOCIATION LIST");
  INDENT();
  for (size_t i = 0; i < generic->num_associations; i++) {
    DEBUG_CALL(generic_association, &generic->associations[i]);
  }
  UNINDENT();

  UNINDENT();
}

DEBUG_FUNC(expr, expr) {
  AST_PRINTZ("EXPRESSION");

  INDENT();
  switch (expr->ty) {
  case AST_EXPR_TY_INVALID:
    AST_PRINTZ("<INVALID>");
    break;
  case AST_EXPR_TY_GENERIC:
    DEBUG_CALL(generic, &expr->generic);
    break;
  case AST_EXPR_TY_TERNARY:
    DEBUG_CALL(ternary, &expr->ternary);
    break;
  case AST_EXPR_TY_VAR:
    DEBUG_CALL(var, &expr->var);
    break;
  case AST_EXPR_TY_CNST:
    DEBUG_CALL(cnst, &expr->cnst);
    break;
  case AST_EXPR_TY_COMPOUNDEXPR:
    DEBUG_CALL(compoundexpr, &expr->compound_expr);
    break;
  case AST_EXPR_TY_CALL:
    DEBUG_CALL(call, &expr->call);
    break;
  case AST_EXPR_TY_UNARY_OP:
    DEBUG_CALL(unary_op, &expr->unary_op);
    break;
  case AST_EXPR_TY_BINARY_OP:
    DEBUG_CALL(binary_op, &expr->binary_op);
    break;
  case AST_EXPR_TY_ARRAYACCESS:
    DEBUG_CALL(arrayaccess, &expr->array_access);
    break;
  case AST_EXPR_TY_MEMBERACCESS:
    DEBUG_CALL(memberaccess, &expr->member_access);
    break;
  case AST_EXPR_TY_POINTERACCESS:
    DEBUG_CALL(pointeraccess, &expr->pointer_access);
    break;
  case AST_EXPR_TY_COMPOUND_LITERAL:
    DEBUG_CALL(compound_literal, &expr->compound_literal);
    break;
  case AST_EXPR_TY_ASSG:
    DEBUG_CALL(assg, &expr->assg);
    break;
  case AST_EXPR_TY_SIZEOF:
    DEBUG_CALL(sizeof, &expr->size_of);
    break;
  case AST_EXPR_TY_ALIGNOF:
    DEBUG_CALL(alignof, &expr->align_of);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(jumpstmt, jump_stmt) {
  switch (jump_stmt->ty) {
  case AST_JUMPSTMT_TY_RETURN:
    AST_PRINTZ("RETURN");
    INDENT();
    if (jump_stmt->return_stmt.expr) {
      DEBUG_CALL(expr, jump_stmt->return_stmt.expr);
    }
    UNINDENT();
    break;
  case AST_JUMPSTMT_TY_GOTO:
    AST_PRINT_SAMELINE_Z("GOTO ");
    AST_PRINT_IDENTIFIER_SAMELINE(&jump_stmt->goto_stmt.label);
    break;
  case AST_JUMPSTMT_TY_BREAK:
    AST_PRINTZ("BREAK");
    break;
  case AST_JUMPSTMT_TY_CONTINUE:
    AST_PRINTZ("CONTINUE");
    break;
  }
}

DEBUG_FUNC(stmt, stmt);

DEBUG_FUNC(switchstmt, switch_stmt) {
  AST_PRINTZ("SWITCH");
  AST_PRINTZ("CONTROL EXPRESSION");
  INDENT();
  DEBUG_CALL(expr, &switch_stmt->ctrl_expr);
  UNINDENT();

  AST_PRINTZ("BODY");
  DEBUG_CALL(stmt, switch_stmt->body);
}

DEBUG_FUNC(stmt, if_stmt);

DEBUG_FUNC(ifstmt, if_stmt) {
  AST_PRINTZ("IF");
  AST_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &if_stmt->cond);
  UNINDENT();

  AST_PRINTZ("BODY");
  DEBUG_CALL(stmt, if_stmt->body);
}

DEBUG_FUNC(ifelsestmt, if_else_stmt) {
  AST_PRINTZ("IF");
  AST_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &if_else_stmt->cond);
  UNINDENT();

  AST_PRINTZ("BODY");
  DEBUG_CALL(stmt, if_else_stmt->body);

  AST_PRINTZ("ELSE");
  DEBUG_CALL(stmt, if_else_stmt->else_body);
}

DEBUG_FUNC(selectstmt, select_stmt) {
  switch (select_stmt->ty) {
  case AST_SELECTSTMT_TY_IF:
    DEBUG_CALL(ifstmt, &select_stmt->if_stmt);
    break;
  case AST_SELECTSTMT_TY_IF_ELSE:
    DEBUG_CALL(ifelsestmt, &select_stmt->if_else_stmt);
    break;
  case AST_SELECTSTMT_TY_SWITCH:
    DEBUG_CALL(switchstmt, &select_stmt->switch_stmt);
    break;
  }
}

DEBUG_FUNC(whilestmt, while_stmt) {
  AST_PRINTZ("WHILE");
  AST_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &while_stmt->cond);
  UNINDENT();

  AST_PRINTZ("BODY");
  DEBUG_CALL(stmt, while_stmt->body);
}

DEBUG_FUNC(dowhilestmt, do_while_stmt) {
  AST_PRINTZ("DO");
  AST_PRINTZ("BODY");
  DEBUG_CALL(stmt, do_while_stmt->body);

  AST_PRINTZ("WHILE");
  AST_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &do_while_stmt->cond);
  UNINDENT();
}

DEBUG_FUNC(init_declarator, init_declarator) {
  DEBUG_CALL(declarator, &init_declarator->declarator);

  if (init_declarator->init) {
    DEBUG_CALL(init, init_declarator->init);
  }
}

DEBUG_FUNC(init_declarator_list, init_declarator_list) {
  AST_PRINTZ("INIT DECLARATOR LIST");
  INDENT();
  for (size_t i = 0; i < init_declarator_list->num_init_declarators; i++) {
    DEBUG_CALL(init_declarator, &init_declarator_list->init_declarators[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(declaration, declaration) {
  AST_PRINTZ("DECLARATION");
  INDENT();
  DEBUG_CALL(declaration_specifier_list, &declaration->specifier_list);
  DEBUG_CALL(init_declarator_list, &declaration->declarator_list);
  UNINDENT();
}

DEBUG_FUNC(declaration_or_expr, decl_or_expr) {
  switch (decl_or_expr->ty) {

  case AST_DECLARATION_OR_EXPR_TY_DECL:
    DEBUG_CALL(declaration, &decl_or_expr->decl);
    break;
  case AST_DECLARATION_OR_EXPR_TY_EXPR:
    DEBUG_CALL(expr, &decl_or_expr->expr);
    break;
  }
}

DEBUG_FUNC(forstmt, for_stmt) {
  AST_PRINTZ("FOR");
  AST_PRINTZ("INIT");
  INDENT();
  if (for_stmt->init) {
    DEBUG_CALL(declaration_or_expr, for_stmt->init);
  }
  UNINDENT();
  AST_PRINTZ("COND");
  INDENT();
  if (for_stmt->cond) {
    DEBUG_CALL(expr, for_stmt->cond);
  }
  UNINDENT();
  AST_PRINTZ("ITER");
  INDENT();
  if (for_stmt->iter) {
    DEBUG_CALL(expr, for_stmt->iter);
  }
  UNINDENT();

  AST_PRINTZ("BODY");
  DEBUG_CALL(stmt, for_stmt->body);
}

DEBUG_FUNC(iterstmt, iter_stmt) {
  switch (iter_stmt->ty) {
  case AST_ITERSTMT_TY_WHILE:
    DEBUG_CALL(whilestmt, &iter_stmt->while_stmt);
    break;
  case AST_ITERSTMT_TY_DO_WHILE:
    DEBUG_CALL(dowhilestmt, &iter_stmt->do_while_stmt);
    break;
  case AST_ITERSTMT_TY_FOR:
    DEBUG_CALL(forstmt, &iter_stmt->for_stmt);
    break;
  }
}

DEBUG_FUNC(labeledstmt, labeled_stmt) {
  switch (labeled_stmt->ty) {
  case AST_LABELEDSTMT_TY_LABEL:
    AST_PRINT_SAMELINE_Z("LABEL ");
    AST_PRINT_IDENTIFIER_SAMELINE(&labeled_stmt->label);
    break;
  case AST_LABELEDSTMT_TY_CASE:
    AST_PRINTZ("CASE");
    DEBUG_CALL(expr, &labeled_stmt->cnst);
    break;
  case AST_LABELEDSTMT_TY_DEFAULT:
    AST_PRINTZ("DEFAULT");
    break;
  }
  AST_PRINTZ("STATEMENT");
  INDENT();
  DEBUG_CALL(stmt, labeled_stmt->stmt);
  UNINDENT();
}

DEBUG_FUNC(staticassert, staticassert) {
  AST_PRINTZ("STATIC_ASSERT");
  INDENT();

  AST_PRINTZ("COND");
  INDENT();
  DEBUG_CALL(expr, &staticassert->cond);
  UNINDENT();

  if (staticassert->message) {
    AST_PRINTZ("MESSAGE");
    INDENT();
    DEBUG_CALL(expr, staticassert->message);
    UNINDENT();
  }
}

DEBUG_FUNC(stmt, stmt) {
  INDENT();

  switch (stmt->ty) {
  case AST_STMT_TY_NULL:
    break;
  case AST_STMT_TY_STATICASSERT:
    DEBUG_CALL(staticassert, &stmt->staticassert);
    break;
  case AST_STMT_TY_DECLARATION:
    DEBUG_CALL(declaration, &stmt->declaration);
    break;
  case AST_STMT_TY_EXPR:
    DEBUG_CALL(expr, &stmt->expr);
    break;
  case AST_STMT_TY_COMPOUND:
    DEBUG_CALL(compoundstmt, &stmt->compound);
    break;
  case AST_STMT_TY_JUMP:
    DEBUG_CALL(jumpstmt, &stmt->jump);
    break;
  case AST_STMT_TY_SELECT:
    DEBUG_CALL(selectstmt, &stmt->select);
    break;
  case AST_STMT_TY_ITER:
    DEBUG_CALL(iterstmt, &stmt->iter);
    break;
  case AST_STMT_TY_LABELED:
    DEBUG_CALL(labeledstmt, &stmt->labeled);
    break;
  }

  UNINDENT();
}

DEBUG_FUNC(compoundstmt, compound_stmt) {
  AST_PRINTZ("COMPOUND STATEMENT: ");
  INDENT();

  for (size_t i = 0; i < compound_stmt->num_stmts; i++) {
    DEBUG_CALL(stmt, &compound_stmt->stmts[i]);
  }

  UNINDENT();
}

DEBUG_FUNC(param, param) {
  AST_PRINTZ("PARAM");
  INDENT();
  switch (param->ty) {
  case AST_PARAM_TY_VARIADIC:
    AST_PRINTZ("VARIADIC");
    break;
  case AST_PARAM_TY_VOID:
    AST_PRINTZ("VOID");
    break;
  case AST_PARAM_TY_DECL:
    DEBUG_CALL(declaration_specifier_list, &param->specifier_list);
    DEBUG_CALL(declarator, &param->declarator);
    break;
  case AST_PARAM_TY_ABSTRACT_DECL:
    DEBUG_CALL(declaration_specifier_list, &param->specifier_list);
    DEBUG_CALL(abstract_declarator, &param->abstract_declarator);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(paramlist, param_list) {
  for (size_t i = 0; i < param_list->num_params; i++) {
    DEBUG_CALL(param, &param_list->params[i]);
  }
}

DEBUG_FUNC(funcdef, func_def) {
  AST_PRINTZ("FUNCTION SIGNATURE ");
  INDENT();
  DEBUG_CALL(declaration_specifier_list, &func_def->specifier_list);
  DEBUG_CALL(declarator, &func_def->declarator);
  DEBUG_CALL(declaration_list, &func_def->declaration_list);
  UNINDENT();

  AST_PRINTZ("FUNCTION DEFINITION ");

  INDENT();
  DEBUG_CALL(compoundstmt, &func_def->body);
  UNINDENT();
}

DEBUG_FUNC(external_declaration, external_declaration) {
  switch (external_declaration->ty) {
  case AST_EXTERNAL_DECLARATION_TY_DECLARATION:
    DEBUG_CALL(declaration, &external_declaration->declaration);
    break;
  case AST_EXTERNAL_DECLARATION_TY_FUNC_DEF:
    DEBUG_CALL(funcdef, &external_declaration->func_def);
    break;
  case AST_EXTERNAL_DECLARATION_TY_STATIC_ASSERT:
    DEBUG_CALL(staticassert, &external_declaration->staticassert);
    break;
  }
}

void debug_print_ast(struct parser *parser,
                     struct ast_translationunit *translation_unit) {
  // logic for respecting --log-syms is hard as we have not parsed all the way
  // to identifiers

  struct ast_printstate state_ = {.indent = 0, .parser = parser};

  struct ast_printstate *state = &state_;

  AST_PRINTZ("PRINTING AST");

  for (size_t i = 0; i < translation_unit->num_external_declarations; i++) {
    struct ast_external_declaration *decl =
        &translation_unit->external_declarations[i];

    DEBUG_CALL(external_declaration, decl);
  }

  AST_PRINTZ("");
}
