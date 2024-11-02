#include "parse.h"

#include "alloc.h"
#include "bit_twiddle.h"
#include "ir/ir.h"
#include "ir/var_refs.h"
#include "lex.h"
#include "log.h"
#include "program.h"
#include "util.h"
#include "var_table.h"
#include "vector.h"

#include <alloca.h>
#include <ctype.h>
#include <string.h>

struct parser {
  struct arena_allocator *arena;
  struct lexer *lexer;

  // `value` contains a `struct ast_tyref *` to the type of the variable
  // or NULL if the variable has been used without a declaration
  struct var_table var_table;

  // types (e.g declared structs)
  struct var_table ty_table;

  // returns need to know which type they coerce to
  struct ast_tyref func_ret_ty;
};

enum parser_create_result parser_create(struct preprocessed_program *program,
                                        struct parser **parser) {
  struct parser *p = nonnull_malloc(sizeof(*p));

  arena_allocator_create(&p->arena);
  if (lexer_create(program, &p->lexer) != LEX_CREATE_RESULT_SUCCESS) {
    err("failed to create lexer");
    return PARSER_CREATE_RESULT_FAILURE;
  }

  p->var_table = var_table_create(p->arena);
  p->ty_table = var_table_create(p->arena);

  *parser = p;

  return PARSER_CREATE_RESULT_SUCCESS;
}

void parser_free(struct parser **parser) {
  lexer_free(&(*parser)->lexer);

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

struct ast_op_info op_info(enum ast_binary_op_ty ty) {
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
  case AST_BINARY_OP_TY_QUOT:
    info.precedence = 10;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  default:
    unreachable("invalid `ast_binary_op_ty`");
  }

  return info;
}

bool op_info_for_token(const struct token *token, struct ast_op_info *info) {
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
  case LEX_TOKEN_TY_OP_QUOT:
    *info = op_info(AST_BINARY_OP_TY_QUOT);
    return true;
  default:
    // not an op
    return false;
  }
}

bool is_fp_ty(const struct ast_tyref *ty) {
  if (ty->ty != AST_TYREF_TY_WELL_KNOWN) {
    return false;
  }

  switch (ty->well_known) {
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

  case WELL_KNOWN_TY_FLOAT:
  case WELL_KNOWN_TY_DOUBLE:
  case WELL_KNOWN_TY_LONG_DOUBLE:
    return false;
  }
}

// FIXME: type pooling so we don't end up with lots of duplicate types
bool is_literal_token(struct parser *parser, enum lex_token_ty tok_ty,
                      struct ast_tyref *ty_ref) {
  switch (tok_ty) {
  case LEX_TOKEN_TY_UNKNOWN:
  case LEX_TOKEN_TY_EOF:
  case LEX_TOKEN_TY_WHITESPACE:
  case LEX_TOKEN_TY_INLINE_COMMENT:
  case LEX_TOKEN_TY_MULTILINE_COMMENT:
  case LEX_TOKEN_TY_OPEN_BRACKET:
  case LEX_TOKEN_TY_CLOSE_BRACKET:
  case LEX_TOKEN_TY_OPEN_SQUARE_BRACKET:
  case LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET:
  case LEX_TOKEN_TY_OPEN_BRACE:
  case LEX_TOKEN_TY_CLOSE_BRACE:
  case LEX_TOKEN_TY_COLON:
  case LEX_TOKEN_TY_QMARK:
  case LEX_TOKEN_TY_SEMICOLON:
  case LEX_TOKEN_TY_COMMA:
  case LEX_TOKEN_TY_DOT:
  case LEX_TOKEN_TY_ARROW:
  case LEX_TOKEN_TY_OP_LOGICAL_NOT:
  case LEX_TOKEN_TY_OP_NOT:
  case LEX_TOKEN_TY_OP_ASSG:
  case LEX_TOKEN_TY_OP_INC:
  case LEX_TOKEN_TY_OP_DEC:
  case LEX_TOKEN_TY_OP_RSHIFT:
  case LEX_TOKEN_TY_OP_RSHIFT_ASSG:
  case LEX_TOKEN_TY_OP_LSHIFT:
  case LEX_TOKEN_TY_OP_LSHIFT_ASSG:
  case LEX_TOKEN_TY_OP_LOGICAL_AND:
  case LEX_TOKEN_TY_OP_AND:
  case LEX_TOKEN_TY_OP_AND_ASSG:
  case LEX_TOKEN_TY_OP_LOGICAL_OR:
  case LEX_TOKEN_TY_OP_OR:
  case LEX_TOKEN_TY_OP_OR_ASSG:
  case LEX_TOKEN_TY_OP_XOR:
  case LEX_TOKEN_TY_OP_XOR_ASSG:
  case LEX_TOKEN_TY_OP_ADD:
  case LEX_TOKEN_TY_OP_ADD_ASSG:
  case LEX_TOKEN_TY_OP_SUB:
  case LEX_TOKEN_TY_OP_SUB_ASSG:
  case LEX_TOKEN_TY_OP_MUL:
  case LEX_TOKEN_TY_OP_MUL_ASSG:
  case LEX_TOKEN_TY_OP_DIV:
  case LEX_TOKEN_TY_OP_DIV_ASSG:
  case LEX_TOKEN_TY_OP_QUOT:
  case LEX_TOKEN_TY_OP_QUOT_ASSG:
  case LEX_TOKEN_TY_OP_EQ:
  case LEX_TOKEN_TY_OP_NEQ:
  case LEX_TOKEN_TY_OP_LT:
  case LEX_TOKEN_TY_OP_LTEQ:
  case LEX_TOKEN_TY_OP_GT:
  case LEX_TOKEN_TY_OP_GTEQ:
  case LEX_TOKEN_TY_ELLIPSIS:
  case LEX_TOKEN_TY_KW_GOTO:
  case LEX_TOKEN_TY_KW_BREAK:
  case LEX_TOKEN_TY_KW_CONTINUE:
  case LEX_TOKEN_TY_KW_DO:
  case LEX_TOKEN_TY_KW_FOR:
  case LEX_TOKEN_TY_KW_SWITCH:
  case LEX_TOKEN_TY_KW_WHILE:
  case LEX_TOKEN_TY_KW_DEFAULT:
  case LEX_TOKEN_TY_KW_CASE:
  case LEX_TOKEN_TY_KW_IF:
  case LEX_TOKEN_TY_KW_ELSE:
  case LEX_TOKEN_TY_KW_TYPEDEF:
  case LEX_TOKEN_TY_KW_STATIC:
  case LEX_TOKEN_TY_KW_EXTERN:
  case LEX_TOKEN_TY_KW_AUTO:
  case LEX_TOKEN_TY_KW_REGISTER:
  case LEX_TOKEN_TY_KW_INLINE:
  case LEX_TOKEN_TY_KW_CONST:
  case LEX_TOKEN_TY_KW_VOLATILE:
  case LEX_TOKEN_TY_KW_VOID:
  case LEX_TOKEN_TY_KW_FLOAT:
  case LEX_TOKEN_TY_KW_DOUBLE:
  case LEX_TOKEN_TY_KW_CHAR:
  case LEX_TOKEN_TY_KW_SHORT:
  case LEX_TOKEN_TY_KW_INT:
  case LEX_TOKEN_TY_KW_LONG:
  case LEX_TOKEN_TY_KW_UNSIGNED:
  case LEX_TOKEN_TY_KW_SIGNED:
  case LEX_TOKEN_TY_KW_RETURN:
  case LEX_TOKEN_TY_KW_ENUM:
  case LEX_TOKEN_TY_KW_STRUCT:
  case LEX_TOKEN_TY_KW_UNION:
  case LEX_TOKEN_TY_KW_SIZEOF:
  case LEX_TOKEN_TY_KW_ALIGNOF:
  case LEX_TOKEN_TY_KW_ALIGNAS:
  case LEX_TOKEN_TY_IDENTIFIER:
    return false;

  case LEX_TOKEN_TY_ASCII_STR_LITERAL:
    // char is signed!
    ty_ref->ty = AST_TYREF_TY_POINTER;
    ty_ref->pointer.underlying =
        arena_alloc(parser->arena, sizeof(*ty_ref->pointer.underlying));
    *ty_ref->pointer.underlying = (struct ast_tyref){
        .ty = AST_TYREF_TY_WELL_KNOWN, .well_known = WELL_KNOWN_TY_SIGNED_CHAR};
    return true;

  case LEX_TOKEN_TY_ASCII_CHAR_LITERAL:
    // char is signed!
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_SIGNED_CHAR;
    return true;

  case LEX_TOKEN_TY_FLOAT_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_FLOAT;
    return true;

  case LEX_TOKEN_TY_DOUBLE_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_DOUBLE;
    return true;

  case LEX_TOKEN_TY_LONG_DOUBLE_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_LONG_DOUBLE;
    return true;

  case LEX_TOKEN_TY_SIGNED_INT_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_SIGNED_INT;
    return true;

  case LEX_TOKEN_TY_UNSIGNED_INT_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_UNSIGNED_INT;
    return true;

  case LEX_TOKEN_TY_SIGNED_LONG_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_SIGNED_LONG;
    return true;

  case LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_UNSIGNED_LONG;
    return true;

  case LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_SIGNED_LONG_LONG;
    return true;

  case LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL:
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_UNSIGNED_LONG_LONG;
    return true;
  }

  unreachable("switch broke");
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

bool ast_binary_op_is_comparison(enum ast_binary_op_ty ty) {
  switch (ty) {
  case AST_BINARY_OP_TY_EQ:
  case AST_BINARY_OP_TY_NEQ:
  case AST_BINARY_OP_TY_GT:
  case AST_BINARY_OP_TY_GTEQ:
  case AST_BINARY_OP_TY_LT:
  case AST_BINARY_OP_TY_LTEQ:
  case AST_BINARY_OP_TY_LOGICAL_OR:
  case AST_BINARY_OP_TY_LOGICAL_AND:
    return true;
  case AST_BINARY_OP_TY_OR:
  case AST_BINARY_OP_TY_AND:
  case AST_BINARY_OP_TY_XOR:
  case AST_BINARY_OP_TY_LSHIFT:
  case AST_BINARY_OP_TY_RSHIFT:
  case AST_BINARY_OP_TY_ADD:
  case AST_BINARY_OP_TY_SUB:
  case AST_BINARY_OP_TY_MUL:
  case AST_BINARY_OP_TY_DIV:
  case AST_BINARY_OP_TY_QUOT:
    return false;
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

void parser_push_scope(struct parser *parser) {
  push_scope(&parser->var_table);
  push_scope(&parser->ty_table);
}

void parser_pop_scope(struct parser *parser) {
  pop_scope(&parser->var_table);
  pop_scope(&parser->ty_table);
}

bool parse_token(struct parser *parser, enum lex_token_ty ty) {
  struct token token;

  peek_token(parser->lexer, &token);
  if (token.ty == ty) {
    consume_token(parser->lexer, token);
    return true;
  }

  return false;
}

bool parse_identifier(struct parser *parser, struct token *token) {
  struct text_pos pos = get_position(parser->lexer);

  peek_token(parser->lexer, token);

  if (token->ty == LEX_TOKEN_TY_IDENTIFIER) {
    consume_token(parser->lexer, *token);

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}
// used when parsing type names, as `short int` is equivalent to `short`
void skip_int_token_if_present(struct parser *parser) {
  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TY_KW_INT) {
    consume_token(parser->lexer, token);
  }
}

bool parse_wkt_item(struct parser *parser, enum well_known_ty *wkt) {
  struct token token;
  peek_token(parser->lexer, &token);

  switch (token.ty) {
  case LEX_TOKEN_TY_KW_FLOAT:
    consume_token(parser->lexer, token);
    *wkt = WELL_KNOWN_TY_FLOAT;
    return true;
  case LEX_TOKEN_TY_KW_DOUBLE:
    consume_token(parser->lexer, token);
    *wkt = WELL_KNOWN_TY_DOUBLE;
    return true;
  case LEX_TOKEN_TY_KW_CHAR:
    consume_token(parser->lexer, token);
    *wkt = WELL_KNOWN_TY_SIGNED_CHAR;
    return true;
  case LEX_TOKEN_TY_KW_SHORT:
    consume_token(parser->lexer, token);
    *wkt = WELL_KNOWN_TY_SIGNED_SHORT;
    skip_int_token_if_present(parser);
    return true;
  case LEX_TOKEN_TY_KW_INT:
    consume_token(parser->lexer, token);
    *wkt = WELL_KNOWN_TY_SIGNED_INT;
    return true;
  case LEX_TOKEN_TY_KW_LONG: {
    consume_token(parser->lexer, token);

    struct token maybe_other_long;
    peek_token(parser->lexer, &maybe_other_long);

    if (maybe_other_long.ty == LEX_TOKEN_TY_KW_DOUBLE) {
      consume_token(parser->lexer, token);
      *wkt = WELL_KNOWN_TY_LONG_DOUBLE;

      // return (ignore `skip_int_token`)
      return true;
    } else if (maybe_other_long.ty == LEX_TOKEN_TY_KW_LONG) {
      *wkt = WELL_KNOWN_TY_SIGNED_LONG_LONG;
      consume_token(parser->lexer, maybe_other_long);
    } else {
      *wkt = WELL_KNOWN_TY_SIGNED_LONG;
    }

    skip_int_token_if_present(parser);
    return true;
  }
  default:
    return false;
  }
}

bool parse_type_qualifiers(struct parser *parser,
                           enum ast_type_qualifier_flags *flags) {
  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TY_KW_CONST) {
    *flags |= AST_TYPE_QUALIFIER_FLAG_CONST;
  } else if (token.ty == LEX_TOKEN_TY_KW_VOLATILE) {
    *flags |= AST_TYPE_QUALIFIER_FLAG_VOLATILE;
  } else {
    return false;
  }

  consume_token(parser->lexer, token);
  return true;
}

bool parse_storage_class_specifiers(
    struct parser *parser, enum ast_storage_class_specifier_flags *flags) {
  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TY_KW_TYPEDEF) {
    *flags |= AST_STORAGE_CLASS_SPECIFIER_FLAG_TYPEDEF;
  } else if (token.ty == LEX_TOKEN_TY_KW_AUTO) {
    *flags |= AST_STORAGE_CLASS_SPECIFIER_FLAG_EXTERN;
  } else if (token.ty == LEX_TOKEN_TY_KW_EXTERN) {
    *flags |= AST_STORAGE_CLASS_SPECIFIER_FLAG_STATIC;
  } else if (token.ty == LEX_TOKEN_TY_KW_STATIC) {
    *flags |= AST_STORAGE_CLASS_SPECIFIER_FLAG_AUTO;
  } else if (token.ty == LEX_TOKEN_TY_KW_AUTO) {
    *flags |= AST_STORAGE_CLASS_SPECIFIER_FLAG_AUTO;
  } else if (token.ty == LEX_TOKEN_TY_KW_REGISTER) {
    *flags |= AST_STORAGE_CLASS_SPECIFIER_FLAG_REGISTER;
  } else {
    return false;
  }

  consume_token(parser->lexer, token);
  return true;
}

bool parse_function_specifiers(struct parser *parser,
                               enum ast_function_specifier_flags *flags) {
  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TY_KW_INLINE) {
    *flags |= AST_FUNCTION_SPECIFIER_FLAG_INLINE;
  } else {
    return false;
  }

  consume_token(parser->lexer, token);
  return true;
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

unsigned long long resolve_constant_expr(struct parser *parser,
                                         const struct ast_expr *expr) {
  UNUSED_ARG(parser);

  if (expr->ty == AST_EXPR_TY_CNST && is_integral_ty(&expr->cnst.cnst_ty)) {
    return expr->cnst.int_value;
  }

  todo("non cnst constant expressions");
}

bool parse_constant_expr(struct parser *parser, struct ast_expr *expr);

bool parse_typedef(struct parser *parser, struct ast_typedef *type_def) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_TYPEDEF)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  UNUSED_ARG(type_def);
  todo(__func__);

  return true;
}

bool parse_pointer(struct parser *parser, struct ast_tyref *ty_ref) {
  if (!parse_token(parser, LEX_TOKEN_TY_OP_MUL)) {
    return false;
  }

  struct ast_tyref *underlying =
      arena_alloc(parser->arena, sizeof(*underlying));
  *underlying = *ty_ref;

  ty_ref->ty = AST_TYREF_TY_POINTER;
  ty_ref->pointer.underlying = underlying;
  ty_ref->type_qualifiers = AST_TYPE_QUALIFIER_FLAG_NONE;
  ty_ref->function_specifiers = AST_FUNCTION_SPECIFIER_FLAG_NONE;

  while (parse_type_qualifiers(parser, &ty_ref->type_qualifiers)) {
    ;
  }

  return true;
}

bool parse_structdecllist(struct parser *parser,
                          struct ast_structdecllist *type_def);

bool parse_enumdecllist(struct parser *parser,
                        struct ast_enumdecllist *type_def);

bool parse_paramlist(struct parser *parser, struct ast_paramlist *param_list);

bool parse_type_specifier(
    struct parser *parser, struct ast_tyref *ty_ref,
    enum ast_storage_class_specifier_flags *storage_class_specifiers);
bool parse_declarator(struct parser *parser, struct token *identifier,
                      struct ast_tyref *ty_ref);

bool parse_abstract_declarator(struct parser *parser, struct ast_tyref *ty_ref);

// does not have id, used in casts/params/return types
bool parse_abstract_declaration(struct parser *parser,
                                struct ast_tyref *ty_ref) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_type_specifier(parser, ty_ref, NULL)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_abstract_declarator(parser, ty_ref)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

bool parse_var(struct parser *parser, struct ast_var *var) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty != LEX_TOKEN_TY_IDENTIFIER) {
    backtrack(parser->lexer, pos);
    return false;
  }

  var->ty = AST_VAR_TY_VAR;
  var->var_ty.ty = AST_TYREF_TY_UNKNOWN;
  var->identifier = token;
  var->scope = SCOPE_GLOBAL;

  struct var_table_entry *entry =
      get_entry(&parser->var_table, identifier_str(parser, &var->identifier));

  if (entry && entry->value) {
    if (entry->ty == VAR_TABLE_ENTRY_TY_ENUM_CNST) {
      var->ty = AST_VAR_TY_ENUM_CNST;
      var->enum_cnst = entry->enum_cnst;
    }

    var->var_ty = *entry->value;
    var->scope = entry->scope;
  }

  consume_token(parser->lexer, token);

  return true;
}

bool parse_float_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  peek_token(parser->lexer, &token);
  struct ast_tyref ty_ref;
  if (is_literal_token(parser, token.ty, &ty_ref) && is_fp_ty(&ty_ref)) {
    const char *literal_text = associated_text(parser->lexer, &token);
    size_t literal_len = strlen(literal_text);

    debug_assert(literal_len, "literal_len was 0");

    char *end_ptr;
    long double float_value = strtold(literal_text, &end_ptr);

    size_t literal_end = literal_len;
    do {
      literal_end--;
    } while (literal_len && (tolower(literal_text[literal_end]) == 'f' ||
                             tolower(literal_text[literal_end]) == 'l'));

    if (end_ptr - 1 != &literal_text[literal_end]) {
      todo("handle constant float parse failure");
    }

    // TODO: handle unrepresentedly large values
    cnst->cnst_ty = ty_ref;
    cnst->flt_value = float_value;

    consume_token(parser->lexer, token);
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_int_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  peek_token(parser->lexer, &token);
  struct ast_tyref ty_ref;
  if (is_literal_token(parser, token.ty, &ty_ref) && is_integral_ty(&ty_ref)) {
    const char *literal_text = associated_text(parser->lexer, &token);
    size_t literal_len = strlen(literal_text);

    debug_assert(literal_len, "literal_len was 0");

    unsigned long long int_value;
    if (token.ty == LEX_TOKEN_TY_ASCII_CHAR_LITERAL) {
      if (literal_len == 3) {
        // simple
        int_value = literal_text[1];
      } else {
        todo("other char literals");
      }
    } else {
      int base = 10;
      if (literal_len >= 2 && literal_text[0] == '0' &&
          literal_text[1] == 'x') {
        base = 16;
      } else if (literal_text[0] == '0') {
        // this classes '0' as octal but that is fine
        base = 8;
      }

      char *end_ptr;
      int_value = strtoull(literal_text, &end_ptr, base);

      size_t literal_end = literal_len;
      do {
        literal_end--;
      } while (literal_len && (tolower(literal_text[literal_end]) == 'u' ||
                               tolower(literal_text[literal_end]) == 'l'));

      if (end_ptr - 1 != &literal_text[literal_end]) {
        todo("handle constant int parse failure");
      }
    }

    // TODO: handle unrepresentedly large values
    cnst->cnst_ty = ty_ref;
    cnst->int_value = int_value;

    consume_token(parser->lexer, token);
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_str_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  struct vector *strings = vector_create(sizeof(char));

  peek_token(parser->lexer, &token);
  struct ast_tyref ty_ref;
  while (is_literal_token(parser, token.ty, &ty_ref) &&
         token.ty == LEX_TOKEN_TY_ASCII_STR_LITERAL) {
    cnst->cnst_ty = ty_ref;

    const char *str = associated_text(parser->lexer, &token);
    vector_extend(strings, str, strlen(str));

    consume_token(parser->lexer, token);
    peek_token(parser->lexer, &token);
  }

  if (vector_empty(strings)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  char null = 0;
  vector_push_back(strings, &null);
  cnst->str_value = arena_alloc(parser->arena, vector_byte_size(strings));
  vector_copy_to(strings, cnst->str_value);
  return true;
}

bool parse_cnst(struct parser *parser, struct ast_cnst *cnst) {
  return parse_str_cnst(parser, cnst) || parse_int_cnst(parser, cnst) ||
         parse_float_cnst(parser, cnst);
}

bool parse_expr(struct parser *parser, struct ast_expr *expr);
bool parse_atom_0(struct parser *parser, struct ast_expr *expr);
bool parse_atom_1(struct parser *parser, struct ast_expr *expr);
bool parse_atom_2(struct parser *parser, struct ast_expr *expr);
bool parse_atom_3(struct parser *parser, struct ast_expr *expr);

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

bool parse_assg(struct parser *parser, struct ast_assg *assg) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr assignee;
  struct ast_expr expr;
  if (!parse_atom_3(parser, &assignee)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  enum lex_token_ty token_ty = LEX_TOKEN_TY_UNKNOWN;
  enum ast_assg_ty assg_ty;
  enum ast_binary_op_ty binary_op_ty;

  for (size_t i = 0; i < ARR_LENGTH(ASSG_TOKENS); i++) {
    struct assg_ty_map map = ASSG_TOKENS[i];
    if (parse_token(parser, map.token_ty)) {
      token_ty = map.token_ty;
      assg_ty = map.assg_ty;
      binary_op_ty = map.binary_op_ty;
      break;
    }
  }

  if (token_ty == LEX_TOKEN_TY_UNKNOWN || !parse_expr(parser, &expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  assg->ty = assg_ty;
  if (assg->ty == AST_ASSG_TY_COMPOUNDASSG) {
    assg->compound_assg = (struct ast_assg_compound_assg){
        .binary_op_ty = binary_op_ty,
        .intermediate_var_ty = resolve_binary_op_intermediate_types(
            parser, binary_op_ty, &assignee.var_ty, &expr.var_ty)};
  }

  assg->var_ty = assignee.var_ty;
  assg->assignee = arena_alloc(parser->arena, sizeof(*assg->assignee));
  *assg->assignee = assignee;
  assg->expr = arena_alloc(parser->arena, sizeof(*assg->expr));
  *assg->expr = expr;

  return true;
}

bool parse_compoundexpr(struct parser *parser,
                        struct ast_compoundexpr *compound_expr);

bool parse_arglist(struct parser *parser, struct ast_arglist *arg_list) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_compoundexpr compound_expr;

  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_compoundexpr(parser, &compound_expr) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    arg_list->args = compound_expr.exprs;
    arg_list->num_args = compound_expr.num_exprs;

    return true;
  }

  backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    arg_list->args = NULL;
    arg_list->num_args = 0;

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

struct ast_tyref var_ty_return_type_of(const struct ast_tyref *ty) {
  // one level of implicit deref occurs
  if (ty->ty == AST_TYREF_TY_POINTER) {
    ty = ty->pointer.underlying;
  }

  invariant_assert(ty->ty == AST_TYREF_TY_FUNC,
                   "only makes sense with func ty");

  return *ty->func.ret_var_ty;
}

bool parse_designator(struct parser *parser,
                      struct ast_designator *designator) {
  struct text_pos pos = get_position(parser->lexer);

  struct token identifier;
  struct ast_expr expr;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET) &&
      parse_constant_expr(parser, &expr) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET)) {
    designator->ty = AST_DESIGNATOR_TY_INDEX;
    designator->index = resolve_constant_expr(parser, &expr);
  } else if (parse_token(parser, LEX_TOKEN_TY_DOT) &&
             parse_identifier(parser, &identifier)) {
    designator->ty = AST_DESIGNATOR_TY_FIELD;
    designator->field = identifier;
  } else {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_designator next;
  if (parse_designator(parser, &next)) {
    designator->next = arena_alloc(parser->arena, sizeof(*designator->next));
    *designator->next = next;
  } else {
    designator->next = NULL;
  }

  return true;
}

bool parse_init(struct parser *parser, struct ast_init *init) {
  struct text_pos pos = get_position(parser->lexer);

  init->designator = NULL;

  struct ast_designator designator;
  if (parse_designator(parser, &designator) &&
      parse_token(parser, LEX_TOKEN_TY_OP_ASSG)) {
    init->designator = arena_alloc(parser->arena, sizeof(*init->designator));
    *init->designator = designator;
  }

  struct ast_expr expr;
  if (!parse_expr(parser, &expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  init->expr = arena_alloc(parser->arena, sizeof(*init->expr));
  *init->expr = expr;
  return true;
}

bool parse_initlist(struct parser *parser, struct ast_initlist *init_list) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct vector *inits = vector_create(sizeof(struct ast_init));
  {
    struct ast_init init;
    while (parse_init(parser, &init)) {
      vector_push_back(inits, &init);

      parse_token(parser, LEX_TOKEN_TY_COMMA);
    }
  }

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  init_list->inits = arena_alloc(parser->arena, vector_byte_size(inits));
  init_list->num_inits = vector_length(inits);

  vector_copy_to(inits, init_list->inits);
  vector_free(&inits);

  return true;
}

// parses highest precedence (literals, vars, constants)
bool parse_atom_0(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  // parenthesised expression
  struct ast_compoundexpr compound_expr;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_compoundexpr(parser, &compound_expr) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    // compound expressions return the last expression
    struct ast_tyref var_ty =
        compound_expr.exprs[compound_expr.num_exprs - 1].var_ty;
    expr->ty = AST_EXPR_TY_COMPOUNDEXPR;
    expr->var_ty = var_ty;
    expr->compound_expr = compound_expr;

    return true;
  }

  backtrack(parser->lexer, pos);

  struct ast_cnst cnst;
  if (parse_cnst(parser, &cnst)) {
    expr->ty = AST_EXPR_TY_CNST;
    expr->var_ty = cnst.cnst_ty;
    expr->cnst = cnst;

    return true;
  }

  struct ast_var var;
  if (parse_var(parser, &var)) {
    expr->ty = AST_EXPR_TY_VAR;
    expr->var_ty = var.var_ty;
    expr->var = var;

    return true;
  }

  return false;
}

void ensure_ty_defined(struct parser *parser, struct ast_tyref *ty_ref) {
  struct ast_tyref underlying_ty = tyref_get_underlying(parser, ty_ref);

  if (underlying_ty.ty == AST_TYREF_TY_TAGGED &&
      underlying_ty.tagged.underlying == NULL) {
    underlying_ty = tyref_get_defined(parser, &underlying_ty);

    struct ast_tyref *tagged;
    switch (ty_ref->ty) {
    case AST_TYREF_TY_POINTER:
      tagged = ty_ref->pointer.underlying;
      break;
    case AST_TYREF_TY_ARRAY:
      tagged = ty_ref->array.element;
      break;
    default:
      unreachable("doesn't make sense");
    }

    debug_assert(tagged->ty == AST_TYREF_TY_TAGGED, "should be TAGGED");
    tagged->tagged.underlying =
        arena_alloc(parser->arena, sizeof(*tagged->tagged.underlying));
    *tagged->tagged.underlying = underlying_ty;
  }
}
// parses precedence level 0:
// vars
bool parse_atom_1(struct parser *parser, struct ast_expr *expr) {
  if (parse_atom_0(parser, expr)) {
    return true;
  }

  struct ast_initlist init_list;
  if (parse_initlist(parser, &init_list)) {
    expr->ty = AST_EXPR_TY_INIT_LIST;
    // init lists are not typed
    expr->var_ty.ty = AST_TYREF_TY_UNKNOWN;
    expr->var_ty.type_qualifiers = 0;
    expr->init_list = init_list;
    return true;
  }

  return false;
}

bool parse_call(struct parser *parser, struct ast_expr *sub_expr,
                struct ast_expr *expr) {
  struct ast_arglist arg_list;
  if (!parse_arglist(parser, &arg_list)) {
    return false;
  }

  struct ast_tyref var_ty = var_ty_return_type_of(&sub_expr->var_ty);
  expr->ty = AST_EXPR_TY_CALL;
  expr->var_ty = var_ty;
  expr->call.var_ty = var_ty;
  expr->call.target = arena_alloc(parser->arena, sizeof(*expr->call.target));
  expr->call.target = sub_expr;
  expr->call.arg_list = arg_list;

  return true;
}

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

bool parse_array_access(struct parser *parser, struct ast_expr *sub_expr,
                        struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr *access_expr =
      arena_alloc(parser->arena, sizeof(*access_expr));
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET) &&
      parse_expr(parser, access_expr) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET)) {
    expr->ty = AST_EXPR_TY_ARRAYACCESS;

    bool sub_expr_is_pointer;
    struct ast_tyref var_ty = resolve_array_access_ty(
        parser, sub_expr, access_expr, &sub_expr_is_pointer);

    expr->var_ty = var_ty;

    if (sub_expr_is_pointer) {
      expr->array_access.lhs = sub_expr;
      expr->array_access.rhs = access_expr;
    } else {
      expr->array_access.lhs = access_expr;
      expr->array_access.rhs = sub_expr;
    }

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_member_access(struct parser *parser, struct ast_expr *sub_expr,
                         struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_DOT)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct token token;
  peek_token(parser->lexer, &token);
  if (token.ty != LEX_TOKEN_TY_IDENTIFIER) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, token);

  struct ast_tyref var_ty =
      resolve_member_access_ty(parser, &sub_expr->var_ty, &token);

  expr->ty = AST_EXPR_TY_MEMBERACCESS;
  expr->var_ty = var_ty;
  expr->member_access =
      (struct ast_memberaccess){.lhs = sub_expr, .member = token};

  return true;
}

bool parse_pointer_access(struct parser *parser, struct ast_expr *sub_expr,
                          struct ast_expr *expr) {

  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_ARROW)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct token token;
  peek_token(parser->lexer, &token);
  if (token.ty != LEX_TOKEN_TY_IDENTIFIER) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, token);

  ensure_ty_defined(parser, &sub_expr->var_ty);

  struct ast_tyref var_ty =
      resolve_pointer_access_ty(parser, &sub_expr->var_ty, &token);

  expr->ty = AST_EXPR_TY_POINTERACCESS;
  expr->var_ty = var_ty;
  expr->pointer_access =
      (struct ast_pointeraccess){.lhs = sub_expr, .member = token};

  return true;
}

bool parse_unary_postfix_op(struct parser *parser, struct ast_expr *sub_expr,
                            struct ast_expr *expr) {
  bool has_unary_postfix = false;
  enum ast_unary_op_ty unary_postfix_ty;
  if (parse_token(parser, LEX_TOKEN_TY_OP_INC)) {
    has_unary_postfix = true;
    unary_postfix_ty = AST_UNARY_OP_TY_POSTFIX_INC;
  } else if (parse_token(parser, LEX_TOKEN_TY_OP_DEC)) {
    has_unary_postfix = true;
    unary_postfix_ty = AST_UNARY_OP_TY_POSTFIX_DEC;
  }

  if (!has_unary_postfix) {
    return false;
  }

  struct ast_tyref var_ty =
      resolve_unary_op_types(parser, unary_postfix_ty, &sub_expr->var_ty, NULL);
  struct ast_unary_op unary_op = {
      .ty = unary_postfix_ty,
      .var_ty = var_ty,
      .expr = sub_expr,
  };

  expr->ty = AST_EXPR_TY_UNARY_OP;
  expr->var_ty = var_ty;
  expr->unary_op = unary_op;

  return true;
}

// parses precedence level 1:
// postfix ++, postfix --, (), [], ., ->, (type){list}
bool parse_atom_2(struct parser *parser, struct ast_expr *expr) {
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

bool parse_cast(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_tyref ty_ref;
  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) ||
      !parse_abstract_declaration(parser, &ty_ref) ||
      !parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*sub_expr));
  if (!parse_atom_3(parser, sub_expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  expr->ty = AST_EXPR_TY_UNARY_OP;
  expr->var_ty = ty_ref;
  expr->unary_op =
      (struct ast_unary_op){.ty = AST_UNARY_OP_TY_CAST,
                            .var_ty = ty_ref,
                            .expr = sub_expr,
                            // TODO: this is redundant
                            .cast = (struct ast_cast){.cast_ty = ty_ref}};

  return true;
}

bool parse_unary_prefix_op(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

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
  // case LEX_TOKEN_TY_OP_SIZEOF:
  //   ty = AST_UNARY_OP_TY_SIZEOF;
  //   break;
  // case LEX_TOKEN_TY_OP_ALIGNOF:
  //   ty = AST_UNARY_OP_TY_ALIGNOF;
  //   break;
  // case LEX_TOKEN_TY_OP_CAST:
  //   ty = AST_UNARY_OP_TY_CAST;
  //   break;
  default:
    // just pure expr
    has_unary_prefix = false;
  }

  if (!has_unary_prefix) {
    return false;
  }

  consume_token(parser->lexer, token);

  struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*sub_expr));
  if (!parse_atom_3(parser, sub_expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (unary_prefix_ty == AST_UNARY_OP_TY_INDIRECTION) {
    ensure_ty_defined(parser, &sub_expr->var_ty);
  }

  struct ast_tyref var_ty =
      resolve_unary_op_types(parser, unary_prefix_ty, &sub_expr->var_ty, NULL);
  struct ast_unary_op unary_op = {
      .ty = unary_prefix_ty,
      .var_ty = var_ty,
      .expr = sub_expr,
  };

  expr->ty = AST_EXPR_TY_UNARY_OP;
  expr->var_ty = var_ty;
  expr->unary_op = unary_op;

  return true;
}

bool parse_sizeof(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_SIZEOF)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  // because of how sizeof works, we need to try and parse `sizeof(<ty_ref>)`
  // first else, something like `sizeof(char) + sizeof(short)` will be resolves
  // as `sizeof( (char) + sizeof(short) )` that is, the size of `+sizeof(short)`
  // cast to `char`

  struct text_pos post_sizeof_pos = get_position(parser->lexer);

  struct ast_tyref ty_ref;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_abstract_declaration(parser, &ty_ref) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    expr->ty = AST_EXPR_TY_SIZEOF;
    expr->var_ty = tyref_pointer_sized_int(parser, false);
    expr->size_of =
        (struct ast_sizeof){.ty = AST_SIZEOF_TY_TYPE, .ty_ref = ty_ref};

    return true;
  }

  backtrack(parser->lexer, post_sizeof_pos);

  struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*sub_expr));
  if (parse_atom_3(parser, sub_expr)) {
    expr->ty = AST_EXPR_TY_SIZEOF;
    expr->var_ty = tyref_pointer_sized_int(parser, false);
    expr->size_of =
        (struct ast_sizeof){.ty = AST_SIZEOF_TY_EXPR, .expr = sub_expr};

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_alignof(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_ALIGNOF) ||
      !parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_tyref ty_ref;
  if (!parse_abstract_declaration(parser, &ty_ref) ||
      !parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  expr->ty = AST_EXPR_TY_ALIGNOF;
  expr->var_ty = tyref_pointer_sized_int(parser, false);
  expr->align_of = (struct ast_alignof){.ty_ref = ty_ref};

  return true;
}

// parses precedence level 2:
// prefix ++, prefix --, unary +, unary -, !, ~, (type), *, &, sizeof, _Alignof
bool parse_atom_3(struct parser *parser, struct ast_expr *expr) {
  if (!parse_unary_prefix_op(parser, expr) && !parse_cast(parser, expr) &&
      !parse_sizeof(parser, expr) && !parse_alignof(parser, expr) &&
      !parse_atom_2(parser, expr)) {
    return false;
  }

  return true;
}

bool parse_expr_precedence_aware(struct parser *parser, unsigned min_precedence,
                                 struct ast_expr *expr) {
  if (!parse_atom_3(parser, expr)) {
    return false;
  }

  // TODO: make iterative
  while (true) {
    struct token lookahead;
    peek_token(parser->lexer, &lookahead);
    debug("looked ahead to %s", token_name(parser->lexer, &lookahead));
    struct ast_op_info info;

    if (!op_info_for_token(&lookahead, &info) ||
        info.precedence < min_precedence) {
      return true;
    }

    consume_token(parser->lexer, lookahead);

    debug_assert(info.associativity != AST_ASSOCIATIVITY_NONE,
                 "only operators with associativity should reach here!");
    unsigned next_min_precedence;
    if (info.associativity == AST_ASSOCIATIVITY_LEFT) {
      next_min_precedence = info.precedence + 1;
    } else {
      next_min_precedence = info.precedence;
    }

    struct ast_expr rhs;
    invariant_assert(
        parse_expr_precedence_aware(parser, next_min_precedence, &rhs),
        "expected parse failed");

    // slightly odd design where `expr` now contains lhs and `rhs` contains
    // `rhs` so we need to in-place modify `expr`
    struct ast_expr lhs = *expr;

    struct ast_tyref intermediate_ty = resolve_binary_op_intermediate_types(
        parser, info.ty, &lhs.var_ty, &rhs.var_ty);

    struct ast_tyref result_ty;
    if (ast_binary_op_is_comparison(info.ty)) {
      result_ty = (struct ast_tyref){.ty = AST_TYREF_TY_WELL_KNOWN,
                                     .well_known = WELL_KNOWN_TY_SIGNED_INT};
    } else {
      result_ty = intermediate_ty;
    }

    expr->ty = AST_EXPR_TY_BINARY_OP;
    expr->var_ty = result_ty;
    struct ast_binary_op *binary_op = &expr->binary_op;
    binary_op->ty = info.ty;
    binary_op->intermediate_var_ty = intermediate_ty;

    binary_op->lhs = arena_alloc(parser->arena, sizeof(*binary_op->lhs));
    *binary_op->lhs = lhs;

    binary_op->rhs = arena_alloc(parser->arena, sizeof(*binary_op->rhs));
    *binary_op->rhs = rhs;
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

bool parse_ternary(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr cond, true_expr, false_expr;
  if (parse_expr_precedence_aware(parser, 0, &cond) &&
      parse_token(parser, LEX_TOKEN_TY_QMARK) &&
      parse_expr_precedence_aware(parser, 0, &true_expr) &&
      parse_token(parser, LEX_TOKEN_TY_COLON) &&
      parse_expr_precedence_aware(parser, 0, &false_expr)) {
    expr->ty = AST_EXPR_TY_TERNARY;
    expr->var_ty =
        resolve_ternary_ty(parser, &true_expr.var_ty, &false_expr.var_ty);
    expr->ternary = (struct ast_ternary){
        .var_ty = expr->var_ty,
        .cond = arena_alloc(parser->arena, sizeof(*expr->ternary.cond)),
        .true_expr =
            arena_alloc(parser->arena, sizeof(*expr->ternary.true_expr)),
        .false_expr =
            arena_alloc(parser->arena, sizeof(*expr->ternary.false_expr)),
    };

    *expr->ternary.cond = cond;
    *expr->ternary.true_expr = true_expr;
    *expr->ternary.false_expr = false_expr;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_constant_expr(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (parse_ternary(parser, expr)) {
    return true;
  }

  // all non-assignment expressions
  if (!parse_expr_precedence_aware(parser, 0, expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

// parse a non-compound expression
bool parse_expr(struct parser *parser, struct ast_expr *expr) {
  // assignment is lowest precedence, so we parse it here

  struct ast_assg assg;
  if (parse_assg(parser, &assg)) {
    expr->ty = AST_EXPR_TY_ASSG;
    expr->var_ty = assg.var_ty;
    expr->assg = assg;

    return true;
  }

  return parse_constant_expr(parser, expr);
}

// there are only two places you can have compound expressions
// * at top level of a statement (e.g `a = 1, b = 2;`)
// * within braces (e.g `(a = 1, b = 2)`)
// so only those places call this method for that purpose.
// `parse_call` calls this method as a helper but doesn't actually parse it as a
// compound expr
bool parse_compoundexpr(struct parser *parser,
                        struct ast_compoundexpr *compound_expr) {
  struct text_pos pos = get_position(parser->lexer);

  // this could be made recursive instead

  struct vector *exprs = vector_create(sizeof(struct ast_expr));

  struct token token;
  struct ast_expr sub_expr;
  do {
    if (!parse_expr(parser, &sub_expr)) {
      backtrack(parser->lexer, pos);
      return false;
    }

    vector_push_back(exprs, &sub_expr);

    peek_token(parser->lexer, &token);
  } while (token.ty == LEX_TOKEN_TY_COMMA &&
           /* hacky */ (consume_token(parser->lexer, token), true));

  if (vector_empty(exprs)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  compound_expr->exprs = arena_alloc(parser->arena, vector_byte_size(exprs));
  compound_expr->num_exprs = vector_length(exprs);

  vector_copy_to(exprs, compound_expr->exprs);
  vector_free(&exprs);

  return true;
}

// parse a non-compound expression, ending with a semicolon
bool parse_compoundexpr_with_semicolon(struct parser *parser,
                                       struct ast_compoundexpr *compoundexpr) {
  struct text_pos pos = get_position(parser->lexer);

  if (parse_compoundexpr(parser, compoundexpr) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_declarator(struct parser *parser, struct token *identifier,
                      struct ast_tyref *ty_ref);

bool parse_decl(struct parser *parser, struct ast_tyref *specifier,
                struct ast_decl *var_decl) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_var var;
  var.var_ty = *specifier;

  struct token identifier;
  if (!parse_declarator(parser, &identifier, &var.var_ty)) {
    return false;
  }

  // ignore any existing scope of the var
  var.identifier = identifier;
  var.scope = cur_scope(&parser->var_table);
  var_decl->var = var;

  struct text_pos post_var_pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_OP_ASSG)) {
    // normal var decl, without assignment
    // return to where we were when we passed the var
    backtrack(parser->lexer, post_var_pos);

    var_decl->ty = AST_DECL_TY_DECL;

    return true;
  }

  struct ast_expr expr;
  // we use `parse_single_expr` as compound expressions are not legal here and
  // interfere with comma-seperated decls
  if (!parse_expr(parser, &expr)) {
    // we parsed the var but not the expr
    // just give up
    // TODO: err message
    backtrack(parser->lexer, pos);
    return false;
  }

  // fixup unsized arrays
  if (var.var_ty.ty == AST_TYREF_TY_ARRAY &&
      var.var_ty.array.ty == AST_TY_ARRAY_TY_UNKNOWN_SIZE) {
    size_t size;
    if (expr.ty == AST_EXPR_TY_CNST) {
      // must be string
      size = strlen(expr.cnst.str_value) + 1;
    } else if (expr.ty == AST_EXPR_TY_INIT_LIST) {
      size_t idx = 0;
      size_t max_idx = 0;
      for (size_t i = 0; i < expr.init_list.num_inits; i++) {
        struct ast_init *init = &expr.init_list.inits[i];
        if (init->designator &&
            init->designator->ty == AST_DESIGNATOR_TY_INDEX) {
          idx = init->designator->index;
        }
        max_idx = MAX(max_idx, idx);

        idx++;
      }

      size = max_idx + 1;
    } else {
      bug("couldn't determine array size");
    }

    var.var_ty.array.ty = AST_TY_ARRAY_TY_KNOWN_SIZE;
    var.var_ty.array.size = size;
  }

  var_decl->ty = AST_DECL_TY_DECL_WITH_ASSG;
  var_decl->assg_expr = expr;
  var_decl->var = var;

  return true;
}

bool parse_decllist(struct parser *parser, struct ast_decllist *decl_list) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_tyref specifier;
  if (!parse_type_specifier(parser, &specifier,
                            &decl_list->storage_class_specifiers)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  enum var_table_entry_ty entry_ty;
  struct var_table *table;
  if (decl_list->storage_class_specifiers &
      AST_STORAGE_CLASS_SPECIFIER_FLAG_TYPEDEF) {
    entry_ty = VAR_TABLE_ENTRY_TY_TYPEDEF;
    table = &parser->ty_table;
  } else {
    entry_ty = VAR_TABLE_ENTRY_TY_VAR;
    table = &parser->var_table;
  }

  struct vector *decls = vector_create(sizeof(struct ast_decl));

  struct ast_decl var_decl;
  while (parse_decl(parser, &specifier, &var_decl)) {
    vector_push_back(decls, &var_decl);

    trace("creating var_table_entry for var name=%s",
          identifier_str(parser, &var_decl.var.identifier));

    struct var_table_entry *entry =
        create_entry(table, identifier_str(parser, &var_decl.var.identifier));
    entry->value = arena_alloc(parser->arena, sizeof(*entry->value));

    entry->ty = entry_ty;
    *entry->value = var_decl.var.var_ty;

    struct token token;
    peek_token(parser->lexer, &token);

    if (token.ty == LEX_TOKEN_TY_COMMA) {
      // another decl
      consume_token(parser->lexer, token);
    }
  }

  if (!parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_decl *new_decls =
      arena_alloc(parser->arena, vector_byte_size(decls));
  vector_copy_to(decls, new_decls);

  decl_list->num_decls = vector_length(decls);
  decl_list->decls = new_decls;

  vector_free(&decls);

  return true;
}

bool parse_jumpstmt(struct parser *parser, struct ast_jumpstmt *jump_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr expr;
  if (parse_token(parser, LEX_TOKEN_TY_KW_RETURN) &&
      parse_expr(parser, &expr) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    jump_stmt->ty = AST_JUMPSTMT_TY_RETURN;
    jump_stmt->return_stmt.var_ty = parser->func_ret_ty;
    jump_stmt->return_stmt.expr =
        arena_alloc(parser->arena, sizeof(*jump_stmt->return_stmt.expr));
    *jump_stmt->return_stmt.expr = expr;

    return true;
  }

  backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_RETURN) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    jump_stmt->ty = AST_JUMPSTMT_TY_RETURN;
    jump_stmt->return_stmt.var_ty = parser->func_ret_ty;
    jump_stmt->return_stmt.expr = NULL;

    return true;
  }

  backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_BREAK) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    jump_stmt->ty = AST_JUMPSTMT_TY_BREAK;
    return true;
  }

  backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_CONTINUE) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    jump_stmt->ty = AST_JUMPSTMT_TY_CONTINUE;
    return true;
  }

  backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_GOTO)) {
    struct token label;
    peek_token(parser->lexer, &label);

    if (label.ty == LEX_TOKEN_TY_IDENTIFIER) {
      consume_token(parser->lexer, label);

      if (parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
        jump_stmt->ty = AST_JUMPSTMT_TY_GOTO;
        jump_stmt->goto_stmt.label = label;
        return true;
      }
    }
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_stmt(struct parser *parser, struct ast_stmt *stmt);

bool parse_labeledstmt(struct parser *parser,
                       struct ast_labeledstmt *labeled_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct token label;
  peek_token(parser->lexer, &label);
  consume_token(parser->lexer, label);

  struct ast_expr expr;

  if (label.ty == LEX_TOKEN_TY_KW_DEFAULT) {
    labeled_stmt->ty = AST_LABELEDSTMT_TY_DEFAULT;
  } else if (label.ty == LEX_TOKEN_TY_KW_CASE && parse_expr(parser, &expr)) {
    labeled_stmt->ty = AST_LABELEDSTMT_TY_CASE;
    labeled_stmt->cnst = resolve_constant_expr(parser, &expr);
  } else if (label.ty == LEX_TOKEN_TY_IDENTIFIER) {
    labeled_stmt->ty = AST_LABELEDSTMT_TY_LABEL;
    labeled_stmt->label = label;
  } else {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_token(parser, LEX_TOKEN_TY_COLON) || !parse_stmt(parser, &stmt)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  labeled_stmt->stmt = arena_alloc(parser->arena, sizeof(*labeled_stmt->stmt));
  *labeled_stmt->stmt = stmt;
  return true;
}

bool parse_ifstmt(struct parser *parser, struct ast_ifstmt *if_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_IF) ||
      !parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_expr expr;
  if (!parse_expr(parser, &expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_stmt(parser, &stmt)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if_stmt->condition = expr;
  if_stmt->body = arena_alloc(parser->arena, sizeof(*if_stmt->body));
  *if_stmt->body = stmt;

  return true;
}

bool parse_ifelsestmt(struct parser *parser,
                      struct ast_ifelsestmt *if_else_stmt) {
  // parse `if {}`, then try parse `else`
  // not perfectly efficient but more elegant

  struct text_pos pos = get_position(parser->lexer);

  struct ast_ifstmt if_stmt;
  struct ast_stmt else_stmt;
  if (!parse_ifstmt(parser, &if_stmt) ||
      !parse_token(parser, LEX_TOKEN_TY_KW_ELSE) ||
      !parse_stmt(parser, &else_stmt)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if_else_stmt->condition = if_stmt.condition;
  if_else_stmt->body = arena_alloc(parser->arena, sizeof(*if_else_stmt->body));
  if_else_stmt->body = if_stmt.body;
  if_else_stmt->else_body =
      arena_alloc(parser->arena, sizeof(*if_else_stmt->else_body));
  *if_else_stmt->else_body = else_stmt;

  return true;
}

bool parse_switchstmt(struct parser *parser,
                      struct ast_switchstmt *switch_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr ctrl_expr;
  if (!parse_token(parser, LEX_TOKEN_TY_KW_SWITCH) ||
      !parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) ||
      !parse_expr(parser, &ctrl_expr) ||
      !parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_stmt(parser, &stmt)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  switch_stmt->ctrl_expr = ctrl_expr;
  switch_stmt->body = arena_alloc(parser->arena, sizeof(*switch_stmt->body));
  *switch_stmt->body = stmt;

  return true;
}

bool parse_whilestmt(struct parser *parser, struct ast_whilestmt *while_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_WHILE) ||
      !parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_expr expr;
  if (!parse_expr(parser, &expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_stmt(parser, &stmt)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  while_stmt->cond = expr;
  while_stmt->body = arena_alloc(parser->arena, sizeof(*while_stmt->body));
  *while_stmt->body = stmt;

  return true;
}

bool parse_dowhilestmt(struct parser *parser,
                       struct ast_dowhilestmt *do_while_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_DO)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt stmt;
  if (!parse_stmt(parser, &stmt)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_KW_WHILE) ||
      !parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_expr expr;
  if (!parse_expr(parser, &expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  do_while_stmt->cond = expr;
  do_while_stmt->body =
      arena_alloc(parser->arena, sizeof(*do_while_stmt->body));
  *do_while_stmt->body = stmt;

  return true;
}

bool parse_declorexpr(struct parser *parser,
                      struct ast_declorexpr *decl_or_expr) {
  struct text_pos pos = get_position(parser->lexer);

  decl_or_expr->decl = NULL;
  decl_or_expr->expr = NULL;

  struct ast_decllist decl_list;
  if (parse_decllist(parser, &decl_list)) {
    decl_or_expr->decl =
        arena_alloc(parser->arena, sizeof(*decl_or_expr->decl));
    *decl_or_expr->decl = decl_list;
    return true;
  }

  backtrack(parser->lexer, pos);

  struct ast_expr expr;
  if (parse_expr(parser, &expr) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    decl_or_expr->expr =
        arena_alloc(parser->arena, sizeof(*decl_or_expr->expr));
    *decl_or_expr->expr = expr;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_forstmt(struct parser *parser, struct ast_forstmt *for_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  // first parse the `for (`
  if (!(parse_token(parser, LEX_TOKEN_TY_KW_FOR) &&
        parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET))) {
    backtrack(parser->lexer, pos);
    return false;
  }

  // then, we look for a vardecllist or an expression, or neither
  struct ast_declorexpr decl_or_expr;
  if (parse_declorexpr(parser, &decl_or_expr)) {
    for_stmt->init = arena_alloc(parser->arena, sizeof(*for_stmt->init));
    *for_stmt->init = decl_or_expr;
  } else if (parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    for_stmt->init = NULL;
  } else {
    backtrack(parser->lexer, pos);
    return false;
  }

  // parse the condition if present, else a semicolon
  struct ast_expr cond;
  if (parse_expr(parser, &cond)) {
    for_stmt->cond = arena_alloc(parser->arena, sizeof(*for_stmt->cond));
    *for_stmt->cond = cond;
  } else {
    for_stmt->cond = NULL;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  // parse the iteration statement if present, else nothing
  struct ast_expr iter;
  if (parse_expr(parser, &iter)) {
    for_stmt->iter = arena_alloc(parser->arena, sizeof(*for_stmt->iter));
    *for_stmt->iter = iter;
  } else {
    for_stmt->iter = NULL;
  }

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_stmt body;
  if (!parse_stmt(parser, &body)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  for_stmt->body = arena_alloc(parser->arena, sizeof(*for_stmt->body));
  *for_stmt->body = body;

  return true;
}

bool parse_iterstmt(struct parser *parser, struct ast_iterstmt *iter_stmt) {
  struct ast_whilestmt while_stmt;
  if (parse_whilestmt(parser, &while_stmt)) {
    iter_stmt->ty = AST_ITERSTMT_TY_WHILE;
    iter_stmt->while_stmt = while_stmt;
    return true;
  }

  struct ast_dowhilestmt do_while_stmt;
  if (parse_dowhilestmt(parser, &do_while_stmt)) {
    iter_stmt->ty = AST_ITERSTMT_TY_DO_WHILE;
    iter_stmt->do_while_stmt = do_while_stmt;
    return true;
  }

  struct ast_forstmt for_stmt;
  if (parse_forstmt(parser, &for_stmt)) {
    iter_stmt->ty = AST_ITERSTMT_TY_FOR;
    iter_stmt->for_stmt = for_stmt;
    return true;
  }

  return false;
}

bool parse_selectstmt(struct parser *parser,
                      struct ast_selectstmt *select_stmt) {
  struct ast_ifelsestmt if_else_stmt;
  if (parse_ifelsestmt(parser, &if_else_stmt)) {
    select_stmt->ty = AST_SELECTSTMT_TY_IF_ELSE;
    select_stmt->if_else_stmt = if_else_stmt;
    return true;
  }

  struct ast_ifstmt if_stmt;
  if (parse_ifstmt(parser, &if_stmt)) {
    select_stmt->ty = AST_SELECTSTMT_TY_IF;
    select_stmt->if_stmt = if_stmt;
    return true;
  }

  struct ast_switchstmt switch_stmt;
  if (parse_switchstmt(parser, &switch_stmt)) {
    select_stmt->ty = AST_SELECTSTMT_TY_SWITCH;
    select_stmt->switch_stmt = switch_stmt;
    return true;
  }

  return false;
}

bool parse_compoundstmt(struct parser *parser,
                        struct ast_compoundstmt *compound_stmt);

bool parse_type_specifier(
    struct parser *parser, struct ast_tyref *ty_ref,
    enum ast_storage_class_specifier_flags *storage_class_specifiers);

bool parse_declarator(struct parser *parser, struct token *identifier,
                      struct ast_tyref *ty_ref);

bool parse_stmt(struct parser *parser, struct ast_stmt *stmt) {
  struct text_pos pos = get_position(parser->lexer);

  // FIXME: this shouldn't be needed
  if (parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    stmt->ty = AST_STMT_TY_NULL;
    return true;
  }

  struct ast_labeledstmt labeled_stmt;
  if (parse_labeledstmt(parser, &labeled_stmt)) {
    stmt->ty = AST_STMT_TY_LABELED;
    stmt->labeled = labeled_stmt;
    return true;
  }

  struct ast_jumpstmt jump_stmt;
  if (parse_jumpstmt(parser, &jump_stmt)) {
    stmt->ty = AST_STMT_TY_JUMP;
    stmt->jump = jump_stmt;
    return true;
  }

  struct ast_selectstmt select_stmt;
  if (parse_selectstmt(parser, &select_stmt)) {
    stmt->ty = AST_STMT_TY_SELECT;
    stmt->select = select_stmt;
    return true;
  }

  struct ast_iterstmt iter_stmt;
  if (parse_iterstmt(parser, &iter_stmt)) {
    stmt->ty = AST_STMT_TY_ITER;
    stmt->iter = iter_stmt;
    return true;
  }

  struct ast_compoundstmt compound_stmt;
  if (parse_compoundstmt(parser, &compound_stmt)) {
    stmt->ty = AST_STMT_TY_COMPOUND;
    stmt->compound = compound_stmt;
    return true;
  }

  struct ast_compoundexpr compound_expr;
  if (parse_compoundexpr_with_semicolon(parser, &compound_expr)) {
    // compound expressions return the last expression
    struct ast_tyref var_ty =
        compound_expr.exprs[compound_expr.num_exprs - 1].var_ty;
    stmt->ty = AST_STMT_TY_EXPR;
    stmt->expr.ty = AST_EXPR_TY_COMPOUNDEXPR;
    stmt->expr.var_ty = var_ty;
    stmt->expr.compound_expr = compound_expr;

    return true;
  }

  // struct ast_expr expr;
  // if (parse_expr(parser, &expr) && parse_token(parser,
  // LEX_TOKEN_TY_SEMICOLON)) {
  //   stmt->ty = AST_STMT_TY_EXPR;
  //   stmt->expr = expr;
  //   return true;
  // }
  // backtrack(parser->lexer, pos);

  struct ast_decllist decl_list;
  if (parse_decllist(parser, &decl_list)) {
    stmt->ty = AST_STMT_TY_DECL_LIST;
    stmt->decl_list = decl_list;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_compoundstmt(struct parser *parser,
                        struct ast_compoundstmt *compound_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  parser_push_scope(parser);

  struct vector *stmts = vector_create(sizeof(struct ast_stmt));
  {
    struct ast_stmt stmt;
    while (parse_stmt(parser, &stmt)) {
      vector_push_back(stmts, &stmt);
    }
  }

  compound_stmt->stmts = arena_alloc(parser->arena, vector_byte_size(stmts));
  compound_stmt->num_stmts = vector_length(stmts);
  vector_copy_to(stmts, compound_stmt->stmts);
  vector_free(&stmts);

  parser_pop_scope(parser);

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

void decay_array_params(struct ast_param *param) {
  if (param->var_ty.ty == AST_TYREF_TY_ARRAY) {
    param->var_ty.ty = AST_TYREF_TY_POINTER;
    param->var_ty.pointer.underlying = param->var_ty.array.element;
  }
}

bool parse_param(struct parser *parser, struct ast_param *param) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);
  if (token.ty == LEX_TOKEN_TY_ELLIPSIS) {
    consume_token(parser->lexer, token);

    // variadic parameter pack
    param->var_ty = (struct ast_tyref){
        .ty = AST_TYREF_TY_VARIADIC,
    };
    param->var = arena_alloc(parser->arena, sizeof(*param->var));
    *param->var = (struct ast_var){.scope = cur_scope(&parser->var_table),
                                   .identifier = token};

    return true;
  }

  struct ast_tyref var_ty;
  struct token identifier;
  // FIXME: this is wrong

  if (!parse_type_specifier(parser, &var_ty, NULL)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (parse_declarator(parser, &identifier, &var_ty)) {
    param->var_ty = var_ty;
    param->var = arena_alloc(parser->arena, sizeof(*param->var));
    param->var->scope = cur_scope(&parser->var_table);
    param->var->identifier = identifier;

    decay_array_params(param);

    return true;
  } else if (parse_abstract_declarator(parser, &var_ty)) {
    param->var_ty = var_ty;
    param->var = NULL;

    decay_array_params(param);

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_paramlist(struct parser *parser, struct ast_paramlist *param_list) {
  struct text_pos pos = get_position(parser->lexer);

  parser_push_scope(parser);

  // TODO: merge with parse_compoundexpr?
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET)) {
    struct text_pos pos = get_position(parser->lexer);

    // this could be made recursive instead

    struct vector *params = vector_create(sizeof(struct ast_param));

    struct text_pos param_pos = get_position(parser->lexer);

    struct token token;
    if (parse_token(parser, LEX_TOKEN_TY_KW_VOID)) {
      peek_token(parser->lexer, &token);
      if (token.ty != LEX_TOKEN_TY_CLOSE_BRACKET) {
        backtrack(parser->lexer, param_pos);
      }
    }

    peek_token(parser->lexer, &token);
    if (token.ty != LEX_TOKEN_TY_CLOSE_BRACKET) {
      struct ast_param param;
      do {
        if (!parse_param(parser, &param)) {
          backtrack(parser->lexer, pos);
          parser_pop_scope(parser);
          return false;
        }

        vector_push_back(params, &param);

        peek_token(parser->lexer, &token);
      } while (token.ty == LEX_TOKEN_TY_COMMA &&
               /* hacky */ (consume_token(parser->lexer, token), true));

      param_list->params = arena_alloc(parser->arena, vector_byte_size(params));
      param_list->num_params = vector_length(params);

      vector_copy_to(params, param_list->params);
      vector_free(&params);
    } else {
      param_list->params = NULL;
      param_list->num_params = 0;
    }

    if (parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
      parser_pop_scope(parser);
      return true;
    }
  }

  backtrack(parser->lexer, pos);
  parser_pop_scope(parser);
  return false;
}

bool parse_declaration_specifiers(
    struct parser *parser, struct ast_tyref *ty_ref,
    enum ast_storage_class_specifier_flags *storage_class_specifiers) {
  if (parse_function_specifiers(parser, &ty_ref->function_specifiers) ||
      (storage_class_specifiers &&
       parse_storage_class_specifiers(parser, storage_class_specifiers)) ||
      parse_type_qualifiers(parser, &ty_ref->type_qualifiers)) {
    return parse_declaration_specifiers(parser, ty_ref,
                                        storage_class_specifiers);
  }

  return false;
}

struct ast_tyref
make_aggregate_ty(struct parser *parser, struct token type, const char *name,
                  struct ast_structdecllist *decl_list,
                  enum ast_function_specifier_flags function_specifiers,
                  enum ast_type_qualifier_flags type_qualifiers) {
  size_t total_fields = 0;
  for (size_t i = 0; i < decl_list->num_struct_decls; i++) {
    total_fields += decl_list->struct_decls[i].num_fields;
  }

  struct ast_struct_field *fields = arena_alloc(
      parser->arena, sizeof(struct ast_struct_field) * total_fields);

  size_t idx = 0;
  for (size_t i = 0; i < decl_list->num_struct_decls; i++) {
    struct ast_structdecl *field_list = &decl_list->struct_decls[i];
    for (size_t j = 0; j < field_list->num_fields; j++) {
      struct ast_struct_field *field = &fields[idx++];

      if (field_list->fields[j].ty == AST_DECL_TY_ANON_DECL) {
        field->name = NULL;
      } else {
        field->name =
            identifier_str(parser, &field_list->fields[j].var.identifier);
      }
      field->var_ty = arena_alloc(parser->arena, sizeof(*field->var_ty));
      *field->var_ty = field_list->fields[j].var.var_ty;
    }
  }

  enum ast_ty_aggregate_ty aggregate_ty;
  switch (type.ty) {
  case LEX_TOKEN_TY_KW_UNION:
    aggregate_ty = AST_TY_AGGREGATE_TY_UNION;
    break;
  case LEX_TOKEN_TY_KW_STRUCT:
    aggregate_ty = AST_TY_AGGREGATE_TY_STRUCT;
    break;
  default:
    unreachable("impossible");
  }

  struct ast_ty_aggregate aggregate;
  aggregate.ty = aggregate_ty;
  aggregate.name = name;
  aggregate.num_field_var_tys = total_fields;
  aggregate.field_var_tys = fields;

  return (struct ast_tyref){.ty = AST_TYREF_TY_AGGREGATE,
                            .function_specifiers = function_specifiers,
                            .type_qualifiers = type_qualifiers,
                            .aggregate = aggregate};
}

bool parse_type_specifier(
    struct parser *parser, struct ast_tyref *ty_ref,
    enum ast_storage_class_specifier_flags *storage_class_specifiers) {
  struct text_pos pos = get_position(parser->lexer);

  if (storage_class_specifiers) {
    *storage_class_specifiers = AST_STORAGE_CLASS_SPECIFIER_FLAG_NONE;
  }

  ty_ref->type_qualifiers = AST_TYPE_QUALIFIER_FLAG_NONE;
  ty_ref->function_specifiers = AST_FUNCTION_SPECIFIER_FLAG_NONE;

  parse_declaration_specifiers(parser, ty_ref, storage_class_specifiers);

  if (parse_wkt(parser, ty_ref)) {
    return true;
  }

  struct token type;
  peek_token(parser->lexer, &type);
  consume_token(parser->lexer, type);

  struct token token;
  const char *name = NULL;
  if (type.ty == LEX_TOKEN_TY_KW_STRUCT || type.ty == LEX_TOKEN_TY_KW_UNION) {
    peek_token(parser->lexer, &token);
    if (token.ty == LEX_TOKEN_TY_IDENTIFIER) {
      name = identifier_str(parser, &token);
      consume_token(parser->lexer, token);
    }

    struct ast_structdecllist decl_list;
    if (parse_structdecllist(parser, &decl_list)) {
      *ty_ref = make_aggregate_ty(parser, type, name, &decl_list,
                                  ty_ref->function_specifiers,
                                  ty_ref->type_qualifiers);

      enum var_table_entry_ty entry_ty;
      switch (type.ty) {
      case LEX_TOKEN_TY_KW_UNION:
        entry_ty = VAR_TABLE_ENTRY_TY_UNION;
        break;
      case LEX_TOKEN_TY_KW_STRUCT:
        entry_ty = VAR_TABLE_ENTRY_TY_STRUCT;
        break;
      default:
        unreachable("impossible");
      }

      if (name) {
        struct var_table_entry *entry = create_entry(&parser->ty_table, name);
        if (!entry->value) {
          entry->value = arena_alloc(parser->arena, sizeof(struct ast_tyref));
        }

        entry->ty = entry_ty;
        *entry->value = *ty_ref;
      }
    } else {
      struct ast_ty_tagged tagged;
      tagged.name = name;

      struct var_table_entry *entry = get_entry(&parser->ty_table, name);
      tagged.underlying = entry ? entry->value : NULL;

      *ty_ref =
          (struct ast_tyref){.ty = AST_TYREF_TY_TAGGED,
                             .function_specifiers = ty_ref->function_specifiers,
                             .type_qualifiers = ty_ref->type_qualifiers,

                             .tagged = tagged};
    }

    return true;
  } else if (type.ty == LEX_TOKEN_TY_KW_ENUM) {
    peek_token(parser->lexer, &token);
    if (token.ty == LEX_TOKEN_TY_IDENTIFIER) {
      name = identifier_str(parser, &token);
      consume_token(parser->lexer, token);
    }

    *ty_ref = (struct ast_tyref){.ty = AST_TYREF_TY_WELL_KNOWN,
                                 .well_known = WELL_KNOWN_TY_SIGNED_INT};

    struct ast_enumdecllist decl_list;
    if (parse_enumdecllist(parser, &decl_list)) {
      if (name) {
        struct var_table_entry *entry =
            get_or_create_entry(&parser->ty_table, name);
        if (!entry->value) {
          entry->value = arena_alloc(parser->arena, sizeof(struct ast_tyref));
        }

        entry->ty = VAR_TABLE_ENTRY_TY_ENUM;
        *entry->value = *ty_ref;
      }
    }

    return true;
  } else if (type.ty == LEX_TOKEN_TY_IDENTIFIER) {
    struct var_table_entry *entry =
        get_entry(&parser->ty_table, identifier_str(parser, &type));

    if (entry && entry->ty == VAR_TABLE_ENTRY_TY_TYPEDEF) {
      *ty_ref = *entry->value;
      return true;
    }
  }

  backtrack(parser->lexer, pos);
  return false;
}

// TODO: unify these

bool parse_abstract_direct_declarator(struct parser *parser,
                                      struct ast_tyref *ty_ref);

bool parse_abstract_declarator(struct parser *parser,
                               struct ast_tyref *ty_ref) {
  while (parse_pointer(parser, ty_ref)) {
    ;
  }

  return parse_abstract_direct_declarator(parser, ty_ref);
}

bool parse_abstract_direct_declarator_modifier(struct parser *parser,
                                               struct ast_tyref *specifier,
                                               struct ast_tyref *ty_ref) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_paramlist param_list;
  if (parse_paramlist(parser, &param_list)) {
    // param type list or identifier list

    ty_ref->ty = AST_TYREF_TY_FUNC;
    ty_ref->func.ret_var_ty =
        arena_alloc(parser->arena, sizeof(*ty_ref->func.ret_var_ty));
    *ty_ref->func.ret_var_ty = *specifier;
    ty_ref->func.num_params = param_list.num_params;
    ty_ref->func.param_identifiers = arena_alloc(
        parser->arena, sizeof(struct token *) * ty_ref->func.num_params);
    ty_ref->func.param_var_tys =
        arena_alloc(parser->arena, sizeof(*ty_ref->func.param_var_tys) *
                                       ty_ref->func.num_params);

    for (size_t i = 0; i < ty_ref->func.num_params; i++) {
      if (param_list.params[i].var) {
        ty_ref->func.param_identifiers[i] =
            &param_list.params[i].var->identifier;
      } else {
        ty_ref->func.param_identifiers[i] = NULL;
      }
      ty_ref->func.param_var_tys[i] = param_list.params[i].var_ty;
    }

    return true;
  } else if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET)) {
    // array
    bool is_static = parse_token(parser, LEX_TOKEN_TY_KW_STATIC);
    // parse_type_qualifiers(parser, &flags);
    is_static = is_static || parse_token(parser, LEX_TOKEN_TY_KW_STATIC);

    size_t size = 0;
    enum ast_ty_array_ty ty;
    struct ast_expr expr;

    if (parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET)) {
      ty = AST_TY_ARRAY_TY_UNKNOWN_SIZE;
    } else if (parse_expr(parser, &expr) &&
               parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET)) {
      ty = AST_TY_ARRAY_TY_KNOWN_SIZE;
      size = resolve_constant_expr(parser, &expr);
    } else {
      backtrack(parser->lexer, pos);
      return false;
    }

    struct ast_tyref *underlying =
        arena_alloc(parser->arena, sizeof(*underlying));
    *underlying = *ty_ref;

    ty_ref->ty = AST_TYREF_TY_ARRAY;
    ty_ref->array =
        (struct ast_ty_array){.ty = ty, .element = underlying, .size = size};
    return true;
  } else {
    backtrack(parser->lexer, pos);
    return false;
  }
}

bool parse_direct_declarator(struct parser *parser, struct token *identifier,
                             struct ast_tyref *ty_ref);

bool parse_declarator(struct parser *parser, struct token *identifier,
                      struct ast_tyref *ty_ref) {
  while (parse_pointer(parser, ty_ref)) {
    ;
  }

  return parse_direct_declarator(parser, identifier, ty_ref);
}

bool parse_direct_declarator_modifier(struct parser *parser,
                                      struct ast_tyref *inner,
                                      struct ast_tyref *ty_ref) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_paramlist param_list;
  if (parse_paramlist(parser, &param_list)) {
    // param type list or identifier list

    struct ast_tyref *underlying =
        arena_alloc(parser->arena, sizeof(*underlying));
    *underlying = *ty_ref;

    struct ast_tyref *func_ty = arena_alloc(parser->arena, sizeof(*func_ty));
    func_ty->ty = AST_TYREF_TY_FUNC;
    func_ty->func.ret_var_ty =
        arena_alloc(parser->arena, sizeof(*func_ty->func.ret_var_ty));
    func_ty->func.ret_var_ty = underlying;
    func_ty->func.num_params = param_list.num_params;
    func_ty->func.param_identifiers = arena_alloc(
        parser->arena, sizeof(struct token *) * func_ty->func.num_params);
    func_ty->func.param_var_tys =
        arena_alloc(parser->arena, sizeof(*func_ty->func.param_var_tys) *
                                       func_ty->func.num_params);

    for (size_t i = 0; i < func_ty->func.num_params; i++) {
      if (param_list.params[i].var) {
        func_ty->func.param_identifiers[i] =
            &param_list.params[i].var->identifier;
      } else {
        func_ty->func.param_identifiers[i] = NULL;
      }
      func_ty->func.param_var_tys[i] = param_list.params[i].var_ty;
    }

    if (!inner) {
      *ty_ref = *func_ty;
    } else {
      if (inner->ty == AST_TYREF_TY_POINTER) {
        inner->pointer.underlying = func_ty;
        *ty_ref = *inner;
      } else if (inner->ty == AST_TYREF_TY_ARRAY) {
        inner->array.element = func_ty;
        *ty_ref = *inner;
      } else if (inner->ty == AST_TYREF_TY_FUNC) {
        inner->func.ret_var_ty = func_ty;
        *ty_ref = *inner;
      }
    }

    return true;
  } else if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET)) {
    // array
    bool is_static = parse_token(parser, LEX_TOKEN_TY_KW_STATIC);
    // parse_type_qualifiers(parser, &flags);
    is_static = is_static || parse_token(parser, LEX_TOKEN_TY_KW_STATIC);

    size_t size = 0;
    enum ast_ty_array_ty ty;
    struct ast_expr expr;

    if (parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET)) {
      ty = AST_TY_ARRAY_TY_UNKNOWN_SIZE;
    } else if (parse_expr(parser, &expr) &&
               parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET)) {
      ty = AST_TY_ARRAY_TY_KNOWN_SIZE;
      size = resolve_constant_expr(parser, &expr);
    } else {
      backtrack(parser->lexer, pos);
      return false;
    }

    struct ast_tyref *underlying =
        arena_alloc(parser->arena, sizeof(*underlying));
    *underlying = *ty_ref;

    ty_ref->ty = AST_TYREF_TY_ARRAY;
    ty_ref->array =
        (struct ast_ty_array){.ty = ty, .element = underlying, .size = size};

    return true;
  } else {
    backtrack(parser->lexer, pos);
    return false;
  }
}

bool parse_direct_declarator(struct parser *parser, struct token *identifier,
                             struct ast_tyref *ty_ref) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_tyref *inner;
  if (parse_identifier(parser, identifier)) {
    inner = NULL;
  } else {
    inner = arena_alloc(parser->arena, sizeof(*inner));
    if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
        parse_declarator(parser, identifier, inner) &&
        parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
      ;
    } else {
      backtrack(parser->lexer, pos);
      return false;
    }
  }

  while (parse_direct_declarator_modifier(parser, inner, ty_ref)) {
    ;
  }

  return true;
}

bool parse_abstract_direct_declarator(struct parser *parser,
                                      struct ast_tyref *ty_ref) {
  struct ast_tyref *inner = arena_alloc(parser->arena, sizeof(*inner));
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_abstract_declarator(parser, inner) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    ;
  }

  while (parse_direct_declarator_modifier(parser, inner, ty_ref)) {
    ;
  }

  return true;
}

bool parse_funcdef(struct parser *parser, struct ast_funcdef *func_def) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_tyref func_ty;
  if (!parse_type_specifier(parser, &func_ty,
                            &func_def->storage_class_specifiers)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct token identifier;
  if (!parse_declarator(parser, &identifier, &func_ty)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (func_ty.ty != AST_TYREF_TY_FUNC) {
    backtrack(parser->lexer, pos);
    return false;
  }

  // FIXME: not sure if shadowing logic works here
  struct var_table_entry *entry = get_or_create_entry(
      &parser->var_table, identifier_str(parser, &identifier));
  entry->value = arena_alloc(parser->arena, sizeof(*entry->value));
  *entry->value = func_ty;

  func_def->var_ty = func_ty;

  // struct ast_decllist decl_list;
  // if (parse_decl_list(parser, &decl_list)) {
  //   todo("func with decl list");
  // }

  parser_push_scope(parser);

  // need to add the params to the locals table
  for (size_t i = 0; i < func_ty.func.num_params; i++) {
    const struct ast_tyref *param = &func_ty.func.param_var_tys[i];
    struct token *identifier = func_ty.func.param_identifiers[i];

    if (!identifier) {
      continue;
    }

    struct ast_var var;
    var.identifier = *identifier;
    var.scope = cur_scope(&parser->var_table);
    var.var_ty = *param;

    const char *name = identifier_str(parser, &var.identifier);
    struct var_table_entry *entry = create_entry(&parser->var_table, name);
    entry->value = arena_alloc(parser->arena, sizeof(struct ast_tyref));
    *entry->value = var.var_ty;
  }

  // return statements need to know the type they coerce to
  parser->func_ret_ty = *func_ty.func.ret_var_ty;

  struct ast_compoundstmt func_body;
  if (parse_compoundstmt(parser, &func_body)) {
    func_def->identifier = identifier;
    func_def->body = func_body;

    parser_pop_scope(parser);
    return true;
  }

  parser_pop_scope(parser);
  backtrack(parser->lexer, pos);
  return false;
}

#define TOKEN_FMT(lexer, token)                                                \
  text_pos_len((token).start, (token).end),                                    \
      text_pos_len((token).start, (token).end), lexer->text[(token).start.idx]

const char *identifier_str(struct parser *parser, const struct token *token) {
  return associated_text(parser->lexer, token);
}

bool parse_enumcnst(struct parser *parser, struct ast_enumcnst *enum_cnst) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);
  if (token.ty != LEX_TOKEN_TY_IDENTIFIER) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, token);

  enum_cnst->ty = AST_ENUMCNST_TY_IMPLICIT_VALUE;
  enum_cnst->identifier = token;

  struct ast_cnst cnst;
  if (parse_token(parser, LEX_TOKEN_TY_OP_ASSG)) {
    if (!parse_int_cnst(parser, &cnst)) {
      backtrack(parser->lexer, pos);
      return false;
    }

    enum_cnst->ty = AST_ENUMCNST_TY_EXPLICIT_VALUE;
    enum_cnst->value = cnst.int_value;
  }

  return true;
}

bool parse_anonymousdecl(struct parser *parser,
                         struct ast_structdecl *field_list) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TY_KW_STRUCT) {
    field_list->ty = AST_STRUCTDECL_TY_STRUCT;
  } else if (token.ty == LEX_TOKEN_TY_KW_UNION) {
    field_list->ty = AST_STRUCTDECL_TY_STRUCT;
  } else {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, token);

  struct ast_structdecllist decl_list;
  if (!parse_structdecllist(parser, &decl_list)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct ast_tyref var_ty = make_aggregate_ty(parser, token, NULL, &decl_list,
                                              AST_FUNCTION_SPECIFIER_FLAG_NONE,
                                              AST_TYPE_QUALIFIER_FLAG_NONE);

  field_list->num_fields = 1;
  field_list->fields = arena_alloc(parser->arena, sizeof(struct ast_decl));
  field_list->fields[0] =
      (struct ast_decl){.ty = AST_DECL_TY_ANON_DECL, .var = {.var_ty = var_ty}};

  if (!parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

bool parse_fieldlist(struct parser *parser, struct ast_structdecl *field_list) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_decllist decl_list;
  if (parse_decllist(parser, &decl_list)) {
    field_list->num_fields = decl_list.num_decls;
    field_list->fields = decl_list.decls;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_enumdecllist(struct parser *parser,
                        struct ast_enumdecllist *enum_decl_list) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct vector *cnsts = vector_create(sizeof(struct ast_enumcnst));

  int enum_value = 0;

  while (true) {
    struct ast_enumcnst enum_cnst;
    if (!parse_enumcnst(parser, &enum_cnst)) {
      break;
    }

    switch (enum_cnst.ty) {
    case AST_ENUMCNST_TY_EXPLICIT_VALUE:
      enum_value = enum_cnst.value;
      break;
    case AST_ENUMCNST_TY_IMPLICIT_VALUE:
      break;
    }

    // need to add the enum cnst into the var table
    const char *name = identifier_str(parser, &enum_cnst.identifier);
    struct var_table_entry *entry = create_entry(&parser->var_table, name);

    entry->ty = VAR_TABLE_ENTRY_TY_ENUM_CNST;
    entry->value = arena_alloc(parser->arena, sizeof(struct ast_tyref));
    *entry->value = (struct ast_tyref){.ty = AST_TYREF_TY_WELL_KNOWN,
                                       .well_known = WELL_KNOWN_TY_SIGNED_INT};
    entry->enum_cnst = enum_value++;

    vector_push_back(cnsts, &enum_cnst);

    if (!parse_token(parser, LEX_TOKEN_TY_COMMA)) {
      break;
    }
  }

  if (parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE)) {
    enum_decl_list->num_enum_cnsts = vector_length(cnsts);
    enum_decl_list->enum_cnsts =
        arena_alloc(parser->arena, vector_byte_size(cnsts));
    vector_copy_to(cnsts, enum_decl_list->enum_cnsts);

    debug_print_entries(stderr, &parser->var_table, NULL, NULL);

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_structdecllist(struct parser *parser,
                          struct ast_structdecllist *type_def) {
  struct text_pos pos = get_position(parser->lexer);

  type_def->num_struct_decls = 0;
  type_def->struct_decls = NULL;

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  parser_push_scope(parser);

  struct vector *struct_decls = vector_create(sizeof(struct ast_structdecl));
  {
    struct ast_structdecl struct_decl;
    while (parse_anonymousdecl(parser, &struct_decl) ||
           parse_fieldlist(parser, &struct_decl)) {
      vector_push_back(struct_decls, &struct_decl);
    }
  }

  type_def->struct_decls =
      arena_alloc(parser->arena, vector_byte_size(struct_decls));
  type_def->num_struct_decls = vector_length(struct_decls);
  vector_copy_to(struct_decls, type_def->struct_decls);
  vector_free(&struct_decls);

  parser_pop_scope(parser);
  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

bool parse_aggregatedecl(struct parser *parser,
                         struct ast_aggregatedecl *type_decl) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_STRUCT)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct token token;
  peek_token(parser->lexer, &token);
  if (token.ty != LEX_TOKEN_TY_IDENTIFIER) {
    backtrack(parser->lexer, pos);
    return false;
  };

  consume_token(parser->lexer, token);

  type_decl->name = token;

  if (!parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

struct parse_result parse(struct parser *parser) {
  struct lexer *lexer = parser->lexer;

  struct vector *func_defs = vector_create(sizeof(struct ast_funcdef));
  struct vector *declarations = vector_create(sizeof(struct ast_decllist));

  while (true) {
    if (lexer_at_eof(lexer)) {
      info("EOF reached by lexer");
      break;
    }

    struct ast_funcdef func_def;
    if (parse_funcdef(parser, &func_def)) {
      vector_push_back(func_defs, &func_def);
      continue;
    }

    struct text_pos pos = get_position(parser->lexer);

    struct ast_decllist var_decl_list;
    if (parse_decllist(parser, &var_decl_list)) {
      vector_push_back(declarations, &var_decl_list);
      continue;
    }
    backtrack(parser->lexer, pos);

    if (!lexer_at_eof(lexer)) {
      // parser failed
      struct text_pos pos = get_position(lexer);
      err("parser finished at position %zu, line=%zu, col=%zu", pos.idx,
          pos.line, pos.col);
      break;
    }

    bug("parser hit nothing");
  }

  struct ast_translationunit translation_unit;

  translation_unit.func_defs = nonnull_malloc(vector_byte_size(func_defs));
  translation_unit.num_func_defs = vector_length(func_defs);
  vector_copy_to(func_defs, translation_unit.func_defs);
  vector_free(&func_defs);

  translation_unit.decl_lists = nonnull_malloc(vector_byte_size(declarations));
  translation_unit.num_decl_lists = vector_length(declarations);
  vector_copy_to(declarations, translation_unit.decl_lists);
  vector_free(&declarations);

  struct parse_result result = {.translation_unit = translation_unit};

  return result;
}

struct ast_printstate {
  struct parser *parser;
  int indent;

  struct graphwriter *gwr;
};

#define DEBUG_FUNC_ENUM(ty, name)                                              \
  void parse_debug_print_##ty(struct ast_printstate *state, enum ast_##ty *name)

#define DEBUG_FUNC(ty, name)                                                   \
  void parse_debug_print_##ty(struct ast_printstate *state,                    \
                              struct ast_##ty *name)
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
    debug_assert(state->indent >= 0, "indent negative!");                      \
  } while (0);

#define PUSH_INDENT()                                                          \
  int tmp_indent = state->indent;                                              \
  state->indent = 0;
#define POP_INDENT() state->indent = tmp_indent;

DEBUG_FUNC_ENUM(storage_class_specifier_flags, storage_class_specifier_flags) {
  UNUSED_ARG(state);

  if (*storage_class_specifier_flags &
      AST_STORAGE_CLASS_SPECIFIER_FLAG_TYPEDEF) {
    AST_PRINTZ("TYPEDEF");
  }

  if (*storage_class_specifier_flags & AST_STORAGE_CLASS_SPECIFIER_FLAG_AUTO) {
    AST_PRINTZ("AUTO");
  }

  if (*storage_class_specifier_flags &
      AST_STORAGE_CLASS_SPECIFIER_FLAG_EXTERN) {
    AST_PRINTZ("EXTERN");
  }

  if (*storage_class_specifier_flags &
      AST_STORAGE_CLASS_SPECIFIER_FLAG_REGISTER) {
    AST_PRINTZ("REGISTER");
  }

  if (*storage_class_specifier_flags &
      AST_STORAGE_CLASS_SPECIFIER_FLAG_STATIC) {
    AST_PRINTZ("STATIC");
  }
}

DEBUG_FUNC_ENUM(type_qualifier_flags, type_qualifier_flags) {
  UNUSED_ARG(state);

  if (*type_qualifier_flags & AST_TYPE_QUALIFIER_FLAG_CONST) {
    AST_PRINTZ("CONST");
  }

  if (*type_qualifier_flags & AST_TYPE_QUALIFIER_FLAG_VOLATILE) {
    AST_PRINTZ("VOLATILE");
  }
}

DEBUG_FUNC(tyref, ty_ref) {
  DEBUG_CALL(type_qualifier_flags, &ty_ref->type_qualifiers);

  switch (ty_ref->ty) {
  case AST_TYREF_TY_UNKNOWN:
    AST_PRINTZ("<unresolved type>");
    break;
  case AST_TYREF_TY_TAGGED:
    if (ty_ref->tagged.underlying) {
      AST_PRINT("TAGGED %s", ty_ref->tagged.name);
    } else {
      AST_PRINT("TAGGED %s (undf)", ty_ref->tagged.name);
    }
    break;
  case AST_TYREF_TY_AGGREGATE:
    switch (ty_ref->aggregate.ty) {
    case AST_TY_AGGREGATE_TY_STRUCT:
      AST_PRINTZ("STRUCT ");
      break;
    case AST_TY_AGGREGATE_TY_UNION:
      AST_PRINTZ("UNION ");
      break;
    }
    INDENT();
    for (size_t i = 0; i < ty_ref->aggregate.num_field_var_tys; i++) {
      AST_PRINT("FIELD %s", ty_ref->aggregate.field_var_tys[i].name);
      INDENT();
      DEBUG_CALL(tyref, ty_ref->aggregate.field_var_tys[i].var_ty);
      UNINDENT();
    }
    UNINDENT();
    break;
  case AST_TYREF_TY_VOID:
    AST_PRINTZ("VOID");
    break;
  case AST_TYREF_TY_VARIADIC:
    AST_PRINTZ("VARIADIC");
    break;
  case AST_TYREF_TY_ARRAY:
    if (ty_ref->array.ty == AST_TYREF_TY_UNKNOWN) {
      AST_PRINTZ("ARRAY UNKNOWN SIZE OF");
    } else {
      AST_PRINT("ARRAY SIZE %llu OF", ty_ref->array.size);
    }
    INDENT();
    DEBUG_CALL(tyref, ty_ref->array.element);
    UNINDENT();
    break;
  case AST_TYREF_TY_POINTER:
    AST_PRINTZ("POINTER TO");
    INDENT();
    DEBUG_CALL(tyref, ty_ref->pointer.underlying);
    UNINDENT();
    break;
  case AST_TYREF_TY_FUNC:
    AST_PRINTZ("FUNC (");
    INDENT();
    for (size_t i = 0; i < ty_ref->func.num_params; i++) {
      DEBUG_CALL(tyref, &ty_ref->func.param_var_tys[i]);
    }
    UNINDENT();
    AST_PRINTZ(")");

    AST_PRINT_SAMELINE_Z(" -> ");
    DEBUG_CALL(tyref, ty_ref->func.ret_var_ty);

    break;
  case AST_TYREF_TY_WELL_KNOWN:
    switch (ty_ref->well_known) {
    case WELL_KNOWN_TY_SIGNED_CHAR:
      AST_PRINTZ("signed char");
      break;
    case WELL_KNOWN_TY_UNSIGNED_CHAR:
      AST_PRINTZ("unsigned char");
      break;
    case WELL_KNOWN_TY_SIGNED_SHORT:
      AST_PRINTZ("short");
      break;
    case WELL_KNOWN_TY_UNSIGNED_SHORT:
      AST_PRINTZ("unsigned short");
      break;
    case WELL_KNOWN_TY_SIGNED_INT:
      AST_PRINTZ("int");
      break;
    case WELL_KNOWN_TY_UNSIGNED_INT:
      AST_PRINTZ("unsigned int");
      break;
    case WELL_KNOWN_TY_SIGNED_LONG:
      AST_PRINTZ("long");
      break;
    case WELL_KNOWN_TY_UNSIGNED_LONG:
      AST_PRINTZ("unsigned long");
      break;
    case WELL_KNOWN_TY_SIGNED_LONG_LONG:
      AST_PRINTZ("long long");
      break;
    case WELL_KNOWN_TY_UNSIGNED_LONG_LONG:
      AST_PRINTZ("unsigned long long");
      break;
    case WELL_KNOWN_TY_FLOAT:
      AST_PRINTZ("float");
      break;
    case WELL_KNOWN_TY_DOUBLE:
      AST_PRINTZ("double");
      break;
    case WELL_KNOWN_TY_LONG_DOUBLE:
      AST_PRINTZ("long double");
      break;
    }

    break;
    // case AST_TYREF_TY_TYPEDEF_NAME:
    //   <#code#>
    //   break;
    // case AST_TYREF_TY_STRUCT:
    //   <#code#>
    //   break;
    // case AST_TYREF_TY_UNION:
    //   <#code#>
    //   break;
    // case AST_TYREF_TY_ENUM:
    //   <#code#>
    //   break;
  }
}

DEBUG_FUNC(compoundstmt, compound_stmt);
DEBUG_FUNC(expr, expr);

DEBUG_FUNC(var, var) {
  AST_PRINT("VARIABLE '%s' SCOPE %d",
            associated_text(state->parser->lexer, &var->identifier),
            var->scope);

  switch (var->ty) {
  case AST_VAR_TY_VAR:
    break;
  case AST_VAR_TY_ENUM_CNST:
    AST_PRINT("VALUE '%d'", var->enum_cnst);
  }

  DEBUG_CALL(tyref, &var->var_ty);
}

DEBUG_FUNC(cnst, cnst) {
  if (is_integral_ty(&cnst->cnst_ty)) {
    AST_PRINT("CONSTANT '%llu'", cnst->int_value);
  } else if (is_fp_ty(&cnst->cnst_ty)) {
    AST_PRINT("CONSTANT '%Lf'", cnst->flt_value);
  } else {
    // must be string literal for now
    AST_PRINT("CONSTANT '%s'", cnst->str_value);
  }
}

DEBUG_FUNC(compoundexpr, compound_expr);

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
  case AST_UNARY_OP_TY_SIZEOF:
    AST_PRINTZ("SIZEOF");
    break;
  case AST_UNARY_OP_TY_ADDRESSOF:
    AST_PRINTZ("ADDRESSOF");
    break;
  case AST_UNARY_OP_TY_ALIGNOF:
    AST_PRINTZ("ALIGNOF");
    break;
  case AST_UNARY_OP_TY_CAST:
    AST_PRINTZ("CAST");
    INDENT();
    DEBUG_CALL(tyref, &unary_op->expr->var_ty);
    AST_PRINTZ("TO");
    DEBUG_CALL(tyref, &unary_op->cast.cast_ty);
    UNINDENT();
    break;
  }

  INDENT();
  DEBUG_CALL(expr, unary_op->expr);
  UNINDENT();
}

DEBUG_FUNC(binary_op, binary_op) {
  INDENT();
  DEBUG_CALL(tyref, &binary_op->intermediate_var_ty);
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
  case AST_BINARY_OP_TY_QUOT:
    AST_PRINTZ("QUOT");
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
  case AST_ASSG_TY_SIMPLEASSG:
    AST_PRINTZ("=");
    break;
  case AST_ASSG_TY_COMPOUNDASSG:
    switch (assg->compound_assg.binary_op_ty) {
    case AST_BINARY_OP_TY_ADD:
      AST_PRINTZ("+=");
      break;
    case AST_BINARY_OP_TY_SUB:
      AST_PRINTZ("-=");
      break;
    case AST_BINARY_OP_TY_MUL:
      AST_PRINTZ("*=");
      break;
    case AST_BINARY_OP_TY_DIV:
      AST_PRINTZ("/=");
      break;
    case AST_BINARY_OP_TY_QUOT:
      AST_PRINTZ("%%=");
      break;
    case AST_BINARY_OP_TY_LSHIFT:
      AST_PRINTZ("<<=");
      break;
    case AST_BINARY_OP_TY_RSHIFT:
      AST_PRINTZ(">>=");
      break;
    case AST_BINARY_OP_TY_AND:
      AST_PRINTZ("&=");
      break;
    case AST_BINARY_OP_TY_OR:
      AST_PRINTZ("|=");
      break;
    case AST_BINARY_OP_TY_XOR:
      AST_PRINTZ("^=");
      break;
    default:
      bug("unrecognised binary op ty in assg");
    }

    DEBUG_CALL(tyref, &assg->compound_assg.intermediate_var_ty);
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
  DEBUG_CALL(tyref, &call->var_ty);
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
  AST_PRINT("%s", identifier_str(state->parser, &pointer_access->member));
  UNINDENT();
}

DEBUG_FUNC(memberaccess, member_access) {
  AST_PRINTZ("MEMBER_ACCESS");

  INDENT();
  DEBUG_CALL(expr, member_access->lhs);
  UNINDENT();

  AST_PRINTZ("MEMBER");

  INDENT();
  AST_PRINT("%s", identifier_str(state->parser, &member_access->member));
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
  switch (designator->ty) {
  case AST_DESIGNATOR_TY_FIELD:
    AST_PRINT("FIELD '%s'", identifier_str(state->parser, &designator->field));
    break;
  case AST_DESIGNATOR_TY_INDEX:
    AST_PRINT("INDEX '%zu'", designator->index);
    break;
  }

  if (designator->next) {
    DEBUG_CALL(designator, designator->next);
  }
}

DEBUG_FUNC(init, init) {
  AST_PRINTZ("INIT");

  if (init->designator) {
    INDENT();
    DEBUG_CALL(designator, init->designator);
    UNINDENT();
  }

  AST_PRINTZ("EXPR");

  INDENT();
  DEBUG_CALL(expr, init->expr);
  UNINDENT();
}

DEBUG_FUNC(initlist, init_list) {
  AST_PRINTZ("INIT LIST");

  INDENT();
  for (size_t i = 0; i < init_list->num_inits; i++) {
    DEBUG_CALL(init, &init_list->inits[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(sizeof, size_of) {
  AST_PRINTZ("SIZEOF");

  INDENT();
  switch (size_of->ty) {
  case AST_SIZEOF_TY_TYPE:
    AST_PRINTZ("TYPE");
    DEBUG_CALL(tyref, &size_of->ty_ref);
    break;
  case AST_SIZEOF_TY_EXPR:
    AST_PRINTZ("EXPR");
    DEBUG_CALL(expr, size_of->expr);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(alignof, align_of) {
  AST_PRINTZ("ALIGNOF");

  INDENT();
  AST_PRINTZ("TYPE");
  DEBUG_CALL(tyref, &align_of->ty_ref);
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

DEBUG_FUNC(expr, expr) {
  AST_PRINTZ("EXPRESSION");

  INDENT();
  DEBUG_CALL(tyref, &expr->var_ty);
  switch (expr->ty) {
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
  case AST_EXPR_TY_INIT_LIST:
    DEBUG_CALL(initlist, &expr->init_list);
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

DEBUG_FUNC(decl, decl) {
  DEBUG_CALL(var, &decl->var);

  if (decl->ty == AST_DECL_TY_DECL_WITH_ASSG) {
    AST_PRINTZ("WITH ASSIGNMENT");

    INDENT();
    DEBUG_CALL(expr, &decl->assg_expr);
    UNINDENT();
  }
}

DEBUG_FUNC(decllist, decl_list) {
  AST_PRINTZ("DECLARATION");
  INDENT();

  DEBUG_CALL(storage_class_specifier_flags,
             &decl_list->storage_class_specifiers);

  for (size_t i = 0; i < decl_list->num_decls; i++) {
    DEBUG_CALL(decl, &decl_list->decls[i]);
  }

  UNINDENT();
}

DEBUG_FUNC(jumpstmt, jump_stmt) {
  switch (jump_stmt->ty) {
  case AST_JUMPSTMT_TY_RETURN:
    AST_PRINTZ("RETURN");
    DEBUG_CALL(tyref, &jump_stmt->return_stmt.var_ty);
    INDENT();
    if (jump_stmt->return_stmt.expr) {
      DEBUG_CALL(expr, jump_stmt->return_stmt.expr);
    }
    UNINDENT();
    break;
  case AST_JUMPSTMT_TY_GOTO:
    AST_PRINT("GOTO %s",
              identifier_str(state->parser, &jump_stmt->goto_stmt.label));
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
  DEBUG_CALL(expr, &if_stmt->condition);
  UNINDENT();

  AST_PRINTZ("BODY");
  DEBUG_CALL(stmt, if_stmt->body);
}

DEBUG_FUNC(ifelsestmt, if_else_stmt) {
  AST_PRINTZ("IF");
  AST_PRINTZ("CONDITION");
  INDENT();
  DEBUG_CALL(expr, &if_else_stmt->condition);
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

DEBUG_FUNC(decllist, decl_list);

DEBUG_FUNC(declorexpr, decl_or_expr) {
  if (decl_or_expr->decl) {
    DEBUG_CALL(decllist, decl_or_expr->decl);
  } else if (decl_or_expr->expr) {
    DEBUG_CALL(expr, decl_or_expr->expr);
  }
}

DEBUG_FUNC(forstmt, for_stmt) {
  AST_PRINTZ("FOR");
  AST_PRINTZ("INIT");
  INDENT();
  if (for_stmt->init) {
    DEBUG_CALL(declorexpr, for_stmt->init);
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
    AST_PRINT("LABEL %s", identifier_str(state->parser, &labeled_stmt->label));
    break;
  case AST_LABELEDSTMT_TY_CASE:
    AST_PRINT("CASE %llu", labeled_stmt->cnst);
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

DEBUG_FUNC(stmt, stmt) {
  INDENT();

  switch (stmt->ty) {
  case AST_STMT_TY_NULL:
    break;
  case AST_STMT_TY_DECL_LIST:
    DEBUG_CALL(decllist, &stmt->decl_list);
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
  AST_PRINT_SAMELINE_Z("PARAM ");
  DEBUG_CALL(tyref, &param->var_ty);
  AST_PRINT(" '%s' SCOPE %d",
            associated_text(state->parser->lexer, &param->var->identifier),
            param->var->scope);
}

DEBUG_FUNC(paramlist, param_list) {
  for (size_t i = 0; i < param_list->num_params; i++) {
    DEBUG_CALL(param, &param_list->params[i]);
  }
}

DEBUG_FUNC(funcsig, func_sig) {
  AST_PRINT("'%s'", associated_text(state->parser->lexer, &func_sig->name));
  AST_PRINTZ("RETURNS: ");

  INDENT();
  DEBUG_CALL(tyref, func_sig->var_ty.func.ret_var_ty);
  UNINDENT();

  DEBUG_CALL(paramlist, &func_sig->param_list);
}

DEBUG_FUNC(funcdef, func_def) {
  AST_PRINT_SAMELINE_Z("FUNCTION DEFINITION ");
  // DEBUG_CALL(funcsig, &func_def->sig);

  DEBUG_CALL(compoundstmt, &func_def->body);
}

DEBUG_FUNC(field, field) {
  AST_PRINTZ("FIELD ");
  AST_PRINT("NAME %s ",
            associated_text(state->parser->lexer, &field->identifier));
  DEBUG_CALL(tyref, &field->var_ty);
  UNINDENT();
}

DEBUG_FUNC(structdecl, struct_decl) {
  AST_PRINTZ("STRUCT DECL");
  INDENT();
  for (size_t i = 0; i < struct_decl->num_fields; i++) {
    DEBUG_CALL(decl, &struct_decl->fields[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(structdecllist, struct_decl_list) {
  AST_PRINTZ("STRUCT DECL LIST");
  INDENT();
  for (size_t i = 0; i < struct_decl_list->num_struct_decls; i++) {
    DEBUG_CALL(structdecl, &struct_decl_list->struct_decls[i]);
  }
  UNINDENT();
}

DEBUG_FUNC(aggregatedecl, aggregate_decl) {
  switch (aggregate_decl->ty) {
  case AST_TYPE_TY_UNION:
    AST_PRINTZ("UNION DECLARATION ");
    break;
  case AST_TYPE_TY_STRUCT:
    AST_PRINTZ("STRUCT DECLARATION ");
    break;
  }
  AST_PRINT("NAME %s ",
            associated_text(state->parser->lexer, &aggregate_decl->name));
}

void debug_print_ast(struct parser *parser,
                     struct ast_translationunit *translation_unit) {
  struct ast_printstate state_ = {.indent = 0, .parser = parser};

  struct ast_printstate *state = &state_;

  AST_PRINTZ("PRINTING AST");

  for (size_t i = 0; i < translation_unit->num_func_defs; i++) {
    DEBUG_CALL(funcdef, &translation_unit->func_defs[i]);
  }

  for (size_t i = 0; i < translation_unit->num_decl_lists; i++) {
    DEBUG_CALL(decllist, &translation_unit->decl_lists[i]);
  }
}
