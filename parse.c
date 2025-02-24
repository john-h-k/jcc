#include "parse.h"

#include "alloc.h"
#include "lex.h"
#include "log.h"
#include "program.h"
#include "util.h"
#include "var_table.h"
#include "vector.h"

#include <ctype.h>
#include <string.h>

#define EXP_PARSE(e, diag)                                                     \
  do {                                                                         \
    if (!(e)) {                                                                \
      BUG("failure during parsing: %s", diag);                                 \
    }                                                                          \
  } while (0)

struct parser {
  struct arena_allocator *arena;
  struct lexer *lexer;

  struct var_table ty_table;

  struct vector *diagnostics;
};

enum parser_create_result parser_create(struct program *program,
                                        struct preproc *preproc,
                                        struct parser **parser) {
  struct parser *p = nonnull_malloc(sizeof(*p));

  arena_allocator_create(&p->arena);
  if (lexer_create(program, preproc, &p->lexer) != LEX_CREATE_RESULT_SUCCESS) {
    err("failed to create lexer");
    return PARSER_CREATE_RESULT_FAILURE;
  }

  p->ty_table = var_table_create(p->arena);

  *parser = p;

  return PARSER_CREATE_RESULT_SUCCESS;
}

void parser_free(struct parser **parser) {
  lexer_free(&(*parser)->lexer);
  var_table_free(&(*parser)->ty_table);

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
  case AST_BINARY_OP_TY_QUOT:
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
  case LEX_TOKEN_TY_OP_QUOT:
    *info = op_info(AST_BINARY_OP_TY_QUOT);
    return true;
  default:
    // not an op
    return false;
  }
}

static bool parse_token(struct parser *parser, enum lex_token_ty ty) {
  struct lex_token token;

  peek_token(parser->lexer, &token);
  if (token.ty == ty) {
    consume_token(parser->lexer, token);
    return true;
  }

  return false;
}

static bool parse_identifier(struct parser *parser, struct lex_token *token) {
  struct text_pos pos = get_position(parser->lexer);

  peek_token(parser->lexer, token);

  if (token->ty == LEX_TOKEN_TY_IDENTIFIER) {
    consume_token(parser->lexer, *token);

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static bool parse_type_qualifier(struct parser *parser,
                                 enum ast_type_qualifier *qualifier) {
  struct lex_token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TY_KW_CONST) {
    *qualifier = AST_TYPE_QUALIFIER_CONST;
  } else if (token.ty == LEX_TOKEN_TY_KW_VOLATILE) {
    *qualifier = AST_TYPE_QUALIFIER_VOLATILE;
  } else if (token.ty == LEX_TOKEN_TY_KW_RESTRICT) {
    *qualifier = AST_TYPE_QUALIFIER_RESTRICT;
  } else {
    return false;
  }

  consume_token(parser->lexer, token);
  return true;
}

static bool parse_function_specifier(struct parser *parser,
                                     enum ast_function_specifier *specifier) {
  struct lex_token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TY_KW_INLINE) {
    *specifier = AST_FUNCTION_SPECIFIER_INLINE;
  } else {
    return false;
  }

  consume_token(parser->lexer, token);
  return true;
}

static bool
parse_storage_class_specifier(struct parser *parser,
                              enum ast_storage_class_specifier *specifier) {
  struct lex_token token;
  peek_token(parser->lexer, &token);

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

  consume_token(parser->lexer, token);
  return true;
}

static bool parse_type_specifier_kw(struct parser *parser,
                                    enum ast_type_specifier_kw *wkt) {
  struct lex_token token;
  peek_token(parser->lexer, &token);

  switch (token.ty) {
  case LEX_TOKEN_TY_KW_VOID:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_VOID;
    return true;
  case LEX_TOKEN_TY_KW_CHAR:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_CHAR;
    return true;
  case LEX_TOKEN_TY_KW_SHORT:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_SHORT;
    return true;
  case LEX_TOKEN_TY_KW_INT:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_INT;
    return true;
  case LEX_TOKEN_TY_KW_LONG:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_LONG;
    return true;
  case LEX_TOKEN_TY_KW_FLOAT:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_FLOAT;
    return true;
  case LEX_TOKEN_TY_KW_DOUBLE:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_DOUBLE;
    return true;
  case LEX_TOKEN_TY_KW_SIGNED:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_SIGNED;
    return true;
  case LEX_TOKEN_TY_KW_UNSIGNED:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_UNSIGNED;
    return true;
  // case LEX_TOKEN_TY_KW_BOOL:
  //   consume_token(parser->lexer, token);
  //   *wkt = AST_TYPE_SPECIFIER_KW_BOOL;
  //   return true;
  // case LEX_TOKEN_TY_KW_COMPLEX:
  //   consume_token(parser->lexer, token);
  //   *wkt = AST_TYPE_SPECIFIER_KW_COMPLEX;
  //   return true;
  case LEX_TOKEN_TY_KW_HALF:
    consume_token(parser->lexer, token);
    *wkt = AST_TYPE_SPECIFIER_KW_HALF;
    return true;
  default:
    return false;
  }
}

static bool parse_expr(struct parser *parser, struct ast_expr *expr);

static bool parse_enumerator(struct parser *parser,
                             struct ast_enumerator *enumerator) {
  if (!parse_identifier(parser, &enumerator->identifier)) {
    return false;
  }

  if (parse_token(parser, LEX_TOKEN_TY_OP_ASSG)) {
    enumerator->value = arena_alloc(parser->arena, sizeof(*enumerator->value));
    EXP_PARSE(parse_expr(parser, enumerator->value),
              "expected expression after = in enum body");
  } else {
    enumerator->value = NULL;
  }

  parse_token(parser, LEX_TOKEN_TY_COMMA);

  return true;
}

static void parse_enumerator_list(struct parser *parser,
                                  struct ast_enumerator_list *enumerator_list) {

  struct vector *list = vector_create(sizeof(*enumerator_list->enumerators));

  struct ast_enumerator enumerator;
  while (parse_enumerator(parser, &enumerator)) {
    parse_token(parser, LEX_TOKEN_TY_COMMA);

    vector_push_back(list, &enumerator);
  }

  parse_token(parser, LEX_TOKEN_TY_COMMA);

  enumerator_list->enumerators =
      arena_alloc(parser->arena, vector_byte_size(list));
  enumerator_list->num_enumerators = vector_length(list);

  vector_copy_to(list, enumerator_list->enumerators);
  vector_free(&list);
}

static bool parse_enum_specifier(struct parser *parser,
                                 struct ast_enum_specifier *enum_specifier) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_ENUM)) {
    return false;
  }

  struct lex_token identifier;
  if (parse_identifier(parser, &identifier)) {
    enum_specifier->identifier =
        arena_alloc(parser->arena, sizeof(*enum_specifier->identifier));
    *enum_specifier->identifier = identifier;
  } else {
    enum_specifier->identifier = NULL;
  }

  struct ast_enumerator_list enumerator_list;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE)) {
    parse_enumerator_list(parser, &enumerator_list);
    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE),
              "expected } after enumerator body");

    enum_specifier->enumerator_list =
        arena_alloc(parser->arena, sizeof(*enum_specifier->enumerator_list));
    *enum_specifier->enumerator_list = enumerator_list;
  } else {
    enum_specifier->enumerator_list = NULL;
  }

  if (!enum_specifier->identifier && !enum_specifier->enumerator_list) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

static void parse_declaration_specifier_list(
    struct parser *parser,
    struct ast_declaration_specifier_list *specifier_list);

static bool parse_declarator(struct parser *parser,
                             struct ast_declarator *declarator);
static void
parse_declaration_list(struct parser *parser,
                       struct ast_declaration_list *declaration_list);

static bool parse_struct_or_union_specifier(
    struct parser *parser,
    struct ast_struct_or_union_specifier *struct_or_union_specifier) {
  struct text_pos pos = get_position(parser->lexer);

  enum ast_struct_or_union_specifier_ty ty;
  if (parse_token(parser, LEX_TOKEN_TY_KW_STRUCT)) {
    ty = AST_STRUCT_OR_UNION_SPECIFIER_TY_STRUCT;
  } else if (parse_token(parser, LEX_TOKEN_TY_KW_UNION)) {
    ty = AST_STRUCT_OR_UNION_SPECIFIER_TY_UNION;
  } else {
    return false;
  }

  struct lex_token identifier;
  if (parse_identifier(parser, &identifier)) {
    struct_or_union_specifier->identifier = arena_alloc(
        parser->arena, sizeof(*struct_or_union_specifier->identifier));
    *struct_or_union_specifier->identifier = identifier;
  } else {
    struct_or_union_specifier->identifier = NULL;
  }

  struct_or_union_specifier->ty = ty;
  struct_or_union_specifier->decl_list = NULL;

  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE)) {
    struct_or_union_specifier->decl_list = arena_alloc(
        parser->arena, sizeof(*struct_or_union_specifier->decl_list));
    parse_declaration_list(parser, struct_or_union_specifier->decl_list);
    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE),
              "expected } after struct_or_union_declaration body");
  }

  if (!struct_or_union_specifier->identifier &&
      !struct_or_union_specifier->decl_list) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

static bool parse_typedef_name(struct parser *parser,
                               struct lex_token *typedef_name) {
  struct lex_token identifier;
  peek_token(parser->lexer, &identifier);

  if (identifier.ty != LEX_TOKEN_TY_IDENTIFIER) {
    return false;
  }

  struct var_table_entry *entry = var_table_get_entry(
      &parser->ty_table, identifier_str(parser, &identifier));
  if (!entry) {
    return false;
  }

  *typedef_name = identifier;

  consume_token(parser->lexer, identifier);
  return true;
}

enum type_specifier_mode {
  TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS,
  TYPE_SPECIFIER_MODE_DISALLOW_TYPEDEFS
};

static bool parse_type_specifier(struct parser *parser,
                                 struct ast_type_specifier *type_specifier,
                                 enum type_specifier_mode mode) {

  if (parse_type_specifier_kw(parser, &type_specifier->type_specifier_kw)) {
    type_specifier->ty = AST_TYPE_SPECIFIER_TY_KW;
    return true;
  }

  if (parse_struct_or_union_specifier(
          parser, &type_specifier->struct_or_union_specifier)) {
    type_specifier->ty = AST_TYPE_SPECIFIER_STRUCT_OR_UNION;
    return true;
  }

  if (parse_enum_specifier(parser, &type_specifier->enum_specifier)) {
    type_specifier->ty = AST_TYPE_SPECIFIER_ENUM;
    return true;
  }

  if (mode == TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS &&
      parse_typedef_name(parser, &type_specifier->typedef_name)) {
    type_specifier->ty = AST_TYPE_SPECIFIER_TYPEDEF_NAME;
    return true;
  }

  return false;
}

static bool parse_decl_specifier(struct parser *parser,
                                 struct ast_declaration_specifier *specifier,
                                 enum type_specifier_mode mode) {
  if (parse_storage_class_specifier(parser,
                                    &specifier->storage_class_specifier)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_STORAGE_CLASS_SPECIFIER;
    return true;
  }

  if (parse_function_specifier(parser, &specifier->function_specifier)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_FUNCTION_SPECIFIER;
    return true;
  }

  if (parse_type_qualifier(parser, &specifier->type_qualifier)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_TYPE_QUALIFIER;
    return true;
  }

  if (parse_type_specifier(parser, &specifier->type_specifier, mode)) {
    specifier->ty = AST_DECL_SPECIFIER_TY_TYPE_SPECIFIER;
    return true;
  }

  return false;
}

static void parse_declaration_specifier_list(
    struct parser *parser,
    struct ast_declaration_specifier_list *specifier_list) {
  struct vector *list = vector_create(sizeof(*specifier_list->decl_specifiers));

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

  enum type_specifier_mode mode = TYPE_SPECIFIER_MODE_ALLOW_TYPEDEFS;

  struct ast_declaration_specifier specifier;
  while (parse_decl_specifier(parser, &specifier, mode)) {
    if (specifier.ty == AST_DECL_SPECIFIER_TY_TYPE_SPECIFIER) {
      mode = TYPE_SPECIFIER_MODE_DISALLOW_TYPEDEFS;
    }

    vector_push_back(list, &specifier);
  }

  specifier_list->decl_specifiers =
      arena_alloc(parser->arena, vector_byte_size(list));
  specifier_list->num_decl_specifiers = vector_length(list);

  vector_copy_to(list, specifier_list->decl_specifiers);
  vector_free(&list);
}

static bool parse_designator(struct parser *parser,
                             struct ast_designator *designator) {
  struct text_pos pos = get_position(parser->lexer);

  if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET)) {
    struct ast_expr expr;
    EXP_PARSE(parse_expr(parser, &expr), "expression after [ in designator");
    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET),
              "] after expression in designator");

    designator->ty = AST_DESIGNATOR_TY_INDEX;
    designator->index = arena_alloc(parser->arena, sizeof(*designator->index));
    *designator->index = expr;

    return true;
  } else if (parse_token(parser, LEX_TOKEN_TY_DOT)) {
    struct lex_token identifier;
    EXP_PARSE(parse_identifier(parser, &identifier),
              "identifier after dot in designator");

    designator->ty = AST_DESIGNATOR_TY_FIELD;
    designator->field = identifier;

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static bool parse_designator_list(struct parser *parser,
                                  struct ast_designator_list *designator_list) {

  struct vector *list = vector_create(sizeof(*designator_list->designators));

  struct ast_designator designator;
  while (parse_designator(parser, &designator)) {
    vector_push_back(list, &designator);
  }

  if (vector_empty(list)) {
    vector_free(&list);
    return false;
  }

  designator_list->designators =
      arena_alloc(parser->arena, vector_byte_size(list));
  designator_list->num_designators = vector_length(list);

  vector_copy_to(list, designator_list->designators);
  vector_free(&list);

  return true;
}

static bool parse_init_list(struct parser *parser,
                            struct ast_init_list *init_list);

static bool parse_init(struct parser *parser, struct ast_init *init) {
  if (parse_init_list(parser, &init->init_list)) {
    init->ty = AST_INIT_TY_INIT_LIST;
    return true;
  } else if (parse_expr(parser, &init->expr)) {
    init->ty = AST_INIT_TY_EXPR;
    return true;
  }

  return false;
}

static bool parse_init_list_init(struct parser *parser,
                                 struct ast_init_list_init *init_list) {
  struct text_pos pos = get_position(parser->lexer);

  *init_list =
      (struct ast_init_list_init){.designator_list = NULL, .init = NULL};

  struct ast_designator_list designator_list;
  struct ast_init init;
  if (parse_designator_list(parser, &designator_list)) {
    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_OP_ASSG),
              "expected = after designator in init list");

    init_list->designator_list =
        arena_alloc(parser->arena, sizeof(*init_list->designator_list));
    *init_list->designator_list = designator_list;

    EXP_PARSE(parse_init(parser, &init), "expected init in init list");
  } else if (!parse_init(parser, &init)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  parse_token(parser, LEX_TOKEN_TY_COMMA);

  init_list->init = arena_alloc(parser->arena, sizeof(*init_list->init));
  *init_list->init = init;
  return true;
}

static bool parse_init_list(struct parser *parser,
                            struct ast_init_list *init_list) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct vector *inits = vector_create(sizeof(struct ast_init_list_init));

  struct ast_init_list_init init;
  while (parse_init_list_init(parser, &init)) {
    vector_push_back(inits, &init);

    parse_token(parser, LEX_TOKEN_TY_COMMA);
  }

  EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE),
            "expected } at end of init list");

  init_list->inits = arena_alloc(parser->arena, vector_byte_size(inits));
  init_list->num_inits = vector_length(inits);

  vector_copy_to(inits, init_list->inits);
  vector_free(&inits);

  return true;
}

static bool parse_pointer(struct parser *parser, struct ast_pointer *pointer) {
  if (!parse_token(parser, LEX_TOKEN_TY_OP_MUL)) {
    return false;
  }

  parse_declaration_specifier_list(parser, &pointer->specifier_list);
  return true;
}

static void parse_pointer_list(struct parser *parser,
                               struct ast_pointer_list *pointer_list) {
  struct vector *list = vector_create(sizeof(*pointer_list->pointers));

  struct ast_pointer pointer;
  while (parse_pointer(parser, &pointer)) {
    vector_push_back(list, &pointer);
  }

  pointer_list->pointers = arena_alloc(parser->arena, vector_byte_size(list));
  pointer_list->num_pointers = vector_length(list);

  vector_copy_to(list, pointer_list->pointers);
  vector_free(&list);
}

static bool
parse_ast_array_declarator(struct parser *parser,
                           struct ast_array_declarator *array_declarator) {
  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET)) {
    return false;
  }

  parse_declaration_specifier_list(parser, &array_declarator->specifier_list);

  enum ast_array_declarator_ty ty;
  if (parse_token(parser, LEX_TOKEN_TY_OP_MUL)) {
    ty = AST_ARRAY_DECLARATOR_TY_STAR;
  } else {
    bool is_static = parse_token(parser, LEX_TOKEN_TY_KW_STATIC);

    is_static = is_static || parse_token(parser, LEX_TOKEN_TY_KW_STATIC);

    struct ast_expr size;
    if (is_static) {
      EXP_PARSE(parse_expr(parser, &size),
                "expected expr in static array type");
      ty = AST_ARRAY_DECLARATOR_TY_STATIC_SIZED;
      array_declarator->size =
          arena_alloc(parser->arena, sizeof(*array_declarator->size));
      *array_declarator->size = size;
    } else if (parse_expr(parser, &size)) {
      ty = AST_ARRAY_DECLARATOR_TY_SIZED;
      array_declarator->size =
          arena_alloc(parser->arena, sizeof(*array_declarator->size));
      *array_declarator->size = size;
    } else {
      ty = AST_ARRAY_DECLARATOR_TY_UNSIZED;
    }
  }

  EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET),
            "expected ) after abstract declarator after");

  array_declarator->ty = ty;
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
  return true;
}

static bool
parse_abstract_declarator(struct parser *parser,
                          struct ast_abstract_declarator *abstract_declarator);

static bool parse_direct_abstract_declarator(
    struct parser *parser,
    struct ast_direct_abstract_declarator *direct_abstract_declarator) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_array_declarator array_declarator;
  if (parse_ast_array_declarator(parser, &array_declarator)) {
    direct_abstract_declarator->ty =
        AST_DIRECT_ABSTRACT_DECLARATOR_TY_ARRAY_DECLARATOR;
    direct_abstract_declarator->array_declarator = arena_alloc(
        parser->arena, sizeof(*direct_abstract_declarator->array_declarator));
    *direct_abstract_declarator->array_declarator = array_declarator;
    return true;
  }

  struct ast_func_declarator func_declarator;
  if (parse_ast_func_declarator(parser, &func_declarator)) {
    direct_abstract_declarator->ty =
        AST_DIRECT_ABSTRACT_DECLARATOR_TY_FUNC_DECLARATOR;
    direct_abstract_declarator->func_declarator = arena_alloc(
        parser->arena, sizeof(*direct_abstract_declarator->func_declarator));
    *direct_abstract_declarator->func_declarator = func_declarator;
    return true;
  }

  struct ast_abstract_declarator abstract_declarator;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_abstract_declarator(parser, &abstract_declarator)) {
    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET),
              "expected ) after abstract declarator after");

    direct_abstract_declarator->ty =
        AST_DIRECT_ABSTRACT_DECLARATOR_TY_PAREN_DECLARATOR;
    direct_abstract_declarator->paren_declarator = arena_alloc(
        parser->arena, sizeof(*direct_abstract_declarator->paren_declarator));
    *direct_abstract_declarator->paren_declarator = abstract_declarator;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static void
parse_direct_abstract_declarator_list(struct parser *parser,
                                      struct ast_direct_abstract_declarator_list
                                          *direct_abstract_declarator_list) {
  struct vector *list = vector_create(
      sizeof(*direct_abstract_declarator_list->direct_abstract_declarators));

  struct ast_direct_abstract_declarator direct_abstract_declarator;
  while (
      parse_direct_abstract_declarator(parser, &direct_abstract_declarator)) {
    vector_push_back(list, &direct_abstract_declarator);
  }

  direct_abstract_declarator_list->direct_abstract_declarators =
      arena_alloc(parser->arena, vector_byte_size(list));
  direct_abstract_declarator_list->num_direct_abstract_declarators =
      vector_length(list);

  vector_copy_to(list,
                 direct_abstract_declarator_list->direct_abstract_declarators);
  vector_free(&list);
}

static bool
parse_abstract_declarator(struct parser *parser,
                          struct ast_abstract_declarator *abstract_declarator) {
  struct text_pos pos = get_position(parser->lexer);

  parse_pointer_list(parser, &abstract_declarator->pointer_list);
  parse_direct_abstract_declarator_list(
      parser, &abstract_declarator->direct_abstract_declarator_list);

  if (!abstract_declarator->pointer_list.num_pointers &&
      !abstract_declarator->direct_abstract_declarator_list
           .num_direct_abstract_declarators) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

static bool
parse_direct_declarator(struct parser *parser,
                        struct ast_direct_declarator *direct_declarator) {
  struct text_pos pos = get_position(parser->lexer);

  if (parse_identifier(parser, &direct_declarator->identifier)) {
    direct_declarator->ty = AST_DIRECT_DECLARATOR_TY_IDENTIFIER;
    return true;
  }

  struct ast_array_declarator array_declarator;
  if (parse_ast_array_declarator(parser, &array_declarator)) {
    direct_declarator->ty = AST_DIRECT_DECLARATOR_TY_ARRAY_DECLARATOR;
    direct_declarator->array_declarator = arena_alloc(
        parser->arena, sizeof(*direct_declarator->array_declarator));
    *direct_declarator->array_declarator = array_declarator;
    return true;
  }

  struct ast_func_declarator func_declarator;
  if (parse_ast_func_declarator(parser, &func_declarator)) {
    direct_declarator->ty = AST_DIRECT_DECLARATOR_TY_FUNC_DECLARATOR;
    direct_declarator->func_declarator =
        arena_alloc(parser->arena, sizeof(*direct_declarator->func_declarator));
    *direct_declarator->func_declarator = func_declarator;
    return true;
  }

  struct ast_declarator declarator;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_declarator(parser, &declarator)) {
    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET),
              "expected ) after declarator after");

    direct_declarator->ty = AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR;
    direct_declarator->paren_declarator = arena_alloc(
        parser->arena, sizeof(*direct_declarator->paren_declarator));
    *direct_declarator->paren_declarator = declarator;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

// TODO: both this and abstract_declarator_list should really be a recursive
// type, rather than list because you have to do hacks like check if you haven't
// yet seen a sub decl
static void parse_direct_declarator_list(
    struct parser *parser,
    struct ast_direct_declarator_list *direct_declarator_list) {
  struct vector *list =
      vector_create(sizeof(*direct_declarator_list->direct_declarators));

  struct ast_direct_declarator direct_declarator;

  while (parse_direct_declarator(parser, &direct_declarator)) {
    if (vector_empty(list)) {
      // first must be identifier or sub decl
      if (direct_declarator.ty != AST_DIRECT_DECLARATOR_TY_IDENTIFIER &&
          direct_declarator.ty != AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR) {
        break;
      }
    }

    vector_push_back(list, &direct_declarator);
  }

  direct_declarator_list->direct_declarators =
      arena_alloc(parser->arena, vector_byte_size(list));
  direct_declarator_list->num_direct_declarators = vector_length(list);

  vector_copy_to(list, direct_declarator_list->direct_declarators);
  vector_free(&list);
}

static bool parse_declarator(struct parser *parser,
                             struct ast_declarator *declarator) {
  struct text_pos pos = get_position(parser->lexer);

  parse_pointer_list(parser, &declarator->pointer_list);
  parse_direct_declarator_list(parser, &declarator->direct_declarator_list);

  bool has_declarator =
      declarator->direct_declarator_list.num_direct_declarators;

  struct text_pos end_of_declarator = get_position(parser->lexer);

  struct ast_expr expr;
  if (parse_token(parser, LEX_TOKEN_TY_COLON) && parse_expr(parser, &expr)) {
    declarator->bitfield_size =
        arena_alloc(parser->arena, sizeof(*declarator->bitfield_size));
    *declarator->bitfield_size = expr;
  } else {
    declarator->bitfield_size = NULL;
    backtrack(parser->lexer, end_of_declarator);
  }

  if (!has_declarator && !declarator->bitfield_size) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

static bool parse_init_declarator(struct parser *parser,
                                  struct ast_init_declarator *init_declarator) {
  if (!parse_declarator(parser, &init_declarator->declarator)) {
    return false;
  }

  struct text_pos pre_init_pos = get_position(parser->lexer);

  struct ast_init init;
  if (parse_token(parser, LEX_TOKEN_TY_OP_ASSG) && parse_init(parser, &init)) {
    init_declarator->init =
        arena_alloc(parser->arena, sizeof(*init_declarator->init));
    *init_declarator->init = init;
  } else {
    backtrack(parser->lexer, pre_init_pos);
    init_declarator->init = NULL;
  }

  return true;
}

static void parse_init_declarator_list(
    struct parser *parser,
    struct ast_init_declarator_list *init_declarator_list) {
  struct vector *list =
      vector_create(sizeof(*init_declarator_list->init_declarators));

  struct ast_init_declarator init_declarator;
  while (parse_init_declarator(parser, &init_declarator)) {
    vector_push_back(list, &init_declarator);

    parse_token(parser, LEX_TOKEN_TY_COMMA);
  }

  init_declarator_list->init_declarators =
      arena_alloc(parser->arena, vector_byte_size(list));
  init_declarator_list->num_init_declarators = vector_length(list);

  vector_copy_to(list, init_declarator_list->init_declarators);
  vector_free(&list);
}

static bool parse_type_name(struct parser *parser,
                            struct ast_type_name *type_name) {
  struct text_pos pos = get_position(parser->lexer);

  parse_declaration_specifier_list(parser, &type_name->specifier_list);
  if (!type_name->specifier_list.num_decl_specifiers) {
    backtrack(parser->lexer, pos);
    return false;
  }

  parse_abstract_declarator(parser, &type_name->abstract_declarator);

  return true;
}

static bool parse_var(struct parser *parser, struct ast_var *var) {
  if (parse_identifier(parser, &var->identifier)) {
    return true;
  }

  return false;
}

static bool parse_float_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct lex_token token;

  peek_token(parser->lexer, &token);

  enum ast_cnst_ty ty;
  switch (token.ty) {
  case LEX_TOKEN_TY_FLOAT_LITERAL:
    ty = AST_CNST_TY_FLOAT;
    break;
  case LEX_TOKEN_TY_DOUBLE_LITERAL:
    ty = AST_CNST_TY_DOUBLE;
    break;
  case LEX_TOKEN_TY_LONG_DOUBLE_LITERAL:
    ty = AST_CNST_TY_LONG_DOUBLE;
    break;
  default:
    return false;
  }

  const char *literal_text = associated_text(parser->lexer, &token);
  size_t literal_len = strlen(literal_text);

  DEBUG_ASSERT(literal_len, "literal_len was 0");

  char *end_ptr;
  long double float_value = strtold(literal_text, &end_ptr);

  size_t literal_end = literal_len;
  do {
    literal_end--;
  } while (literal_len && (tolower(literal_text[literal_end]) == 'f' ||
                           tolower(literal_text[literal_end]) == 'l'));

  if (end_ptr - 1 != &literal_text[literal_end]) {
    TODO("handle constant float parse failure");
  }

  // TODO: handle unrepresentedly large values
  cnst->ty = ty;
  cnst->flt_value = float_value;

  consume_token(parser->lexer, token);
  return true;
}

static bool parse_char_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct lex_token token;

  peek_token(parser->lexer, &token);

  unsigned long long int_value;

  enum ast_cnst_ty ty;
  switch (token.ty) {
  case LEX_TOKEN_TY_ASCII_CHAR_LITERAL: {
    ty = AST_CNST_TY_CHAR;

    size_t literal_len;
    const char *literal_text =
        strlike_associated_text(parser->lexer, &token, &literal_len);
    DEBUG_ASSERT(literal_len, "literal_len was 0");
    int_value = (unsigned long long)literal_text[0];
    break;
  }
  case LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL: {
    ty = AST_CNST_TY_SIGNED_INT;

    size_t literal_len;
    const char *literal_text =
        strlike_associated_text(parser->lexer, &token, &literal_len);
    DEBUG_ASSERT(literal_len, "literal_len was 0");

    wchar_t wchar;
    mbtowc(&wchar, literal_text, literal_len);
    int_value = (unsigned long long)wchar;
    break;
  }
  default:
    return false;
  }

  consume_token(parser->lexer, token);

  cnst->ty = ty;
  cnst->int_value = int_value;

  return true;
}

static bool parse_int_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct lex_token token;

  peek_token(parser->lexer, &token);

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

  const char *literal_text = associated_text(parser->lexer, &token);
  size_t literal_len = strlen(literal_text);
  DEBUG_ASSERT(literal_len, "literal_len was 0");

  int base = 10;
  if (literal_len >= 2 && literal_text[0] == '0' && literal_text[1] == 'x') {
    base = 16;
  } else if (literal_text[0] == '0') {
    // this classes '0' as octal but that is fine
    base = 8;
  }

  char *end_ptr;
  unsigned long long int_value = strtoull(literal_text, &end_ptr, base);

  size_t literal_end = literal_len;
  do {
    literal_end--;
  } while (literal_len && (tolower(literal_text[literal_end]) == 'u' ||
                           tolower(literal_text[literal_end]) == 'l'));

  if (end_ptr - 1 != &literal_text[literal_end]) {
    TODO("handle constant int parse failure");
  }

  // TODO: handle unrepresentedly large values
  cnst->ty = ty;
  cnst->int_value = int_value;

  consume_token(parser->lexer, token);
  return true;
}

static bool parse_str_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct text_pos pos = get_position(parser->lexer);

  struct lex_token token;

  struct vector *strings = vector_create(sizeof(char));

  peek_token(parser->lexer, &token);

  // must be at least one string component (but it could be empty)
  bool is_string = false;
  while (token.ty == LEX_TOKEN_TY_ASCII_STR_LITERAL ||
         token.ty == LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL) {

    if (token.ty == LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL) {
      TODO("wide str literals (must be stored as cnst data)");
    }

    is_string = true;

    cnst->ty = AST_CNST_TY_STR_LITERAL;

    size_t str_len;
    const char *str = strlike_associated_text(parser->lexer, &token, &str_len);
    vector_extend(strings, str, str_len);

    consume_token(parser->lexer, token);
    peek_token(parser->lexer, &token);
  }

  if (!is_string) {
    vector_free(&strings);

    backtrack(parser->lexer, pos);
    return false;
  }

  char null = 0;
  vector_push_back(strings, &null);
  cnst->str_value = arena_alloc(parser->arena, vector_byte_size(strings));
  vector_copy_to(strings, cnst->str_value);

  vector_free(&strings);

  return true;
}

static bool parse_cnst(struct parser *parser, struct ast_cnst *cnst) {
  return parse_str_cnst(parser, cnst) || parse_int_cnst(parser, cnst) ||
         parse_char_cnst(parser, cnst) || parse_float_cnst(parser, cnst);
}

static bool parse_expr(struct parser *parser, struct ast_expr *expr);
static bool parse_atom_0(struct parser *parser, struct ast_expr *expr);
static bool parse_atom_1(struct parser *parser, struct ast_expr *expr);
static bool parse_atom_2(struct parser *parser, struct ast_expr *expr);
static bool parse_atom_3(struct parser *parser, struct ast_expr *expr);

static bool parse_assg(struct parser *parser, struct ast_assg *assg) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr assignee;
  if (!parse_atom_3(parser, &assignee)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct lex_token token;
  peek_token(parser->lexer, &token);

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
  case LEX_TOKEN_TY_OP_QUOT_ASSG:
    ty = AST_ASSG_TY_QUOT;
    break;
  default:
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, token);

  struct ast_expr expr;
  EXP_PARSE(parse_expr(parser, &expr), "expected expr after assignment token");

  assg->ty = ty;
  assg->assignee = arena_alloc(parser->arena, sizeof(*assg->assignee));
  *assg->assignee = assignee;
  assg->expr = arena_alloc(parser->arena, sizeof(*assg->expr));
  *assg->expr = expr;

  return true;
}

static bool parse_compoundexpr(struct parser *parser,
                               struct ast_compoundexpr *compound_expr);

static bool parse_arglist(struct parser *parser, struct ast_arglist *arg_list) {
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

// parses highest precedence (literals, vars, constants)
static bool parse_atom_0(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct lex_token token;
  peek_token(parser->lexer, &token);

  // parenthesised expression
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_compoundexpr(parser, &expr->compound_expr) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    expr->ty = AST_EXPR_TY_COMPOUNDEXPR;
    return true;
  }

  backtrack(parser->lexer, pos);

  if (parse_cnst(parser, &expr->cnst)) {
    expr->ty = AST_EXPR_TY_CNST;
    return true;
  }

  if (parse_var(parser, &expr->var)) {
    expr->ty = AST_EXPR_TY_VAR;
    return true;
  }

  return false;
}

static bool
parse_compound_literal(UNUSED struct parser *parser,
                       UNUSED struct ast_compound_literal *compound_literal) {
  // TODO
  return false;
}

// parses precedence level 0:
// vars
static bool parse_atom_1(struct parser *parser, struct ast_expr *expr) {
  if (parse_atom_0(parser, expr)) {
    return true;
  }

  if (parse_compound_literal(parser, &expr->compound_literal)) {
    expr->ty = AST_EXPR_TY_COMPOUND_LITERAL;
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

  return true;
}

static bool parse_array_access(struct parser *parser, struct ast_expr *lhs,
                               struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (parse_token(parser, LEX_TOKEN_TY_OPEN_SQUARE_BRACKET)) {
    struct ast_expr *rhs = arena_alloc(parser->arena, sizeof(*rhs));

    EXP_PARSE(parse_expr(parser, rhs),
              "expression after [ in expression context");
    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET),
              "] after array access");

    expr->ty = AST_EXPR_TY_ARRAYACCESS;
    expr->array_access.lhs = lhs;
    expr->array_access.rhs = rhs;

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static bool parse_member_access(struct parser *parser,
                                struct ast_expr *sub_expr,
                                struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_DOT)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct lex_token token;
  EXP_PARSE(parse_identifier(parser, &token),
            "identifier after . in member access");

  expr->ty = AST_EXPR_TY_MEMBERACCESS;
  expr->member_access =
      (struct ast_memberaccess){.lhs = sub_expr, .member = token};

  return true;
}

static bool parse_pointer_access(struct parser *parser,
                                 struct ast_expr *sub_expr,
                                 struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_ARROW)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct lex_token token;
  EXP_PARSE(parse_identifier(parser, &token),
            "identifier after -> in pointer access");

  expr->ty = AST_EXPR_TY_POINTERACCESS;
  expr->pointer_access =
      (struct ast_pointeraccess){.lhs = sub_expr, .member = token};

  return true;
}

static bool parse_unary_postfix_op(struct parser *parser,
                                   struct ast_expr *sub_expr,
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

  expr->ty = AST_EXPR_TY_UNARY_OP;
  expr->unary_op = (struct ast_unary_op){
      .ty = unary_postfix_ty,
      .expr = sub_expr,
  };

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
  struct text_pos pos = get_position(parser->lexer);

  struct ast_type_name type_name;
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_type_name(parser, &type_name)) {

    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET),
              ") after type in cast");

    struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*sub_expr));
    if (!parse_atom_3(parser, sub_expr)) {
      backtrack(parser->lexer, pos);
      return false;
    }

    expr->ty = AST_EXPR_TY_UNARY_OP;
    expr->unary_op = (struct ast_unary_op){
        .ty = AST_UNARY_OP_TY_CAST,
        .expr = sub_expr,
        // TODO: this is redundant
        .cast = (struct ast_cast){.type_name = type_name}};

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static bool parse_unary_prefix_op(struct parser *parser,
                                  struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct lex_token token;
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

  expr->ty = AST_EXPR_TY_UNARY_OP;
  expr->unary_op = (struct ast_unary_op){
      .ty = unary_prefix_ty,
      .expr = sub_expr,
  };

  return true;
}

static bool parse_sizeof(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_KW_SIZEOF)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  // because of how sizeof works, we need to try and parse `sizeof(<ty_ref>)`
  // first else, something like `sizeof(char) + sizeof(short)` will be
  // resolves as `sizeof( (char) + sizeof(short) )` that is, the size of
  // `+sizeof(short)` cast to `char`

  struct text_pos post_sizeof_pos = get_position(parser->lexer);

  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) &&
      parse_type_name(parser, &expr->size_of.type_name) &&
      parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    expr->ty = AST_EXPR_TY_SIZEOF;
    expr->size_of.ty = AST_SIZEOF_TY_TYPE;
    return true;
  }

  backtrack(parser->lexer, post_sizeof_pos);

  struct ast_expr *sub_expr = arena_alloc(parser->arena, sizeof(*sub_expr));
  if (parse_atom_3(parser, sub_expr)) {
    expr->ty = AST_EXPR_TY_SIZEOF;
    expr->size_of =
        (struct ast_sizeof){.ty = AST_SIZEOF_TY_EXPR, .expr = sub_expr};

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static bool parse_alignof(struct parser *parser, struct ast_expr *expr) {
  if (!parse_token(parser, LEX_TOKEN_TY_KW_ALIGNOF)) {
    return false;
  }

  EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET),
            "expected ( after alignof");
  EXP_PARSE(parse_type_name(parser, &expr->align_of.type_name),
            "expected type name after alignof");
  EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET),
            "expected ) after alignof");

  expr->ty = AST_EXPR_TY_ALIGNOF;
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
                                        struct ast_expr *expr) {
  if (!parse_atom_3(parser, expr)) {
    return false;
  }

  // TODO: make iterative
  while (true) {
    struct lex_token lookahead;
    peek_token(parser->lexer, &lookahead);
    debug("looked ahead to %s", token_name(parser->lexer, &lookahead));
    struct ast_op_info info;

    if (!op_info_for_token(&lookahead, &info) ||
        info.precedence < min_precedence) {
      return true;
    }

    consume_token(parser->lexer, lookahead);

    DEBUG_ASSERT(info.associativity != AST_ASSOCIATIVITY_NONE,
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

    expr->ty = AST_EXPR_TY_BINARY_OP;

    struct ast_binary_op *binary_op = &expr->binary_op;
    binary_op->ty = info.ty;

    binary_op->lhs = arena_alloc(parser->arena, sizeof(*binary_op->lhs));
    *binary_op->lhs = lhs;

    binary_op->rhs = arena_alloc(parser->arena, sizeof(*binary_op->rhs));
    *binary_op->rhs = rhs;
  }
}

static bool parse_ternary(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr cond, true_expr, false_expr;
  if (parse_expr_precedence_aware(parser, 0, &cond) &&
      parse_token(parser, LEX_TOKEN_TY_QMARK) &&
      parse_expr_precedence_aware(parser, 0, &true_expr) &&
      parse_token(parser, LEX_TOKEN_TY_COLON) &&
      parse_expr_precedence_aware(parser, 0, &false_expr)) {
    expr->ty = AST_EXPR_TY_TERNARY;
    expr->ternary = (struct ast_ternary){
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

static bool parse_constant_expr(struct parser *parser, struct ast_expr *expr) {
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
static bool parse_expr(struct parser *parser, struct ast_expr *expr) {
  // assignment is lowest precedence, so we parse it here

  struct ast_assg assg;
  if (parse_assg(parser, &assg)) {
    expr->ty = AST_EXPR_TY_ASSG;
    expr->assg = assg;

    return true;
  }

  return parse_constant_expr(parser, expr);
}

// there are only two places you can have compound expressions
// * at top level of a statement (e.g `a = 1, b = 2;`)
// * within braces (e.g `(a = 1, b = 2)`)
// so only those places call this method for that purpose.
// `parse_call` calls this method as a helper but doesn't actually parse it as
// a compound expr
static bool parse_compoundexpr(struct parser *parser,
                               struct ast_compoundexpr *compound_expr) {
  struct text_pos pos = get_position(parser->lexer);

  // this could be made recursive instead

  struct vector *exprs = vector_create(sizeof(struct ast_expr));

  struct lex_token token;
  struct ast_expr sub_expr;
  do {
    if (!parse_expr(parser, &sub_expr)) {
      goto failure;
    }

    vector_push_back(exprs, &sub_expr);

    peek_token(parser->lexer, &token);
  } while (token.ty == LEX_TOKEN_TY_COMMA &&
           /* hacky */ (consume_token(parser->lexer, token), true));

  if (vector_empty(exprs)) {
    goto failure;
  }

  compound_expr->exprs = arena_alloc(parser->arena, vector_byte_size(exprs));
  compound_expr->num_exprs = vector_length(exprs);

  goto success;

failure:
  backtrack(parser->lexer, pos);
  vector_copy_to(exprs, compound_expr->exprs);
  vector_free(&exprs);

  return false;

success:
  vector_copy_to(exprs, compound_expr->exprs);
  vector_free(&exprs);

  return true;
}

// parse a non-compound expression, ending with a semicolon
static bool
parse_compoundexpr_with_semicolon(struct parser *parser,
                                  struct ast_compoundexpr *compoundexpr) {
  struct text_pos pos = get_position(parser->lexer);

  if (parse_compoundexpr(parser, compoundexpr) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static void add_typedefs_to_table(
    struct parser *parser,
    const struct ast_direct_declarator_list *direct_decl_list) {
  for (size_t i = 0; i < direct_decl_list->num_direct_declarators; i++) {

    const struct ast_direct_declarator *direct_decl =
        &direct_decl_list->direct_declarators[i];

    if (direct_decl->ty == AST_DIRECT_DECLARATOR_TY_IDENTIFIER) {
      var_table_create_entry(&parser->ty_table,
                             identifier_str(parser, &direct_decl->identifier));
    } else if (direct_decl->ty == AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR) {
      add_typedefs_to_table(
          parser, &direct_decl->paren_declarator->direct_declarator_list);
    }
  }
}

static bool parse_declaration(struct parser *parser,
                              struct ast_declaration *declaration) {
  struct text_pos pos = get_position(parser->lexer);

  parse_declaration_specifier_list(parser, &declaration->specifier_list);
  parse_init_declarator_list(parser, &declaration->declarator_list);

  if (!declaration->specifier_list.num_decl_specifiers ||
      !parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    backtrack(parser->lexer, pos);
    return false;
  }

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

  return true;
}

static void
parse_declaration_list(struct parser *parser,
                       struct ast_declaration_list *declaration_list) {
  struct vector *list = vector_create(sizeof(*declaration_list->declarations));

  struct ast_declaration declaration;
  while (parse_declaration(parser, &declaration)) {
    vector_push_back(list, &declaration);
  }

  declaration_list->declarations =
      arena_alloc(parser->arena, vector_byte_size(list));
  declaration_list->num_declarations = vector_length(list);

  vector_copy_to(list, declaration_list->declarations);
  vector_free(&list);
}

static bool parse_jumpstmt(struct parser *parser,
                           struct ast_jumpstmt *jump_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr expr;
  if (parse_token(parser, LEX_TOKEN_TY_KW_RETURN) &&
      parse_expr(parser, &expr) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    jump_stmt->ty = AST_JUMPSTMT_TY_RETURN;
    jump_stmt->return_stmt.expr =
        arena_alloc(parser->arena, sizeof(*jump_stmt->return_stmt.expr));
    *jump_stmt->return_stmt.expr = expr;

    return true;
  }

  backtrack(parser->lexer, pos);

  if (parse_token(parser, LEX_TOKEN_TY_KW_RETURN) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    jump_stmt->ty = AST_JUMPSTMT_TY_RETURN;
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
    struct lex_token label;
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

static bool parse_stmt(struct parser *parser, struct ast_stmt *stmt);

static bool parse_labeledstmt(struct parser *parser,
                              struct ast_labeledstmt *labeled_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct lex_token label;
  peek_token(parser->lexer, &label);
  consume_token(parser->lexer, label);

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

static bool parse_ifstmt(struct parser *parser, struct ast_ifstmt *if_stmt) {
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

  if_stmt->cond = expr;
  if_stmt->body = arena_alloc(parser->arena, sizeof(*if_stmt->body));
  *if_stmt->body = stmt;

  return true;
}

static bool parse_ifelsestmt(struct parser *parser,
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

  if_else_stmt->cond = if_stmt.cond;
  if_else_stmt->body = arena_alloc(parser->arena, sizeof(*if_else_stmt->body));
  if_else_stmt->body = if_stmt.body;
  if_else_stmt->else_body =
      arena_alloc(parser->arena, sizeof(*if_else_stmt->else_body));
  *if_else_stmt->else_body = else_stmt;

  return true;
}

static bool parse_switchstmt(struct parser *parser,
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

static bool parse_whilestmt(struct parser *parser,
                            struct ast_whilestmt *while_stmt) {
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

static bool parse_dowhilestmt(struct parser *parser,
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

static bool
parse_declaration_or_expr(struct parser *parser,
                          struct ast_declaration_or_expr *decl_or_expr) {
  struct text_pos pos = get_position(parser->lexer);

  if (parse_declaration(parser, &decl_or_expr->decl)) {
    decl_or_expr->ty = AST_DECLARATION_OR_EXPR_TY_DECL;
    return true;
  }

  if (parse_expr(parser, &decl_or_expr->expr)) {
    EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_SEMICOLON),
              "expected ; after expr in for loop initializer");
    decl_or_expr->ty = AST_DECLARATION_OR_EXPR_TY_EXPR;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static bool parse_forstmt(struct parser *parser, struct ast_forstmt *for_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  // first parse the `for (`
  if (!(parse_token(parser, LEX_TOKEN_TY_KW_FOR) &&
        parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET))) {
    backtrack(parser->lexer, pos);
    return false;
  }

  // then, we look for a vardecllist or an expression, or neither
  struct ast_declaration_or_expr decl_or_expr;
  if (parse_declaration_or_expr(parser, &decl_or_expr)) {
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

static bool parse_iterstmt(struct parser *parser,
                           struct ast_iterstmt *iter_stmt) {
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

static bool parse_selectstmt(struct parser *parser,
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

static bool parse_compoundstmt(struct parser *parser,
                               struct ast_compoundstmt *compound_stmt);

static bool parse_stmt(struct parser *parser, struct ast_stmt *stmt) {
  struct text_pos pos = get_position(parser->lexer);

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
    stmt->ty = AST_STMT_TY_EXPR;
    stmt->expr.ty = AST_EXPR_TY_COMPOUNDEXPR;
    stmt->expr.compound_expr = compound_expr;

    return true;
  }

  struct ast_declaration declaration;
  if (parse_declaration(parser, &declaration)) {
    stmt->ty = AST_STMT_TY_DECLARATION;
    stmt->declaration = declaration;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

static bool parse_compoundstmt(struct parser *parser,
                               struct ast_compoundstmt *compound_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  push_scope(&parser->ty_table);

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACE)) {
    backtrack(parser->lexer, pos);
    pop_scope(&parser->ty_table);
    return false;
  }

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

  EXP_PARSE(parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE),
            "expected } at end of compound stmt");

  pop_scope(&parser->ty_table);
  return true;
}

static bool parse_param(struct parser *parser, struct ast_param *param) {
  struct text_pos pos = get_position(parser->lexer);

  if (parse_token(parser, LEX_TOKEN_TY_ELLIPSIS)) {
    param->ty = AST_PARAM_TY_VARIADIC;
    return true;
  } else if (parse_token(parser, LEX_TOKEN_TY_KW_VOID)) {
    struct lex_token token;
    peek_token(parser->lexer, &token);
    if (token.ty == LEX_TOKEN_TY_CLOSE_BRACKET) {
      param->ty = AST_PARAM_TY_VOID;
      return true;
    }

    backtrack(parser->lexer, pos);
  }

  parse_declaration_specifier_list(parser, &param->specifier_list);

  if (!param->specifier_list.num_decl_specifiers) {
    backtrack(parser->lexer, pos);
    return false;
  }

  if (parse_declarator(parser, &param->declarator)) {
    param->ty = AST_PARAM_TY_DECL;
    return true;
  } else if (parse_abstract_declarator(parser, &param->abstract_declarator)) {
    param->ty = AST_PARAM_TY_ABSTRACT_DECL;
    return true;
  } else {
    param->ty = AST_PARAM_TY_ABSTRACT_DECL;
    param->abstract_declarator = (struct ast_abstract_declarator){
        .direct_abstract_declarator_list = {.direct_abstract_declarators = NULL,
                                            .num_direct_abstract_declarators =
                                                0},
        .pointer_list = {.pointers = NULL, .num_pointers = 0}};
    return true;
  }
}

static bool parse_paramlist(struct parser *parser,
                            struct ast_paramlist *param_list) {
  struct text_pos pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET)) {
    return false;
  }

  param_list->params = NULL;
  param_list->num_params = 0;

  struct vector *params = vector_create(sizeof(struct ast_param));

  struct ast_param param;
  while (parse_param(parser, &param)) {
    vector_push_back(params, &param);

    if (!parse_token(parser, LEX_TOKEN_TY_COMMA)) {
      break;
    }
  }

  // allow trailing comma
  parse_token(parser, LEX_TOKEN_TY_COMMA);

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  param_list->params = arena_alloc(parser->arena, vector_byte_size(params));
  param_list->num_params = vector_length(params);

  vector_copy_to(params, param_list->params);
  vector_free(&params);

  return true;
}

static bool parse_funcdef(struct parser *parser, struct ast_funcdef *func_def) {
  struct text_pos pos = get_position(parser->lexer);

  parse_declaration_specifier_list(parser, &func_def->specifier_list);

  if (!parse_declarator(parser, &func_def->declarator)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  parse_declaration_list(parser, &func_def->declaration_list);

  if (!parse_compoundstmt(parser, &func_def->body)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

#define TOKEN_FMT(lexer, token)                                                \
  text_pos_len((token).start, (token).end),                                    \
      text_pos_len((token).start, (token).end), lexer->text[(token).start.idx]

const char *identifier_str(struct parser *parser,
                           const struct lex_token *token) {
  return associated_text(parser->lexer, token);
}

static bool parse_external_declaration(
    struct parser *parser,
    struct ast_external_declaration *external_declaration) {
  if (parse_funcdef(parser, &external_declaration->func_def)) {
    external_declaration->ty = AST_EXTERNAL_DECLARATION_TY_FUNC_DEF;
    return true;
  }

  if (parse_declaration(parser, &external_declaration->declaration)) {
    external_declaration->ty = AST_EXTERNAL_DECLARATION_TY_DECLARATION;
    return true;
  }

  return false;
}

struct parse_result parse(struct parser *parser) {
  struct lexer *lexer = parser->lexer;

  struct vector *declarations =
      vector_create(sizeof(struct ast_external_declaration));

  while (true) {
    if (lexer_at_eof(lexer)) {
      info("EOF reached by lexer");
      break;
    }

    struct ast_external_declaration external_declaration;
    if (parse_external_declaration(parser, &external_declaration)) {
      vector_push_back(declarations, &external_declaration);
      continue;
    }

    if (!lexer_at_eof(lexer)) {
      // parser failed
      struct text_pos pos = get_position(lexer);
      err("parser finished at position %zu, line=%zu, col=%zu", pos.idx,
          pos.line, pos.col);
      break;
    }

    BUG("parser hit nothing");
  }

  struct ast_translationunit translation_unit;

  translation_unit.external_declarations =
      arena_alloc(parser->arena, vector_byte_size(declarations));
  translation_unit.num_external_declarations = vector_length(declarations);
  vector_copy_to(declarations, translation_unit.external_declarations);
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
  } while (0);

#define PUSH_INDENT()                                                          \
  int tmp_indent = state->indent;                                              \
  state->indent = 0;
#define POP_INDENT() state->indent = tmp_indent;

DEBUG_FUNC_ENUM(function_specifier, function_specifier) {
  switch (*function_specifier) {
  case AST_FUNCTION_SPECIFIER_INLINE:
    AST_PRINTZ("INLINE");
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

DEBUG_FUNC(struct_or_union_specifier, struct_or_union_specifier) {
  switch (struct_or_union_specifier->ty) {
  case AST_STRUCT_OR_UNION_SPECIFIER_TY_STRUCT:
    if (struct_or_union_specifier->identifier) {
      AST_PRINT(
          "STRUCT '%s'",
          identifier_str(state->parser, struct_or_union_specifier->identifier));

    } else {
      AST_PRINTZ("STRUCT");
    }
    break;
  case AST_STRUCT_OR_UNION_SPECIFIER_TY_UNION:
    if (struct_or_union_specifier->identifier) {
      AST_PRINT(
          "UNION '%s'",
          identifier_str(state->parser, struct_or_union_specifier->identifier));

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
  AST_PRINT("ENUMERATOR '%s'",
            identifier_str(state->parser, &enumerator->identifier));

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
    AST_PRINT("ENUM '%s'",
              identifier_str(state->parser, enum_specifier->identifier));

  } else {
    AST_PRINTZ("ENUM");
  }

  if (enum_specifier->enumerator_list) {
    DEBUG_CALL(enumerator_list, enum_specifier->enumerator_list);
  }
}

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
    AST_PRINT("TYPEDEF '%s'",
              identifier_str(state->parser, &type_specifier->typedef_name));
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(compoundstmt, compound_stmt);

DEBUG_FUNC(var, var) {
  AST_PRINT("VARIABLE '%s'", identifier_str(state->parser, &var->identifier));
}

DEBUG_FUNC(cnst, cnst) {
  switch (cnst->ty) {
  case AST_CNST_TY_SIGNED_INT:
  case AST_CNST_TY_UNSIGNED_INT:
  case AST_CNST_TY_SIGNED_LONG:
  case AST_CNST_TY_UNSIGNED_LONG:
  case AST_CNST_TY_SIGNED_LONG_LONG:
  case AST_CNST_TY_UNSIGNED_LONG_LONG:
    AST_PRINT("CONSTANT '%llu'", cnst->int_value);
    break;
  case AST_CNST_TY_FLOAT:
  case AST_CNST_TY_DOUBLE:
  case AST_CNST_TY_LONG_DOUBLE:
    AST_PRINT("CONSTANT '%Lf'", cnst->flt_value);
    break;
  case AST_CNST_TY_CHAR:
    AST_PRINT("CONSTANT '%c'", (char)cnst->int_value);
    break;
  case AST_CNST_TY_WIDE_CHAR:
    AST_PRINT("CONSTANT (wide char) '%'llu", cnst->int_value);
    break;
  case AST_CNST_TY_STR_LITERAL:
    AST_PRINT_SAMELINE_Z("CONSTANT ");
    fprint_str(stderr, cnst->str_value);
    break;
  case AST_CNST_TY_WIDE_STR_LITERAL:
    TODO("wide strs");
  }
}

DEBUG_FUNC(compoundexpr, compound_expr);

DEBUG_FUNC(pointer, pointer) {
  AST_PRINTZ("POINTER");
  DEBUG_CALL(declaration_specifier_list, &pointer->specifier_list);
}

DEBUG_FUNC(pointer_list, pointer_list) {
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
    AST_PRINT("IDENTIFIER '%s'",
              identifier_str(state->parser, &direct_declarator->identifier));
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

DEBUG_FUNC(declarator, declarator) {
  AST_PRINTZ("DECLARATOR");
  INDENT();
  DEBUG_CALL(pointer_list, &declarator->pointer_list);
  DEBUG_CALL(direct_declarator_list, &declarator->direct_declarator_list);
  UNINDENT();
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
  case AST_ASSG_TY_QUOT:
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
  AST_PRINTZ("DESIGNATOR");

  INDENT();
  switch (designator->ty) {
  case AST_DESIGNATOR_TY_FIELD:
    AST_PRINT("FIELD '%s'", identifier_str(state->parser, &designator->field));
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

DEBUG_FUNC(sizeof, size_of) {
  AST_PRINTZ("SIZEOF");

  INDENT();
  switch (size_of->ty) {
  case AST_SIZEOF_TY_TYPE:
    DEBUG_CALL(type_name, &size_of->type_name);
    break;
  case AST_SIZEOF_TY_EXPR:
    DEBUG_CALL(expr, size_of->expr);
    break;
  }
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
  if (state->parser) {
    TODO("compound literal");
  }
}

DEBUG_FUNC(expr, expr) {
  AST_PRINTZ("EXPRESSION");

  INDENT();
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
    AST_PRINT("LABEL %s", identifier_str(state->parser, &labeled_stmt->label));
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

DEBUG_FUNC(stmt, stmt) {
  INDENT();

  switch (stmt->ty) {
  case AST_STMT_TY_NULL:
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
  AST_PRINT_SAMELINE_Z("FUNCTION DEFINITION ");

  DEBUG_CALL(compoundstmt, &func_def->body);
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

void debug_print_ast(struct parser *parser,
                     struct ast_translationunit *translation_unit) {
  struct ast_printstate state_ = {.indent = 0, .parser = parser};

  struct ast_printstate *state = &state_;

  AST_PRINTZ("PRINTING AST");

  for (size_t i = 0; i < translation_unit->num_external_declarations; i++) {
    DEBUG_CALL(external_declaration,
               &translation_unit->external_declarations[i]);
  }
}
