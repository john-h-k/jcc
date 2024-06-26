#include "parse.h"

#include "alloc.h"
#include "lex.h"
#include "log.h"
#include "util.h"
#include "var_table.h"
#include "vector.h"

#include <alloca.h>
#include <string.h>

struct parser {
  struct arena_allocator *arena;
  struct lexer *lexer;

  // `value` contains a `struct ast_tyref *` to the type of the variable
  // or NULL if the variable has been used without a declaration
  struct var_table var_table;

  // returns need to know which type they coerce to
  struct ast_tyref func_ret_ty;
};

enum parser_create_result parser_create(const char *program,
                                        struct parser **parser) {
  struct parser *p = nonnull_malloc(sizeof(*p));

  arena_allocator_create(&p->arena);
  if (lexer_create(program, &p->lexer) != LEX_STATUS_SUCCESS) {
    err("failed to create lexer");
    return PARSER_CREATE_RESULT_FAILURE;
  }

  p->var_table = var_table_create(p->arena);

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
  case AST_BINARY_OP_TY_AND:
    info.precedence = 1;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_EQ:
  case AST_BINARY_OP_TY_NEQ:
    info.precedence = 2;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_GT:
  case AST_BINARY_OP_TY_GTEQ:
  case AST_BINARY_OP_TY_LT:
  case AST_BINARY_OP_TY_LTEQ:
    info.precedence = 3;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_LSHIFT:
  case AST_BINARY_OP_TY_RSHIFT:
    info.precedence = 4;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_ADD:
  case AST_BINARY_OP_TY_SUB:
    info.precedence = 5;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_MUL:
  case AST_BINARY_OP_TY_DIV:
  case AST_BINARY_OP_TY_QUOT:
    info.precedence = 6;
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
  case LEX_TOKEN_TY_OP_AND:
    *info = op_info(AST_BINARY_OP_TY_AND);
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

bool is_literal_token(enum lex_token_ty tok_ty,
                      enum well_known_ty *well_known_ty) {
  switch (tok_ty) {
  case LEX_TOKEN_TY_UNKNOWN:
  case LEX_TOKEN_TY_EOF:
  case LEX_TOKEN_TY_WHITESPACE:
  case LEX_TOKEN_TY_INLINE_COMMENT:
  case LEX_TOKEN_TY_MULTILINE_COMMENT:
  case LEX_TOKEN_TY_OPEN_BRACKET:
  case LEX_TOKEN_TY_CLOSE_BRACKET:
  case LEX_TOKEN_TY_OPEN_BRACE:
  case LEX_TOKEN_TY_CLOSE_BRACE:
  case LEX_TOKEN_TY_SEMICOLON:
  case LEX_TOKEN_TY_COMMA:
  case LEX_TOKEN_TY_OP_ASSG:
  case LEX_TOKEN_TY_OP_INC:
  case LEX_TOKEN_TY_OP_DEC:
  case LEX_TOKEN_TY_OP_RSHIFT:
  case LEX_TOKEN_TY_OP_RSHIFT_ASSG:
  case LEX_TOKEN_TY_OP_LSHIFT:
  case LEX_TOKEN_TY_OP_LSHIFT_ASSG:
  case LEX_TOKEN_TY_OP_AND:
  case LEX_TOKEN_TY_OP_AND_ASSG:
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
  case LEX_TOKEN_TY_KW_DO:
  case LEX_TOKEN_TY_KW_FOR:
  case LEX_TOKEN_TY_KW_WHILE:
  case LEX_TOKEN_TY_KW_IF:
  case LEX_TOKEN_TY_KW_ELSE:
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
  case LEX_TOKEN_TY_IDENTIFIER:
    return false;

  case LEX_TOKEN_TY_ASCII_CHAR_LITERAL:
    // char is signed!
    *well_known_ty = WELL_KNOWN_TY_SIGNED_CHAR;
    return true;

  case LEX_TOKEN_TY_SIGNED_INT_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_SIGNED_INT;
    return true;

  case LEX_TOKEN_TY_UNSIGNED_INT_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_UNSIGNED_INT;
    return true;

  case LEX_TOKEN_TY_SIGNED_LONG_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_SIGNED_LONG;
    return true;

  case LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_UNSIGNED_LONG;
    return true;

  case LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_SIGNED_LONG_LONG;
    return true;

  case LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_UNSIGNED_LONG_LONG;
    return true;
  }

  unreachable("switch broke");
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

bool parse_integer_size_name(struct parser *parser, enum well_known_ty *wkt) {
  struct token token;
  peek_token(parser->lexer, &token);

  switch (token.ty) {
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

    if (maybe_other_long.ty == LEX_TOKEN_TY_KW_LONG) {
      *wkt = WELL_KNOWN_TY_SIGNED_LONG_LONG;
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

bool parse_tyref(struct parser *parser, struct ast_tyref *ty_ref) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

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
  if (!parse_integer_size_name(parser, &wkt)) {
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

bool parse_var(struct parser *parser, struct ast_var *var) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty != LEX_TOKEN_TY_IDENTIFIER) {
    backtrack(parser->lexer, pos);
    return false;
  }

  var->identifier = token;

  struct var_table_entry *entry = get_entry(&parser->var_table, identifier_str(parser, &var->identifier));
  if (entry && entry->value) {
    var->var_ty = *(struct ast_tyref *)entry->value;
    var->scope = entry->scope;
  }

  consume_token(parser->lexer, token);

  return true;
}

bool parse_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  peek_token(parser->lexer, &token);
  enum well_known_ty wkt;
  if (is_literal_token(token.ty, &wkt)) {
    // TODO: handle unrepresentedly large values
    cnst->cnst_ty = wkt;
    cnst->value = atoll(associated_text(parser->lexer, &token));

    consume_token(parser->lexer, token);
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_expr(struct parser *parser, struct ast_expr *expr);
bool parse_above_atom(struct parser *parser, struct ast_expr *expr);

bool parse_assg(struct parser *parser, struct ast_assg *assg) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_expr assignee;
  struct ast_expr expr;
  if (!parse_above_atom(parser, &assignee) ||
      !parse_token(parser, LEX_TOKEN_TY_OP_ASSG) ||
      !parse_expr(parser, &expr)) {
    backtrack(parser->lexer, pos);
    return false;
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
  if (!parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET) ||
      !parse_compoundexpr(parser, &compound_expr) ||
      !parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACKET)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  arg_list->args = compound_expr.exprs;
  arg_list->num_args = compound_expr.num_exprs;

  return true;
}

struct ast_tyref var_ty_return_type_of(const struct ast_tyref *ty) {
  invariant_assert(ty->ty == AST_TYREF_TY_FUNC,
                   "only makes sense with func ty");

  return *ty->func.ret_var_ty;
}

// parses an expression that does _not_ involve binary operators
bool parse_atom(struct parser *parser, struct ast_atom *atom) {
  err("parsing atom");
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
    atom->ty = AST_ATOM_TY_COMPOUNDEXPR;
    atom->var_ty = var_ty;
    atom->compound_expr = compound_expr;

    return true;
  }

  backtrack(parser->lexer, pos);

  struct ast_cnst cnst;
  if (parse_cnst(parser, &cnst)) {
    atom->ty = AST_ATOM_TY_CNST;
    atom->var_ty.ty = AST_TYREF_TY_WELL_KNOWN;
    atom->var_ty.well_known = cnst.cnst_ty;
    atom->cnst = cnst;

    return true;
  }

  struct ast_var var;
  if (parse_var(parser, &var)) {
    atom->ty = AST_ATOM_TY_VAR;

    atom->var_ty = var.var_ty;
    atom->var = var;

    return true;
  }

  return false;
}

bool parse_above_atom(struct parser *parser, struct ast_expr *expr) {
  printf("parsing atom\n");
  struct ast_atom atom;
  if (!parse_atom(parser, &atom)) {
    return false;
  }

  struct ast_arglist arg_list;
  if (parse_arglist(parser, &arg_list)) {
    expr->ty = AST_EXPR_TY_CALL;
    expr->var_ty = var_ty_return_type_of(&atom.var_ty);
    expr->call = arena_alloc(parser->arena, sizeof(*expr->call));
    expr->call->var_ty = var_ty_return_type_of(&atom.var_ty);
    expr->call->target = arena_alloc(parser->arena, sizeof(*expr->call->target));
    *expr->call->target = atom;
    expr->call->arg_list = arg_list;

    return true;
  }

  expr->ty = AST_EXPR_TY_ATOM;
  expr->var_ty = atom.var_ty;
  expr->atom = atom;
  return true;
}

struct ast_tyref resolve_binary_op_types(const struct ast_tyref *lhs,
                                         const struct ast_tyref *rhs) {
  debug_assert(lhs->ty != AST_TYREF_TY_UNKNOWN && rhs->ty != AST_TYREF_TY_UNKNOWN, "unknown ty in call to `resolve_binary_op_types`");

  if ((lhs->ty == AST_TYREF_TY_WELL_KNOWN &&
       rhs->ty == AST_TYREF_TY_WELL_KNOWN)) {
    struct ast_tyref result_ty;
    result_ty.ty = AST_TYREF_TY_WELL_KNOWN;

    if (lhs->well_known == rhs->well_known) {
      // they are the same type
      result_ty.well_known = lhs->well_known;
    } else {
      enum well_known_ty signed_lhs = WKT_MAKE_SIGNED(lhs->well_known);
      enum well_known_ty signed_rhs = WKT_MAKE_SIGNED(rhs->well_known);

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

    err("TY %d", result_ty.well_known);
    return result_ty;
  }

  todo("`resolve_binary_op_types` for types other than well known");
}

bool parse_expr_precedence_aware(struct parser *parser, unsigned min_precedence,
                                 struct ast_expr *expr) {
  if (!parse_above_atom(parser, expr)) {
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

    struct ast_tyref result_ty =
        resolve_binary_op_types(&lhs.var_ty, &rhs.var_ty);

    expr->ty = AST_EXPR_TY_BINARY_OP;
    expr->var_ty = result_ty;
    struct ast_binary_op *binary_op = &expr->binary_op;
    binary_op->ty = info.ty;
    binary_op->var_ty = result_ty;

    binary_op->lhs = arena_alloc(parser->arena, sizeof(*binary_op->lhs));
    *binary_op->lhs = lhs;

    binary_op->rhs = arena_alloc(parser->arena, sizeof(*binary_op->rhs));
    *binary_op->rhs = rhs;
  }
}

// parse a non-compound expression
bool parse_expr(struct parser *parser, struct ast_expr *expr) {
  // assignment is lowest precedence, so we parse it here

  struct ast_assg assg;
  if (parse_assg(parser, &assg)) {
    expr->ty = AST_EXPR_TY_ASSG;
    expr->var_ty = assg.var_ty;
    expr->assg = arena_alloc(parser->arena, sizeof(*expr->assg));
    *expr->assg = assg;

    return true;
  }
  
  return parse_expr_precedence_aware(parser, 0, expr);
}

// there are only two places you can have compound expressions
// * at top level of a statement (e.g `a = 1, b = 2;`)
// * within braces (e.g `(a = 1, b = 2)`)
// so only those places call this method for that purpose.
// `parse_call` calls this method as a helper but doesn't actually parse it as a
// compound expr
bool parse_compoundexpr(struct parser *parser,
                        struct ast_compoundexpr *compound_expr) {
  err("parsing compound");
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

bool parse_vardecl(struct parser *parser, struct ast_vardecl *var_decl) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_var var;

  if (!parse_var(parser, &var)) {
    return false;
  }

  // ignore any existing scope of the var
  var.scope = cur_scope(&parser->var_table);
  var_decl->var = var;

  struct text_pos post_var_pos = get_position(parser->lexer);

  if (!parse_token(parser, LEX_TOKEN_TY_OP_ASSG)) {
    // normal var decl, without assignment
    // return to where we were when we passed the var
    backtrack(parser->lexer, post_var_pos);

    var_decl->ty = AST_VARDECL_TY_DECL;

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

  var_decl->ty = AST_VARDECL_TY_DECL_WITH_ASSG;
  var_decl->assg_expr = expr;

  return true;
}

bool parse_vardecllist(struct parser *parser,
                       struct ast_vardecllist *var_decl_list) {
  struct ast_tyref ty_ref;
  if (!parse_tyref(parser, &ty_ref)) {
    return false;
  }

  struct vector *decls = vector_create(sizeof(struct ast_vardecl));

  struct ast_vardecl var_decl;
  while (parse_vardecl(parser, &var_decl)) {
    vector_push_back(decls, &var_decl);

    var_decl.var.var_ty = ty_ref;

    trace("creating var_table_entry for var name=%s",
          identifier_str(parser, &var_decl.var.identifier));

    // copy the type ref into arena memory for lifetime simplicity
    
    const char *name = identifier_str(parser, &var_decl.var.identifier);
    struct var_table_entry *entry = create_entry(&parser->var_table, name);
    entry->value = arena_alloc(parser->arena, sizeof(struct ast_tyref));
    *(struct ast_tyref *)entry->value = var_decl.var.var_ty;

    struct token token;
    peek_token(parser->lexer, &token);

    if (token.ty == LEX_TOKEN_TY_COMMA) {
      // another decl
      consume_token(parser->lexer, token);
      continue;
    } else if (token.ty == LEX_TOKEN_TY_SEMICOLON) {
      consume_token(parser->lexer, token);
      break;
    }
  }

  if (vector_length(decls) == 0) {
    vector_free(&decls);
    return false;
  }

  struct ast_vardecl *new_decls =
      arena_alloc(parser->arena, vector_byte_size(decls));
  vector_copy_to(decls, new_decls);

  var_decl_list->var_ty = ty_ref;
  var_decl_list->num_decls = vector_length(decls);
  var_decl_list->decls = new_decls;

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

  if (parse_token(parser, LEX_TOKEN_TY_KW_RETURN) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    jump_stmt->ty = AST_JUMPSTMT_TY_RETURN;
    jump_stmt->return_stmt.var_ty = parser->func_ret_ty;
    jump_stmt->return_stmt.expr = NULL;

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_stmt(struct parser *parser, struct ast_stmt *stmt);

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
  UNUSED_ARG(parser);
  UNUSED_ARG(switch_stmt);
  return false;
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

  struct ast_vardecllist var_decl_list;
  if (parse_vardecllist(parser, &var_decl_list)) {
    decl_or_expr->decl =
        arena_alloc(parser->arena, sizeof(*decl_or_expr->decl));
    *decl_or_expr->decl = var_decl_list;
    return true;
  }

  struct ast_expr expr;
  if (parse_expr(parser, &expr) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
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
  } else {
    for_stmt->init = NULL;
  }

  // if a decl was provided, it includes semicolon.
  // else we need a semicolon following the expression (or lack of expression)
  if (!decl_or_expr.decl && !parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
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

  // parse the iteration statement if present, else a semicolon
  struct ast_expr iter;
  if (parse_expr(parser, &iter)) {
    for_stmt->iter = arena_alloc(parser->arena, sizeof(*for_stmt->iter));
    *for_stmt->iter = iter;
  } else {
    for_stmt = NULL;
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
    stmt->expr.ty = AST_EXPR_TY_ATOM;
    stmt->expr.var_ty = var_ty;
    stmt->expr.atom.ty = AST_ATOM_TY_COMPOUNDEXPR;
    stmt->expr.atom.var_ty = var_ty;
    stmt->expr.atom.compound_expr = compound_expr;

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

  struct ast_vardecllist var_decl_list;
  if (parse_vardecllist(parser, &var_decl_list)) {
    stmt->ty = AST_STMT_TY_VAR_DECL_LIST;
    stmt->var_decl_list = var_decl_list;
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

  push_scope(&parser->var_table);

  struct vector *stmts = vector_create(sizeof(struct ast_stmt));
  struct ast_stmt stmt;
  while (parse_stmt(parser, &stmt)) {
    vector_push_back(stmts, &stmt);
  }

  compound_stmt->stmts = arena_alloc(parser->arena, vector_byte_size(stmts));
  compound_stmt->num_stmts = vector_length(stmts);
  vector_copy_to(stmts, compound_stmt->stmts);
  vector_free(&stmts);

  pop_scope(&parser->var_table);

  if (!parse_token(parser, LEX_TOKEN_TY_CLOSE_BRACE)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  return true;
}

bool parse_param(struct parser *parser, struct ast_param *param) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_tyref var_ty;
  if (!parse_tyref(parser, &var_ty)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  struct token identifier;
  peek_token(parser->lexer, &identifier);

  if (identifier.ty != LEX_TOKEN_TY_IDENTIFIER) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, identifier);

  param->var_ty = var_ty;
  param->var.scope = cur_scope(&parser->var_table);
  param->var.identifier = identifier;

  return true;
}

bool parse_paramlist(struct parser *parser, struct ast_paramlist *param_list) {
  struct text_pos pos = get_position(parser->lexer);

  push_scope(&parser->var_table);

  // TODO: merge with parse_compoundexpr?
  if (parse_token(parser, LEX_TOKEN_TY_OPEN_BRACKET)) {
    struct text_pos pos = get_position(parser->lexer);

    // this could be made recursive instead

    struct vector *params = vector_create(sizeof(struct ast_param));

    struct token token;

    peek_token(parser->lexer, &token);
    if (token.ty != LEX_TOKEN_TY_CLOSE_BRACKET) {
      struct ast_param param;
      do {
        if (!parse_param(parser, &param)) {
          backtrack(parser->lexer, pos);
          pop_scope(&parser->var_table);
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
      pop_scope(&parser->var_table);
      return true;
    }
  }

  backtrack(parser->lexer, pos);
  pop_scope(&parser->var_table);
  return false;
}

bool parse_funcsig(struct parser *parser, struct ast_funcsig *func_sig) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_tyref ty_ref;
  struct token identifier;
  struct ast_paramlist param_list;

  if (parse_tyref(parser, &ty_ref) && parse_identifier(parser, &identifier) &&
      parse_paramlist(parser, &param_list)) {
    struct ast_ty_func func_ty;
    func_ty.ret_var_ty = arena_alloc(parser->arena, sizeof(*func_ty.ret_var_ty));
    *func_ty.ret_var_ty = ty_ref;
    func_ty.num_param_var_tys = param_list.num_params;
    func_ty.param_var_tys = arena_alloc(parser->arena, sizeof(*func_ty.param_var_tys) * param_list.num_params);
    for (size_t i = 0; i < param_list.num_params; i++) {
      func_ty.param_var_tys[i] = param_list.params[i].var_ty;
    }

    struct ast_tyref func_ty_ref;
    func_ty_ref.ty = AST_TYREF_TY_FUNC;
    func_ty_ref.func = func_ty;

    struct ast_var var;
    var.identifier = identifier;
    var.scope = cur_scope(&parser->var_table);
    var.var_ty = func_ty_ref;

    const char *name = identifier_str(parser, &var.identifier);
    struct var_table_entry *entry = get_or_create_entry(&parser->var_table, name);
    entry->value = arena_alloc(parser->arena, sizeof(struct ast_tyref));
    *(struct ast_tyref *)entry->value = var.var_ty;

    func_sig->name = identifier;
    func_sig->var_ty = func_ty_ref;
    func_sig->param_list = param_list;

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_funcdecl(struct parser *parser, struct ast_funcdecl *func_decl) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_funcsig func_sig;
  if (parse_funcsig(parser, &func_sig) &&
      parse_token(parser, LEX_TOKEN_TY_SEMICOLON)) {
    func_decl->sig = func_sig;
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_funcdef(struct parser *parser, struct ast_funcdef *func_def) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_funcsig func_sig;
  struct ast_compoundstmt func_body;

  if (parse_funcsig(parser, &func_sig)) {
    push_scope(&parser->var_table);

    // return statements need to know the type they coerce to
    parser->func_ret_ty = *func_sig.var_ty.func.ret_var_ty;

    // need to add the params to the locals table
    for (size_t i = 0; i < func_sig.param_list.num_params; i++) {
      const struct ast_param *param = &func_sig.param_list.params[i];

      struct ast_var var;
      var.identifier = param->var.identifier;
      var.scope = cur_scope(&parser->var_table);
      var.var_ty = param->var_ty;

      const char *name = identifier_str(parser, &var.identifier);
      struct var_table_entry *entry = create_entry(&parser->var_table, name);
      entry->value = arena_alloc(parser->arena, sizeof(struct ast_tyref));
      *(struct ast_tyref *)entry->value = var.var_ty;
    }

    if (parse_compoundstmt(parser, &func_body)) {
      func_def->sig = func_sig;
      func_def->body = func_body;

      pop_scope(&parser->var_table);
      return true;
    }

    pop_scope(&parser->var_table);
  }

  backtrack(parser->lexer, pos);
  return false;
}

#define TOKEN_FMT(lexer, token)                                                \
  text_pos_len((token).start, (token).end),                                    \
      text_pos_len((token).start, (token).end), lexer->text[(token).start.idx]

const char *identifier_str(struct parser *parser, const struct token *token) {
  return associated_text(parser->lexer, token);
}

struct parse_result parse(struct parser *parser) {
  struct lexer *lexer = parser->lexer;

  struct vector *defs = vector_create(sizeof(struct ast_funcdef));
  struct vector *decls = vector_create(sizeof(struct ast_funcdecl));

  while (true) {
    if (lexer_at_eof(lexer)) {
      info("EOF reached by lexer");
      break;
    }

    struct ast_funcdecl func_decl;
    struct ast_funcdef func_def;
    if (parse_funcdecl(parser, &func_decl)) {
      info("found func declaration '%s'",
           associated_text(lexer, &func_decl.sig.name));
      vector_push_back(decls, &func_decl);
    } else if (parse_funcdef(parser, &func_def)) {
      info("found func definition '%s'",
           associated_text(lexer, &func_def.sig.name));
      vector_push_back(defs, &func_def);
    } else if (!lexer_at_eof(lexer)) {
      // parser failed
      err("parser finished at position %d", get_position(lexer).idx);
      break;
    }
  }

  struct ast_translationunit translation_unit;
  translation_unit.func_defs = nonnull_malloc(vector_byte_size(defs));
  translation_unit.num_func_defs = vector_length(defs);
  vector_copy_to(defs, translation_unit.func_defs);
  vector_free(&defs);

  translation_unit.func_decls = nonnull_malloc(vector_byte_size(decls));
  translation_unit.num_func_decls = vector_length(decls);
  vector_copy_to(decls, translation_unit.func_decls);
  vector_free(&decls);

  struct parse_result result = {.translation_unit = translation_unit};

  return result;
}

struct ast_printstate {
  struct parser *parser;
  int indent;

  struct graphwriter *gwr;
};

#define DEBUG_FUNC(ty, name)                                                   \
  void debug_print_##ty(struct ast_printstate *state, struct ast_##ty *name)
#define DEBUG_CALL(ty, val) debug_print_##ty(state, val)

#define AST_PRINT_SAMELINE_Z(fmt) slogsl("%*s" fmt, state->indent * 4, "")
#define AST_PRINT_SAMELINE(fmt, ...)                                           \
  slogsl("%*s" fmt, state->indent * 4, "", __VA_ARGS__)

#define AST_PRINTZ(fmt) AST_PRINT_SAMELINE_Z(fmt "\n")
#define AST_PRINT(fmt, ...) AST_PRINT_SAMELINE(fmt "\n", __VA_ARGS__)

#define INDENT() state->indent++
#define UNINDENT() state->indent--

#define PUSH_INDENT()                                                          \
  int tmp_indent = state->indent;                                              \
  state->indent = 0;
#define POP_INDENT() state->indent = tmp_indent;

DEBUG_FUNC(tyref, ty_ref) {
  switch (ty_ref->ty) {
  case AST_TYREF_TY_UNKNOWN:
    AST_PRINTZ("<unresolved type>");
    break;
  case AST_TYREF_TY_POINTER:
    AST_PRINTZ("POINTER TO");
    INDENT();
    DEBUG_CALL(tyref, ty_ref->pointer.underlying);
    UNINDENT();
    break;
  case AST_TYREF_TY_FUNC:
    AST_PRINT_SAMELINE_Z("FUNC ");
    AST_PRINT_SAMELINE_Z("( ");
    AST_PRINT_SAMELINE_Z(" )");
    for (size_t i = 0; i < ty_ref->func.num_param_var_tys; i++) {
      DEBUG_CALL(tyref, &ty_ref->func.param_var_tys[i]);

      if (i + 1 != ty_ref->func.num_param_var_tys) {
        AST_PRINT_SAMELINE_Z(", ");
      }
    }

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
            associated_text(state->parser->lexer, &var->identifier), var->scope);
}

DEBUG_FUNC(cnst, cnst) { AST_PRINT("CONSTANT '%llu'", cnst->value); }

DEBUG_FUNC(compoundexpr, compound_expr);

DEBUG_FUNC(atom, atom) {
  switch (atom->ty) {
  case AST_ATOM_TY_VAR:
    DEBUG_CALL(var, &atom->var);
    break;
  case AST_ATOM_TY_CNST:
    DEBUG_CALL(cnst, &atom->cnst);
    break;
  case AST_ATOM_TY_COMPOUNDEXPR:
    DEBUG_CALL(compoundexpr, &atom->compound_expr);
    break;
  }
}

DEBUG_FUNC(binary_op, binary_op) {
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
  }

  AST_PRINTZ("LHS: ");
  INDENT();
  DEBUG_CALL(expr, binary_op->lhs);
  UNINDENT();

  AST_PRINTZ("RHS: ");
  INDENT();
  DEBUG_CALL(expr, binary_op->rhs);
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

  AST_PRINTZ("ASSIGNED");
  INDENT();
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
  DEBUG_CALL(atom, call->target);

  DEBUG_CALL(arglist, &call->arg_list);
}

DEBUG_FUNC(expr, expr) {
  AST_PRINTZ("EXPRESSION");

  INDENT();
  DEBUG_CALL(tyref, &expr->var_ty);
  switch (expr->ty) {
  case AST_EXPR_TY_ATOM:
    DEBUG_CALL(atom, &expr->atom);
    break;
  case AST_EXPR_TY_CALL:
    DEBUG_CALL(call, expr->call);
    break;
  case AST_EXPR_TY_BINARY_OP:
    DEBUG_CALL(binary_op, &expr->binary_op);
    break;
  case AST_EXPR_TY_ASSG:
    DEBUG_CALL(assg, expr->assg);
    break;
  }
  UNINDENT();
}

DEBUG_FUNC(vardecl, var_decl) {
  DEBUG_CALL(var, &var_decl->var);

  if (var_decl->ty == AST_VARDECL_TY_DECL_WITH_ASSG) {
    AST_PRINTZ("WITH ASSIGNMENT");

    INDENT();
    DEBUG_CALL(expr, &var_decl->assg_expr);
    UNINDENT();
  }
}

DEBUG_FUNC(vardecllist, var_decl_list) {
  DEBUG_CALL(tyref, &var_decl_list->var_ty);

  INDENT();

  for (size_t i = 0; i < var_decl_list->num_decls; i++) {
    DEBUG_CALL(vardecl, &var_decl_list->decls[i]);
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
  }
}

DEBUG_FUNC(switchstmt, switch_stmt) {
  UNUSED_ARG(state);
  UNUSED_ARG(switch_stmt);
  todo("debug func for switch");
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

DEBUG_FUNC(vardecllist, var_decl_list);

DEBUG_FUNC(declorexpr, decl_or_expr) {
  if (decl_or_expr->decl) {
    DEBUG_CALL(vardecllist, decl_or_expr->decl);
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

DEBUG_FUNC(stmt, stmt) {
  INDENT();

  switch (stmt->ty) {
  case AST_STMT_TY_NULL:
    break;
  case AST_STMT_TY_VAR_DECL_LIST:
    DEBUG_CALL(vardecllist, &stmt->var_decl_list);
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
  AST_PRINT(" '%s' SCOPE %d", associated_text(state->parser->lexer, &param->var.identifier), param->var.scope);
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
  DEBUG_CALL(funcsig, &func_def->sig);

  DEBUG_CALL(compoundstmt, &func_def->body);
}

DEBUG_FUNC(funcdecl, func_decl) {
  AST_PRINT_SAMELINE_Z("FUNCTION DECLARATION ");
  DEBUG_CALL(funcsig, &func_decl->sig);
}

void debug_print_ast(struct parser *parser,
                     struct ast_translationunit *translation_unit) {
  struct ast_printstate state_ = {.indent = 0, .parser = parser};

  struct ast_printstate *state = &state_;

  AST_PRINTZ("PRINTING AST");

  for (size_t i = 0; i < translation_unit->num_func_decls; i++) {
    DEBUG_CALL(funcdecl, &translation_unit->func_decls[i]);
  }

  for (size_t i = 0; i < translation_unit->num_func_defs; i++) {
    DEBUG_CALL(funcdef, &translation_unit->func_defs[i]);
  }
}
