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

  int cur_scope;
};

enum parser_create_result create_parser(const char *program,
                                        struct parser **parser) {
  struct parser *p = nonnull_malloc(sizeof(*p));

  create_arena_allocator(&p->arena);
  if (create_lexer(program, &p->lexer) != LEX_STATUS_SUCCESS) {
    err("failed to create lexer");
    return PARSER_CREATE_RESULT_FAILURE;
  }

  p->cur_scope = SCOPE_GLOBAL;
  p->var_table = var_table_create(p);

  *parser = p;

  return PARSER_CREATE_RESULT_SUCCESS;
}

void free_parser(struct parser **parser) {
  free_lexer(&(*parser)->lexer);

  free_arena_allocator(&(*parser)->arena);
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
  case AST_BINARY_OP_TY_ADD:
  case AST_BINARY_OP_TY_SUB:
    info.precedence = 1;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  case AST_BINARY_OP_TY_MUL:
  case AST_BINARY_OP_TY_DIV:
  case AST_BINARY_OP_TY_QUOT:
    info.precedence = 2;
    info.associativity = AST_ASSOCIATIVITY_LEFT;
    break;
  default:
    unreachable("invalid `ast_binary_op_ty`");
  }

  return info;
}

bool op_info_for_token(const struct token *token, struct ast_op_info *info) {
  switch (token->ty) {
  case LEX_TOKEN_TYPE_OP_ADD:
    *info = op_info(AST_BINARY_OP_TY_ADD);
    return true;
  case LEX_TOKEN_TYPE_OP_SUB:
    *info = op_info(AST_BINARY_OP_TY_SUB);
    return true;
  case LEX_TOKEN_TYPE_OP_MUL:
    *info = op_info(AST_BINARY_OP_TY_MUL);
    return true;
  case LEX_TOKEN_TYPE_OP_DIV:
    *info = op_info(AST_BINARY_OP_TY_DIV);
    return true;
  case LEX_TOKEN_TYPE_OP_QUOT:
    *info = op_info(AST_BINARY_OP_TY_QUOT);
    return true;
  default:
    // not an op
    return false;
  }
}

bool parse_var(struct parser *parser, struct ast_var *var) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty != LEX_TOKEN_TYPE_IDENTIFIER) {
    backtrack(parser->lexer, pos);
    return false;
  }

  var->identifier = token;
  var->scope = parser->cur_scope;

  consume_token(parser->lexer, token);

  return true;
}

bool is_literal_token(enum lex_token_type tok_ty,
                      enum well_known_ty *well_known_ty) {
  switch (tok_ty) {
  case LEX_TOKEN_TYPE_UNKNOWN:
  case LEX_TOKEN_TYPE_EOF:
  case LEX_TOKEN_TYPE_WHITESPACE:
  case LEX_TOKEN_TYPE_INLINE_COMMENT:
  case LEX_TOKEN_TYPE_MULTILINE_COMMENT:
  case LEX_TOKEN_TYPE_OPEN_BRACKET:
  case LEX_TOKEN_TYPE_CLOSE_BRACKET:
  case LEX_TOKEN_TYPE_OPEN_BRACE:
  case LEX_TOKEN_TYPE_CLOSE_BRACE:
  case LEX_TOKEN_TYPE_SEMICOLON:
  case LEX_TOKEN_TYPE_COMMA:
  case LEX_TOKEN_TYPE_OP_ASSG:
  case LEX_TOKEN_TYPE_OP_ADD:
  case LEX_TOKEN_TYPE_OP_SUB:
  case LEX_TOKEN_TYPE_OP_MUL:
  case LEX_TOKEN_TYPE_OP_DIV:
  case LEX_TOKEN_TYPE_OP_QUOT:
  case LEX_TOKEN_TYPE_KW_IF:
  case LEX_TOKEN_TYPE_KW_ELSE:
  case LEX_TOKEN_TYPE_KW_CHAR:
  case LEX_TOKEN_TYPE_KW_SHORT:
  case LEX_TOKEN_TYPE_KW_INT:
  case LEX_TOKEN_TYPE_KW_LONG:
  case LEX_TOKEN_TYPE_KW_UNSIGNED:
  case LEX_TOKEN_TYPE_KW_SIGNED:
  case LEX_TOKEN_TYPE_KW_RETURN:
  case LEX_TOKEN_TYPE_IDENTIFIER:
    return false;

  case LEX_TOKEN_TYPE_ASCII_CHAR_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_ASCII_CHAR;
    return true;

  case LEX_TOKEN_TYPE_SIGNED_INT_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_SIGNED_INT;
    return true;

  case LEX_TOKEN_TYPE_UNSIGNED_INT_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_UNSIGNED_INT;
    return true;

  case LEX_TOKEN_TYPE_SIGNED_LONG_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_SIGNED_LONG;
    return true;

  case LEX_TOKEN_TYPE_UNSIGNED_LONG_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_UNSIGNED_LONG;
    return true;

  case LEX_TOKEN_TYPE_SIGNED_LONG_LONG_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_SIGNED_LONG_LONG;
    return true;

  case LEX_TOKEN_TYPE_UNSIGNED_LONG_LONG_LITERAL:
    *well_known_ty = WELL_KNOWN_TY_UNSIGNED_LONG_LONG;
    return true;
  }
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

bool parse_lvalue(struct parser *parser, struct ast_lvalue *lvalue);

bool parse_expr(struct parser *parser, struct ast_expr *expr);

bool parse_assg(struct parser *parser, struct ast_assg *assg) {
  struct text_pos pos = get_position(parser->lexer);
  debug("trying assg");

  struct ast_lvalue lvalue;
  if (!parse_lvalue(parser, &lvalue)) {
    backtrack(parser->lexer, pos);
    return false;
  }
  debug("lvalue");

  struct token token;
  peek_token(parser->lexer, &token);
  if (token.ty != LEX_TOKEN_TYPE_OP_ASSG) {
    backtrack(parser->lexer, pos);
    return false;
  }
  debug("assg");

  consume_token(parser->lexer, token);

  struct ast_expr expr;
  if (!parse_expr(parser, &expr)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  debug("found expr");

  assg->lvalue = lvalue;
  assg->expr = alloc(parser->arena, sizeof(*assg->expr));
  *assg->expr = expr;

  return true;
}

bool parse_rvalue_atom(struct parser *parser, struct ast_rvalue *rvalue) {
  struct ast_assg assg;
  debug("trying assg");
  if (parse_assg(parser, &assg)) {
    rvalue->ty = AST_RVALUE_TY_ASSG;
    rvalue->var_ty = assg.expr->var_ty;
    rvalue->assg = alloc(parser->arena, sizeof(*rvalue->assg));
    *rvalue->assg = assg;
    return true;
  }

  debug("trying cnst");
  struct ast_cnst cnst;
  if (parse_cnst(parser, &cnst)) {
    rvalue->ty = AST_RVALUE_TY_CNST;
    rvalue->var_ty.ty = AST_TYREF_TY_WELL_KNOWN;
    rvalue->var_ty.well_known = cnst.cnst_ty;
    rvalue->cnst = cnst;
    return true;
  }

  // struct ast_compoundexpr compound_expr;
  // if (parse_compoundexpr(parser, &compound_expr)) {
  //   rvalue->ty = AST_RVALUE_TY_COMPOUNDEXPR;
  //   // compound expressions return their last expression
  //   rvalue->var_ty = compound_expr.exprs[compound_expr.num_exprs - 1].var_ty;
  //   rvalue->compound_expr = compound_expr;
  //   return true;
  // }

  return false;
}

struct ast_tyref get_var_type(struct parser *parser,
                              const struct ast_var *var) {
  struct var_table_entry *entry = get_or_create_entry(&parser->var_table, var);

  if (entry->value) {
    debug("var %s was found, type", identifier_str(parser, &var->identifier));
    return *(struct ast_tyref *)entry->value;
  } else {
    debug("var %s was not found, unknown type",
          identifier_str(parser, &var->identifier));
    struct ast_tyref ty_ref = {.ty = AST_TYREF_TY_UNKNOWN};
    return ty_ref;
  }
}

bool parse_lvalue(struct parser *parser, struct ast_lvalue *lvalue) {
  debug("trying lvalue");
  struct ast_var var;
  if (parse_var(parser, &var)) {
    debug("found var");
    lvalue->ty = AST_LVALUE_TY_VAR;
    lvalue->var_ty = get_var_type(parser, &var);
    lvalue->var = var;
    return true;
  }

  return false;
}

bool parse_compoundexpr(struct parser *parser,
                        struct ast_compoundexpr *compound_expr);

// parses an expression that does _not_ involve binary operators
bool parse_atom(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  // parenthesised expression
  if (token.ty == LEX_TOKEN_TYPE_OPEN_BRACKET) {
    consume_token(parser->lexer, token);

    struct ast_compoundexpr compound_expr;
    if (parse_compoundexpr(parser, &compound_expr)) {
      struct token token;
      peek_token(parser->lexer, &token);

      if (token.ty == LEX_TOKEN_TYPE_CLOSE_BRACKET) {
        consume_token(parser->lexer, token);

        // compound expressions return the last expression
        struct ast_tyref var_ty =
            compound_expr.exprs[compound_expr.num_exprs - 1].var_ty;
        expr->ty = AST_EXPR_TY_RVALUE;
        expr->var_ty = var_ty;
        expr->rvalue.ty = AST_RVALUE_TY_COMPOUNDEXPR;
        expr->rvalue.var_ty = var_ty;
        expr->rvalue.compound_expr = compound_expr;

        return true;
      }
    }

    backtrack(parser->lexer, pos);
  }

  debug("trying rvalue");
  struct ast_rvalue rvalue;
  if (parse_rvalue_atom(parser, &rvalue)) {
    expr->ty = AST_EXPR_TY_RVALUE;
    expr->var_ty = rvalue.var_ty;
    expr->rvalue = rvalue;
    return true;
  }

  debug("trying lvalue");
  struct ast_lvalue lvalue;
  if (parse_lvalue(parser, &lvalue)) {
    expr->ty = AST_EXPR_TY_LVALUE;
    expr->var_ty = lvalue.var_ty;
    expr->lvalue = lvalue;
    return true;
  }

  return false;
}

struct ast_tyref resolve_binary_op_types(const struct ast_tyref *lhs,
                                         const struct ast_tyref *rhs) {
  if (true || (lhs->ty == AST_TYREF_TY_WELL_KNOWN &&
               rhs->ty == AST_TYREF_TY_WELL_KNOWN)) {
    struct ast_tyref result_ty;
    result_ty.ty = AST_TYREF_TY_WELL_KNOWN;

    if (lhs->well_known == rhs->well_known) {
      // they are the same type
      result_ty.well_known = lhs->well_known;
    } else {
      if (lhs->well_known == WELL_KNOWN_TY_ASCII_CHAR ||
          rhs->well_known == WELL_KNOWN_TY_ASCII_CHAR) {
        todo("handle ASCII CHAR signedness");
      }

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

    return result_ty;
  }

  // todo("`resolve_binary_op_types` for types other than well known");
}

bool parse_expr_precedence_aware(struct parser *parser, unsigned min_precedence,
                                 struct ast_expr *expr) {
  if (!parse_atom(parser, expr)) {
    return false;
  }

  // TODO: make iterative
  while (true) {
    struct token lookahead;
    debug("loop iter");
    peek_token(parser->lexer, &lookahead);
    struct ast_op_info info;

    if (!op_info_for_token(&lookahead, &info) ||
        info.precedence <= min_precedence) {
      return true;
    }

    consume_token(parser->lexer, lookahead);

    debug_assert(info.associativity != AST_ASSOCIATIVITY_NONE,
                 "only operators with associativity should reach here!");
    unsigned next_min_precedence;
    if (info.associativity == AST_ASSOCIATIVITY_LEFT) {
      next_min_precedence = min_precedence + 1;
    } else {
      next_min_precedence = min_precedence;
    }

    struct ast_expr rhs;
    parse_expr_precedence_aware(parser, next_min_precedence, &rhs);

    // slightly odd design where `expr` now contains lhs and `rhs` contains
    // `rhs` so we need to in-place modify `expr`
    struct ast_expr lhs = *expr;

    struct ast_tyref result_ty =
        resolve_binary_op_types(&lhs.var_ty, &rhs.var_ty);

    expr->ty = AST_EXPR_TY_RVALUE;
    expr->var_ty = result_ty;
    expr->rvalue.ty = AST_RVALUE_TY_BINARY_OP;
    struct ast_binaryop *binary_op = &expr->rvalue.binary_op;
    binary_op->ty = info.ty;
    binary_op->var_ty = result_ty;

    binary_op->lhs = alloc(parser->arena, sizeof(*binary_op->lhs));
    *binary_op->lhs = lhs;

    binary_op->rhs = alloc(parser->arena, sizeof(*binary_op->rhs));
    *binary_op->rhs = rhs;
  }
}

// parse a non-compound expression
bool parse_expr(struct parser *parser, struct ast_expr *expr) {
  return parse_expr_precedence_aware(parser, 0, expr);
}

// there are only two places you can have compound expressions
// * at top level of a statement (e.g `a = 1, b = 2;`)
// * within braces (e.g `(a = 1, b = 2)`)
// so only those places call this method
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
  } while (token.ty == LEX_TOKEN_TYPE_COMMA &&
           /* hacky */ (consume_token(parser->lexer, token), true));

  if (vector_empty(exprs)) {
    backtrack(parser->lexer, pos);
    return false;
  }

  compound_expr->exprs = alloc(parser->arena, vector_byte_size(exprs));
  compound_expr->num_exprs = vector_length(exprs);

  vector_copy_to(exprs, compound_expr->exprs);
  vector_free(&exprs);

  return true;
}

// used when parsing type names, as `short int` is equivalent to `short`
void skip_int_token_if_present(struct parser *parser) {
  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TYPE_KW_INT) {
    consume_token(parser->lexer, token);
  }
}

bool parse_integer_size_name(struct parser *parser, enum well_known_ty *wkt) {
  struct token token;
  peek_token(parser->lexer, &token);

  switch (token.ty) {
  case LEX_TOKEN_TYPE_KW_CHAR:
    consume_token(parser->lexer, token);
    *wkt = WELL_KNOWN_TY_ASCII_CHAR;
    return true;
  case LEX_TOKEN_TYPE_KW_SHORT:
    consume_token(parser->lexer, token);
    *wkt = WELL_KNOWN_TY_SIGNED_SHORT;
    skip_int_token_if_present(parser);
    return true;
  case LEX_TOKEN_TYPE_KW_INT:
    consume_token(parser->lexer, token);
    *wkt = WELL_KNOWN_TY_SIGNED_INT;
    return true;
  case LEX_TOKEN_TYPE_KW_LONG: {
    consume_token(parser->lexer, token);

    struct token maybe_other_long;
    peek_token(parser->lexer, &maybe_other_long);

    if (token.ty == LEX_TOKEN_TYPE_KW_LONG) {
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
  if (token.ty == LEX_TOKEN_TYPE_KW_SIGNED) {
    seen_signed = true;

    consume_token(parser->lexer, token);
  } else if (token.ty == LEX_TOKEN_TYPE_KW_UNSIGNED) {
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
      // unsigned variants are signed variants + 1
      wkt++;
    } else if (!seen_signed && !seen_unsigned &&
               wkt == WELL_KNOWN_TY_SIGNED_CHAR) {
      // no explicit sign or unsign, so ASCII CHAR
      wkt = WELL_KNOWN_TY_ASCII_CHAR;
    }
  }

  ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
  ty_ref->well_known = wkt;

  return true;
}

bool parse_vardecl(struct parser *parser, struct ast_vardecl *var_decl) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_var var;

  if (!parse_var(parser, &var)) {
    return false;
  }

  var_decl->var = var;

  struct text_pos post_var_pos = get_position(parser->lexer);
  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty != LEX_TOKEN_TYPE_OP_ASSG) {
    // normal var decl, without assignment
    backtrack(parser->lexer, post_var_pos);

    var_decl->ty = AST_VARDECL_TY_DECL;

    trace("normal decl");
    return true;
  }

  consume_token(parser->lexer, token);

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

    trace("creating var_table_entry for var name=%s, scope=%d",
          identifier_str(parser, &var_decl.var.identifier), var_decl.var.scope);
    struct var_table_entry *entry =
        create_entry(&parser->var_table, &var_decl.var);

    // copy the type ref into arena memory for lifetime simplicity
    entry->value = alloc(parser->arena, sizeof(ty_ref));
    *(struct ast_tyref *)entry->value = ty_ref;

    struct token token;
    peek_token(parser->lexer, &token);

    if (token.ty == LEX_TOKEN_TYPE_COMMA) {
      // another decl
      consume_token(parser->lexer, token);
      continue;
    } else if (token.ty == LEX_TOKEN_TYPE_SEMICOLON) {
      consume_token(parser->lexer, token);
      break;
    }
  }

  if (vector_length(decls) == 0) {
    vector_free(&decls);
    return false;
  }

  struct ast_vardecl *new_decls = alloc(parser->arena, vector_byte_size(decls));
  vector_copy_to(decls, new_decls);

  var_decl_list->var_ty = ty_ref;
  var_decl_list->num_decls = vector_length(decls);
  var_decl_list->decls = new_decls;

  vector_free(&decls);

  return true;
}

bool parse_jumpstmt(struct parser *parser, struct ast_jumpstmt *jump_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  // FIXME: support return without expression
  peek_token(parser->lexer, &token);
  if (token.ty == LEX_TOKEN_TYPE_KW_RETURN) {
    consume_token(parser->lexer, token);

    struct ast_expr expr;
    if (parse_expr(parser, &expr)) {
      peek_token(parser->lexer, &token);

      if (token.ty == LEX_TOKEN_TYPE_SEMICOLON) {
        jump_stmt->ty = AST_JUMPSTMT_TY_RETURN;
        jump_stmt->ret_expr = expr;

        consume_token(parser->lexer, token);
        return true;
      }
    }
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_stmt(struct parser *parser, struct ast_stmt *stmt);

bool parse_ifstmt(struct parser *parser, struct ast_ifstmt *if_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  peek_token(parser->lexer, &token);
  if (token.ty == LEX_TOKEN_TYPE_KW_IF) {
    consume_token(parser->lexer, token);

    struct ast_expr expr;
    struct ast_stmt stmt;
    if (parse_expr(parser, &expr) && parse_stmt(parser, &stmt)) {
      if_stmt->condition = expr;
      if_stmt->body = alloc(parser->arena, sizeof(*if_stmt->body));
      *if_stmt->body = stmt;
      return true;
    }
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_ifelsestmt(struct parser *parser,
                      struct ast_ifelsestmt *if_else_stmt) {
  // parse `if {}`, then try parse `else`
  // not perfectly efficient but more elegant

  struct text_pos pos = get_position(parser->lexer);

  struct ast_ifstmt if_stmt;
  if (!parse_ifstmt(parser, &if_stmt)) {
    return false;
  }

  struct token token;

  peek_token(parser->lexer, &token);
  if (token.ty == LEX_TOKEN_TYPE_KW_ELSE) {
    consume_token(parser->lexer, token);

    struct ast_stmt else_stmt;
    if (parse_stmt(parser, &else_stmt)) {
      if_else_stmt->condition = if_stmt.condition;
      if_else_stmt->body = alloc(parser->arena, sizeof(*if_else_stmt->body));
      if_else_stmt->body = if_stmt.body;
      if_else_stmt->else_body =
          alloc(parser->arena, sizeof(*if_else_stmt->else_body));
      *if_else_stmt->else_body = else_stmt;
      return true;
    }
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_switchstmt(struct parser *parser,
                      struct ast_switchstmt *switch_stmt) {
  UNUSED_ARG(parser);
  UNUSED_ARG(switch_stmt);
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

  struct ast_compoundstmt compound_stmt;
  if (parse_compoundstmt(parser, &compound_stmt)) {
    stmt->ty = AST_STMT_TY_COMPOUND;
    stmt->compound = compound_stmt;
    return true;
  }

  struct text_pos pos = get_position(parser->lexer);

  struct ast_compoundexpr compound_expr;
  if (parse_compoundexpr(parser, &compound_expr)) {
    struct token token;
    peek_token(parser->lexer, &token);

    if (token.ty == LEX_TOKEN_TYPE_SEMICOLON) {
      consume_token(parser->lexer, token);

      // compound expressions return the last expression
      struct ast_tyref var_ty =
          compound_expr.exprs[compound_expr.num_exprs - 1].var_ty;
      stmt->ty = AST_STMT_TY_EXPR;
      stmt->expr.ty = AST_EXPR_TY_RVALUE;
      stmt->expr.var_ty = var_ty;
      stmt->expr.rvalue.ty = AST_RVALUE_TY_COMPOUNDEXPR;
      stmt->expr.rvalue.var_ty = var_ty;
      stmt->expr.rvalue.compound_expr = compound_expr;

      return true;
    }

    backtrack(parser->lexer, pos);
  }

  struct ast_expr expr;
  if (parse_expr(parser, &expr)) {
    struct token token;
    peek_token(parser->lexer, &token);

    if (token.ty == LEX_TOKEN_TYPE_SEMICOLON) {
      consume_token(parser->lexer, token);

      stmt->ty = AST_STMT_TY_EXPR;
      stmt->expr = expr;
      return true;
    }

    backtrack(parser->lexer, pos);
  }

  struct ast_vardecllist var_decl_list;
  if (parse_vardecllist(parser, &var_decl_list)) {
    stmt->ty = AST_STMT_TY_VAR_DECL_LIST;
    stmt->var_decl_list = var_decl_list;
    return true;
  }

  return false;
}

// Not the most elegant, but this helps prevent mismatched scope calls
#define PARSER_NEW_SCOPE()                                                     \
  int _you_forgot_to_call_parser_end_scope;                                    \
  parser->cur_scope++;
#define PARSER_END_SCOPE()                                                     \
  (void)_you_forgot_to_call_parser_end_scope;                                  \
  parser->cur_scope--;

bool parse_compoundstmt(struct parser *parser,
                        struct ast_compoundstmt *compound_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct token open_brace;

  peek_token(parser->lexer, &open_brace);
  if (open_brace.ty != LEX_TOKEN_TYPE_OPEN_BRACE) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, open_brace);

  PARSER_NEW_SCOPE();

  struct vector *stmts = vector_create(sizeof(struct ast_stmt));
  struct ast_stmt stmt;
  while (parse_stmt(parser, &stmt)) {
    vector_push_back(stmts, &stmt);
  }

  compound_stmt->stmts = alloc(parser->arena, vector_byte_size(stmts));
  compound_stmt->num_stmts = vector_length(stmts);
  vector_copy_to(stmts, compound_stmt->stmts);
  vector_free(&stmts);

  PARSER_END_SCOPE();

  struct token close_brace;

  peek_token(parser->lexer, &close_brace);
  if (close_brace.ty != LEX_TOKEN_TYPE_CLOSE_BRACE) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, close_brace);

  return true;
}

bool parse_identifier(struct parser *parser, struct token *token) {
  struct text_pos pos = get_position(parser->lexer);

  peek_token(parser->lexer, token);

  if (token->ty == LEX_TOKEN_TYPE_IDENTIFIER) {
    consume_token(parser->lexer, *token);

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_arglist(struct parser *parser, struct ast_arglist *arg_list) {
  struct text_pos pos = get_position(parser->lexer);

  struct token open_bracket;

  // TOOD: support args
  peek_token(parser->lexer, &open_bracket);
  if (open_bracket.ty != LEX_TOKEN_TYPE_OPEN_BRACKET) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, open_bracket);

  struct token close_bracket;
  peek_token(parser->lexer, &close_bracket);
  if (close_bracket.ty != LEX_TOKEN_TYPE_CLOSE_BRACKET) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, close_bracket);

  // arglist has no fields yet
  UNUSED_ARG(arg_list);
  return true;
}

bool parse_funcsig(struct parser *parser, struct ast_funcsig *func_sig) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_tyref ty_ref;
  struct token identifier;
  struct ast_arglist arg_list;

  if (parse_tyref(parser, &ty_ref) && parse_identifier(parser, &identifier) &&
      parse_arglist(parser, &arg_list)) {
    func_sig->name = identifier;
    func_sig->ret_ty = ty_ref;
    func_sig->arg_list = arg_list;

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_funcdecl(struct parser *parser, struct ast_funcdecl *func_decl) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_funcsig func_sig;

  if (parse_funcsig(parser, &func_sig)) {
    struct token token;

    peek_token(parser->lexer, &token);

    if (token.ty == LEX_TOKEN_TYPE_SEMICOLON) {
      func_decl->sig = func_sig;
      return true;
    }
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_funcdef(struct parser *parser, struct ast_funcdef *func_def) {
  struct text_pos pos = get_position(parser->lexer);

  struct ast_funcsig func_sig;
  struct ast_compoundstmt func_body;

  if (parse_funcsig(parser, &func_sig) &&
      parse_compoundstmt(parser, &func_body)) {
    func_def->sig = func_sig;
    func_def->body = func_body;

    return true;
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
};

#define DEBUG_FUNC(ty, name)                                                   \
  void debug_print_##ty(struct ast_printstate *state, struct ast_##ty *name)
#define DEBUG_CALL(ty, val) debug_print_##ty(state, val)

#define AST_PRINT_SAMELINE_Z(fmt)                                              \
  slogsl("%*s" fmt, state->indent * 4, "")
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
  case AST_TYREF_TY_WELL_KNOWN:
    switch (ty_ref->well_known) {
    case WELL_KNOWN_TY_ASCII_CHAR:
      AST_PRINTZ("char");
      break;
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
  AST_PRINT("VARIABLE '%s'",
            associated_text(state->parser->lexer, &var->identifier));
}

DEBUG_FUNC(lvalue, lvalue) {
  switch (lvalue->ty) {
  case AST_LVALUE_TY_VAR:
    DEBUG_CALL(var, &lvalue->var);
    break;
  }
}

DEBUG_FUNC(cnst, cnst) { AST_PRINT("CONSTANT '%llu'", cnst->value); }

DEBUG_FUNC(binaryop, binary_op) {
  switch (binary_op->ty) {
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
  DEBUG_CALL(lvalue, &assg->lvalue);

  AST_PRINTZ("ASSIGNED");
  INDENT();
  DEBUG_CALL(expr, assg->expr);
  UNINDENT();

  UNINDENT();
}

DEBUG_FUNC(rvalue, rvalue) {
  switch (rvalue->ty) {
  case AST_RVALUE_TY_CNST:
    DEBUG_CALL(cnst, &rvalue->cnst);
    break;
  case AST_RVALUE_TY_ASSG:
    DEBUG_CALL(assg, rvalue->assg);
    break;
  case AST_RVALUE_TY_BINARY_OP:
    DEBUG_CALL(binaryop, &rvalue->binary_op);
    break;
  case AST_RVALUE_TY_COMPOUNDEXPR:
    DEBUG_CALL(compoundexpr, &rvalue->compound_expr);
    break;
    // case AST_RVALUE_TY_ASSG:
    //   DEBUG_CALL(assg, rvalue->assg);
    //   break;
  }
}

DEBUG_FUNC(expr, expr) {
  AST_PRINTZ("EXPRESSION");

  INDENT();
  DEBUG_CALL(tyref, &expr->var_ty);
  switch (expr->ty) {
  case AST_EXPR_TY_LVALUE:
    DEBUG_CALL(lvalue, &expr->lvalue);
    break;
  case AST_EXPR_TY_RVALUE:
    DEBUG_CALL(rvalue, &expr->rvalue);
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
    DEBUG_CALL(expr, &jump_stmt->ret_expr);
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

DEBUG_FUNC(stmt, stmt) {
  INDENT();

  switch (stmt->ty) {
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

DEBUG_FUNC(arglist, arg_list) {
  AST_PRINTZ("ARGLIST:");
  INDENT();

  UNUSED_ARG(arg_list);
  // TODO
  AST_PRINTZ("void");

  UNINDENT();
}

DEBUG_FUNC(funcsig, func_sig) {
  AST_PRINT("'%s'", associated_text(state->parser->lexer, &func_sig->name));
  AST_PRINTZ("RETURNS: ");

  INDENT();
  DEBUG_CALL(tyref, &func_sig->ret_ty);
  UNINDENT();

  DEBUG_CALL(arglist, &func_sig->arg_list);
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
