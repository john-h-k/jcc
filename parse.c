#include "parse.h"
#include "alloc.h"
#include "lex.h"
#include "log.h"
#include "util.h"
#include "vector.h"

#include <alloca.h>
#include <string.h>

struct parser {
  struct arena_allocator *arena;
  struct lexer *lexer;

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

bool parse_cnst(struct parser *parser, struct ast_cnst *cnst) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  peek_token(parser->lexer, &token);
  if (token.ty == LEX_TOKEN_TYPE_INT_LITERAL) {
    cnst->value = atoi(associated_text(parser->lexer, &token));

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
  if (parse_assg(parser, &assg)) {
    rvalue->ty = AST_RVALUE_TY_ASSG;
    rvalue->assg = alloc(parser->arena, sizeof(*rvalue->assg));
    *rvalue->assg = assg;
    return true;
  }

  struct ast_cnst cnst;
  if (parse_cnst(parser, &cnst)) {
    rvalue->ty = AST_RVALUE_TY_CNST;
    rvalue->cnst = cnst;
    return true;
  }

  // struct ast_compoundexpr compound_expr;
  // if (parse_compoundexpr(parser, &compound_expr)) {
  //   rvalue->ty = AST_RVALUE_TY_COMPOUNDEXPR;
  //   rvalue->compound_expr = compound_expr;
  //   return true;
  // }
  
  return false;
}

bool parse_lvalue(struct parser *parser, struct ast_lvalue *lvalue) {
  struct ast_var var;
  if (parse_var(parser, &var)) {
    lvalue->ty = AST_LVALUE_TY_VAR;
    lvalue->var = var;
    return true;
  }

  return false;
}

// parses an expression that does _not_ involve binary operators
bool parse_atom(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  // parenthesised expression
  if (token.ty == LEX_TOKEN_TYPE_OPEN_BRACKET) {
    consume_token(parser->lexer, token);

    struct ast_expr sub_expr;
    if (parse_expr(parser, &sub_expr)) {
      struct token token;
      peek_token(parser->lexer, &token);
      
      if (token.ty == LEX_TOKEN_TYPE_CLOSE_BRACKET) {
        consume_token(parser->lexer, token);
        *expr = sub_expr;

        return true;
      }
    }

    backtrack(parser->lexer, pos);
  }

  struct ast_rvalue rvalue;
  if (parse_rvalue_atom(parser, &rvalue)) {
    expr->ty = AST_EXPR_TY_RVALUE;
    expr->rvalue = rvalue;
    return true;
  }

  struct ast_lvalue lvalue;
  if (parse_lvalue(parser, &lvalue)) {
    expr->ty = AST_EXPR_TY_LVALUE;
    expr->lvalue = lvalue;
    return true;
  }

  return false;
}

bool parse_expr_precedence_aware(struct parser *parser, unsigned min_precedence,
                                 struct ast_expr *expr) {
  if (!parse_atom(parser, expr)) {
    return false;
  }

  // TODO: make iterative
  while (true) {
    struct token lookahead;
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

    expr->ty = AST_EXPR_TY_RVALUE;
    expr->rvalue.ty = AST_RVALUE_TY_BINARY_OP;
    struct ast_binaryop *binary_op = &expr->rvalue.binary_op;
    binary_op->ty = info.ty;

    binary_op->lhs = alloc(parser->arena, sizeof(*binary_op->lhs));
    *binary_op->lhs = lhs;

    binary_op->rhs = alloc(parser->arena, sizeof(*binary_op->rhs));
    *binary_op->rhs = rhs;
  }
}

bool parse_expr(struct parser *parser, struct ast_expr *expr) {
  return parse_expr_precedence_aware(parser, 0, expr);
}

// bool token_is_typename(const struct token *token) {
//   switch (token->ty) {
//   case LEX_TOKEN_TYPE_KW_INT:
//     return true;

//   case LEX_TOKEN_TYPE_WHITESPACE:
//   case LEX_TOKEN_TYPE_INLINE_COMMENT:
//   case LEX_TOKEN_TYPE_MULTILINE_COMMENT:
//   case LEX_TOKEN_TYPE_OPEN_BRACKET:
//   case LEX_TOKEN_TYPE_CLOSE_BRACKET:
//   case LEX_TOKEN_TYPE_OPEN_BRACE:
//   case LEX_TOKEN_TYPE_CLOSE_BRACE:
//   case LEX_TOKEN_TYPE_SEMICOLON:
//   case LEX_TOKEN_TYPE_OP_ASSG:
//   case LEX_TOKEN_TYPE_OP_ADD:
//   case LEX_TOKEN_TYPE_OP_SUB:
//   case LEX_TOKEN_TYPE_OP_MUL:
//   case LEX_TOKEN_TYPE_OP_DIV:
//   case LEX_TOKEN_TYPE_OP_QUOT:
//   case LEX_TOKEN_TYPE_KW_RETURN:
//   case LEX_TOKEN_TYPE_IDENTIFIER:
//   case LEX_TOKEN_TYPE_INT_LITERAL:
//     return false;
//   }
// }


bool parse_tyref(struct parser *parser, struct ast_tyref *ty_ref) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;
  peek_token(parser->lexer, &token);

  if (token.ty == LEX_TOKEN_TYPE_KW_INT) {
    ty_ref->ty = AST_TYREF_TY_WELL_KNOWN;
    ty_ref->well_known = WELL_KNOWN_TY_INT;

    consume_token(parser->lexer, token);

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
  
  var_decl->var = var;
  var_decl->scope = parser->cur_scope;

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
  fprintf(stderr, "trying decl list\n");
  struct ast_tyref tyref;
  if (!parse_tyref(parser, &tyref)) {
    return false;
  }

  struct vector *decls = vector_create(sizeof(struct ast_vardecl));

  struct ast_vardecl var_decl;
  while (parse_vardecl(parser, &var_decl)) {
    vector_push_back(decls, &var_decl);

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

  struct ast_vardecl *new_decls = alloc(parser->arena, vector_byte_size(decls));
  vector_copy_to(decls, new_decls);

  var_decl_list->var_ty = tyref;
  var_decl_list->num_decls = vector_length(decls);
  var_decl_list->decls = new_decls;

  vector_free(&decls);

  return true;
}

bool parse_jumpstmt(struct parser *parser, struct ast_jumpstmt *jump_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  peek_token(parser->lexer, &token);
  if (token.ty == LEX_TOKEN_TYPE_KW_RETURN) {
    consume_token(parser->lexer, token);
    debug("found return stmt");

    struct ast_expr expr;
    if (parse_expr(parser, &expr)) {
      debug("found expr");
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

bool parse_compoundstmt(struct parser *parser,
                        struct ast_compoundstmt *compound_stmt);

bool parse_stmt(struct parser *parser, struct ast_stmt *stmt) {
  struct ast_jumpstmt jump_stmt;
  if (parse_jumpstmt(parser, &jump_stmt)) {
    stmt->ty = AST_STMT_TY_JUMP;
    stmt->jump = jump_stmt;
    return true;
  }

  struct ast_compoundstmt compound_stmt;
  if (parse_compoundstmt(parser, &compound_stmt)) {
    stmt->ty = AST_STMT_TY_COMPOUND;
    stmt->compound = compound_stmt;
    return true;
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
#define PARSER_NEW_SCOPE() int _you_forgot_to_call_parser_end_scope; parser->cur_scope++;
#define PARSER_END_SCOPE() (void)_you_forgot_to_call_parser_end_scope; parser->cur_scope--;

bool parse_compoundstmt(struct parser *parser,
                        struct ast_compoundstmt *compound_stmt) {
  fprintf(stderr, "trying to parse compound stmt\n");
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

#define DEBUG_FUNC(ty, name) void debug_print_##ty (struct ast_printstate *state, struct ast_##ty *name)
#define DEBUG_CALL(ty, val) debug_print_##ty(state, val)

#define AST_PRINT_SAMELINE_Z(fmt) fprintf(stderr, "%*s" fmt, state->indent * 4, "")
#define AST_PRINT_SAMELINE(fmt, ...) fprintf(stderr, "%*s" fmt, state->indent * 4, "", __VA_ARGS__)

#define AST_PRINTZ(fmt) AST_PRINT_SAMELINE_Z(fmt "\n")
#define AST_PRINT(fmt, ...) AST_PRINT_SAMELINE(fmt "\n", __VA_ARGS__)

#define INDENT() state->indent++
#define UNINDENT() state->indent--

DEBUG_FUNC(tyref, ty_ref) {
  switch (ty_ref->ty) {
  case AST_TYREF_TY_WELL_KNOWN:
    switch (ty_ref->well_known) {      
    case WELL_KNOWN_TY_INT:
      AST_PRINTZ("int");
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
  AST_PRINT("VARIABLE '%s'", associated_text(state->parser->lexer, &var->identifier));
}

DEBUG_FUNC(lvalue, lvalue) {
  switch (lvalue->ty) {
  case AST_LVALUE_TY_VAR:
    DEBUG_CALL(var, &lvalue->var);
    break;
  }
}

DEBUG_FUNC(cnst, cnst) {
  AST_PRINT("CONSTANT '%d'", cnst->value);
}

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
  // case AST_RVALUE_TY_COMPOUNDEXPR:
  //   DEBUG_CALL(compoundexpr, &rvalue->compound_expr);
  //   break;
  // case AST_RVALUE_TY_ASSG:
  //   DEBUG_CALL(assg, rvalue->assg);
  //   break;
  }
}

DEBUG_FUNC(expr, expr) {
  switch (expr->ty) {
  case AST_EXPR_TY_LVALUE:
    DEBUG_CALL(lvalue, &expr->lvalue);
    break;
  case AST_EXPR_TY_RVALUE:
    DEBUG_CALL(rvalue, &expr->rvalue);
    break;
  }
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

void debug_print_ast(struct parser *parser, struct ast_translationunit *translation_unit) {
  struct ast_printstate state_ = {
    .indent = 0,
    .parser = parser
  };

  struct ast_printstate *state = &state_;

  AST_PRINTZ("PRINTING AST");
  
  for (size_t i = 0; i < translation_unit->num_func_decls; i++) {
    DEBUG_CALL(funcdecl, &translation_unit->func_decls[i]);
  }

  for (size_t i = 0; i < translation_unit->num_func_defs; i++) {
    DEBUG_CALL(funcdef, &translation_unit->func_defs[i]);
  }
}
