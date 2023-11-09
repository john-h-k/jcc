#include "parse.h"
#include "alloc.h"
#include "lex.h"
#include "log.h"
#include "util.h"
#include "vector.h"

#include <string.h>

struct parser {
  struct arena_allocator *arena;
  struct lexer *lexer;
};

enum parser_create_result create_parser(const char *program, struct parser **parser) {
  struct parser *p = nonnull_malloc(sizeof(*p));

  create_arena_allocator(&p->arena);
  if (create_lexer(program, &p->lexer) != LEX_STATUS_SUCCESS) {
    err("failed to create lexer");
    return PARSER_CREATE_RESULT_FAILURE;
  }

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

enum ast_atom_ty {
  AST_ATOM_TY_CNST,
};

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
  struct ast_op_info info = { .ty = ty };

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

// parses an expression that does _not_ involve binary operators
bool parse_atom(struct parser *parser, struct ast_expr *expr) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  peek_token(parser->lexer, &token);
  if (token.ty == LEX_TOKEN_TYPE_INT_LITERAL) {
    struct ast_cnst cnst;

    cnst.value = atoi(associated_text(parser->lexer, &token));
    
    expr->ty = AST_EXPR_TY_CNST;
    expr->cnst = cnst;

    consume_token(parser->lexer, token);
    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_expr_precedence_aware(struct parser *parser, unsigned min_precedence, struct ast_expr* expr) {
  if (!parse_atom(parser, expr)) {
    return false;
  }
  
  // TODO: make iterative
  while (true) {
    struct token lookahead;
    peek_token(parser->lexer, &lookahead);
    struct ast_op_info info;
    err("lookahead ty %d", lookahead.ty);

    if (!op_info_for_token(&lookahead, &info) || info.precedence <= min_precedence) {
      err("Exiting");
      return true;
    }

    consume_token(parser->lexer, lookahead);

    debug_assert(info.associativity != AST_ASSOCIATIVITY_NONE, "only operators with associativity should reach here!");
    unsigned next_min_precedence;
    if (info.associativity == AST_ASSOCIATIVITY_LEFT) {
      next_min_precedence = min_precedence + 1;
    } else {
      next_min_precedence = min_precedence;
    }

    struct ast_expr rhs;
    parse_expr_precedence_aware(parser, next_min_precedence, &rhs);

    // slightly odd design where `expr` now contains lhs and `rhs` contains `rhs`
    // so we need to in-place modify `expr`
    struct ast_expr lhs = *expr;
    
    expr->ty = AST_EXPR_TY_BINARY_OP;
    struct ast_binary_op *binary_op = &expr->binary_op;
    binary_op->ty = info.ty;

    binary_op->lhs = alloc(parser->arena, sizeof(*binary_op->lhs));
    *binary_op->lhs = lhs;

    binary_op->rhs = alloc(parser->arena, sizeof(*binary_op->rhs));
    *binary_op->rhs = rhs;

    err("%d", binary_op->lhs->ty);
    err("%d", binary_op->rhs->ty);
  }
}

bool parse_expr(struct parser *parser, struct ast_expr *expr) {
  return parse_expr_precedence_aware(parser, 0, expr);
}

bool parse_stmt(struct parser *parser, struct ast_stmt *stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct token token;

  peek_token(parser->lexer, &token);
  if (token.ty == LEX_TOKEN_TYPE_KW_RETURN) {
    consume_token(parser->lexer, token);

    struct ast_expr expr;
    if (parse_expr(parser, &expr)) {
      peek_token(parser->lexer, &token);

      if (token.ty == LEX_TOKEN_TYPE_SEMICOLON) {
        stmt->ty = AST_STMT_TY_RET;
        stmt->ret = expr;

        consume_token(parser->lexer, token);
        return true;
      }
    }
  }

  backtrack(parser->lexer, pos);
  return false;
}

bool parse_compoundstmt(struct parser *parser, struct ast_compoundstmt *compound_stmt) {
  struct text_pos pos = get_position(parser->lexer);

  struct token open_brace;

  peek_token(parser->lexer, &open_brace);
  if (open_brace.ty != LEX_TOKEN_TYPE_OPEN_BRACE) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, open_brace);

  // very hacky
  size_t stmts_len = 64;
  size_t stmts_count = 0;
  struct ast_stmt *stmts = alloc(parser->arena, sizeof(*stmts) * stmts_len);

  struct ast_stmt stmt;
  while (parse_stmt(parser, &stmt)) {
    if (stmts_count < stmts_len) {
      stmts[stmts_count++] = stmt;
    } else {
      trace("reallocing `stmts` buffer");
      stmts_len *= 2;
      struct ast_stmt *new_stmts = alloc(parser->arena, sizeof(*stmts) * stmts_len);

      memcpy(new_stmts, stmts, sizeof(*stmts) * stmts_count);
      // no leak as lifetime tied to arena
      stmts = new_stmts;
    }
  }

  struct token close_brace;

  peek_token(parser->lexer, &close_brace);
  if (close_brace.ty != LEX_TOKEN_TYPE_CLOSE_BRACE) {
    backtrack(parser->lexer, pos);
    return false;
  }

  consume_token(parser->lexer, close_brace);

  compound_stmt->stmts = stmts;
  compound_stmt->num_stmts = stmts_count;

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

  if (parse_tyref(parser, &ty_ref) && parse_identifier(parser, &identifier) && parse_arglist(parser, &arg_list)) {
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

  if (parse_funcsig(parser, &func_sig) && parse_compoundstmt(parser, &func_body)) {
    func_def->sig = func_sig;
    func_def->body = func_body;

    return true;
  }

  backtrack(parser->lexer, pos);
  return false;
}

#define TOKEN_FMT(lexer, token) text_pos_len((token).start, (token).end), text_pos_len((token).start, (token).end), lexer->text[(token).start.idx]

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
      info("found func declaration '%s'", associated_text(lexer, &func_decl.sig.name));
      vector_push_back(decls, &func_decl);
    } else if (parse_funcdef(parser, &func_def)) {
      info("found func definition '%s'", associated_text(lexer, &func_def.sig.name));
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
   
  struct parse_result result = {
    .translation_unit = translation_unit
  };

  return result;
}

