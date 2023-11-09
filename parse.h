#ifndef __PARSE_H__
#define __PARSE_H__

#include "util.h"
#include "alloc.h"
#include "log.h"
#include "lex.h"

enum well_known_ty {
  WELL_KNOWN_TY_INT
};

enum ast_tyref_ty {
  AST_TYREF_TY_WELL_KNOWN,
};

struct ast_tyref {
  enum ast_tyref_ty ty;

  union {
    enum well_known_ty well_known;
  };
};

struct ast_arglist {
  void* DUMMY;
};

struct ast_funcsig {
  struct ast_tyref ret_ty;
  struct token name;
  struct ast_arglist arg_list;

  // TODO: args
};

struct ast_cnst {
  int value;
};

struct ast_expr;

enum ast_binary_op_ty {
  AST_BINARY_OP_TY_ADD,
  AST_BINARY_OP_TY_SUB,
  AST_BINARY_OP_TY_MUL,
  AST_BINARY_OP_TY_DIV,
  AST_BINARY_OP_TY_QUOT
};

struct ast_binary_op {
  enum ast_binary_op_ty ty;
  struct ast_expr* lhs;
  struct ast_expr* rhs;
};

enum ast_expr_ty {
  AST_EXPR_TY_CNST,
  AST_EXPR_TY_BINARY_OP,
};

struct ast_expr {
  enum ast_expr_ty ty;
  union {
    struct ast_cnst cnst;
    struct ast_binary_op binary_op;
  };
};

enum ast_stmt_ty {
  AST_STMT_TY_RET,
};

struct ast_stmt {
  enum ast_stmt_ty ty;
  union {
    struct ast_expr ret;
  };
};

struct ast_compoundstmt {
  struct ast_stmt* stmts;
  size_t num_stmts;
};

struct ast_funcdef {
  struct ast_funcsig sig;
  struct ast_compoundstmt body;
};

struct ast_funcdecl {
  struct ast_funcsig sig;
};

struct ast_translationunit {
  struct ast_funcdef* func_defs;
  size_t num_func_defs;

  struct ast_funcdecl* func_decls;
  size_t num_func_decls;
};

struct parser;

enum parser_create_result {
  PARSER_CREATE_RESULT_SUCCESS,
  PARSER_CREATE_RESULT_FAILURE
};

struct parse_result {
  struct ast_translationunit translation_unit;
};

enum parser_create_result create_parser(const char *program, struct parser **parser);
struct parse_result parse(struct parser *parser);
void free_parser(struct parser** parser);

#endif
