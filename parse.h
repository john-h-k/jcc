#ifndef __PARSE_H__
#define __PARSE_H__

#include "util.h"
#include "alloc.h"
#include "log.h"
#include "lex.h"

/* Well known types - keywords (`int`, `unsigned`, etc) */

enum well_known_ty {
  WELL_KNOWN_TY_INT
};

/* Type refs - `<enum|struct|union> <identifier`, `<typedef-name>`, or `<keyword>` */

enum ast_tyref_ty {
  AST_TYREF_TY_WELL_KNOWN,
  // AST_TYREF_TY_TYPEDEF_NAME,
  // AST_TYREF_TY_STRUCT,
  // AST_TYREF_TY_UNION,
  // AST_TYREF_TY_ENUM,
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

/* Constant values (literals) */

struct ast_cnst {
  int value;
};

/* Binary expressions - `a <OP> b` */

struct ast_expr;

enum ast_binary_op_ty {
  AST_BINARY_OP_TY_ADD,
  AST_BINARY_OP_TY_SUB,
  AST_BINARY_OP_TY_MUL,
  AST_BINARY_OP_TY_DIV,
  AST_BINARY_OP_TY_QUOT
};

struct ast_binaryop {
  enum ast_binary_op_ty ty;
  struct ast_expr* lhs;
  struct ast_expr* rhs;
};

/* Variable references */

struct ast_var {
  struct token identifier;
};

/* Compound expr - comma seperated expressions with well-defined order of execution */

struct ast_compoundexpr {
  struct ast_expr *exprs;
  size_t num_exprs;
};

/* lvalues - variables */

enum ast_lvalue_ty {
  AST_LVALUE_TY_VAR,
};

struct ast_lvalue {
  enum ast_lvalue_ty ty;

  union {
    struct ast_var var;
  };
};

// Assignments - anything of form `<lvalue> = <lvalue | rvalue>` (so `<lvalue> = <expr>`)

struct ast_assg {
  struct ast_lvalue lvalue;
  struct ast_expr *expr;
};

/* rvalues - constants or arbitrary expressions */

enum ast_rvalue_ty {
  AST_RVALUE_TY_CNST,
  AST_RVALUE_TY_BINARY_OP,
  // AST_RVALUE_TY_COMPOUNDEXPR,
  // AST_RVALUE_TY_ASSG, // while assignments are of the form `lvalue = rvalue`, they themselves evaluate to an rvalue (unlike in C++)
};

struct ast_rvalue {
  enum ast_rvalue_ty ty;

  union {
    struct ast_cnst cnst;
    struct ast_binaryop binary_op;
    struct ast_compoundexpr compound_expr; // compound assignments are *never* lvalues in C
    struct ast_assg *assg;
  };
};

/* Expressions - divided into `lvalue` (can be on left hand side of assignment) and `rvalue` (not an lvalue) */

enum ast_expr_ty {
  AST_EXPR_TY_LVALUE,
  AST_EXPR_TY_RVALUE
};

struct ast_expr {
  enum ast_expr_ty ty;
  union {
    struct ast_lvalue lvalue;
    struct ast_rvalue rvalue;
  };
};

/* Variable declarations - `<typename> <comma seperated list of declarations>` where each declaration is `<name>` or `<name> = <expr>` */

enum ast_vardecl_ty {
  AST_VARDECL_TY_DECL,
  AST_VARDECL_TY_DECL_WITH_ASSG,
};

struct ast_vardecl {
  enum ast_vardecl_ty ty;
  struct ast_var var;

  union {
    struct ast_expr assg_expr;
  };
};

struct ast_vardecllist {
  struct ast_tyref var_ty;

  struct ast_vardecl *decls;
  size_t num_decls;
};

/* Jump statements - `return`, `break`, `continue`, `goto` */

enum ast_jumpstmt_ty {
  // AST_JUMPSTMT_TY_GOTO,
  // AST_JUMPSTMT_TY_BREAK,
  // AST_JUMPSTMT_TY_CONTINUE,
  AST_JUMPSTMT_TY_RETURN
};

struct ast_jumpstmt {
  enum ast_jumpstmt_ty ty;

  union {
    struct ast_expr ret_expr;
  };
};

/* Statements - either declaration, labelled, expression, compound, jump, iteration, or selection */

enum ast_stmt_ty {
  AST_STMT_TY_VAR_DECL_LIST,
  // AST_STMT_TY_LABELLED,
  AST_STMT_TY_EXPR,
  AST_STMT_TY_COMPOUND,
  AST_STMT_TY_JUMP,
  // AST_STMT_TY_ITER,
  // AST_STMT_TY_SELECT,
};

struct ast_stmt;
struct ast_compoundstmt {
  struct ast_stmt* stmts;
  size_t num_stmts;
};

struct ast_stmt {
  enum ast_stmt_ty ty;
  union {
    struct ast_vardecllist var_decl_list;
    struct ast_expr expr;
    struct ast_compoundstmt compound;
    struct ast_jumpstmt jump;
  };
};

/* Function definitions and declarations */

struct ast_funcdef {
  struct ast_funcsig sig;
  struct ast_compoundstmt body;
};

struct ast_funcdecl {
  struct ast_funcsig sig;
};

/* Translation unit (top level) */

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

void debug_print_ast(struct parser *parser, struct ast_translationunit *translation_unit);

#endif
