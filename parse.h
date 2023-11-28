#ifndef PARSE_H
#define PARSE_H

#include "alloc.h"
#include "lex.h"
#include "log.h"
#include "util.h"

#define SCOPE_GLOBAL (-1)

/* Well known types - keywords (`int`, `unsigned`, etc) */

enum well_known_ty {
  // parser.c relies on the unsigned variant being the signed variant +1
  // ir.c relies on the sizes being ascending
  WELL_KNOWN_TY_SIGNED_CHAR,
  WELL_KNOWN_TY_UNSIGNED_CHAR,

  WELL_KNOWN_TY_SIGNED_SHORT,
  WELL_KNOWN_TY_UNSIGNED_SHORT,

  WELL_KNOWN_TY_SIGNED_INT,
  WELL_KNOWN_TY_UNSIGNED_INT,

  WELL_KNOWN_TY_SIGNED_LONG,
  WELL_KNOWN_TY_UNSIGNED_LONG,

  WELL_KNOWN_TY_SIGNED_LONG_LONG,
  WELL_KNOWN_TY_UNSIGNED_LONG_LONG,
};

#define WKT_MAKE_SIGNED(wkt) ((wkt) & ~1)
#define WKT_MAKE_UNSIGNED(wkt) ((wkt) | 1)

#define WKT_IS_SIGNED(wkt) (((wkt) & 1) == 0)

/* Type refs - `<enum|struct|union> <identifier`, `<typedef-name>`, or
 * `<keyword>` */

enum ast_tyref_ty {
  /* Used for variables that were used without declaration and similar. Usually
     an error */
  AST_TYREF_TY_UNKNOWN,
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
  void *DUMMY;
};

struct ast_funcsig {
  struct ast_tyref ret_ty;
  struct token name;
  struct ast_arglist arg_list;

  // TODO: args
};

/* Constant values (literals) */

struct ast_cnst {
  enum well_known_ty cnst_ty;
  unsigned long long value;
};

/* Binary expressions - `a <OP> b` */

struct ast_expr;

enum ast_unary_op_prefix_ty {
  AST_UNARY_OP_PREFIX_TY_INC,
  AST_UNARY_OP_PREFIX_TY_DEC,
  AST_UNARY_OP_PREFIX_TY_PLUS,
  AST_UNARY_OP_PREFIX_TY_MINUS,
  // AST_UNARY_OP_TY_NOT,
  // AST_UNARY_OP_TY_SIZEOF,
};

struct ast_unary_op_prefix {
  enum ast_unary_op_prefix_ty ty;
  struct ast_expr *expr;
};

enum ast_unary_op_postfix_ty {
  AST_UNARY_OP_POSTFIX_TY_INC,
  AST_UNARY_OP_POSTFIX_TY_DEC,
};

struct ast_unary_op_postfix {
  enum ast_unary_op_postfix_ty ty;
  struct ast_expr *expr;
};

enum ast_binary_op_ty {
  AST_BINARY_OP_TY_ADD,
  AST_BINARY_OP_TY_SUB,
  AST_BINARY_OP_TY_MUL,
  AST_BINARY_OP_TY_DIV,
  AST_BINARY_OP_TY_QUOT
};

struct ast_binary_op {
  enum ast_binary_op_ty ty;
  struct ast_tyref var_ty;
  struct ast_expr *lhs;
  struct ast_expr *rhs;
};

enum ast_unary_op_ty { AST_UNARY_OP_TY_PREFIX, AST_UNARY_OP_TY_POSTFIX };

struct ast_unary_op {
  enum ast_unary_op_ty ty;
  union {
    struct ast_unary_op_prefix prefix;
    struct ast_unary_op_postfix postfix;
  };
};

/* Variable references */

struct ast_var {
  struct token identifier;
  int scope;
};

/* Compound expr - comma seperated expressions with well-defined order of
 * execution */

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
  struct ast_tyref var_ty;

  union {
    struct ast_var var;
  };
};

// Assignments - anything of form `<lvalue> = <lvalue | rvalue>` (so `<lvalue>
// = <expr>`)

struct ast_assg {
  struct ast_lvalue lvalue;
  struct ast_expr *expr;
};

/* rvalues - constants or arbitrary expressions */

enum ast_rvalue_ty {
  AST_RVALUE_TY_CNST,
  AST_RVALUE_TY_BINARY_OP,
  AST_RVALUE_TY_COMPOUNDEXPR,
  AST_RVALUE_TY_ASSG, // while assignments are of the form `lvalue = rvalue`,
                      // they themselves evaluate to an rvalue (unlike in C++)
};

struct ast_rvalue {
  enum ast_rvalue_ty ty;
  struct ast_tyref var_ty;

  union {
    struct ast_cnst cnst;
    struct ast_binary_op binary_op;
    struct ast_unary_op unary_op;
    struct ast_compoundexpr
        compound_expr; // compound assignments are *never* lvalues in C
    struct ast_assg *assg;
  };
};

/* Expressions - divided into `lvalue` (can be on left hand side of assignment)
 * and `rvalue` (not an lvalue) */

enum ast_expr_ty { AST_EXPR_TY_LVALUE, AST_EXPR_TY_RVALUE };

struct ast_expr {
  enum ast_expr_ty ty;
  struct ast_tyref var_ty;
  union {
    struct ast_lvalue lvalue;
    struct ast_rvalue rvalue;
  };
};

/* Variable declarations - `<typename> <comma seperated list of declarations>`
 * where each declaration is `<name>` or `<name> = <expr>` */

enum ast_vardecl_ty {
  AST_VARDECL_TY_DECL,
  AST_VARDECL_TY_DECL_WITH_ASSG,
};

struct ast_vardecl {
  enum ast_vardecl_ty ty;
  struct ast_var var;

  // int scope;

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

/* Statements - either declaration, labelled, expression, compound, jump,
 * iteration, or selection */

enum ast_stmt_ty {
  AST_STMT_TY_NULL,
  AST_STMT_TY_VAR_DECL_LIST,
  // AST_STMT_TY_LABELLED,
  AST_STMT_TY_EXPR,
  AST_STMT_TY_COMPOUND,
  AST_STMT_TY_JUMP,
  AST_STMT_TY_ITER,
  AST_STMT_TY_SELECT,
};

struct ast_ifstmt {
  struct ast_expr condition;
  struct ast_stmt *body;
};

struct ast_ifelsestmt {
  struct ast_expr condition;
  struct ast_stmt *body;
  struct ast_stmt *else_body;
};

struct ast_switchstmt {
  struct ast_expr ctrl_var;
  // TODO:
};

enum ast_selectstmt_ty {
  AST_SELECTSTMT_TY_IF,
  AST_SELECTSTMT_TY_IF_ELSE,
  AST_SELECTSTMT_TY_SWITCH,
};

struct ast_selectstmt {
  enum ast_selectstmt_ty ty;

  union {
    struct ast_ifstmt if_stmt;
    struct ast_ifelsestmt if_else_stmt;
    struct ast_switchstmt switch_stmt;
  };
};

struct ast_stmt;
struct ast_compoundstmt {
  struct ast_stmt *stmts;
  size_t num_stmts;
};

struct ast_whilestmt {
  struct ast_expr cond;
  struct ast_stmt *body;
};

struct ast_dowhilestmt {
  struct ast_expr cond;
  struct ast_stmt *body;
};

struct ast_declorexpr {
  struct ast_vardecllist *decl;
  struct ast_expr *expr;
};

struct ast_forstmt {
  struct ast_declorexpr *init;
  struct ast_expr *cond;
  struct ast_expr *iter;
  struct ast_stmt *body;
};

enum ast_iterstmt_ty {
  AST_ITERSTMT_TY_WHILE,
  AST_ITERSTMT_TY_DO_WHILE,
  AST_ITERSTMT_TY_FOR,
};

struct ast_iterstmt {
  enum ast_iterstmt_ty ty;

  union {
    struct ast_whilestmt while_stmt;
    struct ast_dowhilestmt do_while_stmt;
    struct ast_forstmt for_stmt;
  };
};

struct ast_stmt {
  enum ast_stmt_ty ty;
  union {
    struct ast_vardecllist var_decl_list;
    struct ast_expr expr;
    struct ast_compoundstmt compound;
    struct ast_jumpstmt jump;
    struct ast_selectstmt select;
    struct ast_iterstmt iter;
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
  struct ast_funcdef *func_defs;
  size_t num_func_defs;

  struct ast_funcdecl *func_decls;
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

enum parser_create_result parser_create(const char *program,
                                        struct parser **parser);
struct parse_result parse(struct parser *parser);
void parser_free(struct parser **parser);

const char *identifier_str(struct parser *parser, const struct token *token);

void debug_print_ast(struct parser *parser,
                     struct ast_translationunit *translation_unit);

#endif
