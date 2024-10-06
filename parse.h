#ifndef PARSE_H
#define PARSE_H

#include "alloc.h"
#include "lex.h"
#include "log.h"
#include "program.h"
#include "util.h"

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

  WELL_KNOWN_TY_FLOAT,
  WELL_KNOWN_TY_DOUBLE,
  WELL_KNOWN_TY_LONG_DOUBLE,
};

#define WKT_MAKE_SIGNED(wkt) (((wkt) >= WELL_KNOWN_TY_FLOAT) ? (wkt) : ((wkt) & ~1))
#define WKT_MAKE_UNSIGNED(wkt) (((wkt) >= WELL_KNOWN_TY_FLOAT) ? (wkt) : ((wkt) | 1))

#define WKT_IS_SIGNED(wkt) (((wkt) & 1) == 0)

/* Type refs - `<enum|struct|union> <identifier`, `<typedef-name>`, or
 * `<keyword>` */

struct ast_ty_pointer {
  struct ast_tyref *underlying;
};


enum ast_ty_array_ty {
  AST_TY_ARRAY_TY_UNKNOWN_SIZE,
  AST_TY_ARRAY_TY_KNOWN_SIZE,
};

struct ast_ty_array {
  enum ast_ty_array_ty ty;

  struct ast_tyref *element;

  union {
    size_t size;
  };
};

struct ast_struct_field {
  const char *name;
  struct ast_tyref *var_ty;
};

enum ast_ty_aggregate_ty {
  AST_TY_AGGREGATE_TY_STRUCT,
  AST_TY_AGGREGATE_TY_UNION,
};

struct ast_ty_aggregate {
  enum ast_ty_aggregate_ty ty;

  const char *name;

  struct ast_struct_field *field_var_tys;
  size_t num_field_var_tys;
};

struct ast_ty_func {
  struct ast_tyref *ret_var_ty;
  struct ast_tyref *param_var_tys;
  struct token *param_identifiers;
  size_t num_params;
};

enum ast_tyref_ty {
  /* Used for variables that were used without declaration and similar. Usually
     an error */
  AST_TYREF_TY_UNKNOWN,
  AST_TYREF_TY_VOID,
  AST_TYREF_TY_WELL_KNOWN,
  AST_TYREF_TY_FUNC,
  AST_TYREF_TY_POINTER,
  AST_TYREF_TY_ARRAY,
  AST_TYREF_TY_VARIADIC,
  AST_TYREF_TY_AGGREGATE, // e.g `union { int a; }`
  AST_TYREF_TY_TAGGED, // e.g `struct foo`
  // AST_TYREF_TY_TYPEDEF_NAME,
  // AST_TYREF_TY_ENUM,
};

enum ast_storage_class_specifier_flags {
  AST_STORAGE_CLASS_SPECIFIER_FLAG_NONE = 0,
  AST_STORAGE_CLASS_SPECIFIER_FLAG_TYPEDEF = 1,
  AST_STORAGE_CLASS_SPECIFIER_FLAG_EXTERN = 4,
  AST_STORAGE_CLASS_SPECIFIER_FLAG_STATIC = 8,
  AST_STORAGE_CLASS_SPECIFIER_FLAG_AUTO = 16,
  AST_STORAGE_CLASS_SPECIFIER_FLAG_REGISTER = 32,
};

enum ast_function_specifier_flags {
  AST_FUNCTION_SPECIFIER_FLAG_NONE = 0,
  AST_FUNCTION_SPECIFIER_FLAG_INLINE = 1,
};

enum ast_type_qualifier_flags {
  AST_TYPE_QUALIFIER_FLAG_NONE = 0,
  AST_TYPE_QUALIFIER_FLAG_CONST = 1,
  AST_TYPE_QUALIFIER_FLAG_VOLATILE = 2,
};

struct ast_ty_tagged {
  // union/enum/struct share same namespace so this is fine
  const char *name;
};

struct ast_tyref {
  enum ast_tyref_ty ty;

  enum ast_type_qualifier_flags type_qualifiers;
  enum ast_function_specifier_flags function_specifiers;

  union {
    enum well_known_ty well_known;
    struct ast_ty_pointer pointer;
    struct ast_ty_array array;
    struct ast_ty_func func;
    struct ast_ty_aggregate aggregate;
    struct ast_ty_tagged tagged;
  };
};

struct ast_arglist {
  struct ast_expr *args;
  size_t num_args;
};

/* Variable references */

enum ast_var_ty {
  AST_VAR_TY_VAR,
  AST_VAR_TY_ENUM_CNST
};

struct ast_var {
  enum ast_var_ty ty;

  struct token identifier;
  int scope;
  struct ast_tyref var_ty;

  union {
    int enum_cnst;
  };
};

struct ast_param {
  struct ast_tyref var_ty;
  struct ast_var var;
};

struct ast_paramlist {
  struct ast_param *params;
  size_t num_params;
};

struct ast_funcsig {
  struct ast_tyref var_ty;
  struct token name;
  struct ast_paramlist param_list;
};

/* Constant values (literals) */

struct ast_cnst {
  struct ast_tyref cnst_ty;

  union {
    unsigned long long int_value;
    const char *str_value;
    long double flt_value;
  };
};

/* Binary expressions - `a <OP> b` */

struct ast_expr;

enum ast_unary_op_ty {
  AST_UNARY_OP_TY_PREFIX_INC,
  AST_UNARY_OP_TY_PREFIX_DEC,
  AST_UNARY_OP_TY_POSTFIX_INC,
  AST_UNARY_OP_TY_POSTFIX_DEC,
  AST_UNARY_OP_TY_PLUS,
  AST_UNARY_OP_TY_MINUS,
  AST_UNARY_OP_TY_LOGICAL_NOT,
  AST_UNARY_OP_TY_NOT,
  AST_UNARY_OP_TY_INDIRECTION,
  AST_UNARY_OP_TY_SIZEOF,
  AST_UNARY_OP_TY_ADDRESSOF,
  AST_UNARY_OP_TY_ALIGNOF,
  AST_UNARY_OP_TY_CAST,
};

struct ast_cast {
  struct ast_tyref cast_ty;
};

struct ast_unary_op {
  enum ast_unary_op_ty ty;
  struct ast_tyref var_ty;
  struct ast_expr *expr;

  union {
    struct ast_cast cast;
  };
};

enum ast_binary_op_ty {
  AST_BINARY_OP_TY_EQ,
  AST_BINARY_OP_TY_NEQ,
  AST_BINARY_OP_TY_GT,
  AST_BINARY_OP_TY_GTEQ,
  AST_BINARY_OP_TY_LT,
  AST_BINARY_OP_TY_LTEQ,

  AST_BINARY_OP_TY_LOGICAL_OR,
  AST_BINARY_OP_TY_LOGICAL_AND,
  AST_BINARY_OP_TY_OR,
  AST_BINARY_OP_TY_AND,
  AST_BINARY_OP_TY_XOR,
  AST_BINARY_OP_TY_LSHIFT,
  AST_BINARY_OP_TY_RSHIFT,

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

struct ast_call {
  struct ast_tyref var_ty;

  struct ast_expr *target;
  struct ast_arglist arg_list;
};

/* Compound expr - comma seperated expressions with well-defined order of
 * execution */

struct ast_compoundexpr {
  struct ast_expr *exprs;
  size_t num_exprs;
};

/* atom - expression which is entirely isolated and not affected by other
 * operators in how it is parsed */

struct ast_initlist {
  struct ast_expr *exprs;
  size_t num_exprs;
};

// Assignments - anything of form `<lvalue> = <lvalue | rvalue>` (so `<lvalue>
// = <expr>`)

enum ast_assg_ty {
  AST_ASSG_TY_SIMPLEASSG,
  AST_ASSG_TY_COMPOUNDASSG // e.g +=
};

struct ast_assg_compound_assg {
  // the type of the intermediate
  // e.g `int a = 0; a += 10l`
  // is equiv to `int a = 0; a = a + 10l`
  // so the resulting type is `int` but the intermediate type is `long`
  struct ast_tyref intermediate_var_ty;
  enum ast_binary_op_ty binary_op_ty;
};

struct ast_assg {
  enum ast_assg_ty ty;

  struct ast_tyref var_ty;

  struct ast_expr *assignee;
  struct ast_expr *expr;

  union {
    struct ast_assg_compound_assg compound_assg;
  };
};

struct ast_arrayaccess {
  // The lhs will always be an array/pointer type
  // rhs may be integral type
  struct ast_expr *lhs;
  struct ast_expr *rhs;
};

struct ast_memberaccess {
  struct ast_expr *lhs;
  struct token member;
};

struct ast_pointeraccess {
  struct ast_expr *lhs;
  struct token member;
};

enum ast_sizeof_ty {
  AST_SIZEOF_TY_TYPE,
  AST_SIZEOF_TY_EXPR,
};

struct ast_sizeof {
  enum ast_sizeof_ty ty;

  union {
    struct ast_expr *expr;
    struct ast_tyref ty_ref;
  };
};

struct ast_alignof {
  struct ast_tyref ty_ref;
};

/* Expressions - divided into `lvalue` (can be on left hand side of assignment)
 * and `rvalue` (not an lvalue) */

enum ast_expr_ty {
  AST_EXPR_TY_CALL,
  AST_EXPR_TY_UNARY_OP,
  AST_EXPR_TY_BINARY_OP,
  AST_EXPR_TY_ARRAYACCESS,
  AST_EXPR_TY_MEMBERACCESS,
  AST_EXPR_TY_POINTERACCESS,
  AST_EXPR_TY_INIT_LIST, // brace list
  AST_EXPR_TY_ASSG, // while assignments are of the form `lvalue = rvalue`,
                    // they themselves evaluate to an rvalue (unlike in C++)
  AST_EXPR_TY_VAR,
  AST_EXPR_TY_CNST,
  AST_EXPR_TY_COMPOUNDEXPR,
  AST_EXPR_TY_SIZEOF,
  AST_EXPR_TY_ALIGNOF,
};

struct ast_expr {
  enum ast_expr_ty ty;
  struct ast_tyref var_ty;
  union {
    struct ast_sizeof size_of;
    struct ast_alignof align_of;
    struct ast_var var;
    struct ast_cnst cnst;
    struct ast_compoundexpr
        compound_expr; // compound assignments are *never* lvalues in C
    struct ast_unary_op unary_op;
    struct ast_binary_op binary_op;
    struct ast_assg assg;
    struct ast_call call;
    struct ast_arrayaccess array_access;
    struct ast_memberaccess member_access;
    struct ast_pointeraccess pointer_access;
    struct ast_initlist init_list;
  };
};

/* Variable declarations - `<typename> <comma seperated list of declarations>`
 * where each declaration is `<name>` or `<name> = <expr>` */

enum ast_decl_ty {
  AST_DECL_TY_DECL,
  AST_DECL_TY_DECL_WITH_ASSG,
};

struct ast_decl {
  enum ast_decl_ty ty;
  struct ast_var var;

  // int scope;

  union {
    struct ast_expr assg_expr;
  };
};

enum ast_decllist_ty {
  AST_DECL_LIST_TY_VARS,
  AST_DECL_LIST_TY_TYPES,
};

struct ast_decllist {
  enum ast_decllist_ty ty;

  enum ast_storage_class_specifier_flags storage_class_specifiers;

  struct ast_decl *decls;
  size_t num_decls;
};

struct ast_typedef {
  struct ast_decllist var_decl_list;
};

/* Jump statements - `return`, `break`, `continue`, `goto` */

struct ast_returnstmt {
  struct ast_tyref var_ty;
  struct ast_expr *expr;
};

struct ast_gotostmt {
  struct token label;
};

enum ast_jumpstmt_ty {
  // AST_JUMPSTMT_TY_BREAK,
  // AST_JUMPSTMT_TY_CONTINUE,
  AST_JUMPSTMT_TY_GOTO,
  AST_JUMPSTMT_TY_RETURN
};

struct ast_jumpstmt {
  enum ast_jumpstmt_ty ty;

  union {
    struct ast_returnstmt return_stmt;
    struct ast_gotostmt goto_stmt;
  };
};

/* Statements - either declaration, labelled, expression, compound, jump,
 * iteration, or selection */

struct ast_labeledstmt {
  struct token label;

  struct ast_stmt *stmt;
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
  struct ast_decllist *decl;
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

enum ast_stmt_ty {
  AST_STMT_TY_NULL,
  AST_STMT_TY_DECL_LIST,
  AST_STMT_TY_LABELED,
  AST_STMT_TY_EXPR,
  AST_STMT_TY_COMPOUND,
  AST_STMT_TY_JUMP,
  AST_STMT_TY_ITER,
  AST_STMT_TY_SELECT,
};

struct ast_stmt {
  enum ast_stmt_ty ty;
  union {
    struct ast_decllist decl_list;
    struct ast_expr expr;
    struct ast_compoundstmt compound;
    struct ast_jumpstmt jump;
    struct ast_selectstmt select;
    struct ast_iterstmt iter;
    struct ast_labeledstmt labeled;
  };
};

/* Struct definitions and declarations */
struct ast_field {
  struct ast_tyref var_ty;
  struct token identifier;
};

struct ast_structdecl {
  size_t num_fields;
  // struct ast_field *fields;
  struct ast_decl *fields;
};

enum ast_type_ty {
  AST_TYPE_TY_UNION,
  AST_TYPE_TY_STRUCT,
};

struct ast_aggregatedecl {
  enum ast_type_ty ty;

  struct token name;
};

struct ast_structdecllist {
  size_t num_struct_decls;
  struct ast_structdecl *struct_decls;
};

/* Enum definitions */

enum ast_enumcnst_ty {
  AST_ENUMCNST_TY_EXPLICIT_VALUE,
  AST_ENUMCNST_TY_IMPLICIT_VALUE,
};

struct ast_enumcnst {
  struct token identifier;

  enum ast_enumcnst_ty ty;

  union {
    unsigned long long value;
  };
};

struct ast_enumdecllist {
  size_t num_enum_cnsts;
  struct ast_enumcnst *enum_cnsts;
};

/* Function definitions and declarations */

struct ast_funcdef {
  // struct ast_decllist decl;
  struct token identifier;
  struct ast_tyref var_ty;
  struct ast_compoundstmt body;
};

/* Translation unit (top level) */

struct ast_translationunit {
  struct ast_decllist *decl_lists;
  size_t num_decl_lists;

  struct ast_funcdef *func_defs;
  size_t num_func_defs;
};

struct parser;

enum parser_create_result {
  PARSER_CREATE_RESULT_SUCCESS,
  PARSER_CREATE_RESULT_FAILURE
};

struct parse_result {
  struct ast_translationunit translation_unit;
};

enum parser_create_result parser_create(struct preprocessed_program *program,
                                        struct parser **parser);
struct parse_result parse(struct parser *parser);
void parser_free(struct parser **parser);

const char *identifier_str(struct parser *parser, const struct token *token);

struct ast_tyref tyref_make_pointer(struct parser *parser,
                                    const struct ast_tyref *var_ty);

struct ast_tyref tyref_get_underlying(struct parser *parser,
                                      const struct ast_tyref *ty_ref);

bool is_integral_ty(const struct ast_tyref *ty);
bool is_fp_ty(const struct ast_tyref *ty);

struct ast_tyref tyref_pointer_sized_int(struct parser *parser, bool is_signed);                                     

void debug_print_ast(struct parser *parser,
                     struct ast_translationunit *translation_unit);

#endif
