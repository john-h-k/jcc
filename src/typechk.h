#ifndef TYPECHK_H
#define TYPECHK_H

#include "compiler.h"
#include "parse.h"
#include "program.h"
#include "target.h"
#include "var_table.h"

/* Typecheck */
// Converts AST types into typed (`td_`) types
// Performs other changes such as flattening all declarations
// As this makes IR building and validation easier

/* Well known types - keywords (`int`, `unsigned`, etc) */

enum td_storage_class_specifier {
  TD_STORAGE_CLASS_SPECIFIER_NONE,
  TD_STORAGE_CLASS_SPECIFIER_TYPEDEF,
  TD_STORAGE_CLASS_SPECIFIER_EXTERN,
  TD_STORAGE_CLASS_SPECIFIER_STATIC,
  TD_STORAGE_CLASS_SPECIFIER_AUTO,
  TD_STORAGE_CLASS_SPECIFIER_REGISTER,
};

enum td_function_specifier_flags {
  TD_FUNCTION_SPECIFIER_FLAG_NONE = 0,
  TD_FUNCTION_SPECIFIER_FLAG_INLINE = 1 << 0,
  TD_FUNCTION_SPECIFIER_FLAG_NORETURN = 1 << 1,
};

enum td_type_qualifier_flags {
  TD_TYPE_QUALIFIER_FLAG_NONE = 0,
  TD_TYPE_QUALIFIER_FLAG_CONST = 1 << 0,
  TD_TYPE_QUALIFIER_FLAG_VOLATILE = 1 << 1,
  TD_TYPE_QUALIFIER_FLAG_RESTRICT = 1 << 2,
  TD_TYPE_QUALIFIER_FLAG_NONNULL = 1 << 3,
  TD_TYPE_QUALIFIER_FLAG_NULLABLE = 1 << 4,
};

enum well_known_ty {
  // ir.c relies on the sizes being ascending
  // macros below rely on signed being even and unsigned being odd
  // TODO: WELL_KNOWN_TY_BOOL,
  WELL_KNOWN_TY_CHAR = 1,

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

  WELL_KNOWN_TY_INT128,
  WELL_KNOWN_TY_UINT128,

  WELL_KNOWN_TY_BOOL,

  WELL_KNOWN_TY_HALF,
  WELL_KNOWN_TY_FLOAT,
  WELL_KNOWN_TY_DOUBLE,
  WELL_KNOWN_TY_LONG_DOUBLE,
};

#define WKT_IS_INTEGRAL(wkt) ((wkt) < WELL_KNOWN_TY_HALF)
#define WKT_IS_FP(wkt) ((wkt) >= WELL_KNOWN_TY_HALF)

#define WKT_MAKE_SIGNED(wkt)                                                   \
  (((wkt) >= WELL_KNOWN_TY_BOOL) ? (wkt) : ((wkt) & ~1))
#define WKT_MAKE_UNSIGNED(wkt)                                                 \
  (((wkt) >= WELL_KNOWN_TY_BOOL) ? (wkt) : ((wkt) | 1))

#define WKT_IS_SIGNED(wkt) (((wkt) != WELL_KNOWN_TY_BOOL) && ((wkt) & 1) == 0)

struct td_ty_pointer {
  struct td_var_ty *underlying;
};

struct td_ty_array {
  struct td_var_ty *underlying;
  size_t size;
};

enum td_ty_aggregate_ty {
  TD_TY_AGGREGATE_TY_STRUCT,
  TD_TY_AGGREGATE_TY_UNION,
};

struct td_ty_aggregate {
  enum td_ty_aggregate_ty ty;

  struct sized_str name;

  struct td_struct_field *fields;
  size_t num_fields;
};

struct td_ty_incomplete_aggregate {
  enum td_ty_aggregate_ty ty;

  struct sized_str name;
};

enum td_ty_func_ty {
  TD_TY_FUNC_TY_UNKNOWN_ARGS, // e.g `int foo()`
  TD_TY_FUNC_TY_KNOWN_ARGS,   // e.g `int foo(void), int foo(int, float)`
  TD_TY_FUNC_TY_VARIADIC      // e.g `int foo(int, ...)`
};

struct td_ty_func {
  enum td_ty_func_ty ty;

  struct td_var_ty *ret;
  struct td_ty_param *params;
  size_t num_params;
};

enum td_var_ty_ty {
  /* Used for variables that were used without declaration and similar. Usually
     an error */
  TD_VAR_TY_TY_UNKNOWN,
  TD_VAR_TY_TY_VOID,
  TD_VAR_TY_TY_WELL_KNOWN,
  TD_VAR_TY_TY_FUNC,
  TD_VAR_TY_TY_POINTER,
  TD_VAR_TY_TY_ARRAY,
  TD_VAR_TY_TY_VARIADIC,
  TD_VAR_TY_TY_AGGREGATE, // e.g `union { int a; }`
  TD_VAR_TY_TY_INCOMPLETE_AGGREGATE
  // TD_VAR_TY_TY_TYPEDEF_NAME,
};

// FIXME: this is probably bad for perf (full of pointers when sparsely full)

enum td_attr_format_archetype { TD_ATTR_FORMAT_ARCHETYPE_PRINTF };

struct td_attr_format {
  enum td_attr_format_archetype archetype;

  // these are indexed from ONE
  size_t string_index;
  size_t first_to_check;
};

enum td_declaration_attr_ty {
  TD_DECLARATION_ATTR_TY_WEAK,
  TD_DECLARATION_ATTR_TY_FORMAT
};

struct td_declaration_attr {
  enum td_declaration_attr_ty ty;

  union {
    struct td_attr_format format;
  };
};

struct td_declaration_attr_list {
  struct td_declaration_attr *attrs;
  size_t num_attrs;
};

struct td_var_attrs {
  struct td_attr_format *format;
  bool weak;
};

struct td_var_ty {
  enum td_var_ty_ty ty;

  enum td_type_qualifier_flags type_qualifiers;
  struct td_var_attrs attrs;

  union {
    enum well_known_ty well_known;
    struct td_ty_pointer pointer;
    struct td_ty_array array;
    struct td_ty_func func;
    struct td_ty_aggregate aggregate;
    struct td_ty_incomplete_aggregate incomplete_aggregate;
  };
};

enum td_struct_field_flags {
  TD_STRUCT_FIELD_FLAG_NONE = 0,
  TD_STRUCT_FIELD_FLAG_BITFIELD = 1,
};

struct td_struct_field {
  struct sized_str identifier;
  struct td_var_ty var_ty;

  enum td_struct_field_flags flags;

  union {
    unsigned long long bitfield_width;
  };
};

struct td_ty_param {
  struct td_var_ty var_ty;
  struct sized_str identifier;
  struct text_span span;
};

extern struct td_var_ty TD_VAR_TY_UNKNOWN;
extern struct td_var_ty TD_VAR_TY_VOID;
extern struct td_var_ty TD_VAR_TY_VOID_POINTER;
extern struct td_var_ty TD_VAR_TY_CHAR_POINTER;
extern struct td_var_ty TD_VAR_TY_INT_POINTER;
extern struct td_var_ty TD_VAR_TY_CONST_CHAR_POINTER;

extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_BOOL;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_CHAR;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_SIGNED_CHAR;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_UNSIGNED_CHAR;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_SIGNED_SHORT;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_UNSIGNED_SHORT;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_SIGNED_INT;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_UNSIGNED_INT;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_SIGNED_LONG;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_UNSIGNED_LONG;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_SIGNED_LONG_LONG;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_UNSIGNED_LONG_LONG;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_HALF;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_FLOAT;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_DOUBLE;
extern struct td_var_ty TD_VAR_TY_WELL_KNOWN_LONG_DOUBLE;

// TODO: try and parse init lists as expressions to give better error messages
enum td_init_ty {
  TD_INIT_TY_EXPR,
  TD_INIT_TY_INIT_LIST,
};

struct td_arglist {
  struct td_expr *args;
  size_t num_args;
};

/* Variable references */

enum td_var_var_ty {
  TD_VAR_VAR_TY_ENUMERATOR,
  TD_VAR_VAR_TY_VAR, // `auto`
};

struct td_var {
  enum td_var_var_ty ty;

  struct sized_str identifier;
  int scope;

  union {
    int enumerator;
  };

  struct text_span span;
};

struct td_param {
  struct sized_str name;

  struct td_var_ty var_ty;
};

struct td_paramlist {
  struct td_param *params;
  size_t num_params;
};

/* Constant values (literals) */

enum td_cnst_ty {
  TD_CNST_TY_NUM,
  TD_CNST_TY_STRING,
};

enum td_cnst_str_ty {
  TD_CNST_STR_TY_ASCII,
  TD_CNST_STR_TY_WIDE,
};

struct td_ascii_str {
  char *value;
  size_t len;
};

struct td_wide_str {
  uint32_t *value;
  size_t len;
};

struct td_cnst_str {
  enum td_cnst_str_ty ty;

  union {
    struct td_ascii_str ascii;
    struct td_wide_str wide;
  };
};

struct td_cnst {
  enum td_cnst_ty ty;

  union {
    struct ap_val num_value;
    struct td_cnst_str str_value;
  };
};

/* Binary expressions - `a <OP> b` */

struct td_expr;

enum td_unary_op_ty {
  TD_UNARY_OP_TY_PREFIX_INC,
  TD_UNARY_OP_TY_PREFIX_DEC,
  TD_UNARY_OP_TY_POSTFIX_INC,
  TD_UNARY_OP_TY_POSTFIX_DEC,
  TD_UNARY_OP_TY_PLUS,
  TD_UNARY_OP_TY_MINUS,
  TD_UNARY_OP_TY_LOGICAL_NOT,
  TD_UNARY_OP_TY_NOT,
  TD_UNARY_OP_TY_INDIRECTION,
  TD_UNARY_OP_TY_ADDRESSOF,
  TD_UNARY_OP_TY_CAST,
};

struct td_cast {
  struct td_var_ty var_ty;
};

struct td_unary_op {
  enum td_unary_op_ty ty;
  struct td_expr *expr;

  union {
    struct td_cast cast;
  };
};

enum td_binary_op_ty {
  TD_BINARY_OP_TY_EQ,
  TD_BINARY_OP_TY_NEQ,
  TD_BINARY_OP_TY_GT,
  TD_BINARY_OP_TY_GTEQ,
  TD_BINARY_OP_TY_LT,
  TD_BINARY_OP_TY_LTEQ,

  TD_BINARY_OP_TY_LOGICAL_OR,
  TD_BINARY_OP_TY_LOGICAL_AND,
  TD_BINARY_OP_TY_OR,
  TD_BINARY_OP_TY_AND,
  TD_BINARY_OP_TY_XOR,
  TD_BINARY_OP_TY_LSHIFT,
  TD_BINARY_OP_TY_RSHIFT,

  TD_BINARY_OP_TY_ADD,
  TD_BINARY_OP_TY_SUB,
  TD_BINARY_OP_TY_MUL,
  TD_BINARY_OP_TY_DIV,
  TD_BINARY_OP_TY_MOD
};

struct td_binary_op {
  enum td_binary_op_ty ty;

  struct td_expr *lhs;
  struct td_expr *rhs;
};

struct td_call {
  struct td_expr *target;
  struct td_arglist arg_list;
};

/* Compound expr - comma seperated expressions with well-defined order of
 * execution */

struct td_compoundexpr {
  struct td_expr *exprs;
  size_t num_exprs;
};

/* atom - expression which is entirely isolated and not affected by other
 * operators in how it is parsed */

enum td_designator_ty {
  TD_DESIGNATOR_TY_FIELD,
  TD_DESIGNATOR_TY_INDEX,
};

struct td_designator {
  enum td_designator_ty ty;

  struct td_var_ty var_ty;

  union {
    struct sized_str field;
    unsigned long long index;
  };
};

struct td_designator_list {
  struct td_var_ty var_ty;

  size_t num_designators;
  struct td_designator *designators;
};

struct td_init_list_init {
  struct td_designator_list *designator_list;
  struct td_init *init;
};

struct td_init_list {
  struct td_var_ty var_ty;

  struct td_init_list_init *inits;
  size_t num_inits;
};

// Assignments - anything of form `<lvalue> = <lvalue | rvalue>` (so `<lvalue>
// = <expr>`)

enum td_assg_ty {
  TD_ASSG_TY_BASIC,
  TD_ASSG_TY_ADD,
  TD_ASSG_TY_SUB,
  TD_ASSG_TY_MUL,
  TD_ASSG_TY_DIV,
  TD_ASSG_TY_MOD,
  TD_ASSG_TY_AND,
  TD_ASSG_TY_OR,
  TD_ASSG_TY_XOR,
  TD_ASSG_TY_LSHIFT,
  TD_ASSG_TY_RSHIFT,
};

struct td_assg {
  enum td_assg_ty ty;

  bool cast_assignee, cast_result;
  struct td_var_ty assignee_var_ty, result_var_ty;

  struct td_expr *assignee;
  struct td_expr *expr;
};

struct td_arrayaccess {
  // The lhs will always be an array/pointer type
  // rhs may be integral type
  struct td_expr *lhs;
  struct td_expr *rhs;
};

struct td_memberaccess {
  struct td_expr *lhs;
  struct sized_str member;
};

struct td_pointeraccess {
  struct td_expr *lhs;
  struct sized_str member;
};

enum td_sizeof_ty {
  TD_SIZEOF_TY_TYPE,
  TD_SIZEOF_TY_EXPR,
};

struct td_sizeof {
  struct td_var_ty var_ty;
};

struct td_alignof {
  struct td_var_ty var_ty;
};

struct td_ternary {
  struct td_expr *cond;
  struct td_expr *true_expr;
  struct td_expr *false_expr;
};

/* Expressions - divided into `lvalue` (can be on left hand side of assignment)
 * and `rvalue` (not an lvalue) */

struct td_compound_literal {
  struct td_var_ty var_ty;
  struct td_init_list init_list;
};

enum td_expr_ty {
  TD_EXPR_TY_INVALID,
  TD_EXPR_TY_TERNARY,
  TD_EXPR_TY_CALL,
  TD_EXPR_TY_UNARY_OP,
  TD_EXPR_TY_BINARY_OP,
  TD_EXPR_TY_ARRAYACCESS,
  TD_EXPR_TY_MEMBERACCESS,
  TD_EXPR_TY_POINTERACCESS,
  TD_EXPR_TY_ASSG,
  TD_EXPR_TY_VAR,
  TD_EXPR_TY_CNST,
  TD_EXPR_TY_COMPOUNDEXPR,
  TD_EXPR_TY_SIZEOF,
  TD_EXPR_TY_ALIGNOF,
  TD_EXPR_TY_COMPOUND_LITERAL,
};

struct td_expr {
  enum td_expr_ty ty;
  struct td_var_ty var_ty;

  union {
    struct td_ternary ternary;
    struct td_sizeof size_of;
    struct td_alignof align_of;
    struct td_var var;
    struct td_cnst cnst;
    struct td_compoundexpr compound_expr;
    struct td_unary_op unary_op;
    struct td_binary_op binary_op;
    struct td_assg assg;
    struct td_call call;
    struct td_arrayaccess array_access;
    struct td_memberaccess member_access;
    struct td_pointeraccess pointer_access;
    struct td_compound_literal compound_literal;
  };

  // TODO: have on more td_ types for analysis
  struct text_span span;
};

/* Variable declarations - `<typename> <comma seperated list of declarations>`
 * where each declaration is `<name>` or `<name> = <expr>` */

struct td_init {
  enum td_init_ty ty;

  union {
    struct td_expr expr;
    struct td_init_list init_list;
  };
};

enum td_var_declaration_ty {
  TD_VAR_DECLARATION_TY_VAR,
  TD_VAR_DECLARATION_TY_BITFIELD,
};

struct td_var_declaration {
  enum td_var_declaration_ty ty;

  struct td_var_ty var_ty;

  struct td_var var;
  struct td_init *init;

  unsigned long long bitfield_width;

  struct text_span span;
};

struct td_declaration {
  enum td_storage_class_specifier storage_class_specifier;
  enum td_function_specifier_flags function_specifier_flags;

  struct td_var_ty base_ty;

  size_t num_var_declarations;
  struct td_var_declaration *var_declarations;

  struct text_span span;
};

/* Jump statements - `return`, `break`, `continue`, `goto` */

struct td_returnstmt {
  struct td_expr *expr;
};

struct td_gotostmt {
  struct sized_str label;
};

enum td_jumpstmt_ty {
  TD_JUMPSTMT_TY_BREAK,
  TD_JUMPSTMT_TY_CONTINUE,
  TD_JUMPSTMT_TY_GOTO,
  TD_JUMPSTMT_TY_RETURN
};

struct td_jumpstmt {
  enum td_jumpstmt_ty ty;

  union {
    struct td_returnstmt return_stmt;
    struct td_gotostmt goto_stmt;
  };
};

/* Statements - either declaration, labelled, expression, compound, jump,
 * iteration, or selection */

enum td_labeledstmt_ty {
  TD_LABELEDSTMT_TY_LABEL,
  TD_LABELEDSTMT_TY_CASE,
  TD_LABELEDSTMT_TY_DEFAULT,
};

struct td_labeledstmt {
  enum td_labeledstmt_ty ty;

  struct td_stmt *stmt;

  union {
    unsigned long long cnst;
    struct sized_str label;
  };
};

struct td_ifstmt {
  struct td_expr cond;
  struct td_stmt *body;
};

struct td_ifelsestmt {
  struct td_expr cond;
  struct td_stmt *body;
  struct td_stmt *else_body;
};

struct td_switchstmt {
  struct td_expr ctrl_expr;
  struct td_stmt *body;
};

enum td_selectstmt_ty {
  TD_SELECTSTMT_TY_IF,
  TD_SELECTSTMT_TY_IF_ELSE,
  TD_SELECTSTMT_TY_SWITCH,
};

struct td_selectstmt {
  enum td_selectstmt_ty ty;

  union {
    struct td_ifstmt if_stmt;
    struct td_ifelsestmt if_else_stmt;
    struct td_switchstmt switch_stmt;
  };
};

struct td_stmt;
struct td_compoundstmt {
  struct td_stmt *stmts;
  size_t num_stmts;
};

struct td_whilestmt {
  struct td_expr cond;
  struct td_stmt *body;
};

struct td_dowhilestmt {
  struct td_expr cond;
  struct td_stmt *body;
};

enum td_declaration_or_expr_ty {
  TD_DECLARATION_OR_EXPR_TY_DECL,
  TD_DECLARATION_OR_EXPR_TY_EXPR,
};

struct td_declaration_or_expr {
  enum td_declaration_or_expr_ty ty;

  union {
    struct td_declaration decl;
    struct td_expr expr;
  };
};

struct td_forstmt {
  struct td_declaration_or_expr *init;
  struct td_expr *cond;
  struct td_expr *iter;
  struct td_stmt *body;
};

enum td_iterstmt_ty {
  TD_ITERSTMT_TY_WHILE,
  TD_ITERSTMT_TY_DO_WHILE,
  TD_ITERSTMT_TY_FOR,
};

struct td_iterstmt {
  enum td_iterstmt_ty ty;

  union {
    struct td_whilestmt while_stmt;
    struct td_dowhilestmt do_while_stmt;
    struct td_forstmt for_stmt;
  };
};

enum td_stmt_ty {
  TD_STMT_TY_NULL,
  TD_STMT_TY_DECLARATION,
  TD_STMT_TY_LABELED,
  TD_STMT_TY_EXPR,
  TD_STMT_TY_COMPOUND,
  TD_STMT_TY_JUMP,
  TD_STMT_TY_ITER,
  TD_STMT_TY_SELECT,
};

struct td_stmt {
  enum td_stmt_ty ty;
  union {
    struct td_declaration declaration;
    struct td_expr expr;
    struct td_compoundstmt compound;
    struct td_jumpstmt jump;
    struct td_selectstmt select;
    struct td_iterstmt iter;
    struct td_labeledstmt labeled;
  };

  struct text_span span;
};

/* Function definitions and declarations */

struct td_declaration_list {
  size_t num_declarations;
  struct td_declaration *declarations;
};

struct td_funcdef {
  enum td_storage_class_specifier storage_class_specifier;
  enum td_function_specifier_flags function_specifier_flags;

  struct td_var_declaration var_declaration;
  struct td_stmt body;

  struct text_span span;
};

enum td_external_declaration_ty {
  TD_EXTERNAL_DECLARATION_TY_DECLARATION,
  TD_EXTERNAL_DECLARATION_TY_FUNC_DEF
};

struct td_external_declaration {
  enum td_external_declaration_ty ty;

  union {
    struct td_funcdef func_def;
    struct td_declaration declaration;
  };
};

/* Translation unit (top level) */

struct td_translationunit {
  struct td_external_declaration *external_declarations;
  size_t num_external_declarations;
};

struct typechk;

bool td_var_ty_is_integral_ty(const struct td_var_ty *ty);
bool td_var_ty_is_fp_ty(const struct td_var_ty *ty);
bool td_var_ty_is_scalar_ty(const struct td_var_ty *ty);
bool td_binary_op_is_comparison(enum td_binary_op_ty ty);

struct td_var_ty td_var_ty_pointer_sized_int(struct typechk *tchk,
                                             bool is_signed);

struct td_var_ty
td_var_ty_make_pointer(struct typechk *tchk, const struct td_var_ty *var_ty,
                       enum td_type_qualifier_flags qualifiers);

struct td_var_ty td_var_ty_get_underlying(struct typechk *tchk,
                                          const struct td_var_ty *ty_ref);

struct typechk;

enum typechk_create_result {
  TYPECHK_CREATE_RESULT_SUCCESS,
  TYPECHK_CREATE_RESULT_FAILURE
};

enum typechk_result_ty {
  TYPECHK_RESULT_TY_SUCCESS,
  TYPECHK_RESULT_TY_FAILURE,
};

struct typechk_result {
  enum typechk_result_ty ty;

  struct td_translationunit translation_unit;
};

bool td_var_ty_eq(struct typechk *tchk, const struct td_var_ty *l,
                  const struct td_var_ty *r);

enum typechk_create_result
typechk_create(const struct target *target, const struct compile_args *args,
               struct parser *parser, struct compiler_diagnostics *diagnostics,
               struct typechk **tchk);

struct typechk_result td_typechk(struct typechk *tchk,
                                 struct ast_translationunit *translation_unit);
void typechk_free(struct typechk **tchk);

enum td_node_ty {
  TD_NODE_TY_EXPR,
  TD_NODE_TY_STMT,
  TD_NODE_TY_VAR_DECL,
};

struct td_node {
  enum td_node_ty ty;

  union {
    const struct td_expr *expr;
    const struct td_stmt *stmt;
    const struct td_var_declaration *var_decl;
  };

  struct text_span span;
};

typedef void (td_walk_callback)(const struct td_node *node, void *metadata);
void td_walk(struct typechk *tchk, const struct td_translationunit *unit, td_walk_callback cb, void *metadata);

void debug_print_td(struct typechk *tchk, struct hashtbl *log_symbols,
                    struct td_translationunit *translation_unit);

void hash_td_var(struct hasher *hasher, const void *o);
bool eq_td_var(const void *l, const void *r);

#endif
