#ifndef PARSE_H
#define PARSE_H

#include "lex.h"
#include "program.h"

/* Type refs - `<enum|struct|union> <identifier`, `<typedef-name>`, or
 * `<keyword>` */

enum ast_attribute_ty {
  AST_ATTRIBUTE_TY_EMPTY,
  AST_ATTRIBUTE_TY_NAMED,
  AST_ATTRIBUTE_TY_PARAMETERIZED,
};

struct ast_attribute_param {
  struct ast_expr *expr;
};

struct ast_attribute {
  enum ast_attribute_ty ty;

  struct lex_token name;
  struct ast_attribute_param *params;
  size_t num_params;
};

struct ast_attribute_list {
  struct ast_attribute *attributes;
  size_t num_attributes;
};

struct ast_attribute_specifier {  
  struct ast_attribute_list attribute_list;
};

enum ast_storage_class_specifier {
  AST_STORAGE_CLASS_SPECIFIER_TYPEDEF,
  AST_STORAGE_CLASS_SPECIFIER_EXTERN,
  AST_STORAGE_CLASS_SPECIFIER_STATIC,
  AST_STORAGE_CLASS_SPECIFIER_AUTO,
  AST_STORAGE_CLASS_SPECIFIER_REGISTER,
};

enum ast_function_specifier {
  AST_FUNCTION_SPECIFIER_INLINE,
};

enum ast_type_qualifier {
  AST_TYPE_QUALIFIER_CONST,
  AST_TYPE_QUALIFIER_VOLATILE,
  AST_TYPE_QUALIFIER_RESTRICT,
};

struct ast_declaration_specifier_list {
  size_t num_decl_specifiers;
  struct ast_declaration_specifier *decl_specifiers;
};

struct ast_pointer {
  struct ast_declaration_specifier_list specifier_list;
};

struct ast_pointer_list {
  size_t num_pointers;
  struct ast_pointer *pointers;
};

enum ast_direct_declarator_ty {
  AST_DIRECT_DECLARATOR_TY_IDENTIFIER,
  AST_DIRECT_DECLARATOR_TY_PAREN_DECLARATOR,
  AST_DIRECT_DECLARATOR_TY_ARRAY_DECLARATOR,
  AST_DIRECT_DECLARATOR_TY_FUNC_DECLARATOR,
};

struct ast_direct_declarator {
  enum ast_direct_declarator_ty ty;

  union {
    struct lex_token identifier;
    struct ast_declarator *paren_declarator;
    struct ast_array_declarator *array_declarator;
    struct ast_func_declarator *func_declarator;
  };
};

struct ast_direct_declarator_list {
  size_t num_direct_declarators;
  struct ast_direct_declarator *direct_declarators;
};

enum ast_declarator_ty {
  AST_DECLARATOR_TY_VALUE,
  AST_DECLARATOR_TY_POINTER,
};

struct ast_declarator {
  struct ast_pointer_list pointer_list;
  struct ast_direct_declarator_list direct_declarator_list;
  // TODO: we actually want attribute_list_list
  // __attribute__((foo, bar)) is an attribute_list
  // __attribute__((foo)) __attribute__(bar) is an attribute_list_list
  struct ast_attribute_specifier attribute_specifier;
  struct ast_expr *bitfield_size;
};

// struct ast_struct_declarator {
//   struct ast_declarator declarator;
//   struct ast_expr *bitfield_size;
// };

// struct ast_struct_declarator_list {
//   size_t num_declarators;
//   struct ast_struct_declarator *declarators;
// };

// struct ast_struct_declaration {
//   struct ast_declaration_specifier_list decl_specifiers;
//   struct ast_struct_declarator_list struct_declarator_list;
// };

struct ast_struct_declaration_list {
  size_t num_declarations;
  struct ast_declaration *declarations;
};

enum ast_struct_or_union_specifier_ty {
  AST_STRUCT_OR_UNION_SPECIFIER_TY_STRUCT,
  AST_STRUCT_OR_UNION_SPECIFIER_TY_UNION,
};

struct ast_declaration_list {
  size_t num_declarations;
  struct ast_declaration *declarations;
};

struct ast_struct_or_union_specifier {
  enum ast_struct_or_union_specifier_ty ty;

  struct lex_token *identifier;
  struct ast_declaration_list *decl_list;
};

struct ast_enumerator {
  struct lex_token identifier;
  struct ast_expr *value;
};

struct ast_enumerator_list {
  size_t num_enumerators;
  struct ast_enumerator *enumerators;
};

struct ast_enum_specifier {
  struct lex_token *identifier;
  struct ast_enumerator_list *enumerator_list;
};

enum ast_type_specifier_ty {
  AST_TYPE_SPECIFIER_TY_KW,
  AST_TYPE_SPECIFIER_STRUCT_OR_UNION,
  AST_TYPE_SPECIFIER_ENUM,
  AST_TYPE_SPECIFIER_TYPEDEF_NAME
};

enum ast_type_specifier_kw {
  AST_TYPE_SPECIFIER_KW_VOID,
  AST_TYPE_SPECIFIER_KW_CHAR,
  AST_TYPE_SPECIFIER_KW_SHORT,
  AST_TYPE_SPECIFIER_KW_INT,
  AST_TYPE_SPECIFIER_KW_LONG,
  AST_TYPE_SPECIFIER_KW_FLOAT,
  AST_TYPE_SPECIFIER_KW_DOUBLE,
  AST_TYPE_SPECIFIER_KW_SIGNED,
  AST_TYPE_SPECIFIER_KW_UNSIGNED,

  AST_TYPE_SPECIFIER_KW_BOOL,
  AST_TYPE_SPECIFIER_KW_COMPLEX,

  AST_TYPE_SPECIFIER_KW_HALF,
};

struct ast_type_specifier {
  enum ast_type_specifier_ty ty;

  union {
    enum ast_type_specifier_kw type_specifier_kw;
    struct ast_struct_or_union_specifier struct_or_union_specifier;
    struct ast_enum_specifier enum_specifier;
    struct lex_token typedef_name;
  };
};

enum ast_decl_specifier_ty {
  AST_DECL_SPECIFIER_TY_STORAGE_CLASS_SPECIFIER,
  AST_DECL_SPECIFIER_TY_TYPE_SPECIFIER,
  AST_DECL_SPECIFIER_TY_TYPE_QUALIFIER,
  AST_DECL_SPECIFIER_TY_FUNCTION_SPECIFIER,
  AST_DECL_SPECIFIER_TY_ATTRIBUTE_SPECIFIER,
};

struct ast_declaration_specifier {
  enum ast_decl_specifier_ty ty;

  union {
    enum ast_storage_class_specifier storage_class_specifier;
    enum ast_type_qualifier type_qualifier;
    enum ast_function_specifier function_specifier;
    struct ast_type_specifier type_specifier;
    struct ast_attribute_specifier attribute_specifier;
  };
};

enum ast_array_declarator_ty {
  AST_ARRAY_DECLARATOR_TY_STAR,
  AST_ARRAY_DECLARATOR_TY_STATIC_SIZED,
  AST_ARRAY_DECLARATOR_TY_SIZED,
  AST_ARRAY_DECLARATOR_TY_UNSIZED,
};

struct ast_array_declarator {
  enum ast_array_declarator_ty ty;
  struct ast_declaration_specifier_list specifier_list;
  struct ast_expr *size;
};

struct ast_func_declarator {
  struct ast_paramlist *param_list;
};

enum ast_direct_abstract_declarator_ty {
  AST_DIRECT_ABSTRACT_DECLARATOR_TY_PAREN_DECLARATOR,
  AST_DIRECT_ABSTRACT_DECLARATOR_TY_ARRAY_DECLARATOR,
  AST_DIRECT_ABSTRACT_DECLARATOR_TY_FUNC_DECLARATOR,
};

struct ast_direct_abstract_declarator {
  enum ast_direct_abstract_declarator_ty ty;

  union {
    struct ast_abstract_declarator *paren_declarator;
    struct ast_array_declarator *array_declarator;
    struct ast_func_declarator *func_declarator;
  };
};

struct ast_direct_abstract_declarator_list {
  size_t num_direct_abstract_declarators;
  struct ast_direct_abstract_declarator *direct_abstract_declarators;
};

struct ast_abstract_declarator {
  struct ast_pointer_list pointer_list;
  struct ast_direct_abstract_declarator_list direct_abstract_declarator_list;
};

struct ast_type_name {
  struct ast_declaration_specifier_list specifier_list;
  struct ast_abstract_declarator abstract_declarator;
};

// TODO: try and parse init lists as expressions to give better error messages
enum ast_init_ty {
  AST_INIT_TY_EXPR,
  AST_INIT_TY_INIT_LIST,
};

struct ast_arglist {
  struct ast_expr *args;
  size_t num_args;
};

/* Variable references */

struct ast_var {
  struct lex_token identifier;
};

enum ast_param_ty {
  AST_PARAM_TY_DECL,
  AST_PARAM_TY_ABSTRACT_DECL,
  AST_PARAM_TY_VARIADIC,
  AST_PARAM_TY_VOID
};

struct ast_param {
  enum ast_param_ty ty;

  struct ast_declaration_specifier_list specifier_list;

  union {
    struct ast_declarator declarator;
    struct ast_abstract_declarator abstract_declarator;
  };
};

struct ast_paramlist {
  struct ast_param *params;
  size_t num_params;
};

/* Constant values (literals) */

enum ast_cnst_ty {
  AST_CNST_TY_SIGNED_INT,
  AST_CNST_TY_UNSIGNED_INT,
  AST_CNST_TY_SIGNED_LONG,
  AST_CNST_TY_UNSIGNED_LONG,
  AST_CNST_TY_SIGNED_LONG_LONG,
  AST_CNST_TY_UNSIGNED_LONG_LONG,

  AST_CNST_TY_FLOAT,
  AST_CNST_TY_DOUBLE,
  AST_CNST_TY_LONG_DOUBLE,

  AST_CNST_TY_CHAR,
  AST_CNST_TY_WIDE_CHAR,

  AST_CNST_TY_STR_LITERAL,
  AST_CNST_TY_WIDE_STR_LITERAL,
};

struct ast_cnst_str {
  const char *value;
  size_t len;
};

struct ast_cnst {
  enum ast_cnst_ty ty;

  union {
    unsigned long long int_value;
    struct ast_cnst_str str_value;
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
  AST_UNARY_OP_TY_ADDRESSOF,
  AST_UNARY_OP_TY_CAST,
};

struct ast_cast {
  struct ast_type_name type_name;
};

struct ast_unary_op {
  enum ast_unary_op_ty ty;
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
  struct ast_expr *lhs;
  struct ast_expr *rhs;
};

struct ast_call {
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

enum ast_designator_ty {
  AST_DESIGNATOR_TY_FIELD,
  AST_DESIGNATOR_TY_INDEX,
};

struct ast_designator {
  enum ast_designator_ty ty;

  union {
    struct lex_token field;
    struct ast_expr *index;
  };
};

struct ast_designator_list {
  size_t num_designators;
  struct ast_designator *designators;
};

struct ast_init_list_init {
  struct ast_designator_list *designator_list;
  struct ast_init *init;
};

struct ast_init_list {
  struct ast_init_list_init *inits;
  size_t num_inits;
};

// Assignments - anything of form `<lvalue> = <lvalue | rvalue>` (so `<lvalue>
// = <expr>`)

enum ast_assg_ty {
  AST_ASSG_TY_BASIC,
  AST_ASSG_TY_ADD,
  AST_ASSG_TY_SUB,
  AST_ASSG_TY_MUL,
  AST_ASSG_TY_DIV,
  AST_ASSG_TY_QUOT,
  AST_ASSG_TY_AND,
  AST_ASSG_TY_OR,
  AST_ASSG_TY_XOR,
  AST_ASSG_TY_LSHIFT,
  AST_ASSG_TY_RSHIFT,
};

struct ast_assg {
  enum ast_assg_ty ty;

  struct ast_expr *assignee;
  struct ast_expr *expr;
};

struct ast_arrayaccess {
  // The lhs will always be an array/pointer type
  // rhs may be integral type
  struct ast_expr *lhs;
  struct ast_expr *rhs;
};

struct ast_memberaccess {
  struct ast_expr *lhs;
  struct lex_token member;
};

struct ast_pointeraccess {
  struct ast_expr *lhs;
  struct lex_token member;
};

enum ast_sizeof_ty {
  AST_SIZEOF_TY_TYPE,
  AST_SIZEOF_TY_EXPR,
};

struct ast_sizeof {
  enum ast_sizeof_ty ty;

  union {
    struct ast_expr *expr;
    struct ast_type_name type_name;
  };
};

struct ast_alignof {
  struct ast_type_name type_name;
};

struct ast_ternary {
  struct ast_expr *cond;
  struct ast_expr *true_expr;
  struct ast_expr *false_expr;
};

/* Expressions - divided into `lvalue` (can be on left hand side of assignment)
 * and `rvalue` (not an lvalue) */

struct ast_compound_literal {
  struct ast_type_name type_name;
  struct ast_init_list init_list;
};

enum ast_expr_ty {
  AST_EXPR_TY_TERNARY,
  AST_EXPR_TY_CALL,
  AST_EXPR_TY_UNARY_OP,
  AST_EXPR_TY_BINARY_OP,
  AST_EXPR_TY_ARRAYACCESS,
  AST_EXPR_TY_MEMBERACCESS,
  AST_EXPR_TY_POINTERACCESS,
  AST_EXPR_TY_ASSG,
  AST_EXPR_TY_VAR,
  AST_EXPR_TY_CNST,
  AST_EXPR_TY_COMPOUNDEXPR,
  AST_EXPR_TY_SIZEOF,
  AST_EXPR_TY_ALIGNOF,
  AST_EXPR_TY_COMPOUND_LITERAL,
};

struct ast_expr {
  enum ast_expr_ty ty;

  union {
    struct ast_ternary ternary;
    struct ast_sizeof size_of;
    struct ast_alignof align_of;
    struct ast_var var;
    struct ast_cnst cnst;
    struct ast_compoundexpr compound_expr;
    struct ast_unary_op unary_op;
    struct ast_binary_op binary_op;
    struct ast_assg assg;
    struct ast_call call;
    struct ast_arrayaccess array_access;
    struct ast_memberaccess member_access;
    struct ast_pointeraccess pointer_access;
    struct ast_compound_literal compound_literal;
  };
};

/* Variable declarations - `<typename> <comma seperated list of declarations>`
 * where each declaration is `<name>` or `<name> = <expr>` */

struct ast_init {
  enum ast_init_ty ty;

  union {
    struct ast_expr expr;
    struct ast_init_list init_list;
  };
};

struct ast_init_declarator {
  struct ast_declarator declarator;
  struct ast_init *init;
};

struct ast_init_declarator_list {
  size_t num_init_declarators;
  struct ast_init_declarator *init_declarators;
};

struct ast_declaration {
  struct ast_declaration_specifier_list specifier_list;
  struct ast_init_declarator_list declarator_list;
};

/* Jump statements - `return`, `break`, `continue`, `goto` */

struct ast_returnstmt {
  struct ast_expr *expr;
};

struct ast_gotostmt {
  struct lex_token label;
};

enum ast_jumpstmt_ty {
  AST_JUMPSTMT_TY_BREAK,
  AST_JUMPSTMT_TY_CONTINUE,
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

enum ast_labeledstmt_ty {
  AST_LABELEDSTMT_TY_LABEL,
  AST_LABELEDSTMT_TY_CASE,
  AST_LABELEDSTMT_TY_DEFAULT,
};

struct ast_labeledstmt {
  enum ast_labeledstmt_ty ty;

  struct ast_stmt *stmt;

  union {
    struct ast_expr cnst;
    struct lex_token label;
  };
};

struct ast_ifstmt {
  struct ast_expr cond;
  struct ast_stmt *body;
};

struct ast_ifelsestmt {
  struct ast_expr cond;
  struct ast_stmt *body;
  struct ast_stmt *else_body;
};

struct ast_switchstmt {
  struct ast_expr ctrl_expr;
  struct ast_stmt *body;
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

enum ast_declaration_or_expr_ty {
  AST_DECLARATION_OR_EXPR_TY_DECL,
  AST_DECLARATION_OR_EXPR_TY_EXPR,
};

struct ast_declaration_or_expr {
  enum ast_declaration_or_expr_ty ty;

  union {
    struct ast_declaration decl;
    struct ast_expr expr;
  };
};

struct ast_forstmt {
  struct ast_declaration_or_expr *init;
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
  AST_STMT_TY_DECLARATION,
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
    struct ast_declaration declaration;
    struct ast_expr expr;
    struct ast_compoundstmt compound;
    struct ast_jumpstmt jump;
    struct ast_selectstmt select;
    struct ast_iterstmt iter;
    struct ast_labeledstmt labeled;
  };
};

/* Function definitions and declarations */

struct ast_funcdef {
  struct ast_declaration_specifier_list specifier_list;
  struct ast_declarator declarator;
  struct ast_declaration_list declaration_list;
  struct ast_compoundstmt body;
};

enum ast_external_declaration_ty {
  AST_EXTERNAL_DECLARATION_TY_DECLARATION,
  AST_EXTERNAL_DECLARATION_TY_FUNC_DEF
};

struct ast_external_declaration {
  enum ast_external_declaration_ty ty;

  union {
    struct ast_funcdef func_def;
    struct ast_declaration declaration;
  };
};

/* Translation unit (top level) */

struct ast_translationunit {
  struct ast_external_declaration *external_declarations;
  size_t num_external_declarations;
};

struct parser;

enum parser_create_result {
  PARSER_CREATE_RESULT_SUCCESS,
  PARSER_CREATE_RESULT_FAILURE
};

struct parse_result {
  struct ast_translationunit translation_unit;
};

enum parser_create_result parser_create(struct program *program,
                                        struct preproc *preproc,
                                        struct parser **parser);
struct parse_result parse(struct parser *parser);
void parser_free(struct parser **parser);

const char *identifier_str(struct parser *parser,
                           const struct lex_token *token);

void debug_print_ast(struct parser *parser,
                     struct ast_translationunit *translation_unit);

#endif
