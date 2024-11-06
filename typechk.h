#ifndef TYPECHK_H
#define TYPECHK_H

#include "parse.h"

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

  WELL_KNOWN_TY_HALF,
  WELL_KNOWN_TY_FLOAT,
  WELL_KNOWN_TY_DOUBLE,
  WELL_KNOWN_TY_LONG_DOUBLE,
};

#define WKT_MAKE_SIGNED(wkt)                                                   \
  (((wkt) >= WELL_KNOWN_TY_FLOAT) ? (wkt) : ((wkt) & ~1))
#define WKT_MAKE_UNSIGNED(wkt)                                                 \
  (((wkt) >= WELL_KNOWN_TY_FLOAT) ? (wkt) : ((wkt) | 1))

#define WKT_IS_SIGNED(wkt) (((wkt) & 1) == 0)

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
  struct token **param_identifiers;
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
  AST_TYREF_TY_TAGGED,    // e.g `struct foo`
  // AST_TYREF_TY_TYPEDEF_NAME,
  // AST_TYREF_TY_ENUM,
};

struct typechk {
  // `value` contains a `struct ast_tyref *` to the type of the variable
  // or NULL if the variable has been used without a declaration
  struct var_table var_table;

  // types (e.g declared structs)
  struct var_table ty_table;  
};

bool is_integral_ty(const struct ast_tyref *ty);
bool is_fp_ty(const struct ast_tyref *ty);

struct ast_tyref tyref_pointer_sized_int(struct parser *parser, bool is_signed);


struct ast_tyref tyref_make_pointer(struct parser *parser,
                                    const struct ast_tyref *var_ty);

struct ast_tyref tyref_get_defined(struct parser *parser,
                                   const struct ast_tyref *ty_ref);

struct ast_tyref tyref_get_underlying(struct parser *parser,
                                      const struct ast_tyref *ty_ref);


void ast_typchk(struct ast_translationunit *translation_unit);

#endif
