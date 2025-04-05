#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "program.h"

struct diagnostics;

enum compiler_diagnostic_severity {
  COMPILER_DIAGNOSTIC_SEVERITY_ERROR,
  COMPILER_DIAGNOSTIC_SEVERITY_WARN,
  COMPILER_DIAGNOSTIC_SEVERITY_INFO
};

enum compiler_diagnostic_class {
  /* Preprocessor errors */
  COMPILER_DIAGNOSTIC_CLASS_PREPROC,

  /* Syntax errors */
  COMPILER_DIAGNOSTIC_CLASS_PARSE,

  /* Semantic errors such as attempting % on floats */
  COMPILER_DIAGNOSTIC_CLASS_SEMANTIC,

  /* Internal errors/bugs in compiler */
  COMPILER_DIAGNOSTIC_CLASS_INTERNAL
};

#define COMPILER_PREPROC_DIAGNOSTIC_LIST                                       \
  DIAG_FN(WARN, "warn-directive", warn_directive, WARN_DIRECTIVE,              \
          const char *)                                                        \
  DIAG_FN(ERROR, "error-directive", error_directive, ERROR_DIRECTIVE,          \
          const char *)                                                        \
  DIAG_FN(ERROR, "bad-token-in-cond", bad_token_in_cond, BAD_TOKEN_IN_COND,    \
          const char *)

#define COMPILER_PARSE_DIAGNOSTIC_LIST                                         \
  DIAG_FN(ERROR, "decl-not-stmt", decl_not_stmt, DECL_NOT_STMT, const char *)  \
  DIAG_FN(ERROR, "syntax-err", syntax_err, SYNTAX_ERR, const char *)           \
  DIAG_FN(ERROR, "addr-label", addr_label, ADDR_LABEL, const char *)           \
  DIAG_FN(ERROR, "expected-token", expected_token, EXPECTED_TOKEN,             \
          const char *)                                                        \
  DIAG_FN(ERROR, "expected-expr", expected_expr, EXPECTED_EXPR, const char *)  \
  DIAG_FN(ERROR, "invalid-int-literal", invalid_int_literal,                   \
          INVALID_INT_LITERAL, const char *)                                   \
  DIAG_FN(ERROR, "invalid-floating-point-literal",                             \
          invalid_floating_point_literal, INVALID_FLOATING_POINT_LITERAL,      \
          const char *)                                                        \
  DIAG_FN(ERROR, "invalid-float-literal", invalid_float_literal,               \
          INVALID_FLOAT_LITERAL, const char *)                                 \
  DIAG_FN(ERROR, "expected-init", expected_init, EXPECTED_INIT, const char *)  \
  DIAG_FN(ERROR, "expected-type-name", expected_type_name, EXPECTED_TYPE_NAME, \
          const char *)

#define COMPILER_SEMANTIC_DIAGNOSTIC_LIST                                      \
  DIAG_FN(WARN, "empty-init-c23", empty_init_c23, EMPTY_INIT_C23,              \
          const char *)                                                        \
  DIAG_FN(WARN, "unrecognised-attribute", unrecognised_attr,                   \
          UNRECOGNISED_ATTR, const char *)                                     \
  DIAG_FN(WARN, "too-many-inits", too_many_inits, TOO_MANY_INITS,              \
          const char *)                                                        \
  DIAG_FN(WARN, "pointer-type-mismatch", pointer_type_mismatch,                \
          POINTER_TYPE_MISMATCH, const char *)                                 \
  DIAG_FN(ERROR, "typecheck-sub-ptr-compatible", pointer_sub_types,            \
          POINTER_SUB_TYPES, const char *)                                     \
  DIAG_FN(ERROR, "typecheck-ptr-compatible", pointer_types, POINTER_TYPES,     \
          const char *)                                                        \
  DIAG_FN(ERROR, "typecheck-callable", fn_not_callable, FN_NOT_CALLABLE,       \
          const char *)                                                        \
  DIAG_FN(ERROR, "typecheck-param-count", bad_param_count, BAD_PARAM_COUNT,    \
          const char *)                                                        \
  DIAG_FN(ERROR, "typecheck-deref", bad_deref, BAD_DEREF, const char *)        \
  DIAG_FN(ERROR, "typecheck-incomplete-type", incomplete_type,                 \
          INCOMPLETE_TYPE, const char *)                                       \
  DIAG_FN(ERROR, "typecheck-no-member", no_member, NO_MEMBER, const char *)    \
  DIAG_FN(WARN, "typecheck-bad-printf-args", bad_printf_args, BAD_PRINTF_ARGS, \
          const char *)                                                        \
  DIAG_FN(WARN, "typecheck-bad-printf-specifier", bad_printf_specifier,        \
          BAD_PRINTF_SPECIFIER, const char *)                                  \
  DIAG_FN(ERROR, "typecheck-no-var", no_var, NO_VAR, const char *)             \
  DIAG_FN(ERROR, "typecheck-expected-var", expected_var, EXPECTED_VAR,         \
          const char *)                                                        \
  DIAG_FN(ERROR, "typecheck-fp-bitwise", fp_bitwise, FP_BITWISE, const char *) \
  DIAG_FN(ERROR, "typecheck-cast", cast, CAST, const char *)                   \
  DIAG_FN(WARN, "typecheck-no-decl", no_decl, NO_DECL, const char *)           \
  DIAG_FN(ERROR, "typecheck-static-assert-message", static_assert_message,     \
          STATIC_ASSERT_MESSAGE, const char *)                                 \
  DIAG_FN(ERROR, "typecheck-static-assert", static_assert, STATIC_ASSERT,      \
          const char *)                                                        \
  DIAG_FN(ERROR, "typecheck-not-assignable", not_assignable, NOT_ASSIGNABLE,   \
          const char *)                                                        \
                                                                               \
  DIAG_FN(ERROR, "generic-no-match", generic_no_match, GENERIC_NO_MATCH,       \
          const char *)                                                        \
  DIAG_FN(ERROR, "duplicate-generic-default", duplicate_generic_default,       \
          DUPLICATE_GENERIC_DEFAULT, const char *)                             \
  DIAG_FN(ERROR, "compatible-generic-associations",                            \
          compatible_generic_associations, COMPATIBLE_GENERIC_ASSOCIATIONS,    \
          const char *)                                                        \
  DIAG_FN(ERROR, "empty-init", empty_init, EMPTY_INIT, const char *)           \
  DIAG_FN(ERROR, "scalar-designator", scalar_designator, SCALAR_DESIGNATOR,    \
          const char *)                                                        \
  DIAG_FN(ERROR, "scalar-init-multiple-values", scalar_init_multiple_values,   \
          SCALAR_INIT_MULTIPLE_VALUES, const char *)                           \
  DIAG_FN(ERROR, "enum-identifier-or-list", enum_type, ENUM_TYPE,              \
          const char *)                                                        \
  DIAG_FN(ERROR, "bad-enum-init", bad_enum_init, BAD_ENUM_INIT, const char *)  \
  DIAG_FN(ERROR, "bad-integral-cnst-expr", bad_integral_cnst_expr,             \
          BAD_INTEGRAL_CNST_EXPR, const char *)                                \
  DIAG_FN(ERROR, "bad-static-init-expr", bad_static_init_expr,                 \
          BAD_STATIC_INIT_EXPR, const char *)                                  \
  DIAG_FN(ERROR, "struct-or-union-identifier-or-list", aggregate_type,         \
          AGGREGATE_TYPE, const char *)                                        \
  DIAG_FN(ERROR, "init-in-aggregate", init_in_aggregate, INIT_IN_AGGREGATE,    \
          const char *)                                                        \
  DIAG_FN(ERROR, "init-in-extern", init_in_extern, INIT_IN_EXTERN,             \
          const char *)                                                        \
  DIAG_FN(ERROR, "array-init-type", array_init_type, ARRAY_INIT_TYPE,          \
          const char *)                                                        \
  DIAG_FN(ERROR, "array-init-field-designator", array_init_field_designator,   \
          ARRAY_INIT_FIELD_DESIGNATOR, const char *)                           \
  DIAG_FN(ERROR, "declarator-multiple-sub-declarators", decl_multiple_sub,     \
          DECL_MULTIPLE_SUB, const char *)                                     \
                                                                               \
  DIAG_FN(ERROR, "bad-typedef", bad_typedef, BAD_TYPEDEF, const char *)        \
                                                                               \
  DIAG_FN(ERROR, "bad-bitfield-context", bad_bitfield_context,                 \
          BAD_BITFIELD_CONTEXT, const char *)                                  \
  DIAG_FN(ERROR, "bad-storage-class-specifier-context", bad_storage_context,   \
          BAD_STORAGE_CONTEXT, const char *)                                   \
  DIAG_FN(ERROR, "bad-type-qualifier-specifier-context",                       \
          bad_type_qualifier_context, BAD_TYPE_QUALIFIER_CONTEXT,              \
          const char *)                                                        \
  DIAG_FN(ERROR, "bad-type-specifier-specifier-context",                       \
          bad_type_specifier_context, BAD_TYPE_SPECIFIER_CONTEXT,              \
          const char *)                                                        \
  DIAG_FN(ERROR, "bad-function-specifier-specifier-context",                   \
          bad_function_specifier_context, BAD_FUNCTION_SPECIFIER_CONTEXT,      \
          const char *)                                                        \
                                                                               \
  DIAG_FN(ERROR, "multiple-storage-class-specifiers", storage_class_multiple,  \
          STORAGE_CLASS_MULTIPLE, const char *)                                \
  DIAG_FN(ERROR, "multiple-function-specifiers", function_specifier_multiple,  \
          FUNCTION_SPECIFIER_MULTIPLE, const char *)                           \
  DIAG_FN(WARN, "duplicate-type-qualifiers", type_qualifier_duplicate,         \
          TYPE_QUALIFIER_DUPLICATE, const char *)                              \
  DIAG_FN(ERROR, "bad-type-specifiers", bad_type_specifiers,                   \
          BAD_TYPE_SPECIFIERS, const char *)

enum preproc_diagnostic_ty {
#define DIAG_FN(_0, _1, _2, enum, ...) PREPROC_DIAGNOSTIC_TY_##enum,

  COMPILER_PREPROC_DIAGNOSTIC_LIST

#undef DIAG_FN
};

struct preproc_diagnostic {
  enum preproc_diagnostic_ty ty;
};

enum parse_diagnostic_ty {
#define DIAG_FN(_0, _1, _2, enum, ...) PARSE_DIAGNOSTIC_TY_##enum,

  COMPILER_PARSE_DIAGNOSTIC_LIST

#undef DIAG_FN
};

struct parse_diagnostic {
  enum parse_diagnostic_ty ty;
};

enum semantic_diagnostic_ty {
#define DIAG_FN(_0, _1, _2, enum, ...) SEMANTIC_DIAGNOSTIC_TY_##enum,

  COMPILER_SEMANTIC_DIAGNOSTIC_LIST

#undef DIAG_FN
};

struct semantic_diagnostic {
  enum semantic_diagnostic_ty ty;
};

struct internal_diagnostic {
  const char *message;
};

struct compiler_diagnostic_ty {
  enum compiler_diagnostic_class class;
  enum compiler_diagnostic_severity severity;
  const char *name;
};

#define DIAG_FN(_0, _1, name, enum, ty)                                        \
  extern struct compiler_diagnostic_ty DIAGNOSTIC_PREPROC_##enum;

COMPILER_PREPROC_DIAGNOSTIC_LIST

#undef DIAG_FN

#define DIAG_FN(_0, _1, name, enum, ty)                                        \
  extern struct compiler_diagnostic_ty DIAGNOSTIC_PARSER_##enum;

COMPILER_PARSE_DIAGNOSTIC_LIST

#undef DIAG_FN

#define DIAG_FN(_0, _1, name, enum, ty)                                        \
  extern struct compiler_diagnostic_ty DIAGNOSTIC_SEMANTIC_##enum;

COMPILER_SEMANTIC_DIAGNOSTIC_LIST

#undef DIAG_FN

#define MK_PREPROC_DIAGNOSTIC(name, lo, span_val, point_val, value)            \
  (struct compiler_diagnostic) {                                               \
    .message = value, .span = span_val, .point = point_val,                    \
    .ty = DIAGNOSTIC_PREPROC_##name, .preproc_diagnostic = {                   \
      .ty = PREPROC_DIAGNOSTIC_TY_##name,                                      \
    }                                                                          \
  }

#define MK_PARSER_DIAGNOSTIC(name, lo, span_val, point_val, value)             \
  (struct compiler_diagnostic) {                                               \
    .message = value, .span = span_val, .point = point_val,                    \
    .ty = DIAGNOSTIC_PARSER_##name, .parse_diagnostic = {                      \
      .ty = PARSE_DIAGNOSTIC_TY_##name,                                        \
    }                                                                          \
  }

#define MK_SEMANTIC_DIAGNOSTIC(name, lo, span_val, point_val, value)           \
  (struct compiler_diagnostic) {                                               \
    .message = value, .span = span_val, .point = point_val,                    \
    .ty = DIAGNOSTIC_SEMANTIC_##name, .semantic_diagnostic = {                 \
      .ty = SEMANTIC_DIAGNOSTIC_TY_##name,                                     \
    }                                                                          \
  }

struct compiler_diagnostic {
  struct compiler_diagnostic_ty ty;

  // puts a caret here
  struct text_pos point;
  struct text_span span;

  const char *message;

  union {
    struct preproc_diagnostic preproc_diagnostic;
    struct parse_diagnostic parse_diagnostic;
    struct semantic_diagnostic semantic_diagnostic;
    struct internal_diagnostic internal_diagnostic;
  };
};

struct compiler_diagnostics_args {
  bool warnings_as_errors;
};

struct compiler_diagnostics;

struct compiler_diagnostics *
compiler_diagnostics_create(struct compiler_diagnostics_args args);
void compiler_diagnostics_add(struct compiler_diagnostics *diagnostics,
                              struct compiler_diagnostic diagnostic);
void compiler_diagnostics_free(struct compiler_diagnostics **diagnostics);

struct compiler_diagnostics_iter {
  struct compiler_diagnostics *diagnostics;
  size_t idx;
};

bool compiler_diagnostics_err(struct compiler_diagnostics *diagnostics);

struct compiler_diagnostics_iter
compiler_diagnostics_iter(struct compiler_diagnostics *diagnostics);

bool compiler_diagnostics_iter_next(struct compiler_diagnostics_iter *iter,
                                    struct compiler_diagnostic *diagnostic);

#endif
