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
  /* Syntax errors */
  COMPILER_DIAGNOSTIC_CLASS_PARSE,

  /* Semantic errors such as attempting % on floats */
  COMPILER_DIAGNOSTIC_CLASS_SEMANTIC,

  /* Internal errors/bugs in compiler */
  COMPILER_DIAGNOSTIC_CLASS_INTERNAL
};

#define COMPILER_PARSE_DIAGNOSTIC_LIST                                         \
  DIAG_FN(ERROR, "expected-token", expected_token, EXPECTED_TOKEN, const char *) \
  DIAG_FN(ERROR, "expected-expr", expected_expr, EXPECTED_EXPR, const char *) \
  DIAG_FN(ERROR, "expected-init", expected_init, EXPECTED_INIT, const char *) \
  DIAG_FN(ERROR, "expected-type-name", expected_type_name, EXPECTED_TYPE_NAME, const char *) \

enum parse_diagnostic_ty {
#define DIAG_FN(_0, _1, _2, enum, ...) PARSE_DIAGNOSTIC_TY_##enum,

  COMPILER_PARSE_DIAGNOSTIC_LIST

#undef DIAG_FN
};

struct parse_diagnostic {
  enum parse_diagnostic_ty ty;

  struct text_pos start;
  // puts a caret here
  struct text_pos point;
  struct text_pos end;

  union {
#define DIAG_FN(_0, _1, name, _3, ty) ty name;

    COMPILER_PARSE_DIAGNOSTIC_LIST

#undef DIAG_FN
  };
};

struct semantic_diagnostic {
  struct text_pos start;
  struct text_pos end;
};

struct internal_diagnostic {
  const char *message;
};

struct compiler_diagnostic_ty {
  enum compiler_diagnostic_class class;
  enum compiler_diagnostic_severity severity;
};

#define DIAG_FN(_0, _1, name, enum, ty)                                        \
  extern struct compiler_diagnostic_ty DIAGNOSTIC_PARSER_##enum;

COMPILER_PARSE_DIAGNOSTIC_LIST

#undef DIAG_FN

#define MK_PARSER_DIAGNOSTIC(name, lo, start_val, point_val, end_val, value)   \
  (struct compiler_diagnostic) {                                               \
    .ty = DIAGNOSTIC_PARSER_##name, .parse_diagnostic = {                      \
      .ty = PARSE_DIAGNOSTIC_TY_##name,                                        \
      .lo = value,                                                             \
      .start = start_val,                                                      \
      .point = point_val,                                                      \
      .end = end_val                                                           \
    }                                                                          \
  }

struct compiler_diagnostic {
  struct compiler_diagnostic_ty ty;

  union {
    struct parse_diagnostic parse_diagnostic;
    struct semantic_diagnostic semantic_diagnostic;
    struct internal_diagnostic internal_diagnostic;
  };
};

struct compiler_diagnostics;

struct compiler_diagnostics *compiler_diagnostics_create(void);
void compiler_diagnostics_add(struct compiler_diagnostics *diagnostics,
                              struct compiler_diagnostic diagnostic);
void compiler_diagnostics_free(struct compiler_diagnostics **diagnostics);

struct compiler_diagnostics_iter {
  struct compiler_diagnostics *diagnostics;
  size_t idx;
};

struct compiler_diagnostics_iter
compiler_diagnostics_iter(struct compiler_diagnostics *diagnostics);

bool compiler_diagnostics_iter_next(struct compiler_diagnostics_iter *iter,
                                    struct compiler_diagnostic *diagnostic);

#endif
