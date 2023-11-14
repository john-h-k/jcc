#ifndef ERROR_H
#define ERROR_H

#include "lex.h"

struct diagnostics;

enum compiler_diagnostic_severity {
  COMPILER_DIAGNOSTIC_SEVERITY_ERROR,
  COMPILER_DIAGNOSTIC_SEVERITY_WARN,
  COMPILER_DIAGNOSTIC_SEVERITY_INFO
};

enum compiler_diagnostic_ty {
  /* Syntax errors */
  COMPILER_DIAGNOSTIC_PARSE,

  /* Semantic errors such as attempting % on floats */
  COMPILER_DIAGNOSTIC_SEMANTIC,

  /* Internal errors/bugs in compiler */
  COMPILER_DIAGNOSTIC_INTERNAL
};

struct compiler_diagnostic_id {
  size_t id;
};

struct parse_diagnostic {  
  struct text_pos start;
  struct text_pos end;
};

struct semantic_diagnostic {
  struct text_pos start;
  struct text_pos end;
};

struct internal_diagnostic {
  const char *message;
};

struct compiler_diagnostic {
  struct compiler_diagnostic_id id;
  enum compiler_diagnostic_ty ty;
  enum compiler_diagnostic_severity severity;

  union {
    struct parse_diagnostic parse_diagnostic;
    struct semantic_diagnostic semantic_diagnostic;
    struct internal_diagnostic internal_diagnostic;
  };
};

#endif
