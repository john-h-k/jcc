#ifndef PREPROC_H
#define PREPROC_H

#include "program.h"
#include <stdio.h>

enum preproc_create_result { PREPROC_CREATE_RESULT_SUCCESS = 0 };

struct preproc;

enum preproc_create_result preproc_create(struct program *program,
                                          size_t num_include_paths,
                                          const char **include_paths,
                                          struct preproc **preproc);

struct preprocessed_program {
  const char *text;

  const struct preproc_token *tokens;
  size_t num_tokens;
};

enum preproc_token_ty {
  PREPROC_TOKEN_TY_UNKNOWN,
  PREPROC_TOKEN_TY_EOF,

  PREPROC_TOKEN_TY_DIRECTIVE,
  PREPROC_TOKEN_TY_IDENTIFIER,
  PREPROC_TOKEN_TY_PREPROC_NUMBER,
  PREPROC_TOKEN_TY_STRING_LITERAL,
  PREPROC_TOKEN_TY_PUNCTUATOR,
  PREPROC_TOKEN_TY_NEWLINE,
  PREPROC_TOKEN_TY_WHITESPACE,
  PREPROC_TOKEN_TY_COMMENT,
  PREPROC_TOKEN_TY_OTHER,
};

enum preproc_token_punctuator_ty {
  PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET,         // (
  PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET,        // )
  PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACE,           // {
  PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACE,          // }
  PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_SQUARE_BRACKET,  // [
  PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_SQUARE_BRACKET, // ]

  PREPROC_TOKEN_PUNCTUATOR_TY_COLON,     // :
  PREPROC_TOKEN_PUNCTUATOR_TY_SEMICOLON, // ;
  PREPROC_TOKEN_PUNCTUATOR_TY_COMMA,     // ,
  PREPROC_TOKEN_PUNCTUATOR_TY_DOT,       // .
  PREPROC_TOKEN_PUNCTUATOR_TY_ARROW,     // ->
  PREPROC_TOKEN_PUNCTUATOR_TY_QMARK,     // ?

  /* Operators */

  PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_NOT, // !
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_NOT,         // ~

  PREPROC_TOKEN_PUNCTUATOR_TY_OP_INC, // ++
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_DEC, // --

  PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_AND, // &&
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_OR,  // ||
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND,         // &
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND_ASSG,    // &=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR,          // |
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR_ASSG,     // |=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR,         // ^
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR_ASSG,    // ^=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_RSHIFT,      // >>
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_RSHIFT_ASSG, // >>=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_LSHIFT,      // <<
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_LSHIFT_ASSG, // <<=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD,         // +
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD_ASSG,    // +=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB,         // -
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB_ASSG,    // -=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL,         // *
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL_ASSG,    // *=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV,         // /
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV_ASSG,    // /=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT,        // %
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT_ASSG,   // %=

  PREPROC_TOKEN_PUNCTUATOR_TY_OP_ASSG, // =

  PREPROC_TOKEN_PUNCTUATOR_TY_OP_EQ,   // ==
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_NEQ,  // !=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_LT,   // <
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_LTEQ, // <=
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_GT,   // >
  PREPROC_TOKEN_PUNCTUATOR_TY_OP_GTEQ, // >=

  /* Variadic */

  PREPROC_TOKEN_PUNCTUATOR_TY_ELLIPSIS,
};

struct preproc_token_punctuator {
  enum preproc_token_punctuator_ty ty;
};

struct preproc_token {
  enum preproc_token_ty ty;

  struct text_span span;

  union {
    struct preproc_token_punctuator punctuator;
  };
};

void preproc_next_token(struct preproc *preproc,
                               struct preproc_token *token);

void preproc_process(struct preproc *preproc, FILE *file);

void preproc_free(struct preproc **preproc);

#endif
