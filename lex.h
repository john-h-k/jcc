#ifndef LEX_H
#define LEX_H

#include "util.h"

#include <stdlib.h>

enum lex_token_ty {
  LEX_TOKEN_TY_UNKNOWN,
  LEX_TOKEN_TY_EOF,

  /* Trivia (whitespace, comments). Handled entirely by lexer */
  LEX_TOKEN_TY_WHITESPACE,
  LEX_TOKEN_TY_INLINE_COMMENT,
  LEX_TOKEN_TY_MULTILINE_COMMENT,

  /* Bracket types */

  LEX_TOKEN_TY_OPEN_BRACKET,  // (
  LEX_TOKEN_TY_CLOSE_BRACKET, // )
  LEX_TOKEN_TY_OPEN_BRACE,    // {
  LEX_TOKEN_TY_CLOSE_BRACE,   // }

  LEX_TOKEN_TY_SEMICOLON, // ;
  LEX_TOKEN_TY_COMMA,     // ,
  LEX_TOKEN_TY_DOT,       // .

  /* Operators */

  LEX_TOKEN_TY_OP_INC, // ++
  LEX_TOKEN_TY_OP_DEC, // --

  LEX_TOKEN_TY_OP_AND,         // &
  LEX_TOKEN_TY_OP_AND_ASSG,    // &=
  LEX_TOKEN_TY_OP_OR,          // |
  LEX_TOKEN_TY_OP_OR_ASSG,     // |=
  LEX_TOKEN_TY_OP_XOR,         // ^
  LEX_TOKEN_TY_OP_XOR_ASSG,    // ^=
  LEX_TOKEN_TY_OP_RSHIFT,      // >>
  LEX_TOKEN_TY_OP_RSHIFT_ASSG, // >>=
  LEX_TOKEN_TY_OP_LSHIFT,      // <<
  LEX_TOKEN_TY_OP_LSHIFT_ASSG, // <<=
  LEX_TOKEN_TY_OP_ADD,         // +
  LEX_TOKEN_TY_OP_ADD_ASSG,    // +=
  LEX_TOKEN_TY_OP_SUB,         // -
  LEX_TOKEN_TY_OP_SUB_ASSG,    // -=
  LEX_TOKEN_TY_OP_MUL,         // *
  LEX_TOKEN_TY_OP_MUL_ASSG,    // *=
  LEX_TOKEN_TY_OP_DIV,         // /
  LEX_TOKEN_TY_OP_DIV_ASSG,    // /=
  LEX_TOKEN_TY_OP_QUOT,        // %
  LEX_TOKEN_TY_OP_QUOT_ASSG,   // %=

  LEX_TOKEN_TY_OP_ASSG, // =

  LEX_TOKEN_TY_OP_EQ,   // ==
  LEX_TOKEN_TY_OP_NEQ,  // !=
  LEX_TOKEN_TY_OP_LT,   // <
  LEX_TOKEN_TY_OP_LTEQ, // <=
  LEX_TOKEN_TY_OP_GT,   // >
  LEX_TOKEN_TY_OP_GTEQ, // >=

  /* Variadic */

  LEX_TOKEN_TY_ELLIPSIS,

  /* Keywords */
  LEX_TOKEN_TY_KW_FOR,
  LEX_TOKEN_TY_KW_DO,
  LEX_TOKEN_TY_KW_WHILE,
  LEX_TOKEN_TY_KW_IF,
  LEX_TOKEN_TY_KW_ELSE,

  LEX_TOKEN_TY_KW_TYPEDEF,
  LEX_TOKEN_TY_KW_STATIC,
  LEX_TOKEN_TY_KW_EXTERN,
  LEX_TOKEN_TY_KW_AUTO,
  LEX_TOKEN_TY_KW_REGISTER,

  LEX_TOKEN_TY_KW_CONST,
  LEX_TOKEN_TY_KW_VOLATILE,

  LEX_TOKEN_TY_KW_VOID,

  LEX_TOKEN_TY_KW_CHAR,
  LEX_TOKEN_TY_KW_SHORT,
  LEX_TOKEN_TY_KW_INT,
  LEX_TOKEN_TY_KW_LONG,
  LEX_TOKEN_TY_KW_SIGNED,
  LEX_TOKEN_TY_KW_UNSIGNED,
  LEX_TOKEN_TY_KW_RETURN,

  LEX_TOKEN_TY_KW_ENUM,
  LEX_TOKEN_TY_KW_STRUCT,
  LEX_TOKEN_TY_KW_UNION,

  // LEX_TOKEN_TY_KW_STRUCT,
  // LEX_TOKEN_TY_KW_ENUM,
  // LEX_TOKEN_TY_KW_UNION,

  LEX_TOKEN_TY_IDENTIFIER,

  /* Literals (all suffixes are case-insensitive) */

  LEX_TOKEN_TY_ASCII_CHAR_LITERAL, // 'a'
  LEX_TOKEN_TY_ASCII_STR_LITERAL,  // "foobar"

  // Note: `lex.c` relies on `unsigned` being `signed + 1`

  LEX_TOKEN_TY_SIGNED_INT_LITERAL,   // 10
  LEX_TOKEN_TY_UNSIGNED_INT_LITERAL, // 10u

  LEX_TOKEN_TY_SIGNED_LONG_LITERAL,   // 10l
  LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL, // 10ul or 10lu

  LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL,   // 10ll
  LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL, // 10ull or 10llu
};

enum lex_status { LEX_STATUS_SUCCESS = 0 };

struct text_pos {
  size_t idx;
  size_t line;
  size_t col;
};

struct text_span {
  struct text_pos start;
  struct text_pos end;
};

int text_pos_len(struct text_pos start, struct text_pos end);
int text_span_len(const struct text_span *span);

void next_col(struct text_pos *pos);

void next_line(struct text_pos *pos);

struct token {
  struct text_span span;

  enum lex_token_ty ty;
};

struct lexer;

bool lexer_at_eof(struct lexer *lexer);
enum lex_status lexer_create(const char *program, struct lexer **lexer);
void lexer_free(struct lexer **lexer);

struct text_pos get_position(struct lexer *lexer);
void backtrack(struct lexer *lexer, struct text_pos position);

void peek_token(struct lexer *lexer, struct token *token);
void consume_token(struct lexer *lexer, struct token token);

// returns the associated text for a token, or NULL
// e.g
// * `token.ty == LEX_TOKEN_TY_OPEN_PAREN`, this returns NULL
// * `token.ty == LEX_TOKEN_TY_IDENTIFIER`, this returns the identifier
const char *associated_text(struct lexer *lexer, const struct token *token);
const char *token_name(struct lexer *lexer, struct token *token);

#endif
