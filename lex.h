#ifndef LEX_H
#define LEX_H

#include <stdlib.h>

enum lex_token_type {
  LEX_TOKEN_TYPE_WHITESPACE,
  LEX_TOKEN_TYPE_INLINE_COMMENT,
  LEX_TOKEN_TYPE_MULTILINE_COMMENT,

  /* Bracket types */
  
  LEX_TOKEN_TYPE_OPEN_BRACKET,   // (
  LEX_TOKEN_TYPE_CLOSE_BRACKET,  // )
  LEX_TOKEN_TYPE_OPEN_BRACE,     // {
  LEX_TOKEN_TYPE_CLOSE_BRACE,    // }

  LEX_TOKEN_TYPE_SEMICOLON,
  LEX_TOKEN_TYPE_COMMA,

  /* Operators */

  LEX_TOKEN_TYPE_OP_ASSG,  // +

  LEX_TOKEN_TYPE_OP_ADD,  // +
  LEX_TOKEN_TYPE_OP_SUB,  // -
  LEX_TOKEN_TYPE_OP_MUL,  // *
  LEX_TOKEN_TYPE_OP_DIV,  // /
  LEX_TOKEN_TYPE_OP_QUOT, // %

  /* Keywords */

  LEX_TOKEN_TYPE_KW_CHAR,
  LEX_TOKEN_TYPE_KW_SHORT,
  LEX_TOKEN_TYPE_KW_INT,
  LEX_TOKEN_TYPE_KW_LONG,
  LEX_TOKEN_TYPE_KW_SIGNED,
  LEX_TOKEN_TYPE_KW_UNSIGNED,
  LEX_TOKEN_TYPE_KW_RETURN,

  // LEX_TOKEN_TYPE_KW_STRUCT,
  // LEX_TOKEN_TYPE_KW_ENUM,
  // LEX_TOKEN_TYPE_KW_UNION,

  LEX_TOKEN_TYPE_IDENTIFIER,

  /* Literals (all suffixes are case-insensitive) */
  
  LEX_TOKEN_TYPE_ASCII_CHAR_LITERAL,         // 'a'

  // Note: `lex.c` relies on `unsigned` being `signed + 1`

  LEX_TOKEN_TYPE_SIGNED_INT_LITERAL,         // 10
  LEX_TOKEN_TYPE_UNSIGNED_INT_LITERAL,       // 10u

  LEX_TOKEN_TYPE_SIGNED_LONG_LITERAL,        // 10l
  LEX_TOKEN_TYPE_UNSIGNED_LONG_LITERAL,      // 10ul or 10lu

  LEX_TOKEN_TYPE_SIGNED_LONG_LONG_LITERAL,   // 10ll
  LEX_TOKEN_TYPE_UNSIGNED_LONG_LONG_LITERAL, // 10ull or 10llu
};

enum lex_status {
  LEX_STATUS_SUCCESS = 0
};

struct text_pos {
  size_t idx;
  size_t line;
  size_t col;
};

int text_pos_len(struct text_pos start, struct text_pos end);

void next_col(struct text_pos *pos);

void next_line(struct text_pos *pos);

struct token {
  struct text_pos start;
  struct text_pos end;

  enum lex_token_type ty;
};

struct lexer;

bool lexer_at_eof(struct lexer *lexer);
enum lex_status create_lexer(const char *program, struct lexer **lexer);
void free_lexer(struct lexer **lexer);

enum peek_token_result {
  PEEK_TOKEN_RESULT_SUCCESS,
  PEEK_TOKEN_RESULT_EOF
};

struct text_pos get_position(struct lexer *lexer);
void backtrack(struct lexer *lexer, struct text_pos position);

enum peek_token_result peek_token(struct lexer *lexer, struct token *token);

void consume_token(struct lexer *lexer, struct token token);

// returns the associated text for a token, or NULL
// e.g
// * `token.ty == LEX_TOKEN_TYPE_OPEN_PAREN`, this returns NULL
// * `token.ty == LEX_TOKEN_TYPE_IDENTIFIER`, this returns the identifier
const char *associated_text(struct lexer* lexer, const struct token *token);
const char *token_name(struct lexer *lexer, struct token *token);

#endif
