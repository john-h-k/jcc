#ifndef LEX_H
#define LEX_H

#include "preproc.h"
#include "program.h"
#include "util.h"

enum lex_token_ty {
  LEX_TOKEN_TY_UNKNOWN,
  LEX_TOKEN_TY_EOF,

  /* Trivia (whitespace, comments). Handled entirely by lexer */
  LEX_TOKEN_TY_WHITESPACE,
  LEX_TOKEN_TY_INLINE_COMMENT,
  LEX_TOKEN_TY_MULTILINE_COMMENT,

  /* Bracket types */

  LEX_TOKEN_TY_OPEN_BRACKET,         // (
  LEX_TOKEN_TY_CLOSE_BRACKET,        // )
  LEX_TOKEN_TY_OPEN_BRACE,           // {
  LEX_TOKEN_TY_CLOSE_BRACE,          // }
  LEX_TOKEN_TY_OPEN_SQUARE_BRACKET,  // [
  LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET, // ]

  LEX_TOKEN_TY_COLON,     // :
  LEX_TOKEN_TY_SEMICOLON, // ;
  LEX_TOKEN_TY_COMMA,     // ,
  LEX_TOKEN_TY_DOT,       // .
  LEX_TOKEN_TY_ARROW,     // ->
  LEX_TOKEN_TY_QMARK,     // ?

  /* Operators */

  LEX_TOKEN_TY_OP_LOGICAL_NOT, // !
  LEX_TOKEN_TY_OP_NOT,         // ~

  LEX_TOKEN_TY_OP_INC, // ++
  LEX_TOKEN_TY_OP_DEC, // --

  LEX_TOKEN_TY_OP_LOGICAL_AND, // &&
  LEX_TOKEN_TY_OP_LOGICAL_OR,  // ||
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
  LEX_TOKEN_TY_OP_MOD,         // %
  LEX_TOKEN_TY_OP_MOD_ASSG,    // %=

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
  LEX_TOKEN_TY_KW_NONNULL,
  LEX_TOKEN_TY_KW_NULLABLE,
  LEX_TOKEN_TY_KW_ASM,
  LEX_TOKEN_TY_KW_TYPEOF,
  LEX_TOKEN_TY_KW_TYPEOF_UNQUAL,
  LEX_TOKEN_TY_KW_GENERIC,
  LEX_TOKEN_TY_KW_STATICASSERT,
  LEX_TOKEN_TY_KW_NORETURN,
  LEX_TOKEN_TY_KW_ATTRIBUTE,
  LEX_TOKEN_TY_KW_GOTO,
  LEX_TOKEN_TY_KW_BREAK,
  LEX_TOKEN_TY_KW_CONTINUE,
  LEX_TOKEN_TY_KW_FOR,
  LEX_TOKEN_TY_KW_DO,
  LEX_TOKEN_TY_KW_WHILE,
  LEX_TOKEN_TY_KW_SWITCH,
  LEX_TOKEN_TY_KW_IF,
  LEX_TOKEN_TY_KW_ELSE,
  LEX_TOKEN_TY_KW_DEFAULT,
  LEX_TOKEN_TY_KW_CASE,

  LEX_TOKEN_TY_KW_TYPEDEF,
  LEX_TOKEN_TY_KW_STATIC,
  LEX_TOKEN_TY_KW_EXTERN,
  LEX_TOKEN_TY_KW_AUTO,
  LEX_TOKEN_TY_KW_REGISTER,

  LEX_TOKEN_TY_KW_INLINE,

  LEX_TOKEN_TY_KW_CONST,
  LEX_TOKEN_TY_KW_VOLATILE,
  LEX_TOKEN_TY_KW_RESTRICT,

  LEX_TOKEN_TY_KW_VOID,
  LEX_TOKEN_TY_KW_BOOL,

  LEX_TOKEN_TY_KW_UINT128,
  LEX_TOKEN_TY_KW_HALF,
  LEX_TOKEN_TY_KW_FLOAT,
  LEX_TOKEN_TY_KW_DOUBLE,
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

  LEX_TOKEN_TY_KW_SIZEOF,
  LEX_TOKEN_TY_KW_ALIGNOF, // both `_Alignof` (C11) and `alignof` (C23 onwards)
  LEX_TOKEN_TY_KW_ALIGNAS, // both `_Alignas` (C11) and `alignas` (C23 onwards)

  LEX_TOKEN_TY_IDENTIFIER,

  /* Literals (all suffixes are case-insensitive) */

  LEX_TOKEN_TY_ASCII_CHAR_LITERAL,      // 'a'
  LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL, // L'a'
  LEX_TOKEN_TY_ASCII_STR_LITERAL,       // "foobar"
  LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL,  // "foobar"

  LEX_TOKEN_TY_FLOAT_LITERAL,       // 10.0f
  LEX_TOKEN_TY_DOUBLE_LITERAL,      // 10.0
  LEX_TOKEN_TY_LONG_DOUBLE_LITERAL, // 10.0l

  // Note: `lex.c` relies on `unsigned` being `signed + 1`

  LEX_TOKEN_TY_SIGNED_INT_LITERAL,   // 10
  LEX_TOKEN_TY_UNSIGNED_INT_LITERAL, // 10u

  LEX_TOKEN_TY_SIGNED_LONG_LITERAL,   // 10l
  LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL, // 10ul or 10lu

  LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL,   // 10ll
  LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL, // 10ull or 10llu
};

enum lex_create_result { LEX_CREATE_RESULT_SUCCESS = 0 };

struct lex_token {
  enum lex_token_ty ty;

  const char *text;
  struct text_span span;

  // FIXME: this is temporary, see `consume_token`
  size_t internal_lexer_next_pos;
};

struct lex_pos {
  size_t id;
};

struct lexer;

enum lex_create_result lexer_create(struct program program,
                                    struct preproc *preproc,
                                    enum compile_preproc_mode mode,
                                    struct lexer **lexer);
void lexer_free(struct lexer **lexer);

void lex_all(struct lexer *lexer);

struct lex_pos lex_get_position(struct lexer *lexer);
void lex_backtrack(struct lexer *lexer, struct lex_pos position);

void lex_peek_token(struct lexer *lexer, struct lex_token *token);
void lex_consume_token(struct lexer *lexer, struct lex_token token);

struct text_pos lex_cur_pos(struct lexer *lexer);

bool lexer_at_eof(struct lexer *lexer);

// returns the associated text for a token, or NULL
// e.g
// * `token.ty == LEX_TOKEN_TY_OPEN_PAREN`, this returns NULL
// * `token.ty == LEX_TOKEN_TY_IDENTIFIER`, this returns the identifier
// TODO: maybe make this so it doesn't copy, and returns string length as well
struct sized_str lex_associated_text(const struct lexer *lexer,
                                 const struct lex_token *token);

struct sized_str lex_strlike_associated_text(const struct lexer *lexer,
                                         const struct lex_token *token);

const char *lex_token_name(const struct lexer *lexer,
                       const struct lex_token *token);

#endif
