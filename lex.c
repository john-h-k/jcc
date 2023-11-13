#include <ctype.h>

#include "alloc.h"
#include "lex.h"
#include "log.h"
#include "util.h"

struct lexer {
  struct arena_allocator *arena;
  const char *text;
  size_t len;

  struct text_pos pos;

  const char **associated_texts;
};

enum lex_status create_lexer(const char *program, struct lexer **lexer) {
  info("beginning lex stage");

  struct arena_allocator *arena;
  create_arena_allocator(&arena);

  struct lexer *l = nonnull_malloc(sizeof(*l));
  l->arena = arena;
  // copy out the program so lifetimes aren't tied
  l->text = alloc_strcpy(arena, program);
  l->len = strlen(program);
  l->pos.idx = 0;
  l->pos.line = 0;
  l->pos.col = 0;

  *lexer = l;

  return LEX_STATUS_SUCCESS;
}

void free_lexer(struct lexer **lexer) {
  free_arena_allocator(&(*lexer)->arena);
  (*lexer)->arena = NULL;
  free(*lexer);

  *lexer = NULL;
}

bool valid_identifier_char(char c) {
  return isalpha(c) || isdigit(c) || c == '_';
}

bool valid_first_identifier_char(char c) {
  return !isdigit(c) && valid_identifier_char(c);
}

enum lex_token_type refine_ty(struct lexer *lexer, struct text_pos start,
                              struct text_pos end) {
  struct keyword {
    const char *str;
    size_t len;
    enum lex_token_type ty;
  };

  size_t len = text_pos_len(start, end);
  debug("testing '%*.*s' for kw", len, len, &lexer->text[start.idx]);

  static struct keyword keywords[] = {
      {"return", sizeof("return") - 1, LEX_TOKEN_TYPE_KW_RETURN},
      {"int", sizeof("int") - 1, LEX_TOKEN_TYPE_KW_INT}};

  for (size_t i = 0; i < sizeof(keywords) / sizeof(*keywords); i++) {
    if (len == keywords[i].len &&
        memcmp(&lexer->text[start.idx], keywords[i].str, keywords[i].len) == 0) {
      return keywords[i].ty;
    }
  }

  return LEX_TOKEN_TYPE_IDENTIFIER;
}

const char *token_name(struct lexer *lexer, struct token *token);

struct text_pos get_position(struct lexer *lexer) {
  return lexer->pos;
}

void backtrack(struct lexer *lexer, struct text_pos position) {
  lexer->pos = position;
}


void consume_token(struct lexer *lexer, struct token token) {
  lexer->pos = token.end;
}

bool lexer_at_eof(struct lexer *lexer) {
  return lexer->pos.idx >= lexer->len;
}

enum peek_token_result peek_token(struct lexer *lexer, struct token *token) {
  while (lexer->pos.idx < lexer->len && isspace(lexer->text[lexer->pos.idx])) {
    if (lexer->text[lexer->pos.idx] == '\n') {
      // skip newlines, adjust position
      next_line(&lexer->pos);
    } else {
      // just adjust position
      next_col(&lexer->pos);
    }
  }

  struct text_pos start = lexer->pos;
  struct text_pos end = start;

  if (end.idx >= lexer->len) {
    return PEEK_TOKEN_RESULT_EOF;
  }

  char c = lexer->text[start.idx];

  trace("lexing char '%c'", c);

  enum lex_token_type ty;
  switch (c) {
  case '(':
    ty = LEX_TOKEN_TYPE_OPEN_BRACKET;
    next_col(&end);
    break;
  case ')':
    ty = LEX_TOKEN_TYPE_CLOSE_BRACKET;
    next_col(&end);
    break;

  case '{':
    ty = LEX_TOKEN_TYPE_OPEN_BRACE;
    next_col(&end);
    break;
  case '}':
    ty = LEX_TOKEN_TYPE_CLOSE_BRACE;
    next_col(&end);
    break;

  case ';':
    ty = LEX_TOKEN_TYPE_SEMICOLON;
    next_col(&end);
    break;

  case '=':
    ty = LEX_TOKEN_TYPE_OP_ASSG;
    next_col(&end);
    break;
  case '+':
    ty = LEX_TOKEN_TYPE_OP_ADD;
    next_col(&end);
    break;
  case '-':
    ty = LEX_TOKEN_TYPE_OP_SUB;
    next_col(&end);
    break;
  case '*':
    ty = LEX_TOKEN_TYPE_OP_MUL;
    next_col(&end);
    break;
  case '/':
    ty = LEX_TOKEN_TYPE_OP_DIV;
    next_col(&end);
    break;
  case '%':
    ty = LEX_TOKEN_TYPE_OP_QUOT;
    next_col(&end);
    break;

  default: {
    if (isdigit(c)) {
      ty = LEX_TOKEN_TYPE_INT_LITERAL;

      for (size_t i = end.idx; i < lexer->len && isdigit(lexer->text[i]);
           i++) {
        next_col(&end);
      }
    } else if (valid_first_identifier_char(c)) {
      ty = LEX_TOKEN_TYPE_IDENTIFIER;

      for (size_t i = end.idx;
           i < lexer->len && valid_identifier_char(lexer->text[i]); i++) {
        next_col(&end);
      }

      // slightly hacky solution - retroactively determine if identifier is a
      // keyword
      ty = refine_ty(lexer, start, end);
    } else {
      unreachable("unknown!");
    }
  }
  }

  token->ty = ty;
  token->start = start;
  token->end = end;

  debug("parse token %s\n", token_name(lexer, token));

  return PEEK_TOKEN_RESULT_SUCCESS;
}

const char *associated_text(struct lexer *lexer, const struct token *token) {
  switch (token->ty) {
  case LEX_TOKEN_TYPE_IDENTIFIER:
  case LEX_TOKEN_TYPE_INT_LITERAL: {
    size_t len = text_pos_len(token->start, token->end);
    char *p = alloc(lexer->arena, len);
    memcpy(p, &lexer->text[token->start.idx], len);
    return p;
  }
  default:
    return NULL;
  }
}

#define CASE_RET(name) case name: return #name;

const char *token_name(struct lexer *lexer, struct token *token) {
  UNUSED_ARG(lexer);

  switch (token->ty) {
  CASE_RET(LEX_TOKEN_TYPE_WHITESPACE)

  CASE_RET(LEX_TOKEN_TYPE_OP_ASSG)
  CASE_RET(LEX_TOKEN_TYPE_OP_ADD)
  CASE_RET(LEX_TOKEN_TYPE_OP_SUB)
  CASE_RET(LEX_TOKEN_TYPE_OP_MUL)
  CASE_RET(LEX_TOKEN_TYPE_OP_DIV)
  CASE_RET(LEX_TOKEN_TYPE_OP_QUOT)

  CASE_RET(LEX_TOKEN_TYPE_INLINE_COMMENT)
  CASE_RET(LEX_TOKEN_TYPE_MULTILINE_COMMENT)
  CASE_RET(LEX_TOKEN_TYPE_OPEN_BRACKET)
  CASE_RET(LEX_TOKEN_TYPE_CLOSE_BRACKET)
  CASE_RET(LEX_TOKEN_TYPE_OPEN_BRACE)
  CASE_RET(LEX_TOKEN_TYPE_CLOSE_BRACE)
  CASE_RET(LEX_TOKEN_TYPE_SEMICOLON)
  CASE_RET(LEX_TOKEN_TYPE_KW_INT)
  CASE_RET(LEX_TOKEN_TYPE_KW_RETURN)
  CASE_RET(LEX_TOKEN_TYPE_IDENTIFIER)
  CASE_RET(LEX_TOKEN_TYPE_INT_LITERAL)
  }
}
