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

const char *associated_text(struct lexer *lexer, struct token *token) {
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

const char *token_name(struct lexer *lexer, struct token *token) {
  UNUSED_ARG(lexer);

  const char *name;

  switch (token->ty) {
  case LEX_TOKEN_TYPE_WHITESPACE:
    name = "LEX_TOKEN_TYPE_WHITESPACE";
    break;
  case LEX_TOKEN_TYPE_INLINE_COMMENT:
    name = "LEX_TOKEN_TYPE_INLINE_COMMENT";
    break;
  case LEX_TOKEN_TYPE_MULTILINE_COMMENT:
    name = "LEX_TOKEN_TYPE_MULTILINE_COMMENT";
    break;
  case LEX_TOKEN_TYPE_OPEN_BRACKET:
    name = "LEX_TOKEN_TYPE_OPEN_BRACKET";
    break;
  case LEX_TOKEN_TYPE_CLOSE_BRACKET:
    name = "LEX_TOKEN_TYPE_CLOSE_BRACKET";
    break;
  case LEX_TOKEN_TYPE_OPEN_BRACE:
    name = "LEX_TOKEN_TYPE_OPEN_BRACE";
    break;
  case LEX_TOKEN_TYPE_CLOSE_BRACE:
    name = "LEX_TOKEN_TYPE_CLOSE_BRACE";
    break;
  case LEX_TOKEN_TYPE_SEMICOLON:
    name = "LEX_TOKEN_TYPE_SEMICOLON";
    break;
  case LEX_TOKEN_TYPE_KW_INT:
    name = "LEX_TOKEN_TYPE_KW_INT";
    break;
  case LEX_TOKEN_TYPE_KW_RETURN:
    name = "LEX_TOKEN_TYPE_KW_RETURN";
    break;
  case LEX_TOKEN_TYPE_IDENTIFIER:
    name = "LEX_TOKEN_TYPE_IDENTIFIER";
    break;
  case LEX_TOKEN_TYPE_INT_LITERAL:
    name = "LEX_TOKEN_TYPE_INT_LITERAL";
    break;
  }

  return name;
}
