#include "lex.h"

#include "alloc.h"
#include "log.h"
#include "util.h"

#include <ctype.h>

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
  l->text = arena_alloc_strcpy(arena, program);
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

enum lex_token_ty refine_ty(struct lexer *lexer, struct text_pos start,
                            struct text_pos end) {
  struct keyword {
    const char *str;
    size_t len;
    enum lex_token_ty ty;
  };

  size_t len = text_pos_len(start, end);
  debug("testing '%*.*s' for kw", len, len, &lexer->text[start.idx]);

#define KEYWORD(kw, ty)                                                        \
  { kw, sizeof(kw) - 1, ty }

  // TODO: hashify
  static struct keyword keywords[] = {
      KEYWORD("do", LEX_TOKEN_TYPE_KW_DO),
      KEYWORD("for", LEX_TOKEN_TYPE_KW_FOR),
      KEYWORD("while", LEX_TOKEN_TYPE_KW_WHILE),
      KEYWORD("if", LEX_TOKEN_TYPE_KW_IF),
      KEYWORD("else", LEX_TOKEN_TYPE_KW_ELSE),
      KEYWORD("return", LEX_TOKEN_TYPE_KW_RETURN),
      KEYWORD("char", LEX_TOKEN_TYPE_KW_CHAR),
      KEYWORD("short", LEX_TOKEN_TYPE_KW_SHORT),
      KEYWORD("int", LEX_TOKEN_TYPE_KW_INT),
      KEYWORD("long", LEX_TOKEN_TYPE_KW_LONG),
      KEYWORD("unsigned", LEX_TOKEN_TYPE_KW_UNSIGNED),
      KEYWORD("signed", LEX_TOKEN_TYPE_KW_SIGNED),
  };

  for (size_t i = 0; i < sizeof(keywords) / sizeof(*keywords); i++) {
    if (len == keywords[i].len &&
        memcmp(&lexer->text[start.idx], keywords[i].str, keywords[i].len) ==
            0) {
      return keywords[i].ty;
    }
  }

  return LEX_TOKEN_TYPE_IDENTIFIER;
}

const char *token_name(struct lexer *lexer, struct token *token);

struct text_pos get_position(struct lexer *lexer) { return lexer->pos; }

void backtrack(struct lexer *lexer, struct text_pos position) {
  lexer->pos = position;
}

void consume_token(struct lexer *lexer, struct token token) {
  lexer->pos = token.span.end;
}

// it returns bool in case we hit EOF without hitting the end token
void find_eol(struct lexer *lexer, struct text_pos *cur_pos) {
  for (; cur_pos->idx < lexer->len && lexer->text[cur_pos->idx] != '\n';
       next_col(cur_pos)) {
    // nothing
  }

  if (cur_pos->idx < lexer->len) {
    next_line(cur_pos);
  }

  // we have either hit end of line or end of file
  // we treat both as a valid eol
}

// attempts to find the `*/` token
// rather than just lexing the comment itself
// this may be marginally faster, also may not make much of a difference
// it returns bool in case we hit EOF without hitting the end token
bool try_find_comment_end(struct lexer *lexer, struct text_pos *cur_pos) {
  while (/* token must be at least 2 chars */ cur_pos->idx + 1 < lexer->len) {
    if (lexer->text[cur_pos->idx] == '\n') {
      next_line(cur_pos);
    } else if (lexer->text[cur_pos->idx] == '*' &&
               lexer->text[cur_pos->idx + 1] == '/') {
      // found it!
      next_col(cur_pos);
      next_col(cur_pos);
      return true;
    } else {
      next_col(cur_pos);
    }
  }

  return false;
}

bool try_consume(struct lexer *lexer, struct text_pos *pos, char c) {
  debug_assert(
      lexer->pos.idx != pos->idx,
      "calling `try_consume` with `pos` the same as lexer makes no sense");
  if (pos->idx < lexer->len && lexer->text[pos->idx] == c) {
    if (c == '\n') {
      next_line(pos);
    } else {
      next_col(pos);
    }

    return true;
  }

  return false;
}

bool lexer_at_eof(struct lexer *lexer) {
  // needed to skip whitespace
  struct token token;
  peek_token(lexer, &token);

  return lexer->pos.idx >= lexer->len;
}

void peek_token(struct lexer *lexer, struct token *token) {
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
    token->ty = LEX_TOKEN_TYPE_EOF;
    token->span.start = start;
    token->span.end = end;
    return;
  }

  char c = lexer->text[start.idx];

  trace("lexing char '%c'", c);

  enum lex_token_ty ty;
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
  case ',':
    ty = LEX_TOKEN_TYPE_COMMA;
    next_col(&end);
    break;

  case '=':
    ty = LEX_TOKEN_TYPE_OP_ASSG;
    next_col(&end);
    break;
  case '+':
    next_col(&end);
    if (try_consume(lexer, &end, '+')) {
      ty = LEX_TOKEN_TYPE_OP_INC;
    } else if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TYPE_OP_ADD_ASSG;
    } else {
      ty = LEX_TOKEN_TYPE_OP_ADD;
    }
    break;
  case '-':
    next_col(&end);
    if (try_consume(lexer, &end, '-')) {
      ty = LEX_TOKEN_TYPE_OP_DEC;
    } else if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TYPE_OP_SUB_ASSG;
    } else {
      ty = LEX_TOKEN_TYPE_OP_SUB;
    }
    break;
  case '*':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TYPE_OP_MUL_ASSG;
    } else {
      ty = LEX_TOKEN_TYPE_OP_MUL;
    }
    break;
  case '/':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TYPE_OP_DIV_ASSG;
    } else {
      ty = LEX_TOKEN_TYPE_OP_DIV;
    }

    // this approach is ugly, TODO: refactor
    // look for `//` and `/*` comment tokens
    if (end.idx < lexer->len && lexer->text[end.idx] == '/') {
      ty = LEX_TOKEN_TYPE_INLINE_COMMENT;
      find_eol(lexer, &end);
      lexer->pos = end;

      // find next token
      peek_token(lexer, token);
      return;
    } else if (end.idx < lexer->len && lexer->text[end.idx] == '*') {
      ty = LEX_TOKEN_TYPE_MULTILINE_COMMENT;
      if (!try_find_comment_end(lexer, &end)) {
        bug("handle unended /* comments");
      }
      lexer->pos = end;

      // find next token
      peek_token(lexer, token);
      return;
    }

    ty = LEX_TOKEN_TYPE_OP_DIV;
    break;
  case '%':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TYPE_OP_QUOT_ASSG;
    } else {
      ty = LEX_TOKEN_TYPE_OP_QUOT;
    }
    break;

  default: {
    if (isdigit(c)) {
      ty = LEX_TOKEN_TYPE_SIGNED_INT_LITERAL;

      for (size_t i = end.idx; i < lexer->len && isdigit(lexer->text[i]); i++) {
        next_col(&end);
      }

      bool is_unsigned = false;

      while (end.idx + 1 < lexer->len) {
        debug("current pos %d", end.idx);
        switch (tolower(lexer->text[end.idx + 1])) {
        case 'u':
          debug("found u!");
          is_unsigned = true;
          next_col(&end);
          continue;
        case 'l':
          debug("found l!");
          if (end.idx + 2 < lexer->len &&
              tolower(lexer->text[end.idx + 1]) == 'l') {
            ty = LEX_TOKEN_TYPE_SIGNED_LONG_LONG_LITERAL;
          } else {
            ty = LEX_TOKEN_TYPE_SIGNED_LONG_LITERAL;
          }
          next_col(&end);
          continue;
        default:
          break;
        }

        break;
      }

      if (is_unsigned) {
        ty++;
      }
    } else if (valid_first_identifier_char(c)) {
      ty = LEX_TOKEN_TYPE_IDENTIFIER;

      for (size_t i = end.idx;
           i < lexer->len && valid_identifier_char(lexer->text[i]); i++) {
        next_col(&end);
      }

      // slightly hacky solution - retroactively determine if identifier
      // is a keyword
      ty = refine_ty(lexer, start, end);
    } else {
      unreachable("lexer hit an unknown token!");
    }
  }
  }

  token->ty = ty;
  token->span.start = start;
  token->span.end = end;

  debug("parse token %s\n", token_name(lexer, token));
}

int text_pos_len(struct text_pos start, struct text_pos end) {
  return end.idx - start.idx;
}

int text_span_len(const struct text_span *span) {
  return text_pos_len(span->start, span->end);
}

void next_col(struct text_pos *pos) {
  pos->idx++;
  pos->col++;
}

void next_line(struct text_pos *pos) {
  pos->idx++;
  pos->line++;
  pos->col = 0;
}

const char *associated_text(struct lexer *lexer, const struct token *token) {
  switch (token->ty) {
  case LEX_TOKEN_TYPE_IDENTIFIER:
  case LEX_TOKEN_TYPE_ASCII_CHAR_LITERAL:
  case LEX_TOKEN_TYPE_SIGNED_INT_LITERAL:
  case LEX_TOKEN_TYPE_UNSIGNED_INT_LITERAL:
  case LEX_TOKEN_TYPE_SIGNED_LONG_LITERAL:
  case LEX_TOKEN_TYPE_UNSIGNED_LONG_LITERAL:
  case LEX_TOKEN_TYPE_SIGNED_LONG_LONG_LITERAL:
  case LEX_TOKEN_TYPE_UNSIGNED_LONG_LONG_LITERAL: {
    size_t len = text_span_len(&token->span);
    char *p = arena_alloc(lexer->arena, len + 1);
    memcpy(p, &lexer->text[token->span.start.idx], len);
    p[len] = '\0';
    return p;
  }
  default:
    return NULL;
  }
}

#define CASE_RET(name)                                                         \
  case name:                                                                   \
    return #name;

const char *token_name(struct lexer *lexer, struct token *token) {
  UNUSED_ARG(lexer);

  switch (token->ty) {
    CASE_RET(LEX_TOKEN_TYPE_UNKNOWN)
    CASE_RET(LEX_TOKEN_TYPE_EOF)

    CASE_RET(LEX_TOKEN_TYPE_WHITESPACE)
    CASE_RET(LEX_TOKEN_TYPE_INLINE_COMMENT)
    CASE_RET(LEX_TOKEN_TYPE_MULTILINE_COMMENT)

    CASE_RET(LEX_TOKEN_TYPE_OP_INC)
    CASE_RET(LEX_TOKEN_TYPE_OP_DEC)

    CASE_RET(LEX_TOKEN_TYPE_OP_ASSG)

    CASE_RET(LEX_TOKEN_TYPE_OP_ADD_ASSG)
    CASE_RET(LEX_TOKEN_TYPE_OP_SUB_ASSG)
    CASE_RET(LEX_TOKEN_TYPE_OP_MUL_ASSG)
    CASE_RET(LEX_TOKEN_TYPE_OP_DIV_ASSG)
    CASE_RET(LEX_TOKEN_TYPE_OP_QUOT_ASSG)

    CASE_RET(LEX_TOKEN_TYPE_OP_ADD)
    CASE_RET(LEX_TOKEN_TYPE_OP_SUB)
    CASE_RET(LEX_TOKEN_TYPE_OP_MUL)
    CASE_RET(LEX_TOKEN_TYPE_OP_DIV)
    CASE_RET(LEX_TOKEN_TYPE_OP_QUOT)

    CASE_RET(LEX_TOKEN_TYPE_SEMICOLON)
    CASE_RET(LEX_TOKEN_TYPE_COMMA)

    CASE_RET(LEX_TOKEN_TYPE_KW_DO)
    CASE_RET(LEX_TOKEN_TYPE_KW_FOR)
    CASE_RET(LEX_TOKEN_TYPE_KW_WHILE)
    CASE_RET(LEX_TOKEN_TYPE_KW_IF)
    CASE_RET(LEX_TOKEN_TYPE_KW_ELSE)
    CASE_RET(LEX_TOKEN_TYPE_KW_RETURN)

    CASE_RET(LEX_TOKEN_TYPE_KW_CHAR)
    CASE_RET(LEX_TOKEN_TYPE_KW_SHORT)
    CASE_RET(LEX_TOKEN_TYPE_KW_INT)
    CASE_RET(LEX_TOKEN_TYPE_KW_LONG)
    CASE_RET(LEX_TOKEN_TYPE_KW_SIGNED)
    CASE_RET(LEX_TOKEN_TYPE_KW_UNSIGNED)

    CASE_RET(LEX_TOKEN_TYPE_OPEN_BRACKET)
    CASE_RET(LEX_TOKEN_TYPE_CLOSE_BRACKET)
    CASE_RET(LEX_TOKEN_TYPE_OPEN_BRACE)
    CASE_RET(LEX_TOKEN_TYPE_CLOSE_BRACE)
    CASE_RET(LEX_TOKEN_TYPE_IDENTIFIER)

    CASE_RET(LEX_TOKEN_TYPE_ASCII_CHAR_LITERAL)

    CASE_RET(LEX_TOKEN_TYPE_SIGNED_INT_LITERAL)
    CASE_RET(LEX_TOKEN_TYPE_UNSIGNED_INT_LITERAL)

    CASE_RET(LEX_TOKEN_TYPE_SIGNED_LONG_LITERAL)
    CASE_RET(LEX_TOKEN_TYPE_UNSIGNED_LONG_LITERAL)

    CASE_RET(LEX_TOKEN_TYPE_SIGNED_LONG_LONG_LITERAL)
    CASE_RET(LEX_TOKEN_TYPE_UNSIGNED_LONG_LONG_LITERAL)
  }
}
