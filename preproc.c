#include "preproc.h"

#include "alloc.h"
#include "liveness.h"
#include "log.h"
#include "program.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>
#include <stddef.h>
#include <string.h>

struct preproc {
  struct arena_allocator *arena;
  const char *text;
  size_t len;

  size_t num_include_paths;
  const char **include_paths;

  char *processed;
  size_t processed_head;
  size_t processed_len;

  // if the current line has seen a token that is not whitespace
  bool line_has_nontrivial_token;

  // whether we are in an include/embed that accepts <foo> style strings
  bool in_angle_string_context;

  struct text_pos pos;

  const char **associated_texts;
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

static const char *preproc_token_name(enum preproc_token_ty ty) {
#define CASE_RET(name)                                                         \
  case name:                                                                   \
    return #name;

  switch (ty) {
    CASE_RET(PREPROC_TOKEN_TY_UNKNOWN)
    CASE_RET(PREPROC_TOKEN_TY_EOF)
    CASE_RET(PREPROC_TOKEN_TY_DIRECTIVE)
    CASE_RET(PREPROC_TOKEN_TY_IDENTIFIER)
    CASE_RET(PREPROC_TOKEN_TY_PREPROC_NUMBER)
    CASE_RET(PREPROC_TOKEN_TY_STRING_LITERAL)
    CASE_RET(PREPROC_TOKEN_TY_PUNCTUATOR)
    CASE_RET(PREPROC_TOKEN_TY_NEWLINE)
    CASE_RET(PREPROC_TOKEN_TY_WHITESPACE)
    CASE_RET(PREPROC_TOKEN_TY_COMMENT)
    CASE_RET(PREPROC_TOKEN_TY_OTHER)
  }

#undef CASE_RET
}

struct preproc_token {
  enum preproc_token_ty ty;

  struct text_span span;
};

enum preproc_create_result preproc_create(struct program *program,
                                          size_t num_include_paths,
                                          const char **include_paths,
                                          struct preproc **preproc) {
  info("beginning lex stage");

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct preproc *p = nonnull_malloc(sizeof(*p));
  p->arena = arena;
  p->num_include_paths = num_include_paths;
  p->include_paths = include_paths;
  // copy out the program so lifetimes aren't tied
  p->text = arena_alloc_strcpy(arena, program->text);
  p->len = strlen(program->text);
  p->pos.idx = 0;
  p->pos.line = 0;
  p->pos.col = 0;

  // use "length of unproccessde program" as starting size
  size_t initial_processed_size = p->len;
  p->processed =
      arena_alloc(arena, sizeof(*p->processed) * initial_processed_size);
  p->processed_len = initial_processed_size;
  p->processed_head = 0;
  p->line_has_nontrivial_token = false;
  p->in_angle_string_context = false;

  *preproc = p;

  return PREPROC_CREATE_RESULT_SUCCESS;
}

void preproc_free(struct preproc **preproc) {
  arena_allocator_free(&(*preproc)->arena);
  (*preproc)->arena = NULL;
  free(*preproc);

  *preproc = NULL;
}

static void find_multiline_comment_end(struct preproc *preproc,
                                struct text_pos *cur_pos) {
  while (/* token must be at least 2 chars */ cur_pos->idx + 1 < preproc->len) {
    if (preproc->text[cur_pos->idx] == '\n') {
      next_line(cur_pos);
    } else if (preproc->text[cur_pos->idx] == '*' &&
               preproc->text[cur_pos->idx + 1] == '/') {
      // found it!
      next_col(cur_pos);
      next_col(cur_pos);
      return;
    } else {
      next_col(cur_pos);
    }
  }

  // if not found, it will just push to end of file and next token will be EOF
}

static bool is_newline(char c) { return c == '\n'; }

static bool is_whitespace(char c) { return isspace(c) && !is_newline(c); }

static bool is_punctuator(char c) {
  return ispunct(c) && c != '$' && c != '@' && c != '`';
}

static bool is_identifier_char(char c) { return isalpha(c) || isdigit(c) || c == '_'; }

static bool is_first_identifier_char(char c) {
  return is_identifier_char(c) && !isdigit(c);
}

static bool is_preproc_number_char(char c) {
  // legal chars are letters, digits, underscores, periods, and exponents (not
  // handled here)
  return isalpha(c) || isdigit(c) || c == '_' || c == '.';
}

static void preproc_next_token(struct preproc *preproc, struct preproc_token *token) {
  struct text_pos start = preproc->pos;
  struct text_pos end = start;

  if (start.idx >= preproc->len) {
    token->ty = PREPROC_TOKEN_TY_EOF;
    token->span.start = start;
    token->span.end = end;
    return;
  }

  if (preproc->pos.idx < preproc->len &&
      is_newline(preproc->text[preproc->pos.idx])) {
    next_line(&end);
    preproc->line_has_nontrivial_token = false;
    preproc->in_angle_string_context = false;

    token->ty = PREPROC_TOKEN_TY_NEWLINE;
    token->span = (struct text_span){.start = start, .end = end};

    preproc->pos = end;
    return;
  }

  while (end.idx < preproc->len && is_whitespace(preproc->text[end.idx])) {
    next_col(&end);
  }

  if (start.idx != end.idx) {
    // we have processed whitespace

    token->ty = PREPROC_TOKEN_TY_WHITESPACE;
    token->span = (struct text_span){.start = start, .end = end};

    preproc->pos = end;
    return;
  }

  // we will find a token here that is not whitespace
  // save old value as it is needed for determining if a directive is valid
  bool line_has_nontrivial_token = preproc->line_has_nontrivial_token;
  preproc->line_has_nontrivial_token = true;

  char c = preproc->text[end.idx];

  if (c == '/' && end.idx + 1 < preproc->len &&
      (preproc->text[end.idx + 1] == '/' ||
       preproc->text[end.idx + 1] == '*')) {
    // comment!
    char comment_ty = preproc->text[end.idx + 1];

    if (comment_ty == '/') {
      while (end.idx < preproc->len && !is_newline(preproc->text[end.idx])) {
        next_col(&end);
      }
    } else {
      find_multiline_comment_end(preproc, &end);
    }

    token->ty = PREPROC_TOKEN_TY_COMMENT;
    token->span = (struct text_span){.start = start, .end = end};

    preproc->pos = end;
    return;
  }

  switch (c) {
  case '<':
  case '"':
  case '\'':
    if (c == '<' && !preproc->in_angle_string_context) {
      break;
    }

    // string/char literal
    // skip first single-quote
    next_col(&end);

    char end_char = c == '<' ? '>' : c;

    // move forward while
    bool char_escaped = false;
    for (size_t i = end.idx;
         i < preproc->len && !(!char_escaped && preproc->text[i] == end_char);
         i++) {
      // next char is escaped if this char is a non-escaped backslash
      char_escaped = !char_escaped && preproc->text[i] == '\\';
      next_col(&end);
    }

    // skip final single-quote
    next_col(&end);

    token->ty = PREPROC_TOKEN_TY_STRING_LITERAL;
    token->span = (struct text_span){.start = start, .end = end};

    preproc->pos = end;
    return;
  case '#':
    if (!line_has_nontrivial_token) {
      next_col(&end);
      token->ty = PREPROC_TOKEN_TY_DIRECTIVE;
      token->span = (struct text_span){.start = start, .end = end};

      preproc->pos = end;
      return;
    }
  }

  // we need to check for preproccessing number first as they can begin with `.`
  // and would be wrongly classed as punctuators
  if (isdigit(c) || (end.idx + 1 < preproc->len && c == '.' &&
                     isdigit(preproc->text[end.idx]))) {
    next_col(&end);

    while (end.idx < preproc->len) {
      char nc = preproc->text[end.idx];

      if (is_preproc_number_char(nc)) {
        next_col(&end);
      } else if (end.idx + 1 < preproc->len &&
                 (tolower(nc) == 'e' || tolower(nc) == 'p') &&
                 (preproc->text[end.idx + 1] == '+' ||
                  preproc->text[end.idx + 1] == '-')) {
        // need to check if it is an exponent
        next_col(&end);
        next_col(&end);
      } else {
        token->ty = PREPROC_TOKEN_TY_PREPROC_NUMBER;
        token->span = (struct text_span){.start = start, .end = end};

        preproc->pos = end;
        return;
      }
    }
  }

  if (is_punctuator(c)) {
    next_col(&end);

    token->ty = PREPROC_TOKEN_TY_PUNCTUATOR;
    token->span = (struct text_span){.start = start, .end = end};

    preproc->pos = end;
    return;
  }

  if (is_first_identifier_char(c)) {
    while (end.idx < preproc->len &&
           is_identifier_char(preproc->text[end.idx])) {
      next_col(&end);
    }

    token->ty = PREPROC_TOKEN_TY_IDENTIFIER;
    token->span = (struct text_span){.start = start, .end = end};

    preproc->pos = end;
    return;
  }

  next_col(&end);
  token->ty = PREPROC_TOKEN_TY_OTHER;
  token->span = (struct text_span){.start = start, .end = end};

  preproc->pos = end;
}

static void preproc_next_non_whitespace_token(struct preproc *preproc,
                                       struct preproc_token *token) {
  do {
    preproc_next_token(preproc, token);
  } while (token->ty == PREPROC_TOKEN_TY_WHITESPACE);
}

static bool token_streq(struct preproc *preproc, struct preproc_token token,
                 const char *str) {
  size_t token_len = token.span.end.idx - token.span.start.idx;
  size_t len = MIN(token_len, strlen(str));

  return strncmp(&preproc->text[token.span.start.idx], str, len) == 0;
}

struct preprocessed_program preproc_process(struct preproc *preproc) {
  const char *original_text = preproc->text;
  size_t original_len = preproc->len;

  // as line continuations can only ever shrink the file, this is okay
  char *new_text = arena_alloc_strcpy(preproc->arena, original_text);

  size_t head = 0;

  // first step: remove all line continuations
  for (size_t i = 0; i < original_len; i++) {
    if (original_text[i] != '\\') {
      new_text[head++] = original_text[i];
      continue;
    }

    size_t end = i + 1;
    while (end < original_len && is_whitespace(original_text[end])) {
      end++;
    }

    if (end < original_len && is_newline(original_text[end])) {
      // this was a line continuation
      i = end;
    } else {
      new_text[head++] = original_text[i];
    }
  }

  preproc->text = new_text;
  preproc->len = head;

  struct vector *preprocessed = vector_create(sizeof(char));

  // now we do tokenization

  size_t cur_line = preproc->pos.line;
  struct preproc_token token = {.ty = PREPROC_TOKEN_TY_UNKNOWN};
  while (token.ty != PREPROC_TOKEN_TY_EOF) {
    preproc_next_token(preproc, &token);
    trace("found preproc token %s from %zu:%zu (%zu) to %zu:%zu (%zu)\n",
          preproc_token_name(token.ty), token.span.start.line,
          token.span.start.col, token.span.start.idx, token.span.end.line,
          token.span.end.col, token.span.end.idx);

    // this is sort of redundant, as we do this "is it first token on line"
    // check in the tokenize for directives
    bool new_line = preproc->pos.line != cur_line;
    cur_line = preproc->pos.line;

    switch (token.ty) {
    case PREPROC_TOKEN_TY_UNKNOWN:
      unreachable("unknown token (should never happen)");
    case PREPROC_TOKEN_TY_EOF:
      break;
    case PREPROC_TOKEN_TY_DIRECTIVE: {
      struct preproc_token directive;
      preproc_next_token(preproc, &directive);

      if (directive.ty == PREPROC_TOKEN_TY_IDENTIFIER &&
          token_streq(preproc, directive, "include")) {
        preproc->in_angle_string_context = true;

        struct preproc_token filename_token;
        preproc_next_non_whitespace_token(preproc, &filename_token);

        // remove quotes
        debug_assert(
            filename_token.span.end.idx - filename_token.span.start.idx >= 2,
            "filename token can't be <2 chars");
        size_t filename_len =
            filename_token.span.end.idx - filename_token.span.start.idx - 2;
        char *filename = arena_alloc(preproc->arena, filename_len + 1);
        filename[filename_len] = 0;
        strncpy(filename, &preproc->text[filename_token.span.start.idx + 1],
                filename_len);

        todo("actually include file");
        // break;
      }

      todo("other directives");
    }
    case PREPROC_TOKEN_TY_WHITESPACE:
      // keep leading whitespace
      if (new_line) {
        goto copy;
      } else {
        goto space;
      }
    case PREPROC_TOKEN_TY_IDENTIFIER:
      // if identifier is a macro do something else
      goto copy;
    case PREPROC_TOKEN_TY_PREPROC_NUMBER:
    case PREPROC_TOKEN_TY_STRING_LITERAL:
    case PREPROC_TOKEN_TY_PUNCTUATOR:
    case PREPROC_TOKEN_TY_OTHER:
    copy:
      for (size_t i = token.span.start.idx; i < token.span.end.idx; i++) {
        vector_push_back(preprocessed, &preproc->text[i]);
      }
      break;
    case PREPROC_TOKEN_TY_NEWLINE: {
      char nl = '\n';
      vector_push_back(preprocessed, &nl);
      break;
    }
    case PREPROC_TOKEN_TY_COMMENT:
    space: {
      // comments are replaced with a single space
      char space = ' ';
      vector_push_back(preprocessed, &space);
      break;
    }
    }
  }

  // FIXME: lifetimes are a mess here the vector is intentionally leaked because
  // the data is used by lexer
  char null = 0;
  vector_push_back(preprocessed, &null);

  return (struct preprocessed_program){.text = vector_head(preprocessed)};
}
