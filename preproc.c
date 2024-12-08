#include "preproc.h"

#include "alloc.h"
#include "hash.h"
#include "hashtbl.h"
#include "io.h"
#include "lex.h"
#include "liveness.h"
#include "log.h"
#include "program.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>
#include <stddef.h>
#include <string.h>

struct preproc_text {
  struct text_pos pos;

  const char *text;
  size_t len;
};

struct preproc {
  struct arena_allocator *arena;

  struct vector *texts;
  struct vector *buffer_tokens;

  struct hashtbl *defines;

  size_t num_include_paths;
  const char **include_paths;

  // if the current line has seen a token that is not whitespace
  bool line_has_nontrivial_token;

  // whether we are in an include/embed that accepts <foo> style strings
  bool in_angle_string_context;

  const char **associated_texts;
};

UNUSED static const char *preproc_token_name(enum preproc_token_ty ty) {
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

struct preproc_define {
  struct preproc_token name;
  struct vector *value;
};

struct preproc_identifier {
  const char *value;
  size_t length;
};

static void hash_preproc_ident(struct hasher *hasher, const void *value) {
  const struct preproc_identifier *ident = value;

  hasher_hash_bytes(hasher, ident->value, ident->length);
}

static bool preproc_ident_eq(const void *l, const void *r) {
  const struct preproc_identifier *l_ident = l;
  const struct preproc_identifier *r_ident = r;

  if (l_ident->length != r_ident->length) {
    return false;
  }

  return memcmp(l_ident->value, r_ident->value, l_ident->length) == 0;
}

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

  p->texts = vector_create(sizeof(struct preproc_text));

  struct preproc_text text = {.text = program->text,
                              .len = strlen(program->text),
                              .pos = {.col = 0, .line = 0, .idx = 0}};

  vector_push_back(p->texts, &text);

  p->line_has_nontrivial_token = false;
  p->in_angle_string_context = false;

  p->defines = hashtbl_create(sizeof(struct preproc_identifier),
                              sizeof(struct preproc_define), hash_preproc_ident,
                              preproc_ident_eq);

  // tokens that have appeared (e.g from a macro) and need to be processed next
  p->buffer_tokens = vector_create(sizeof(struct preproc_token));

  *preproc = p;

  return PREPROC_CREATE_RESULT_SUCCESS;
}

void preproc_free(struct preproc **preproc) {
  arena_allocator_free(&(*preproc)->arena);
  (*preproc)->arena = NULL;
  free(*preproc);

  *preproc = NULL;
}

static void find_multiline_comment_end(struct preproc_text *preproc_text,
                                       struct text_pos *cur_pos) {
  while (/* token must be at least 2 chars */ cur_pos->idx + 1 <
         preproc_text->len) {
    if (preproc_text->text[cur_pos->idx] == '\n') {
      next_line(cur_pos);
    } else if (preproc_text->text[cur_pos->idx] == '*' &&
               preproc_text->text[cur_pos->idx + 1] == '/') {
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

static bool is_identifier_char(char c) {
  return isalpha(c) || isdigit(c) || c == '_';
}

static bool is_first_identifier_char(char c) {
  return is_identifier_char(c) && !isdigit(c);
}

static bool is_preproc_number_char(char c) {
  // legal chars are letters, digits, underscores, periods, and exponents (not
  // handled here)
  return isalpha(c) || isdigit(c) || c == '_' || c == '.';
}

static bool try_consume(struct preproc_text *preproc_text, struct text_pos *pos,
                        char c) {
  debug_assert(
      preproc_text->pos.idx != pos->idx,
      "calling `try_consume` with `pos` the same as preproc makes no sense");

  debug_assert(c != '\n', "can't use on newlines");

  if (pos->idx < preproc_text->len && preproc_text->text[pos->idx] == c) {
    next_col(pos);

    return true;
  }

  return false;
}

static bool try_consume2(struct preproc_text *preproc_text,
                         struct text_pos *pos, char c0, char c1) {
  debug_assert(
      preproc_text->pos.idx != pos->idx,
      "calling `try_consume` with `pos` the same as preproc makes no sense");

  debug_assert(c0 != '\n' && c1 != '\n', "can't use on newlines");

  if (pos->idx + 1 < preproc_text->len && preproc_text->text[pos->idx] == c0 &&
      preproc_text->text[pos->idx + 1] == c1) {
    next_col(pos);
    next_col(pos);

    return true;
  }

  return false;
}

static void preproc_next_raw_token(struct preproc *preproc,
                                   struct preproc_token *token) {
  struct preproc_text *preproc_text;

  while (vector_length(preproc->texts)) {
    preproc_text = vector_tail(preproc->texts);

    if (preproc_text->pos.idx < preproc_text->len) {
      break;
    }

    vector_pop(preproc->texts);
  }

  if (vector_empty(preproc->texts)) {
    token->ty = PREPROC_TOKEN_TY_EOF;
    token->text = NULL;
    token->span.start = (struct text_pos){0};
    token->span.end = (struct text_pos){0};
  }

  struct text_pos start = preproc_text->pos;
  struct text_pos end = start;

  if (start.idx >= preproc_text->len) {
    token->ty = PREPROC_TOKEN_TY_EOF;
    token->text = NULL;
    token->span.start = start;
    token->span.end = end;
    return;
  }

  while (preproc_text->pos.idx + 1 < preproc_text->len &&
         preproc_text->text[preproc_text->pos.idx] == '\\' &&
         is_newline(preproc_text->text[preproc_text->pos.idx + 1])) {
    // literally just skip this, don't even generate a token
    next_col(&end);
    next_line(&end);

    preproc_text->pos = end;

    start = preproc_text->pos;
    end = start;
  }

  if (preproc_text->pos.idx < preproc_text->len &&
      is_newline(preproc_text->text[preproc_text->pos.idx])) {
    next_line(&end);

    preproc->line_has_nontrivial_token = false;
    preproc->in_angle_string_context = false;

    token->ty = PREPROC_TOKEN_TY_NEWLINE;
    token->text = &preproc_text->text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

  while (end.idx < preproc_text->len &&
         is_whitespace(preproc_text->text[end.idx])) {
    next_col(&end);
  }

  if (start.idx != end.idx) {
    // we have processed whitespace

    token->ty = PREPROC_TOKEN_TY_WHITESPACE;
    token->text = &preproc_text->text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

  // we will find a token here that is not whitespace
  // save old value as it is needed for determining if a directive is valid
  bool line_has_nontrivial_token = preproc->line_has_nontrivial_token;
  preproc->line_has_nontrivial_token = true;

  char c = preproc_text->text[end.idx];

  if (c == '/' && end.idx + 1 < preproc_text->len &&
      (preproc_text->text[end.idx + 1] == '/' ||
       preproc_text->text[end.idx + 1] == '*')) {
    // comment!
    char comment_ty = preproc_text->text[end.idx + 1];

    if (comment_ty == '/') {
      while (end.idx < preproc_text->len &&
             !is_newline(preproc_text->text[end.idx])) {
        next_col(&end);
      }
    } else {
      find_multiline_comment_end(preproc_text, &end);
    }

    token->ty = PREPROC_TOKEN_TY_COMMENT;
    token->text = &preproc_text->text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
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
         i < preproc_text->len &&
         !(!char_escaped && preproc_text->text[i] == end_char);
         i++) {
      // next char is escaped if this char is a non-escaped backslash
      char_escaped = !char_escaped && preproc_text->text[i] == '\\';
      next_col(&end);
    }

    // skip final single-quote
    next_col(&end);

    token->ty = PREPROC_TOKEN_TY_STRING_LITERAL;
    token->text = &preproc_text->text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  case '#':
    if (!line_has_nontrivial_token) {
      next_col(&end);
      token->ty = PREPROC_TOKEN_TY_DIRECTIVE;
      token->text = &preproc_text->text[start.idx];
      token->span = (struct text_span){.start = start, .end = end};

      preproc_text->pos = end;
      return;
    }
  }

  // we need to check for preproccessing number first as they can begin with `.`
  // and would be wrongly classed as punctuators
  if (isdigit(c) || (end.idx + 1 < preproc_text->len && c == '.' &&
                     isdigit(preproc_text->text[end.idx]))) {
    next_col(&end);

    while (end.idx < preproc_text->len) {
      char nc = preproc_text->text[end.idx];

      if (is_preproc_number_char(nc)) {
        next_col(&end);
      } else if (end.idx + 1 < preproc_text->len &&
                 (tolower(nc) == 'e' || tolower(nc) == 'p') &&
                 (preproc_text->text[end.idx + 1] == '+' ||
                  preproc_text->text[end.idx + 1] == '-')) {
        // need to check if it is an exponent
        next_col(&end);
        next_col(&end);
      } else {
        token->ty = PREPROC_TOKEN_TY_PREPROC_NUMBER;
        token->text = &preproc_text->text[start.idx];
        token->span = (struct text_span){.start = start, .end = end};

        preproc_text->pos = end;
        return;
      }
    }
  }

  // Look for punctuators

  enum preproc_token_punctuator_ty punc_ty;
  switch (c) {
  case '?':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_QMARK;
    next_col(&end);
    break;

  case '(':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET;
    next_col(&end);
    break;
  case ')':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET;
    next_col(&end);
    break;

  case '[':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_SQUARE_BRACKET;
    next_col(&end);
    break;
  case ']':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_SQUARE_BRACKET;
    next_col(&end);
    break;

  case '{':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACE;
    next_col(&end);
    break;
  case '}':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACE;
    next_col(&end);
    break;
  case ':':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_COLON;
    next_col(&end);
    break;
  case ';':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_SEMICOLON;
    next_col(&end);
    break;
  case ',':
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_COMMA;
    next_col(&end);
    break;
  case '.':
    next_col(&end);

    if (try_consume2(preproc_text, &end, '.', '.')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_ELLIPSIS;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_DOT;
    }
    break;

  case '>':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_GTEQ;
    } else if (try_consume(preproc_text, &end, '>')) {
      if (try_consume(preproc_text, &end, '=')) {
        punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_RSHIFT_ASSG;
      } else {
        punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_RSHIFT;
      }
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_GT;
    }
    break;
  case '<':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LTEQ;
    } else if (try_consume(preproc_text, &end, '<')) {
      if (try_consume(preproc_text, &end, '=')) {
        punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LSHIFT_ASSG;
      } else {
        punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LSHIFT;
      }
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LT;
    }
    break;
  case '~':
    next_col(&end);
    punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_NOT;
    break;
  case '!':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_NEQ;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_NOT;
    }
    break;
  case '=':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_EQ;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ASSG;
    }
    break;
  case '&':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND_ASSG;
    } else if (try_consume(preproc_text, &end, '&')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_AND;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND;
    }
    break;
  case '|':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR_ASSG;
    } else if (try_consume(preproc_text, &end, '|')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_OR;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR;
    }
    break;
  case '^':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR;
    }
    break;
  case '+':
    next_col(&end);
    if (try_consume(preproc_text, &end, '+')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_INC;
    } else if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD;
    }
    break;
  case '-':
    next_col(&end);
    if (try_consume(preproc_text, &end, '-')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DEC;
    } else if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB_ASSG;
    } else if (try_consume(preproc_text, &end, '>')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_ARROW;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB;
    }
    break;
  case '*':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL;
    }
    break;
  case '/':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV;
    }

    break;
  case '%':
    next_col(&end);
    if (try_consume(preproc_text, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT;
    }
    break;

  default:
    goto not_punctuator;
  }

  *token = (struct preproc_token){.ty = PREPROC_TOKEN_TY_PUNCTUATOR,
                                  .text = &preproc_text->text[start.idx],
                                  .span = {.start = start, .end = end},
                                  .punctuator = {.ty = punc_ty}};

  preproc_text->pos = end;
  return;

not_punctuator:

  if (is_first_identifier_char(c)) {
    while (end.idx < preproc_text->len &&
           is_identifier_char(preproc_text->text[end.idx])) {
      next_col(&end);
    }

    token->ty = PREPROC_TOKEN_TY_IDENTIFIER;
    token->text = &preproc_text->text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

  next_col(&end);
  token->ty = PREPROC_TOKEN_TY_OTHER;
  token->text = &preproc_text->text[start.idx];
  token->span = (struct text_span){.start = start, .end = end};

  preproc_text->pos = end;
}

static void preproc_next_non_whitespace_token(struct preproc *preproc,
                                              struct preproc_token *token) {
  do {
    preproc_next_raw_token(preproc, token);
  } while (token->ty == PREPROC_TOKEN_TY_WHITESPACE);
}

static struct vector *preproc_tokens_til_eol(struct preproc *preproc,
                                             struct vector *buffer) {
  struct vector *tokens = vector_create(sizeof(struct preproc_token));

  struct preproc_token token;
  while (true) {
    preproc_next_raw_token(preproc, &token);

    if (token.ty == PREPROC_TOKEN_TY_NEWLINE) {
      break;
    }

    vector_push_back(tokens, &token);
  }

  vector_push_back(buffer, &token);

  return tokens;
}

static bool token_streq(struct preproc_token token, const char *str) {
  size_t token_len = token.span.end.idx - token.span.start.idx;
  size_t len = MIN(token_len, strlen(str));

  return strncmp(token.text, str, len) == 0;
}

void preproc_next_token(struct preproc *preproc, struct preproc_token *token) {
  // expands tokens, adds defines, etc

  while (true) {
    if (vector_empty(preproc->buffer_tokens)) {
      preproc_next_raw_token(preproc, token);
    } else {
      *token = *(struct preproc_token *)vector_pop(preproc->buffer_tokens);
    }

    switch (token->ty) {
    case PREPROC_TOKEN_TY_UNKNOWN:
      unreachable();
    case PREPROC_TOKEN_TY_DIRECTIVE: {
      struct preproc_token directive;
      preproc_next_token(preproc, &directive);

      if (directive.ty == PREPROC_TOKEN_TY_IDENTIFIER &&
          token_streq(directive, "include")) {
        preproc->in_angle_string_context = true;

        struct preproc_token filename_token;
        preproc_next_non_whitespace_token(preproc, &filename_token);

        // remove quotes
        size_t filename_len = text_span_len(&filename_token.span);

        debug_assert(filename_len >= 2, "filename token can't be <2 chars");

        char *filename = arena_alloc(preproc->arena, filename_len + 1);
        filename[filename_len] = 0;

        strncpy(filename, &filename_token.text[1], filename_len);

        bool is_angle = filename_token.text[0] == '<';

        const char *content = NULL;
        if (is_angle) {
          for (size_t i = 0; i < preproc->num_include_paths; i++) {
            const char *path =
                path_combine(preproc->include_paths[i], filename);
            content = read_file(path);
            if (content) {
              break;
            }
          }
        } else {
          content = read_file(filename);
        }
        todo("include");
        // break;
      } else if (directive.ty == PREPROC_TOKEN_TY_IDENTIFIER &&
                 token_streq(directive, "define")) {

        struct preproc_token def_name;
        preproc_next_non_whitespace_token(preproc, &def_name);

        struct vector *tokens =
            preproc_tokens_til_eol(preproc, preproc->buffer_tokens);

        struct preproc_identifier ident = {
            .value = def_name.text, .length = text_span_len(&def_name.span)};

        struct preproc_define define = {.name = def_name, .value = tokens};

        hashtbl_insert(preproc->defines, &ident, &define);
      } else if (directive.ty == PREPROC_TOKEN_TY_IDENTIFIER &&
                 token_streq(directive, "undef")) {

        struct preproc_token def_name;
        preproc_next_non_whitespace_token(preproc, &def_name);

        struct preproc_identifier ident = {
            .value = def_name.text, .length = text_span_len(&def_name.span)};

        hashtbl_remove(preproc->defines, &ident);
      } else {
        todo("other directives");
      }
      break;
    }
    case PREPROC_TOKEN_TY_IDENTIFIER: {
      // if identifier is a macro do something else
      struct preproc_identifier ident = {.value = token->text,
                                         .length = text_span_len(&token->span)};

      struct preproc_define *define = hashtbl_lookup(preproc->defines, &ident);

      if (define) {
        for (size_t i = vector_length(define->value); i; i--) {
          struct preproc_token *def_tok = vector_get(define->value, i - 1);
          vector_push_back(preproc->buffer_tokens, def_tok);
        }

        break;
      } else {
        return;
      }
    }
    case PREPROC_TOKEN_TY_WHITESPACE:
    case PREPROC_TOKEN_TY_PREPROC_NUMBER:
    case PREPROC_TOKEN_TY_STRING_LITERAL:
    case PREPROC_TOKEN_TY_PUNCTUATOR:
    case PREPROC_TOKEN_TY_NEWLINE:
    case PREPROC_TOKEN_TY_OTHER:
    case PREPROC_TOKEN_TY_COMMENT:
    case PREPROC_TOKEN_TY_EOF:
      return;
    }
  }
}

void preproc_process(struct preproc *preproc, FILE *file) {
  struct preproc_token token;

  bool last_was_newline = false;

  do {
    preproc_next_token(preproc, &token);

    switch (token.ty) {
    case PREPROC_TOKEN_TY_UNKNOWN:
      fprintf(file, "?UNKNOWN?");
      break;
    case PREPROC_TOKEN_TY_EOF:
      break;
    case PREPROC_TOKEN_TY_DIRECTIVE:
      bug("directive in process");
    case PREPROC_TOKEN_TY_IDENTIFIER:
    case PREPROC_TOKEN_TY_PREPROC_NUMBER:
    case PREPROC_TOKEN_TY_STRING_LITERAL:
    case PREPROC_TOKEN_TY_PUNCTUATOR:
    case PREPROC_TOKEN_TY_OTHER:
      fprintf(file, "%.*s", (int)text_span_len(&token.span), token.text);
      break;
    case PREPROC_TOKEN_TY_WHITESPACE:
      if (last_was_newline) {
        fprintf(file, "%.*s", (int)text_span_len(&token.span), token.text);
      } else {
        fprintf(file, " ");
      }
      break;
    case PREPROC_TOKEN_TY_NEWLINE:
      if (!last_was_newline) {
        fprintf(file, "\n");
      }
      break;
    case PREPROC_TOKEN_TY_COMMENT:
      fprintf(file, " ");
      break;
    }

    last_was_newline = token.ty == PREPROC_TOKEN_TY_NEWLINE;
  } while (token.ty != PREPROC_TOKEN_TY_EOF);
}
