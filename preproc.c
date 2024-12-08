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
  const char *text;
  size_t len;
};

struct preproc {
  struct arena_allocator *arena;

  // struct vector *texts;

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

static bool try_consume(struct preproc *preproc, struct text_pos *pos, char c) {
  debug_assert(
      preproc->pos.idx != pos->idx,
      "calling `try_consume` with `pos` the same as preproc makes no sense");

  debug_assert(c != '\n', "can't use on newlines");

  if (pos->idx < preproc->len && preproc->text[pos->idx] == c) {
    next_col(pos);

    return true;
  }

  return false;
}

static bool try_consume2(struct preproc *preproc, struct text_pos *pos, char c0, char c1) {
  debug_assert(
      preproc->pos.idx != pos->idx,
      "calling `try_consume` with `pos` the same as preproc makes no sense");

  debug_assert(c0 != '\n' && c1 != '\n', "can't use on newlines");

  if (pos->idx + 1 < preproc->len && preproc->text[pos->idx] == c0 && preproc->text[pos->idx + 1] == c1) {
    next_col(pos);
    next_col(pos);

    return true;
  }

  return false;
}

static void preproc_next_raw_token(struct preproc *preproc, struct preproc_token *token) {
  struct text_pos start = preproc->pos;
  struct text_pos end = start;

  if (start.idx >= preproc->len) {
    token->ty = PREPROC_TOKEN_TY_EOF;
    token->span.start = start;
    token->span.end = end;
    return;
  }

  while (preproc->pos.idx + 1 < preproc->len &&
         preproc->text[preproc->pos.idx] == '\\' &&
         is_newline(preproc->text[preproc->pos.idx + 1])) {
    // literally just skip this, don't even generate a token
    next_col(&end);
    next_line(&end);

    preproc->pos = end;

    start = preproc->pos;
    end = start;
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

    if (try_consume2(preproc, &end, '.', '.')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_ELLIPSIS;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_DOT;
    }
    break;

  case '>':
    next_col(&end);
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_GTEQ;
    } else if (try_consume(preproc, &end, '>')) {
      if (try_consume(preproc, &end, '=')) {
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
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LTEQ;
    } else if (try_consume(preproc, &end, '<')) {
      if (try_consume(preproc, &end, '=')) {
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
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_NEQ;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_NOT;
    }
    break;
  case '=':
    next_col(&end);
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_EQ;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ASSG;
    }
    break;
  case '&':
    next_col(&end);
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND_ASSG;
    } else if (try_consume(preproc, &end, '&')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_AND;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND;
    }
    break;
  case '|':
    next_col(&end);
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR_ASSG;
    } else if (try_consume(preproc, &end, '|')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_OR;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR;
    }
    break;
  case '^':
    next_col(&end);
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR;
    }
    break;
  case '+':
    next_col(&end);
    if (try_consume(preproc, &end, '+')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_INC;
    } else if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD;
    }
    break;
  case '-':
    next_col(&end);
    if (try_consume(preproc, &end, '-')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DEC;
    } else if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB_ASSG;
    } else if (try_consume(preproc, &end, '>')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_ARROW;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB;
    }
    break;
  case '*':
    next_col(&end);
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL;
    }
    break;
  case '/':
    next_col(&end);
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV;
    }

    break;
  case '%':
    next_col(&end);
    if (try_consume(preproc, &end, '=')) {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT;
    }
    break;

  default:
    goto not_punctuator;
  }

  *token = (struct preproc_token){
    .ty = PREPROC_TOKEN_TY_PUNCTUATOR,
    .span = {.start = start, .end = end},
    .punctuator = {
      .ty = punc_ty
    }
  };

  preproc->pos = end;
  return;

  not_punctuator:

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

static struct vector *preproc_tokens_til_eol(struct preproc *preproc,
                                             struct vector *buffer) {
  struct vector *tokens = vector_create(sizeof(struct preproc_token));

  struct preproc_token token;
  while (true) {
    preproc_next_token(preproc, &token);

    if (token.ty == PREPROC_TOKEN_TY_NEWLINE) {
      break;
    }

    vector_push_back(tokens, &token);
  }

  vector_push_back(buffer, &token);

  return tokens;
}

static bool token_streq(struct preproc *preproc, struct preproc_token token,
                        const char *str) {
  size_t token_len = token.span.end.idx - token.span.start.idx;
  size_t len = MIN(token_len, strlen(str));

  return strncmp(&preproc->text[token.span.start.idx], str, len) == 0;
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

// void preproc_process(struct preproc *preproc, FILE *file) {

void preproc_next_token(struct preproc *preproc, struct preproc_token *token) {
  // expands tokens, adds defines, etc

  struct hashtbl *defines = hashtbl_create(
      sizeof(struct preproc_identifier), sizeof(struct preproc_define),
      hash_preproc_ident, preproc_ident_eq);

  // now we do tokenization

  struct vector *buffer_tokens = vector_create(sizeof(struct preproc_token));

  size_t cur_line = preproc->pos.line;

  while (true) {
    if (vector_empty(buffer_tokens)) {
      preproc_next_token(preproc, &token);
    } else {
      token = *(struct preproc_token *)vector_pop(buffer_tokens);
    }

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
      unreachable();
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

        bool is_angle = preproc->text[filename_token.span.start.idx] == '<';

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
                 token_streq(preproc, directive, "define")) {

        struct preproc_token def_name;
        preproc_next_non_whitespace_token(preproc, &def_name);

        struct vector *tokens = preproc_tokens_til_eol(preproc, buffer_tokens);

        struct preproc_identifier ident = {
            .value = &preproc->text[def_name.span.start.idx],
            .length = text_span_len(&def_name.span)};

        struct preproc_define define = {.name = def_name, .value = tokens};

        hashtbl_insert(defines, &ident, &define);
      } else if (directive.ty == PREPROC_TOKEN_TY_IDENTIFIER &&
                 token_streq(preproc, directive, "undef")) {
        struct preproc_token def_name;
        preproc_next_non_whitespace_token(preproc, &def_name);

        struct preproc_identifier ident = {
            .value = &preproc->text[def_name.span.start.idx],
            .length = text_span_len(&def_name.span)};

        hashtbl_remove(defines, &ident);

      } else {
        todo("other directives");
      }
      break;
    }
    case PREPROC_TOKEN_TY_WHITESPACE:
      // keep leading whitespace
      if (new_line) {
        goto copy;
      } else {
        goto space;
      }
    case PREPROC_TOKEN_TY_IDENTIFIER: {
      // if identifier is a macro do something else
      struct preproc_identifier ident = {
          .value = &preproc->text[token.span.start.idx],
          .length = text_span_len(&token.span)};

      struct preproc_define *define = hashtbl_lookup(defines, &ident);

      if (define) {
        for (size_t i = vector_length(define->value); i; i--) {
          struct preproc_token *def_tok = vector_get(define->value, i - 1);
          vector_push_back(buffer_tokens, def_tok);
        }

        break;
      }

      goto copy;
    }
    case PREPROC_TOKEN_TY_PREPROC_NUMBER:
    case PREPROC_TOKEN_TY_STRING_LITERAL:
    case PREPROC_TOKEN_TY_PUNCTUATOR:
    case PREPROC_TOKEN_TY_OTHER:
    copy:
      fprintf(file, "%.*s", (int)text_span_len(&token.span),
              &preproc->text[token.span.start.idx]);
      break;
    case PREPROC_TOKEN_TY_NEWLINE: {
      fprintf(file, "\n");
      break;
    }
    case PREPROC_TOKEN_TY_COMMENT:
    space : {
      // comments are replaced with a single space
      fprintf(file, " ");
      break;
    }
    }
  }
}
