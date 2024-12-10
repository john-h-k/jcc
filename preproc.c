#include "preproc.h"

#include "alloc.h"
#include "hash.h"
#include "hashtbl.h"
#include "io.h"
#include "ir/ir.h"
#include "lex.h"
#include "log.h"
#include "program.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>
#include <stddef.h>
#include <string.h>
#include <time.h>

struct preproc_text {
  struct text_pos pos;

  const char *text;
  size_t len;

  // the values that can be set by the preprocessor
  size_t line;
  const char *file;

  struct path_components path;
};

struct preproc {
  struct arena_allocator *arena;

  struct vector *texts;
  struct vector *buffer_tokens;

  struct hashtbl *defines;

  struct vector *enabled;

  size_t num_include_paths;
  const char **include_paths;

  const char *fixed_timestamp;

  // if the current line has seen a token that is not whitespace
  bool line_has_nontrivial_token;

  // whether we are in an include/embed that accepts <foo> style strings
  bool in_angle_string_context;
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

enum preproc_define_value_ty {
  PREPROC_DEFINE_VALUE_TY_TOKEN,
  PREPROC_DEFINE_VALUE_TY_TOKEN_VEC,
};

struct preproc_define_value {
  enum preproc_define_value_ty ty;

  union {
    struct preproc_token token;
    struct vector *vec;
  };
};

struct preproc_define {
  struct preproc_token name;
  struct preproc_define_value value;
};

static void preproc_create_builtin_macros(struct preproc *preproc) {
  // FIXME: vectors leak, vector should probably be arena-based

#define DEF_BUILTIN(n, v)                                                      \
  do {                                                                         \
    size_t name_len = strlen((n));                                             \
    struct sized_str ident = {.str = (n), .len = name_len};                    \
                                                                               \
    struct preproc_define define = {                                           \
        .name = {.ty = PREPROC_TOKEN_TY_IDENTIFIER,                            \
                 .span = MK_INVALID_TEXT_SPAN(0, name_len),                    \
                 .text = (n)},                                                 \
        .value = {                                                             \
            .ty = PREPROC_DEFINE_VALUE_TY_TOKEN,                               \
            .token = {.ty = PREPROC_TOKEN_TY_IDENTIFIER,                       \
                      .span = MK_INVALID_TEXT_SPAN(0, strlen((v))),            \
                      .text = (v)},                                            \
        }};                                                                    \
                                                                               \
    hashtbl_insert(preproc->defines, &ident, &define);                         \
  } while (0);

  DEF_BUILTIN("__JCC__", "1");
  DEF_BUILTIN("__jcc__", "1");

  DEF_BUILTIN("__STDC__", "1");
  // TODO: support different version targets. This is C11
  DEF_BUILTIN("__STDC_VERSION__", "201112L");
  DEF_BUILTIN("__STDC_HOSTED__", "1");

  DEF_BUILTIN("__STDC_UTF_16__", "1");
  DEF_BUILTIN("__STDC_UTF_32__", "1");

  // C23 only
  DEF_BUILTIN("__STDC_EMBED_NOT_FOUND__", "0");
  DEF_BUILTIN("__STDC_EMBED_FOUND__", "1");
  DEF_BUILTIN("__STDC_EMBED_EMPTY__", "2");

  // C11
  DEF_BUILTIN("__STDC_NO_ATOMICS__", "1");
  DEF_BUILTIN("__STDC_NO_COMPLEX__", "1");
  DEF_BUILTIN("__STDC_NO_THREADS__", "1");
  DEF_BUILTIN("__STDC_NO_VLA__", "1");

  // magic macros such as __TIME__ are handled in the processor

#undef DEF_BUILTIN
}

static struct preproc_text create_preproc_text(const char *text,
                                               const char *path) {
  struct path_components components =
      path ? path_components(path) : (struct path_components){NULL, NULL};

  // FIXME: spans are entirely broken at the moment
  return (struct preproc_text){
      .text = text,
      .len = strlen(text),
      .pos = {.col = 0, .line = 0, .idx = 0},
      .line = 0,
      .file = components.file,
      .path = components,
  };
}

enum preproc_special_macro {
  PREPROC_SPECIAL_MACRO_FILE,
  PREPROC_SPECIAL_MACRO_LINE,
  PREPROC_SPECIAL_MACRO_TIME,
  PREPROC_SPECIAL_MACRO_DATE,
};

static struct hashtbl *SPECIAL_MACROS = NULL;

enum preproc_create_result
preproc_create(struct program *program, const char *path,
               size_t num_include_paths, const char **include_paths,
               const char *fixed_timestamp, struct preproc **preproc) {
  if (fixed_timestamp) {
    debug_assert(strlen(fixed_timestamp) >= 19,
                 "`fixed_timestamp` must be at least 19");
  }

  if (!SPECIAL_MACROS) {
    debug("building special macro table");

    SPECIAL_MACROS =
        hashtbl_create_sized_str_keyed(sizeof(enum preproc_special_macro));

#define SPECIAL_MACRO(kw, ty)                                                  \
  do {                                                                         \
    struct sized_str k = {                                                     \
        .str = kw,                                                             \
        .len = strlen(kw),                                                     \
    };                                                                         \
    enum preproc_special_macro v = ty;                                         \
    hashtbl_insert(SPECIAL_MACROS, &k, &v);                                    \
  } while (0);

    SPECIAL_MACRO("__FILE__", PREPROC_SPECIAL_MACRO_FILE);
    SPECIAL_MACRO("__LINE__", PREPROC_SPECIAL_MACRO_LINE);
    SPECIAL_MACRO("__TIME__", PREPROC_SPECIAL_MACRO_TIME);
    SPECIAL_MACRO("__DATE__", PREPROC_SPECIAL_MACRO_DATE);
#undef SPECIAL_MACRO

    debug("built special macro table (len=%zu)", hashtbl_size(SPECIAL_MACROS));
  }

  info("beginning lex stage");

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct preproc *p = nonnull_malloc(sizeof(*p));
  p->arena = arena;
  p->num_include_paths = num_include_paths;
  p->include_paths = include_paths;

  p->texts = vector_create(sizeof(struct preproc_text));
  p->enabled = vector_create(sizeof(bool));
  p->fixed_timestamp = fixed_timestamp;

  bool enabled = true;
  vector_push_back(p->enabled, &enabled);

  struct preproc_text text = create_preproc_text(program->text, path);
  vector_push_back(p->texts, &text);

  p->line_has_nontrivial_token = false;
  p->in_angle_string_context = false;

  p->defines = hashtbl_create_sized_str_keyed(sizeof(struct preproc_define));

  // tokens that have appeared (e.g from a macro) and need to be processed next
  p->buffer_tokens = vector_create(sizeof(struct preproc_token));

  *preproc = p;

  preproc_create_builtin_macros(p);

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
    return;
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
  case 'L':
  case '<':
  case '"':
  case '\'':
    if (c == 'L') {
      if (end.idx < preproc_text->len) {
        c = preproc_text->text[end.idx + 1];
        next_col(&end);
      } else {
        break;
      }
    }

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
    next_col(&end);

    if (!line_has_nontrivial_token) {
      token->ty = PREPROC_TOKEN_TY_DIRECTIVE;
    } else if (try_consume(preproc_text, &end, '#')) {
      token->ty = PREPROC_TOKEN_TY_PUNCTUATOR;
      token->punctuator = (struct preproc_token_punctuator){
          .ty = PREPROC_TOKEN_PUNCTUATOR_TY_CONCAT};
    } else {
      token->ty = PREPROC_TOKEN_TY_PUNCTUATOR;
      token->punctuator = (struct preproc_token_punctuator){
          .ty = PREPROC_TOKEN_PUNCTUATOR_TY_STRINGIFY};
    }

    token->text = &preproc_text->text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
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

static bool token_streq(struct preproc_token token, const char *str) {
  size_t token_len = token.span.end.idx - token.span.start.idx;
  size_t len = strlen(str);

  return len == token_len && strncmp(token.text, str, len) == 0;
}
static bool try_expand_token(struct preproc *preproc,
                             struct preproc_text *preproc_text,
                             struct preproc_token *token, struct vector *buffer,
                             struct hashtbl *parents) {
  if (token->ty != PREPROC_TOKEN_TY_IDENTIFIER) {
    return false;
  }

  // if identifier is a macro do something else
  struct sized_str ident = {.str = token->text,
                            .len = text_span_len(&token->span)};

  if (!parents) {
    parents = hashtbl_create_sized_str_keyed(0);
  } else {
    void *parent = hashtbl_lookup(parents, &ident);

    if (parent) {
      // already seen this macro, do not expand it again
      return false;
    }
  }

  hashtbl_insert(parents, &ident, NULL);

  struct preproc_define *macro = hashtbl_lookup(preproc->defines, &ident);

  if (macro) {
    struct preproc_define_value *value = &macro->value;
    switch (value->ty) {
    case PREPROC_DEFINE_VALUE_TY_TOKEN:
      if (!try_expand_token(preproc, preproc_text, &value->token, buffer,
                            parents)) {
        vector_push_back(buffer, &value->token);
      }
      break;
    case PREPROC_DEFINE_VALUE_TY_TOKEN_VEC: {
      size_t num_tokens = vector_length(value->vec);
      for (size_t i = num_tokens; i; i--) {
        struct preproc_token *def_tok = vector_get(value->vec, i - 1);
        if (!try_expand_token(preproc, preproc_text, def_tok, buffer,
                              parents)) {
          vector_push_back(buffer, def_tok);
        }
      }
      break;
    }
    }

    return true;
  }

  enum preproc_special_macro *special_macro =
      hashtbl_lookup(SPECIAL_MACROS, &ident);

  if (special_macro) {
    // i null-terminate all the strings in here
    // not necessary because of the `span` field
    // but in other contexts it is safe to print the entire string, just
    // span is needed to print the right part null-terminating these keeps
    // that invariant

    switch (*special_macro) {
    case PREPROC_SPECIAL_MACRO_FILE: {
      const char *file =
          preproc_text->file ? preproc_text->file : "(null file)";
      size_t file_len = strlen(file) + 2;

      char *buff = arena_alloc(preproc->arena, sizeof(*buff) * (file_len + 1));
      buff[0] = '\"';
      strcpy(&buff[1], file);
      buff[file_len - 1] = '\"';
      buff[file_len] = '\0';

      // FIXME: this will not properly escape weird characters (e.g null) in
      // filename
      struct preproc_token special_tok = {.ty = PREPROC_TOKEN_TY_STRING_LITERAL,
                                          .span =
                                              MK_INVALID_TEXT_SPAN(0, file_len),
                                          .text = buff};
      vector_push_back(buffer, &special_tok);
      break;
    }
    case PREPROC_SPECIAL_MACRO_LINE: {
      size_t line = preproc_text->line + preproc_text->pos.line;
      // our index is from 0
      line++;
      size_t len = num_digits(line);
      char *buff = arena_alloc(preproc->arena, sizeof(*buff) * (len + 1));

      snprintf(buff, len + 1, "%zu", line);

      struct preproc_token special_tok = {.ty = PREPROC_TOKEN_TY_PREPROC_NUMBER,
                                          .span = MK_INVALID_TEXT_SPAN(0, len),
                                          .text = buff};
      vector_push_back(buffer, &special_tok);
      break;
    }
    case PREPROC_SPECIAL_MACRO_TIME: {
      const char *asc;
      if (preproc->fixed_timestamp) {
        asc = preproc->fixed_timestamp;
      } else {
        time_t epoch = time(NULL);
        struct tm *tm = localtime(&epoch);
        asc = asctime(tm);
      }

      // i think it is safe to modify the return of `asctime`, but i am
      // unsure so i will copy to prevent weird bugs
      size_t len = 8 + 2;
      char *buff = arena_alloc(preproc->arena, sizeof(*buff) * (len + 1));
      buff[0] = '\"';
      memcpy(&buff[1], &asc[11], sizeof(*buff) * len);
      buff[len - 1] = '\"';
      buff[len] = '\0';

      struct preproc_token special_tok = {.ty = PREPROC_TOKEN_TY_STRING_LITERAL,
                                          .span = MK_INVALID_TEXT_SPAN(0, len),
                                          .text = buff};
      vector_push_back(buffer, &special_tok);
      break;
    }
    case PREPROC_SPECIAL_MACRO_DATE: {
      const char *asc;
      if (preproc->fixed_timestamp) {
        asc = preproc->fixed_timestamp;
      } else {
        time_t epoch = time(NULL);
        struct tm *tm = localtime(&epoch);
        asc = asctime(tm);
      }

      size_t len = 10 + 2;
      char *buff = arena_alloc(preproc->arena, sizeof(*buff) * (len + 1));
      buff[0] = '\"';
      memcpy(&buff[1], asc, sizeof(*buff) * len);
      buff[len - 1] = '\"';
      buff[len] = '\0';

      struct preproc_token special_tok = {.ty = PREPROC_TOKEN_TY_STRING_LITERAL,
                                          .span = MK_INVALID_TEXT_SPAN(0, len),
                                          .text = buff};
      vector_push_back(buffer, &special_tok);
      break;
    }
    }

    return true;
  }

  return false;
}

enum preproc_token_mode {
  PREPROC_TOKEN_MODE_NO_EXPAND,
  PREPROC_TOKEN_MODE_EXPAND,
};

static void preproc_tokens_til_eol(struct preproc *preproc,
                                   struct preproc_text *preproc_text,
                                   struct vector *buffer,
                                   enum preproc_token_mode mode) {
  // this skips leading and trailing whitespace

  ssize_t last_nontrivial_token = -1;

  struct preproc_token token;
  while (true) {
    preproc_next_raw_token(preproc, &token);

    // TODO: do we handle EOF properly everywhere? or do we assume files end in
    // newline
    if (token.ty == PREPROC_TOKEN_TY_NEWLINE ||
        token.ty == PREPROC_TOKEN_TY_EOF) {
      break;
    }

    if ((token.ty == PREPROC_TOKEN_TY_WHITESPACE ||
         token.ty == PREPROC_TOKEN_TY_COMMENT) &&
        last_nontrivial_token == -1) {
      // skip leading whitespace
      continue;
    }

    if (mode == PREPROC_TOKEN_MODE_NO_EXPAND ||
        !try_expand_token(preproc, preproc_text, &token, buffer, NULL)) {
      vector_push_back(buffer, &token);
    }

    if (token.ty != PREPROC_TOKEN_TY_WHITESPACE &&
        token.ty != PREPROC_TOKEN_TY_COMMENT) {
      last_nontrivial_token = vector_length(buffer);
    }
  }

  if (last_nontrivial_token != -1) {
    vector_resize(buffer, last_nontrivial_token);
  }
}

static struct preproc_define *get_define(struct preproc *preproc,
                                         struct vector *directive_tokens) {
  if (vector_length(directive_tokens) != 1) {
    todo("handle bad define, had multiple tokens");
  }

  struct preproc_token def_name =
      *(struct preproc_token *)vector_head(directive_tokens);

  struct sized_str ident = {.str = def_name.text,
                            .len = text_span_len(&def_name.span)};

  return hashtbl_lookup(preproc->defines, &ident);
}

void preproc_next_token(struct preproc *preproc, struct preproc_token *token) {
  // expands tokens, adds defines, etc

  while (true) {
    enum preproc_token_mode mode;

    if (vector_empty(preproc->buffer_tokens)) {
      preproc_next_raw_token(preproc, token);
      mode = PREPROC_TOKEN_MODE_EXPAND;
    } else {
      // values in buffer have already been expanded
      *token = *(struct preproc_token *)vector_pop(preproc->buffer_tokens);
      mode = PREPROC_TOKEN_MODE_NO_EXPAND;
    }

    if (token->ty == PREPROC_TOKEN_TY_EOF) {
      return;
    }

    struct preproc_text *preproc_text = vector_tail(preproc->texts);

    // handle conditional directives first, as they can change `enabled`

    bool enabled = *(bool *)vector_tail(preproc->enabled);
    bool outer_enabled;
    if (vector_length(preproc->enabled) >= 2) {
      outer_enabled =
          vector_get(preproc->enabled, vector_length(preproc->enabled) - 2);
    } else {
      outer_enabled = false;
    }

    struct preproc_token directive;
    struct vector *directive_tokens =
        vector_create(sizeof(struct preproc_token));
    size_t num_directive_tokens;

#define EXPANDED_DIR_TOKENS()                                                  \
  do {                                                                         \
    preproc_tokens_til_eol(preproc, preproc_text, directive_tokens,            \
                           PREPROC_TOKEN_MODE_EXPAND);                         \
    num_directive_tokens = vector_length(directive_tokens);                    \
  } while (0)

#define UNEXPANDED_DIR_TOKENS()                                                \
  do {                                                                         \
    preproc_tokens_til_eol(preproc, preproc_text, directive_tokens,            \
                           PREPROC_TOKEN_MODE_NO_EXPAND);                      \
    num_directive_tokens = vector_length(directive_tokens);                    \
  } while (0)

    if (token->ty == PREPROC_TOKEN_TY_DIRECTIVE) {
      preproc_next_raw_token(preproc, &directive);

      if (directive.ty != PREPROC_TOKEN_TY_IDENTIFIER) {
        todo("error for non identifier directive");
      }

      if (token_streq(directive, "ifdef")) {
        UNEXPANDED_DIR_TOKENS();

        bool now_enabled = enabled && get_define(preproc, directive_tokens);
        vector_push_back(preproc->enabled, &now_enabled);
        continue;
      } else if (token_streq(directive, "ifndef")) {
        UNEXPANDED_DIR_TOKENS();

        bool now_enabled = enabled && !get_define(preproc, directive_tokens);
        vector_push_back(preproc->enabled, &now_enabled);
        continue;
      } else if (token_streq(directive, "endif")) {
        UNEXPANDED_DIR_TOKENS();

        vector_pop(preproc->enabled);
        continue;
      } else if (token_streq(directive, "else")) {
        UNEXPANDED_DIR_TOKENS();

        *(bool *)vector_tail(preproc->enabled) = outer_enabled && !enabled;
        continue;
      } else if (token_streq(directive, "elif")) {
        UNEXPANDED_DIR_TOKENS();

        todo("elif");
      } else if (token_streq(directive, "elifdef")) {
        UNEXPANDED_DIR_TOKENS();

        if (enabled) {
          *(bool *)vector_tail(preproc->enabled) = false;
        } else {
          bool now_enabled = get_define(preproc, directive_tokens);
          *(bool *)vector_tail(preproc->enabled) = outer_enabled && now_enabled;
        }
        continue;
      } else if (token_streq(directive, "elifndef")) {
        UNEXPANDED_DIR_TOKENS();

        if (enabled) {
          *(bool *)vector_tail(preproc->enabled) = false;
        } else {
          bool now_enabled = !get_define(preproc, directive_tokens);
          *(bool *)vector_tail(preproc->enabled) = outer_enabled && now_enabled;
        }
        continue;
      }

      // TODO: elifdef, elifndef
    }

    if (!enabled) {
      continue;
    }

    if (token->ty == PREPROC_TOKEN_TY_DIRECTIVE) {
      // `directive` token is already parsed

      // these directives do NOT expand
      if (token_streq(directive, "define")) {
        UNEXPANDED_DIR_TOKENS();

        struct preproc_token def_name =
            *(struct preproc_token *)vector_head(directive_tokens);

        size_t first_def_tok = 1;
        for (; first_def_tok < num_directive_tokens; first_def_tok++) {
          struct preproc_token *tok =
              vector_get(directive_tokens, first_def_tok);
          if (tok->ty != PREPROC_TOKEN_TY_NEWLINE &&
              tok->ty != PREPROC_TOKEN_TY_WHITESPACE) {
            break;
          }
        }

        vector_remove_range(directive_tokens, 0, first_def_tok);

        struct sized_str ident = {.str = def_name.text,
                                  .len = text_span_len(&def_name.span)};

        struct preproc_define define = {
            .name = def_name,
            .value = {.ty = PREPROC_DEFINE_VALUE_TY_TOKEN_VEC,
                      .vec = directive_tokens}};

        hashtbl_insert(preproc->defines, &ident, &define);
      } else if (token_streq(directive, "undef")) {
        UNEXPANDED_DIR_TOKENS();

        if (num_directive_tokens != 1) {
          todo("handle bad define, had multiple tokens");
        }

        struct preproc_token def_name =
            *(struct preproc_token *)vector_head(directive_tokens);

        struct sized_str ident = {.str = def_name.text,
                                  .len = text_span_len(&def_name.span)};


        hashtbl_remove(preproc->defines, &ident);
      } else if (token_streq(directive, "include")) {
        // these directives DO expand
        EXPANDED_DIR_TOKENS();

        preproc->in_angle_string_context = true;

        if (num_directive_tokens != 1) {
          todo("handle bad include, had multiple tokens");
        }

        struct preproc_token filename_token =
            *(struct preproc_token *)vector_head(directive_tokens);

        size_t filename_len = text_span_len(&filename_token.span);

        debug_assert(filename_len >= 2, "filename token can't be <2 chars");

        filename_len -= 2;

        // remove quotes
        char *filename = arena_alloc(preproc->arena, filename_len + 1);
        filename[filename_len] = 0;

        strncpy(filename, &filename_token.text[1], filename_len);

        bool is_angle = filename_token.text[0] == '<';

        const char *path = NULL;
        const char *content = NULL;
        if (is_angle) {
          for (size_t i = 0; i < preproc->num_include_paths; i++) {
            const char *search_path =
                path_combine(preproc->include_paths[i], filename);

            content = read_file(search_path);
            if (content) {
              path = search_path;
              break;
            }
          }
        } else {
          const char *search_path;
          if (preproc_text->path.dir) {
            search_path = path_combine(preproc_text->path.dir, filename);
          } else {
            search_path = filename;
          }

          content = read_file(search_path);
          path = search_path;
        }

        if (!content) {
          todo("handle failed includes");
        }

        struct preproc_text include_text = create_preproc_text(content, path);
        vector_push_back(preproc->texts, &include_text);

        preproc->line_has_nontrivial_token = false;
        preproc->in_angle_string_context = false;
      } else if (directive.ty == PREPROC_TOKEN_TY_IDENTIFIER &&
                 token_streq(directive, "line")) {
        EXPANDED_DIR_TOKENS();

        if (num_directive_tokens < 1 || num_directive_tokens > 2) {
          todo("handle bad line directive, had less than 1 / more than 2 "
               "tokens");
        }

        struct preproc_token line_num_tok =
            *(struct preproc_token *)vector_head(directive_tokens);
        char *end;
        size_t line_num = strtoull(line_num_tok.text, &end, 10);

        if (end - line_num_tok.text !=
            (long long)text_span_len(&line_num_tok.span)) {
          todo("handle failed line number parse");
        }

        preproc_text->line = line_num;
        debug("set line to '%zu'", line_num);

        if (num_directive_tokens == 2) {
          struct preproc_token file_tok =
              *(struct preproc_token *)vector_head(directive_tokens);
          size_t name_len = text_span_len(&file_tok.span);

          char *file =
              arena_alloc(preproc->arena, sizeof(*file) * name_len + 1);
          memcpy(file, file_tok.text, name_len);
          file[name_len] = '\0';

          preproc_text->file = file;
          debug("set file to '%s'", file);
        }
      } else {
        todo("other directives ('%.*s')", (int)text_span_len(&directive.span),
             directive.text);
      }

#undef UNEXPANDED_DIR_TOKENS
#undef EXPANDED_DIR_TOKENS

      continue;
    }

    if (mode == PREPROC_TOKEN_MODE_EXPAND &&
        try_expand_token(preproc, preproc_text, token, preproc->buffer_tokens,
                         NULL)) {
      continue;
    }

    return;
  }
}

void preproc_process(struct preproc *preproc, FILE *file) {
  struct preproc_token token;

  bool last_was_newline = false;
  bool last_was_whitespace = false;

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
      } else if (!last_was_whitespace) {
        fprintf(file, " ");
      }
      break;
    case PREPROC_TOKEN_TY_NEWLINE:
      if (!last_was_newline) {
        fprintf(file, "\n");
      }
      break;
    case PREPROC_TOKEN_TY_COMMENT:
      if (!last_was_whitespace) {
        fprintf(file, " ");
      }
      break;
    }

    last_was_newline = token.ty == PREPROC_TOKEN_TY_NEWLINE;
    last_was_whitespace = token.ty == PREPROC_TOKEN_TY_WHITESPACE ||
                          token.ty == PREPROC_TOKEN_TY_COMMENT;
  } while (token.ty != PREPROC_TOKEN_TY_EOF);
}
