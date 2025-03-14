#include "preproc.h"

#include "alloc.h"
#include "compiler.h"
#include "hash.h"
#include "hashtbl.h"
#include "io.h"
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

  // bool
  struct vector *enabled;
};

struct preproc {
  struct arena_allocator *arena;

  // struct preproc_text
  struct vector *texts;

  struct vector *unexpanded_buffer_tokens;
  struct vector *buffer_tokens;

  // struct sized_str, struct preproc_define
  struct hashtbl *defines;

  struct preproc_create_args args;

  // if the current line has seen a token that is not whitespace
  bool line_has_nontrivial_token;

  // whether we are in an include/embed that accepts <foo> style strings
  bool in_angle_string_context;

  // if a ## was just seen, we concat the next token
  bool concat_next_token;

  // if a defined/__has_feature etc was just seen, we don't expand until end
  bool keep_next_token;

  // after a `defined(SYM` symbol, we need to know to look to strip the close
  // bracket
  bool waiting_for_close;
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

enum preproc_define_value_ty {
  PREPROC_DEFINE_VALUE_TY_TOKEN,
  PREPROC_DEFINE_VALUE_TY_TOKEN_VEC,
  PREPROC_DEFINE_VALUE_TY_MACRO_FN
};

enum preproc_macro_fn_flags {
  PREPROC_MACRO_FN_FLAG_NONE = 0,
  PREPROC_MACRO_FN_FLAG_VARIADIC = 1,
};

struct preproc_macro_fn {
  size_t num_params;
  struct preproc_token *params;

  struct vector *tokens;

  enum preproc_macro_fn_flags flags;
};

struct preproc_define_value {
  enum preproc_define_value_ty ty;

  union {
    struct preproc_token token;
    struct vector *vec;
    struct preproc_macro_fn macro_fn;
  };
};

struct preproc_define {
  struct preproc_token name;
  struct preproc_define_value value;
};

static void preproc_create_builtin_macros(struct preproc *preproc,
                                          enum compile_target target) {
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

  // HACK: apple headers have __asm in them in weird locations. we define a
  // macro here to get rid of that
  // FIXME pthis breaks other inline asm as it will silently vanish! should fix

  {

    size_t name_len = strlen("__asm");
    struct sized_str ident = {.str = "__asm", .len = name_len};

    struct preproc_define define = {
        .name = {.ty = PREPROC_TOKEN_TY_IDENTIFIER,
                 .span = MK_INVALID_TEXT_SPAN(0, name_len),
                 .text = "__asm"},
        .value = {
            .ty = PREPROC_DEFINE_VALUE_TY_MACRO_FN,
            .macro_fn = {.num_params = 0,
                         .params = NULL,
                         .tokens = vector_create_in_arena(
                             sizeof(struct preproc_token), preproc->arena),
                         .flags = PREPROC_MACRO_FN_FLAG_VARIADIC}}};

    hashtbl_insert(preproc->defines, &ident, &define);
  }

  DEF_BUILTIN("__extension__", "");
  DEF_BUILTIN("__inline", "");

  if (target == COMPILE_TARGET_LINUX_RV32I) {
    // TEMP: stops it generating `bswap64` using `uint64_t` which causes IR to fail (as no 64 bit support on RV32I yet)
    DEF_BUILTIN("__GNUC__", "1");
  }

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

  switch (target) {
  case COMPILE_TARGET_MACOS_ARM64:
    DEF_BUILTIN("__APPLE__", "1");
    DEF_BUILTIN("__aarch64__", "1");
    DEF_BUILTIN("__arm64__", "1");
    DEF_BUILTIN("__LP64__", "1");
    DEF_BUILTIN("_LP64", "1");
    break;
  case COMPILE_TARGET_MACOS_X86_64:
    DEF_BUILTIN("__APPLE__", "1");
    DEF_BUILTIN("__x86_64__", "1");
    DEF_BUILTIN("__LP64__", "1");
    DEF_BUILTIN("_LP64", "1");
    break;
  case COMPILE_TARGET_LINUX_ARM64:
    DEF_BUILTIN("__linux__", "1");
    DEF_BUILTIN("__aarch64__", "1");
    DEF_BUILTIN("__arm64__", "1");
    DEF_BUILTIN("__LP64__", "1");
    DEF_BUILTIN("_LP64", "1");
    break;
  case COMPILE_TARGET_LINUX_X86_64:
    DEF_BUILTIN("__linux__", "1");
    DEF_BUILTIN("__x86_64__", "1");
    DEF_BUILTIN("__LP64__", "1");
    DEF_BUILTIN("_LP64", "1");
    break;
  case COMPILE_TARGET_LINUX_RV32I:
    DEF_BUILTIN("__linux__", "1");
    DEF_BUILTIN("__riscv", "1");
    DEF_BUILTIN("__riscv32", "1");
    DEF_BUILTIN("__riscv__", "1");
    DEF_BUILTIN("__LP32__", "1");
    DEF_BUILTIN("_LP32", "1");
    break;
  case COMPILE_TARGET_EEP:
    break;
  }

  // magic macros such as __TIME__ are handled in the processor

#undef DEF_BUILTIN
}

static struct preproc_text create_preproc_text(struct preproc *preproc,
                                               const char *text,
                                               const char *path) {
  struct path_components components =
      path ? path_components(preproc->arena, path)
           : (struct path_components){NULL, NULL, NULL};

  // FIXME: should have this in temp arena
  struct vector *enabled = vector_create_in_arena(sizeof(bool), preproc->arena);

  bool is_enabled = true;
  vector_push_back(enabled, &is_enabled);

  // FIXME: spans are entirely broken at the moment
  return (struct preproc_text){.text = text,
                               .len = strlen(text),
                               .pos = {.col = 0, .line = 0, .idx = 0},
                               .line = 0,
                               .file = components.file,
                               .path = components,
                               .enabled = enabled};
}

enum preproc_special_macro {
  PREPROC_SPECIAL_MACRO_FILE,
  PREPROC_SPECIAL_MACRO_LINE,
  PREPROC_SPECIAL_MACRO_TIME,
  PREPROC_SPECIAL_MACRO_DATE,
};

static struct hashtbl *SPECIAL_MACROS = NULL;

enum preproc_create_result preproc_create(struct program *program,
                                          struct preproc_create_args args,
                                          struct preproc **preproc) {
  if (args.fixed_timestamp) {
    DEBUG_ASSERT(strlen(args.fixed_timestamp) >= 19,
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
  p->args = args;

  if (args.verbose) {
    fprintf(stderr, "sys_include_paths: \n");
    for (size_t i = 0; i < args.num_sys_include_paths; i++) {
      fprintf(stderr, " %s\n", args.sys_include_paths[i]);
    }

    fprintf(stderr, "\ninclude_paths: \n");
    for (size_t i = 0; i < args.num_include_paths; i++) {
      fprintf(stderr, " %s\n", args.include_paths[i]);
    }
  }

  p->texts = vector_create(sizeof(struct preproc_text));

  struct preproc_text text = create_preproc_text(p, program->text, args.path);
  vector_push_back(p->texts, &text);

  p->line_has_nontrivial_token = false;
  p->in_angle_string_context = false;
  p->concat_next_token = false;
  p->keep_next_token = false;
  p->waiting_for_close = false;

  p->defines = hashtbl_create_sized_str_keyed(sizeof(struct preproc_define));

  // tokens that have appeared (e.g from a macro) and need to be processed next
  p->buffer_tokens = vector_create(sizeof(struct preproc_token));
  p->unexpanded_buffer_tokens = vector_create(sizeof(struct preproc_token));

  *preproc = p;

  preproc_create_builtin_macros(p, args.target);

  return PREPROC_CREATE_RESULT_SUCCESS;
}

void preproc_free(struct preproc **preproc) {
  struct hashtbl_iter *defines = hashtbl_iter((*preproc)->defines);
  struct hashtbl_entry entry;
  while (hashtbl_iter_next(defines, &entry)) {
    struct preproc_define *define = entry.data;

    if (define->value.ty == PREPROC_DEFINE_VALUE_TY_TOKEN_VEC) {
      vector_free(&define->value.vec);
    } else if (define->value.ty == PREPROC_DEFINE_VALUE_TY_MACRO_FN) {
      vector_free(&define->value.macro_fn.tokens);
    }
  }

  hashtbl_free(&(*preproc)->defines);

  size_t num_texts = vector_length((*preproc)->texts);
  for (size_t i = 0; i < num_texts; i++) {
    struct preproc_text *text = vector_get((*preproc)->texts, i);
    vector_free(&text->enabled);
  }

  vector_free(&(*preproc)->texts);
  vector_free(&(*preproc)->buffer_tokens);
  vector_free(&(*preproc)->unexpanded_buffer_tokens);

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
  DEBUG_ASSERT(
      preproc_text->pos.idx != pos->idx,
      "calling `try_consume` with `pos` the same as preproc makes no sense");

  DEBUG_ASSERT(c != '\n', "can't use on newlines");

  if (pos->idx < preproc_text->len && preproc_text->text[pos->idx] == c) {
    next_col(pos);

    return true;
  }

  return false;
}

// tries to consume two tokens (you can infer this from the function name)
static bool try_consume2(struct preproc_text *preproc_text,
                         struct text_pos *pos, char c0, char c1) {
  DEBUG_ASSERT(
      preproc_text->pos.idx != pos->idx,
      "calling `try_consume` with `pos` the same as preproc makes no sense");

  DEBUG_ASSERT(c0 != '\n' && c1 != '\n', "can't use on newlines");

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

    if (preproc_text->pos.idx < preproc_text->len ||
        vector_length(preproc->texts) == 1) {
      break;
    }

    DEBUG_ASSERT(vector_length(preproc_text->enabled) == 1,
                 "text %s ended with enabled depth of %zu (should have been 1)",
                 preproc_text->file, vector_length(preproc_text->enabled));

    vector_pop(preproc->texts);
  }

  if (vector_length(preproc->unexpanded_buffer_tokens)) {
    *token =
        *(struct preproc_token *)vector_pop(preproc->unexpanded_buffer_tokens);
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

    preproc->line_has_nontrivial_token = false;

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
    if (end.idx < preproc_text->len) {
      c = preproc_text->text[end.idx + 1];

      if (c == '<' || c == '"' || c == '\'') {
        next_col(&end);
        goto string_literal;
      }
    }

    break;

  case '<':
  case '"':
  case '\'':

  string_literal: {

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
  }
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

      if (end.idx + 1 < preproc_text->len &&
          (tolower(nc) == 'e' || tolower(nc) == 'p') &&
          (preproc_text->text[end.idx + 1] == '+' ||
           preproc_text->text[end.idx + 1] == '-')) {
        // need to check if it is an exponent
        next_col(&end);
        next_col(&end);
      } else if (is_preproc_number_char(nc)) {
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

    // HACK: we may have set in_angle_ctx because of __has_include
    // once we hit a close-bracket token, we know it must be done, so reset
    preproc->in_angle_string_context = false;
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

static void
preproc_next_nontrivial_token(struct preproc *preproc,
                              struct preproc_token *token,
                              enum preproc_expand_token_flags flags) {
  while (true) {
    preproc_next_token(preproc, token, flags);

    if (token->ty == PREPROC_TOKEN_TY_EOF) {
      BUG("hit eof scanning for nontrivial token; code was probably invalid "
          "but we should handle it better");
    }

    if (token->ty == PREPROC_TOKEN_TY_WHITESPACE ||
        token->ty == PREPROC_TOKEN_TY_COMMENT ||
        token->ty == PREPROC_TOKEN_TY_NEWLINE) {
      // skip leading whitespace
      continue;
    }

    return;
  }
}

static bool try_expand_token(struct preproc *preproc,
                             struct preproc_text *preproc_text,
                             struct preproc_token *token, struct vector *buffer,
                             struct hashtbl *parents,
                             enum preproc_expand_token_flags flags);

static void expand_token(struct preproc *preproc,
                         struct preproc_text *preproc_text,
                         struct preproc_token *token, struct vector *buffer,
                         struct hashtbl *parents,
                         enum preproc_expand_token_flags flags) {
  if (!try_expand_token(preproc, preproc_text, token, buffer, parents, flags)) {
    vector_push_back(buffer, token);
  }
}

static void preproc_append_tokens(struct preproc *preproc,
                                  struct preproc_text *preproc_text,
                                  struct vector *tokens, struct vector *buffer,
                                  struct hashtbl *parents,
                                  enum preproc_expand_token_flags flags) {
  size_t num_tokens = vector_length(tokens);
  for (size_t i = num_tokens; i; i--) {
    struct preproc_token *def_tok = vector_get(tokens, i - 1);
    expand_token(preproc, preproc_text, def_tok, buffer, parents, flags);
  }
}

static bool token_is_trivial(const struct preproc_token *token) {
  return token->ty == PREPROC_TOKEN_TY_WHITESPACE ||
         token->ty == PREPROC_TOKEN_TY_COMMENT;
}

// tokens that shouldn't be stripped from an `#if` expression or similar
static bool well_known_token(struct sized_str token) {
  static const char *well_known[] = {
      "defined", "has_include", "__has_include", "has_embed", "__has_embed",
      "__has_feature", "__has_builtin", "__has_attribute", "__has_c_attribute",
      // needed because clang headers do not properly avoid it
      // TODO: have more flexible solution to this (so we can ignore arbitary
      // __foo type macros)
      "__building_module"};

  for (size_t i = 0; i < ARR_LENGTH(well_known); i++) {
    // FIXME: there are a few strncmp here which should really use
    // max(lhs, rhs) as the len to prevent false prefix matching
    if (!strncmp(token.str, well_known[i], strlen(well_known[i]))) {
      return true;
    }
  }

  return false;
}

static struct preproc_token preproc_concat(struct preproc *preproc,
                                           struct preproc_token *token,
                                           struct preproc_token *last) {
  if (!token->text) {
    return *last;
  }

  if (!last->text) {
    return *token;
  }

#define TYPE_VAL(l, r)                                                         \
  ((long long)PREPROC_TOKEN_TY_##l << 32) | (PREPROC_TOKEN_TY_##r)
  switch (((long long)token->ty << 32) | last->ty) {
  case TYPE_VAL(IDENTIFIER, IDENTIFIER):
  case TYPE_VAL(IDENTIFIER, PREPROC_NUMBER):
  // because we process backwards, we have to accept `0 ## _bar`, because it
  // could be part of `foo_ ## 0 ## _bar`
  case TYPE_VAL(PREPROC_NUMBER, IDENTIFIER):
  case TYPE_VAL(PREPROC_NUMBER, PREPROC_NUMBER): {
    // for preproc number ## preproc number we just assume the output is
    // valid this isn't the best idea, we should probably re-parse it

    // because we use buffer as a stack, this is in REVERSE order
    size_t llen = text_span_len(&token->span);
    size_t rlen = text_span_len(&last->span);

    char *new = arena_alloc(preproc->arena, llen + rlen);
    memcpy(new, token->text, llen);
    memcpy(new + llen, last->text, rlen);

    struct preproc_token new_tok = {
        .ty = token->ty,
        .span = MK_INVALID_TEXT_SPAN(0, llen + rlen),
        .text = new,
    };

    return new_tok;
  }
  default:
    BUG("unsupported pair for concatenating tokens (%s ## %s)",
        preproc_token_name(token->ty), preproc_token_name(last->ty));
  }
}

static struct preproc_token preproc_stringify(struct preproc *preproc,
                                              struct vector *tokens);

static bool try_expand_token(struct preproc *preproc,
                             struct preproc_text *preproc_text,
                             struct preproc_token *token, struct vector *buffer,
                             struct hashtbl *parents,
                             enum preproc_expand_token_flags flags) {
  if (preproc->keep_next_token) {
    if (token_is_trivial(token)) {
      return true;
    } else if (token->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
               token->punctuator.ty ==
                   PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET) {
      // do we need to keep the whitespace/parens? we implicitly strip them here
      preproc->waiting_for_close = true;
      return true;
    } else if (token->ty == PREPROC_TOKEN_TY_IDENTIFIER ||
               token->ty == PREPROC_TOKEN_TY_STRING_LITERAL) {
      vector_push_back(buffer, token);
      if (!preproc->waiting_for_close) {
        preproc->keep_next_token = false;
      }
      return true;
    } else if (preproc->waiting_for_close &&
               token->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
               token->punctuator.ty ==
                   PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET) {
      preproc->waiting_for_close = false;
      preproc->keep_next_token = false;
      return true;
    } else {
      BUG("bad token type in `defined` or similar construct");
    }
  }

  if (token->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
      token->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_CONCAT) {
    if (preproc->concat_next_token) {
      BUG("saw two ## concat operators in a row; illegal");
    }

    preproc->concat_next_token = true;
    return true;
  }

  if (preproc->concat_next_token) {
    // this should only be called by fn macros, so it is fine to rely on the
    // tokens we care about being in unexpanded buffer (else the prev token
    // could already have been processed and passed out)

    if (token_is_trivial(token)) {
      return true;
    }

    struct preproc_token *last = NULL;
    while (vector_length(buffer)) {
      last = vector_pop(buffer);

      if (!token_is_trivial(last)) {
        break;
      }
    }

    if (!last) {
      preproc->concat_next_token = false;
      expand_token(preproc, preproc_text, token, buffer, parents, flags);
    } else {
      struct preproc_token new_tok = preproc_concat(preproc, token, last);

      // need to set _before_ recursive call into expand_token
      preproc->concat_next_token = false;
      parents = hashtbl_create_sized_str_keyed(0);
      expand_token(preproc, preproc_text, &new_tok, buffer, parents, flags);
    }

    // post-concat, self reference is not considered
    if (parents) {
      hashtbl_free(&parents);
    }
    return true;
  }

  if (token->ty != PREPROC_TOKEN_TY_IDENTIFIER) {
    return false;
  }

  // if identifier is a macro do something else
  struct sized_str ident = {.str = token->text,
                            .len = text_span_len(&token->span)};

  struct preproc_define *macro = hashtbl_lookup(preproc->defines, &ident);

  if (macro) {
    bool free_parents;
    if (!parents) {
      parents = hashtbl_create_sized_str_keyed(0);
      free_parents = true;
    } else {
      free_parents = false;
      void *parent = hashtbl_lookup(parents, &ident);

      if (parent) {
        // already seen this macro, do not expand it again
        return false;
      }
    }

    hashtbl_insert(parents, &ident, NULL);

    struct preproc_define_value *value = &macro->value;
    switch (value->ty) {
    case PREPROC_DEFINE_VALUE_TY_TOKEN:
      expand_token(preproc, preproc_text, &value->token, buffer, parents,
                   flags);
      break;
    case PREPROC_DEFINE_VALUE_TY_TOKEN_VEC: {
      preproc_append_tokens(preproc, preproc_text, value->vec, buffer, parents,
                            flags);
      break;
    }
    case PREPROC_DEFINE_VALUE_TY_MACRO_FN: {
      struct preproc_macro_fn macro_fn = value->macro_fn;

      struct vector *args =
          vector_create_in_arena(sizeof(struct vector *), preproc->arena);
      struct vector *arg =
          vector_create_in_arena(sizeof(struct preproc_token), preproc->arena);
      vector_push_back(args, &arg);

      // first need to take the arguments
      struct preproc_token open;
      preproc_next_nontrivial_token(preproc, &open, flags);

      if (open.ty != PREPROC_TOKEN_TY_PUNCTUATOR ||
          open.punctuator.ty != PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET) {
        vector_push_back(buffer, &open);
        return false;
      }

      int depth = 1;
      bool skip_trivial = false;
      bool seen_first_arg = false;
      while (true) {
        struct preproc_token next;
        preproc_next_token(preproc, &next, flags);

        if (next.ty == PREPROC_TOKEN_TY_EOF) {
          BUG("eof unexepctedly");
        }

        if (next.ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
            next.punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET) {
          depth++;

          vector_push_back(arg, &next);
        } else if (next.ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                   next.punctuator.ty ==
                       PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET) {
          if (!depth) {
            BUG("more close brackets than valid");
          }

          depth--;

          if (!depth) {
            if (!seen_first_arg && !macro_fn.num_params &&
                !(macro_fn.flags & PREPROC_MACRO_FN_FLAG_VARIADIC)) {
              vector_pop(args);
            }
            break;
          }

          vector_push_back(arg, &next);
        } else if (depth == 1 && next.ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                   next.punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_COMMA) {
          arg = vector_create_in_arena(sizeof(struct preproc_token),
                                       preproc->arena);
          vector_push_back(args, &arg);

          seen_first_arg = true;

          // strip leading whitespace
          skip_trivial = true;
        } else {
          if (!skip_trivial || !token_is_trivial(&next)) {
            vector_push_back(arg, &next);
            skip_trivial = false;
          }
        }
      }

      if (!(vector_length(args) == macro_fn.num_params ||
            ((macro_fn.flags & PREPROC_MACRO_FN_FLAG_VARIADIC) &&
             vector_length(args) >= macro_fn.num_params))) {
        BUG("wrong number of args (%zu) for fn-like macro with %zu params",
            vector_length(args), macro_fn.num_params);
      }

      struct vector *expanded_fn = vector_create(sizeof(struct preproc_token));
      struct vector *concat_points = vector_create(sizeof(size_t));

      // FIXME: super inefficient O(nm), macro fn should have hashtbl in it
      size_t num_tokens = vector_length(macro_fn.tokens);
      bool stringify = false;

      for (size_t i = num_tokens; i; i--) {
        struct preproc_token *def_tok = vector_get(macro_fn.tokens, i - 1);

        if (def_tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
            def_tok->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_CONCAT) {
          size_t len = vector_length(expanded_fn);
          // so we concat `len - 1` and `len`
          vector_push_back(concat_points, &len);

          continue;
        }

        if (stringify && !token_is_trivial(def_tok)) {
          DEBUG_ASSERT(def_tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                           def_tok->punctuator.ty ==
                               PREPROC_TOKEN_PUNCTUATOR_TY_STRINGIFY,
                       "expected # token because last token was stringify");
          // last was a stringify, so this is the '#'
          stringify = false;
          continue;
        }

        size_t next = i;
        for (; next > 1; next--) {
          struct preproc_token *next_tok =
              vector_get(macro_fn.tokens, next - 2);

          if (token_is_trivial(next_tok)) {
            continue;
          }

          if (next_tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
              next_tok->punctuator.ty ==
                  PREPROC_TOKEN_PUNCTUATOR_TY_STRINGIFY) {
            stringify = true;
            break;
          }
        }

        bool expanded = false;

        if (def_tok->ty == PREPROC_TOKEN_TY_IDENTIFIER) {
          if (!strncmp(def_tok->text, "__VA_ARGS__", strlen("__VA_ARGS__"))) {
            if (!(macro_fn.flags & PREPROC_MACRO_FN_FLAG_VARIADIC)) {
              BUG("__VA_ARGS__ in non-variadic macro");
            }

            size_t num_args = vector_length(args);
            for (size_t j = num_args; j > macro_fn.num_params; j--) {
              struct vector *arg_tokens =
                  *(struct vector **)vector_get(args, j - 1);

              if (stringify) {
                struct preproc_token str_tok =
                    preproc_stringify(preproc, arg_tokens);

                vector_push_back(expanded_fn, &str_tok);
              } else {
                size_t num_arg_tokens = vector_length(arg_tokens);
                // for (size_t k = num_arg_tokens; k; k--) {
                //   vector_push_back(expanded_fn, vector_get(arg_tokens, k -
                //   1));
                for (size_t k = 0; k < num_arg_tokens; k++) {
                  vector_push_back(expanded_fn, vector_get(arg_tokens, k));
                }
              }

              if (j - 1 != macro_fn.num_params) {
                struct preproc_token space = {.ty = PREPROC_TOKEN_TY_WHITESPACE,
                                              .text = " ",
                                              .span =
                                                  MK_INVALID_TEXT_SPAN(0, 1)};

                vector_push_back(expanded_fn, &space);

                struct preproc_token comma = {
                    .ty = PREPROC_TOKEN_TY_PUNCTUATOR,
                    .punctuator = {.ty = PREPROC_TOKEN_PUNCTUATOR_TY_COMMA},
                    .text = ",",
                    .span = MK_INVALID_TEXT_SPAN(0, 1)};

                vector_push_back(expanded_fn, &comma);
              }
            }
            expanded = true;
          } else {
            for (size_t j = 0; j < macro_fn.num_params; j++) {
              struct preproc_token param_tok = macro_fn.params[j];

              DEBUG_ASSERT(param_tok.ty == PREPROC_TOKEN_TY_IDENTIFIER,
                           "macro param must be identifier");

              struct sized_str def_ident = {
                  .str = def_tok->text, .len = text_span_len(&def_tok->span)};
              struct sized_str param_ident = {
                  .str = param_tok.text, .len = text_span_len(&param_tok.span)};

              if (hashtbl_eq_sized_str(&def_ident, &param_ident)) {
                struct vector *arg_tokens =
                    *(struct vector **)vector_get(args, j);

                if (stringify) {
                  struct preproc_token str_tok =
                      preproc_stringify(preproc, arg_tokens);

                  vector_push_back(expanded_fn, &str_tok);
                } else {
                  size_t num_arg_tokens = vector_length(arg_tokens);
                  for (size_t k = num_arg_tokens; k; k--) {
                    vector_push_back(expanded_fn,
                                     vector_get(arg_tokens, k - 1));
                  }
                }
                expanded = true;
                break;
              }
            }
          }
        }

        if (!expanded) {
          vector_push_back(expanded_fn, def_tok);
        }
      }

      if (stringify) {
        // no arg found after stringify token
        BUG("expected macro parameter token after stringify operator '#'");
      }

      size_t num_exp_tokens = vector_length(expanded_fn);
      size_t num_concat_points = vector_length(concat_points);
      for (size_t i = 0; i < num_concat_points; i++) {
        size_t concat_point = *(size_t *)vector_get(concat_points, i);

        struct preproc_token *left = vector_get(expanded_fn, concat_point - 1);

        for (size_t j = concat_point; j; (j--, left--)) {
          if (left->text && !token_is_trivial(left)) {
            break;
          }
        }

        struct preproc_token *right = vector_get(expanded_fn, concat_point);

        for (size_t j = concat_point; j < num_exp_tokens; (j++, right++)) {
          if (right->text && !token_is_trivial(right)) {
            break;
          }
        }

        struct preproc_token new = preproc_concat(preproc, right, left);
        *left = new;

        // use this as marker to skip token
        right->text = NULL;
      }

      for (size_t i = 0; i < num_exp_tokens; i++) {
        struct preproc_token *tok = vector_get(expanded_fn, i);

        if (!tok->text) {
          continue;
        }

        vector_push_back(preproc->unexpanded_buffer_tokens, tok);
      }

      vector_free(&expanded_fn);
      vector_free(&concat_points);

      break;
    }
    }

    if (free_parents) {
      hashtbl_free(&parents);
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
      if (preproc->args.fixed_timestamp) {
        asc = preproc->args.fixed_timestamp;
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
      if (preproc->args.fixed_timestamp) {
        asc = preproc->args.fixed_timestamp;
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

  if (well_known_token(ident)) {
    // mark to not expand symbols until we end this check
    preproc->keep_next_token = true;
    vector_push_back(buffer, token);
    return true;
  }

  if (flags & PREPROC_EXPAND_TOKEN_FLAG_UNDEF_ZERO) {
    struct preproc_token zero_tok = {.ty = PREPROC_TOKEN_TY_PREPROC_NUMBER,
                                     .span = MK_INVALID_TEXT_SPAN(0, 1),
                                     .text = "0"};
    vector_push_back(buffer, &zero_tok);
    return true;
  }

  return false;
}

enum preproc_token_mode {
  PREPROC_TOKEN_MODE_NO_EXPAND,
  PREPROC_TOKEN_MODE_EXPAND
};

struct include_info {
  const char *path;
  const char *content;
};

enum try_find_include_mode {
  TRY_FIND_INCLUDE_MODE_READ,

  // for `has_include`
  TRY_FIND_INCLUDE_MODE_TEST,
};

static bool try_include_path(struct preproc *preproc, const char *path,
                             const char **content,
                             enum try_find_include_mode mode) {
  if (!strcmp(path, "stdarg.h")) {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: special header 'starg.h'");
    }

    const char *STDARG_CONTENT =
        "\n"
        "#ifndef STDARG_H\n"
        "#define STDARG_H\n"
        "\n"
        "typedef void * __gnuc_va_list;\n"
        "#define __GNUC_VA_LIST\n"
        "\n"
        "#ifndef __need___va_list\n"
        "\n"
        "typedef void * va_list;\n"
        "\n"
        "#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202000L\n"
        "#define va_start(ap, ...) __builtin_va_start(ap, 0)\n"
        "#else\n"
        "#define va_start(ap, param) __builtin_va_start(ap, param)\n"
        "#endif\n"
        "\n"
        "#define va_end(ap) __builtin_va_end(ap);\n"
        "#define va_arg(ap, type) __builtin_va_arg(ap, type);\n"
        "#define va_copy(dest, src) __builtin_va_copy(dest, src)\n"
        "typedef __builtin_va_list va_list;\n"
        "#endif\n"
        "#endif\n";

    *content = STDARG_CONTENT;

    return true;
  } else if (!strcmp(path, "stddef.h")) {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: special header 'starg.h'");
    }

    const char *STDARG_CONTENT =
        "\n"
        "#ifndef STDDEF_H\n"
        "#define STDDEF_H\n"
        "\n"
        // "#ifdef __need_wint_t\n"
        "typedef int wint_t;\n"
        // "#endif\n"
        "\n"
        "#define NULL ((void*)0)\n"
        "typedef long ptrdiff_t;\n"
        "typedef unsigned long size_t;\n"
        // TODO: `max_align_t`
        // "typedef align max_align_t;\n"
        "\n"
        "#define offsetof(st, m) __builtin_offsetof(st, m)\n"
        "\n"
        "#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202000L\n"
        "typedef __NULLPTR_TYPE__ nullptr_t;\n"
        "#define unreachable() __builtin_unreachable()\n"
        "#endif\n"
        "\n"
        "#endif\n";

    *content = STDARG_CONTENT;

    return true;
  }

  if (preproc->args.verbose) {
    fprintf(stderr, "preproc: trying path '%s'\n", path);
  }

  bool found;

  switch (mode) {
  case TRY_FIND_INCLUDE_MODE_READ:
    *content = read_path(preproc->arena, path);
    found = *content != NULL;
    break;
  case TRY_FIND_INCLUDE_MODE_TEST: {
    FILE *file = fopen(path, "r");
    fclose(file);
    found = file != NULL;
    break;
  }
  }

  if (found) {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: found\n");
    }
    return true;
  } else {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: NOT found\n");
    }
    return false;
  }
}

static struct include_info try_find_include(struct preproc *preproc,
                                            struct preproc_text *preproc_text,
                                            const char *filename, bool is_angle,
                                            enum try_find_include_mode mode) {
  struct include_info info = {0};

  if (preproc->args.verbose && mode == TRY_FIND_INCLUDE_MODE_READ) {
    if (is_angle) {
      fprintf(stderr, "preproc: including <%s>\n", filename);
    } else {
      fprintf(stderr, "preproc: including \"%s\"\n", filename);
    }
  }

  if (!strcmp(filename, "stdarg.h") || !strcmp(filename, "stddef.h")) {
    info.path = filename;
    try_include_path(preproc, info.path, &info.content, mode);
    return info;
  }

  if (!is_angle) {
    if (preproc_text->path.dir) {
      info.path =
          path_combine(preproc->arena, preproc_text->path.dir, filename);
    } else {
      info.path = filename;
    }

    if (try_include_path(preproc, info.path, &info.content, mode)) {
      return info;
    }

    for (size_t i = 0; i < preproc->args.num_include_paths; i++) {
      info.path = path_combine(preproc->arena, preproc->args.include_paths[i],
                               filename);

      if (try_include_path(preproc, info.path, &info.content, mode)) {
        return info;
      }
    }
  }

  for (size_t i = 0; i < preproc->args.num_sys_include_paths; i++) {
    info.path = path_combine(preproc->arena, preproc->args.sys_include_paths[i],
                             filename);

    if (try_include_path(preproc, info.path, &info.content, mode)) {
      return info;
    }
  }

  return (struct include_info){0};
}

static void preproc_tokens_til_eol(struct preproc *preproc,
                                   struct preproc_text *preproc_text,
                                   struct vector *buffer,
                                   enum preproc_token_mode mode,
                                   enum preproc_expand_token_flags flags) {
  // this skips leading and trailing whitespace

  ssize_t last_nontrivial_token = -1;

  struct preproc_token token;
  while (true) {
    preproc_next_raw_token(preproc, &token);

    // TODO: do we handle EOF properly everywhere? or do we assume files end
    // in newline
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

    DEBUG_ASSERT(token.ty != PREPROC_TOKEN_TY_IDENTIFIER ||
                     !memchr(token.text, '\n', text_span_len(&token.span)),
                 "newline in identifier");

    if (token.ty == PREPROC_TOKEN_TY_IDENTIFIER &&
        (token_streq(token, "has_include") ||
         token_streq(token, "__has_include"))) {
      preproc->in_angle_string_context = true;
    }

    info("sub tok: %.*s", (int)text_span_len(&token.span), token.text);

    if (mode == PREPROC_TOKEN_MODE_NO_EXPAND ||
        !try_expand_token(preproc, preproc_text, &token, buffer, NULL, flags)) {
      vector_push_back(buffer, &token);
    }

    if (token.ty != PREPROC_TOKEN_TY_WHITESPACE &&
        token.ty != PREPROC_TOKEN_TY_COMMENT) {
      last_nontrivial_token = (ssize_t)vector_length(buffer);
    }
  }

  if (last_nontrivial_token != -1) {
    vector_resize(buffer, (size_t)last_nontrivial_token);
  }
}

static struct preproc_define *get_define(struct preproc *preproc,
                                         struct preproc_token def_name) {
  struct sized_str ident = {.str = def_name.text,
                            .len = text_span_len(&def_name.span)};

  return hashtbl_lookup(preproc->defines, &ident);
}

static int op_precedence(enum preproc_token_punctuator_ty ty) {
  switch (ty) {
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_OR:
    return 1;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_AND:
    return 2;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR:
    return 3;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR:
    return 4;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND:
    return 5;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_EQ:
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_NEQ:
    return 6;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_GT:
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_GTEQ:
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LT:
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LTEQ:
    return 7;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LSHIFT:
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_RSHIFT:
    return 8;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD:
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB:
    return 9;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL:
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV:
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT:
    return 10;
  default:
    BUG("bad token ty");
  }
}

struct include_path {
  bool is_angle;
  const char *filename;
};

static struct include_path get_include_path(struct preproc *preproc,
                                            struct preproc_token *token) {
  size_t filename_len = text_span_len(&token->span);

  DEBUG_ASSERT(filename_len >= 2, "filename token can't be <2 chars");

  filename_len -= 2;

  // remove quotes
  char *filename = arena_alloc(preproc->arena, filename_len + 1);
  filename[filename_len] = 0;

  strncpy(filename, &token->text[1], filename_len);

  bool is_angle = token->text[0] == '<';

  return (struct include_path){.is_angle = is_angle, .filename = filename};
}

static unsigned long long eval_expr(struct preproc *preproc,
                                    struct preproc_text *preproc_text,
                                    struct vector *tokens, size_t *i,
                                    size_t num_tokens, int min_prec);

static unsigned long long eval_atom(struct preproc *preproc,
                                    struct preproc_text *preproc_text,
                                    struct vector *tokens, size_t *i,
                                    size_t num_tokens) {
  for (; *i < num_tokens;) {
    struct preproc_token *token = vector_get(tokens, *i);

    switch (token->ty) {
    case PREPROC_TOKEN_TY_PREPROC_NUMBER: {
      unsigned long long num;
      if (!try_parse_integer(token->text, text_span_len(&token->span), &num)) {
        BUG("bad value in preproc expr");
      }

      (*i)++;
      return num;
    }
#define MAX_PREC 100
    case PREPROC_TOKEN_TY_PUNCTUATOR:
      switch (token->punctuator.ty) {
      case PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET: {
        (*i)++;
        return eval_expr(preproc, preproc_text, tokens, i, num_tokens,
                         MAX_PREC);
      }
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB: {
        // THIS IS WRONG ! min prec needs to be higha
        (*i)++;
        unsigned long long val =
            eval_expr(preproc, preproc_text, tokens, i, num_tokens, MAX_PREC);
        return -val;
      }
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_NOT: {
        (*i)++;
        unsigned long long val =
            eval_expr(preproc, preproc_text, tokens, i, num_tokens, MAX_PREC);
        return !val;
      }
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_NOT: {
        (*i)++;
        unsigned long long val =
            eval_expr(preproc, preproc_text, tokens, i, num_tokens, MAX_PREC);
        return ~val;
      }
      default:
        fprintf(stderr, "%.*s\n", 50, token->text);
        BUG("did not expect this token type in directive expr");
        break;
      }
    case PREPROC_TOKEN_TY_IDENTIFIER: {
      (*i)++;

      while (*i < num_tokens &&
             token_is_trivial((struct preproc_token *)vector_get(tokens, *i))) {
        (*i)++;
      }

      struct preproc_token *next = vector_get(tokens, *i);
      (*i)++;

      if (next->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
          next->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET) {

        while (
            *i < num_tokens &&
            token_is_trivial((struct preproc_token *)vector_get(tokens, *i))) {
          (*i)++;
        }

        next = vector_get(tokens, *i);
        (*i)++;

        while (
            *i < num_tokens &&
            token_is_trivial((struct preproc_token *)vector_get(tokens, *i))) {
          (*i)++;
        }

        struct preproc_token *close = vector_get(tokens, *i);
        (*i)++;

        if (close->ty != PREPROC_TOKEN_TY_PUNCTUATOR ||
            close->punctuator.ty != PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET) {
          BUG("expected `)` after `defined/__has_feature etc(`");
        }
      }

      if (next->ty != PREPROC_TOKEN_TY_IDENTIFIER &&
          next->ty != PREPROC_TOKEN_TY_STRING_LITERAL) {
        BUG("expected identifier after `defined/__has_feature etc(`");
      }

#define STREQ(a, b) (!strncmp((a), (b), strlen((b))))

      if (STREQ(token->text, "defined")) {
        return get_define(preproc, *next) ? 1 : 0;
      } else if (STREQ(token->text, "__has_include") ||
                 STREQ(token->text, "has_include")) {
        struct include_path include_path = get_include_path(preproc, next);
        struct include_info include_info =
            try_find_include(preproc, preproc_text, include_path.filename,
                             include_path.is_angle, TRY_FIND_INCLUDE_MODE_READ);

        return include_info.path != NULL;
      } else if (STREQ(token->text, "__has_feature")) {
        return 0;
      } else {
        warn("unknown identifier '%.*s' in preproc",
             (int)text_span_len(&token->span), token->text);
        return 0;
      }
    }
    case PREPROC_TOKEN_TY_WHITESPACE:
    case PREPROC_TOKEN_TY_COMMENT:
      (*i)++;
      continue;
    default:
      fprintf(stderr, "%.*s\n", 50, token->text);
      BUG("did not expect this token type in directive expr");
    }
  }

  BUG("expected value in expression");
}

static unsigned long long eval_expr(struct preproc *preproc,
                                    struct preproc_text *preproc_text,
                                    struct vector *tokens, size_t *i,
                                    size_t num_tokens, int min_prec) {
  long long value = eval_atom(preproc, preproc_text, tokens, i, num_tokens);

  for (; *i < num_tokens;) {
    struct preproc_token *token = vector_get(tokens, *i);

    (*i)++;

    switch (token->ty) {
    case PREPROC_TOKEN_TY_UNKNOWN:
    case PREPROC_TOKEN_TY_EOF:
    case PREPROC_TOKEN_TY_DIRECTIVE:
    case PREPROC_TOKEN_TY_STRING_LITERAL:
    case PREPROC_TOKEN_TY_OTHER:
    case PREPROC_TOKEN_TY_NEWLINE:
    case PREPROC_TOKEN_TY_IDENTIFIER:
    case PREPROC_TOKEN_TY_PREPROC_NUMBER:
      fprintf(stderr, "%.*s\n", 50, token->text);
      BUG("did not expect this token type in directive expr (%s)",
          preproc_text->file);
    case PREPROC_TOKEN_TY_PUNCTUATOR: {
      if (token->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET ||
          token->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_COLON) {

        (*i)--;
        return value;
      }

      if (token->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_QMARK) {
        long long ternary_lhs =
            eval_expr(preproc, preproc_text, tokens, i, num_tokens, 0);

        while (*i < num_tokens) {
          struct preproc_token *t = vector_get(tokens, *i);
          if (t->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
              t->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_COLON) {
            break;
          }

          (*i)++;
        }
        (*i)++;

        long long ternary_rhs =
            eval_expr(preproc, preproc_text, tokens, i, num_tokens, 0);

        return value ? ternary_lhs : ternary_rhs;
      }

      int precedence = op_precedence(token->punctuator.ty);

      if (precedence < min_prec) {
        (*i)--;
        return value;
      }

      long long rhs = eval_expr(preproc, preproc_text, tokens, i, num_tokens,
                                precedence + 1);

      switch (token->punctuator.ty) {
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_OR:
        value = value || rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_AND:
        value = value && rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR:
        value = value | rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR:
        value = value ^ rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND:
        value = value & rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_EQ:
        value = value == rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_NEQ:
        value = value != rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_GT:
        value = value > rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_GTEQ:
        value = value >= rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LT:
        value = value < rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LTEQ:
        value = value <= rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LSHIFT:
        value = value << rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_RSHIFT:
        value = value >> rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD:
        value = value + rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB:
        value = value - rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL:
        value = value * rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV:
        value = value / rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT:
        value = value % rhs;
        break;
      default:
        BUG("bad token ty");
      }
      break;
    }
    case PREPROC_TOKEN_TY_WHITESPACE:
    case PREPROC_TOKEN_TY_COMMENT:
      continue;
    }
  }

  return value;
}

void preproc_next_token(struct preproc *preproc, struct preproc_token *token,
                        enum preproc_expand_token_flags flags) {
  // expands tokens, adds defines, etc

  struct vector *directive_tokens = NULL;

  while (true) {
    if (directive_tokens) {
      vector_free(&directive_tokens);
    }

    enum preproc_token_mode mode;

    if (vector_length(preproc->buffer_tokens)) {
      // values in buffer have already been expanded
      *token = *(struct preproc_token *)vector_pop(preproc->buffer_tokens);
      mode = PREPROC_TOKEN_MODE_NO_EXPAND;
    } else {
      preproc_next_raw_token(preproc, token);
      mode = PREPROC_TOKEN_MODE_EXPAND;
    }

    if (token->ty == PREPROC_TOKEN_TY_EOF) {
      return;
    }

    struct preproc_text *preproc_text = vector_tail(preproc->texts);

    // handle conditional directives first, as they can change `enabled`

    size_t num_enabled = vector_length(preproc_text->enabled);
    bool enabled = *(bool *)vector_tail(preproc_text->enabled);

    // outer enabled is whether this block runs at all
    // cond done is whether an `if` or `elif` branch has run, and so the rest
    // should not run
    // TODO: make this a struct and less horrendously ugly
    bool outer_enabled;
    bool cond_done;
    if (vector_length(preproc_text->enabled) >= 3) {
      outer_enabled =
          *(bool *)vector_get(preproc_text->enabled, num_enabled - 3);
      cond_done = *(bool *)vector_get(preproc_text->enabled, num_enabled - 2);
    } else {
      outer_enabled = true;
      cond_done = false;
    }

    struct preproc_token directive;
    size_t num_directive_tokens;

#define EXPANDED_DIR_TOKENS()                                                  \
  do {                                                                         \
    bool old = *(bool *)vector_tail(preproc_text->enabled);                    \
    *(bool *)vector_tail(preproc_text->enabled) = true;                        \
    preproc_tokens_til_eol(preproc, preproc_text, directive_tokens,            \
                           PREPROC_TOKEN_MODE_EXPAND,                          \
                           PREPROC_EXPAND_TOKEN_FLAG_NONE);                    \
    *(bool *)vector_tail(preproc_text->enabled) = old;                         \
    num_directive_tokens = vector_length(directive_tokens);                    \
  } while (0)
#define EXPANDED_UNDEF_ZERO_DIR_TOKENS()                                       \
  do {                                                                         \
    bool old = *(bool *)vector_tail(preproc_text->enabled);                    \
    *(bool *)vector_tail(preproc_text->enabled) = true;                        \
    preproc_tokens_til_eol(preproc, preproc_text, directive_tokens,            \
                           PREPROC_TOKEN_MODE_EXPAND,                          \
                           PREPROC_EXPAND_TOKEN_FLAG_UNDEF_ZERO);              \
    *(bool *)vector_tail(preproc_text->enabled) = old;                         \
    num_directive_tokens = vector_length(directive_tokens);                    \
  } while (0)

#define UNEXPANDED_DIR_TOKENS()                                                \
  do {                                                                         \
    preproc_tokens_til_eol(preproc, preproc_text, directive_tokens,            \
                           PREPROC_TOKEN_MODE_NO_EXPAND,                       \
                           PREPROC_EXPAND_TOKEN_FLAG_NONE);                    \
    num_directive_tokens = vector_length(directive_tokens);                    \
  } while (0)

    if (token->ty == PREPROC_TOKEN_TY_DIRECTIVE) {
      directive_tokens =
          vector_create_in_arena(sizeof(struct preproc_token), preproc->arena);

      do {
        preproc_next_raw_token(preproc, &directive);
      } while (directive.ty == PREPROC_TOKEN_TY_COMMENT ||
               directive.ty == PREPROC_TOKEN_TY_WHITESPACE);

      if (directive.ty != PREPROC_TOKEN_TY_IDENTIFIER) {
        TODO("error for non identifier directive");
      }
      if (token_streq(directive, "ifdef")) {
        bool now_enabled = false;
        if (enabled) {
          UNEXPANDED_DIR_TOKENS();
          now_enabled = get_define(
              preproc, *(struct preproc_token *)vector_head(directive_tokens));
        }
        vector_push_back(preproc_text->enabled, &now_enabled);
        vector_push_back(preproc_text->enabled, &now_enabled);
        continue;
      } else if (token_streq(directive, "ifndef")) {
        bool now_enabled = false;
        if (enabled) {
          UNEXPANDED_DIR_TOKENS();
          now_enabled = !get_define(
              preproc,

              *(struct preproc_token *)vector_head(directive_tokens));
        }
        vector_push_back(preproc_text->enabled, &now_enabled);
        vector_push_back(preproc_text->enabled, &now_enabled);
        continue;
      } else if (token_streq(directive, "if")) {
        size_t i = 0;
        bool now_enabled = false;
        if (enabled) {
          EXPANDED_UNDEF_ZERO_DIR_TOKENS();
          now_enabled = eval_expr(preproc, preproc_text, directive_tokens, &i,
                                  vector_length(directive_tokens), 0);
        }
        vector_push_back(preproc_text->enabled, &now_enabled);
        vector_push_back(preproc_text->enabled, &now_enabled);
        continue;
      } else if (token_streq(directive, "endif")) {
        UNEXPANDED_DIR_TOKENS();

        vector_pop(preproc_text->enabled);
        vector_pop(preproc_text->enabled);

        continue;
      } else if (token_streq(directive, "else")) {
        UNEXPANDED_DIR_TOKENS();

        *(bool *)vector_tail(preproc_text->enabled) =
            outer_enabled && !cond_done;
        continue;
      } else if (token_streq(directive, "elif")) {
        if (cond_done) {
          *(bool *)vector_tail(preproc_text->enabled) = false;
        } else {
          size_t i = 0;

          bool now_enabled = false;
          if (outer_enabled) {
            EXPANDED_UNDEF_ZERO_DIR_TOKENS();
            now_enabled = eval_expr(preproc, preproc_text, directive_tokens, &i,
                                    vector_length(directive_tokens), 0);
          }
          *(bool *)vector_tail(preproc_text->enabled) = now_enabled;
          *(bool *)vector_get(preproc_text->enabled, num_enabled - 2) =
              now_enabled;
        }
        continue;
      } else if (token_streq(directive, "elifdef")) {
        UNEXPANDED_DIR_TOKENS();

        if (cond_done) {
          *(bool *)vector_tail(preproc_text->enabled) = false;
        } else {
          bool now_enabled = get_define(
              preproc, *(struct preproc_token *)vector_head(directive_tokens));
          *(bool *)vector_tail(preproc_text->enabled) =
              outer_enabled && now_enabled;
          *(bool *)vector_get(preproc_text->enabled, num_enabled - 2) =
              outer_enabled && now_enabled;
        }
        continue;
      } else if (token_streq(directive, "elifndef")) {
        UNEXPANDED_DIR_TOKENS();

        if (cond_done) {
          *(bool *)vector_tail(preproc_text->enabled) = false;
        } else {
          bool now_enabled = !get_define(
              preproc, *(struct preproc_token *)vector_head(directive_tokens));
          *(bool *)vector_tail(preproc_text->enabled) =
              outer_enabled && now_enabled;
          *(bool *)vector_get(preproc_text->enabled, num_enabled - 2) =
              outer_enabled && now_enabled;
        }
        continue;
      }
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

        struct preproc_define define = {
            .name = def_name,
        };

        struct preproc_token *tok = NULL;
        if (first_def_tok < num_directive_tokens) {
          tok = vector_get(directive_tokens, first_def_tok);
        }

        enum preproc_macro_fn_flags macro_flags = PREPROC_MACRO_FN_FLAG_NONE;

        if (tok && tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
            tok->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET) {
          first_def_tok++;
          // fn-like macro

          struct vector *params = vector_create_in_arena(
              sizeof(struct preproc_token), preproc->arena);

          while (true) {
            if (first_def_tok >= num_directive_tokens) {
              BUG("invalid macro fn, expected `)`");
            }

            tok = vector_get(directive_tokens, first_def_tok++);

            if (tok->ty == PREPROC_TOKEN_TY_IDENTIFIER) {
              vector_push_back(params, tok);
            } else if (tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR) {
              if (tok->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_COMMA) {
                continue;
              } else if (tok->punctuator.ty ==
                         PREPROC_TOKEN_PUNCTUATOR_TY_ELLIPSIS) {
                macro_flags |= PREPROC_MACRO_FN_FLAG_VARIADIC;
              } else if (tok->punctuator.ty ==
                         PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET) {
                break;
              } else {
                BUG("bad token in macro fn def");
              }
            }
          }

          define.value = (struct preproc_define_value){
              .ty = PREPROC_DEFINE_VALUE_TY_MACRO_FN,
              .macro_fn = {.num_params = vector_length(params),
                           .params = vector_head(params),
                           .tokens = directive_tokens,
                           .flags = macro_flags}};
        } else {
          if (vector_empty(directive_tokens)) {
            // give it a single empty token instead to make logic easier
            // (e.g concat operator can always wait for next token)

            vector_free(&directive_tokens);

            struct preproc_token empty = {
                .ty = PREPROC_TOKEN_TY_IDENTIFIER,
                .span = MK_INVALID_TEXT_SPAN(0, 0),
                .text = "",
            };
            define.value = (struct preproc_define_value){
                .ty = PREPROC_DEFINE_VALUE_TY_TOKEN, .token = empty};
          } else {

            define.value = (struct preproc_define_value){
                .ty = PREPROC_DEFINE_VALUE_TY_TOKEN_VEC,
                .vec = directive_tokens};
          }
        }

        for (; first_def_tok < num_directive_tokens; first_def_tok++) {
          tok = vector_get(directive_tokens, first_def_tok);
          if (tok->ty != PREPROC_TOKEN_TY_NEWLINE &&
              tok->ty != PREPROC_TOKEN_TY_WHITESPACE) {
            break;
          }
        }

        vector_remove_range(directive_tokens, 0, first_def_tok);

        struct sized_str ident = {.str = def_name.text,
                                  .len = text_span_len(&def_name.span)};

        // don't free them
        directive_tokens = NULL;

        struct preproc_define *def = hashtbl_lookup(preproc->defines, &ident);
        if (def) {
          if (def->value.ty == PREPROC_DEFINE_VALUE_TY_TOKEN_VEC) {
            vector_free(&def->value.vec);
          }

          *def = define;
        } else {
          hashtbl_insert(preproc->defines, &ident, &define);
        }
      } else if (token_streq(directive, "undef")) {
        UNEXPANDED_DIR_TOKENS();

        if (num_directive_tokens != 1) {
          TODO("handle bad define, had multiple tokens");
        }

        struct preproc_token def_name =
            *(struct preproc_token *)vector_head(directive_tokens);

        struct sized_str ident = {.str = def_name.text,
                                  .len = text_span_len(&def_name.span)};

        // FIXME: inefficient lookup + remove
        struct preproc_define *def = hashtbl_lookup(preproc->defines, &ident);
        if (def && def->value.ty == PREPROC_DEFINE_VALUE_TY_TOKEN_VEC) {
          vector_free(&def->value.vec);
        }

        hashtbl_remove(preproc->defines, &ident);
      } else if (token_streq(directive, "include_next")) {
        TODO("include_next");
      } else if (token_streq(directive, "include")) {
        preproc->in_angle_string_context = true;

        UNEXPANDED_DIR_TOKENS();

        if (num_directive_tokens != 1) {
          TODO("handle bad include, had multiple tokens");
        }

        // TODO: allow macros as header names (GCC allows this)

        struct preproc_token *filename_token = vector_head(directive_tokens);

        struct include_path include_path =
            get_include_path(preproc, filename_token);
        struct include_info include_info =
            try_find_include(preproc, preproc_text, include_path.filename,
                             include_path.is_angle, TRY_FIND_INCLUDE_MODE_READ);

        if (!include_info.path) {
          TODO("handle failed include search for '%s'", include_path.filename);
        } else if (!include_info.content) {
          TODO("handle failed include read for '%s'", include_path.filename);
        }

        struct preproc_text include_text = create_preproc_text(
            preproc, include_info.content, include_info.path);
        vector_push_back(preproc->texts, &include_text);

        preproc->line_has_nontrivial_token = false;
        preproc->in_angle_string_context = false;
      } else if (directive.ty == PREPROC_TOKEN_TY_IDENTIFIER &&
                 token_streq(directive, "line")) {
        EXPANDED_DIR_TOKENS();

        if (num_directive_tokens < 1 || num_directive_tokens > 2) {
          TODO("handle bad line directive, had less than 1 / more than 2 "
               "tokens");
        }

        struct preproc_token line_num_tok =
            *(struct preproc_token *)vector_head(directive_tokens);
        char *end;
        size_t line_num = strtoull(line_num_tok.text, &end, 10);

        if (end - line_num_tok.text !=
            (long long)text_span_len(&line_num_tok.span)) {
          TODO("handle failed line number parse");
        }

        preproc_text->line = line_num - directive.span.start.line - 2;
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
      } else if (token_streq(directive, "error")) {
        // these directives DO expand
        EXPANDED_DIR_TOKENS();

        errsl("preproc error: ");
        for (size_t i = 0; i < num_directive_tokens; i++) {
          struct preproc_token *err_token = vector_get(directive_tokens, i);
          errsl("%.*s", (int)text_span_len(&err_token->span), err_token->text);
        }
        errsl("\n");

        BUG("PREPROC #error directive");
      } else if (token_streq(directive, "warning")) {
        EXPANDED_DIR_TOKENS();

        // TODO: emit diagnostic (once we have those...)
        warnsl("preproc warning: ");
        for (size_t i = 0; i < num_directive_tokens; i++) {
          struct preproc_token *warn_token = vector_get(directive_tokens, i);
          slogsl("%.*s", (int)text_span_len(&warn_token->span),
                 warn_token->text);
        }
        slogsl("\n");
      } else {
        TODO("other directives ('%.*s')", (int)text_span_len(&directive.span),
             directive.text);
      }

#undef UNEXPANDED_DIR_TOKENS
#undef EXPANDED_DIR_TOKENS

      continue;
    }

    if (mode == PREPROC_TOKEN_MODE_EXPAND &&
        try_expand_token(preproc, preproc_text, token, preproc->buffer_tokens,
                         NULL, flags)) {
      continue;
    }

    break;
  }

  if (directive_tokens) {
    vector_free(&directive_tokens);
  }
}

static void add_escaped_str(struct vector *buf, const char *str, size_t len) {
  vector_ensure_capacity(buf, vector_length(buf) + len);

  char slash = '\\';
  for (size_t i = 0; i < len; i++) {
    char ch = str[i];

#define ADD_ESCAPED(esc)                                                       \
  case esc: {                                                                  \
    char c = esc;                                                              \
    vector_push_back(buf, &slash);                                             \
    vector_push_back(buf, &c);                                                 \
    break;                                                                     \
  }

    switch (ch) {
      ADD_ESCAPED('\0')
      ADD_ESCAPED('\a')
      ADD_ESCAPED('\b')
      ADD_ESCAPED('\f')
      ADD_ESCAPED('\n')
      ADD_ESCAPED('\r')
      ADD_ESCAPED('\t')
      ADD_ESCAPED('\v')
      ADD_ESCAPED('\\')
      ADD_ESCAPED('\'')
      ADD_ESCAPED('"')
    default:
      vector_push_back(buf, &ch);
      break;
    }

#undef ADD_ESCAPED
  }
}

static struct preproc_token preproc_stringify(struct preproc *preproc,
                                              struct vector *tokens) {
  // _very_ similar to `preproc_process` maybe we can merge them

  bool last_was_newline = false;
  bool last_was_whitespace = false;

  struct vector *buf = vector_create_in_arena(sizeof(char), preproc->arena);

  char quote = '"';
  vector_push_back(buf, &quote);

  size_t num_tokens = vector_length(tokens);
  for (size_t i = 0; i < num_tokens; i++) {
    struct preproc_token token = *(struct preproc_token *)vector_get(tokens, i);

#define ADD_STR(str, len) add_escaped_str(buf, str, len);

    switch (token.ty) {
    case PREPROC_TOKEN_TY_UNKNOWN:
      ADD_STR("?UNKNOWN?", strlen("?UNKNOWN?"));
      break;
    case PREPROC_TOKEN_TY_EOF:
      break;
    case PREPROC_TOKEN_TY_DIRECTIVE:
      BUG("directive in process");
    case PREPROC_TOKEN_TY_IDENTIFIER:
    case PREPROC_TOKEN_TY_PREPROC_NUMBER:
    case PREPROC_TOKEN_TY_STRING_LITERAL:
    case PREPROC_TOKEN_TY_PUNCTUATOR:
    case PREPROC_TOKEN_TY_OTHER:
      ADD_STR(token.text, text_span_len(&token.span));
      break;
    case PREPROC_TOKEN_TY_WHITESPACE:
      if (last_was_newline) {
        ADD_STR(token.text, text_span_len(&token.span));
      } else if (!last_was_whitespace) {
        ADD_STR(" ", 1);
      }
      break;
    case PREPROC_TOKEN_TY_NEWLINE:
      if (!last_was_newline) {
        ADD_STR("\n", 1);
      }
      break;
    case PREPROC_TOKEN_TY_COMMENT:
      if (!last_was_whitespace) {
        ADD_STR(" ", 1);
      }
      break;
    }

    last_was_newline = token.ty == PREPROC_TOKEN_TY_NEWLINE;
    last_was_whitespace = token.ty == PREPROC_TOKEN_TY_WHITESPACE ||
                          token.ty == PREPROC_TOKEN_TY_COMMENT;
  }

  vector_push_back(buf, &quote);

  return (struct preproc_token){
      .ty = PREPROC_TOKEN_TY_STRING_LITERAL,
      .span = MK_INVALID_TEXT_SPAN(/* garbage */ 0, vector_length(buf)),
      .text = vector_head(buf)};
}

void preproc_process(struct preproc *preproc, FILE *file) {
  struct preproc_token token;

  bool last_was_newline = false;
  bool last_was_whitespace = false;

  do {
    preproc_next_token(preproc, &token, PREPROC_EXPAND_TOKEN_FLAG_NONE);

    switch (token.ty) {
    case PREPROC_TOKEN_TY_UNKNOWN:
      fprintf(file, "?UNKNOWN?");
      break;
    case PREPROC_TOKEN_TY_EOF:
      break;
    case PREPROC_TOKEN_TY_DIRECTIVE:
      BUG("directive in process");
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
