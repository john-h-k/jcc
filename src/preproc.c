#include "preproc.h"

#include "alloc.h"
#include "compiler.h"
#include "diagnostics.h"
#include "fcache.h"
#include "hashtbl.h"
#include "io.h"
#include "log.h"
#include "profile.h"
#include "program.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>
#include <stddef.h>
#include <string.h>
#include <time.h>
#include <wchar.h>

#if ARCH_AARCH64 && __has_include(<arm_neon.h>)
#define USE_NEON 1
#include <arm_neon.h>
#include <stddef.h>
#include <stdint.h>
#endif

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
  struct fcache *fcache;
  struct arena_allocator *arena;

  // struct preproc_text
  struct vector *texts;

  struct vector *unexpanded_buffer_tokens;
  struct vector *buffer_tokens;

  // ustr_t, struct preproc_define
  struct hashtbl *defines;

  // stores current macros we are expanding in to prevent recursion
  struct hashtbl *parents;

  struct compiler_diagnostics *diagnostics;

  struct preproc_create_args args;

  // if the current line has seen a token that is not whitespace
  bool line_has_nontrivial_token;

  // whether we are in an include/embed that accepts <foo> style strings
  bool in_angle_string_context;

  // if a ## was just seen, we concat the next token
  bool concat_next_token;

  // if a defined/__has_feature etc was just seen, we don't expand until end
  ustr_t query_ident;
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

enum preproc_macro_fn_token_ty {
  PREPROC_MACRO_FN_TOKEN_TY_TOKEN,
  PREPROC_MACRO_FN_TOKEN_TY_PARAM,
  PREPROC_MACRO_FN_TOKEN_TY_STRINGIFIED_PARAM,
  PREPROC_MACRO_FN_TOKEN_TY_VA_ARGS,
  PREPROC_MACRO_FN_TOKEN_TY_STRINGIFIED_VA_ARGS,
  PREPROC_MACRO_FN_TOKEN_TY_VA_OPT,
};

struct preproc_macro_fn_token {
  enum preproc_macro_fn_token_ty ty;

  union {
    struct preproc_token token;
    struct preproc_token opt;
    size_t param;
  };
};

struct preproc_macro_fn {
  size_t num_params;
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

enum preproc_define_flags {
  PREPROC_DEFINE_FLAG_NONE = 0,
  // e.g `defined`, `__has_include`
  PREPROC_DEFINE_FLAG_WELL_KNOWN = 1 << 0,
};

struct preproc_define {
  struct preproc_token name;
  struct preproc_define_value value;
  enum preproc_define_flags flags;
};

static void preproc_create_builtin_macros(struct preproc *preproc,
                                          enum compile_target target) {
  // FIXME: vectors leak, vector should probably be arena-based

#define DEF_BUILTIN(tok_ty, n, v, f)                                           \
  do {                                                                         \
    size_t name_len = strlen((n));                                             \
    ustr_t ident = {.str = (n), .len = name_len};                    \
                                                                               \
    struct preproc_define define = {                                           \
        .name = {.ty = tok_ty,                                                 \
                 .span = MK_INVALID_TEXT_SPAN(0, name_len),                    \
                 .text = (n)},                                                 \
        .value =                                                               \
            {                                                                  \
                .ty = PREPROC_DEFINE_VALUE_TY_TOKEN,                           \
                .token = {.ty = tok_ty,                                        \
                          .span = MK_INVALID_TEXT_SPAN(0, strlen((v))),        \
                          .text = (v)},                                        \
            },                                                                 \
        .flags = f};                                                           \
                                                                               \
    hashtbl_insert(preproc->defines, &ident, &define);                         \
  } while (0)

#define DEF_BUILTIN_NUM(n, v)                                                  \
  DEF_BUILTIN(PREPROC_TOKEN_TY_PREPROC_NUMBER, (n), (v),                       \
              PREPROC_DEFINE_FLAG_NONE)
#define DEF_BUILTIN_IDENT(n, v)                                                \
  DEF_BUILTIN(PREPROC_TOKEN_TY_IDENTIFIER, (n), (v), PREPROC_DEFINE_FLAG_NONE)

#define DEF_BUILTIN_WELL_KNOWN(n)                                              \
  DEF_BUILTIN(PREPROC_TOKEN_TY_IDENTIFIER, (n), (n),                           \
              PREPROC_DEFINE_FLAG_WELL_KNOWN)

  // HACK: musl doesn't include `stdarg.h` so until we support
  // `__builtin_va_list`, just typedef it away
  // DEF_BUILTIN_IDENT("__builtin_va_list", "void *");

  DEF_BUILTIN_WELL_KNOWN("defined");
  DEF_BUILTIN_WELL_KNOWN("has_include");
  DEF_BUILTIN_WELL_KNOWN("__has_include");

  // TODO: implement these
  DEF_BUILTIN_WELL_KNOWN("has_embed");
  DEF_BUILTIN_WELL_KNOWN("__has_embed");
  DEF_BUILTIN_WELL_KNOWN("__has_feature");
  DEF_BUILTIN_WELL_KNOWN("__has_builtin");
  DEF_BUILTIN_WELL_KNOWN("__has_attribute");
  DEF_BUILTIN_WELL_KNOWN("__has_c_attribute");
  DEF_BUILTIN_WELL_KNOWN("__has_extension");

  // needed because clang headers do not properly avoid it
  // TODO: have more flexible solution to this (so we can ignore arbitary
  // __foo type macros)
  DEF_BUILTIN_WELL_KNOWN("__building_module");

  DEF_BUILTIN_IDENT("__extension__", "");

  DEF_BUILTIN_NUM("__FLT_MAX__", "3.40282347E+38");
  DEF_BUILTIN_NUM("__DBL_MAX__", "1.7976931348623157E+308");

  DEF_BUILTIN_NUM("__JCC__", "1");
  DEF_BUILTIN_NUM("__jcc__", "1");

  DEF_BUILTIN_NUM("__STDC__", "1");

  switch (preproc->args.c_standard) {
  case COMPILE_C_STANDARD_C11:
    DEF_BUILTIN_NUM("__STDC_VERSION__", "201112L");
    break;
  case COMPILE_C_STANDARD_C17:
    DEF_BUILTIN_NUM("__STDC_VERSION__", "201710L");
    break;
  case COMPILE_C_STANDARD_C23:
    DEF_BUILTIN_NUM("__STDC_VERSION__", "202311L");
    break;
  }

  // TODO: support different version targets. This is C11
  DEF_BUILTIN_NUM("__STDC_HOSTED__", "1");

  DEF_BUILTIN_NUM("__STDC_UTF_16__", "1");
  DEF_BUILTIN_NUM("__STDC_UTF_32__", "1");

  // C23 only
  DEF_BUILTIN_NUM("__STDC_EMBED_NOT_FOUND__", "0");
  DEF_BUILTIN_NUM("__STDC_EMBED_FOUND__", "1");
  DEF_BUILTIN_NUM("__STDC_EMBED_EMPTY__", "2");

  // C11
  DEF_BUILTIN_NUM("__STDC_NO_ATOMICS__", "1");
  DEF_BUILTIN_NUM("__STDC_NO_COMPLEX__", "1");
  DEF_BUILTIN_NUM("__STDC_NO_THREADS__", "1");
  DEF_BUILTIN_NUM("__STDC_NO_VLA__", "1");

  // currently all backends are little endian
  DEF_BUILTIN_NUM("__LITTLE_ENDIAN__", "1");

  switch (target) {
  case COMPILE_TARGET_MACOS_ARM64:
    // needed for <TargetConditionals.h>
    DEF_BUILTIN_NUM("TARGET_CPU_ARM64", "1");
    DEF_BUILTIN_NUM("TARGET_OS_MAC", "1");

    DEF_BUILTIN_NUM("__APPLE__", "1");
    DEF_BUILTIN_NUM("__aarch64__", "1");
    DEF_BUILTIN_NUM("__arm64__", "1");
    DEF_BUILTIN_NUM("__LP64__", "1");
    DEF_BUILTIN_NUM("_LP64", "1");
    break;
  case COMPILE_TARGET_MACOS_X86_64:
    // needed for <TargetConditionals.h>
    DEF_BUILTIN_NUM("TARGET_CPU_X86_64", "1");
    DEF_BUILTIN_NUM("TARGET_OS_MAC", "1");

    DEF_BUILTIN_NUM("__APPLE__", "1");
    DEF_BUILTIN_NUM("__x86_64__", "1");
    DEF_BUILTIN_NUM("__LP64__", "1");
    DEF_BUILTIN_NUM("_LP64", "1");
    break;
  case COMPILE_TARGET_LINUX_ARM64:
    DEF_BUILTIN_NUM("__linux__", "1");
    DEF_BUILTIN_NUM("__aarch64__", "1");
    DEF_BUILTIN_NUM("__arm64__", "1");
    DEF_BUILTIN_NUM("__LP64__", "1");
    DEF_BUILTIN_NUM("_LP64", "1");
    break;
  case COMPILE_TARGET_LINUX_X86_64:
    DEF_BUILTIN_NUM("__linux__", "1");
    DEF_BUILTIN_NUM("__x86_64__", "1");
    DEF_BUILTIN_NUM("__LP64__", "1");
    DEF_BUILTIN_NUM("_LP64", "1");
    break;
  case COMPILE_TARGET_LINUX_RV32I:
    DEF_BUILTIN_NUM("__linux__", "1");
    DEF_BUILTIN_NUM("__riscv", "1");
    DEF_BUILTIN_NUM("__riscv32", "1");
    DEF_BUILTIN_NUM("__riscv__", "1");
    DEF_BUILTIN_NUM("__ILP32__", "1");
    DEF_BUILTIN_NUM("_ILP32", "1");

    DEF_BUILTIN_IDENT("__INT32_TYPE__", "int");
    DEF_BUILTIN_IDENT("__INTPTR_TYPE__", "int");
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
  return (struct preproc_text){
      .text = text,
      .len = strlen(text),
      .pos = {.file = path, .col = 0, .line = 0, .idx = 0},
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

enum preproc_unexpanded_token_ty {
  PREPROC_UNEXPANDED_TOKEN_TY_BEGIN_EXPAND,
  PREPROC_UNEXPANDED_TOKEN_TY_END_EXPAND,
  PREPROC_UNEXPANDED_TOKEN_TY_TOKEN,
};

struct preproc_unexpanded_token {
  enum preproc_unexpanded_token_ty ty;

  union {
    ustr_t ident;
    struct preproc_token token;
  };
};

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

enum preproc_create_result
preproc_create(struct program program, struct fcache *fcache,
               struct preproc_create_args args,
               struct compiler_diagnostics *diagnostics,
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
    ustr_t k = {                                                     \
        .str = kw,                                                             \
        .len = strlen(kw),                                                     \
    };                                                                         \
    enum preproc_special_macro v = ty;                                         \
    hashtbl_insert(SPECIAL_MACROS, &k, &v);                                    \
  } while (0)

    SPECIAL_MACRO("__FILE__", PREPROC_SPECIAL_MACRO_FILE);
    SPECIAL_MACRO("__LINE__", PREPROC_SPECIAL_MACRO_LINE);
    SPECIAL_MACRO("__TIME__", PREPROC_SPECIAL_MACRO_TIME);
    SPECIAL_MACRO("__DATE__", PREPROC_SPECIAL_MACRO_DATE);
#undef SPECIAL_MACRO

    debug("built special macro table (len=%zu)", hashtbl_size(SPECIAL_MACROS));
  }

  info("beginning preproc stage");

  struct arena_allocator *arena;
  arena_allocator_create("preproc", &arena);

  struct preproc *p = nonnull_malloc(sizeof(*p));
  p->arena = arena;
  p->fcache = fcache;
  p->args = args;
  p->diagnostics = diagnostics;

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

  p->texts = vector_create_in_arena(sizeof(struct preproc_text), arena);

  struct preproc_text text = create_preproc_text(p, program.text, args.path);
  vector_push_back(p->texts, &text);

  p->line_has_nontrivial_token = false;
  p->in_angle_string_context = false;
  p->concat_next_token = false;
  p->keep_next_token = false;
  p->waiting_for_close = false;

  p->defines = hashtbl_create_sized_str_keyed_in_arena(
      p->arena, sizeof(struct preproc_define));

  // tokens that have appeared (e.g from a macro) and need to be processed next
  p->buffer_tokens =
      vector_create_in_arena(sizeof(struct preproc_token), arena);
  p->unexpanded_buffer_tokens =
      vector_create_in_arena(sizeof(struct preproc_unexpanded_token), arena);
  p->parents = hashtbl_create_sized_str_keyed_in_arena(p->arena, 0);

  *preproc = p;

  preproc_create_builtin_macros(p, args.target);

  for (size_t i = 0; i < args.num_defines; i++) {
    struct preproc_define_macro *define = &args.defines[i];

    // FIXME: properly tokenise
    if (!define->value.str || is_first_identifier_char(define->value.str[0])) {
      struct preproc_define def = {
          .name = {.ty = PREPROC_TOKEN_TY_IDENTIFIER,
                   .span = MK_INVALID_TEXT_SPAN(0, define->name.len),
                   .text = define->name.str},
          .value = {
              .ty = PREPROC_DEFINE_VALUE_TY_TOKEN,
              .token = {.ty = PREPROC_TOKEN_TY_IDENTIFIER,
                        .span = MK_INVALID_TEXT_SPAN(0, define->value.len),
                        .text = define->value.str},
          }};

      hashtbl_insert(p->defines, &define->name, &def);
    } else if (isdigit(define->value.str[0])) {
      struct preproc_define def = {
          .name = {.ty = PREPROC_TOKEN_TY_IDENTIFIER,
                   .span = MK_INVALID_TEXT_SPAN(0, define->name.len),
                   .text = define->name.str},
          .value = {
              .ty = PREPROC_DEFINE_VALUE_TY_TOKEN,
              .token = {.ty = PREPROC_TOKEN_TY_PREPROC_NUMBER,
                        .span = MK_INVALID_TEXT_SPAN(0, define->value.len),
                        .text = define->value.str},
          }};

      hashtbl_insert(p->defines, &define->name, &def);
    } else if (define->value.str[0] == '\"') {
      struct preproc_define def = {
          .name = {.ty = PREPROC_TOKEN_TY_IDENTIFIER,
                   .span = MK_INVALID_TEXT_SPAN(0, define->name.len),
                   .text = define->name.str},
          .value = {
              .ty = PREPROC_DEFINE_VALUE_TY_TOKEN,
              .token = {.ty = PREPROC_TOKEN_TY_STRING_LITERAL,
                        .span = MK_INVALID_TEXT_SPAN(0, define->value.len),
                        .text = define->value.str},
          }};

      hashtbl_insert(p->defines, &define->name, &def);
    } else {
      TODO("other -D tok types");
    }
  }

  return PREPROC_CREATE_RESULT_SUCCESS;
}

void preproc_free(struct preproc **preproc) {
  arena_allocator_free(&(*preproc)->arena);

  (*preproc)->arena = NULL;
  free(*preproc);

  *preproc = NULL;
}

static void find_multiline_comment_end_scalar(struct preproc_text *preproc_text,
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

// disabled as does not correctly handle newlines
#if USE_NEON && 0
void find_multiline_comment_end(struct preproc_text *preproc_text,
                                struct text_pos *cur_pos);

void find_multiline_comment_end(struct preproc_text *preproc_text,
                                struct text_pos *cur_pos) {
  const unsigned char *text =
      (const unsigned char *)preproc_text->text + cur_pos->idx;
  size_t len = preproc_text->len;

  uint8x16_t indices0 = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  uint8x16_t indices1 = {16, 17, 18, 19, 20, 21, 22, 23,
                         24, 25, 26, 27, 28, 29, 30, 31};

#define BLOCK_SZ 32

  size_t rem = len - cur_pos->idx;
  size_t nb = rem / BLOCK_SZ;

  uint8_t trail = 0;
  for (size_t i = 0; i < nb; i++) {
    uint8x16_t v0 = vld1q_u8(text);
    uint8x16_t v1 = vld1q_u8(text + 16);

    uint8x16_t eq_nl0 = vceqq_u8(v0, vdupq_n_u8('\n'));
    uint8x16_t eq_nl1 = vceqq_u8(v1, vdupq_n_u8('\n'));

    uint16x8_t widen0_0 = vpaddlq_u8(eq_nl0);
    uint32x4_t widen0_1 = vpaddlq_u16(widen0_0);
    uint64x2_t widen0_2 = vpaddlq_u32(widen0_1);

    uint16x8_t widen1_0 = vpaddlq_u8(eq_nl1);
    uint32x4_t widen1_1 = vpaddlq_u16(widen1_0);
    uint64x2_t widen1_2 = vpaddlq_u32(widen1_1);

    uint8x16_t eq_star0 = vceqq_u8(v0, vdupq_n_u8('*'));
    uint8x16_t eq_star1 = vceqq_u8(v1, vdupq_n_u8('*'));

    if (trail && vgetq_lane_u8(v0, 0) == '/') {
      cur_pos->idx += 1;
      return;
    }

    cur_pos->line += vgetq_lane_u64(widen0_2, 0) + vgetq_lane_u64(widen0_2, 1);

    if (vgetq_lane_u8(eq_star0, 15) && vgetq_lane_u8(v1, 0) == '/') {
      cur_pos->idx += 16 + 1;
      return;
    }

    cur_pos->line += vgetq_lane_u64(widen1_2, 0) + vgetq_lane_u64(widen1_2, 1);

    trail = vgetq_lane_u8(eq_star1, 15);

    uint8x16_t s0 = vextq_u8(v0, vdupq_n_u8(0), 1);
    uint8x16_t s1 = vextq_u8(v1, vdupq_n_u8(0), 1);

    uint8x16_t eq_slash0 = vceqq_u8(s0, vdupq_n_u8('/'));
    uint8x16_t eq_slash1 = vceqq_u8(s1, vdupq_n_u8('/'));

    uint8x16_t match0 = vandq_u8(eq_star0, eq_slash0);

    uint8x16_t match1 = vandq_u8(eq_star1, eq_slash1);

    uint8x16_t masked0 = vbslq_u8(match0, indices0, vdupq_n_u8(0xFF));
    uint8x16_t masked1 = vbslq_u8(match1, indices1, vdupq_n_u8(0xFF));

    uint8_t min0 = vminvq_u8(masked0);
    uint8_t min1 = vminvq_u8(masked1);

    if (min0 != 0xFF) {
      cur_pos->idx += min0 + 2;
      return;
    }

    if (min1 != 0xFF) {
      cur_pos->idx += min1 + 2;
      return;
    }

    text += BLOCK_SZ;
    cur_pos->idx += BLOCK_SZ;
  }

#undef BLOCK_SZ

  if (trail) {
    cur_pos->idx--;
  }

  find_multiline_comment_end_scalar(preproc_text, cur_pos);
}

#else
static void find_multiline_comment_end(struct preproc_text *preproc_text,
                                       struct text_pos *cur_pos) {
  find_multiline_comment_end_scalar(preproc_text, cur_pos);
}
#endif

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

void preproc_next_raw_token(struct preproc *preproc,
                            struct preproc_token *token) {
  struct preproc_text *preproc_text;

  while (vector_length(preproc->texts)) {
    preproc_text = vector_tail(preproc->texts);

    if (preproc_text->pos.idx < preproc_text->len ||
        vector_length(preproc->texts) == 1) {
      break;
    }

    // FIXME: why does this sometimes get hit?
    // DEBUG_ASSERT(vector_length(preproc_text->enabled) == 1,
    //              "text %s ended with enabled depth of %zu (should have been
    //              1)", preproc_text->file,
    //              vector_length(preproc_text->enabled));

    vector_pop(preproc->texts);
  }

  const char *text = preproc_text->text;

  while (vector_length(preproc->unexpanded_buffer_tokens)) {
    struct preproc_unexpanded_token *unexp =
        vector_pop(preproc->unexpanded_buffer_tokens);

    switch (unexp->ty) {
    case PREPROC_UNEXPANDED_TOKEN_TY_BEGIN_EXPAND:
      hashtbl_insert(preproc->parents, &unexp->ident, NULL);
      break;
    case PREPROC_UNEXPANDED_TOKEN_TY_END_EXPAND:
      hashtbl_remove(preproc->parents, &unexp->ident);
      break;
    case PREPROC_UNEXPANDED_TOKEN_TY_TOKEN:
      *token = unexp->token;
      return;
    }
  }

  struct text_pos start = preproc_text->pos;
  struct text_pos end = start;
  size_t len = preproc_text->len;

  if (start.idx >= preproc_text->len) {
    token->ty = PREPROC_TOKEN_TY_EOF;
    token->text = NULL;
    token->span.start = start;
    token->span.end = end;
    return;
  }

  while (end.idx + 1 < len && text[end.idx] == '\\' &&
         is_newline(text[end.idx + 1])) {
    // literally just skip this, don't even generate a token
    next_col(&end);
    next_line(&end);

    preproc_text->pos = end;

    preproc->line_has_nontrivial_token = false;

    start = preproc_text->pos;
    end = start;
  }

  if (end.idx < len && is_newline(text[end.idx])) {
    next_line(&end);

    preproc->line_has_nontrivial_token = false;
    preproc->in_angle_string_context = false;

    token->ty = PREPROC_TOKEN_TY_NEWLINE;
    token->text = &text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

  while (end.idx < len && is_whitespace(text[end.idx])) {
    next_col(&end);
  }

  if (start.idx != end.idx) {
    // we have processed whitespace

    token->ty = PREPROC_TOKEN_TY_WHITESPACE;
    token->text = &text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

  // we will find a token here that is not whitespace
  // save old value as it is needed for determining if a directive is valid
  bool line_has_nontrivial_token = preproc->line_has_nontrivial_token;
  preproc->line_has_nontrivial_token = true;

  char c = text[end.idx];
  char c2 = text[end.idx + 1];

  if (c == '/' && c2 == '/') {
    while (end.idx < len && !is_newline(text[end.idx])) {
      next_col(&end);
    }

    token->ty = PREPROC_TOKEN_TY_COMMENT;
    token->text = &text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

  if (c == '/' && c2 == '*') {
    next_col(&end);
    next_col(&end);
    find_multiline_comment_end(preproc_text, &end);

    token->ty = PREPROC_TOKEN_TY_COMMENT;
    token->text = &text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

  // FIXME: make sure there are excess null chars at end of string

  switch (c) {
  case 'L': {
    char next = text[end.idx + 1];

    if (next == '<' || next == '"' || next == '\'') {
      c = next;
      next_col(&end);
      goto string_literal;
    }

    // BUG: doesn't work on JCC
    // goto not_punctuator;

    while (end.idx < len && is_identifier_char(text[end.idx])) {
      next_col(&end);
    }

    token->ty = PREPROC_TOKEN_TY_IDENTIFIER;
    token->text = &text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }
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

    // BUG: doesn't work on JCC
    // for (size_t i = end.idx;
    //      i < preproc_text->len &&
    //      !(!char_escaped && text[i] == end_char);
    //      i++) {
    for (size_t i = end.idx;; i++) {
      if (!(i < len && !(!char_escaped && text[i] == end_char))) {
        break;
      }
      // next char is escaped if this char is a non-escaped backslash
      char_escaped = !char_escaped && text[i] == '\\';
      next_col(&end);
    }

    // skip final single-quote
    next_col(&end);

    token->ty = PREPROC_TOKEN_TY_STRING_LITERAL;
    token->text = &text[start.idx];
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

    token->text = &text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

  // we need to check for preproccessing number first as they can begin with `.`
  // and would be wrongly classed as punctuators
  if (isdigit(c) || (end.idx + 1 < len && c == '.' && isdigit(text[end.idx]))) {
    next_col(&end);

    while (end.idx < len) {
      char nc = text[end.idx];
      char sgn = text[end.idx + 1];

      if ((tolower(nc) == 'e' || tolower(nc) == 'p') &&
          (sgn == '+' || sgn == '-')) {
        // need to check if it is an exponent
        next_col(&end);
        next_col(&end);
      } else if (is_preproc_number_char(nc)) {
        next_col(&end);
      } else {
        token->ty = PREPROC_TOKEN_TY_PREPROC_NUMBER;
        token->text = &text[start.idx];
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
    if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_NEQ;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_NOT;
    }
    break;
  case '=':
    next_col(&end);
    if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_EQ;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ASSG;
    }
    break;
  case '&':
    next_col(&end);
    if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND_ASSG;
    } else if (c2 == '&') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_AND;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND;
    }
    break;
  case '|':
    next_col(&end);
    if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR_ASSG;
    } else if (c2 == '|') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_OR;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR;
    }
    break;
  case '^':
    next_col(&end);
    if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR;
    }
    break;
  case '+':
    next_col(&end);
    if (c2 == '+') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_INC;
    } else if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD;
    }
    break;
  case '-':
    next_col(&end);
    if (c2 == '-') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DEC;
    } else if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB_ASSG;
    } else if (c2 == '>') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_ARROW;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB;
    }
    break;
  case '*':
    next_col(&end);
    if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL;
    }
    break;
  case '/':
    next_col(&end);
    if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV;
    }

    break;
  case '%':
    next_col(&end);
    if (c2 == '=') {
      next_col(&end);
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_MOD_ASSG;
    } else {
      punc_ty = PREPROC_TOKEN_PUNCTUATOR_TY_OP_MOD;
    }
    break;

  default:
    goto not_punctuator;
  }

  *token = (struct preproc_token){.ty = PREPROC_TOKEN_TY_PUNCTUATOR,
                                  .text = &text[start.idx],
                                  .span = {.start = start, .end = end},
                                  .punctuator = {.ty = punc_ty}};

  preproc_text->pos = end;
  return;

not_punctuator: {
  size_t rem = len - end.idx;
  size_t read = 1;

  wchar_t wch;
  if ((unsigned char)c < 128) {
    next_col(&end);
  } else {
    read = mbtowc(&wch, &text[end.idx], rem);
    switch (read) {
    case 0: // null char??
    case (size_t)-1:
    case (size_t)-2:
      // invalid in some manner
      goto other;
    }

    for (size_t i = 0; i < read; i++) {
      next_col(&end);
    }
  }

  // TODO: this logic will consider unicode whitespace etc as part of identifier

  if (read > 1 || is_first_identifier_char(c)) {
    // all multi-byte chars accepted as identifier characters

    while (end.idx < len) {
      c = text[end.idx];
      if ((unsigned char)c < 128) {
        if (!is_identifier_char(c)) {
          break;
        }

        next_col(&end);
      } else {
        read = mbtowc(&wch, &text[end.idx], rem);
        switch (read) {
        case 0: // null char??
        case (size_t)-1:
        case (size_t)-2:
          // invalid in some manner
          goto other;
        }

        if (read == 1 && !is_identifier_char((char)wch)) {
          break;
        }

        for (size_t i = 0; i < read; i++) {
          next_col(&end);
        }
      }
    }

    token->ty = PREPROC_TOKEN_TY_IDENTIFIER;
    token->text = &text[start.idx];
    token->span = (struct text_span){.start = start, .end = end};

    preproc_text->pos = end;
    return;
  }

other:
  next_col(&end);
  token->ty = PREPROC_TOKEN_TY_OTHER;
  token->text = &text[start.idx];
  token->span = (struct text_span){.start = start, .end = end};

  preproc_text->pos = end;
}
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
                             enum preproc_expand_token_flags flags);

static void expand_token(struct preproc *preproc,
                         struct preproc_text *preproc_text,
                         struct preproc_token *token, struct vector *buffer,
                         enum preproc_expand_token_flags flags) {
  if (!try_expand_token(preproc, preproc_text, token, buffer, flags)) {
    vector_push_back(buffer, token);
  }
}

UNUSED static void
preproc_append_tokens(struct preproc *preproc,
                      struct preproc_text *preproc_text, struct vector *tokens,
                      struct vector *buffer,
                      enum preproc_expand_token_flags flags) {
  size_t num_tokens = vector_length(tokens);
  for (size_t i = num_tokens; i; i--) {
    struct preproc_token *def_tok = vector_get(tokens, i - 1);
    expand_token(preproc, preproc_text, def_tok, buffer, flags);
  }
}

static bool token_is_trivial(const struct preproc_token *token) {
  return token->ty == PREPROC_TOKEN_TY_WHITESPACE ||
         token->ty == PREPROC_TOKEN_TY_COMMENT;
}

static bool well_known_token(struct preproc *preproc, ustr_t ident);

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
  case TYPE_VAL(IDENTIFIER, STRING_LITERAL): {
    size_t llen = text_span_len(&token->span);
    size_t rlen = text_span_len(&last->span);

    if (llen && (llen > 1 || token->text[0] != 'L')) {
      BUG("can only concat L with string/char literal");
    }

    char *new = arena_alloc(preproc->arena, llen + rlen + 1);
    memcpy(new, token->text, llen);
    memcpy(new + llen, last->text, rlen);
    new[llen + rlen] = '\0';

    struct preproc_token new_tok = {
        .ty = PREPROC_TOKEN_TY_STRING_LITERAL,
        .span = MK_INVALID_TEXT_SPAN(0, llen + rlen),
        .text = new,
    };

    return new_tok;
  }
  case TYPE_VAL(IDENTIFIER, PUNCTUATOR):
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
    BUG("unsupported pair for concatenating tokens (line %zu) (%s ## %s)",
        token->span.start.line, preproc_token_name(token->ty),
        preproc_token_name(last->ty));
  }
}

static struct preproc_token preproc_stringify(struct preproc *preproc,
                                              struct vector *tokens);

static bool try_expand_token(struct preproc *preproc,
                             struct preproc_text *preproc_text,
                             struct preproc_token *token, struct vector *buffer,
                             enum preproc_expand_token_flags flags) {
  if (preproc->keep_next_token) {
    if (token_is_trivial(token)) {
      return true;
    } else if (token->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
               token->punctuator.ty ==
                   PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET) {
      // do we need to keep the whitespace/parens? we implicitly strip them
      // here
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
      compiler_diagnostics_add(
          preproc->diagnostics,
          MK_PREPROC_DIAGNOSTIC(
              BAD_TOKEN_IN_QUERY, bad_token_in_query, token->span,
              MK_INVALID_TEXT_POS(0),
              arena_alloc_snprintf(
                  preproc->arena, "bad token in preprocessor query '%.*s'",
                  (int)preproc->query_ident.len, preproc->query_ident.str)));
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
    // this should only be called by macros, so it is fine to rely on the
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
      expand_token(preproc, preproc_text, token, buffer, flags);
    } else {
      struct preproc_token new_tok = preproc_concat(preproc, token, last);

      // need to set _before_ recursive call into expand_token
      preproc->concat_next_token = false;
      expand_token(preproc, preproc_text, &new_tok, buffer, flags);
    }

    // HMMM: concat and recursion have weird behaviours, do we need to modify
    // parents?

    return true;
  }

  if (token->ty != PREPROC_TOKEN_TY_IDENTIFIER) {
    return false;
  }

  // if identifier is a macro do something else
  ustr_t ident = {.str = token->text,
                            .len = text_span_len(&token->span)};

  struct preproc_define *macro = hashtbl_lookup(preproc->defines, &ident);
  // well known macros arent expanded they just are "defined"
  if (macro && !(macro->flags & PREPROC_DEFINE_FLAG_WELL_KNOWN)) {
    PROFILE_CREATE_MULTI(macro_expand);
    PROFILE_BEGIN_MULTI(macro_expand);

    // TODO: lookup/insert pair can be made more efficient
    if (hashtbl_lookup(preproc->parents, &ident)) {
      // already seen this macro, do not expand it again
      PROFILE_END_MULTI(macro_expand);
      return false;
    }

    struct vector *expanded_fn =
        vector_create_in_arena(sizeof(struct preproc_token), preproc->arena);
    struct vector *concat_points =
        vector_create_in_arena(sizeof(size_t), preproc->arena);

    struct preproc_define_value *value = &macro->value;
    switch (value->ty) {
    case PREPROC_DEFINE_VALUE_TY_TOKEN:
      vector_push_back(expanded_fn, &value->token);
      break;
    case PREPROC_DEFINE_VALUE_TY_TOKEN_VEC: {
      size_t num_tokens = vector_length(value->vec);
      for (size_t i = num_tokens; i; i--) {
        struct preproc_token *def_tok = vector_get(value->vec, i - 1);

        if (def_tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
            def_tok->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_CONCAT) {
          size_t len = vector_length(expanded_fn);
          DEBUG_ASSERT(len > 0, "first tok was concat");
          // so we concat `len - 1` and `len`
          vector_push_back(concat_points, &len);

          continue;
        }

        vector_push_back(expanded_fn, def_tok);
      }
      break;
    }
    case PREPROC_DEFINE_VALUE_TY_MACRO_FN: {
      struct preproc_macro_fn macro_fn = value->macro_fn;

      struct vector *args =
          vector_create_in_arena(sizeof(struct vector *), preproc->arena);
      struct vector *arg =
          vector_create_in_arena(sizeof(struct preproc_token), preproc->arena);
      vector_push_back(args, &arg);

      enum preproc_expand_token_flags macro_expand_flags =
          flags & ~PREPROC_EXPAND_TOKEN_FLAG_UNDEF_ZERO;

      // first need to take the arguments
      struct preproc_token open;
      preproc_next_nontrivial_token(preproc, &open, macro_expand_flags);

      if (open.ty != PREPROC_TOKEN_TY_PUNCTUATOR ||
          open.punctuator.ty != PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET) {
        vector_push_back(buffer, &open);
        PROFILE_END_MULTI(macro_expand);
        return false;
      }

      int depth = 1;
      int brace_depth = 0;
      bool skip_trivial = true;
      bool seen_first_arg = false;
      while (true) {
        struct preproc_token next;
        preproc_next_token(preproc, &next, macro_expand_flags);

        if (next.ty == PREPROC_TOKEN_TY_EOF) {
          BUG("eof unexepctedly");
        }

        if (next.ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
            (next.punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACE ||
             next.punctuator.ty ==
                 PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_SQUARE_BRACKET)) {
          brace_depth++;

          vector_push_back(arg, &next);
          skip_trivial = false;
        } else if (next.ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                   (next.punctuator.ty ==
                        PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACE ||
                    next.punctuator.ty ==
                        PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_SQUARE_BRACKET)) {
          if (!brace_depth) {
            BUG("more close braces than valid");
          }

          brace_depth--;

          vector_push_back(arg, &next);
          skip_trivial = false;
        } else if (next.ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                   next.punctuator.ty ==
                       PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET) {
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
        } else if (depth == 1 && !brace_depth &&
                   next.ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                   next.punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_COMMA) {
          arg = vector_create_in_arena(sizeof(struct preproc_token),
                                       preproc->arena);
          vector_push_back(args, &arg);

          seen_first_arg = true;

          // strip leading whitespace
          skip_trivial = true;
        } else {
          if (!skip_trivial || (!token_is_trivial(&next) &&
                                next.ty != PREPROC_TOKEN_TY_NEWLINE)) {
            vector_push_back(arg, &next);
            skip_trivial = false;
          }
        }
      }

      size_t num_args = vector_length(args);
      if (!(vector_length(args) == macro_fn.num_params ||
            ((macro_fn.flags & PREPROC_MACRO_FN_FLAG_VARIADIC) &&
             vector_length(args) >= macro_fn.num_params))) {
        BUG("wrong number of args (%zu) for fn-like macro %.*s with %zu params",
            vector_length(args), (int)ident.len, ident.str,
            macro_fn.num_params);
      }

      size_t num_tokens = vector_length(macro_fn.tokens);

      if (!num_tokens) {
        struct preproc_token empty = {
            .ty = PREPROC_TOKEN_TY_IDENTIFIER,
            .span = MK_INVALID_TEXT_SPAN(0, 0),
            .text = "",
        };

        vector_push_back(expanded_fn, &empty);
      }

      for (size_t i = num_tokens; i; i--) {
        struct preproc_macro_fn_token *fn_tok =
            vector_get(macro_fn.tokens, i - 1);

        switch (fn_tok->ty) {
        case PREPROC_MACRO_FN_TOKEN_TY_VA_OPT: {
          if (!(macro_fn.flags & PREPROC_MACRO_FN_FLAG_VARIADIC)) {
            BUG("not variadic");
            break;
          }

          if (num_args > macro_fn.num_params) {
            vector_push_back(expanded_fn, &fn_tok->opt);
          }
          break;
        }
        case PREPROC_MACRO_FN_TOKEN_TY_PARAM: {
          struct vector *arg_tokens =
              *(struct vector **)vector_get(args, fn_tok->param);

          size_t num_arg_tokens = vector_length(arg_tokens);

          if (num_arg_tokens) {
            for (size_t k = num_arg_tokens; k; k--) {
              vector_push_back(expanded_fn, vector_get(arg_tokens, k - 1));
            }
          } else {
            struct preproc_token empty = {
                .ty = PREPROC_TOKEN_TY_IDENTIFIER,
                .span = MK_INVALID_TEXT_SPAN(0, 0),
                .text = "",
            };

            vector_push_back(expanded_fn, &empty);
          }

          break;
        }
        case PREPROC_MACRO_FN_TOKEN_TY_STRINGIFIED_PARAM: {
          struct vector *arg_tokens =
              *(struct vector **)vector_get(args, fn_tok->param);

          struct preproc_token str_tok = preproc_stringify(preproc, arg_tokens);

          vector_push_back(expanded_fn, &str_tok);
          break;
        }
        case PREPROC_MACRO_FN_TOKEN_TY_VA_ARGS: {
          if (!(macro_fn.flags & PREPROC_MACRO_FN_FLAG_VARIADIC)) {
            BUG("__VA_ARGS__ in non-variadic macro");
          }

          if (num_args <= macro_fn.num_params) {
            struct preproc_token empty = {.ty = PREPROC_TOKEN_TY_IDENTIFIER,
                                          .text = "",
                                          .span = MK_INVALID_TEXT_SPAN(0, 0)};

            vector_push_back(expanded_fn, &empty);
          } else {
            for (size_t j = num_args; j > macro_fn.num_params; j--) {
              struct vector *arg_tokens =
                  *(struct vector **)vector_get(args, j - 1);

              size_t num_arg_tokens = vector_length(arg_tokens);
              for (size_t k = num_arg_tokens; k; k--) {
                vector_push_back(expanded_fn, vector_get(arg_tokens, k - 1));
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
          }
          break;
        }
        case PREPROC_MACRO_FN_TOKEN_TY_STRINGIFIED_VA_ARGS: {
          if (!(macro_fn.flags & PREPROC_MACRO_FN_FLAG_VARIADIC)) {
            BUG("__VA_ARGS__ in non-variadic macro");
          }

          for (size_t j = num_args; j > macro_fn.num_params; j--) {
            struct vector *arg_tokens =
                *(struct vector **)vector_get(args, j - 1);

            struct preproc_token str_tok =
                preproc_stringify(preproc, arg_tokens);

            vector_push_back(expanded_fn, &str_tok);

            if (j - 1 != macro_fn.num_params) {
              struct preproc_token space = {.ty = PREPROC_TOKEN_TY_WHITESPACE,
                                            .text = " ",
                                            .span = MK_INVALID_TEXT_SPAN(0, 1)};

              vector_push_back(expanded_fn, &space);

              struct preproc_token comma = {
                  .ty = PREPROC_TOKEN_TY_PUNCTUATOR,
                  .punctuator = {.ty = PREPROC_TOKEN_PUNCTUATOR_TY_COMMA},
                  .text = ",",
                  .span = MK_INVALID_TEXT_SPAN(0, 1)};

              vector_push_back(expanded_fn, &comma);
            }
          }
          break;
        }
        case PREPROC_MACRO_FN_TOKEN_TY_TOKEN: {
          struct preproc_token *def_tok = &fn_tok->token;

          if (def_tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
              def_tok->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_CONCAT) {
            size_t len = vector_length(expanded_fn);
            // so we concat `len - 1` and `len`
            vector_push_back(concat_points, &len);

            continue;
          }

          vector_push_back(expanded_fn, def_tok);
          break;
        }
        }
      }

      break;
    }
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

    // reverse order
    struct preproc_unexpanded_token begin = {
        .ty = PREPROC_UNEXPANDED_TOKEN_TY_END_EXPAND, .ident = ident};
    vector_push_back(preproc->unexpanded_buffer_tokens, &begin);

    for (size_t i = 0; i < num_exp_tokens; i++) {
      struct preproc_token *tok = vector_get(expanded_fn, i);

      if (!tok->text) {
        continue;
      }

      struct preproc_unexpanded_token unexp_tok = {
          .ty = PREPROC_UNEXPANDED_TOKEN_TY_TOKEN, .token = *tok};

      vector_push_back(preproc->unexpanded_buffer_tokens, &unexp_tok);
    }

    struct preproc_unexpanded_token end = {
        .ty = PREPROC_UNEXPANDED_TOKEN_TY_BEGIN_EXPAND, .ident = ident};
    vector_push_back(preproc->unexpanded_buffer_tokens, &end);

    vector_free(&expanded_fn);
    vector_free(&concat_points);

    PROFILE_END_MULTI(macro_expand);
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

  if (well_known_token(preproc, ident)) {
    // mark to not expand symbols until we end this check
    preproc->keep_next_token = true;
    preproc->query_ident = ident;
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

  if (!strcmp(path, "stdbool.h")) {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: special header 'stdbool.h'\n");
    }

    const char *STDBOOL_CONTENT =
        "\n"
        "#ifndef STDBOOL_H\n"
        "#define STDBOOL_H\n"
        "\n"
        "#if !defined(__STDC_VERSION__) || __STDC_VERSION__ < 202311L\n"
        "#define bool _Bool\n"
        "#define true 1\n"
        "#define false 0\n"
        "#endif\n"
        "#endif\n";

    *content = STDBOOL_CONTENT;

    return true;
  } else if (!strcmp(path, "stdnoreturn.h")) {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: special header 'stdnoreturn.h'\n");
    }

    const char *STDNORETURN_CONTENT =
        "\n"
        "#ifndef STDNORETURN_H\n"
        "#define STDNORETURN_H\n"
        "\n"
        "#if !defined(__STDC_VERSION__) || __STDC_VERSION__ < 202311L\n"
        "#define noreturn _Noreturn\n" // TODO: _Bool
        "#endif\n"
        "#endif\n";

    *content = STDNORETURN_CONTENT;

    return true;
  } else if (!strcmp(path, "float.h")) {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: special header 'float.h'\n");
    }

    const char *FLOAT_CONTENT = "\n"
                                "#ifndef FLOAT_H\n"
                                "#define FLOAT_H\n"
                                "\n"
                                "#define FLT_MAX __FLT_MAX__\n"
                                "#define DBL_MAX __DBL_MAX__\n"
                                "#endif\n";

    *content = FLOAT_CONTENT;

    return true;
  } else if (!strcmp(path, "stdarg.h")) {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: special header 'starg.h'\n");
    }

    const char *STDARG_CONTENT =
        "\n"
        "#ifndef STDARG_H\n"
        "#define STDARG_H\n"
        "\n"
        // libc
        "#define __gnuc_va_list void *\n"
        "#ifdef __GLIBC__\n"
        "#endif\n"
        "\n"
        "#ifdef __need___va_list\n"
        // "typedef void * __gnuc_va_list;\n"
        "#define __GNUC_VA_LIST\n"
        "\n"
        "#else\n"
        "\n"
        // for some reason apple predefines it
        // "#if !defined(__APPLE__) && !defined(__MACH__)\n"
        "typedef void * va_list;\n"
        // "#endif\n"
        "\n"
        "#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202000L\n"
        "#define va_start(ap, ...) __builtin_va_start(ap, 0)\n"
        "#else\n"
        "#define va_start(ap, param) __builtin_va_start(ap, param)\n"
        "#endif\n"
        "\n"
        "#undef va_end\n"
        "#undef va_arg\n"
        "#undef va_copy\n"
        "#define va_end(ap) __builtin_va_end(ap)\n"
        "#define va_arg(ap, type) __builtin_va_arg(ap, type)\n"
        "#define va_copy(dest, src) __builtin_va_copy(dest, src)\n"
        // "typedef __builtin_va_list va_list;\n"
        // TEMP:
        "#define __builtin_va_arg(ap, type) ((type){0})\n"
        "inline static void *__builtin_va_start(...) { return (void *)0; }\n"
        "inline static void __builtin_va_end(...) { }\n"
        "#define __builtin_va_copy(dst, src) (dst) = (src)\n"

        "#endif\n"
        "#endif\n";

    *content = STDARG_CONTENT;

    return true;
  } else if (!strcmp(path, "stddef.h")) {
    if (preproc->args.verbose) {
      fprintf(stderr, "preproc: special header 'stddef.h'\n");
    }

    const char *STDDEF_CONTENT =
        "\n"
        "#ifndef STDDEF_H\n"
        "#define STDDEF_H\n"
        "\n"
        // "#ifdef __need_wint_t\n"
        "typedef int wint_t;\n"
        "typedef int wchar_t;\n"
        // "#endif\n"
        "\n"
        "#define NULL ((void*)0)\n"
        "typedef long ptrdiff_t;\n"
        "typedef unsigned long size_t;\n"
        // TODO: `max_align_t`
        // "typedef align max_align_t;\n"
        "\n"
        "#define offsetof(st, m) ((size_t)&(((st *)0)->m))\n"
        // "#define offsetof(st, m) __builtin_offsetof(st, m)\n"
        "\n"
        "#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202000L\n"
        "typedef __NULLPTR_TYPE__ nullptr_t;\n"
        "#define unreachable() __builtin_unreachable()\n"
        "#endif\n"
        "\n"
        "#endif\n";

    *content = STDDEF_CONTENT;

    return true;
  }

  if (preproc->args.verbose) {
    fprintf(stderr, "preproc: trying path '%s'\n", path);
  }

  bool found;

  switch (mode) {
  case TRY_FIND_INCLUDE_MODE_READ: {
    struct fcache_file file;
    found = fcache_read_path(preproc->fcache, MK_USTR(path), &file);
    // FIXME: does not respect `len`
    *content = file.data;
    break;
  }
  case TRY_FIND_INCLUDE_MODE_TEST:
    found = fcache_test_path(preproc->fcache, MK_USTR(path));
    break;
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

  if (!strcmp(filename, "stdarg.h") || !strcmp(filename, "stddef.h") ||
      !strcmp(filename, "stdbool.h") || !strcmp(filename, "stdnoreturn.h") ||
      !strcmp(filename, "float.h")) {
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
      preproc->in_angle_string_context = false;
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

    if (mode == PREPROC_TOKEN_MODE_NO_EXPAND ||
        !try_expand_token(preproc, preproc_text, &token, buffer, flags)) {
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
                                         ustr_t ident) {
  return hashtbl_lookup(preproc->defines, &ident);
}

// tokens that shouldn't be stripped from an `#if` expression or similar
static bool well_known_token(struct preproc *preproc, ustr_t ident) {
  // FIXME: from TargetConditionals
  // 'The long term solution is to add suppport for __is_target_arch and
  // __is_target_os'

  struct preproc_define *define = get_define(preproc, ident);

  return define && (define->flags & PREPROC_DEFINE_FLAG_WELL_KNOWN);
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
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_MOD:
    return 10;
  default:
    return -1;
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

static unsigned long long eval_has_query(struct preproc *preproc,
                                         struct preproc_text *preproc_text,
                                         struct preproc_token *token,
                                         struct preproc_token *value) {
  ustr_t token_str = {.str = token->text,
                                .len = text_span_len(&token->span)};
  token_str = ustr_strip_prefix(token_str, MK_USTR("__"));

  if (ustr_eq(token_str, MK_USTR("defined"))) {
    ustr_t value_str = {.str = value->text,
                                  .len = text_span_len(&value->span)};

    return get_define(preproc, value_str) ? 1 : 0;
  } else if (ustr_eq(token_str, MK_USTR("has_include"))) {
    struct include_path include_path = get_include_path(preproc, value);
    struct include_info include_info =
        try_find_include(preproc, preproc_text, include_path.filename,
                         include_path.is_angle, TRY_FIND_INCLUDE_MODE_TEST);

    return include_info.path != NULL;
  } else if (ustr_eq(token_str, MK_USTR("has_feature"))) {
    return 0;
  } else {
    // __has_attribute etc not implemented
    // warn("unknown identifier '%.*s' in preproc",
    //      (int)text_span_len(&token->span), token->text);
    return 0;
  }
}

static unsigned long long eval_atom(struct preproc *preproc,
                                    struct preproc_text *preproc_text,
                                    struct vector *tokens, size_t *i,
                                    size_t num_tokens) {
  for (; *i < num_tokens;) {
    struct preproc_token *token = vector_get(tokens, *i);

    switch (token->ty) {
    case PREPROC_TOKEN_TY_PREPROC_NUMBER: {
      (*i)++;

      unsigned long long num;
      if (!try_parse_integer(token->text, text_span_len(&token->span), &num)) {
        compiler_diagnostics_add(
            preproc->diagnostics,
            MK_PREPROC_DIAGNOSTIC(BAD_TOKEN_IN_COND, bad_token_in_cond,
                                  token->span, MK_INVALID_TEXT_POS(0),
                                  "bad token in preprocessor condition"));
        return 0;
      }

      return num;
    }
    case PREPROC_TOKEN_TY_STRING_LITERAL: {
      // FIXME: this is hacked logic, it needs to more generally parse chars
      // will break for

      (*i)++;

      size_t len = text_span_len(&token->span);
      switch (len) {
      case 3:
        return token->text[1];
      case 5:
        // assume `L'\0'` for test
        return 0;
      case 6:
        return strtoul(&token->text[2], NULL, 8);
      default:
        TODO("proper preproc char parse ('%.*s')", (int)len, token->text);
      }
    }
#define MAX_PREC 100
    case PREPROC_TOKEN_TY_PUNCTUATOR:
      switch (token->punctuator.ty) {
      case PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET: {
        (*i)++;
        return eval_expr(preproc, preproc_text, tokens, i, num_tokens, 0);
      }
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB: {
        (*i)++;
        unsigned long long val =
            eval_expr(preproc, preproc_text, tokens, i, num_tokens, MAX_PREC);
        return -val;
      }
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD: {
        (*i)++;
        unsigned long long val =
            eval_expr(preproc, preproc_text, tokens, i, num_tokens, MAX_PREC);
        return val;
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
        (*i)++;
        compiler_diagnostics_add(
            preproc->diagnostics,
            MK_PREPROC_DIAGNOSTIC(BAD_TOKEN_IN_COND, bad_token_in_cond,
                                  token->span, MK_INVALID_TEXT_POS(0),
                                  "bad token in preprocessor condition"));
        return 0;
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
        BUG("expected identifier after `defined/__has_feature` etc (found "
            "'%.*s')",
            (int)text_span_len(&next->span), next->text);
      }

      return eval_has_query(preproc, preproc_text, token, next);
    }
    case PREPROC_TOKEN_TY_WHITESPACE:
    case PREPROC_TOKEN_TY_COMMENT:
      (*i)++;
      continue;
    default:
      (*i)++;
      compiler_diagnostics_add(
          preproc->diagnostics,
          MK_PREPROC_DIAGNOSTIC(BAD_TOKEN_IN_COND, bad_token_in_cond,
                                token->span, MK_INVALID_TEXT_POS(0),
                                "bad token in preprocessor condition"));
      return 0;
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
      (*i)++;
      compiler_diagnostics_add(
          preproc->diagnostics,
          MK_PREPROC_DIAGNOSTIC(BAD_TOKEN_IN_COND, bad_token_in_cond,
                                token->span, MK_INVALID_TEXT_POS(0),
                                "bad token in preprocessor condition"));
      return 0;
    case PREPROC_TOKEN_TY_PUNCTUATOR: {
      if (token->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET) {
        return value;
      }

      if (token->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_COLON) {
        (*i)--;
        return value;
      }

      if (token->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_QMARK) {
        if (min_prec == MAX_PREC) {
          (*i)--;
          return value;
        }

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

      if (precedence == -1) {
        compiler_diagnostics_add(
            preproc->diagnostics,
            MK_PREPROC_DIAGNOSTIC(
                BAD_TOKEN_IN_COND, bad_token_in_cond, token->span,
                MK_INVALID_TEXT_POS(0),
                arena_alloc_snprintf(
                    preproc->arena, "bad token in preprocessor condition '%.*s'",
                    (int)text_span_len(&token->span), token->text)));

        return 0;
      }

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
        if (rhs == 0) {
          // TODO: emit diagnostic only if being executed
          // ie `0 && (0/0)` should not emit
          return 0;
        }
        value = value / rhs;
        break;
      case PREPROC_TOKEN_PUNCTUATOR_TY_OP_MOD:
        value = value % rhs;
        break;
      default:
        (*i)++;
        compiler_diagnostics_add(
            preproc->diagnostics,
            MK_PREPROC_DIAGNOSTIC(BAD_TOKEN_IN_COND, bad_token_in_cond,
                                  token->span, MK_INVALID_TEXT_POS(0),
                                  "bad token in preprocessor condition"));
        return 0;
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
          struct preproc_token *def_name = vector_head(directive_tokens);
          ustr_t def_str = {.str = def_name->text,
                                      .len =
                                          (int)text_span_len(&def_name->span)};
          now_enabled = get_define(preproc, def_str);
        }
        vector_push_back(preproc_text->enabled, &now_enabled);
        vector_push_back(preproc_text->enabled, &now_enabled);
        continue;
      } else if (token_streq(directive, "ifndef")) {
        bool now_enabled = false;
        if (enabled) {
          UNEXPANDED_DIR_TOKENS();
          struct preproc_token *def_name = vector_head(directive_tokens);
          ustr_t def_str = {.str = def_name->text,
                                      .len =
                                          (int)text_span_len(&def_name->span)};
          now_enabled = !get_define(preproc, def_str);
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
          struct preproc_token *def_name = vector_head(directive_tokens);
          ustr_t def_str = {.str = def_name->text,
                                      .len =
                                          (int)text_span_len(&def_name->span)};
          bool now_enabled = get_define(preproc, def_str);
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
          struct preproc_token *def_name = vector_head(directive_tokens);
          ustr_t def_str = {.str = def_name->text,
                                      .len =
                                          (int)text_span_len(&def_name->span)};
          bool now_enabled = !get_define(preproc, def_str);
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

          struct hashtbl *params = hashtbl_create_sized_str_keyed_in_arena(
              preproc->arena, sizeof(size_t));

          size_t param_idx = 0;

          while (true) {
            if (first_def_tok >= num_directive_tokens) {
              BUG("invalid macro fn, expected `)`");
            }

            tok = vector_get(directive_tokens, first_def_tok++);

            if (tok->ty == PREPROC_TOKEN_TY_IDENTIFIER) {
              ustr_t ident = {.len = text_span_len(&tok->span),
                                        .str = tok->text};

              hashtbl_insert(params, &ident, &param_idx);
              param_idx++;
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

          struct vector *tokens = vector_create_in_arena(
              sizeof(struct preproc_macro_fn_token), preproc->arena);

          bool stringify = false;
          for (size_t i = first_def_tok; i < num_directive_tokens; i++) {
            struct preproc_token *def_tok = vector_get(directive_tokens, i);

            struct preproc_macro_fn_token fn_tok;

            if (def_tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                def_tok->punctuator.ty ==
                    PREPROC_TOKEN_PUNCTUATOR_TY_STRINGIFY) {
              stringify = true;
              continue;
            }

            if (def_tok->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                def_tok->punctuator.ty == PREPROC_TOKEN_PUNCTUATOR_TY_CONCAT) {
              // look for `, ## __VA_ARGS__`
              if (i - first_def_tok >= 1) {
                size_t pos = i - 1;

                size_t num_prev = 0;
                struct preproc_token *prev = NULL;
                while (pos > first_def_tok) {
                  prev = vector_get(directive_tokens, pos);

                  pos--;
                  num_prev++;

                  if (!token_is_trivial(prev)) {
                    break;
                  }
                }

                pos = i + 1;

                struct preproc_token *succ = NULL;
                while (pos < num_directive_tokens) {
                  succ = vector_get(directive_tokens, pos);

                  pos++;

                  if (!token_is_trivial(succ)) {
                    break;
                  }
                }

                if ((prev && prev->ty == PREPROC_TOKEN_TY_PUNCTUATOR &&
                     prev->punctuator.ty ==
                         PREPROC_TOKEN_PUNCTUATOR_TY_COMMA) &&
                    (succ &&
                     text_span_len(&succ->span) == strlen("__VA_ARGS__") &&
                     !strncmp(succ->text, "__VA_ARGS__",
                              strlen("__VA_ARGS__")))) {

                  // need to remove the preceding tokens
                  for (size_t j = 0; j < num_prev; j++) {
                    vector_pop(tokens);
                  }

                  fn_tok = (struct preproc_macro_fn_token){
                      .ty = PREPROC_MACRO_FN_TOKEN_TY_VA_OPT, .opt = *prev};

                  vector_push_back(tokens, &fn_tok);

                  continue;
                }
              }
            }

            if (def_tok->ty == PREPROC_TOKEN_TY_IDENTIFIER) {
              ustr_t ident = {.len = text_span_len(&def_tok->span),
                                        .str = def_tok->text};

              size_t *idx = hashtbl_lookup(params, &ident);
              if (idx) {
                fn_tok = (struct preproc_macro_fn_token){
                    .ty = stringify
                              ? PREPROC_MACRO_FN_TOKEN_TY_STRINGIFIED_PARAM
                              : PREPROC_MACRO_FN_TOKEN_TY_PARAM,
                    .param = *idx};

                stringify = false;
              } else if (text_span_len(&def_tok->span) ==
                             strlen("__VA_ARGS__") &&
                         !strncmp(def_tok->text, "__VA_ARGS__",
                                  strlen("__VA_ARGS__"))) {
                fn_tok = (struct preproc_macro_fn_token){
                    .ty = stringify
                              ? PREPROC_MACRO_FN_TOKEN_TY_STRINGIFIED_VA_ARGS
                              : PREPROC_MACRO_FN_TOKEN_TY_VA_ARGS,
                };

                stringify = false;
              } else {
                fn_tok = (struct preproc_macro_fn_token){
                    .ty = PREPROC_MACRO_FN_TOKEN_TY_TOKEN, .token = *def_tok};
              }
            } else {
              fn_tok = (struct preproc_macro_fn_token){
                  .ty = PREPROC_MACRO_FN_TOKEN_TY_TOKEN, .token = *def_tok};
            }

            vector_push_back(tokens, &fn_tok);
          }

          define.value = (struct preproc_define_value){
              .ty = PREPROC_DEFINE_VALUE_TY_MACRO_FN,
              .macro_fn = {.num_params = hashtbl_size(params),
                           .tokens = tokens,
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

        ustr_t ident = {.str = def_name.text,
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

        ustr_t ident = {.str = def_name.text,
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
      } else if (token_streq(directive, "warning") ||
                 token_streq(directive, "error")) {
        EXPANDED_DIR_TOKENS();

        bool is_warn = token_streq(directive, "warning");

        bool wrote = false;

        struct vector *buf =
            vector_create_in_arena(sizeof(char), preproc->arena);

        for (size_t i = 0; i < num_directive_tokens; i++) {
          struct preproc_token *warn_token = vector_get(directive_tokens, i);

          // TODO: only do this in system header files
          if (is_warn &&
              (num_directive_tokens == 1 &&
               !strncmp(warn_token->text, "\"Unsupported compiler detected\"",
                        strlen("\"Unsupported compiler detected\"")))) {
            // ignore
            break;
          }

          wrote = true;
          vector_extend(buf, warn_token->text,
                        text_span_len(&warn_token->span));
        }

        if (!wrote) {
          continue;
        }

        struct text_pos end;
        if (num_directive_tokens) {
          end =
              ((struct preproc_token *)vector_tail(directive_tokens))->span.end;
        } else {
          end = token->span.end;
        }

        const char *msg;

        if (vector_empty(buf)) {
          msg = "empty '#warning' directive";
        } else {
          char nll = 0;
          vector_push_back(buf, &nll);
          msg = vector_head(buf);
        }

        if (is_warn) {
          compiler_diagnostics_add(
              preproc->diagnostics,
              MK_PREPROC_DIAGNOSTIC(WARN_DIRECTIVE, warn_directive,
                                    MK_TEXT_SPAN(token->span.start, end),
                                    MK_INVALID_TEXT_POS(0), msg));
        } else {
          compiler_diagnostics_add(
              preproc->diagnostics,
              MK_PREPROC_DIAGNOSTIC(ERROR_DIRECTIVE, error_directive,
                                    MK_TEXT_SPAN(token->span.start, end),
                                    MK_INVALID_TEXT_POS(0), msg));
        }

        if (wrote) {
          slogsl("\n");
        }
      } else {
        UNEXPANDED_DIR_TOKENS();
        // TODO("other directives ('%.*s')",
        // (int)text_span_len(&directive.span), directive.text);
      }

#undef UNEXPANDED_DIR_TOKENS
#undef EXPANDED_DIR_TOKENS

      continue;
    }

    if (mode == PREPROC_TOKEN_MODE_EXPAND &&
        try_expand_token(preproc, preproc_text, token, preproc->buffer_tokens,
                         flags)) {
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

#define ADD_ESCAPED(esc, c)                                                    \
  case esc: {                                                                  \
    char v = c;                                                                \
    vector_push_back(buf, &slash);                                             \
    vector_push_back(buf, &v);                                                 \
    break;                                                                     \
  }

    switch (ch) {
      ADD_ESCAPED('\0', '0')
      ADD_ESCAPED('\a', 'a')
      ADD_ESCAPED('\b', 'b')
      ADD_ESCAPED('\f', 'f')
      ADD_ESCAPED('\n', 'n')
      ADD_ESCAPED('\r', 'r')
      ADD_ESCAPED('\t', 't')
      ADD_ESCAPED('\v', 'v')
      ADD_ESCAPED('\\', '\\')
      // hmm, we only want to add this in the right type of string...
      ADD_ESCAPED('\'', '\'')
      ADD_ESCAPED('"', '"')
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

#define ADD_STR(str, len) add_escaped_str(buf, str, len)

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

  bool first = true;
  bool last_was_newline = true;
  bool last_was_whitespace = true;

  do {
    preproc_next_token(preproc, &token, PREPROC_EXPAND_TOKEN_FLAG_NONE);

    int len = (int)text_span_len(&token.span);

    if (!len) {
      continue;
    }

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
      fprintf(file, "%.*s", len, token.text);
      break;
    case PREPROC_TOKEN_TY_WHITESPACE:
      if (first) {
        continue;
      }

      if (last_was_newline) {
        fprintf(file, "%.*s", len, token.text);
      } else if (!last_was_whitespace) {
        fprintf(file, " ");
      }

      last_was_newline = false;
      last_was_whitespace = true;
      continue;
    case PREPROC_TOKEN_TY_NEWLINE:
      if (first) {
        continue;
      }

      if (last_was_newline) {
        continue;
      }

      last_was_newline = true;

      fprintf(file, "\n");
      continue;
    case PREPROC_TOKEN_TY_COMMENT:
      if (last_was_whitespace || last_was_newline) {
        last_was_whitespace = true;
        continue;
      }

      last_was_newline = false;
      last_was_whitespace = true;

      fprintf(file, " ");
      continue;
    }

    first = false;
    last_was_newline = false;
    last_was_whitespace = false;
  } while (token.ty != PREPROC_TOKEN_TY_EOF);
}
