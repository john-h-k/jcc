#include "lex.h"

#include "alloc.h"
#include "hashtbl.h"
#include "log.h"
#include "preproc.h"
#include "program.h"
#include "util.h"
#include "vector.h"

struct lexer {
  struct arena_allocator *arena;

  struct program *program;
  struct preproc *preproc;

  // once we generate a token, we put it here
  struct vector *tokens;
  size_t pos;
  struct text_pos last_text_pos;

  struct text_pos text_pos;

  const char **associated_texts;
};

static struct hashtbl *KEYWORDS = NULL;

enum lex_create_result lexer_create(struct program *program,
                                    struct preproc *preproc,
                                    struct lexer **lexer) {
  if (!KEYWORDS) {
    debug("building kw table");

    KEYWORDS = hashtbl_create_sized_str_keyed(sizeof(enum lex_token_ty));

#define KEYWORD(kw, ty)                                                        \
  do {                                                                         \
    struct sized_str k = {                                                     \
        .str = kw,                                                             \
        .len = strlen(kw),                                                     \
    };                                                                         \
    enum lex_token_ty v = ty;                                                  \
    hashtbl_insert(KEYWORDS, &k, &v);                                          \
  } while (0);

    KEYWORD("__attribute__", LEX_TOKEN_TY_KW_ATTRIBUTE);

    // We falsely reserve some keywords here i believe (e.g banning `align` in
    // C11)

    KEYWORD("_Generic", LEX_TOKEN_TY_KW_GENERIC);
    KEYWORD("_Static_assert", LEX_TOKEN_TY_KW_STATICASSERT);
    KEYWORD("static_assert", LEX_TOKEN_TY_KW_STATICASSERT);
    KEYWORD("_Noreturn", LEX_TOKEN_TY_KW_NORETURN);
    KEYWORD("goto", LEX_TOKEN_TY_KW_GOTO);
    KEYWORD("break", LEX_TOKEN_TY_KW_BREAK);
    KEYWORD("continue", LEX_TOKEN_TY_KW_CONTINUE);
    KEYWORD("do", LEX_TOKEN_TY_KW_DO);
    KEYWORD("for", LEX_TOKEN_TY_KW_FOR);
    KEYWORD("while", LEX_TOKEN_TY_KW_WHILE);
    KEYWORD("switch", LEX_TOKEN_TY_KW_SWITCH);
    KEYWORD("default", LEX_TOKEN_TY_KW_DEFAULT);
    KEYWORD("case", LEX_TOKEN_TY_KW_CASE);
    KEYWORD("if", LEX_TOKEN_TY_KW_IF);
    KEYWORD("else", LEX_TOKEN_TY_KW_ELSE);
    KEYWORD("return", LEX_TOKEN_TY_KW_RETURN);

    KEYWORD("typedef", LEX_TOKEN_TY_KW_TYPEDEF);
    KEYWORD("static", LEX_TOKEN_TY_KW_STATIC);
    KEYWORD("auto", LEX_TOKEN_TY_KW_AUTO);
    KEYWORD("extern", LEX_TOKEN_TY_KW_EXTERN);
    KEYWORD("register", LEX_TOKEN_TY_KW_REGISTER);

    KEYWORD("inline", LEX_TOKEN_TY_KW_INLINE);

    KEYWORD("const", LEX_TOKEN_TY_KW_CONST);
    KEYWORD("volatile", LEX_TOKEN_TY_KW_VOLATILE);
    KEYWORD("restrict", LEX_TOKEN_TY_KW_RESTRICT);

    // TODO: indicate non standard to user
    KEYWORD("__restrict", LEX_TOKEN_TY_KW_RESTRICT);
    KEYWORD("__inline", LEX_TOKEN_TY_KW_INLINE);

    KEYWORD("void", LEX_TOKEN_TY_KW_VOID);

    KEYWORD("_Bool", LEX_TOKEN_TY_KW_BOOL);
    // TODO: only allow for C23 (similar with alignof)
    KEYWORD("bool", LEX_TOKEN_TY_KW_BOOL);

    // required by macOS
    KEYWORD("__uint128_t", LEX_TOKEN_TY_KW_UINT128);

    KEYWORD("__fp16", LEX_TOKEN_TY_KW_HALF);
    KEYWORD("_Float16", LEX_TOKEN_TY_KW_HALF);
    KEYWORD("float", LEX_TOKEN_TY_KW_FLOAT);
    KEYWORD("double", LEX_TOKEN_TY_KW_DOUBLE);
    KEYWORD("char", LEX_TOKEN_TY_KW_CHAR);
    KEYWORD("short", LEX_TOKEN_TY_KW_SHORT);
    KEYWORD("int", LEX_TOKEN_TY_KW_INT);
    KEYWORD("long", LEX_TOKEN_TY_KW_LONG);
    KEYWORD("unsigned", LEX_TOKEN_TY_KW_UNSIGNED);
    KEYWORD("signed", LEX_TOKEN_TY_KW_SIGNED);
    KEYWORD("enum", LEX_TOKEN_TY_KW_ENUM);
    KEYWORD("struct", LEX_TOKEN_TY_KW_STRUCT);
    KEYWORD("union", LEX_TOKEN_TY_KW_UNION);

    KEYWORD("sizeof", LEX_TOKEN_TY_KW_SIZEOF);
    KEYWORD("alignof", LEX_TOKEN_TY_KW_ALIGNOF);
    KEYWORD("_Alignof", LEX_TOKEN_TY_KW_ALIGNOF);
    KEYWORD("alignas", LEX_TOKEN_TY_KW_ALIGNAS);
    KEYWORD("_Alignas", LEX_TOKEN_TY_KW_ALIGNAS);

#undef KEYWORD

    debug("built kw table (len=%zu)", hashtbl_size(KEYWORDS));
  }

  info("beginning lex stage");

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct lexer *l = nonnull_malloc(sizeof(*l));
  l->arena = arena;

  l->program = program;
  l->preproc = preproc;

  l->tokens = vector_create_in_arena(sizeof(struct lex_token), arena);
  l->pos = 0;
  l->last_text_pos = (struct text_pos){0};

  *lexer = l;

  return LEX_CREATE_RESULT_SUCCESS;
}

void lexer_free(struct lexer **lexer) {
  vector_free(&(*lexer)->tokens);

  arena_allocator_free(&(*lexer)->arena);

  (*lexer)->arena = NULL;
  free(*lexer);

  *lexer = NULL;
}

/* The lexer parses identifiers, but these could be identifiers, typedef-names,
   or keywords. This function converts identifiers into their "real" type */
static enum lex_token_ty refine_ty(struct preproc_token *token) {
  struct keyword {
    const char *str;
    size_t len;
    enum lex_token_ty ty;
  };

  DEBUG_ASSERT(KEYWORDS, "keywords should have been built");

  struct sized_str key = {.str = token->text,
                          .len = text_span_len(&token->span)};

  enum lex_token_ty *kw_ty = hashtbl_lookup(KEYWORDS, &key);

  if (kw_ty) {
    return *kw_ty;
  }

  return LEX_TOKEN_TY_IDENTIFIER;
}

static struct sized_str process_raw_string(const struct lexer *lexer,
                                           const struct lex_token *token) {
  // TODO: this i think will wrongly accept multilines
  // FIXME: definitely wrong for wide strings

  const char *text = token->text;
  size_t len = text_span_len(&token->span);

  bool is_wide = token->ty == LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL ||
                 token->ty == LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL;
  struct vector *buff = vector_create_in_arena(
      is_wide ? sizeof(int32_t) : sizeof(char), lexer->arena);

  char end_char = (token->ty == LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL ||
                   token->ty == LEX_TOKEN_TY_ASCII_CHAR_LITERAL)
                      ? '\''
                      : '"';
  size_t str_start = (token->ty == LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL ||
                      token->ty == LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL)
                         ? 2
                         : 1;

  size_t str_len = 0;
  bool char_escaped = false;

  // BUG: JCC fails with this
  // for (size_t i = str_start; i < len && !(!char_escaped && text[i] ==
  // end_char);
  //      i++) {
  for (size_t i = str_start;; i++) {
    if (!(i < len && !(!char_escaped && text[i] == end_char))) {
      break;
    }

    if (is_wide) {
      if ((text[i] & 0xC0) == 0xC0) {
        int32_t ch = ((text[i] & 0x1F) << 6) | (text[i + 1] & 0x3F);
        vector_push_back(buff, &ch);
        i += 1;
        continue;
      } else if ((text[i] & 0xE0) == 0xE0) {
        int32_t ch = ((text[i] & 0x0F) << 12) | ((text[i + 1] & 0x3F) << 6) |
                     (text[i + 2] & 0x3F);
        vector_push_back(buff, &ch);
        i += 2;
        continue;
      } else if ((text[i] & 0xF0) == 0xF0) {
        int32_t ch = ((text[i] & 0x07) << 18) | ((text[i + 1] & 0x3F) << 12) |
                     ((text[i + 2] & 0x3F) << 6) | (text[i + 3] & 0x3F);
        vector_push_back(buff, &ch);
        i += 3;
        continue;
      }
    }
    if (char_escaped) {
#define PUSH_CHAR(ch)                                                          \
  if (is_wide) {                                                               \
    int32_t pc = (uint32_t)(unsigned char)ch;                                  \
    vector_push_back(buff, &pc);                                               \
  } else {                                                                     \
    char pc = (char)ch;                                                        \
    vector_push_back(buff, &pc);                                               \
  }
#define ADD_ESCAPED(ch, esc)                                                   \
  case ch: {                                                                   \
    PUSH_CHAR(esc);                                                            \
    break;                                                                     \
  }
      if (text[i] >= '0' && text[i] <= '7') {
        size_t octal_start = i;

        while (i < token->span.end.idx) {
          if (text[i] >= '0' && text[i] <= '7') {
            i++;
          } else {
            break;
          }
        }

        // because loop increments it
        i--;

        size_t octal_len = MIN(3, i - octal_start + 1);
        char oct_buff[4] = {0};
        for (size_t j = 0; j < octal_len; j++) {
          oct_buff[j] = text[octal_start + j];
        }

        unsigned char value = (unsigned char)strtoul(oct_buff, NULL, 8);
        PUSH_CHAR(value);
      } else if (text[i] == 'u') {
        // FIXME: C23 allows arbitrary num digits, not just 4
        char u_buff[5] = {0};
        memcpy(u_buff, &text[i + 1], 4);
        i += 4;

        unsigned long codepoint = strtoul(u_buff, NULL, 16);

        if (is_wide) {
          PUSH_CHAR(codepoint);
        } else if (codepoint <= 0x7F) {
          char c = codepoint & 0x7F;
          PUSH_CHAR(c);
        } else if (codepoint <= 0x7FF) {
          char c[2] = {(char)(0xC0 | ((codepoint >> 6) & 0x1F)),
                       (char)(0x80 | (codepoint & 0x3F))};
          vector_extend(buff, c, 2);
        } else if (codepoint <= 0xFFFF) {
          char c[3] = {(char)(0xE0 | ((codepoint >> 12) & 0x0F)),
                       (char)(0x80 | ((codepoint >> 6) & 0x3F)),
                       (char)(0x80 | (codepoint & 0x3F))};
          vector_extend(buff, c, 3);
        } else if (codepoint <= 0x10FFFF) {
          char c[4] = {(char)(0xF0 | ((codepoint >> 18) & 0x07)),
                       (char)(0x80 | ((codepoint >> 12) & 0x3F)),
                       (char)(0x80 | ((codepoint >> 6) & 0x3F)),
                       (char)(0x80 | (codepoint & 0x3F))};
          vector_extend(buff, c, 4);
        }
      } else if (text[i] == 'x') {
        char x_buff[3] = {0};
        memcpy(x_buff, &text[i + 1], 2);
        i += 2;

        unsigned char value = (unsigned char)strtoul(x_buff, NULL, 16);
        PUSH_CHAR(value);
      } else {
        switch (text[i]) {
          ADD_ESCAPED('0', '\0')
          ADD_ESCAPED('a', '\a')
          ADD_ESCAPED('b', '\b')
          // non-standard so not included for now
          // ADD_ESCAPED('e', '\e')
          ADD_ESCAPED('f', '\f')
          ADD_ESCAPED('n', '\n')
          ADD_ESCAPED('r', '\r')
          ADD_ESCAPED('t', '\t')
          ADD_ESCAPED('v', '\v')
          ADD_ESCAPED('\\', '\\')
          ADD_ESCAPED('\'', '\'')
          ADD_ESCAPED('"', '"')
          ADD_ESCAPED('?', '\?')
        default:
          TODO("\\x \\u \\U and \\octal escapes");
          // either octal escape, or invalid
        }
      }

#undef ADD_ESCAPED
    } else if (text[i] != '\\') {
      PUSH_CHAR(text[i]);
    }

    // next char is escaped if this char is a non-escaped backslash
    char_escaped = !char_escaped && text[i] == '\\';
  }

  str_len = vector_byte_size(buff);

  PUSH_CHAR(0);

  return (struct sized_str){.str = vector_head(buff), .len = str_len};
}

bool lexer_at_eof(struct lexer *lexer) {
  // needed to skip whitespace
  struct lex_token token;
  lex_peek_token(lexer, &token);

  return token.ty == LEX_TOKEN_TY_EOF;
}

static enum lex_token_ty
preproc_punctuator_to_lex_token_ty(enum preproc_token_punctuator_ty ty);

static enum lex_token_ty
lex_string_literal(const struct preproc_token *preproc_token) {
  DEBUG_ASSERT(preproc_token->ty == PREPROC_TOKEN_TY_STRING_LITERAL,
               "wrong preproc token ty");

  switch (preproc_token->text[0]) {
  case '<':
    BUG("found angle-bracket string literal in lexer");
  case '\"':
    return LEX_TOKEN_TY_ASCII_STR_LITERAL;
  case '\'':
    return LEX_TOKEN_TY_ASCII_CHAR_LITERAL;
  case 'L':
    switch (preproc_token->text[1]) {
    case '\"':
      return LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL;
    case '\'':
      return LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL;
    }
  default:
    TODO("other string/char literal types");
  }
}

static enum lex_token_ty
lex_number_literal(const struct preproc_token *preproc_token) {
  DEBUG_ASSERT(preproc_token->ty == PREPROC_TOKEN_TY_PREPROC_NUMBER,
               "wrong preproc token ty");

  enum FLAG_ENUM lit_ty {
    LIT_TY_NONE = 0,
    LIT_TY_FLT = 1,
    LIT_TY_U = 2,
    LIT_TY_L = 4,
    LIT_TY_LL = 8,
    LIT_TY_Z = 16,
    LIT_TY_F = 32,
  };

  struct text_pos start = preproc_token->span.start;
  struct text_pos end = preproc_token->span.end;

  const char *text = preproc_token->text;
  size_t len = text_pos_len(start, end);

  enum lit_ty lit_ty = LIT_TY_NONE;

  bool is_hex = len >= 2 && (text[0] == '0' && text[1] == 'x');

  size_t end_idx = len;

  while (end_idx) {
    char c = text[end_idx];

    switch (c) {
    case 'l':
    case 'L':
      if (end_idx && (text[end_idx - 1] == 'l' || text[end_idx - 1] == 'L')) {
        end_idx--;
        lit_ty |= LIT_TY_LL;
      } else {
        lit_ty |= LIT_TY_L;
      }
      break;
    case 'f':
    case 'F':
      if (!is_hex) {
        lit_ty |= LIT_TY_F;
      }
      break;
    case 'u':
    case 'U':
      lit_ty |= LIT_TY_U;
      break;
    case 'z':
    case 'Z':
      lit_ty |= LIT_TY_Z;
      break;
    }

    end_idx--;
  }

  if ((memchr(text, '.', len)) ||
      (!is_hex && (memchr(text, 'e', len) || memchr(text, 'E', len))) ||
      (is_hex && (memchr(text, 'p', len) || memchr(text, 'P', len)))) {
    lit_ty |= LIT_TY_FLT;
  }

  enum lex_token_ty ty;
  switch (lit_ty) {
  // Integer
  case LIT_TY_NONE:
    ty = LEX_TOKEN_TY_SIGNED_INT_LITERAL;
    break;
  case LIT_TY_U:
    ty = LEX_TOKEN_TY_UNSIGNED_INT_LITERAL;
    break;
  case LIT_TY_L:
    ty = LEX_TOKEN_TY_SIGNED_LONG_LITERAL;
    break;
  case LIT_TY_LL:
    ty = LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL;
    break;
  case LIT_TY_Z:
    TODO("Z-sized literals");
  case LIT_TY_U | LIT_TY_L:
    ty = LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL;
    break;
  case LIT_TY_U | LIT_TY_LL:
    ty = LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL;
    break;
  case LIT_TY_U | LIT_TY_Z:
    TODO("Z-sized literals");

  // Floating-point
  case LIT_TY_FLT:
    ty = LEX_TOKEN_TY_DOUBLE_LITERAL;
    break;
  case LIT_TY_F:
  case LIT_TY_FLT | LIT_TY_F:
    ty = LEX_TOKEN_TY_FLOAT_LITERAL;
    break;
  case LIT_TY_FLT | LIT_TY_L:
    ty = LEX_TOKEN_TY_LONG_DOUBLE_LITERAL;
    break;

  default:
    // return an arbitrary type, but try and respect is_flt so that parser can
    // generate a more accurate diagnostic
    return (lit_ty & LIT_TY_FLT) ? LEX_TOKEN_TY_DOUBLE_LITERAL
                                 : LEX_TOKEN_TY_SIGNED_INT_LITERAL;
  }

  return ty;
}

static void lex_next_token(struct lexer *lexer, struct lex_token *token) {
  while (true) {
    // need to loop as lexer discards certain tokens (whitespace, comments, etc)

    struct preproc_token preproc_token;
    do {
      preproc_next_token(lexer->preproc, &preproc_token,
                         PREPROC_EXPAND_TOKEN_FLAG_NONE);
    } while (preproc_token.ty != PREPROC_TOKEN_TY_EOF &&
             !text_span_len(&preproc_token.span));

    switch (preproc_token.ty) {
    case PREPROC_TOKEN_TY_UNKNOWN:
      *token = (struct lex_token){.ty = LEX_TOKEN_TY_UNKNOWN,
                                  .text = preproc_token.text,
                                  .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_EOF:
      *token = (struct lex_token){.ty = LEX_TOKEN_TY_EOF,
                                  .text = preproc_token.text,
                                  .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_IDENTIFIER: {
      enum lex_token_ty ty = refine_ty(&preproc_token);
      *token = (struct lex_token){
          .ty = ty, .text = preproc_token.text, .span = preproc_token.span};
      return;
    }
    case PREPROC_TOKEN_TY_PREPROC_NUMBER:
      *token = (struct lex_token){.ty = lex_number_literal(&preproc_token),
                                  .text = preproc_token.text,
                                  .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_STRING_LITERAL:
      *token = (struct lex_token){.ty = lex_string_literal(&preproc_token),
                                  .text = preproc_token.text,
                                  .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_PUNCTUATOR:
      *token = (struct lex_token){
          .ty = preproc_punctuator_to_lex_token_ty(preproc_token.punctuator.ty),
          .text = preproc_token.text,
          .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_COMMENT:
    case PREPROC_TOKEN_TY_NEWLINE:
    case PREPROC_TOKEN_TY_WHITESPACE:
      continue;
    case PREPROC_TOKEN_TY_DIRECTIVE:
      BUG("directive token reached lexer");
    case PREPROC_TOKEN_TY_OTHER:
      TODO("handler OTHER preproc tokens in lexer");
    }
  }
}

void lex_peek_token(struct lexer *lexer, struct lex_token *token) {
  // TODO: we could use a deque instead of vector and pop tokens as we go?

  if (lexer->pos < vector_length(lexer->tokens)) {
    *token = *(struct lex_token *)vector_get(lexer->tokens, lexer->pos);
  } else {
    lex_next_token(lexer, token);
    token->internal_lexer_next_pos = lexer->pos + 1;

    vector_push_back(lexer->tokens, token);
  }
}

// TODO: make this return a real type
// just hacking it into text pos as not to need to change `parse.c`
struct lex_pos lex_get_position(struct lexer *lexer) {
  // shouldn't really be last tex pos... should be "next"
  return (struct lex_pos){.id = lexer->pos,
                          .text_pos = lex_get_last_text_pos(lexer)};
}

void lex_backtrack(struct lexer *lexer, struct lex_pos position) {
  lexer->pos = position.id;
  lexer->last_text_pos = position.text_pos;
}

struct text_pos lex_get_last_text_pos(const struct lexer *lexer) {
  return lexer->last_text_pos;
}

void lex_consume_token(struct lexer *lexer, struct lex_token token) {
  // FIXME: when you consume `token`, you jump to the next token regardless of
  // if you have consumed previous tokens instead, it should move forward 1
  // token we have the `internal_lexer_pos` field which corresponds to
  // `lexer->pos` and have assertions that are not jumping past tokens once we
  // have checked these assertions are never hit, we can remove it all also see
  // `backtrack` and `get_position` which generate fake `text_pos` for this
  // purpose

  DEBUG_ASSERT(token.internal_lexer_next_pos == lexer->pos + 1,
               "jumped %zu tokens (expected 1)",
               token.internal_lexer_next_pos - lexer->pos);
  lexer->pos = token.internal_lexer_next_pos;
  lexer->last_text_pos = token.span.end;
}

struct sized_str lex_strlike_associated_text(const struct lexer *lexer,
                                             const struct lex_token *token) {
  return process_raw_string(lexer, token);
}

struct sized_str lex_associated_text(const struct lexer *lexer,
                                     const struct lex_token *token) {
  switch (token->ty) {
  case LEX_TOKEN_TY_ASCII_STR_LITERAL:
  case LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL:
  case LEX_TOKEN_TY_ASCII_CHAR_LITERAL:
  case LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL:
    BUG("use `strlike_associated_text` instead");
  case LEX_TOKEN_TY_IDENTIFIER:
  case LEX_TOKEN_TY_FLOAT_LITERAL:
  case LEX_TOKEN_TY_DOUBLE_LITERAL:
  case LEX_TOKEN_TY_LONG_DOUBLE_LITERAL:
  case LEX_TOKEN_TY_SIGNED_INT_LITERAL:
  case LEX_TOKEN_TY_UNSIGNED_INT_LITERAL:
  case LEX_TOKEN_TY_SIGNED_LONG_LITERAL:
  case LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL:
  case LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL:
  case LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL: {
    size_t len = text_span_len(&token->span);
    char *p = arena_alloc(lexer->arena, len + 1);
    memcpy(p, token->text, len);
    p[len] = '\0';
    return (struct sized_str){p, len};
  }
  case LEX_TOKEN_TY_ELLIPSIS:
    return (struct sized_str){"...", 3};
  default:
    BUG("associated text did not make sense for token '%s'",
        lex_token_name(lexer, token));
  }
}

const char *lex_token_name(UNUSED_ARG(const struct lexer *lexer),
                           const struct lex_token *token) {
#define CASE_RET(name)                                                         \
  case name:                                                                   \
    return #name;

  switch (token->ty) {
    CASE_RET(LEX_TOKEN_TY_UNKNOWN)
    CASE_RET(LEX_TOKEN_TY_EOF)

    CASE_RET(LEX_TOKEN_TY_WHITESPACE)
    CASE_RET(LEX_TOKEN_TY_INLINE_COMMENT)
    CASE_RET(LEX_TOKEN_TY_MULTILINE_COMMENT)

    CASE_RET(LEX_TOKEN_TY_OP_NOT)
    CASE_RET(LEX_TOKEN_TY_OP_LOGICAL_NOT)

    CASE_RET(LEX_TOKEN_TY_OP_INC)
    CASE_RET(LEX_TOKEN_TY_OP_DEC)

    CASE_RET(LEX_TOKEN_TY_OP_ASSG)

    CASE_RET(LEX_TOKEN_TY_OP_LOGICAL_OR)
    CASE_RET(LEX_TOKEN_TY_OP_OR)
    CASE_RET(LEX_TOKEN_TY_OP_OR_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_XOR)
    CASE_RET(LEX_TOKEN_TY_OP_XOR_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_LOGICAL_AND)
    CASE_RET(LEX_TOKEN_TY_OP_AND)
    CASE_RET(LEX_TOKEN_TY_OP_AND_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_LSHIFT)
    CASE_RET(LEX_TOKEN_TY_OP_LSHIFT_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_RSHIFT)
    CASE_RET(LEX_TOKEN_TY_OP_RSHIFT_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_ADD_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_SUB_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_MUL_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_DIV_ASSG)
    CASE_RET(LEX_TOKEN_TY_OP_MOD_ASSG)

    CASE_RET(LEX_TOKEN_TY_OP_ADD)
    CASE_RET(LEX_TOKEN_TY_OP_SUB)
    CASE_RET(LEX_TOKEN_TY_OP_MUL)
    CASE_RET(LEX_TOKEN_TY_OP_DIV)
    CASE_RET(LEX_TOKEN_TY_OP_MOD)
    CASE_RET(LEX_TOKEN_TY_OP_EQ)
    CASE_RET(LEX_TOKEN_TY_OP_NEQ)
    CASE_RET(LEX_TOKEN_TY_OP_LT)
    CASE_RET(LEX_TOKEN_TY_OP_LTEQ)
    CASE_RET(LEX_TOKEN_TY_OP_GT)
    CASE_RET(LEX_TOKEN_TY_OP_GTEQ)

    CASE_RET(LEX_TOKEN_TY_COLON)
    CASE_RET(LEX_TOKEN_TY_SEMICOLON)
    CASE_RET(LEX_TOKEN_TY_COMMA)
    CASE_RET(LEX_TOKEN_TY_DOT)
    CASE_RET(LEX_TOKEN_TY_ARROW)
    CASE_RET(LEX_TOKEN_TY_QMARK)

    CASE_RET(LEX_TOKEN_TY_ELLIPSIS)

    CASE_RET(LEX_TOKEN_TY_KW_GENERIC)
    CASE_RET(LEX_TOKEN_TY_KW_STATICASSERT)
    CASE_RET(LEX_TOKEN_TY_KW_BOOL)
    CASE_RET(LEX_TOKEN_TY_KW_UINT128)
    CASE_RET(LEX_TOKEN_TY_KW_NORETURN)
    CASE_RET(LEX_TOKEN_TY_KW_ATTRIBUTE)
    CASE_RET(LEX_TOKEN_TY_KW_GOTO)
    CASE_RET(LEX_TOKEN_TY_KW_BREAK)
    CASE_RET(LEX_TOKEN_TY_KW_CONTINUE)
    CASE_RET(LEX_TOKEN_TY_KW_DO)
    CASE_RET(LEX_TOKEN_TY_KW_FOR)
    CASE_RET(LEX_TOKEN_TY_KW_WHILE)
    CASE_RET(LEX_TOKEN_TY_KW_SWITCH)
    CASE_RET(LEX_TOKEN_TY_KW_DEFAULT)
    CASE_RET(LEX_TOKEN_TY_KW_CASE)
    CASE_RET(LEX_TOKEN_TY_KW_IF)
    CASE_RET(LEX_TOKEN_TY_KW_ELSE)
    CASE_RET(LEX_TOKEN_TY_KW_RETURN)
    CASE_RET(LEX_TOKEN_TY_KW_ENUM)
    CASE_RET(LEX_TOKEN_TY_KW_STRUCT)
    CASE_RET(LEX_TOKEN_TY_KW_UNION)
    CASE_RET(LEX_TOKEN_TY_KW_SIZEOF)
    CASE_RET(LEX_TOKEN_TY_KW_ALIGNOF)
    CASE_RET(LEX_TOKEN_TY_KW_ALIGNAS)

    CASE_RET(LEX_TOKEN_TY_KW_TYPEDEF)
    CASE_RET(LEX_TOKEN_TY_KW_STATIC)
    CASE_RET(LEX_TOKEN_TY_KW_EXTERN)
    CASE_RET(LEX_TOKEN_TY_KW_AUTO)
    CASE_RET(LEX_TOKEN_TY_KW_REGISTER)

    CASE_RET(LEX_TOKEN_TY_KW_INLINE)

    CASE_RET(LEX_TOKEN_TY_KW_CONST)
    CASE_RET(LEX_TOKEN_TY_KW_VOLATILE)
    CASE_RET(LEX_TOKEN_TY_KW_RESTRICT)

    CASE_RET(LEX_TOKEN_TY_KW_VOID)

    CASE_RET(LEX_TOKEN_TY_KW_HALF)
    CASE_RET(LEX_TOKEN_TY_KW_FLOAT)
    CASE_RET(LEX_TOKEN_TY_KW_DOUBLE)
    CASE_RET(LEX_TOKEN_TY_KW_CHAR)
    CASE_RET(LEX_TOKEN_TY_KW_SHORT)
    CASE_RET(LEX_TOKEN_TY_KW_INT)
    CASE_RET(LEX_TOKEN_TY_KW_LONG)
    CASE_RET(LEX_TOKEN_TY_KW_SIGNED)
    CASE_RET(LEX_TOKEN_TY_KW_UNSIGNED)

    CASE_RET(LEX_TOKEN_TY_OPEN_SQUARE_BRACKET)
    CASE_RET(LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET)
    CASE_RET(LEX_TOKEN_TY_OPEN_BRACKET)
    CASE_RET(LEX_TOKEN_TY_CLOSE_BRACKET)
    CASE_RET(LEX_TOKEN_TY_OPEN_BRACE)
    CASE_RET(LEX_TOKEN_TY_CLOSE_BRACE)
    CASE_RET(LEX_TOKEN_TY_IDENTIFIER)

    CASE_RET(LEX_TOKEN_TY_ASCII_STR_LITERAL)
    CASE_RET(LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL)
    CASE_RET(LEX_TOKEN_TY_ASCII_CHAR_LITERAL)
    CASE_RET(LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL)

    CASE_RET(LEX_TOKEN_TY_FLOAT_LITERAL)
    CASE_RET(LEX_TOKEN_TY_DOUBLE_LITERAL)
    CASE_RET(LEX_TOKEN_TY_LONG_DOUBLE_LITERAL)

    CASE_RET(LEX_TOKEN_TY_SIGNED_INT_LITERAL)
    CASE_RET(LEX_TOKEN_TY_UNSIGNED_INT_LITERAL)

    CASE_RET(LEX_TOKEN_TY_SIGNED_LONG_LITERAL)
    CASE_RET(LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL)

    CASE_RET(LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL)
    CASE_RET(LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL)
  }

#undef CASE_RET
}

static enum lex_token_ty
preproc_punctuator_to_lex_token_ty(enum preproc_token_punctuator_ty ty) {
  switch (ty) {
  case PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACKET:
    return LEX_TOKEN_TY_OPEN_BRACKET;
  case PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACKET:
    return LEX_TOKEN_TY_CLOSE_BRACKET;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_BRACE:
    return LEX_TOKEN_TY_OPEN_BRACE;
  case PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_BRACE:
    return LEX_TOKEN_TY_CLOSE_BRACE;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OPEN_SQUARE_BRACKET:
    return LEX_TOKEN_TY_OPEN_SQUARE_BRACKET;
  case PREPROC_TOKEN_PUNCTUATOR_TY_CLOSE_SQUARE_BRACKET:
    return LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET;

  case PREPROC_TOKEN_PUNCTUATOR_TY_COLON:
    return LEX_TOKEN_TY_COLON;
  case PREPROC_TOKEN_PUNCTUATOR_TY_SEMICOLON:
    return LEX_TOKEN_TY_SEMICOLON;
  case PREPROC_TOKEN_PUNCTUATOR_TY_COMMA:
    return LEX_TOKEN_TY_COMMA;
  case PREPROC_TOKEN_PUNCTUATOR_TY_DOT:
    return LEX_TOKEN_TY_DOT;
  case PREPROC_TOKEN_PUNCTUATOR_TY_ARROW:
    return LEX_TOKEN_TY_ARROW;
  case PREPROC_TOKEN_PUNCTUATOR_TY_QMARK:
    return LEX_TOKEN_TY_QMARK;

  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_NOT:
    return LEX_TOKEN_TY_OP_LOGICAL_NOT;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_NOT:
    return LEX_TOKEN_TY_OP_NOT;

  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_INC:
    return LEX_TOKEN_TY_OP_INC;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_DEC:
    return LEX_TOKEN_TY_OP_DEC;

  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_AND:
    return LEX_TOKEN_TY_OP_LOGICAL_AND;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LOGICAL_OR:
    return LEX_TOKEN_TY_OP_LOGICAL_OR;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND:
    return LEX_TOKEN_TY_OP_AND;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_AND_ASSG:
    return LEX_TOKEN_TY_OP_AND_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR:
    return LEX_TOKEN_TY_OP_OR;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_OR_ASSG:
    return LEX_TOKEN_TY_OP_OR_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR:
    return LEX_TOKEN_TY_OP_XOR;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_XOR_ASSG:
    return LEX_TOKEN_TY_OP_XOR_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_RSHIFT:
    return LEX_TOKEN_TY_OP_RSHIFT;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_RSHIFT_ASSG:
    return LEX_TOKEN_TY_OP_RSHIFT_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LSHIFT:
    return LEX_TOKEN_TY_OP_LSHIFT;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LSHIFT_ASSG:
    return LEX_TOKEN_TY_OP_LSHIFT_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD:
    return LEX_TOKEN_TY_OP_ADD;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_ADD_ASSG:
    return LEX_TOKEN_TY_OP_ADD_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB:
    return LEX_TOKEN_TY_OP_SUB;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_SUB_ASSG:
    return LEX_TOKEN_TY_OP_SUB_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL:
    return LEX_TOKEN_TY_OP_MUL;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_MUL_ASSG:
    return LEX_TOKEN_TY_OP_MUL_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV:
    return LEX_TOKEN_TY_OP_DIV;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_DIV_ASSG:
    return LEX_TOKEN_TY_OP_DIV_ASSG;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_MOD:
    return LEX_TOKEN_TY_OP_MOD;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_MOD_ASSG:
    return LEX_TOKEN_TY_OP_MOD_ASSG;

  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_ASSG:
    return LEX_TOKEN_TY_OP_ASSG;

  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_EQ:
    return LEX_TOKEN_TY_OP_EQ;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_NEQ:
    return LEX_TOKEN_TY_OP_NEQ;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LT:
    return LEX_TOKEN_TY_OP_LT;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_LTEQ:
    return LEX_TOKEN_TY_OP_LTEQ;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_GT:
    return LEX_TOKEN_TY_OP_GT;
  case PREPROC_TOKEN_PUNCTUATOR_TY_OP_GTEQ:
    return LEX_TOKEN_TY_OP_GTEQ;

  case PREPROC_TOKEN_PUNCTUATOR_TY_ELLIPSIS:
    return LEX_TOKEN_TY_ELLIPSIS;
  case PREPROC_TOKEN_PUNCTUATOR_TY_STRINGIFY:
  case PREPROC_TOKEN_PUNCTUATOR_TY_CONCAT:
    BUG("stringify/concat tokens should not reach lexer");
  }
}
