#include "lex.h"

#include "alloc.h"
#include "hashtbl.h"
#include "log.h"
#include "preproc.h"
#include "program.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>

struct lexer {
  struct arena_allocator *arena;

  struct program *program;
  struct preproc *preproc;

  const char *text;
  size_t len;

  // once we generate a token, we put it here
  struct vector *tokens;
  size_t pos;

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

    // TODO: hashify
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

    KEYWORD("void", LEX_TOKEN_TY_KW_VOID);
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

    debug("build kw table (len=%zu)", hashtbl_size(KEYWORDS));
  }

  info("beginning lex stage");

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct lexer *l = nonnull_malloc(sizeof(*l));
  l->arena = arena;

  l->program = program;
  l->preproc = preproc;

  l->text = program->text;
  l->len = strlen(program->text);

  l->tokens = vector_create(sizeof(struct lex_token));
  l->pos = 0;
  

  *lexer = l;

  return LEX_CREATE_RESULT_SUCCESS;
}

void lexer_free(struct lexer **lexer) {
  arena_allocator_free(&(*lexer)->arena);
  (*lexer)->arena = NULL;
  free(*lexer);

  *lexer = NULL;
}

/* The lexer parses identifiers, but these could be identifiers, typedef-names,
   or keywords. This function converts identifiers into their "real" type */
static enum lex_token_ty refine_ty(struct lexer *lexer,
                                   const struct text_span *span) {
  struct keyword {
    const char *str;
    size_t len;
    enum lex_token_ty ty;
  };

  debug_assert(KEYWORDS, "keywords should have been built");

  struct sized_str key = {.str = &lexer->text[span->start.idx],
                          .len = text_span_len(span)};

  enum lex_token_ty *kw_ty = hashtbl_lookup(KEYWORDS, &key);

  if (kw_ty) {
    return *kw_ty;
  }

  return LEX_TOKEN_TY_IDENTIFIER;
}

static const char *process_raw_string(const struct lexer *lexer,
                                      const struct lex_token *token,
                                      size_t *str_len) {
  // TODO: this i think will wrongly accept multilines
  // FIXME: definitely wrong for wide strings

  struct vector *buff = vector_create(sizeof(char));

  char end_char = (token->ty == LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL ||
                   token->ty == LEX_TOKEN_TY_ASCII_CHAR_LITERAL)
                      ? '\''
                      : '"';
  size_t str_start = (token->ty == LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL ||
                      token->ty == LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL)
                         ? token->span.start.idx + 2
                         : token->span.start.idx + 1;

  *str_len = 0;
  bool char_escaped = false;
  for (size_t i = str_start; i <= token->span.end.idx &&
                             !(!char_escaped && lexer->text[i] == end_char);
       i++) {
    if (char_escaped) {
#define ADD_ESCAPED(ch, esc)                                                   \
  case ch: {                                                                   \
    char c = esc;                                                              \
    vector_push_back(buff, &c);                                                \
    break;                                                                     \
  }

      if (lexer->text[i] == '0') {
        size_t octal_start = i + 1;

        while (i + 1 < token->span.end.idx) {
          if (lexer->text[i + 1] >= '0' && lexer->text[i + 1] <= '7') {
            i++;
          } else {
            break;
          }
        }

        size_t octal_len = MIN(2, i - octal_start + 1);
        char oct_buff[3] = {0};
        for (size_t j = 0; j < octal_len; j++) {
          oct_buff[j] = lexer->text[octal_start + j];
        }

        unsigned char value = (unsigned char)strtoul(oct_buff, NULL, 8);
        vector_push_back(buff, &value);
      } else if (lexer->text[i] == 'u') {
        // FIXME: C23 allows arbitrary num digits, not just 4
        char u_buff[5] = {0};
        memcpy(u_buff, &lexer->text[i + 1], 4);
        i += 4;

        unsigned codepoint = strtoul(u_buff, NULL, 16);

        if (codepoint <= 0x7F) {
          char c = codepoint & 0x7F;
          vector_push_back(buff, &c);
        } else if (codepoint <= 0x7FF) {
          char c[2] = {0xC0 | ((codepoint >> 6) & 0x1F),
                       0x80 | (codepoint & 0x3F)};
          vector_extend(buff, c, 2);
        } else if (codepoint <= 0xFFFF) {
          char c[3] = {0xE0 | ((codepoint >> 12) & 0x0F),
                       0x80 | ((codepoint >> 6) & 0x3F),
                       0x80 | (codepoint & 0x3F)};
          vector_extend(buff, c, 3);
        } else if (codepoint <= 0x10FFFF) {
          char c[4] = {0xF0 | ((codepoint >> 18) & 0x07),
                       0x80 | ((codepoint >> 12) & 0x3F),
                       0x80 | ((codepoint >> 6) & 0x3F),
                       0x80 | (codepoint & 0x3F)};
          vector_extend(buff, c, 4);
        }
      } else if (lexer->text[i] == 'x') {
        char x_buff[3] = {0};
        memcpy(x_buff, &lexer->text[i + 1], 2);
        i += 2;

        unsigned char value = (unsigned char)strtoul(x_buff, NULL, 16);
        vector_push_back(buff, &value);
      } else {
        switch (lexer->text[i]) {
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
          todo("\\x \\u \\U and \\octal escapes");
          // either octal escape, or invalid
        }
      }

#undef ADD_ESCAPED
    } else if (lexer->text[i] != '\\') {
      vector_push_back(buff, &lexer->text[i]);
    }

    // next char is escaped if this char is a non-escaped backslash
    char_escaped = !char_escaped && lexer->text[i] == '\\';
  }

  *str_len = vector_length(buff);

  char null = 0;
  vector_push_back(buff, &null);

  char *value = nonnull_malloc(vector_byte_size(buff));
  vector_copy_to(buff, value);
  vector_free(&buff);

  return value;
}


bool lexer_at_eof(struct lexer *lexer) {
  // needed to skip whitespace
  struct lex_token token;
  peek_token(lexer, &token);

  return token.ty == LEX_TOKEN_TY_EOF;
}

// FIXME: keeping this code so don't need to rewrite it, but not sure if needed
// static void lex_literal() {
  // // all integers must begin with a digit
  // // any digit for decimal, `0` for hex/octal
  // // floats can be digit or `.`

  // bool is_float = c == '.';

  // next_col(&end);

  // // remove hex prefix
  // bool is_hex = try_consume(lexer, &end, 'x');

  // // this is generous and will allow 0BE for example, when only 0xBE is valid
  // // that's okay, let parser handle it
  // for (; end.idx < lexer->len; next_col(&end)) {
  //   if (lexer->text[end.idx] == '.') {
  //     is_float = true;
  //     continue;
  //   }

  //   if (!is_hex &&
  //       (lexer->text[end.idx] == 'E' || lexer->text[end.idx] == 'e')) {
  //     is_float = true;
  //     next_col(&end);

  //     if (end.idx < lexer->len &&
  //         (lexer->text[end.idx] == '+' || lexer->text[end.idx] == '-')) {
  //       next_col(&end);
  //     }

  //     // skip the sign after exponent
  //     continue;
  //   }

  //   if ((is_float && !isdigit(lexer->text[end.idx])) ||
  //       (!is_float && !isxdigit(lexer->text[end.idx]))) {
  //     break;
  //   }
  // }

  // bool is_unsigned = false;

  // ty = is_float ? LEX_TOKEN_TY_DOUBLE_LITERAL : LEX_TOKEN_TY_SIGNED_INT_LITERAL;
  // while (end.idx < lexer->len) {
  //   switch (tolower(lexer->text[end.idx])) {
  //   case 'u':
  //     is_unsigned = true;
  //     next_col(&end);
  //     continue;
  //   case 'f':
  //     ty = LEX_TOKEN_TY_FLOAT_LITERAL;
  //     next_col(&end);
  //     continue;
  //   case 'l':
  //     if (!is_float && end.idx + 2 < lexer->len &&
  //         tolower(lexer->text[end.idx + 1]) == 'l') {
  //       ty = LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL;
  //     } else if (is_float) {
  //       ty = LEX_TOKEN_TY_LONG_DOUBLE_LITERAL;
  //     } else {
  //       ty = LEX_TOKEN_TY_SIGNED_LONG_LITERAL;
  //     }
  //     next_col(&end);
  //     continue;
  //   default:
  //     break;
  //   }

  //   break;
  // }

  // if (is_unsigned) {
  //   invariant_assert(!is_float, "can't be unsigned and float");
  //   ty++;
  // }
  // break;

  // // default: {
  // if (c == '\'' ||
  //     (c == 'L' && end.idx < lexer->len && lexer->text[end.idx + 1] == '\'')) {
  //   ty = LEX_TOKEN_TY_ASCII_CHAR_LITERAL;

  //   // skip first single-quote
  //   if (c == 'L') {
  //     next_col(&end);
  //     ty = LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL;
  //   }
  //   next_col(&end);

  //   // move forward while
  //   bool char_escaped = false;
  //   for (size_t i = end.idx;
  //        i < lexer->len && !(!char_escaped && lexer->text[i] == '\''); i++) {
  //     // next char is escaped if this char is a non-escaped backslash
  //     char_escaped = !char_escaped && lexer->text[i] == '\\';
  //     next_col(&end);
  //   }

  //   // skip final single-quote
  //   next_col(&end);
  // } else if (c == '"' || (c == 'L' && end.idx < lexer->len &&
  //                         lexer->text[end.idx + 1] == '"')) {
  //   // TODO: logic is same as for char, could dedupe
  //   ty = LEX_TOKEN_TY_ASCII_STR_LITERAL;

  //   // skip first double-quote
  //   if (c == 'L') {
  //     next_col(&end);
  //     ty = LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL;
  //   }
  //   next_col(&end);

  //   // move forward while
  //   bool char_escaped = false;
  //   for (size_t i = end.idx;
  //        i < lexer->len && !(!char_escaped && lexer->text[i] == '"'); i++) {
  //     // next char is escaped if this char is a non-escaped backslash
  //     char_escaped = !char_escaped && lexer->text[i] == '\\';
  //     next_col(&end);
  //   }

  //   // skip final double-quote
  //   next_col(&end);
// }

static enum lex_token_ty preproc_punctuator_to_lex_token_ty(enum preproc_token_punctuator_ty ty);

static enum lex_token_ty lex_string_literal(struct lexer *lexer, const struct preproc_token *preproc_token) {
  debug_assert(preproc_token->ty == PREPROC_TOKEN_TY_STRING_LITERAL, "wrong preproc token ty");

  struct text_pos start = preproc_token->span.start;

  char c = lexer->text[start.idx];

  switch (c) {
    case '<':
      bug("found angle-bracket string literal in lexer");
    case '\"':
      return LEX_TOKEN_TY_ASCII_STR_LITERAL;
    case '\'':
      return LEX_TOKEN_TY_ASCII_CHAR_LITERAL;
    default:
      todo("other string/char literal types");
  }
}

static enum lex_token_ty lex_number_literal(struct lexer *lexer, const struct preproc_token *preproc_token) {
  debug_assert(preproc_token->ty == PREPROC_TOKEN_TY_PREPROC_NUMBER, "wrong preproc token ty");

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

  size_t len = text_pos_len(start, end);

  enum lit_ty lit_ty = LIT_TY_NONE;

  bool is_hex = len >= 2 && (lexer->text[start.idx] == '0' && lexer->text[start.idx + 1] == 'x');

  size_t start_idx = start.idx;
  size_t end_idx = end.idx;
  while (end_idx >= start_idx) {
    char c = lexer->text[end_idx];

    switch (c) {
      case 'l':
      case 'L':
        if (end_idx > start_idx && (lexer->text[end_idx - 1] == 'l' || lexer->text[end_idx - 1] == 'L')) {
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

  if (
    (memchr(&lexer->text[start_idx], '.', len))
    || (!is_hex && (memchr(&lexer->text[start_idx], 'e', len) || memchr(&lexer->text[start_idx], 'E', len)))
    || (is_hex && (memchr(&lexer->text[start_idx], 'p', len) || memchr(&lexer->text[start_idx], 'P', len)))) {
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
      todo("Z-sized literals");
    case LIT_TY_U | LIT_TY_L:
      ty = LEX_TOKEN_TY_UNSIGNED_LONG_LITERAL;
      break;
    case LIT_TY_U | LIT_TY_LL:
      ty = LEX_TOKEN_TY_UNSIGNED_LONG_LONG_LITERAL;
      break;
    case LIT_TY_U | LIT_TY_Z:
      todo("Z-sized literals");

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
      todo("handle bad suffixes for number literals");
  }

  return ty;
}

static void lex_next_token(struct lexer *lexer, struct lex_token *token) {
  while (true) {
    // need to loop as lexer discards certain tokens (whitespace, comments, etc)

    struct preproc_token preproc_token;
    preproc_next_token(lexer->preproc, &preproc_token);

    switch (preproc_token.ty) {
    case PREPROC_TOKEN_TY_UNKNOWN:
      *token = (struct lex_token){.ty = LEX_TOKEN_TY_UNKNOWN,
                                  .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_EOF:
      *token = (struct lex_token){.ty = LEX_TOKEN_TY_EOF,
                                  .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_DIRECTIVE:
      bug("directive token reached lexer");
    case PREPROC_TOKEN_TY_IDENTIFIER: {
      enum lex_token_ty ty = refine_ty(lexer, &preproc_token.span);
      *token = (struct lex_token){.ty = ty, .span = preproc_token.span};
      return;
    }
    case PREPROC_TOKEN_TY_PREPROC_NUMBER:
      *token = (struct lex_token){.ty = lex_number_literal(lexer, &preproc_token), .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_STRING_LITERAL:
      *token = (struct lex_token){.ty = lex_string_literal(lexer, &preproc_token), .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_PUNCTUATOR:
      *token = (struct lex_token){.ty = preproc_punctuator_to_lex_token_ty(preproc_token.punctuator.ty),
                                  .span = preproc_token.span};
      return;
    case PREPROC_TOKEN_TY_OTHER:
      todo("handler OTHER preproc tokens in lexer");

    case PREPROC_TOKEN_TY_COMMENT:
    case PREPROC_TOKEN_TY_NEWLINE:
    case PREPROC_TOKEN_TY_WHITESPACE:
      continue;
    }
  }
}

void peek_token(struct lexer *lexer, struct lex_token *token) {
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
struct text_pos get_position(struct lexer *lexer) {
  return (struct text_pos){.idx = lexer->pos};
}

void backtrack(struct lexer *lexer, struct text_pos position) {
  lexer->pos = position.idx;
}

void consume_token(struct lexer *lexer, struct lex_token token) {
  // FIXME: when you consume `token`, you jump to the next token regardless of if you have consumed previous tokens
  // instead, it should move forward 1 token
  // we have the `internal_lexer_pos` field which corresponds to `lexer->pos`
  // and have assertions that are not jumping past tokens
  // once we have checked these assertions are never hit, we can remove it all
  // also see `backtrack` and `get_position` which generate fake `text_pos` for this purpose

  debug_assert(token.internal_lexer_next_pos == lexer->pos + 1, "jumped %zu tokens (expected 1)", token.internal_lexer_next_pos - lexer->pos);
  lexer->pos = token.internal_lexer_next_pos;
  // lexer->pos = token.span.end;
}


const char *strlike_associated_text(const struct lexer *lexer,
                                    const struct lex_token *token,
                                    size_t *str_len) {
  return process_raw_string(lexer, token, str_len);
}

const char *associated_text(const struct lexer *lexer,
                            const struct lex_token *token) {
  switch (token->ty) {
  case LEX_TOKEN_TY_ASCII_STR_LITERAL:
  case LEX_TOKEN_TY_ASCII_WIDE_STR_LITERAL:
  case LEX_TOKEN_TY_ASCII_CHAR_LITERAL:
  case LEX_TOKEN_TY_ASCII_WIDE_CHAR_LITERAL:
    bug("use `strlike_associated_text` instead");
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
    memcpy(p, &lexer->text[token->span.start.idx], len);
    p[len] = '\0';
    return p;
  }
  case LEX_TOKEN_TY_ELLIPSIS:
    return "...";
  default:
    bug("associated text did not make sense for token '%s'",
        token_name(lexer, token));
  }
}

const char *token_name(UNUSED_ARG(const struct lexer *lexer),
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
    CASE_RET(LEX_TOKEN_TY_OP_QUOT_ASSG)

    CASE_RET(LEX_TOKEN_TY_OP_ADD)
    CASE_RET(LEX_TOKEN_TY_OP_SUB)
    CASE_RET(LEX_TOKEN_TY_OP_MUL)
    CASE_RET(LEX_TOKEN_TY_OP_DIV)
    CASE_RET(LEX_TOKEN_TY_OP_QUOT)
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

static enum lex_token_ty preproc_punctuator_to_lex_token_ty(enum preproc_token_punctuator_ty ty) {
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
    case PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT:
      return LEX_TOKEN_TY_OP_QUOT;
    case PREPROC_TOKEN_PUNCTUATOR_TY_OP_QUOT_ASSG:
      return LEX_TOKEN_TY_OP_QUOT_ASSG;

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
  }
}
