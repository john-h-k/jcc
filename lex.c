#include "lex.h"

#include "alloc.h"
#include "log.h"
#include "util.h"

#include <ctype.h>

struct lexer {
  struct program *program;

  struct arena_allocator *arena;
  const char *text;
  size_t len;

  struct text_pos pos;

  const char **associated_texts;
};

enum lex_create_result lexer_create(struct preprocessed_program *program, struct lexer **lexer) {
  info("beginning lex stage");

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct lexer *l = nonnull_malloc(sizeof(*l));
  l->arena = arena;
  l->text = arena_alloc_strcpy(arena, program->text);
  l->len = strlen(l->text);
  l->pos.idx = 0;
  l->pos.line = 0;
  l->pos.col = 0;

  *lexer = l;

  return LEX_CREATE_RESULT_SUCCESS;
}

void lexer_free(struct lexer **lexer) {
  arena_allocator_free(&(*lexer)->arena);
  (*lexer)->arena = NULL;
  free(*lexer);

  *lexer = NULL;
}

bool valid_identifier_char(char c) {
  return isalpha(c) || isdigit(c) || c == '_';
}

/* Identifiers cannot start with digits */
bool valid_first_identifier_char(char c) {
  return !isdigit(c) && valid_identifier_char(c);
}

/* The lexer parses identifiers, but these could be identifiers, typedef-names,
   or keywords. This function converts identifiers into their "real" type */
enum lex_token_ty refine_ty(struct lexer *lexer, struct text_pos start,
                            struct text_pos end) {
  struct keyword {
    const char *str;
    size_t len;
    enum lex_token_ty ty;
  };

  size_t len = text_pos_len(start, end);

#define KEYWORD(kw, ty)                                                        \
  { kw, sizeof(kw) - 1, ty }

  // TODO: hashify
  static struct keyword keywords[] = {
      KEYWORD("goto", LEX_TOKEN_TY_KW_GOTO),
      KEYWORD("break", LEX_TOKEN_TY_KW_BREAK),
      KEYWORD("continue", LEX_TOKEN_TY_KW_CONTINUE),
      KEYWORD("do", LEX_TOKEN_TY_KW_DO),
      KEYWORD("for", LEX_TOKEN_TY_KW_FOR),
      KEYWORD("while", LEX_TOKEN_TY_KW_WHILE),
      KEYWORD("if", LEX_TOKEN_TY_KW_IF),
      KEYWORD("else", LEX_TOKEN_TY_KW_ELSE),
      KEYWORD("return", LEX_TOKEN_TY_KW_RETURN),

      KEYWORD("typedef", LEX_TOKEN_TY_KW_TYPEDEF),
      KEYWORD("static", LEX_TOKEN_TY_KW_STATIC),
      KEYWORD("auto", LEX_TOKEN_TY_KW_AUTO),
      KEYWORD("extern", LEX_TOKEN_TY_KW_EXTERN),
      KEYWORD("register", LEX_TOKEN_TY_KW_REGISTER),

      KEYWORD("inline", LEX_TOKEN_TY_KW_INLINE),

      KEYWORD("const", LEX_TOKEN_TY_KW_CONST),
      KEYWORD("volatile", LEX_TOKEN_TY_KW_VOLATILE),

      KEYWORD("void", LEX_TOKEN_TY_KW_VOID),
      KEYWORD("float", LEX_TOKEN_TY_KW_FLOAT),
      KEYWORD("double", LEX_TOKEN_TY_KW_DOUBLE),
      KEYWORD("char", LEX_TOKEN_TY_KW_CHAR),
      KEYWORD("short", LEX_TOKEN_TY_KW_SHORT),
      KEYWORD("int", LEX_TOKEN_TY_KW_INT),
      KEYWORD("long", LEX_TOKEN_TY_KW_LONG),
      KEYWORD("unsigned", LEX_TOKEN_TY_KW_UNSIGNED),
      KEYWORD("signed", LEX_TOKEN_TY_KW_SIGNED),
      KEYWORD("enum", LEX_TOKEN_TY_KW_ENUM),
      KEYWORD("struct", LEX_TOKEN_TY_KW_STRUCT),
      KEYWORD("union", LEX_TOKEN_TY_KW_UNION),

      KEYWORD("sizeof", LEX_TOKEN_TY_KW_SIZEOF),
      KEYWORD("alignof", LEX_TOKEN_TY_KW_ALIGNOF),
      KEYWORD("_Alignof", LEX_TOKEN_TY_KW_ALIGNOF),
      KEYWORD("alignas", LEX_TOKEN_TY_KW_ALIGNAS),
      KEYWORD("_Alignas", LEX_TOKEN_TY_KW_ALIGNAS),
  };

#undef KEYWORD

  for (size_t i = 0; i < ARR_LENGTH(keywords); i++) {
    if (len == keywords[i].len &&
        memcmp(&lexer->text[start.idx], keywords[i].str, len) == 0) {
      return keywords[i].ty;
    }
  }

  return LEX_TOKEN_TY_IDENTIFIER;
}

struct text_pos get_position(struct lexer *lexer) { return lexer->pos; }

void backtrack(struct lexer *lexer, struct text_pos position) {
  lexer->pos = position;
}

void consume_token(struct lexer *lexer, struct token token) {
  lexer->pos = token.span.end;
}

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

const char *process_raw_string(const struct lexer *lexer, const struct token *token) {
  // TODO: this i think will wrongly accept multilines

  size_t max_str_len = token->span.end.idx - token->span.start.idx;

  char *buff = arena_alloc(lexer->arena, max_str_len - 1);

  size_t str_len = 0;
  bool char_escaped = false;
  for (size_t i = token->span.start.idx + 1;
       i <= token->span.end.idx && !(!char_escaped && lexer->text[i] == '"');
       i++) {
    if (char_escaped) {
#define ADD_ESCAPED(ch, esc)                                                   \
  case ch:                                                                     \
    buff[str_len++] = esc;                                                     \
    break;

      switch (lexer->text[i]) {
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
        break;
      }

#undef ADD_ESCAPED
    } else if (lexer->text[i] != '\\') {
      buff[str_len++] = lexer->text[i];
    }

    // next char is escaped if this char is a non-escaped backslash
    char_escaped = !char_escaped && lexer->text[i] == '\\';
  }

  buff[str_len] = 0;
  return buff;
}

/* Attempts to consume and move forward the position if it finds char `c` */
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

// this is really more a parsing exercise than a lexing exercise, but it makes
// parser cleaner
bool try_lex_fp_literal(struct lexer *lexer, struct text_pos *cur_pos) {
  UNUSED_ARG(lexer);
  UNUSED_ARG(cur_pos);
  todo("impl");
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
    token->ty = LEX_TOKEN_TY_EOF;
    token->span.start = start;
    token->span.end = end;
    return;
  }

  char c = lexer->text[start.idx];

  trace("lexing char '%c'", c);
  size_t context = MIN(lexer->len - start.idx, 25);
  trace("on '%.*s'\n", context, &lexer->text[start.idx]);

  enum lex_token_ty ty;
  switch (c) {
  case '(':
    ty = LEX_TOKEN_TY_OPEN_BRACKET;
    next_col(&end);
    break;
  case ')':
    ty = LEX_TOKEN_TY_CLOSE_BRACKET;
    next_col(&end);
    break;

  case '[':
    ty = LEX_TOKEN_TY_OPEN_SQUARE_BRACKET;
    next_col(&end);
    break;
  case ']':
    ty = LEX_TOKEN_TY_CLOSE_SQUARE_BRACKET;
    next_col(&end);
    break;

  case '{':
    ty = LEX_TOKEN_TY_OPEN_BRACE;
    next_col(&end);
    break;
  case '}':
    ty = LEX_TOKEN_TY_CLOSE_BRACE;
    next_col(&end);
    break;
  case ':':
    ty = LEX_TOKEN_TY_COLON;
    next_col(&end);
    break;
  case ';':
    ty = LEX_TOKEN_TY_SEMICOLON;
    next_col(&end);
    break;
  case ',':
    ty = LEX_TOKEN_TY_COMMA;
    next_col(&end);
    break;
  case '.':
    next_col(&end);

    if (try_consume(lexer, &end, '.')) {
      if (try_consume(lexer, &end, '.')) {
        ty = LEX_TOKEN_TY_ELLIPSIS;
      } else {
        ty = LEX_TOKEN_TY_UNKNOWN;
      }
    } else {
      // NOTE: `.75` is a valid float
      // grammar requires a digit after `.` to be valid so we check for that
      if (end.idx < lexer->len && isdigit(lexer->text[end.idx])) {
        goto number_literal;
      }

      ty = LEX_TOKEN_TY_DOT;
    }
    break;

  case '>':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_GTEQ;
    } else if (try_consume(lexer, &end, '>')) {
      if (try_consume(lexer, &end, '=')) {
        ty = LEX_TOKEN_TY_OP_RSHIFT_ASSG;
      } else {
        ty = LEX_TOKEN_TY_OP_RSHIFT;
      }
    } else {
      ty = LEX_TOKEN_TY_OP_GT;
    }
    break;
  case '<':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_LTEQ;
    } else if (try_consume(lexer, &end, '<')) {
      if (try_consume(lexer, &end, '=')) {
        ty = LEX_TOKEN_TY_OP_LSHIFT_ASSG;
      } else {
        ty = LEX_TOKEN_TY_OP_LSHIFT;
      }
    } else {
      ty = LEX_TOKEN_TY_OP_LT;
    }
    break;
  case '~':
    next_col(&end);
    ty = LEX_TOKEN_TY_OP_NOT;
    break;
  case '!':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_NEQ;
    } else {
      ty = LEX_TOKEN_TY_OP_LOGICAL_NOT;
    }
    break;
  case '=':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_EQ;
    } else {
      ty = LEX_TOKEN_TY_OP_ASSG;
    }
    break;
  case '&':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_AND_ASSG;
    } else if (try_consume(lexer, &end, '&')) {
      ty = LEX_TOKEN_TY_OP_LOGICAL_AND;
    } else {
      ty = LEX_TOKEN_TY_OP_AND;
    }
    break;
  case '|':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_OR_ASSG;
    } else if (try_consume(lexer, &end, '|')) {
      ty = LEX_TOKEN_TY_OP_LOGICAL_OR;
    } else {
      ty = LEX_TOKEN_TY_OP_OR;
    }
    break;
  case '^':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_XOR_ASSG;
    } else {
      ty = LEX_TOKEN_TY_OP_XOR;
    }
    break;
  case '+':
    next_col(&end);
    if (try_consume(lexer, &end, '+')) {
      ty = LEX_TOKEN_TY_OP_INC;
    } else if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_ADD_ASSG;
    } else {
      ty = LEX_TOKEN_TY_OP_ADD;
    }
    break;
  case '-':
    next_col(&end);
    if (try_consume(lexer, &end, '-')) {
      ty = LEX_TOKEN_TY_OP_DEC;
    } else if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_SUB_ASSG;
    } else if (try_consume(lexer, &end, '>')) {
      ty = LEX_TOKEN_TY_ARROW;
    } else {
      ty = LEX_TOKEN_TY_OP_SUB;
    }
    break;
  case '*':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_MUL_ASSG;
    } else {
      ty = LEX_TOKEN_TY_OP_MUL;
    }
    break;
  case '/':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_DIV_ASSG;
    } else {
      ty = LEX_TOKEN_TY_OP_DIV;
    }

    break;
  case '%':
    next_col(&end);
    if (try_consume(lexer, &end, '=')) {
      ty = LEX_TOKEN_TY_OP_QUOT_ASSG;
    } else {
      ty = LEX_TOKEN_TY_OP_QUOT;
    }
    break;

  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  number_literal: {
    // all integers must begin with a digit
    // any digit for decimal, `0` for hex/octal
    // floats can be digit or `.`

    bool is_float = c == '.';

    next_col(&end);

    // remove hex prefix
    bool is_hex = try_consume(lexer, &end, 'x');

    // this is generous and will allow 0BE for example, when only 0xBE is valid
    // that's okay, let parser handle it
    for (; end.idx < lexer->len; next_col(&end)) {
      if (lexer->text[end.idx] == '.') {
        is_float = true;
        continue;
      }

      if (!is_hex && (lexer->text[end.idx] == 'E' || lexer->text[end.idx] == 'e')) {
        is_float = true;
        next_col(&end);

        if (end.idx < lexer->len && (lexer->text[end.idx] == '+' || lexer->text[end.idx] == '-')) {
          next_col(&end);
        }

        // skip the sign after exponent
        continue;
      }

      if ((is_float && !isdigit(lexer->text[end.idx])) || (!is_float && !isxdigit(lexer->text[end.idx]))) {
        break;
      }
    }

    bool is_unsigned = false;

    ty = is_float ? LEX_TOKEN_TY_DOUBLE_LITERAL : LEX_TOKEN_TY_SIGNED_INT_LITERAL;
    while (end.idx < lexer->len) {
      switch (tolower(lexer->text[end.idx])) {
      case 'u':
        is_unsigned = true;
        next_col(&end);
        continue;
      case 'f':
        ty = LEX_TOKEN_TY_FLOAT_LITERAL;
        next_col(&end);
        continue;
      case 'l':
        if (!is_float && end.idx + 2 < lexer->len &&
            tolower(lexer->text[end.idx + 1]) == 'l') {
          ty = LEX_TOKEN_TY_SIGNED_LONG_LONG_LITERAL;
        } else if (is_float) {
          ty = LEX_TOKEN_TY_LONG_DOUBLE_LITERAL;
        } else {
          ty = LEX_TOKEN_TY_SIGNED_LONG_LITERAL;
        }
        next_col(&end);
        continue;
      default:
        break;
      }

      break;
    }

    if (is_unsigned) {
      invariant_assert(!is_float, "can't be unsigned and float");
      ty++;
    }
    break;
  }

  default: {
    if (c == '\'') {
      ty = LEX_TOKEN_TY_ASCII_CHAR_LITERAL;

      // skip first single-quote
      next_col(&end);

      // move forward while
      bool char_escaped = false;
      for (size_t i = end.idx;
           i < lexer->len && !(!char_escaped && lexer->text[i] == '\''); i++) {
        // next char is escaped if this char is a non-escaped backslash
        char_escaped = !char_escaped && lexer->text[i] == '\\';
        next_col(&end);
      }

      // skip final single-quote
      next_col(&end);
    } else if (c == '"') {
      // TODO: logic is same as for char, could dedupe
      ty = LEX_TOKEN_TY_ASCII_STR_LITERAL;

      // skip first double-quote
      next_col(&end);

      // move forward while
      bool char_escaped = false;
      for (size_t i = end.idx;
           i < lexer->len && !(!char_escaped && lexer->text[i] == '"'); i++) {
        // next char is escaped if this char is a non-escaped backslash
        char_escaped = !char_escaped && lexer->text[i] == '\\';
        next_col(&end);
      }

      // skip final double-quote
      next_col(&end);

    } else if (valid_first_identifier_char(c)) {
      ty = LEX_TOKEN_TY_IDENTIFIER;

      for (size_t i = end.idx;
           i < lexer->len && valid_identifier_char(lexer->text[i]); i++) {
        next_col(&end);
      }

      // slightly hacky solution - retroactively determine if identifier
      // is a keyword
      ty = refine_ty(lexer, start, end);
    } else {
      bug("lexer hit an unknown token! line=%zu, col=%zu, value=%u", start.line, start.col, c);
    }
  }
  }

  token->ty = ty;
  token->span.start = start;
  token->span.end = end;

  debug("parse token %s\n", token_name(lexer, token));
}

const char *associated_text(const struct lexer *lexer, const struct token *token) {
  switch (token->ty) {
  case LEX_TOKEN_TY_ASCII_STR_LITERAL:
    return process_raw_string(lexer, token);
    break;
  case LEX_TOKEN_TY_IDENTIFIER:
  case LEX_TOKEN_TY_ASCII_CHAR_LITERAL:
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
    bug("associated text did not make sense for token '%s'", token_name(lexer, token));
  }
}

const char *token_name(const struct lexer *lexer, const struct token *token) {
  UNUSED_ARG(lexer);

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

    CASE_RET(LEX_TOKEN_TY_ELLIPSIS)

    CASE_RET(LEX_TOKEN_TY_KW_GOTO)
    CASE_RET(LEX_TOKEN_TY_KW_BREAK)
    CASE_RET(LEX_TOKEN_TY_KW_CONTINUE)
    CASE_RET(LEX_TOKEN_TY_KW_DO)
    CASE_RET(LEX_TOKEN_TY_KW_FOR)
    CASE_RET(LEX_TOKEN_TY_KW_WHILE)
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
    CASE_RET(LEX_TOKEN_TY_ASCII_CHAR_LITERAL)

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
