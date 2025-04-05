#include "json.h"

#include "alloc.h"
#include "ap_val.h"
#include "hashtbl.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>

struct json_ctx {
  struct arena_allocator *arena;
};

static size_t json_parse_ws(struct sized_str *str) {
  size_t rd = 0;

  while (str->len && isspace(str->str[0])) {
    str->str++;
    str->len--;
    rd++;
  }

  return rd;
}

static struct sized_str process_raw_string(struct json_ctx *ctx,
                                           struct sized_str value) {
  // TODO: this i think will wrongly accept multilines
  // FIXME: definitely wrong for wide strings

  struct vector *buff = vector_create_in_arena(sizeof(char), ctx->arena);

  size_t str_len = 0;
  bool char_escaped = false;

  for (size_t i = 0; i < value.len; i++) {
    if (char_escaped) {
#define PUSH_CHAR(ch)                                                          \
  char pc = (char)ch;                                                          \
  vector_push_back(buff, &pc);

#define ADD_ESCAPED(ch, esc)                                                   \
  case ch: {                                                                   \
    PUSH_CHAR(esc);                                                            \
    break;                                                                     \
  }
      if (value.str[i] == 'u') {
        char u_buff[5] = {0};
        memcpy(u_buff, &value.str[i + 1], 4);
        i += 4;

        unsigned long codepoint = strtoul(u_buff, NULL, 16);

        if (codepoint <= 0x7F) {
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
      } else {
        switch (value.str[i]) {
          ADD_ESCAPED('b', '\b')
          ADD_ESCAPED('f', '\f')
          ADD_ESCAPED('n', '\n')
          ADD_ESCAPED('r', '\r')
          ADD_ESCAPED('t', '\t')
          ADD_ESCAPED('\\', '\\')
          ADD_ESCAPED('\'', '\'')
          ADD_ESCAPED('/', '/')
          ADD_ESCAPED('"', '"')
        default:
          TODO("\\x \\u \\U and \\octal escapes");
          // either octal escape, or invalid
        }
      }

#undef ADD_ESCAPED
    } else if (value.str[i] != '\\') {
      PUSH_CHAR(value.str[i]);
    }

    // next char is escaped if this char is a non-escaped backslash
    char_escaped = !char_escaped && value.str[i] == '\\';
  }

  str_len = vector_byte_size(buff);

  PUSH_CHAR(0);

  return (struct sized_str){.str = vector_head(buff), .len = str_len};
}

static size_t json_parse_chunk(struct json_ctx *context, struct sized_str str,
                               struct json_result *result) {
  size_t rd = 0;

  if (!str.len) {
    *result =
        (struct json_result){.ty = JSON_RESULT_TY_ERR, .err = {.pos = rd}};
    return 0;
  }

  rd += json_parse_ws(&str);

  if (szstr_prefix(str, MK_SIZED("null"))) {
    *result = (struct json_result){.ty = JSON_RESULT_TY_VALUE,
                                   .value = JSON_MK_NULL()};
    return rd + strlen("null");
  }

  if (szstr_prefix(str, MK_SIZED("true"))) {
    *result = (struct json_result){.ty = JSON_RESULT_TY_VALUE,
                                   .value = JSON_MK_BOOL(true)};
    return rd + strlen("true");
  }

  if (szstr_prefix(str, MK_SIZED("false"))) {
    *result = (struct json_result){.ty = JSON_RESULT_TY_VALUE,
                                   .value = JSON_MK_BOOL(false)};
    return rd + strlen("false");
  }

  char ch = str.str[0];

  switch (ch) {
  case '"': {
    // string

    // move forward while
    bool char_escaped = false;

    size_t i = 1;
    size_t len = 0;
    for (; i < str.len; i++, len++) {
      if (!char_escaped && str.str[i] == '"') {
        i++;
        break;
      }

      if (i + 1 == str.len) {
        *result =
            (struct json_result){.ty = JSON_RESULT_TY_ERR, .err = {.pos = i}};
        return i;
      }

      char_escaped = !char_escaped && str.str[i] == '\\';
    }

    struct sized_str val = {.str = str.str + 1, .len = len};

    struct sized_str processed = process_raw_string(context, val);

    // TODO: process string for escapes
    *result = (struct json_result){.ty = JSON_RESULT_TY_VALUE,
                                   .value = JSON_MK_STR(processed)};
    return rd + i;
  }

  case '[': {
    str.str++;
    str.len--;
    rd++;

    rd += json_parse_ws(&str);

    // array
    struct vector *values =
        vector_create_in_arena(sizeof(struct json_value), context->arena);

    while (true) {
      switch (str.str[0]) {
      case ']':
        *result = (struct json_result){
            .ty = JSON_RESULT_TY_VALUE,
            .value = {.ty = JSON_VALUE_TY_ARRAY,
                      .arr_val = {
                          .num_values = vector_length(values),
                          .values = vector_head(values),
                      }}};
        return rd + 1;
      case ',':
        str.str++;
        str.len--;
        rd++;
      }

      struct json_result value;
      size_t val_rd = json_parse_chunk(context, str, &value);

      str.str += val_rd;
      str.len -= val_rd;
      rd += val_rd;

      if (value.ty == JSON_RESULT_TY_ERR) {
        *result = value;
        return rd;
      }

      vector_push_back(values, &value.value);

      rd += json_parse_ws(&str);

      if (!str.len) {
        *result =
            (struct json_result){.ty = JSON_RESULT_TY_ERR, .err = {.pos = rd}};
        return rd;
      }
    }
  }
  case '{': {
    str.str++;
    str.len--;
    rd++;

    rd += json_parse_ws(&str);

    // object
    struct hashtbl *fields = hashtbl_create_sized_str_keyed_in_arena(
        context->arena, sizeof(struct json_value));

    while (true) {
      switch (str.str[0]) {
      case '}':
        *result = (struct json_result){.ty = JSON_RESULT_TY_VALUE,
                                       .value = {.ty = JSON_VALUE_TY_OBJECT,
                                                 .obj_val = {
                                                     .fields = fields,
                                                 }}};
        return rd + 1;
      case ',':
        str.str++;
        str.len--;
        rd++;
      }

      struct json_result name;
      size_t name_rd = json_parse_chunk(context, str, &name);

      if (name.ty == JSON_RESULT_TY_ERR ||
          name.value.ty != JSON_VALUE_TY_STRING) {
        *result = name;
        return rd;
      }

      str.str += name_rd;
      str.len -= name_rd;
      rd += name_rd;

      rd += json_parse_ws(&str);

      if (!str.len || str.str[0] != ':') {
        *result =
            (struct json_result){.ty = JSON_RESULT_TY_ERR, .err = {.pos = rd}};
        return rd;
      }

      str.str++;
      str.len--;
      rd++;

      struct json_result value;
      size_t val_rd = json_parse_chunk(context, str, &value);

      str.str += val_rd;
      str.len -= val_rd;
      rd += val_rd;

      if (value.ty == JSON_RESULT_TY_ERR) {
        *result = value;
        return rd;
      }

      hashtbl_insert(fields, &name.value.str_val, &value.value);

      rd += json_parse_ws(&str);

      if (!str.len) {
        *result =
            (struct json_result){.ty = JSON_RESULT_TY_ERR, .err = {.pos = rd}};
        return rd;
      }
    }
  }

  default: {
    struct ap_val num_val;
    size_t parse_rd;
    if ((parse_rd = ap_val_try_parse_int(context->arena, 64, str, &num_val)) ||
        (parse_rd = ap_val_try_parse_float(context->arena, AP_FLOAT_TY_F64, str,
                                           &num_val))) {
      rd += parse_rd;
      *result = (struct json_result){
          .ty = JSON_RESULT_TY_VALUE,
          .value = {.ty = JSON_VALUE_TY_NUMBER, .num_val = num_val}};
      return rd;
    }
    TODO("other (%c)", ch);
  }
  }
}

struct json_result json_parse(struct sized_str str) {
  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  struct json_ctx ctx = {.arena = arena};

  struct json_result result;
  size_t rd = json_parse_chunk(&ctx, str, &result);

  // skip trailing whitespace
  while (rd < str.len && isspace(str.str[rd])) {
    rd++;
  }

  switch (result.ty) {
  case JSON_RESULT_TY_VALUE:
    return result;
  case JSON_RESULT_TY_ERR:
    if (rd != str.len) {
      return (struct json_result){.ty = JSON_RESULT_TY_ERR, .err = {.pos = rd}};
    }

    return result;
  }
}

struct json_print_context {
  FILE *file;
  size_t indent;
};

static void json_print_value_part(struct json_print_context *ctx,
                                  const struct json_value *value) {
  switch (value->ty) {
  case JSON_VALUE_TY_NULL:
    fprintf(ctx->file, "null");
    break;
  case JSON_VALUE_TY_BOOL:
    fprintf(ctx->file, value->bool_val ? "true" : "false");
    break;
  case JSON_VALUE_TY_NUMBER:
    ap_val_fprintf(ctx->file, value->num_val);
    break;
  case JSON_VALUE_TY_STRING:
    fprint_str(ctx->file, value->str_val.str, value->str_val.len);
    break;
  case JSON_VALUE_TY_ARRAY:
    fprintf(ctx->file, "[\n");

    ctx->indent++;
    for (size_t i = 0; i < value->arr_val.num_values; i++) {
      fprintf(ctx->file, "%*s", (int)(ctx->indent * 2), "");
      json_print_value_part(ctx, &value->arr_val.values[i]);
      fprintf(ctx->file, ",\n");
    }
    ctx->indent--;

    fprintf(ctx->file, "%*s", (int)(ctx->indent * 2), "");
    fprintf(ctx->file, "]");
    break;
  case JSON_VALUE_TY_OBJECT:
    fprintf(ctx->file, "{\n");

    ctx->indent++;
    struct hashtbl_iter *iter = hashtbl_iter(value->obj_val.fields);
    struct hashtbl_entry entry;
    while (hashtbl_iter_next(iter, &entry)) {
      const struct sized_str *name = entry.key;
      const struct json_value *field = entry.data;

      fprintf(ctx->file, "%*s", (int)(ctx->indent * 2), "");
      fprintf(ctx->file, "\"%.*s\": ", (int)name->len, name->str);
      json_print_value_part(ctx, field);
      fprintf(ctx->file, ",\n");
    }
    ctx->indent--;

    fprintf(ctx->file, "%*s", (int)(ctx->indent * 2), "");
    fprintf(ctx->file, "}");
    break;
  }
}

void json_print_value(FILE *file, const struct json_value *value) {
  struct json_print_context ctx = {.file = file, .indent = 0};

  json_print_value_part(&ctx, value);
  fprintf(file, "\n");
}

void json_print_result(FILE *file, const struct json_result *result) {
  switch (result->ty) {
  case JSON_RESULT_TY_VALUE:
    json_print_value(file, &result->value);
    break;
  case JSON_RESULT_TY_ERR:
    fprintf(file, "JSON parse err at pos %zu\n", result->err.pos);
    break;
  }
}

struct json_writer {
  struct vector *buffer;
  bool needs_sep;
};

struct json_writer *json_writer_create(void) {
  struct json_writer *writer = nonnull_malloc(sizeof(*writer));
  writer->buffer = vector_create(sizeof(char));
  writer->needs_sep = false;

  return writer;
}

void json_writer_free(struct json_writer **writer) {
  free(*writer);
  *writer = NULL;
}

struct sized_str json_writer_get_buf(struct json_writer *writer) {
  // NOTE: `clear` invalidates this buffer!

  return (struct sized_str){
      .str = vector_head(writer->buffer),
      .len = vector_length(writer->buffer),
  };
}

void json_writer_clear(struct json_writer *writer) {
  vector_clear(writer->buffer);
  writer->needs_sep = false;
}

void json_writer_write_null(struct json_writer *writer,
                            UNUSED struct json_null_t null) {
  if (writer->needs_sep) {
    vector_push_back(writer->buffer, &(char){','});
  }

  vector_extend(writer->buffer, "null", strlen("null"));
  writer->needs_sep = true;
}

void json_writer_write_bool(struct json_writer *writer, bool value) {
  if (writer->needs_sep) {
    vector_push_back(writer->buffer, &(char){','});
  }

  if (value) {
    vector_extend(writer->buffer, "true", strlen("true"));
  } else {
    vector_extend(writer->buffer, "false", strlen("false"));
  }

  writer->needs_sep = true;
}

PRINTF_ARGS(1)
static void json_writer_snprintf(struct json_writer *writer, const char *format,
                                 ...) {
  if (!format || !format[0]) {
    return;
  }

  va_list args, args_copy;

  va_start(args, format);
  va_copy(args_copy, args);

  int len = vsnprintf(NULL, 0, format, args_copy);

  va_end(args_copy);

  DEBUG_ASSERT(len >= 0, "vnsprintf call failed");

  size_t tail = vector_length(writer->buffer);
  vector_extend(writer->buffer, NULL, (size_t)len);

  vector_ensure_capacity(writer->buffer, vector_length(writer->buffer) + 1);

  vsnprintf(vector_get(writer->buffer, tail), len + 1, format, args);

  va_end(args);
}

void json_writer_write_integer(struct json_writer *writer, long long value) {
  if (writer->needs_sep) {
    vector_push_back(writer->buffer, &(char){','});
  }

  json_writer_snprintf(writer, "%lld", value);
  writer->needs_sep = true;
}

void json_writer_write_double(struct json_writer *writer, double value) {
  if (writer->needs_sep) {
    vector_push_back(writer->buffer, &(char){','});
  }

  json_writer_snprintf(writer, "%f", value);
  writer->needs_sep = true;
}

void json_writer_write_string(struct json_writer *writer,
                              struct sized_str value) {
  if (writer->needs_sep) {
    vector_push_back(writer->buffer, &(char){','});
  }

  size_t len = sprint_str(NULL, 0, value.str, value.len);

  size_t tail = vector_length(writer->buffer);
  vector_extend(writer->buffer, NULL, len);

  // ensure capacity is 1 greater, for the null char
  vector_ensure_capacity(writer->buffer, vector_length(writer->buffer) + 1);

  sprint_str(vector_get(writer->buffer, tail), len + 1, value.str, value.len);
  writer->needs_sep = true;
}

void json_writer_write_field_name(struct json_writer *writer,
                                  struct sized_str name) {
  json_writer_write_string(writer, name);

  writer->needs_sep = false;
  vector_push_back(writer->buffer, &(char){':'});
}

static void json_writer_start_agg(struct json_writer *writer, char ch) {
  if (writer->needs_sep) {
    vector_push_back(writer->buffer, &(char){','});
  }

  vector_push_back(writer->buffer, &ch);
  writer->needs_sep = false;
}

static void json_writer_end_agg(struct json_writer *writer, char ch) {
  vector_push_back(writer->buffer, &ch);
  writer->needs_sep = true;
}

void json_writer_write_begin_obj(struct json_writer *writer) {
  json_writer_start_agg(writer, '{');
}

void json_writer_write_end_obj(struct json_writer *writer) {
  json_writer_end_agg(writer, '}');
}

void json_writer_write_begin_arr(struct json_writer *writer) {
  json_writer_start_agg(writer, '[');
}

void json_writer_write_end_arr(struct json_writer *writer) {
  json_writer_end_agg(writer, ']');
}

const char *json_value_ty_name(enum json_value_ty ty) {
  static const char *names[] = {
      [JSON_VALUE_TY_NULL] = "null",     [JSON_VALUE_TY_BOOL] = "bool",
      [JSON_VALUE_TY_NUMBER] = "number", [JSON_VALUE_TY_STRING] = "string",
      [JSON_VALUE_TY_ARRAY] = "array",   [JSON_VALUE_TY_OBJECT] = "object",
  };

  size_t idx = ty;
  DEBUG_ASSERT(idx < ARR_LENGTH(names), "invalid json value ty");

  return names[idx];
}
