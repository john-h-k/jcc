#include "json.h"

#include "alloc.h"
#include "ap_val.h"
#include "hashtbl.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>

struct json_context {
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

static size_t json_parse_chunk(struct json_context *context,
                               struct sized_str str,
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

    // TODO: process string for escapes
    *result = (struct json_result){.ty = JSON_RESULT_TY_VALUE,
                                   .value = JSON_MK_STR(val)};
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

  struct json_context ctx = {.arena = arena};

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

static void json_print_value(struct json_print_context *ctx,
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
      json_print_value(ctx, &value->arr_val.values[i]);
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
      json_print_value(ctx, field);
      fprintf(ctx->file, ",\n");
    }
    ctx->indent--;

    fprintf(ctx->file, "%*s", (int)(ctx->indent * 2), "");
    fprintf(ctx->file, "}");
    break;
  }
}

void json_print(FILE *file, const struct json_result *result) {
  struct json_print_context ctx = {.file = file, .indent = 0};

  switch (result->ty) {
  case JSON_RESULT_TY_VALUE:
    json_print_value(&ctx, &result->value);
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
  return (struct sized_str){
      .str = vector_head(writer->buffer),
      .len = vector_length(writer->buffer),
  };
}

void json_writer_write_null(struct json_writer *writer) {
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
  va_list args, args_copy;

  va_start(args, format);
  va_copy(args_copy, args);

  int len = vsnprintf(NULL, 0, format, args_copy);

  va_end(args_copy);

  DEBUG_ASSERT(len > 0, "vnsprintf call failed");

  size_t tail = vector_length(writer->buffer);
  vector_extend(writer->buffer, NULL, len);

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
