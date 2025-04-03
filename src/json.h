#ifndef JSON_H
#define JSON_H

#include "ap_val.h"
#include "hashtbl.h"

enum json_value_ty {
  JSON_VALUE_TY_NULL,
  JSON_VALUE_TY_BOOL,
  JSON_VALUE_TY_NUMBER,
  JSON_VALUE_TY_STRING,
  JSON_VALUE_TY_ARRAY,
  JSON_VALUE_TY_OBJECT,
};

struct json_array {
  struct json_value *values;
  size_t num_values;
};

struct json_object {
  // key:   `struct sized_str`
  // value: `struct json_value`
  // empty object CAN (but is not guaranteed to) have NULL value here
  struct hashtbl *fields;
};

struct json_value {
  enum json_value_ty ty;

  union {
    bool bool_val;
    struct ap_val num_val;
    struct sized_str str_val;
    struct json_array arr_val;
    struct json_object obj_val;
  };
};

#define JSON_MK_NULL() ((struct json_value){.ty = JSON_VALUE_TY_NULL})
#define JSON_MK_BOOL(val)                                                      \
  ((struct json_value){.ty = JSON_VALUE_TY_BOOL, .bool_val = (val)})
#define JSON_MK_STR(val)                                                       \
  ((struct json_value){.ty = JSON_VALUE_TY_STRING, .str_val = (val)})
#define JSON_MK_STR_LIT(val)                                                   \
  ((struct json_value){.ty = JSON_VALUE_TY_STRING, .str_val = MK_SIZED((val))})

struct json_err {
  size_t pos;
};

enum json_result_ty {
  JSON_RESULT_TY_VALUE,
  JSON_RESULT_TY_ERR,
};

struct json_result {
  enum json_result_ty ty;

  union {
    struct json_value value;
    struct json_err err;
  };
};

const char *json_value_ty_name(enum json_value_ty ty);

struct json_result json_parse(struct sized_str str);
void json_print_result(FILE *file, const struct json_result *result);
void json_print_value(FILE *file, const struct json_value *value);

struct json_writer;

struct json_writer *json_writer_create(void);
void json_writer_free(struct json_writer **writer);

void json_writer_write_null(struct json_writer *writer);
void json_writer_write_bool(struct json_writer *writer, bool value);

void json_writer_write_integer(struct json_writer *writer, long long value);
void json_writer_write_double(struct json_writer *writer, double value);

void json_writer_write_string(struct json_writer *writer,
                              struct sized_str value);

void json_writer_write_begin_obj(struct json_writer *writer);
void json_writer_write_field_name(struct json_writer *writer,
                                  struct sized_str name);
void json_writer_write_end_obj(struct json_writer *writer);

void json_writer_write_begin_arr(struct json_writer *writer);
void json_writer_write_end_arr(struct json_writer *writer);
struct sized_str json_writer_get_buf(struct json_writer *writer);
void json_writer_clear(struct json_writer *writer);

#ifdef __GNUC__
#define FIX_MACRO_BLOCK(block)                                                 \
  PUSH_NO_WARN("-Wcompound-token-split-by-macro");                             \
  PUSH_NO_WARN("-Wgnu-statement-expression-from-macro-expansion");             \
  (void)(block);                                                                     \
  POP_NO_WARN();                                                               \
  POP_NO_WARN();
#else
#define FIX_MACRO_BLOCK(block) block
#endif

#define JSON_OBJECT(writer, name, block)                                       \
  do {                                                                         \
    if ((name) != NULL) {                                                      \
      JSON_WRITE_FIELD_NAME((writer), (name));                                 \
    }                                                                          \
    json_writer_write_begin_obj((writer));                                     \
    FIX_MACRO_BLOCK(block);                                                    \
    json_writer_write_end_obj((writer));                                       \
  } while (0);

#define JSON_ARRAY(writer, name, block)                                        \
  do {                                                                         \
    if ((name) != NULL) {                                                      \
      JSON_WRITE_FIELD_NAME((writer), (name));                                 \
    }                                                                          \
    json_writer_write_begin_arr((writer));                                     \
    FIX_MACRO_BLOCK(block);                                                    \
    json_writer_write_end_arr((writer));                                       \
  } while (0);

#define JSON_WRITE(writer, value)                                              \
  _Generic((value),                                                            \
      bool: json_writer_write_bool,                                            \
      size_t: json_writer_write_integer,                                       \
      long long: json_writer_write_integer,                                    \
      double: json_writer_write_double,                                        \
      struct sized_str: json_writer_write_string)((writer), (value))

#define JSON_WRITE_FIELD_NAME(writer, name)                                    \
  do {                                                                         \
    json_writer_write_field_name((writer), MK_SIZED((name)));                  \
  } while (0);

#define JSON_WRITE_FIELD(writer, name, value)                                  \
  do {                                                                         \
    json_writer_write_field_name((writer), MK_SIZED((name)));                  \
    JSON_WRITE((writer), (value));                                             \
  } while (0);

#endif
