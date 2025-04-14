#ifndef JSON_H
#define JSON_H

#include "ap_val.h"
#include "hashtbl.h"

typedef struct {
  char placeholder;
} json_null_t;

#define JSON_NULL ((json_null_t){0})

struct json_array {
  struct json_value *values;
  size_t num_values;
};

struct json_object {
  // key:   `ustr_t`
  // value: `struct json_value`
  // empty object CAN (but is not guaranteed to) have NULL value here
  struct hashtbl *fields;
};

enum json_value_ty {
  JSON_VALUE_TY_NULL,
  JSON_VALUE_TY_BOOL,
  JSON_VALUE_TY_NUMBER,
  JSON_VALUE_TY_STRING,
  JSON_VALUE_TY_ARRAY,
  JSON_VALUE_TY_OBJECT,
};

struct json_value {
  enum json_value_ty ty;

  union {
    bool bool_val;
    struct ap_val num_val;
    ustr_t str_val;
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
  ((struct json_value){.ty = JSON_VALUE_TY_STRING, .str_val = MK_USTR((val))})

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

struct json_result json_parse(ustr_t str);
void json_print_result(FILE *file, const struct json_result *result);
void json_print_value(FILE *file, const struct json_value *value);

const char *json_value_ty_name(enum json_value_ty ty);

struct json_writer;

struct json_writer *json_writer_create(void);
void json_writer_free(struct json_writer **writer);

void json_writer_write_null(struct json_writer *writer,
                            UNUSED json_null_t null);
void json_writer_write_bool(struct json_writer *writer, bool value);

void json_writer_write_integer(struct json_writer *writer, long long value);
void json_writer_write_double(struct json_writer *writer, double value);

void json_writer_write_string(struct json_writer *writer, ustr_t value);

void json_writer_write_begin_obj(struct json_writer *writer);
void json_writer_write_field_name(struct json_writer *writer, ustr_t name);
void json_writer_write_end_obj(struct json_writer *writer);

void json_writer_write_begin_arr(struct json_writer *writer);
void json_writer_write_end_arr(struct json_writer *writer);

ustr_t json_writer_get_buf(struct json_writer *writer);

void json_writer_clear(struct json_writer *writer);

#define JSON_OBJECT(writer, name, block)                                       \
  do {                                                                         \
    if ((name) != NULL) {                                                      \
      JSON_WRITE_FIELD_NAME((writer), (name));                                 \
    }                                                                          \
    json_writer_write_begin_obj((writer));                                     \
    {block};                                                                   \
    json_writer_write_end_obj((writer));                                       \
  } while (0)

#define JSON_ARRAY(writer, name, block)                                        \
  do {                                                                         \
    if ((name) != NULL) {                                                      \
      JSON_WRITE_FIELD_NAME((writer), (name));                                 \
    }                                                                          \
    json_writer_write_begin_arr((writer));                                     \
    {block};                                                                   \
    json_writer_write_end_arr((writer));                                       \
  } while (0)

#define JSON_WRITE(writer, value)                                              \
  _Generic((value),                                                            \
                                                                               \
      /* Null */                                                               \
      json_null_t: json_writer_write_null,                              \
                                                                               \
      /* Bool */                                                               \
      bool: json_writer_write_bool,                                            \
                                                                               \
      /* Integer */                                                            \
      size_t: json_writer_write_integer,                                       \
      long: json_writer_write_integer,                                         \
      long long: json_writer_write_integer,                                    \
      unsigned: json_writer_write_integer,                                     \
      int: json_writer_write_integer,                                          \
                                                                               \
      /* Floating-point */                                                     \
      double: json_writer_write_double,                                        \
                                                                               \
      /* String */                                                             \
      ustr_t: json_writer_write_string)                                        \
                                                                               \
      ((writer), (value))

#define JSON_WRITE_FIELD_NAME(writer, name)                                    \
  do {                                                                         \
    json_writer_write_field_name((writer), MK_USTR((name)));                   \
  } while (0)

#define JSON_WRITE_FIELD(writer, name, value)                                  \
  do {                                                                         \
    json_writer_write_field_name((writer), MK_USTR((name)));                   \
    JSON_WRITE((writer), (value));                                             \
  } while (0)

#endif
