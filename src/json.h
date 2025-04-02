#ifndef JSON_H
#define JSON_H

#include "ap_val.h"
#include "hashtbl.h"

#define JSON_BOOL_FIELD
#define JSON_NUMBER_FIELD
#define JSON_STRING_FIELD
#define JSON_ARRAY_FIELD
#define JSON_OBJECT_FIELD

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
#define JSON_MK_BOOL(val) ((struct json_value){.ty = JSON_VALUE_TY_BOOL, .bool_val = (val)})
#define JSON_MK_STR(val) ((struct json_value){.ty = JSON_VALUE_TY_STRING, .str_val = (val)})
#define JSON_MK_STR_LIT(val) ((struct json_value){.ty = JSON_VALUE_TY_STRING, .str_val = MK_SIZED((val))})

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

struct json_result json_parse(struct sized_str str);
void json_print(FILE *file, const struct json_result *result);

#endif
