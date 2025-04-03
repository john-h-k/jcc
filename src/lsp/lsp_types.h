#ifndef LSP_LSP_TYPES_H
#define LSP_LSP_TYPES_H

#include "../json.h"
#include "../util.h"

#include <stddef.h>

struct client_caps {
  int _empty;
};

struct trace_val {
  int _empty;
};

struct init_params {
  size_t process_id;

  struct {
    struct sized_str name;
    struct sized_str version;
  } client_info;

  struct sized_str locale;
  struct sized_str root_path;
  struct sized_str root_uri;
  struct client_caps client_capabilities;
  struct trace_val trace;

  struct workspace_folder *workspace_folders;
  size_t num_workspace_folders;
};

struct text_doc {
  struct sized_str uri;
  struct sized_str language_id;
  size_t version;
  struct sized_str text;
};

struct text_doc_id {
  struct sized_str uri;
};

struct didopen_textdoc_params {
  struct text_doc text_doc;
};

struct didclose_textdoc_params {
  struct text_doc_id text_doc;
};

#define TRY_DE_SIZE_T(name, camel, req)                                        \
  do {                                                                         \
    struct sized_str name_str = MK_SIZED(camel);                               \
    struct json_value *val = hashtbl_lookup(object->fields, &name_str);        \
    if ((req && !val) || (val && val->ty != JSON_VALUE_TY_NUMBER)) {           \
      return false;                                                            \
    }                                                                          \
    if (val) {                                                                 \
      name = ap_val_as_size_t(val->num_val);                                   \
    } else {                                                                   \
      memset(&name, 0, sizeof(name));                                           \
    }                                                                          \
  } while (0);

#define TRY_DE_STR(name, camel, req)                                           \
  do {                                                                         \
    struct sized_str name_str = MK_SIZED(camel);                               \
    struct json_value *val = hashtbl_lookup(object->fields, &name_str);        \
    if ((req && !val) || (val && val->ty != JSON_VALUE_TY_STRING)) {           \
      return false;                                                            \
    }                                                                          \
    if (val) {                                                                 \
      name = val->str_val;                                                     \
    } else {                                                                   \
      memset(&name, 0, sizeof(name));                                           \
    }                                                                          \
  } while (0);

#define TRY_DE_VALUE(name, camel, req)                                         \
  do {                                                                         \
    struct sized_str name_str = MK_SIZED(camel);                               \
    const struct json_value *val = hashtbl_lookup(object->fields, &name_str);  \
    if (req && !val) {                                                         \
      return false;                                                            \
    }                                                                          \
    name = val;                                                                \
  } while (0);

#define TRY_DE_OBJECT(name, camel, req)                                        \
  do {                                                                         \
    struct sized_str name_str = MK_SIZED(camel);                               \
    const struct json_value *val = hashtbl_lookup(object->fields, &name_str);  \
    if ((req && !val) || (val && val->ty != JSON_VALUE_TY_OBJECT)) {           \
      return false;                                                            \
    }                                                                          \
    name = val ? &val->obj_val : NULL;                                         \
  } while (0);

#define TRY_DE_TYPE(ty_name, name, camel, req)                                 \
  do {                                                                         \
    struct sized_str name_str = MK_SIZED(camel);                               \
    const struct json_value *val = hashtbl_lookup(object->fields, &name_str);  \
    if ((req && !val) || (val && val->ty != JSON_VALUE_TY_OBJECT)) {           \
      return false;                                                            \
    }                                                                          \
    if (val && !try_de_##ty_name(val, &name)) {                                \
      return false;                                                            \
    } else {                                                                   \
      memset(&name, 0, sizeof(name));                                           \
    }                                                                          \
  } while (0);

enum req_msg_method {
  REQ_MSG_METHOD_INITIALIZE,
  REQ_MSG_METHOD_INITIALIZED,
  REQ_MSG_METHOD_SHUTDOWN,
  REQ_MSG_METHOD_EXIT,

  REQ_MSG_METHOD_TEXTDOCUMENT_DIDOPEN,
  REQ_MSG_METHOD_TEXTDOCUMENT_DIDCLOSE,
};

struct req_msg {
  size_t id;
  enum req_msg_method method;

  union {
    struct init_params init_params;
    struct didopen_textdoc_params didopen_textdoc_params;
    struct didclose_textdoc_params didclose_textdoc_params;
  };
};

bool try_de_req_msg(const struct json_value *value, struct req_msg *msg);

#endif
