#ifndef LSP_LSP_TYPES_H
#define LSP_LSP_TYPES_H

#include "../json.h"
#include "../program.h"
#include "../util.h"

#include <stddef.h>

typedef int64_t lsp_integer;

enum pos_encoding_kind {
  POS_ENCODING_KIND_UTF8,
  POS_ENCODING_KIND_UTF16,
  POS_ENCODING_KIND_UTF32,
};

enum text_doc_sync_kind {
  TEXT_DOC_SYNC_KIND_NONE = 0,
  TEXT_DOC_SYNC_KIND_FULL = 1,
  TEXT_DOC_SYNC_KIND_INCREMENTAL = 2,
};

struct general_caps {
  struct {
    bool cancel;
    //   // string[] retryOnContentModified
  } stale_req_support;

  size_t num_position_encodings;
  enum pos_encoding_kind *position_encodings;
};

enum diagnostic_tag {
  DIAGNOSTIC_TAG_UNNECESSARY = 1,
  DIAGNOSTIC_TAG_DEPRECATED = 2,
};

enum diagnostic_severity {
  DIAGNOSTIC_SEVERITY_ERROR = 1,
  DIAGNOSTIC_SEVERITY_WARNING = 2,
  DIAGNOSTIC_SEVERITY_INFO = 3,
  DIAGNOSTIC_SEVERITY_HINT = 4,
};

struct pub_diagnostics_caps {
  bool related_info;

  struct {
    size_t num_values;
    enum diagnostic_tag *values;
  } tag_support;

  bool version_support;
  bool code_desc_support;
  bool data_support;
};

struct definition_caps {
  bool dynamic_registration;
  bool link_support;
};

struct type_definition_caps {
  bool dynamic_registration;
  bool link_support;
};

struct text_doc_caps {
  struct pub_diagnostics_caps publish_diagnostics;
  struct definition_caps definitions;
  struct type_definition_caps type_definitions;
};

struct window_caps {
  bool work_done_progress;
  // showMessage?: ShowMessageRequestClientCapabilities;
  // showDocument?: ShowDocumentClientCapabilities;
};

struct client_caps {
  // workspace

  // FIXME: this (and many other fields) are optional and we need to represent
  // that we could do classic pointer, or a field within them, or a field
  // alongside them
  struct text_doc_caps text_doc_caps;
  // struct notebook_caps notebook__doc;
  struct window_caps window_caps;
  struct general_caps general_caps;
};

struct trace_val {
  int _empty;
};

struct init_params {
  lsp_integer process_id;

  struct {
    ustr_t name;
    ustr_t version;
  } client_info;

  ustr_t locale;
  ustr_t root_path;
  ustr_t root_uri;
  struct client_caps client_capabilities;
  struct trace_val trace;

  struct workspace_folder *workspace_folders;
  size_t num_workspace_folders;
};

struct text_doc {
  ustr_t uri;
  ustr_t language_id;
  lsp_integer version;
  ustr_t text;
};

struct text_doc_id {
  ustr_t uri;
};

struct ver_text_doc_id {
  ustr_t uri;
  lsp_integer version;
};

struct didopen_textdoc_params {
  struct text_doc text_doc;
};

enum text_doc_change_ev_ty {
  TEXT_DOC_CHANGE_EV_TY_INCREMENTAL,
  TEXT_DOC_CHANGE_EV_TY_FULL,
};

struct text_doc_change_ev {
  enum text_doc_change_ev_ty ty;

  struct text_span span;
  lsp_integer range_length;

  ustr_t text;
};

struct didchange_textdoc_params {
  struct ver_text_doc_id text_doc;

  size_t num_changes;
  struct text_doc_change_ev *changes;
};

struct didclose_textdoc_params {
  struct text_doc_id text_doc;
};

struct definition_textdoc_params {
  struct text_doc_id text_doc;
  struct text_pos pos;
};

struct type_definition_textdoc_params {
  struct text_doc_id text_doc;
  struct text_pos pos;
};

#define DE_FAIL(...) BUG(__VA_ARGS__)

#define TRY_DE_OBJECT(from, to, ...)                                           \
  do {                                                                         \
    if ((from)->ty != JSON_VALUE_TY_OBJECT) {                                  \
      DE_FAIL("expected object but found %s", json_value_ty_name((from)->ty)); \
      return false;                                                            \
    }                                                                          \
                                                                               \
    (to) = __VA_ARGS__(from)->obj_val;                                         \
  } while (0)

#define TRY_DE_INT(from, to)                                                   \
  do {                                                                         \
    if ((from)->ty != JSON_VALUE_TY_NUMBER) {                                  \
      DE_FAIL("expected int but found %s", json_value_ty_name((from)->ty));    \
      return false;                                                            \
    }                                                                          \
                                                                               \
    (to) = ap_val_as_ll((from)->num_val);                                      \
  } while (0)

#define TRY_DE_STR(from, to)                                                   \
  do {                                                                         \
    if ((from)->ty != JSON_VALUE_TY_STRING) {                                  \
      DE_FAIL("expected string but found %s", json_value_ty_name((from)->ty)); \
      return false;                                                            \
    }                                                                          \
                                                                               \
    (to) = (from)->str_val;                                                    \
  } while (0)

#define TRY_DE_BOOL(from, to)                                                  \
  do {                                                                         \
    if ((from)->ty != JSON_VALUE_TY_BOOL) {                                    \
      DE_FAIL("expected bool but found %s", json_value_ty_name((from)->ty));   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    (to) = (from)->bool_val;                                                   \
  } while (0)

#define TRY_DE_TYPE(from, to, ty_name)                                         \
  do {                                                                         \
    if (!try_de_##ty_name(ctx, (from), (to))) {                                \
      DE_FAIL("deserialization of type '" #ty_name "' failed");                \
      return false;                                                            \
    }                                                                          \
  } while (0)

#define TRY_DE_INT_FIELD(object, name, camel, req)                             \
  do {                                                                         \
    ustr_t name_str = MK_USTR(camel);                               \
    struct json_value *val = hashtbl_lookup(object->fields, &name_str);        \
                                                                               \
    if (req && !val) {                                                         \
      DE_FAIL("field '%s' required but not present", camel);                   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val && val->ty != JSON_VALUE_TY_NUMBER) {                              \
      DE_FAIL("expected number but found %s", json_value_ty_name(val->ty));    \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val) {                                                                 \
      name = ap_val_as_ll(val->num_val);                                       \
    } else {                                                                   \
      memset(&name, 0, sizeof(name));                                          \
    }                                                                          \
  } while (0)

#define TRY_DE_UINT_FIELD(object, name, camel, req)                            \
  do {                                                                         \
    ustr_t name_str = MK_USTR(camel);                               \
    struct json_value *val = hashtbl_lookup(object->fields, &name_str);        \
                                                                               \
    if (req && !val) {                                                         \
      DE_FAIL("field '%s' required but not present", camel);                   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val && val->ty != JSON_VALUE_TY_NUMBER) {                              \
      DE_FAIL("expected number but found %s", json_value_ty_name(val->ty));    \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val) {                                                                 \
      name = ap_val_as_ull(val->num_val);                                      \
    } else {                                                                   \
      memset(&name, 0, sizeof(name));                                          \
    }                                                                          \
  } while (0)

#define TRY_DE_STR_FIELD(object, name, camel, req)                             \
  do {                                                                         \
    ustr_t name_str = MK_USTR(camel);                               \
    struct json_value *val = hashtbl_lookup(object->fields, &name_str);        \
                                                                               \
    if (req && !val) {                                                         \
      DE_FAIL("field '%s' required but not present", camel);                   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val && val->ty != JSON_VALUE_TY_STRING) {                              \
      DE_FAIL("expected string but found %s", json_value_ty_name(val->ty));    \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val) {                                                                 \
      name = val->str_val;                                                     \
    } else {                                                                   \
      memset(&name, 0, sizeof(name));                                          \
    }                                                                          \
  } while (0)

#define TRY_DE_BOOL_FIELD(object, name, camel, req)                            \
  do {                                                                         \
    ustr_t name_str = MK_USTR(camel);                               \
    struct json_value *val = hashtbl_lookup(object->fields, &name_str);        \
                                                                               \
    if (req && !val) {                                                         \
      DE_FAIL("field '%s' required but not present", camel);                   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val && val->ty != JSON_VALUE_TY_BOOL) {                                \
      DE_FAIL("expected bool but found %s", json_value_ty_name(val->ty));      \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val) {                                                                 \
      name = val->bool_val;                                                    \
    } else {                                                                   \
      name = false;                                                            \
    }                                                                          \
  } while (0)

#define TRY_DE_VALUE_FIELD(object, name, camel, req)                           \
  do {                                                                         \
    ustr_t name_str = MK_USTR(camel);                               \
    const struct json_value *val = hashtbl_lookup(object->fields, &name_str);  \
                                                                               \
    if (req && !val) {                                                         \
      DE_FAIL("field '%s' required but not present", camel);                   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    name = val;                                                                \
  } while (0)

#define TRY_DE_OBJECT_FIELD(object, name, camel, req)                          \
  do {                                                                         \
    ustr_t name_str = MK_USTR(camel);                               \
    const struct json_value *val = hashtbl_lookup(object->fields, &name_str);  \
                                                                               \
    if (req && !val) {                                                         \
      DE_FAIL("field '%s' required but not present", camel);                   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val && val->ty != JSON_VALUE_TY_OBJECT) {                              \
      DE_FAIL("expected object but found %s", json_value_ty_name(val->ty));    \
      return false;                                                            \
    }                                                                          \
                                                                               \
    name = val ? &val->obj_val : NULL;                                         \
  } while (0)

#define TRY_DE_TYPE_FIELD(object, ty_name, name, camel, req)                   \
  do {                                                                         \
    ustr_t name_str = MK_USTR(camel);                               \
    const struct json_value *val = hashtbl_lookup(object->fields, &name_str);  \
                                                                               \
    if (req && !val) {                                                         \
      DE_FAIL("field '%s' required but not present", camel);                   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val && !try_de_##ty_name(ctx, val, &name)) {                           \
      return false;                                                            \
    }                                                                          \
  } while (0)

// if using `TYPE` you pass the type as a variadic arg
#define TRY_DE_ARRAY_FIELD(de_ty, object, name, num_name, camel, req, ...)     \
  do {                                                                         \
    ustr_t name_str = MK_USTR(camel);                               \
    const struct json_value *val = hashtbl_lookup(object->fields, &name_str);  \
                                                                               \
    if (req && !val) {                                                         \
      DE_FAIL("field '%s' required but not present", camel);                   \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (val && val->ty != JSON_VALUE_TY_ARRAY) {                               \
      DE_FAIL("expected array but found %s", json_value_ty_name(val->ty));     \
      return false;                                                            \
    }                                                                          \
                                                                               \
    if (!val) {                                                                \
      name = NULL;                                                             \
      num_name = 0;                                                            \
      break;                                                                   \
    }                                                                          \
                                                                               \
    const struct json_array *arr = &val->arr_val;                              \
    char *arr_vals = arena_alloc(ctx->arena, sizeof(*name) * arr->num_values); \
    for (size_t i = 0; i < arr->num_values; i++) {                             \
      TRY_DE_##de_ty(&arr->values[i], (void *)&arr_vals[i * sizeof(*name)],    \
                     __VA_ARGS__);                                             \
    }                                                                          \
                                                                               \
    name = (void *)arr_vals;                                                   \
    num_name = arr->num_values;                                                \
  } while (0)

#define REQ_METHODS                                                            \
  REQ_METHOD(INITIALIZE, "initialize")                                         \
  REQ_METHOD(INITIALIZED, "initialized")                                       \
  REQ_METHOD(SHUTDOWN, "shutdown")                                             \
  REQ_METHOD(EXIT, "exit")                                                     \
                                                                               \
  REQ_METHOD(TEXTDOCUMENT_DIDOPEN, "textDocument/didOpen")                     \
  REQ_METHOD(TEXTDOCUMENT_DIDCHANGE, "textDocument/didChange")                 \
  REQ_METHOD(TEXTDOCUMENT_DIDCLOSE, "textDocument/didClose")                   \
  REQ_METHOD(TEXTDOCUMENT_DEFINITION, "textDocument/definition")               \
  REQ_METHOD(TEXTDOCUMENT_TYPEDEFINITION, "textDocument/typeDefinition")

#define REQ_METHOD(name, _) REQ_MSG_METHOD_##name,
enum req_msg_method { REQ_METHODS };
#undef REQ_METHOD

struct req_msg {
  lsp_integer id;
  enum req_msg_method method;

  union {
    struct init_params init_params;
    struct didopen_textdoc_params didopen_textdoc_params;
    struct didchange_textdoc_params didchange_textdoc_params;
    struct didclose_textdoc_params didclose_textdoc_params;
    struct definition_textdoc_params definition_textdoc_params;
    struct type_definition_textdoc_params type_definition_textdoc_params;
  };
};

struct json_de_ctx {
  struct arena_allocator *arena;
};

bool try_de_req_msg(struct json_de_ctx *ctx, const struct json_value *value,
                    struct req_msg *msg);

#endif
