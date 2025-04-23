#include "lsp_types.h"

#include "../log.h"

#define TRY_DE_FN(tag, ty, name)                                               \
  START_NO_UNUSED_ARGS                                                         \
  static bool try_de_##ty(MAYBE_UNUSED struct json_de_ctx *ctx,                \
                          const struct json_value *value, tag ty *name)        \
      END_NO_UNUSED_ARGS

#define TRY_DE_ENUM(ty, name) TRY_DE_FN(enum, ty, name)
#define TRY_DE_STRUCT(ty, name) TRY_DE_FN(struct, ty, name)

TRY_DE_STRUCT(text_pos, pos) {
  *pos = (struct text_pos){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_UINT_FIELD(object, pos->line, "line", true);
  TRY_DE_UINT_FIELD(object, pos->col, "character", true);

  return true;
}

TRY_DE_STRUCT(text_span, span) {
  // does not fill out the `file` field... maybe we should be using a diff type

  *span = (struct text_span){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_TYPE_FIELD(object, text_pos, span->start, "start", true);
  TRY_DE_TYPE_FIELD(object, text_pos, span->end, "end", true);

  return true;
}

TRY_DE_STRUCT(definition_textdoc_params, params) {
  *params = (struct definition_textdoc_params){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  const struct json_object *doc;
  TRY_DE_OBJECT_FIELD(object, doc, "textDocument", true);
  TRY_DE_STR_FIELD(doc, params->text_doc.uri, "uri", true);

  TRY_DE_TYPE_FIELD(object, text_pos, params->pos, "position", true);

  return true;
}

TRY_DE_STRUCT(type_definition_textdoc_params, params) {
  *params = (struct type_definition_textdoc_params){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  const struct json_object *doc;
  TRY_DE_OBJECT_FIELD(object, doc, "textDocument", true);
  TRY_DE_STR_FIELD(doc, params->text_doc.uri, "uri", true);

  TRY_DE_TYPE_FIELD(object, text_pos, params->pos, "position", true);

  return true;
}

TRY_DE_STRUCT(text_doc_change_ev, ev) {
  *ev = (struct text_doc_change_ev){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_STR_FIELD(object, ev->text, "text", true);

  // FIXME: we do a double lookup here
  if (hashtbl_lookup(object->fields, &MK_USTR("range"))) {
    ev->ty = TEXT_DOC_CHANGE_EV_TY_INCREMENTAL;
    TRY_DE_TYPE_FIELD(object, text_span, ev->span, "range", true);
    TRY_DE_INT_FIELD(object, ev->range_length, "rangeLength", false);
  } else {
    ev->ty = TEXT_DOC_CHANGE_EV_TY_FULL;
  }

  return true;
}

TRY_DE_STRUCT(didchange_textdoc_params, params) {
  *params = (struct didchange_textdoc_params){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  const struct json_object *doc;
  TRY_DE_OBJECT_FIELD(object, doc, "textDocument", true);

  TRY_DE_STR_FIELD(doc, params->text_doc.uri, "uri", true);
  TRY_DE_INT_FIELD(doc, params->text_doc.version, "version", true);

  TRY_DE_ARRAY_FIELD(TYPE, object, params->changes, params->num_changes,
                     "contentChanges", true, text_doc_change_ev);

  return true;
}

TRY_DE_STRUCT(didopen_textdoc_params, params) {
  *params = (struct didopen_textdoc_params){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  const struct json_object *doc;
  TRY_DE_OBJECT_FIELD(object, doc, "textDocument", true);

  TRY_DE_STR_FIELD(doc, params->text_doc.uri, "uri", true);
  TRY_DE_STR_FIELD(doc, params->text_doc.language_id, "languageId", true);
  TRY_DE_INT_FIELD(doc, params->text_doc.version, "version", true);
  TRY_DE_STR_FIELD(doc, params->text_doc.text, "text", true);

  return true;
}

TRY_DE_STRUCT(didclose_textdoc_params, params) {
  *params = (struct didclose_textdoc_params){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  const struct json_object *doc;
  TRY_DE_OBJECT_FIELD(object, doc, "textDocument", true);

  TRY_DE_STR_FIELD(doc, params->text_doc.uri, "uri", true);

  return true;
}

TRY_DE_ENUM(diagnostic_tag, tag) {
  long long tag_val;
  TRY_DE_INT(value, tag_val);

  switch (tag_val) {
  case DIAGNOSTIC_TAG_DEPRECATED:
    *tag = DIAGNOSTIC_TAG_DEPRECATED;
    return true;
  case DIAGNOSTIC_TAG_UNNECESSARY:
    *tag = DIAGNOSTIC_TAG_UNNECESSARY;
    return true;
  default:
    return false;
  }
}

TRY_DE_STRUCT(pub_diagnostics_caps, caps) {
  *caps = (struct pub_diagnostics_caps){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  const struct json_object *tag_support;
  TRY_DE_OBJECT_FIELD(object, tag_support, "tagSupport", false);

  if (tag_support) {
    TRY_DE_ARRAY_FIELD(TYPE, tag_support, caps->tag_support.values,
                       caps->tag_support.num_values, "valueSet", false,
                       diagnostic_tag);
  }

  TRY_DE_BOOL_FIELD(object, caps->related_info, "relatedInformation", false);
  TRY_DE_BOOL_FIELD(object, caps->version_support, "versionSupport", false);
  TRY_DE_BOOL_FIELD(object, caps->code_desc_support, "codeDescriptionSupport",
                    false);
  TRY_DE_BOOL_FIELD(object, caps->data_support, "dataSupport", false);

  return true;
}

TRY_DE_STRUCT(definition_caps, caps) {
  *caps = (struct definition_caps){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_BOOL_FIELD(object, caps->dynamic_registration, "dynamicRegistration",
                    false);
  TRY_DE_BOOL_FIELD(object, caps->link_support, "linkSupport", false);

  return true;
}

TRY_DE_STRUCT(type_definition_caps, caps) {
  *caps = (struct type_definition_caps){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_BOOL_FIELD(object, caps->dynamic_registration, "dynamicRegistration",
                    false);
  TRY_DE_BOOL_FIELD(object, caps->link_support, "linkSupport", false);

  return true;
}

TRY_DE_STRUCT(text_doc_caps, caps) {
  *caps = (struct text_doc_caps){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_TYPE_FIELD(object, pub_diagnostics_caps, caps->publish_diagnostics,
                    "publishDiagnostics", false);
  TRY_DE_TYPE_FIELD(object, definition_caps, caps->definitions, "definition",
                    false);
  TRY_DE_TYPE_FIELD(object, type_definition_caps, caps->type_definitions,
                    "typeDefinition", false);

  return true;
}

TRY_DE_STRUCT(window_caps, caps) {
  *caps = (struct window_caps){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_BOOL_FIELD(object, caps->work_done_progress, "workDoneProgress",
                    false);

  return true;
}

TRY_DE_ENUM(pos_encoding_kind, kind) {
  ustr_t ident;
  TRY_DE_STR(value, ident);

  if (ustr_eq(ident, MK_USTR("utf-8"))) {
    *kind = POS_ENCODING_KIND_UTF8;
    return true;
  }

  if (ustr_eq(ident, MK_USTR("utf-16"))) {
    *kind = POS_ENCODING_KIND_UTF16;
    return true;
  }

  if (ustr_eq(ident, MK_USTR("utf-32"))) {
    *kind = POS_ENCODING_KIND_UTF32;
    return true;
  }

  return false;
}

TRY_DE_STRUCT(general_caps, caps) {
  *caps = (struct general_caps){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  const struct json_object *stale_req;
  TRY_DE_OBJECT_FIELD(object, stale_req, "staleRequestSupport", false);

  if (stale_req) {
    TRY_DE_BOOL_FIELD(stale_req, caps->stale_req_support.cancel, "cancel",
                      true);
    // TRY_DE_STR_FIELD(stale_req, arams->client_info.version, "version",
    // false);
  }

  TRY_DE_ARRAY_FIELD(TYPE, object, caps->position_encodings,
                     caps->num_position_encodings, "positionEncodings", false,
                     pos_encoding_kind);

  return true;
}

TRY_DE_STRUCT(client_caps, caps) {
  *caps = (struct client_caps){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_TYPE_FIELD(object, text_doc_caps, caps->text_doc_caps, "textDocument",
                    false);
  TRY_DE_TYPE_FIELD(object, window_caps, caps->window_caps, "window", false);
  TRY_DE_TYPE_FIELD(object, general_caps, caps->general_caps, "general", false);

  return true;
}

TRY_DE_STRUCT(init_params, params) {
  *params = (struct init_params){0};

  const struct json_object *object;
  TRY_DE_OBJECT(value, object, &);

  TRY_DE_INT_FIELD(object, params->process_id, "processId", true);

  const struct json_object *client_info;
  TRY_DE_OBJECT_FIELD(object, client_info, "clientInfo", false);

  if (client_info) {
    TRY_DE_STR_FIELD(client_info, params->client_info.name, "name", true);
    TRY_DE_STR_FIELD(client_info, params->client_info.version, "version",
                     false);
  }

  TRY_DE_STR_FIELD(object, params->locale, "locale", false);
  TRY_DE_STR_FIELD(object, params->root_path, "rootPath", false);
  TRY_DE_STR_FIELD(object, params->root_uri, "rootUri", false);

  // TRY_DE_STR_FIELD(params->init_opts, "initializationOptions", false);
  TRY_DE_TYPE_FIELD(object, client_caps, params->client_capabilities,
                    "capabilities", true);

  // TRY_DE_TYPE_FIELD(trace, params->trace, "trace", false);
  // TRY_DE_TYPE_FIELD(workspace_folders, params->workspace_folders,
  // "workspaceFolders", false);

  return true;
}

static struct hashtbl *METHODS = NULL;

bool try_de_req_msg(struct json_de_ctx *ctx, const struct json_value *value,
                    struct req_msg *msg) {
  if (!METHODS) {
    debug("building method table");

    METHODS = hashtbl_create_ustr_keyed(sizeof(enum req_msg_method));

#define METHOD(name, ty)                                                       \
  do {                                                                         \
    ustr_t k = {                                                               \
        .str = name,                                                           \
        .len = strlen(name),                                                   \
    };                                                                         \
    enum req_msg_method v = ty;                                                \
    hashtbl_insert(METHODS, &k, &v);                                           \
  } while (0)

#define REQ_METHOD(name, str) METHOD(str, REQ_MSG_METHOD_##name);
    REQ_METHODS
#undef REQ_METHOD
  }

  *msg = (struct req_msg){0};

  if (value->ty != JSON_VALUE_TY_OBJECT) {
    return false;
  }

  const struct json_object *object = &value->obj_val;

  TRY_DE_INT_FIELD(object, msg->id, "id", false);

  ustr_t method;
  TRY_DE_STR_FIELD(object, method, "method", true);

  const struct json_value *params;
  TRY_DE_VALUE_FIELD(object, params, "params", false);

  enum req_msg_method *p = hashtbl_lookup(METHODS, &method);
  if (!p) {
    BUG("unknown message '%.*s'", (int)method.len, method.str);
    return false;
  }

  msg->method = *p;

  switch (msg->method) {
  case REQ_MSG_METHOD_INITIALIZE:
    return try_de_init_params(ctx, params, &msg->init_params);
  case REQ_MSG_METHOD_INITIALIZED:
  case REQ_MSG_METHOD_SHUTDOWN:
  case REQ_MSG_METHOD_EXIT:
    return msg;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDOPEN:
    return try_de_didopen_textdoc_params(ctx, params,
                                         &msg->didopen_textdoc_params);
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDCHANGE:
    return try_de_didchange_textdoc_params(ctx, params,
                                           &msg->didchange_textdoc_params);
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDCLOSE:
    return try_de_didclose_textdoc_params(ctx, params,
                                          &msg->didclose_textdoc_params);
  case REQ_MSG_METHOD_TEXTDOCUMENT_DEFINITION:
    return try_de_definition_textdoc_params(ctx, params,
                                            &msg->definition_textdoc_params);
  case REQ_MSG_METHOD_TEXTDOCUMENT_TYPEDEFINITION:
    return try_de_type_definition_textdoc_params(
        ctx, params, &msg->type_definition_textdoc_params);
  }
}
