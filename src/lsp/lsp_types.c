#include "lsp_types.h"

#include "../log.h"

static bool try_de_client_caps(const struct json_value *value,
                               struct client_caps *caps) {
  *caps = (struct client_caps){0};

  if (value->ty != JSON_VALUE_TY_OBJECT) {
    fprintf(stderr, "not obj\n");
    return false;
  }

  // const struct json_object *object = &value->obj_val;
  return true;
}

static bool
try_de_didopen_textdoc_params(const struct json_value *value,
                              struct didopen_textdoc_params *params) {
  *params = (struct didopen_textdoc_params){0};

  json_print_value(stderr, value);

  if (value->ty != JSON_VALUE_TY_OBJECT) {
    return false;
  }

  const struct json_object *object = &value->obj_val;

  const struct json_object *doc;
  TRY_DE_OBJECT(doc, "textDocument", true);

  object = doc;

  TRY_DE_STR(params->text_doc.uri, "uri", true);
  TRY_DE_STR(params->text_doc.language_id, "languageId", true);
  TRY_DE_SIZE_T(params->text_doc.version, "version", true);
  TRY_DE_STR(params->text_doc.text, "text", true);

  return true;
}

static bool
try_de_didclose_textdoc_params(const struct json_value *value,
                              struct didclose_textdoc_params *params) {
  *params = (struct didclose_textdoc_params){0};

  json_print_value(stderr, value);

  if (value->ty != JSON_VALUE_TY_OBJECT) {
    return false;
  }

  const struct json_object *object = &value->obj_val;

  const struct json_object *doc;
  TRY_DE_OBJECT(doc, "textDocument", true);

  object = doc;

  TRY_DE_STR(params->text_doc.uri, "uri", true);

  return true;
}

static bool try_de_init_params(const struct json_value *value,
                               struct init_params *params) {
  *params = (struct init_params){0};

  if (value->ty != JSON_VALUE_TY_OBJECT) {
    return false;
  }

  const struct json_object *object = &value->obj_val;

  TRY_DE_SIZE_T(params->process_id, "processId", true);

  const struct json_object *client_info;
  TRY_DE_OBJECT(client_info, "clientInfo", false);

  if (client_info) {
    const struct json_object *outer = object;
    object = client_info;

    TRY_DE_STR(params->client_info.name, "name", true);
    TRY_DE_STR(params->client_info.version, "version", false);

    object = outer;
  }

  TRY_DE_STR(params->locale, "locale", false);
  TRY_DE_STR(params->root_path, "rootPath", false);
  TRY_DE_STR(params->root_uri, "rootUri", false);

  // TRY_DE_STR(params->init_opts, "initializationOptions", false);
  TRY_DE_TYPE(client_caps, params->client_capabilities, "capabilities", true);

  // TRY_DE_TYPE(trace, params->trace, "trace", false);
  // TRY_DE_TYPE(workspace_folders, params->workspace_folders,
  // "workspaceFolders", false);

  return true;
}

static struct hashtbl *METHODS = NULL;

bool try_de_req_msg(const struct json_value *value, struct req_msg *msg) {
  if (!METHODS) {
    debug("building method table");

    METHODS = hashtbl_create_sized_str_keyed(sizeof(enum req_msg_method));

#define METHOD(name, ty)                                                       \
  do {                                                                         \
    struct sized_str k = {                                                     \
        .str = name,                                                           \
        .len = strlen(name),                                                   \
    };                                                                         \
    enum req_msg_method v = ty;                                                \
    hashtbl_insert(METHODS, &k, &v);                                           \
  } while (0);

    METHOD("initialize", REQ_MSG_METHOD_INITIALIZE);
    METHOD("initialized", REQ_MSG_METHOD_INITIALIZED);
    METHOD("shutdown", REQ_MSG_METHOD_SHUTDOWN);
    METHOD("exit", REQ_MSG_METHOD_EXIT);

    METHOD("textDocument/didOpen", REQ_MSG_METHOD_TEXTDOCUMENT_DIDOPEN);
    METHOD("textDocument/didClose", REQ_MSG_METHOD_TEXTDOCUMENT_DIDCLOSE);
  }

  *msg = (struct req_msg){0};

  if (value->ty != JSON_VALUE_TY_OBJECT) {
    BUG("not object?");
    return false;
  }

  const struct json_object *object = &value->obj_val;

  TRY_DE_SIZE_T(msg->id, "id", false);

  struct sized_str method;
  TRY_DE_STR(method, "method", true);

  const struct json_value *params;
  TRY_DE_VALUE(params, "params", false);

  enum req_msg_method *p = hashtbl_lookup(METHODS, &method);
  if (!p) {
    BUG("unknown message '%.*s'", (int)method.len, method.str);
    return false;
  }

  msg->method = *p;

  switch (msg->method) {
  case REQ_MSG_METHOD_INITIALIZE:
    return try_de_init_params(params, &msg->init_params);
  case REQ_MSG_METHOD_INITIALIZED:
  case REQ_MSG_METHOD_SHUTDOWN:
  case REQ_MSG_METHOD_EXIT:
    return msg;
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDOPEN:
    return try_de_didopen_textdoc_params(params, &msg->didopen_textdoc_params);
  case REQ_MSG_METHOD_TEXTDOCUMENT_DIDCLOSE:
    return try_de_didclose_textdoc_params(params, &msg->didclose_textdoc_params);
  }
}
