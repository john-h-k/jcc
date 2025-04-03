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

static bool try_de_init_params(const struct json_value *value,
                               struct init_params *params) {
  *params = (struct init_params){0};

  if (value->ty != JSON_VALUE_TY_OBJECT) {
    fprintf(stderr, "not obj\n");
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
  }

  *msg = (struct req_msg){0};

  if (value->ty != JSON_VALUE_TY_OBJECT) {
    return false;
  }

  const struct json_object *object = &value->obj_val;

  TRY_DE_SIZE_T(msg->id, "id", true);

  struct sized_str method;
  TRY_DE_STR(method, "method", true);

  const struct json_value *params;
  TRY_DE_VALUE(params, "params", false);

  enum req_msg_method *p = hashtbl_lookup(METHODS, &method);
  if (!p) {
    return false;
  }

  msg->method = *p;

  switch (msg->method) {
  case REQ_MSG_METHOD_INITIALIZE:
    return try_de_init_params(params, &msg->init_params);
  case REQ_MSG_METHOD_INITIALIZED:
    return msg;
  }
}
