#ifndef LSP_LSP_TYPES_H
#define LSP_LSP_TYPES_H

#include <stddef.h>
#include "../util.h"
#include "../json.h"

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
  };

  struct sized_str locale;
  struct sized_str root_path;
  struct sized_str root_uri;
  struct client_caps client_capabilities;
  struct trace_val trace;

  struct workspace_folder *workspace_folders;
  size_t num_workspace_folders;
};

enum req_msg_method {
  REQ_MSG_METHOD_INITIALIZE,
};

struct req_msg {
  size_t id;
  enum req_msg_method method;

  union {
    struct init_params init_params;
  };
};

#endif

