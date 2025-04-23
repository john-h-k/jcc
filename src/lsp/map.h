#ifndef LSP_MAP_H
#define LSP_MAP_H

#include "../preproc.h"
#include "../program.h"
#include "../typechk.h"
#include "ctx.h"

struct lsp_map;

enum lsp_node_ty { LSP_NODE_TY_PREPROC_EVENT, LSP_NODE_TY_TD_NODE };

struct lsp_node {
  enum lsp_node_ty ty;

  struct text_span span;

  union {
    struct td_node node;
    struct preproc_event event;
  };
};

struct lsp_map *lsp_map_create(struct lsp_ctx *ctx, const struct lsp_doc *doc);

struct text_span lsp_map_lookup(const struct lsp_map *map, struct text_pos pos);

#endif
