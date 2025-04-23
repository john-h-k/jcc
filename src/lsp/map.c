#include "map.h"

#include "../compiler.h"
#include "../preproc.h"
#include "../typechk.h"
#include "../vector.h"
#include "ctx.h"

static int compare_text_spans(const void *l, const void *r) {
  const struct td_node *ls = l;
  const struct td_node *rs = r;

  const struct text_pos *l_start = &ls->span.start;
  const struct text_pos *r_start = &rs->span.start;

  if (l_start->idx > r_start->idx) {
    return 1;
  } else if (l_start->idx == r_start->idx) {
    return 0;
  } else {
    return -1;
  }
}

struct lsp_map {
  const struct lsp_doc *doc;

  struct vector *locs;
  struct hashtbl *defs;
};

static void build_def_cb(const struct td_node *node, void *metadata) {
  struct lsp_map *data = metadata;

  // TODO: support decls
  switch (node->ty) {
  case TD_NODE_TY_EXPR: {
    const struct td_expr *expr = node->expr;
    if (expr->ty == TD_EXPR_TY_VAR) {
      vector_push_back(data->locs, &(struct lsp_node){.ty = LSP_NODE_TY_TD_NODE,
                                                      .span = node->span,
                                                      .node = *node});
    }
    break;
  }
  case TD_NODE_TY_VAR_DECL: {
    const struct td_var_declaration *var_decl = node->var_decl;
    struct text_span span = var_decl->var.span;
    hashtbl_insert(data->defs, &var_decl->var, &span);

    // push it back so that goto def on a decl itself still works
    vector_push_back(data->locs, &(struct lsp_node){.ty = LSP_NODE_TY_TD_NODE,
                                                    .span = span,
                                                    .node = *node});
    break;
  }
  default:
    break;
  }
}

struct lsp_map *lsp_map_create(struct lsp_ctx *ctx, const struct lsp_doc *doc) {
  struct compiler *compiler = doc->compiler;

  struct preproc *preproc = compiler_get_preproc(compiler);

  struct typechk *tchk;
  struct typechk_result result;
  compiler_get_tchk(compiler, &tchk, &result);

  if (!tchk) {
    return NULL;
  }

  struct lsp_map *map = arena_alloc(ctx->arena, sizeof(*map));
  *map = (struct lsp_map){
      .doc = doc,
      .locs = vector_create_in_arena(sizeof(struct lsp_node), ctx->arena),
      .defs = hashtbl_create_in_arena(ctx->arena, sizeof(struct td_var),
                                      sizeof(struct text_span), hash_td_var,
                                      eq_td_var)};

  td_walk(tchk, &result.translation_unit, build_def_cb, map);

  // now add preprocessor events

  struct preproc_events events = preproc_get_events(preproc);

  for (size_t i = 0; i < events.num_events; i++) {
    const struct preproc_event *event = &events.events[i];

    switch (event->ty) {
    case PREPROC_EVENT_TY_INCLUDE:
      break;
    case PREPROC_EVENT_TY_MACRO_EXPAND:
      break;
    }

    vector_push_back(map->locs,
                     &(struct lsp_node){.ty = LSP_NODE_TY_PREPROC_EVENT,
                                        .span = event->span,
                                        .event = *event});
  }

  vector_sort(map->locs, compare_text_spans);

  return map;
}

struct text_span lsp_map_lookup(const struct lsp_map *map,
                                struct text_pos pos) {
  const struct lsp_node *node = NULL;

  size_t num_locs = vector_length(map->locs);

  // FIXME: linear search
  for (size_t i = 0; i < num_locs; i++) {
    struct lsp_node *loc = vector_get(map->locs, i);

    struct text_pos start = loc->span.start;
    struct text_pos end = loc->span.end;

    if ((end.line > pos.line || (end.line == pos.line && end.col > pos.col)) &&
        (start.line < pos.line ||
         (start.line == pos.line && start.col <= pos.col))) {
      node = loc;
      break;
    }
  }

  if (!node) {
    return MK_INVALID_TEXT_SPAN2();
  }

  struct text_span *def = NULL;
  switch (node->ty) {
  case LSP_NODE_TY_PREPROC_EVENT: {
    const struct preproc_event *event = &node->event;

    switch (event->ty) {
    case PREPROC_EVENT_TY_INCLUDE: {
      struct text_pos start = {
        .file = event->include.path,
        // start of file       
      };
      return MK_TEXT_SPAN(start, start);
    }
    case PREPROC_EVENT_TY_MACRO_EXPAND:
      return event->macro_expand.span;
    }

    break;
  }
  case LSP_NODE_TY_TD_NODE:
    if (node->node.ty == TD_NODE_TY_VAR_DECL) {
      // self
      return node->span;
    } else if (node->node.ty == TD_NODE_TY_EXPR && node->node.expr->ty == TD_EXPR_TY_VAR) {
      const struct td_var *var = &node->node.var_decl->var;
      def = hashtbl_lookup(map->defs, var);
    }
    break;
  }

  return def ? *def : MK_INVALID_TEXT_SPAN2();
}
