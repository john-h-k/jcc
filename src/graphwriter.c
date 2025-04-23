#include "graphwriter.h"

#include "alloc.h"
#include "util.h"

#include <math.h>

struct graphwriter {
  struct arena_allocator *arena;

  FILE *file;

  enum graph_ty ty;
  enum graph_strictness strictness;
};

struct graph_vertex {
  struct graphwriter *gwr;

  const char *id;
};

struct graph_edge {
  struct graphwriter *gwr;

  // if undirected, order is arbitrary
  struct graph_vertex *from;
  struct graph_vertex *to;
};

struct graphwriter *graphwriter_create(struct arena_allocator *arena,
                                       enum graph_ty ty,
                                       enum graph_strictness strictness,
                                       FILE *file) {
  struct graphwriter *gwr = aralloc(arena, sizeof(*gwr));

  gwr->ty = ty;
  gwr->strictness = strictness;
  gwr->file = file;
  gwr->arena = arena;

  switch (gwr->strictness) {
  case GRAPH_STRICTNESS_STRICT:
    fprintf(gwr->file, "strict ");
    break;
  case GRAPH_STRICTNESS_UNSTRICT:
    // no keyword
    break;
  }

  switch (gwr->ty) {
  case GRAPH_TY_UNDIRECTED:
    fprintf(gwr->file, "graph");
    break;
  case GRAPH_TY_DIRECTED:
    fprintf(gwr->file, "digraph");
    break;
  }

  fprintf(gwr->file, " {\n");
  fprintf(gwr->file, "    graph [dpi=300]\n");

  return gwr;
}

void graphwriter_free(struct graphwriter **gwr) {
  fprintf((*gwr)->file, "}\n");
  *gwr = NULL;

  // does not close the file!
}

static const char *edge_connector(enum graph_ty ty) {
  switch (ty) {
  case GRAPH_TY_UNDIRECTED:
    return "--";
  case GRAPH_TY_DIRECTED:
    return "->";
  }
}

static void write_end(struct graphwriter *gwr) { fprintf(gwr->file, "\n"); }

// FIXME: these break with double quotes

static void write_vertex(struct graph_vertex *vertex) {
  fprintf(vertex->gwr->file, "    \"%s\"", vertex->id);
}

static void write_edge(struct graph_edge *edge) {
  fprintf(edge->gwr->file, "    \"%s\" %s \"%s\"", edge->from->id,
          edge_connector(edge->gwr->ty), edge->to->id);
}

static struct graph_vertex *create_vertex(struct graphwriter *gwr,
                                          const char *id) {
  struct graph_vertex *gvx = aralloc(gwr->arena, sizeof(*gvx));

  gvx->gwr = gwr;
  gvx->id = id;

  return gvx;
}

struct graph_vertex *vertex_from_str(struct graphwriter *gwr, const char *id) {
  // don't tie vertex lifetime to caller string, copy into arena
  char *copy = aralloc_strdup(gwr->arena, id);
  struct graph_vertex *vertex = create_vertex(gwr, copy);
  write_vertex(vertex);
  write_end(gwr);
  return vertex;
}

struct graph_vertex *vertex_from_integral(struct graphwriter *gwr, size_t id) {
  size_t digits = num_digits(id);
  size_t sz = (sizeof(char) * digits) + 1;
  char *buff = aralloc(gwr->arena, sz);

  snprintf(buff, sz, "%zu", id);

  struct graph_vertex *vertex = create_vertex(gwr, buff);
  write_vertex(vertex);
  write_end(gwr);
  return vertex;
}

struct graph_edge *edge(struct graphwriter *gwr, struct graph_vertex *from,
                        struct graph_vertex *to) {
  struct graph_edge *edge = aralloc(gwr->arena, sizeof(*edge));

  edge->gwr = gwr;
  edge->from = from;
  edge->to = to;

  write_edge(edge);
  write_end(gwr);

  return edge;
}

// void graph_attr(struct graphwriter *graph, struct graph_attr attr);
void edge_attr(struct graph_edge *edge, const struct graph_edge_attr *attr) {
  write_edge(edge);

  switch (attr->ty) {
  case GRAPH_EDGE_ATTR_TY_COLOR:
    fprintf(edge->gwr->file, " [color=%s] ", attr->color.color);
    break;
  case GRAPH_EDGE_ATTR_TY_FONT:
    fprintf(edge->gwr->file, " [fontname=%s] ", attr->font.font);
    break;
  case GRAPH_EDGE_ATTR_TY_LABEL:
    fprintf(edge->gwr->file, " [label=%s] ", attr->label.label);
    break;
  }

  write_end(edge->gwr);
}

void vertex_attr(struct graph_vertex *vertex,
                 const struct graph_vertex_attr *attr) {
  write_vertex(vertex);

  switch (attr->ty) {
  case GRAPH_VERTEX_ATTR_TY_COLOR:
    fprintf(vertex->gwr->file, " [color=%s] ", attr->color.color);
    break;
  case GRAPH_VERTEX_ATTR_TY_FONT:
    fprintf(vertex->gwr->file, " [fontname=%s] ", attr->font.font);
    break;
  case GRAPH_EDGE_ATTR_TY_LABEL:
    fprintf(vertex->gwr->file, " [label=%s] ", attr->label.label);
    break;
  case GRAPH_VERTEX_ATTR_TY_SHAPE:
    switch (attr->shape) {
    case GRAPH_SHAPE_RECT:
      fprintf(vertex->gwr->file, " [shape=rect] ");
      break;
    case GRAPH_SHAPE_ELLIPSE:
      fprintf(vertex->gwr->file, " [shape=ellipse] ");
      break;
    }
    break;
  }

  write_end(vertex->gwr);
}

FILE *begin_edge_attr(struct graph_edge *edge, enum graph_edge_attr_ty ty) {
  write_edge(edge);

  switch (ty) {
  case GRAPH_EDGE_ATTR_TY_COLOR:
    fprintf(edge->gwr->file, " [color=\"");
    break;
  case GRAPH_EDGE_ATTR_TY_FONT:
    fprintf(edge->gwr->file, " [font=\"");
    break;
  case GRAPH_EDGE_ATTR_TY_LABEL:
    fprintf(edge->gwr->file, " [label=\"");
    break;
  }

  return edge->gwr->file;
}

void end_edge_attr(struct graph_edge *edge) {
  fprintf(edge->gwr->file, "\"]");
  write_end(edge->gwr);
}

FILE *begin_vertex_attr(struct graph_vertex *vertex,
                        enum graph_vertex_attr_ty ty) {
  write_vertex(vertex);

  switch (ty) {
  case GRAPH_VERTEX_ATTR_TY_COLOR:
    fprintf(vertex->gwr->file, " [color=\"");
    break;
  case GRAPH_VERTEX_ATTR_TY_FONT:
    fprintf(vertex->gwr->file, " [font=\"");
    break;
  case GRAPH_VERTEX_ATTR_TY_SHAPE:
    fprintf(vertex->gwr->file, " [shape=\"");
    break;
  case GRAPH_VERTEX_ATTR_TY_LABEL:
    fprintf(vertex->gwr->file, " [label=\"");
    break;
  }

  return vertex->gwr->file;
}

void end_vertex_attr(struct graph_vertex *vertex) {
  fprintf(vertex->gwr->file, "\"]");
  write_end(vertex->gwr);
}
