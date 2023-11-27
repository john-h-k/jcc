#ifndef GRAPHWRITER_H
#define GRAPHWRITER_H

#include "alloc.h"

#include <stdio.h>

struct graphwriter;
struct graph_vertex;
struct graph_edge;

enum graph_ty { GRAPH_TY_UNDIRECTED, GRAPH_TY_DIRECTED };

enum graph_strictness { GRAPH_STRICTNESS_STRICT, GRAPH_STRICTNESS_UNSTRICT };

struct graph_color {
  const char *color;
};

struct graph_font {
  const char *font;
};

struct graph_label {
  const char *label;
};

enum graph_edge_attr_ty {
  GRAPH_EDGE_ATTR_TY_COLOR,
  GRAPH_EDGE_ATTR_TY_FONT,
  GRAPH_EDGE_ATTR_TY_LABEL,
};

struct graph_edge_attr {
  enum graph_edge_attr_ty ty;

  union {
    struct graph_color color;
    struct graph_font font;
    struct graph_label label;
  };
};

enum graph_vertex_attr_ty {
  GRAPH_VERTEX_ATTR_TY_COLOR,
  GRAPH_VERTEX_ATTR_TY_FONT,
  GRAPH_VERTEX_ATTR_TY_LABEL,
  GRAPH_VERTEX_ATTR_TY_SHAPE
};

enum graph_shape { GRAPH_SHAPE_RECT, GRAPH_SHAPE_ELLIPSE };

struct graph_vertex_attr {
  enum graph_vertex_attr_ty ty;

  union {
    struct graph_color color;
    struct graph_font font;
    struct graph_label label;
    enum graph_shape shape;
  };
};

struct graphwriter *graphwriter_create(struct arena_allocator *arena,
                                       enum graph_ty ty,
                                       enum graph_strictness strictness,
                                       FILE *file);
void graphwriter_free(struct graphwriter **gwr);

struct graph_vertex *vertex_from_str(struct graphwriter *gwr, const char *id);
struct graph_vertex *vertex_from_integral(struct graphwriter *gwr, size_t id);
struct graph_edge *edge(struct graphwriter *gwr, struct graph_vertex *from,
                        struct graph_vertex *to);

// void graph_attr(struct graphwriter *graph, struct graph_attr attr);
void edge_attr(struct graph_edge *edge, const struct graph_edge_attr *attr);
void vertex_attr(struct graph_vertex *vertex,
                 const struct graph_vertex_attr *attr);

// allow streaming writes into attrs
FILE *begin_edge_attr(struct graph_edge *edge, enum graph_edge_attr_ty ty);
void end_edge_attr(struct graph_edge *edge);

FILE *begin_vertex_attr(struct graph_vertex *vertex,
                        enum graph_vertex_attr_ty ty);
void end_vertex_attr(struct graph_vertex *vertex);

#endif
