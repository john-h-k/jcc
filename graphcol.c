#include "graphcol.h"

#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "liveness.h"
#include "target.h"
#include "util.h"
#include "vector.h"

struct graphcol_state {
  struct ir_func *irb;

  struct interval_data interval_data;
};

enum graphcol_reg_ty {
  GRAPHCOL_REG_TY_GP,
  GRAPHCOL_REG_TY_FP,
};

struct adj_info {
  struct interval *neighbour;
};

static void remove_node(struct vector **interf, size_t i) {
  struct vector *adj = interf[i];
  size_t degree = vector_length(adj);

  // FIXME: literally immediately leaks
  interf[i] = NULL;
  for (size_t j = 0; j < degree; j++) {
    struct adj_info *adj_info = vector_get(adj, j);

    // FIXME: super inefficient

    struct vector *other = interf[adj_info->neighbour->op->id];
    size_t other_degree = vector_length(other);
    for (size_t k = 0; k < other_degree; k++) {
      struct adj_info *other_info = vector_get(other, k);

      if (other_info->neighbour->op->id == i) {
        vector_remove_at(other, k);
        break;
      }
    }
  }
}

static void build_interf_graph(struct graphcol_state *state,
                               struct reg_set_info *info,
                               struct vector **interf,
                               enum graphcol_reg_ty reg_ty) {
  struct interval_data *data = &state->interval_data;

  for (size_t i = 0; i < data->num_intervals; i++) {
    struct vector **adj = &interf[i];
    *adj = vector_create(sizeof(struct adj_info));
  }

  struct vector *active = vector_create(sizeof(struct interval *));

  for (size_t i = 0; i < data->num_intervals; i++) {
    struct interval *interval = &data->intervals[i];

    if (!op_produces_value(interval->op)) {
      continue;
    }

    if (reg_ty == GRAPHCOL_REG_TY_GP &&
        !var_ty_is_integral(&interval->op->var_ty)) {
      continue;
    } else if (reg_ty == GRAPHCOL_REG_TY_FP &&
               !var_ty_is_fp(&interval->op->var_ty)) {
      continue;
    }

    size_t num_active = vector_length(active);
    size_t k = 0;
    for (size_t j = 0; j < num_active; j++) {
      struct interval *other = *(struct interval **)vector_get(active, j);

      if (other->end >= interval->start) {
        *(struct interval **)vector_get(active, k++) = other;
      }
    }
    vector_truncate(active, k);

    for (size_t j = 0; j < k; j++) {
      struct interval *other = *(struct interval **)vector_get(active, j);

      struct vector *interf_self = interf[interval->op->id];
      struct vector *interf_other = interf[other->op->id];

      struct adj_info other_adj = {
          .neighbour = other,
      };

      struct adj_info self_adj = {
          .neighbour = interval,
      };

      vector_push_back(interf_self, &other_adj);
      vector_push_back(interf_other, &self_adj);
    }

    vector_push_back(active, &interval);
  }

  size_t total_reg = info->num_volatile + info->num_volatile;

  struct vector *trivial = vector_create(sizeof(size_t));
  struct vector *spills = vector_create(sizeof(size_t));

  while (true) {
    bool removed_node;

    do {
      removed_node = false;

      for (size_t i = 0; i < data->num_intervals; i++) {
        struct vector *adj = interf[i];

        if (!adj) {
          continue;
        }

        size_t degree = vector_length(adj);
        if (degree < total_reg) {
          vector_push_back(trivial, &i);

          removed_node = true;

          remove_node(interf, i);
        }
      }
    } while (removed_node);

    bool nodes_rem = false;
    for (size_t i = 0; i < data->num_intervals; i++) {
      struct vector *adj = interf[i];

      if (!adj) {
        continue;
      }

      if (!nodes_rem) {
        vector_push_back(spills, &i);
        remove_node(interf, i);
        nodes_rem = true;
        break;
      }
    }

    if (!nodes_rem) {
      break;
    }
  }

  debug_print_ir_func(stderr, state->irb, print_ir_intervals, data->intervals);
  BUG("");
}

// uses graph-coloring technique to allocate registers - relies on SSA form!
void graphcol_register_alloc(struct ir_func *irb, struct reg_info reg_info) {
  struct interval_data data = construct_intervals(irb);

  DEBUG_ASSERT(data.num_intervals == irb->op_count,
               "expected one interval per op!");

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        sort_interval_by_start_point);

  // adjacency list
  struct vector **gp_interf =
      arena_alloc(irb->arena, sizeof(struct interval *) * data.num_intervals);
  struct vector **fp_interf =
      arena_alloc(irb->arena, sizeof(struct interval *) * data.num_intervals);

  struct graphcol_state state = {
      .irb = irb,
      .interval_data = data,
  };

  build_interf_graph(&state, &reg_info.gp_registers, gp_interf,
                     GRAPHCOL_REG_TY_GP);
  build_interf_graph(&state, &reg_info.fp_registers, fp_interf,
                     GRAPHCOL_REG_TY_FP);

  TODO("graph col");
}
