#include "graphcol.h"

#include "bitset.h"
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

struct adj_info {
  struct interval *neighbour;
};

// static void remove_node(struct vector **interf, size_t i) {
//   struct vector *adj = interf[i];
//   size_t degree = vector_length(adj);

//   // FIXME: literally immediately leaks
//   interf[i] = NULL;
//   for (size_t j = 0; j < degree; j++) {
//     struct adj_info *adj_info = vector_get(adj, j);

//     // FIXME: super inefficient

//     struct vector *other = interf[adj_info->neighbour->op->id];
//     size_t other_degree = vector_length(other);
//     for (size_t k = 0; k < other_degree; k++) {
//       struct adj_info *other_info = vector_get(other, k);

//       if (other_info->neighbour->op->id == i) {
//         vector_remove_at(other, k);
//         break;
//       }
//     }
//   }
// }

static void build_interf_graph(struct graphcol_state *state,
                               struct reg_set_info *info,
                               struct vector **interf, enum ir_reg_ty reg_ty) {
  struct interval_data *data = &state->interval_data;
  size_t num_intervals = data->num_intervals;

  size_t total_reg = info->num_volatile + info->num_nonvolatile;

  struct bitset *all_registers = bitset_create(total_reg, false);
  if (reg_ty == IR_REG_TY_INTEGRAL) {
    // state->irb->reg_usage.gp_registers_used = all_registers;
  } else {
    // state->irb->reg_usage.fp_registers_used = all_registers;
  }
  TODO("registers used in graphcol");

  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &data->intervals[i];

    interf[i] = NULL;

    if (!ir_op_produces_value(interval->op)) {
      continue;
    }

    if (reg_ty == IR_REG_TY_INTEGRAL &&
        ir_var_ty_is_integral(&interval->op->var_ty)) {
      interf[i] = vector_create(sizeof(size_t));
    } else if (reg_ty == IR_REG_TY_FP &&
               ir_var_ty_is_fp(&interval->op->var_ty)) {
      interf[i] = vector_create(sizeof(size_t));
    }
  }

  struct vector *active = vector_create(sizeof(size_t));
  size_t *degree = calloc(num_intervals, sizeof(size_t));

  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &data->intervals[i];

    if (!interf[i] || !ir_op_produces_value(interval->op)) {
      continue;
    }

    size_t num_active = vector_length(active);
    size_t k = 0;

    // remove expired intervals
    for (size_t j = 0; j < num_active; j++) {
      size_t other_idx = *(size_t *)vector_get(active, j);
      struct interval *other = &data->intervals[other_idx];

      if (other->end >= interval->start) {
        *(size_t *)vector_get(active, k++) = other_idx;
      }
    }
    vector_truncate(active, k);

    for (size_t j = 0; j < k; j++) {
      size_t other_idx = *(size_t *)vector_get(active, j);

      vector_push_back(interf[interval->op->id], &other_idx);
      vector_push_back(interf[other_idx], &interval->op->id);

      degree[interval->op->id]++;
      degree[other_idx]++;
    }

    vector_push_back(active, &interval->op->id);
  }

  struct vector *trivial = vector_create(sizeof(size_t));
  struct vector *spills = vector_create(sizeof(size_t));

  while (true) {
    bool removed_node;

    do {
      removed_node = false;

      for (size_t i = 0; i < num_intervals; i++) {
        if (!interf[i]) {
          continue;
        }

        if (degree[i] == SIZE_MAX)
          continue;

        if (degree[i] < total_reg) {
          vector_push_back(trivial, &i);

          removed_node = true;

          struct vector *adj = interf[i];
          for (size_t j = 0; j < vector_length(adj); j++) {

            size_t neighbor = *(size_t *)vector_get(adj, j);
            if (degree[neighbor] != SIZE_MAX) {
              degree[neighbor]--;
            }
          }
          degree[i] = SIZE_MAX;
          // vector_clear(adj);
        }
      }
    } while (removed_node);

    bool nodes_rem = false;
    for (size_t i = 0; i < num_intervals; i++) {
      if (!interf[i]) {
        continue;
      }

      if (degree[i] != SIZE_MAX) {
        vector_push_back(spills, &i);
        struct vector *adj = interf[i];

        for (size_t j = 0; j < vector_length(adj); j++) {
          size_t neighbor = *(size_t *)vector_get(adj, j);
          if (degree[neighbor] != SIZE_MAX) {
            degree[neighbor]--;
          }
        }

        degree[i] = SIZE_MAX;
        // vector_clear(adj);

        nodes_rem = true;
        break;
      }
    }

    if (!nodes_rem)
      break;
  }

  size_t *assigned_registers = calloc(num_intervals, sizeof(size_t));
  memset(assigned_registers, 0xFF, num_intervals * sizeof(size_t));

  while (vector_length(trivial) > 0) {
    size_t i = *(size_t *)vector_tail(trivial);
    vector_pop(trivial);

    bool *used_regs =
        arena_alloc(state->irb->arena, sizeof(*used_regs) * total_reg);
    memset(used_regs, false, total_reg);

    struct vector *adj = interf[i];
    for (size_t j = 0; j < vector_length(adj); j++) {
      size_t neighbor = *(size_t *)vector_get(adj, j);
      if (assigned_registers[neighbor] != SIZE_MAX) {
        used_regs[assigned_registers[neighbor]] = true;
      }
    }

    for (size_t r = 0; r < total_reg; r++) {
      if (!used_regs[r]) {
        assigned_registers[i] = r;
        data->intervals[i].op->reg = (struct ir_reg){.ty = reg_ty, .idx = r};

        bitset_set(all_registers, r, true);
        break;
      }
    }
  }

  for (size_t i = 0; i < vector_length(spills); i++) {
    size_t spilled_var = *(size_t *)vector_get(spills, i);

    ir_spill_op(state->irb, data->intervals[spilled_var].op);
  }

  debug_print_ir_func(stderr, state->irb, print_ir_intervals, data->intervals);
}

// uses graph-coloring technique to allocate registers - relies on SSA form!
void graphcol_register_alloc(struct ir_func *irb, struct reg_info reg_info) {
  TODO("does not currently work!");

  ir_clear_metadata(irb);
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
                     IR_REG_TY_INTEGRAL);
  build_interf_graph(&state, &reg_info.fp_registers, fp_interf, IR_REG_TY_FP);
}
