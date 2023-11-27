#include "lsra.h"

#include "ir/prettyprint.h"
#include "util.h"
#include "vector.h"

#include <limits.h>

struct interval {
  bool active;

  struct ir_op *op;
  size_t start;
  size_t end;

  // this is the interval representative for this interval
  struct interval *representative;
};

struct interval_data {
  struct interval *intervals;
  size_t num_intervals;
};

#define MARK_REG_USED(reg_pool, i) (reg_pool) &= ~(1 << i)
#define MARK_REG_FREE(reg_pool, i) (reg_pool) |= (1 << i)

struct cur_interval_data {
  struct interval_data *data;
  struct ir_op *op;
};

void op_used_callback(struct ir_op **op, void *cb_metadata) {
  struct cur_interval_data *cur_data = cb_metadata;

  struct interval *interval = &cur_data->data->intervals[(*op)->id];

  interval->end = cur_data->op->ty == IR_OP_TY_PHI ? cur_data->op->id - 1 : cur_data->op->id;
}

struct ir_op *last_value_producing_op(struct ir_basicblock *basicblock) {
  struct ir_op *last_op = NULL;
  struct ir_stmt *stmt = basicblock->last;
  while (stmt && !last_op) {
    struct ir_op *op = stmt->last;

    while (op && !last_op) {
      if (op_produces_value(op->ty)) {
        last_op = op;
      }

      op = op->pred;
    }

    stmt = stmt->pred;
  }

  // we now have the last op in the value's basic block that contains an
  // interval
  invariant_assert(last_op,
                   "trying to assign phi lifetime for bb but couldn't find "
                   "value-producing instruction in the block id %zu",
                   basicblock->id);

  return last_op;
}

struct interval *get_representative(struct interval *interval) {
  if (interval->representative == interval) {
    return interval;
  }

  return get_representative(interval->representative);
}

bool try_join_intervals(struct interval *l, struct interval *r) {
  struct interval *l_rep = get_representative(l);
  struct interval *r_rep = get_representative(r);

  if (l_rep == r_rep) {
    return true;
  }

  // TODO: add the fact that the intervals must be compatible (same reg type)
  if (true || l_rep->end <= r_rep->start || l_rep->start >= r_rep->end) {
    r_rep->start = MIN(l_rep->start, r_rep->start);
    r_rep->end = MAX(l_rep->end, r_rep->end);
   
    l_rep->active = false;
    l_rep->representative = r_rep;

    return true;
  } else {
    return false;
  }
}

struct interval_data construct_intervals(struct ir_builder *irb) {
  struct interval_data data;
  data.intervals =
      arena_alloc(irb->arena, sizeof(*data.intervals) * irb->op_count);
  data.num_intervals = 0;

  memset(data.intervals, 0, sizeof(*data.intervals) * irb->op_count);

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->phis;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        op->lcl_idx = NO_LCL;
        op->reg = NO_REG;

        debug_assert(op->id < irb->op_count, "out of range!");
        struct interval *interval;
        
        interval = &data.intervals[op->id];
        data.num_intervals++;

        interval->active = true;
        interval->op = op;
        interval->representative = interval;

        if (op->ty == IR_OP_TY_PHI) {
          interval->start = op->stmt->succ->first->id;
        } else {
          interval->start = op->id;
        }

        interval->end = MAX(interval->end, interval->start);
        
        debug_assert(
            op->metadata == NULL,
            "metadata left over in op during LSRA, will be overwritten");
        op->metadata = interval;

        struct cur_interval_data cur_data = {
          .data = &data,
          .op = op
        };
  
        if (op->ty != IR_OP_TY_PHI) {
          walk_op_uses(op, op_used_callback, &cur_data);
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  debug_print_ir(stderr, irb, print_ir_intervals, NULL);

  // merge phi intervals
  basicblock = irb->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->phis;
    struct ir_op *op = stmt->first;
    while (op) {
      struct interval *phi_interval = &data.intervals[op->id];

      for (size_t i = 0; i < op->phi.num_values; i++) {
        if (!try_join_intervals(&data.intervals[op->phi.values[i]->id], phi_interval)) {
          bug("couldn't join intervals!");
        }
      }

      op = op->succ;
    }

    basicblock = basicblock->succ;
  }
  
  return data;
}

void spill_at_interval(struct interval *intervals, size_t cur_interval,
                       size_t *active, size_t *num_active) {
  struct interval *spill = &intervals[active[*num_active - 1]];
  if (spill->end > intervals[cur_interval].end) {
    intervals[cur_interval].op->reg = spill->op->reg;
    spill->op->reg = REG_SPILLED;

    // remove `spill`
    (*num_active)--;

    // insert into `active`, sorted by end point
    for (size_t j = 0; j <= *num_active; j++) {
      if (j == *num_active) {
        active[j] = cur_interval;
        (*num_active)++;
        break;
      } else if (intervals[active[j + 1]].end > intervals[cur_interval].end) {
        memmove(&active[j + 1], &active[j],
                sizeof(*active) * (*num_active - j));
        active[j] = cur_interval;
        (*num_active)++;
        break;
      }
    }
  } else {
    intervals[cur_interval].op->reg = REG_SPILLED;
  }
}

void expire_old_intervals(struct interval *intervals,
                          struct interval *cur_interval, size_t *active,
                          size_t *num_active, unsigned long *reg_pool) {
  size_t num_expired_intervals = 0;

  for (size_t i = 0; i < *num_active; i++) {
    debug("%zu", i);
    struct interval *interval = &intervals[active[i]];

    if (!interval->active) {
      continue;
    }
    
    if (cur_interval->start <= interval->end) {
      break;
    }

    num_expired_intervals++;

    MARK_REG_FREE(*reg_pool, interval->op->reg);
  }

  // shift the active array down

  *num_active -= num_expired_intervals;
  memmove(active, &active[num_expired_intervals],
          sizeof(*active) * (*num_active));
}

int sort_interval_by_start_point(const void *a, const void *b) {
  size_t a_start = ((struct interval *)a)->start;
  size_t b_start = ((struct interval *)b)->start;

  if (a_start > b_start) {
    return 1;
  } else if (a_start < b_start) {
    return -1;
  } else {
    return 0;
  }
}

struct alloc_fixup_data {
  struct ir_builder *irb;
  struct ir_op *consumer;
};

void alloc_fixup_callback(struct ir_op **op, void *metadata) {
  struct alloc_fixup_data *data = metadata;

  if ((*op)->reg == REG_SPILLED) {
    if ((*op)->lcl_idx == NO_LCL) {
      (*op)->lcl_idx = data->irb->num_locals++;
      data->irb->total_locals_size += var_ty_size(data->irb, &(*op)->var_ty);

      struct ir_op *store = insert_after_ir_op(
          data->irb, *op, IR_OP_TY_STORE_LCL, IR_OP_VAR_TY_NONE);
      store->store_lcl.lcl_idx = (*op)->lcl_idx;
      store->store_lcl.value = *op;
    }

    struct ir_op *load = insert_before_ir_op(data->irb, data->consumer,
                                             IR_OP_TY_LOAD_LCL, (*op)->var_ty);
    load->load_lcl.lcl_idx = (*op)->lcl_idx;

    *op = load;
  }
}

void alloc_fixup(struct ir_builder *irb, struct interval_data *data) {
  UNUSED_ARG(data);

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {

    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        struct alloc_fixup_data metadata = {.irb = irb, .consumer = op};
        walk_op_uses(op, alloc_fixup_callback, &metadata);

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}

int compare_interval_id(const void *a, const void *b) {
  size_t a_id = ((struct interval *)a)->op->id;
  size_t b_id = ((struct interval *)b)->op->id;

  return (ssize_t)a_id - (ssize_t)b_id;
}

void print_ir_intervals(FILE *file, struct ir_op *op, void *metadata) {
  UNUSED_ARG(metadata);

  struct interval *interval = op->metadata;
  if (interval) {
    invariant_assert(interval->op->id == op->id, "intervals are not ID keyed");
    fslogsl(file, "start=%05zu, end=%05zu | ", interval->start, interval->end);
  } else {
    fslogsl(file, "no associated interval | ");
  }

  op->metadata = NULL;

  switch (op->reg) {
  case NO_REG:
    fslogsl(file, "    (UNASSIGNED)");
    break;
  case REG_SPILLED:
    fslogsl(file, "    (SPILLED)");
    break;
  default:
    fslogsl(file, "    register=%zu", op->reg);
    break;
  }
}

struct interval_data register_alloc_pass(struct ir_builder *irb,
                                         struct reg_info reg_info) {
  struct interval_data data = construct_intervals(irb);
  struct interval *intervals = data.intervals;
  size_t num_intervals = data.num_intervals;

  qsort(intervals, num_intervals, sizeof(*intervals),
        sort_interval_by_start_point);

  size_t *active = arena_alloc(irb->arena, sizeof(size_t) * reg_info.num_regs);
  size_t num_active = 0;

  invariant_assert(reg_info.num_regs <= sizeof(unsigned long) * 8,
                   "LSRA does not currently support more than `sizeof(unsigned "
                   "long) * 8` register as it uses a bitmask");

  unsigned long reg_pool = (1 << reg_info.num_regs) - 1;

  // intervals must be sorted by start point
  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &intervals[i];
    if (!interval->active) {
      continue;
    }

    expire_old_intervals(intervals, interval, active, &num_active, &reg_pool);

    if (!op_produces_value(interval->op->ty)) {
      // stores don't need a register
      // TODO: this logic should be more centralised and clear, not just for
      // this op
      continue;
    }

    if (num_active == reg_info.num_regs) {
      spill_at_interval(intervals, i, active, &num_active);
    } else {
      unsigned long free_reg = tzcnt(reg_pool);
      debug_assert(free_reg < reg_info.num_regs,
                   "reg pool unexpectedly empty!");

      MARK_REG_USED(reg_pool, free_reg);
      interval->op->reg = free_reg;

      // insert into `active`, sorted by end point
      for (size_t j = 0; j <= num_active; j++) {
        if (j == num_active) {
          active[j] = i;
          num_active++;
          break;
        } else if (intervals[active[j + 1]].end > intervals[i].end) {
          memmove(&active[j + 1], &active[j],
                  sizeof(*active) * (num_active - j));
          active[j] = i;
          num_active++;
          break;
        }
      }
    }
  }

  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &intervals[i];
    if (interval->op && !interval->active) {
      interval->op->reg = get_representative(interval)->op->reg;
    }
  }

  return data;
}

void register_alloc(struct ir_builder *irb, struct reg_info reg_info) {
  rebuild_ids(irb);

  struct interval_data data = register_alloc_pass(irb, reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  debug_print_ir(stderr, irb, print_ir_intervals, data.intervals);

  BEGIN_SUB_STAGE("SPILL HANDLING");

  // insert LOAD and STORE ops as needed
  alloc_fixup(irb, &data);

  // can't print intervals here, as `alloc_fixup` inserted new ops which don't
  // have valid intervals yet
  debug_print_ir(stderr, irb, NULL, NULL);

  BEGIN_SUB_STAGE("SECOND-PASS REGALLOC");

  data = register_alloc_pass(irb, reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  debug_print_ir(stderr, irb, print_ir_intervals, NULL);
}
