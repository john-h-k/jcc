#include "lsra.h"
#include "ir.h"
#include "util.h"
#include "vector.h"
#include <limits.h>

#define MARK_REG_USED(reg_pool, i) reg_pool &= ~(1 << i)
#define MARK_REG_FREE(reg_pool, i) reg_pool |= (1 << i)

void op_used_callback(struct ir_op *op, void *cb_metadata) {
  struct interval_data *data = cb_metadata;

  struct interval *interval = &data->intervals[op->id];

  interval->end = data->num_intervals;
}

struct interval_data construct_intervals(struct ir_builder *irb) {
  struct interval_data data;
  data.intervals = alloc(irb->arena, sizeof(*data.intervals) * irb->op_count);
  data.num_intervals = 0;//irb->op_count;

  memset(data.intervals, 0, sizeof(*data.intervals) * irb->op_count);

  struct ir_stmt *stmt = irb->first;
  while (stmt) {
    struct ir_op *op = stmt->first;
    while (op) {
      struct interval *interval = &data.intervals[op->id];

      interval->op_id = op->id;
      interval->start = data.num_intervals++;

      walk_op_uses(op, op_used_callback, &data);

      op = op->succ;
    }

    stmt = stmt->succ;
  }

  return data;
}

void spill_at_interval(struct interval *intervals, size_t cur_interval, size_t *active, size_t *num_active) {
  // FIXME: optimise mem reads

  struct interval *spill = &intervals[active[*num_active - 1]];
  if (spill->end > intervals[cur_interval].end) {
    intervals[cur_interval].reg = spill->reg;
    spill->reg = REG_SPILLED;

    // remove `spill`
    (*num_active)--;

    // insert into `active`, sorted by end point
    for (size_t j = 0; j < *num_active; j++) {
      if (j + 1 == *num_active || intervals[active[j + 1]].end > intervals[cur_interval].end) {
        active[j] = cur_interval;
        (*num_active)++;
        break;
      }
    }
  } else {
    intervals[cur_interval].reg = REG_SPILLED;
  }
}

void expire_old_intervals(struct interval *intervals, struct interval *cur_interval, size_t *active, size_t *num_active, unsigned long *reg_pool) {
  size_t num_expired_intervals = 0;
  
  for (size_t i = 0; i < *num_active; i++) {
    struct interval *interval = &intervals[active[i]];
    debug("cur interval %zu, intervals[i].end %zu", cur_interval->start, interval->end);
    if (cur_interval->start <= interval->end) {
      return;
    }

    num_expired_intervals++;


    debug("marking reg %ul free", interval->reg);
    MARK_REG_FREE(*reg_pool, interval->reg);
  }

  // shift the active array down
  memmove(active, &active[num_expired_intervals], sizeof(*active) * (*num_active - num_expired_intervals));
  num_active -= num_expired_intervals;
}

int sort_interval_by_start_point(const void *a, const void *b) {
  size_t a_start = ((struct interval*)a)->start;
  size_t b_start = ((struct interval*)b)->start;

  if (a_start > b_start) {
    return 1;
  } else if (a_start < b_start) {
    return -1;
  } else {
    return 0;
  }
}

struct interval_data register_alloc(struct ir_builder *irb, struct reg_info reg_info) {
  struct interval_data data = construct_intervals(irb);
  struct interval *intervals = data.intervals;
  size_t num_intervals = data.num_intervals;

  qsort(intervals, num_intervals, sizeof(*intervals), sort_interval_by_start_point);

  size_t *active = alloc(irb->arena, sizeof(size_t) * reg_info.num_regs);
  size_t num_active = 0;

  invariant_assert(reg_info.num_regs <= sizeof(unsigned long) * 8,
                   "LSRA does not currently support more than `sizeof(unsigned "
                   "long) * 8` register as it uses a bitmask");

  unsigned long reg_pool = ULONG_MAX;

  // intervals must be sorted by start point
  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &intervals[i];
    expire_old_intervals(intervals, interval, active, &num_active, &reg_pool);

    if (num_active == reg_info.num_regs) {
      spill_at_interval(intervals, i, active, &num_active);
    } else {
      debug("reg pool: %ul", reg_pool);
      unsigned long free_reg = tzcnt(reg_pool);
      debug("free reg: %ul", free_reg);
      debug_assert(free_reg != sizeof(reg_pool) * 8, "reg pool unexpectedly empty!");

      debug("marking reg %ul used", free_reg);
      MARK_REG_USED(reg_pool, free_reg);
      interval->reg = free_reg;

      bool inserted= false;
      // insert into `active`, sorted by end point
      for (size_t j = 0; j <= num_active; j++) {
        if (j == num_active || intervals[active[j + 1]].end > intervals[i].end) {
          active[j] = i;
          num_active++;
          inserted = true;
          break;
        }
      }
      invariant_assert(inserted, "didnt insert");
    }
  }

  return data;
}
