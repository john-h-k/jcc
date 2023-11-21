#include "lsra.h"
#include "ir.h"
#include "util.h"
#include "vector.h"
#include <limits.h>

struct interval {
  struct ir_op *op;
  size_t start;
  size_t end;
};

struct interval_data {
  struct interval *intervals;
  size_t num_intervals;
};

#define MARK_REG_USED(reg_pool, i) reg_pool &= ~(1 << i)
#define MARK_REG_FREE(reg_pool, i) reg_pool |= (1 << i)

void op_used_callback(struct ir_op **op, void *cb_metadata) {
  struct interval_data *data = cb_metadata;

  debug("op callback id %zu", (*op)->id);
  struct interval *interval = &data->intervals[(*op)->id];

  interval->end = data->num_intervals;
  debug("op=%zu, op start %zu, op end %zu", interval->op->id, interval->start, interval->end);
}

struct interval_data construct_intervals(struct ir_builder *irb) {
  struct interval_data data;
  data.intervals = alloc(irb->arena, sizeof(*data.intervals) * irb->op_count);
  data.num_intervals = 0;

  memset(data.intervals, 0, sizeof(*data.intervals) * irb->op_count);

  struct ir_stmt *stmt = irb->first;
  while (stmt) {
    struct ir_op *op = stmt->first;
    while (op) {
      debug("constructing interval for op %zu", op->id);
      op->lcl_idx = NO_LCL;
      op->reg = NO_REG;

      debug_assert(op->id < irb->op_count, "out of range!");
      struct interval *interval = &data.intervals[op->id];

      interval->op = op;
      interval->start = data.num_intervals;
      debug_assert(op->metadata == NULL, "metadata left over in op during LSRA, will be overwritten");
      op->metadata = interval;

      walk_op_uses(op, op_used_callback, &data);
      data.num_intervals++;

      op = op->succ;
    }

    stmt = stmt->succ;
  }

  return data;
}

void spill_at_interval(struct interval *intervals, size_t cur_interval, size_t *active, size_t *num_active) {
  struct interval *spill = &intervals[active[*num_active - 1]];
  if (spill->end > intervals[cur_interval].end) {
    intervals[cur_interval].op->reg = spill->op->reg;
    spill->op->reg = REG_SPILLED;

    // remove `spill`
    (*num_active)--;

    // insert into `active`, sorted by end point
    for (size_t j = 0; j <= *num_active; j++) {
      if (j == *num_active || intervals[active[j + 1]].end > intervals[cur_interval].end) {
        active[j] = cur_interval;
        (*num_active)++;
        break;
      }
    }
  } else {
    intervals[cur_interval].op->reg = REG_SPILLED;
  }
}

void expire_old_intervals(struct interval *intervals, struct interval *cur_interval, size_t *active, size_t *num_active, unsigned long *reg_pool) {
  size_t num_expired_intervals = 0;
  
  for (size_t i = 0; i < *num_active; i++) {
    struct interval *interval = &intervals[active[i]];
    // debug("cur interval %zu, intervals[i].end %zu", cur_interval->start, interval->end);
    if (cur_interval->start <= interval->end) {
      break;
    }

    num_expired_intervals++;

    // debug("marking reg %ul free", interval->reg);
    MARK_REG_FREE(*reg_pool, interval->op->reg);
  }

  // shift the active array down 
  memmove(active, &active[num_expired_intervals], sizeof(*active) * (*num_active - num_expired_intervals));
  *num_active -= num_expired_intervals;
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


      struct ir_op *store = insert_after_ir_op(data->irb, *op, IR_OP_TY_STORE_LCL, IR_OP_VAR_TY_NONE);
      store->store_lcl.lcl_idx = (*op)->lcl_idx;
      store->store_lcl.value = *op;
    }
    
    struct ir_op *load = insert_before_ir_op(data->irb, data->consumer, IR_OP_TY_LOAD_LCL, (*op)->var_ty);
    load->load_lcl.lcl_idx = (*op)->lcl_idx;

    *op = load;
  }
}

void alloc_fixup(struct ir_builder *irb, struct interval_data *data) {
  UNUSED_ARG(data);

  struct ir_stmt *stmt = irb->first;
  while (stmt) {
    struct ir_op *op = stmt->first;
    while (op) {
      struct alloc_fixup_data metadata = { .irb = irb, .consumer = op };
      walk_op_uses(op, alloc_fixup_callback, &metadata);

      op = op->succ;
    }

    stmt = stmt->succ;
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
    fprintf(file, "start=%05zu, end=%05zu | ", interval->start, interval->end);
  } else {
    fprintf(file, "no associated interval | ");
  }

  op->metadata = NULL;

  switch(op->reg) {
  case NO_REG:
    fprintf(file, "    (UNASSIGNED)");
    break;
  case REG_SPILLED:
    fprintf(file, "    (SPILLED)");
    break;
  default:
    fprintf(file, "    register=%zu", op->reg);
    break;
  }
}

struct interval_data register_alloc_pass(struct ir_builder *irb, struct reg_info reg_info) {
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
    // debug("%zu active", num_active);
    struct interval *interval = &intervals[i];
    expire_old_intervals(intervals, interval, active, &num_active, &reg_pool);

    if (num_active == reg_info.num_regs) {
      spill_at_interval(intervals, i, active, &num_active);
    } else {
      // debug("reg pool: %ul", reg_pool);
      unsigned long free_reg = tzcnt(reg_pool);
      // debug("free reg: %ul", free_reg);
      debug_assert(free_reg != sizeof(reg_pool) * 8, "reg pool unexpectedly empty!");

      MARK_REG_USED(reg_pool, free_reg);
      debug("ASSIGNED REG %zu FOR OP %zu", free_reg, interval->op->id);
      interval->op->reg = free_reg;

      bool inserted = false;
      // insert into `active`, sorted by end point
      for (size_t j = 0; j <= num_active; j++) {
        if (j == num_active || intervals[active[j + 1]].end > intervals[i].end) {
          active[j] = i;
          num_active++;
          inserted = true;
          break;
        }
      }

      invariant_assert(inserted, "didn't insert interval!");
    }
  }

  return data;
}

void register_alloc(struct ir_builder *irb, struct reg_info reg_info) {
  struct interval_data data = register_alloc_pass(irb, reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals), compare_interval_id);
  debug_print_ir(irb, irb->first, print_ir_intervals, data.intervals);

  // insert LOAD and STORE ops as needed
  alloc_fixup(irb, &data);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals), compare_interval_id);
  debug_print_ir(irb, irb->first, print_ir_intervals, data.intervals);
  
  register_alloc_pass(irb, reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals), compare_interval_id);
  debug_print_ir(irb, irb->first, print_ir_intervals, data.intervals);
}
