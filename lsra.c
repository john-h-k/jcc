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

#define MARK_REG_USED(reg_pool, i) (reg_pool) &= ~(1 << i)
#define MARK_REG_FREE(reg_pool, i) (reg_pool) |= (1 << i)

void op_used_callback(struct ir_op **op, void *cb_metadata) {
  struct interval_data *data = cb_metadata;

  debug("op callback id %zu", (*op)->id);
  struct interval *interval = &data->intervals[(*op)->id];

  interval->end = MAX(interval->end, data->num_intervals);

  if (interval->op) {
    debug("op=%zu, op start %zu, op end %zu", interval->op->id, interval->start, interval->end);
  }
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
  
  // we now have the last op in the value's basic block that contains an interval
  invariant_assert(last_op, "trying to assign phi lifetime for bb but couldn't find value-producing instruction in the block id %zu", basicblock->id);

  return last_op; 
}

#define INTERVAL_END_OF_BASICBLOCK (SIZE_T_MAX)

struct interval_data construct_intervals(struct ir_builder *irb) {
  struct interval_data data;
  data.intervals = arena_alloc(irb->arena, sizeof(*data.intervals) * irb->op_count);
  data.num_intervals = 0;

  memset(data.intervals, 0, sizeof(*data.intervals) * irb->op_count);

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    basicblock->max_interval = 0;

    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        debug("constructing interval for op %zu", op->id);
        op->lcl_idx = NO_LCL;
        op->reg = NO_REG;

        debug_assert(op->id < irb->op_count, "out of range!");
        struct interval *interval = &data.intervals[op->id];

        size_t start;

        // special case - a phi op actually starts existing at the end of its earliest basicblock
        if (op->ty == IR_OP_TY_PHI) {
          start = 0; // if the phi has no values
          for (size_t i = 0; i < op->phi.num_values; i++) {
            struct ir_op *value = op->phi.values[i];
            struct ir_basicblock *value_block = value->stmt->basicblock;

            struct interval *value_interval = &data.intervals[value->id];

            struct ir_op *last_op = last_value_producing_op(value_block);
            value_interval->end = INTERVAL_END_OF_BASICBLOCK;

            struct interval *last_interval = &data.intervals[last_op->id];
            
            interval->start = MIN(interval->start, last_interval->start);
          }
        } else {
          start = data.num_intervals;
        }

        interval->op = op;
        interval->start = start;

        if (interval->end < interval->start) {
          interval->end = interval->start; // this ensures intervals are still valid for unused values
        }

        debug_assert(op->metadata == NULL, "metadata left over in op during LSRA, will be overwritten");
        op->metadata = interval;

        walk_op_uses(op, op_used_callback, &data);
        data.num_intervals++;

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  for (size_t i = 0; i < data.num_intervals; i++) {
    struct interval *interval = &data.intervals[i];

    if (interval->end == INTERVAL_END_OF_BASICBLOCK) {
      struct ir_op *last_op = last_value_producing_op(interval->op->stmt->basicblock);
      interval->end = data.intervals[last_op->id].start;
    }
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
      if (j == *num_active) {
        active[j] = cur_interval;
        (*num_active)++;
        break;
      } else if (intervals[active[j + 1]].end > intervals[cur_interval].end) {
        memmove(&active[j + 1], &active[j], sizeof(*active) * (*num_active - j));
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

    debug("op %zu, marking reg %ul free", interval->op->id, interval->op->reg);
    MARK_REG_FREE(*reg_pool, interval->op->reg);
  }

  // shift the active array down 
  
  debug("expired: %zu", num_expired_intervals);
  *num_active -= num_expired_intervals;
  memmove(active, &active[num_expired_intervals], sizeof(*active) * (*num_active));
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

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {

    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        struct alloc_fixup_data metadata = { .irb = irb, .consumer = op };
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

  switch(op->reg) {
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

struct interval_data register_alloc_pass(struct ir_builder *irb, struct reg_info reg_info) {
  struct interval_data data = construct_intervals(irb);
  struct interval *intervals = data.intervals;
  size_t num_intervals = data.num_intervals;

  qsort(intervals, num_intervals, sizeof(*intervals), sort_interval_by_start_point);

  size_t *active = arena_alloc(irb->arena, sizeof(size_t) * reg_info.num_regs);
  size_t num_active = 0;

  invariant_assert(reg_info.num_regs <= sizeof(unsigned long) * 8,
                   "LSRA does not currently support more than `sizeof(unsigned "
                   "long) * 8` register as it uses a bitmask");

  unsigned long reg_pool = (1 << reg_info.num_regs) - 1;

  // intervals must be sorted by start point
  for (size_t i = 0; i < num_intervals; i++) {
    debug("%zu active", num_active);
    struct interval *interval = &intervals[i];
    expire_old_intervals(intervals, interval, active, &num_active, &reg_pool);

    if (!op_produces_value(interval->op->ty)) {
      // stores don't need a register
      // TODO: this logic should be more centralised and clear, not just for this op
      continue;
    }

    if (num_active == reg_info.num_regs) {
      spill_at_interval(intervals, i, active, &num_active);
    } else {
      unsigned long free_reg = tzcnt(reg_pool);
      debug("active: %ul", num_active);
      debug("reg pool: %ul", reg_pool);
      debug("reg pool free: %ul", __builtin_popcount(reg_pool));
      debug("free reg: %ul", free_reg);
      debug_assert(free_reg < reg_info.num_regs, "reg pool unexpectedly empty!");

      MARK_REG_USED(reg_pool, free_reg);
      debug("ASSIGNED REG %zu FOR OP %zu", free_reg, interval->op->id);
      interval->op->reg = free_reg;

      // insert into `active`, sorted by end point
      for (size_t j = 0; j <= num_active; j++) {
        if (j == num_active) {
          active[j] = i;
          num_active++;
          break;
        } else if (intervals[active[j + 1]].end > intervals[i].end) {
          memmove(&active[j + 1], &active[j], sizeof(*active) * (num_active - j));
          active[j] = i;
          num_active++;
          break;
        }
      }
    }
  }

  return data;
}

void register_alloc(struct ir_builder *irb, struct reg_info reg_info) {
  struct interval_data data = register_alloc_pass(irb, reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals), compare_interval_id);
  debug_print_ir(irb, irb->first, print_ir_intervals, data.intervals);

  BEGIN_SUB_STAGE("SPILL HANDLING");

  // insert LOAD and STORE ops as needed
  alloc_fixup(irb, &data);

  // can't print intervals here, as `alloc_fixup` inserted new ops which don't have valid intervals yet
  debug_print_ir(irb, irb->first, NULL, NULL);
  
  BEGIN_SUB_STAGE("SECOND-PASS REGALLOC");

  data = register_alloc_pass(irb, reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals), compare_interval_id);
  debug_print_ir(irb, irb->first, print_ir_intervals, NULL);
}
