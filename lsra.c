#include "lsra.h"

#include "bit_twiddle.h"
#include "compiler.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "log.h"
#include "util.h"
#include "vector.h"
#include "liveness.h"

#include <limits.h>

#define MARK_REG_USED(reg_pool, i) (reg_pool) &= ~(1 << i)
#define MARK_REG_FREE(reg_pool, i) (reg_pool) |= (1 << i)


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
      } else if (intervals[active[j]].end > intervals[cur_interval].end) {
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
    struct interval *interval = &intervals[active[i]];
    if (interval->end >= cur_interval->start) {
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
  struct interval *a_int = (struct interval *)a;
  struct interval *b_int = (struct interval *)b;
  size_t a_start = a_int->start;
  size_t b_start = b_int->start;

  if (a_start > b_start) {
    return 1;
  } else if (a_start < b_start) {
    return -1;
  }

  // put params at front, this is used for giving them the correct registers for
  // calling conv
  enum ir_op_flags a_flags = a_int->op->flags;
  enum ir_op_flags b_flags = b_int->op->flags;
  if ((a_flags & IR_OP_FLAG_PARAM) > (b_flags & IR_OP_FLAG_PARAM)) {
    return -1;
  } else if ((a_flags & IR_OP_FLAG_PARAM) < (b_flags & IR_OP_FLAG_PARAM)) {
    return 1;
  }

  if (a_int->op->id > b_int->op->id) {
    return 1;
  } else if (a_int->op->id < b_int->op->id) {
    return -1;
  }

  return 0;
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


struct interval_data register_alloc_pass(struct ir_builder *irb,
                                         struct reg_info reg_info) {

  struct interval_data data = construct_intervals(irb);

  BEGIN_SUB_STAGE("INTERVALS");
  if (log_enabled()) {
    debug_print_ir(stderr, irb, print_ir_intervals, data.intervals);
  }

  struct interval *intervals = data.intervals;
  size_t num_intervals = data.num_intervals;

  qsort(intervals, num_intervals, sizeof(*intervals),
        sort_interval_by_start_point);

  size_t num_regs = reg_info.num_volatile + reg_info.num_nonvolatile;

  size_t *active = arena_alloc(irb->arena, sizeof(size_t) * num_regs);
  size_t num_active = 0;

  invariant_assert(num_regs <= sizeof(unsigned long) * 8,
                   "LSRA does not currently support more than `sizeof(unsigned "
                   "long) * 8` register as it uses a bitmask");

  unsigned long reg_pool = (1 << num_regs) - 1;

  // intervals must be sorted by start point
  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &intervals[i];
    expire_old_intervals(intervals, interval, active, &num_active, &reg_pool);

    // update live_regs early in case the op is not value producing/marked
    // DONT_GIVE_REG and we back out
    interval->op->live_regs = ~reg_pool & ((1 << num_regs) - 1);

    if (interval->op->reg == REG_FLAGS || interval->op->reg == DONT_GIVE_REG) {
      continue;
    }

    if (interval->op->flags & IR_OP_FLAG_MUST_SPILL || num_active == num_regs) {
      spill_at_interval(intervals, i, active, &num_active);
    } else {
      if (interval->op->reg == REG_FLAGS ||
          interval->op->reg == DONT_GIVE_REG) {
        continue;
      }

      unsigned long free_reg = tzcnt(reg_pool);
      debug_assert(free_reg < num_regs, "reg pool unexpectedly empty!");

      MARK_REG_USED(reg_pool, free_reg);
      if (free_reg >= reg_info.num_volatile) {
        irb->nonvolatile_registers_used |= (1 << free_reg);
      }

      interval->op->reg = free_reg;

      size_t cur_interval = i;

      // insert into `active`, sorted by end point
      for (size_t j = 0; j <= num_active; j++) {
        if (j == num_active) {
          active[j] = cur_interval;
          num_active++;
          break;
        } else if (intervals[active[j]].end > intervals[cur_interval].end) {
          memmove(&active[j + 1], &active[j],
                  sizeof(*active) * (num_active - j));
          active[j] = cur_interval;
          num_active++;
          break;
        }
      }
    }

    interval->op->live_regs = ~reg_pool & ((1 << num_regs) - 1);
  }

  struct ir_basicblock *basicblock = irb->first;

  // continously fix up phi until everything has a register
  // by just looping we don't need to worry about order
  bool unreg_left = true;
  while (unreg_left) {
    unreg_left = false;
    basicblock = irb->first;

    while (basicblock) {
      struct ir_stmt *stmt = basicblock->first;
      while (stmt) {
        struct ir_op *op = stmt->first;
        while (op) {
          if (op->ty == IR_OP_TY_PHI) {
            if (op->reg == DONT_GIVE_REG) {
              unreg_left = true;
            } else {
              for (size_t i = 0; i < op->phi.num_values; i++) {
                struct ir_op *value = op->phi.values[i];
                value->reg = op->reg;
              }
            }
          }

          op = op->succ;
        }

        stmt = stmt->succ;
      }

      basicblock = basicblock->succ;
    }
  }

  return data;
}

/* Performs register allocation on the IR
     - phi elimination must have occured earlier
     - registers may be spilt to new local variables

  Works via a two-pass allocation approach:
     - first pass assigns "trivial" registers and marks which variables need
  spill
     - after the first pass, we insert `storelcl` and `loadlcl` instructions
  with appropriate locals as will be needed
     - we then re-run allocation, which will assign all registers including
  those needed for spills
*/
void lsra_register_alloc(struct ir_builder *irb, struct reg_info reg_info) {
  rebuild_ids(irb);

  struct interval_data data = register_alloc_pass(irb, reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  if (log_enabled()) {
    debug_print_ir(stderr, irb, print_ir_intervals, data.intervals);
  }

  BEGIN_SUB_STAGE("SPILL HANDLING");

  // continually re-spill and re-allocate until no unresolved spills are present
  // i am concerned this could sometimes not terminate
  // a cursory think about the issue suggests it *should* terminate but i may simply be missing edge cases
  while (true) {
    // removes intervals from last pass
    clear_metadata(irb);

    // insert LOAD and STORE ops as needed
    alloc_fixup(irb, &data);

    if (log_enabled()) {
      // can't print intervals here, as `alloc_fixup` inserted new ops which don't
      // have valid intervals yet
      debug_print_ir(stderr, irb, NULL, NULL);
    }

    BEGIN_SUB_STAGE("SECOND-PASS REGALLOC");

    bool spill_present = false;
    data = register_alloc_pass(irb, reg_info);
    for (size_t i = 0; i < data.num_intervals; i++) {
      if (data.intervals[i].op->reg == REG_SPILLED) {
        spill_present = true;
      }
    }

    if (!spill_present) {
      break;
    }
  }

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  if (log_enabled()) {
    debug_print_ir(stderr, irb, print_ir_intervals, NULL);
  }
}
