#include "lsra.h"

#include "bit_twiddle.h"
#include "bitset.h"
#include "compiler.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "liveness.h"
#include "log.h"
#include "util.h"
#include "vector.h"

#include <limits.h>

enum register_alloc_mode {
  REGISTER_ALLOC_MODE_REGS,
  REGISTER_ALLOC_MODE_LCLS,
};

struct register_alloc_info {
  struct reg_info reg_info;
};

void spill_at_interval(struct ir_builder *irb, struct interval *intervals,
                       size_t cur_interval, size_t *active,
                       size_t *num_active) {
  struct interval *spill = &intervals[active[*num_active - 1]];
  if (spill->end > intervals[cur_interval].end) {
    intervals[cur_interval].op->reg = spill->op->reg;
    spill_op(irb, spill->op);

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
    spill_op(irb, intervals[cur_interval].op);
  }
}

void expire_old_intervals(struct interval *intervals,
                          struct interval *cur_interval, size_t *active,
                          size_t *num_active, struct bitset *reg_pool) {
  size_t num_expired_intervals = 0;

  for (size_t i = 0; i < *num_active; i++) {
    struct interval *interval = &intervals[active[i]];
    if (interval->end >= cur_interval->start) {
      break;
    }

    num_expired_intervals++;

    if (interval->op->reg != REG_SPILLED) {
      // intervals can contain ops that were spilled for other reasons
      // but are still alive
      // so only free a reg if this interval actually *has* a reg
      bitset_set(reg_pool, interval->op->reg, true);
    }
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

struct fixup_spills_data {
  struct ir_builder *irb;
  struct ir_op *consumer;
};

bool op_needs_reg(struct ir_op *op) {
  // addressof operator does not need a reg, so does not need a load
  return op->ty != IR_OP_TY_ADDR;
}

void fixup_spills_callback(struct ir_op **op, void *metadata) {
  struct fixup_spills_data *data = metadata;

  if (data->consumer->ty != IR_OP_TY_PHI &&
      data->consumer->flags & IR_OP_FLAG_SPILL) {
    // the store that consumes (and stores) this value will be marked with this
    // flag it of course does not need to have a load (as it uses the op after
    // creation)
    return;
  }

  if ((*op)->reg == REG_SPILLED) {
    debug_assert((*op)->lcl, "op %zu should have had local by `%s`", (*op)->id,
                 __func__);
    if (op_needs_reg(data->consumer)) {
      struct ir_op *load;
      if (data->consumer->ty == IR_OP_TY_PHI) {
        load = replace_ir_op(data->irb, data->consumer, IR_OP_TY_LOAD_LCL,
                             (*op)->var_ty);
      } else {
        load = insert_before_ir_op(data->irb, data->consumer, IR_OP_TY_LOAD_LCL,
                                   (*op)->var_ty);
      }
      load->load_lcl.lcl = *op;
      load->lcl = (*op)->lcl;
      load->reg = data->consumer->reg;
      load->flags |= IR_OP_FLAG_SPILL;

      *op = load;
    } else {
      *op = (*op)->lcl->store;
    }
  }
}

void fixup_spills(struct ir_builder *irb, struct interval_data *data) {
  UNUSED_ARG(data);

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {

    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        if (op->ty == IR_OP_TY_PHI) {
          // phi being spilled means nothing
          // as the creators will all be spilled and do the store
          op = op->succ;
          continue;
        }

        // walk all things this op uses, and add loads for any that are spilled
        struct fixup_spills_data metadata = {.irb = irb, .consumer = op};

        walk_op_uses(op, fixup_spills_callback, &metadata);

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
                                         struct register_alloc_info info,
                                         bool *spilled) {

  struct interval_data data = construct_intervals(irb);

  *spilled = false;

  BEGIN_SUB_STAGE("INTERVALS");
  if (log_enabled()) {
    debug_print_ir(stderr, irb, print_ir_intervals, data.intervals);
  }

  struct interval *intervals = data.intervals;
  size_t num_intervals = data.num_intervals;

  qsort(intervals, num_intervals, sizeof(*intervals),
        sort_interval_by_start_point);

  size_t num_regs = info.reg_info.num_volatile + info.reg_info.num_nonvolatile;

  size_t *active = arena_alloc(irb->arena, sizeof(size_t) * num_regs);
  size_t num_active = 0;

  struct bitset *reg_pool = bitset_create(num_regs);
  bitset_clear(reg_pool, true);

  // intervals must be sorted by start point
  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &intervals[i];
    expire_old_intervals(intervals, interval, active, &num_active, reg_pool);

    // update live_regs early in case the op is not value producing/marked
    // DONT_GIVE_REG and we back out
    unsigned long long free_regs = bitset_as_ull(reg_pool);
    interval->op->live_regs = ~free_regs & ((1ull << num_regs) - 1);

    if (interval->op->reg == REG_FLAGS ||
        (interval->op->flags & IR_OP_FLAG_DONT_GIVE_SLOT)) {
      continue;
    }

    if (interval->op->flags & IR_OP_FLAG_MUST_SPILL) {
      // we spill here, and strip the flag for the next regalloc run as then it
      // will need a register
      interval->op->flags &= ~IR_OP_FLAG_MUST_SPILL;
      if (interval->op->ty == IR_OP_TY_PHI) {
        for (size_t i = 0; i < interval->op->phi.num_values; i++) {
          struct ir_op *value = interval->op->phi.values[i];

          value->flags &= ~IR_OP_FLAG_MUST_SPILL;
        }
      }

      spill_op(irb, interval->op);
      *spilled = true;
    } else if (num_active == num_regs) {
      spill_at_interval(irb, intervals, i, active, &num_active);
      *spilled = true;
    } else {
      if (interval->op->reg == REG_FLAGS ||
          (interval->op->flags & IR_OP_FLAG_DONT_GIVE_SLOT)) {
        continue;
      }

      size_t free_slot = bitset_tzcnt(reg_pool);
      debug_assert(free_slot < num_regs, "reg pool unexpectedly empty!");

      bitset_set(reg_pool, free_slot, false);

      if (free_slot >= info.reg_info.num_volatile) {
        irb->nonvolatile_registers_used |= (1ull << free_slot);
      }

      interval->op->reg = free_slot;

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

    // re-set live regs as we may have assigned a new reg
    free_regs = bitset_as_ull(reg_pool);
    interval->op->live_regs = ~free_regs & ((1ull << num_regs) - 1);
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
            if (op->flags & IR_OP_FLAG_DONT_GIVE_SLOT) {
              unreg_left = true;
            } else {
              for (size_t i = 0; i < op->phi.num_values; i++) {
                struct ir_op *value = op->phi.values[i];
                value->reg = op->reg;
                value->flags &= ~IR_OP_FLAG_DONT_GIVE_SLOT;
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
     - after the first pass, we run another alloc to allocate local slots, and
  insert `storelcl` and `loadlcl` instructions with appropriate locals as will
  be needed
*/
void lsra_register_alloc(struct ir_builder *irb, struct reg_info reg_info) {
  struct register_alloc_info alloc_info =
      (struct register_alloc_info){.reg_info = reg_info};

  bool spill_exists = true;
  int attempts = 0;

  while (spill_exists) {
    // concerned this may not terminate
    // a cursory logic check suggests it *should* but include this so we can see
    // if it is failing
    attempts++;
    if (attempts > 10) {
      bug("LSRA didn't terminate after %d attempts", attempts);
    }

    BEGIN_SUB_STAGE("REGALLOC");

    clear_metadata(irb);
    struct interval_data data =
        register_alloc_pass(irb, alloc_info, &spill_exists);

    qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
          compare_interval_id);

    if (log_enabled()) {
      debug_print_ir(stderr, irb, print_ir_intervals, data.intervals);
    }

    BEGIN_SUB_STAGE("SPILL HANDLING");

    // insert LOAD and STORE ops as needed
    fixup_spills(irb, &data);

    qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
          compare_interval_id);

    if (log_enabled()) {
      debug_print_ir(stderr, irb, print_ir_intervals, NULL);
    }
  }
}
