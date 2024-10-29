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

struct register_alloc_info {
  struct reg_info integral_reg_info;
  struct reg_info float_reg_info;
};

struct register_alloc_state {
  struct interval_data interval_data;

  // contains indices into the intervals above
  size_t *active;
  size_t num_active;

  struct bitset *gp_reg_pool;
  struct bitset *fp_reg_pool;
};

void spill_at_interval(struct ir_func *irb, struct register_alloc_state *state,
                       size_t cur_interval) {
  struct interval *intervals = state->interval_data.intervals;

  struct interval *spill = &intervals[state->active[state->num_active - 1]];
  if (spill->end > intervals[cur_interval].end) {
    intervals[cur_interval].op->reg = spill->op->reg;
    spill_op(irb, spill->op);

    // remove `spill`
    state->num_active--;

    // insert into `active`, sorted by end point
    for (size_t j = 0; j <= state->num_active; j++) {
      if (j == state->num_active) {
        state->active[j] = cur_interval;
        state->num_active++;
        break;
      } else if (intervals[state->active[j]].end >
                 intervals[cur_interval].end) {
        memmove(&state->active[j + 1], &state->active[j],
                sizeof(*state->active) * (state->num_active - j));
        state->active[j] = cur_interval;
        state->num_active++;
        break;
      }
    }
  } else {
    spill_op(irb, intervals[cur_interval].op);
  }
}

void expire_old_intervals(struct register_alloc_state *state,
                          struct interval *cur_interval) {
  size_t num_expired_intervals = 0;

  for (size_t i = 0; i < state->num_active; i++) {
    struct interval *interval =
        &state->interval_data.intervals[state->active[i]];
    if (interval->end >= cur_interval->start) {
      break;
    }

    num_expired_intervals++;

    // intervals can contain ops that were spilled for other reasons
    // but are still alive
    // so only free a reg if this interval actually *has* a reg
    if (interval->op->reg.ty == IR_REG_TY_INTEGRAL) {
      bitset_set(state->gp_reg_pool, interval->op->reg.idx, true);
    } else if (interval->op->reg.ty == IR_REG_TY_FP) {
      bitset_set(state->fp_reg_pool, interval->op->reg.idx, true);
    }
  }

  // shift the active array down

  state->num_active -= num_expired_intervals;
  memmove(state->active, &state->active[num_expired_intervals],
          sizeof(*state->active) * (state->num_active));
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
  struct ir_func *irb;
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

  if ((*op)->reg.ty == IR_REG_TY_SPILLED) {
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
      load->load_lcl.lcl = (*op)->lcl;
      load->reg = data->consumer->reg;
      load->flags |= IR_OP_FLAG_SPILL;

      *op = load;
    } else {
      *op = (*op)->lcl->store;
    }
  }
}

void fixup_spills(struct ir_func *irb, struct interval_data *data) {
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

typedef bool (*op_needs_alloc)(struct ir_op *);

void insert_active(struct register_alloc_state *state, size_t cur_interval) {
  struct interval *intervals = state->interval_data.intervals;
  size_t *active = state->active;

  // insert sorted by endpoint
  // TODO: use a heap
  for (size_t j = 0; j <= state->num_active; j++) {
    if (j == state->num_active) {
      state->active[j] = cur_interval;
      state->num_active++;
      break;
    } else if (intervals[active[j]].end > intervals[cur_interval].end) {
      memmove(&active[j + 1], &active[j],
              sizeof(*active) * (state->num_active - j));
      active[j] = cur_interval;
      state->num_active++;
      break;
    }
  }
}

struct interval_data register_alloc_pass(struct ir_func *irb,
                                         struct reg_info *info, bool *spilled) {

  struct interval_data data = construct_intervals(irb);
  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        sort_interval_by_start_point);

  size_t num_gp_regs =
      info->gp_registers.num_volatile + info->gp_registers.num_nonvolatile;
  size_t num_fp_regs =
      info->fp_registers.num_volatile + info->fp_registers.num_nonvolatile;

  struct register_alloc_state state = {
      .interval_data = data,
      .active =
          arena_alloc(irb->arena, sizeof(size_t) * (num_fp_regs + num_gp_regs)),
      .num_active = 0,
      .gp_reg_pool = bitset_create(num_gp_regs, true),
      .fp_reg_pool = bitset_create(num_fp_regs, true)};

  bitset_clear(state.gp_reg_pool, true);
  bitset_clear(state.fp_reg_pool, true);

  BEGIN_SUB_STAGE("INTERVALS");
  if (log_enabled()) {
    debug_print_ir_func(stderr, irb, print_ir_intervals, data.intervals);
  }

  struct interval *const intervals = state.interval_data.intervals;
  const size_t num_intervals = state.interval_data.num_intervals;
  // intervals must be sorted by start point
  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &intervals[i];
    expire_old_intervals(&state, interval);

    if (interval->op->reg.ty == IR_REG_TY_FLAGS ||
        (interval->op->flags & IR_OP_FLAG_DONT_GIVE_REG) ||
        !op_produces_value(interval->op->ty)) {
      continue;
    }

    struct bitset *reg_pool;
    struct bitset *all_used_reg_pool;
    enum ir_reg_ty reg_ty;
    if (var_ty_is_integral(&interval->op->var_ty)) {
      reg_ty = IR_REG_TY_INTEGRAL;
      reg_pool = state.gp_reg_pool;
      all_used_reg_pool = irb->reg_usage.gp_registers_used;
    } else if (var_ty_is_fp(&interval->op->var_ty)) {
      reg_ty = IR_REG_TY_FP;
      reg_pool = state.fp_reg_pool;
      all_used_reg_pool = irb->reg_usage.fp_registers_used;
    } else {
      bug("don't know what register type to allocate for op %zu",
          interval->op->id);
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
    } else if (bitset_any(reg_pool, true)) {
      // we can allocate a register from the pool
      size_t free_slot = bitset_tzcnt(reg_pool);
      debug_assert(free_slot < bitset_length(reg_pool),
                   "reg pool unexpectedly empty!");

      bitset_set(reg_pool, free_slot, false);
      bitset_set(all_used_reg_pool, free_slot, true);

      interval->op->reg = (struct ir_reg){.ty = reg_ty, .idx = free_slot};

      size_t cur_interval = i;
      insert_active(&state, cur_interval);
    } else {
      // need to spill, no free registers
      spill_at_interval(irb, &state, i);
      *spilled = true;
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

bool op_needs_int_reg(struct ir_op *op) {
  // pointers etc live in integer registers
  return !var_ty_is_fp(&op->var_ty);
}

bool op_needs_fp_reg(struct ir_op *op) { return var_ty_is_fp(&op->var_ty); }

void lsra_register_alloc(struct ir_func *irb, struct reg_info reg_info) {
  irb->reg_usage = (struct ir_reg_usage){
      .fp_registers_used =
          bitset_create(reg_info.fp_registers.num_volatile +
                            reg_info.fp_registers.num_nonvolatile,
                        false),
      .gp_registers_used =
          bitset_create(reg_info.gp_registers.num_volatile +
                            reg_info.gp_registers.num_nonvolatile,
                        false),
  };

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

    spill_exists = false;

    clear_metadata(irb);
    struct interval_data data =
        register_alloc_pass(irb, &reg_info, &spill_exists);

    qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
          compare_interval_id);

    if (log_enabled()) {
      debug_print_ir_func(stderr, irb, print_ir_intervals, data.intervals);
    }

    BEGIN_SUB_STAGE("SPILL HANDLING");

    // insert LOAD and STORE ops as needed
    fixup_spills(irb, &data);

    qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
          compare_interval_id);

    if (log_enabled()) {
      debug_print_ir_func(stderr, irb, print_ir_intervals, NULL);
    }
  }
}
