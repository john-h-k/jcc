#include "lsra.h"

#include "bitset.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "liveness.h"
#include "log.h"
#include "util.h"

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

static void insert_active(struct register_alloc_state *state,
                          size_t cur_interval) {
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

static void spill_at_interval(struct ir_func *irb,
                              struct register_alloc_state *state,
                              struct interval *last_active,
                              size_t cur_interval) {
  struct interval *intervals = state->interval_data.intervals;

  if (last_active->end > intervals[cur_interval].end) {
    // spill active
    intervals[cur_interval].op->reg = last_active->op->reg;
    spill_op(irb, last_active->op);

    state->num_active--;

    insert_active(state, cur_interval);
  } else {
    // spill current interval
    spill_op(irb, intervals[cur_interval].op);
  }
}

static void expire_old_intervals(struct register_alloc_state *state,
                                 struct interval *cur_interval) {
  size_t num_expired_intervals = 0;

  for (size_t i = 0; i < state->num_active; i++) {
    struct interval *interval =
        &state->interval_data.intervals[state->active[i]];
    if (interval->end > cur_interval->start) {
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

struct lsra_reg_info {
  bool has_ssp;
  size_t ssp_reg;

  size_t num_volatile_gp;
  size_t num_volatile_fp;
  size_t num_gp_regs;
  size_t num_fp_regs;
  size_t gp_spill_reg;
  size_t fp_spill_reg;
};

struct fixup_spills_data {
  struct ir_func *irb;
  struct ir_op *consumer;

  size_t gp_spill_reg;
  size_t fp_spill_reg;

  struct lsra_reg_info info;
};

static bool op_needs_reg(struct ir_op *op) {
  // addressof operator does not need a reg, so does not need a load
  return op->ty != IR_OP_TY_ADDR;
}

static void fixup_spills_callback(struct ir_op **op, void *metadata) {
  struct fixup_spills_data *data = metadata;

  if (data->consumer->ty != IR_OP_TY_PHI &&
      data->consumer->flags & IR_OP_FLAG_SPILL) {
    // the store that consumes (and stores) this value will be marked with this
    // flag it of course does not need to have a load (as it uses the op after
    // creation)
    return;
  }

  if ((*op)->flags & IR_OP_FLAG_SPILLED) {
    DEBUG_ASSERT((*op)->lcl, "op %zu should have had local by `%s`", (*op)->id,
                 __func__);
    if (op_needs_reg(data->consumer)) {
      struct ir_op *load;
      if (data->consumer->ty == IR_OP_TY_PHI) {
        load = replace_ir_op(data->irb, data->consumer, IR_OP_TY_LOAD,
                             (*op)->var_ty);
      } else {
        load = insert_before_ir_op(data->irb, data->consumer, IR_OP_TY_LOAD,
                                   (*op)->var_ty);
      }
      load->load = (struct ir_op_load){
        .ty = IR_OP_LOAD_TY_LCL,
        .lcl = (*op)->lcl
      };

      if (var_ty_is_integral(&load->var_ty)) {
        load->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = data->info.gp_spill_reg};
      } else if (var_ty_is_fp(&load->var_ty)) {
        load->reg =
            (struct ir_reg){.ty = IR_REG_TY_FP, .idx = data->info.fp_spill_reg};
      } else {
        BUG("dont know what to allocate");
      }

      load->flags |= IR_OP_FLAG_SPILL;

      *op = load;
    } else {
      *op = (*op)->lcl->store;
    }
  }
}

static void fixup_spills(struct ir_func *irb, struct lsra_reg_info *info) {
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
        struct fixup_spills_data metadata = {
            .irb = irb, .info = *info, .consumer = op};

        walk_op_uses(op, fixup_spills_callback, &metadata);

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }
}

static int compare_interval_id(const void *a, const void *b) {
  size_t a_id = ((const struct interval *)a)->op->id;
  size_t b_id = ((const struct interval *)b)->op->id;

  return (int)((ssize_t)a_id - (ssize_t)b_id);
}

static void force_spill_register(struct ir_func *irb, struct register_alloc_state *state, struct bitset *reg_pool, struct ir_reg reg) {
  // for some reason this check leads to regs not being spilled
  // if (!bitset_get(reg_pool, reg.idx)) {
  //   return;
  // }

  bitset_set(reg_pool, reg.idx, false);

  // HACK: need to properly store reg->op map

  for (size_t j = 0; j < state->num_active; j++) {
    struct ir_op *op = state->interval_data.intervals[state->active[j]].op;

    if (op->reg.ty == reg.ty && op->reg.idx == reg.idx) {
      spill_op(irb, op);
      op->flags |= IR_OP_FLAG_SPILLED;
    }
  }
}

static struct interval_data register_alloc_pass(struct ir_func *irb,
                                                struct lsra_reg_info *info) {

  struct interval_data data = construct_intervals(irb);

  // we reserve one reg in each bank for spills

  struct register_alloc_state state = {
      .interval_data = data,
      .active = arena_alloc(
          irb->arena, sizeof(size_t) * (info->num_fp_regs + info->num_gp_regs)),
      .num_active = 0,
      .gp_reg_pool = bitset_create(info->num_gp_regs, true),
      .fp_reg_pool = bitset_create(info->num_fp_regs, true)};

  bitset_clear(state.gp_reg_pool, true);
  bitset_clear(state.fp_reg_pool, true);

  BEGIN_SUB_STAGE("INTERVALS");
  if (log_enabled()) {
    debug_print_ir_func(stderr, irb, print_ir_intervals, data.intervals);
  }

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        sort_interval_by_start_point);

  struct interval *const intervals = state.interval_data.intervals;
  const size_t num_intervals = state.interval_data.num_intervals;
  // intervals must be sorted by start point
  for (size_t i = 0; i < num_intervals; i++) {
    struct interval *interval = &intervals[i];
    expire_old_intervals(&state, interval);

    if (interval->op->ty == IR_OP_TY_CALL) {
      // need to spill everything that is volatile & active

      for (size_t j = 0; j < state.num_active; j++) {
        struct interval *live = &intervals[state.active[j]];
        struct ir_reg reg = live->op->reg;

        if (live->end <= interval->op->id) {
          // don't need save
          continue;
        }

        if ((reg.ty == IR_REG_TY_INTEGRAL && reg.idx < info->num_volatile_gp) ||
            (reg.ty == IR_REG_TY_FP && reg.idx < info->num_volatile_gp)) {

          struct ir_lcl *lcl = add_local(irb, &live->op->var_ty);
          lcl->flags |= IR_LCL_FLAG_SPILL;

          struct ir_var_ty ptr_int = var_ty_for_pointer_size(irb->unit);
          struct ir_op *store_addr = insert_before_ir_op(
              irb, interval->op, IR_OP_TY_ADDR, ptr_int);

          struct ir_op *load_addr = insert_after_ir_op(
              irb, interval->op, IR_OP_TY_ADDR, ptr_int);

          if (info->has_ssp) {
            store_addr->reg = (struct ir_reg){ .ty = IR_REG_TY_INTEGRAL, .idx = info->ssp_reg };
            load_addr->reg = (struct ir_reg){ .ty = IR_REG_TY_INTEGRAL, .idx = info->ssp_reg };
          } else {
            store_addr->flags |= IR_OP_FLAG_CONTAINED;
            load_addr->flags |= IR_OP_FLAG_CONTAINED;
          }

          store_addr->addr =
              (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};

          load_addr->addr =
              (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};

          struct ir_op *save = insert_after_ir_op(
              irb, store_addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
          save->reg = reg;
          save->store =
              (struct ir_op_store){
              .ty = IR_OP_STORE_TY_ADDR,
              .addr = store_addr, .value = live->op};

          struct ir_op *reload = insert_after_ir_op(
              irb, load_addr, IR_OP_TY_LOAD, live->op->var_ty);
          reload->reg = reg;
          reload->load = (struct ir_op_load){
            .ty = IR_OP_LOAD_TY_ADDR,
              .addr = load_addr,
          };
        }
      }
    }

    if (interval->op->reg.ty == IR_REG_TY_FLAGS ||
        (interval->op->flags & IR_OP_FLAG_CONTAINED) ||
        !op_produces_value(interval->op)) {
      continue;
    }

    if (interval->op->flags & IR_OP_FLAG_MUST_SPILL) {
      // we spill here, and strip the flag for the next regalloc run as then it
      // will need a register
      interval->op->flags &= ~IR_OP_FLAG_MUST_SPILL;
      if (interval->op->ty == IR_OP_TY_PHI) {
        for (size_t j = 0; j < interval->op->phi.num_values; j++) {
          struct ir_op *value = interval->op->phi.values[j].value;

          value->flags &= ~IR_OP_FLAG_MUST_SPILL;
        }
      }

      spill_op(irb, interval->op);
      continue;
    }

    struct bitset *reg_pool;
    struct bitset *all_used_reg_pool;
    enum ir_reg_ty reg_ty;
    size_t spill_reg;
    if (var_ty_is_integral(&interval->op->var_ty)) {
      reg_ty = IR_REG_TY_INTEGRAL;
      reg_pool = state.gp_reg_pool;
      all_used_reg_pool = irb->reg_usage.gp_registers_used;
      spill_reg = info->gp_spill_reg;
    } else if (var_ty_is_fp(&interval->op->var_ty)) {
      reg_ty = IR_REG_TY_FP;
      reg_pool = state.fp_reg_pool;
      all_used_reg_pool = irb->reg_usage.fp_registers_used;
      spill_reg = info->fp_spill_reg;
    } else {
      BUG("don't know what register type to allocate for op %zu",
          interval->op->id);
    }

    size_t pref_reg = SIZE_MAX;
    if (interval->op->flags & IR_OP_FLAG_FIXED_REG) {
      force_spill_register(irb, &state, reg_pool, interval->op->reg);

      pref_reg = interval->op->reg.idx;

      printf("fixed slot %zu for op %zu\n", pref_reg, interval->op->id);
      // FIXME: logic here wrt active intervals definitely needs fixing      
      // also, this does not respect reg.ty but should
    } else if (interval->op->ty == IR_OP_TY_BINARY_OP) {
      // TODO: generalise this so it preferentially does it for other ops that read dest (e.g aarch64 bitfield insert, x64 `not`/`neg`)
      // probably by `IR_OP_FLAG_READS_DEST` having an associated field on `ir_op` for which operand it reads
      struct ir_op *lhs = interval->op->binary_op.lhs;

      if (lhs->reg.ty != IR_REG_TY_NONE && lhs->reg.ty == reg_ty && bitset_get(reg_pool, lhs->reg.idx)) {
        printf("used rhs %zu for op %zu\n", pref_reg, interval->op->id);
        pref_reg = lhs->reg.idx;
      }
    }

    for (size_t j = 0; j < interval->op->write_info.num_reg_writes; j++) {
      force_spill_register(irb, &state, reg_pool, interval->op->write_info.writes[j]);
    }
     
    if (pref_reg != SIZE_MAX || bitset_any(reg_pool, true)) {
      size_t free_slot;

      if (pref_reg != SIZE_MAX) {
        free_slot = pref_reg;
      } else {
        // we can allocate a register from the pool
        free_slot = bitset_tzcnt(reg_pool);
        DEBUG_ASSERT(free_slot < bitset_length(reg_pool),
                     "reg pool unexpectedly empty!");
      }

      printf("found slot %zu for op %zu\n", free_slot, interval->op->id);

      bitset_set(reg_pool, free_slot, false);
      bitset_set(all_used_reg_pool, free_slot, true);

      interval->op->reg = (struct ir_reg){.ty = reg_ty, .idx = free_slot};

      size_t cur_interval = i;
      insert_active(&state, cur_interval);
    } else {
      // need to spill, no free registers
      interval->op->reg = (struct ir_reg){.ty = reg_ty, .idx = spill_reg};

      size_t *last_active = &state.active[state.num_active - 1];
      while (true) {
        struct ir_op *op = state.interval_data.intervals[*last_active].op;
        if (reg_ty == IR_REG_TY_INTEGRAL && var_ty_is_integral(&op->var_ty)) {
          break;
        } else if (reg_ty == IR_REG_TY_FP && var_ty_is_fp(&op->var_ty)) {
          break;
        }

        last_active--;
      }

      spill_at_interval(irb, &state,
                        &state.interval_data.intervals[*last_active], i);
    }
  }

  return data;
}

/* Performs register allocation on the IR
     - phi elimination must have occured earlier
     - registers may be spilt to new local variables

*/
void lsra_register_alloc(struct ir_func *irb, struct reg_info reg_info) {
  bool has_ssp = false;
  size_t ssp_reg = 0;

  size_t num_nonvolatile_gp = reg_info.gp_registers.num_nonvolatile;
  // FIXME: need better logic
  // this is tough because what if we only need ssp (secondary stack pointer) during lsra?
  // at the start, all locals are within one-instr depth, but then during spilling we exceed this and need another
  // FIXME: temp disable SSP because it forces prologue
  if (false && (irb->flags & IR_FUNC_FLAG_NEEDS_SSP)) {
  // if (true || (irb->flags & IR_FUNC_FLAG_NEEDS_SSP)) {
    num_nonvolatile_gp--;
    has_ssp = true;
    ssp_reg = reg_info.gp_registers.num_volatile + num_nonvolatile_gp;
  }

  irb->reg_usage = (struct ir_reg_usage){
      .fp_registers_used =
          bitset_create(reg_info.fp_registers.num_volatile +
                            reg_info.fp_registers.num_nonvolatile,
                        false),
      .gp_registers_used =
          bitset_create(reg_info.gp_registers.num_volatile +
                            num_nonvolatile_gp,
                        false),
  };

  if (has_ssp) {
    bitset_set(irb->reg_usage.gp_registers_used, ssp_reg, true);
  }

  size_t num_gp_regs = reg_info.gp_registers.num_volatile +
                       num_nonvolatile_gp - 1;
  size_t num_fp_regs = reg_info.fp_registers.num_volatile +
                       reg_info.fp_registers.num_nonvolatile - 1;

  size_t gp_spill_reg = num_gp_regs;
  size_t fp_spill_reg = num_fp_regs;

  struct lsra_reg_info lsra_reg_info = {
      .has_ssp = has_ssp,
      .ssp_reg = ssp_reg,
      .num_volatile_gp = reg_info.gp_registers.num_volatile,
      .num_volatile_fp = reg_info.fp_registers.num_volatile,
      .num_gp_regs = num_gp_regs,
      .num_fp_regs = num_fp_regs,
      .gp_spill_reg = gp_spill_reg,
      .fp_spill_reg = fp_spill_reg,
  };

  BEGIN_SUB_STAGE("REGALLOC");

  clear_metadata(irb);
  struct interval_data data = register_alloc_pass(irb, &lsra_reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  if (log_enabled()) {
    debug_print_ir_func(stderr, irb, print_ir_intervals, data.intervals);
  }

  BEGIN_SUB_STAGE("SPILL HANDLING");

  // insert LOAD and STORE ops as needed
  fixup_spills(irb, &lsra_reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  if (log_enabled()) {
    debug_print_ir_func(stderr, irb, print_ir_intervals, NULL);
  }
}
