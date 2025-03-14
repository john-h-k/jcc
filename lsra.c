#include "lsra.h"

#include "bit_twiddle.h"
#include "bitset.h"
#include "hashtbl.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "liveness.h"
#include "log.h"
#include "lower.h"
#include "util.h"
#include "vector.h"

struct register_alloc_info {
  struct reg_info integral_reg_info;
  struct reg_info float_reg_info;
};

struct reg_state {
  struct ir_op *live;
  struct ir_reg reg;

  bool free;
};

struct register_alloc_state {
  struct interval_data interval_data;

  // contains indices into the intervals above
  size_t *active;
  size_t num_active;

  // contains preferred reg (or NONE if no preferred reg) for op
  // this is the backwards feed part of preferences
  // so `ret %7` will give `%7` a preferred reg in here
  // whereas `%5 = %6` will get its preference by looking at `%6`
  struct ir_reg *preferences;

  // TODO: data structure efficiency can be improved a lot in here, every time
  // we expire an interval it is an O(n) vector removal
  struct vector *gp_reg_states;
  struct vector *fp_reg_states;

  struct vector *gp_reg_pool;
  struct vector *fp_reg_pool;
};

static bool try_get_preferred_reg(struct register_alloc_state *state,
                                  enum ir_reg_ty reg_ty,
                                  struct interval *interval,
                                  struct ir_reg *reg) {
  struct ir_op *op = interval->op;

  struct ir_reg candidate = state->preferences[op->id];
  if (candidate.ty == IR_REG_TY_NONE) {
    switch (op->ty) {
    case IR_OP_TY_PHI:
      DEBUG_ASSERT(op->phi.num_values, "empty phi");
      candidate = op->phi.values[0].value->reg;
      break;
    case IR_OP_TY_MOV:
      if (op->mov.value) {
        candidate = op->mov.value->reg;
      }
      break;
    case IR_OP_TY_CAST_OP:
      candidate = op->cast_op.value->reg;
      break;
    case IR_OP_TY_UNARY_OP:
      candidate = op->unary_op.value->reg;
      break;
    case IR_OP_TY_BINARY_OP:
      candidate = op->binary_op.lhs->reg;
      break;
    default:
      break;
    }
  }

  if (candidate.ty != IR_REG_TY_NONE && candidate.ty == reg_ty) {
    *reg = candidate;
    return true;
  }

  return false;
}

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

struct call_save_lcl {
  struct ir_lcl *lcl;
  bool in_use;
  size_t size;
};

struct lsra_reg_info {
  bool has_ssp;
  size_t ssp_reg;

  size_t num_volatile_gp;
  size_t num_volatile_fp;
  size_t num_gp_regs;
  size_t num_fp_regs;
  size_t gp_spill_reg;
  size_t fp_spill_reg;

  struct hashtbl *nonvolatile_registers_used;

  struct vector *call_save_lcls;
};

static void spill_op_and_lower(struct lsra_reg_info *info, struct ir_func *irb,
                               struct ir_op *op) {
  if (op->ty == IR_OP_TY_PHI) {
    // don't actually do the store, phi elim will deal with that

    op->flags |= IR_OP_FLAG_SPILLED;
    op->lcl = ir_add_local(irb, &op->var_ty);
    return;
  }

  struct ir_op *store = ir_spill_op(irb, op);

  if (store) {
    struct ir_op *addr = ir_build_addr(irb, store);
    addr->var_ty = ir_var_ty_for_pointer_size(irb->unit);

    if (info->has_ssp) {
      addr->reg =
          (struct ir_reg){.ty = IR_REG_TY_INTEGRAL, .idx = info->ssp_reg};
    } else {
      addr->flags |= IR_OP_FLAG_CONTAINED;
    }

    store->store = (struct ir_op_store){
        .ty = IR_OP_STORE_TY_ADDR, .value = store->store.value, .addr = addr};
  }
}

static void spill_at_interval(struct lsra_reg_info *info, struct ir_func *irb,
                              struct register_alloc_state *state,
                              struct interval *last_active,
                              size_t cur_interval) {
  struct interval *intervals = state->interval_data.intervals;

    printf("spiling %zu as it ends at %zu whereas %zu ends at %zu\n", last_active->op->id, last_active->end, intervals[cur_interval].op->id, intervals[cur_interval].end);
  if (last_active->end > intervals[cur_interval].end) {
    printf("spiling %zu as it ends at %zu whereas %zu ends at %zu\n", last_active->op->id, last_active->end, intervals[cur_interval].op->id, intervals[cur_interval].end);
    // spill active
    intervals[cur_interval].op->reg = last_active->op->reg;
    spill_op_and_lower(info, irb, last_active->op);

    state->num_active--;

    insert_active(state, cur_interval);
  } else {
    // spill current interval
    spill_op_and_lower(info, irb, intervals[cur_interval].op);
  }
}

static void mark_register_live(struct register_alloc_state *state,
                               struct ir_reg reg, struct ir_op *op) {
  struct vector *reg_states;
  struct vector *reg_pool;
  if (reg.ty == IR_REG_TY_INTEGRAL) {
    reg_states = state->gp_reg_states;
    reg_pool = state->gp_reg_pool;
  } else if (reg.ty == IR_REG_TY_FP) {
    reg_states = state->fp_reg_states;
    reg_pool = state->fp_reg_pool;
  } else {
    BUG("can't mark register of this type as live");
  }

  struct reg_state *reg_state = vector_get(reg_states, reg.idx);
  reg_state->free = false;

  reg_state->live = op;

  size_t num_free = vector_length(reg_pool);
  for (size_t i = 0; i < num_free; i++) {
    size_t idx = *(size_t *)vector_get(reg_pool, i);

    if (idx == reg.idx) {
      vector_remove_at(reg_pool, i);
      break;
    }
  }
}

static void mark_register_free(struct register_alloc_state *state,
                               struct ir_reg reg) {
  struct vector *reg_states;
  struct vector *reg_pool;
  if (reg.ty == IR_REG_TY_INTEGRAL) {
    reg_states = state->gp_reg_states;
    reg_pool = state->gp_reg_pool;
  } else if (reg.ty == IR_REG_TY_FP) {
    reg_states = state->fp_reg_states;
    reg_pool = state->fp_reg_pool;
  } else {
    BUG("can't mark register of this type as free");
  }

  struct reg_state *reg_state = vector_get(reg_states, reg.idx);

  if (reg_state->free) {
    return;
  }

  vector_push_back(reg_pool, &reg.idx);

  reg_state->live = NULL;
  reg_state->free = true;
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
    if (interval->op->reg.ty != IR_REG_TY_INTEGRAL &&
        interval->op->reg.ty != IR_REG_TY_FP) {
      continue;
    }

    // if (!(interval->op->flags & IR_OP_FLAG_SPILLED)) {
    mark_register_free(state, interval->op->reg);
    // }

    for (size_t j = 0; j < interval->op->write_info.num_reg_writes; j++) {
      struct ir_reg write = interval->op->write_info.writes[j];

      mark_register_free(state, write);
    }
  }

  // shift the active array down
  state->num_active -= num_expired_intervals;
  memmove(state->active, &state->active[num_expired_intervals],
          sizeof(*state->active) * (state->num_active));
}

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

static void fixup_spills_callback(struct ir_op **op,
                                  UNUSED enum ir_op_use_ty use_ty,
                                  void *metadata) {
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
      struct ir_op *addr;
      if (data->consumer->ty == IR_OP_TY_PHI) {
        addr = ir_replace_op(data->irb, data->consumer, IR_OP_TY_ADDR,
                             ir_var_ty_for_pointer_size(data->irb->unit));
      } else {
        addr = ir_insert_before_op(data->irb, data->consumer, IR_OP_TY_ADDR,
                                   ir_var_ty_for_pointer_size(data->irb->unit));
      }
      addr->addr =
          (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = (*op)->lcl};
      addr->reg =
          (struct ir_reg){.ty = IR_REG_TY_INTEGRAL, .idx = data->info.ssp_reg};

      struct ir_op *load =
          ir_insert_after_op(data->irb, addr, IR_OP_TY_LOAD, (*op)->var_ty);
      load->load = (struct ir_op_load){.ty = IR_OP_LOAD_TY_ADDR, .addr = addr};

      if (ir_var_ty_is_integral(&load->var_ty)) {
        load->reg = (struct ir_reg){.ty = IR_REG_TY_INTEGRAL,
                                    .idx = data->info.gp_spill_reg};
      } else if (ir_var_ty_is_fp(&load->var_ty)) {
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

        ir_walk_op_uses(op, fixup_spills_callback, &metadata);

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

static void force_spill_register(struct lsra_reg_info *info,
                                 struct ir_func *irb,
                                 struct register_alloc_state *state,
                                 struct ir_reg reg, struct interval *interval) {
  // for some reason this check leads to regs not being spilled
  // if (!bitset_get(reg_pool, reg.idx)) {
  //   return;
  // }

  // HACK: need to properly store reg->op map

  for (size_t j = 0; j < state->num_active; j++) {
    struct interval *active = &state->interval_data.intervals[state->active[j]];
    struct ir_op *consumer = active->op;

    if (active->start > interval->end) {
      continue;
    }

    if (consumer->reg.ty == reg.ty && consumer->reg.idx == reg.idx) {
      spill_op_and_lower(info, irb, consumer);
      consumer->flags |= IR_OP_FLAG_SPILLED;
    }
  }

  mark_register_live(state, reg, interval->op);
}

static int sort_interval_by_desc_start_point(const void *a, const void *b) {
  return -sort_interval_by_start_point(*(const struct interval *const *)a,
                                       *(const struct interval *const *)b);
}

struct add_to_prefs_callback_data {
  struct ir_op *consumer;
  struct register_alloc_state *state;
};

static void add_to_prefs_callback(struct ir_op **op,
                                  UNUSED enum ir_op_use_ty use_ty,
                                  void *metadata) {
  struct add_to_prefs_callback_data *data = metadata;

  if ((*op)->flags & IR_OP_FLAG_FIXED_REG) {
    return;
  }

  struct ir_reg *pref = &data->state->preferences[(*op)->id];
  if (pref->ty != IR_REG_TY_NONE) {
    // only consider the first preference
    // there is probably a better way to pick between them
    return;
  }

  *pref = data->consumer->reg;
}

static struct ir_lcl *get_call_save_lcl(struct ir_func *irb,
                                        struct lsra_reg_info *state,
                                        struct ir_var_ty *var_ty
                                      ) {
  struct ir_var_ty_info info = ir_var_ty_info(irb->unit, var_ty);

  size_t num_call_save_lcls = vector_length(state->call_save_lcls);
  for (size_t i = 0; i < num_call_save_lcls; i++) {
    struct call_save_lcl *save = vector_get(state->call_save_lcls, i);

    if (!save->in_use) {
      if (save->size < info.size) {
        // expand this one
        save->lcl->var_ty = *var_ty;
        save->size = info.size;
      }

      save->in_use = true;
      return save->lcl;
    }
  }

  struct call_save_lcl new = {
    .lcl = ir_add_local(irb, var_ty),
    .size = info.size,
    .in_use = true
  };

  new.lcl->flags |= IR_LCL_FLAG_CALL_SAVE;  

  vector_push_back(state->call_save_lcls, &new);

  return new.lcl;
}

static void clear_call_save_lcls(struct lsra_reg_info *state) {
  size_t num_call_save_lcls = vector_length(state->call_save_lcls);
  for (size_t i = 0; i < num_call_save_lcls; i++) {
    struct call_save_lcl *save = vector_get(state->call_save_lcls, i);
    save->in_use = false;
  }
}

static struct interval_data register_alloc_pass(struct ir_func *irb,
                                                struct lsra_reg_info *info) {

  struct interval_data data = construct_intervals(irb);

  struct vector *fixed_reg_intervals = vector_create(sizeof(struct interval *));

  for (size_t i = 0; i < data.num_intervals; i++) {
    struct interval *interval = &data.intervals[i];
    struct ir_op *op = interval->op;

    if (op->flags & IR_OP_FLAG_FIXED_REG) {
      vector_push_back(fixed_reg_intervals, &interval);
    }
  }

  if (vector_length(fixed_reg_intervals)) {
    qsort(vector_head(fixed_reg_intervals), vector_length(fixed_reg_intervals),
          vector_element_size(fixed_reg_intervals),
          sort_interval_by_desc_start_point);
  }

  // we reserve one reg in each bank for spills

  struct register_alloc_state state = {
      .interval_data = data,
      .active = arena_alloc(
          irb->arena, sizeof(size_t) * (info->num_fp_regs + info->num_gp_regs)),
      .num_active = 0,
      .preferences =
          arena_alloc(irb->arena, sizeof(struct ir_reg) * irb->op_count),
      .fp_reg_states = vector_create(sizeof(struct reg_state)),
      .gp_reg_states = vector_create(sizeof(struct reg_state)),
      .gp_reg_pool = vector_create(sizeof(size_t)),
      .fp_reg_pool = vector_create(sizeof(size_t))};

  memset(state.preferences, 0, sizeof(struct ir_reg) * irb->op_count);

  struct ir_func_iter iter = ir_func_iter(irb, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *iter_op;
  while (ir_func_iter_next(&iter, &iter_op)) {
    if (!(iter_op->flags & IR_OP_FLAG_FIXED_REG)) {
      continue;
    }

    struct add_to_prefs_callback_data cb_data = {.state = &state,
                                                 .consumer = iter_op};
    ir_walk_op_uses(iter_op, add_to_prefs_callback, &cb_data);
  }

#define ADD_REGS(start, end, reg_ty, name)                                     \
  for (size_t i = start; i > end; i--) {                                       \
    size_t idx = i - 1;                                                        \
                                                                               \
    struct reg_state reg = {                                                   \
        .live = NULL, .reg = {.ty = reg_ty, .idx = idx}, .free = true};        \
                                                                               \
    if (info->has_ssp && info->ssp_reg == idx) {                               \
      reg.free = false;                                                        \
    } else {                                                                   \
      vector_push_back(state.name##_reg_pool, &idx);                           \
    }                                                                          \
                                                                               \
    vector_push_back(state.name##_reg_states, &reg);                           \
  }

  // prioritise volatile
  // TODO: make each interval select vol/nonvol preference based on live range
  ADD_REGS(info->num_gp_regs, 0, IR_REG_TY_INTEGRAL, gp);
  ADD_REGS(info->num_fp_regs, 0, IR_REG_TY_FP, fp);

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

        if (live->start >= interval->op->id) {
          // not yet live
          continue;
        }

        if (live->end <= interval->op->id) {
          // don't need save
          continue;
        }

        if ((reg.ty == IR_REG_TY_INTEGRAL && reg.idx < info->num_volatile_gp) ||
            (reg.ty == IR_REG_TY_FP && reg.idx < info->num_volatile_fp)) {
          struct ir_lcl *lcl = get_call_save_lcl(irb, info, &live->op->var_ty);

          struct ir_var_ty ptr_int = ir_var_ty_for_pointer_size(irb->unit);
          struct ir_op *store_addr =
              ir_insert_before_op(irb, interval->op, IR_OP_TY_ADDR, ptr_int);

          struct ir_op *load_addr =
              ir_insert_after_op(irb, interval->op, IR_OP_TY_ADDR, ptr_int);

          // FIXME: need to make sure the save locals are allocated low enough on the stack that they can definitely be contained
          store_addr->flags |= IR_OP_FLAG_CONTAINED;
          load_addr->flags |= IR_OP_FLAG_CONTAINED;

          store_addr->addr =
              (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};

          load_addr->addr =
              (struct ir_op_addr){.ty = IR_OP_ADDR_TY_LCL, .lcl = lcl};

          struct ir_op *save = ir_insert_after_op(
              irb, store_addr, IR_OP_TY_STORE, IR_VAR_TY_NONE);
          save->reg = reg;
          save->store = (struct ir_op_store){
              .ty = IR_OP_STORE_TY_ADDR, .addr = store_addr, .value = live->op};

          struct ir_op *reload = ir_insert_after_op(
              irb, load_addr, IR_OP_TY_LOAD, live->op->var_ty);
          reload->reg = reg;
          reload->flags |= IR_OP_FLAG_SIDE_EFFECTS;
          reload->load = (struct ir_op_load){
              .ty = IR_OP_LOAD_TY_ADDR,
              .addr = load_addr,
          };
        }
      }

      clear_call_save_lcls(info);
    }

    if (interval->op->reg.ty == IR_REG_TY_FLAGS ||
        (interval->op->flags & IR_OP_FLAG_CONTAINED) ||
        !ir_op_produces_value(interval->op)) {
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

      spill_op_and_lower(info, irb, interval->op);
      continue;
    }

    struct vector *reg_pool;
    struct vector *reg_states;
    enum ir_reg_ty reg_ty;
    size_t spill_reg;
    if (ir_var_ty_is_integral(&interval->op->var_ty)) {
      reg_ty = IR_REG_TY_INTEGRAL;
      reg_pool = state.gp_reg_pool;
      reg_states = state.gp_reg_states;
      spill_reg = info->gp_spill_reg;
    } else if (ir_var_ty_is_fp(&interval->op->var_ty)) {
      reg_ty = IR_REG_TY_FP;
      reg_pool = state.fp_reg_pool;
      reg_states = state.fp_reg_states;
      spill_reg = info->fp_spill_reg;
    } else {
      BUG("don't know what register type to allocate for op %zu",
          interval->op->id);
    }

    size_t pref_reg = SIZE_MAX;
    if (interval->op->flags & IR_OP_FLAG_FIXED_REG) {
      DEBUG_ASSERT(interval->op->reg.ty != IR_REG_TY_NONE,
                   "op had .fixed_reg but no reg was given");
      DEBUG_ASSERT(interval->op->reg.ty == reg_ty,
                   "fixed reg was not of right type. if you have moved fp to a "
                   "gp reg (or vice versa) you must also change op type");

      force_spill_register(info, irb, &state, interval->op->reg, interval);

      pref_reg = interval->op->reg.idx;

      // FIXME: logic here wrt active intervals definitely needs fixing
      // also, this does not respect reg.ty but should
    }

    for (size_t j = 0; j < interval->op->write_info.num_reg_writes; j++) {
      force_spill_register(info, irb, &state,
                           interval->op->write_info.writes[j], interval);
    }

    if (pref_reg == SIZE_MAX) {
      // look for a register we can use that isn't used by a fixed reg
      // put available regs into a bitmask then remove them as we look through
      // the fixed regs with overlapping intervals

      unsigned long long fixed_regs = 0;

      for (size_t j = vector_length(fixed_reg_intervals); j; j--) {
        struct interval *fixed =
            *(struct interval **)vector_get(fixed_reg_intervals, j - 1);

        if (fixed->start > interval->end || fixed->end <= interval->start) {
          continue;
        }

        for (size_t k = 0; k < fixed->op->write_info.num_reg_writes; k++) {
          struct ir_reg write = fixed->op->write_info.writes[k];

          if (write.ty == reg_ty) {
            fixed_regs |= (1 << write.idx);
          }
        }

        if (fixed->start == interval->end) {
          continue;
        }

        struct ir_op *fixed_op = fixed->op;
        if (fixed_op->reg.ty == reg_ty) {
          fixed_regs |= (1 << fixed_op->reg.idx);
        }
      }

      struct ir_reg preferred;
      if (try_get_preferred_reg(&state, reg_ty, interval, &preferred) &&
          !NTH_BIT(fixed_regs, preferred.idx) &&
          preferred.idx < vector_length(reg_states) &&
          ((struct reg_state *)vector_get(reg_states, preferred.idx))->free) {
        pref_reg = preferred.idx;
      } else {
        for (size_t j = vector_length(reg_pool); j; j--) {
          size_t idx = *(size_t *)vector_get(reg_pool, j - 1);

          if (!NTH_BIT(fixed_regs, idx)) {
            pref_reg = idx;
            break;
          }
        }
      }
    }

    if (pref_reg != SIZE_MAX) {
      size_t free_slot = pref_reg;

      struct ir_reg reg = {.ty = reg_ty, .idx = free_slot};
      mark_register_live(&state, reg, interval->op);

      if ((reg.ty == IR_REG_TY_INTEGRAL && reg.idx >= info->num_volatile_gp) ||
          (reg.ty == IR_REG_TY_FP && reg.idx >= info->num_volatile_fp)) {
        hashtbl_insert(info->nonvolatile_registers_used, &reg, NULL);
      }

      interval->op->reg = (struct ir_reg){.ty = reg_ty, .idx = free_slot};

      size_t cur_interval = i;
      insert_active(&state, cur_interval);
    } else {
      // need to spill, no free registers
      interval->op->reg = (struct ir_reg){.ty = reg_ty, .idx = spill_reg};

      DEBUG_ASSERT(state.num_active, "spilled but no active intervals");
      size_t *last_active = &state.active[state.num_active - 1];
      while (true) {
        struct ir_op *op = state.interval_data.intervals[*last_active].op;
        if (reg_ty == IR_REG_TY_INTEGRAL &&
            ir_var_ty_is_integral(&op->var_ty)) {
          break;
        } else if (reg_ty == IR_REG_TY_FP && ir_var_ty_is_fp(&op->var_ty)) {
          break;
        }

        last_active--;
      }

      spill_at_interval(info, irb, &state,
                        &state.interval_data.intervals[*last_active], i);
    }
  }

  vector_free(&fixed_reg_intervals);
  vector_free(&state.fp_reg_states);
  vector_free(&state.gp_reg_states);
  vector_free(&state.fp_reg_pool);
  vector_free(&state.gp_reg_pool);

  return data;
}

/* Performs register allocation on the IR
     - phi elimination must have occured earlier
     - registers may be spilt to new local variables

*/
void lsra_register_alloc(struct ir_func *irb, struct reg_info reg_info) {
  // FIXME: is `NULL` safe here? padding may be problem...

  struct hashtbl *nonvolatile_registers_used =
      hashtbl_create(sizeof(struct ir_reg), 0, NULL, NULL);

  bool has_ssp = false;
  size_t ssp_reg = 0;
  // FIXME: need better logic
  // this is tough because what if we only need ssp (secondary stack pointer)
  // during lsra? at the start, all locals are within one-instr depth, but
  // then during spilling we exceed this and need another
  // FIXME: temp disable SSP because it forces prologue
  // if (false && (irb->flags & IR_FUNC_FLAG_NEEDS_SSP)) {
  if (true || (irb->flags & IR_FUNC_FLAG_NEEDS_SSP)) {
    has_ssp = true;
    ssp_reg = reg_info.ssp;
  }

  // remove 1 for spill reg
  size_t num_gp_regs = reg_info.gp_registers.num_volatile +
                       reg_info.gp_registers.num_nonvolatile - 1;
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

      .nonvolatile_registers_used = nonvolatile_registers_used,

      .call_save_lcls = vector_create(sizeof(struct call_save_lcl))};

  ir_clear_metadata(irb);
  struct interval_data data = register_alloc_pass(irb, &lsra_reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  if (log_enabled()) {
    debug_print_ir_func(stderr, irb, print_ir_intervals, data.intervals);
  }

  // insert LOAD and STORE ops as needed
  fixup_spills(irb, &lsra_reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  if (log_enabled()) {
    debug_print_ir_func(stderr, irb, print_ir_intervals, NULL);
  }

  size_t num_nonvolatile_used = hashtbl_size(nonvolatile_registers_used);

  irb->reg_usage = (struct ir_reg_usage){
      .nonvolatile_used =
          arena_alloc(irb->arena, sizeof(*irb->reg_usage.nonvolatile_used) *
                                      num_nonvolatile_used),
      .num_nonvolatile_used = num_nonvolatile_used};

  struct hashtbl_iter *iter = hashtbl_iter(nonvolatile_registers_used);
  struct hashtbl_entry entry;
  size_t idx = 0;
  while (hashtbl_iter_next(iter, &entry)) {
    DEBUG_ASSERT(idx < num_nonvolatile_used, "hashtbl size mismatch");

    const struct ir_reg *reg = entry.key;

    irb->reg_usage.nonvolatile_used[idx++] = *reg;
  }

  DEBUG_ASSERT(idx == num_nonvolatile_used, "hashtbl size mismatch");

  vector_free(&lsra_reg_info.call_save_lcls);
  hashtbl_free(&nonvolatile_registers_used);
}
