#include "lsra.h"

#include "ir/ir.h"
#include "ir/prettyprint.h"
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

struct interval_callback_data {
  struct ir_op *op;
  struct interval_data *data;
};

void op_used_callback(struct ir_op **op, void *cb_metadata) {
  struct interval_callback_data *cb = cb_metadata;

  struct interval *interval = &cb->data->intervals[(*op)->id];

  interval->end = MAX(interval->end, cb->op->id);
}

/* Builds the intervals for each value in the SSA representation
     - IDs are rebuilt before calling this so that op ID can be used as an
   inreasing inex
     - indexes can be non-sequential but must be increasing
*/
struct interval_data construct_intervals(struct ir_builder *irb, unsigned *bb_lens) {
  struct interval_data data;
  data.intervals =
      arena_alloc(irb->arena, sizeof(*data.intervals) * irb->op_count);
  data.num_intervals = 0;

  memset(data.intervals, 0, sizeof(*data.intervals) * irb->op_count);

  // NOTE: this logic relies on MOV <PARAM> instructions existing for all params
  // AND being in order of params
  size_t arg_regs = 0;

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        op->lcl_idx = NO_LCL;

        if (op->ty == IR_OP_TY_MOV && op->mov.value == NULL) {
          op->reg = arg_regs++;
        } else {
          // reset registers unless flags because flags is never allocated
          if (op->reg != REG_FLAGS && op->reg != DONT_GIVE_REG) {
            op->reg = NO_REG;
          }
        }

        debug_assert(op->id < irb->op_count,
                     "out of range! (id %zu with opcount %zu)", op->id,
                     irb->op_count);
        struct interval *interval = &data.intervals[op->id];

        interval->op = op;
        interval->start = op->id;

        if (op->ty == IR_OP_TY_PHI) {
          size_t start = op->id;
          size_t end = op->id;

          for (size_t i = 0; i < op->phi.num_values; i++) {
            struct ir_op *prev = op->phi.values[i];

            // a phi can be dependent on itself, and in that case we still need it to be assigned a register
            if (prev->id != op->id) {
              prev->reg = DONT_GIVE_REG;
            }

            debug_assert(bb_lens[op->stmt->basicblock->id * irb->basicblock_count + prev->stmt->basicblock->id], "bb_len was 0");
            start = MIN(start, prev->id);
            end = MAX(end, bb_lens[op->stmt->basicblock->id * irb->basicblock_count + prev->stmt->basicblock->id]);
          }

          interval->start = start;
          interval->end = end;
        }

        // we can get intervals with an end before their start if the value is
        // unused fix them up to be valid
        if (interval->end < interval->start) {
          interval->end = interval->start;
        }

        debug_assert(
            op->metadata == NULL,
            "metadata left over in op during LSRA, will be overwritten");
        op->metadata = interval;

        struct interval_callback_data cb_data = {.op = op, .data = &data};

        walk_op_uses(op, op_used_callback, &cb_data);
        data.num_intervals++;

        op = op->succ;
      }

      stmt = stmt->succ;
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
    struct interval *interval = &intervals[active[i]];
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

void print_ir_intervals(FILE *file, struct ir_op *op, void *metadata) {
  UNUSED_ARG(metadata);

  struct interval *interval = op->metadata;
  if (interval) {
    invariant_assert(interval->op->id == op->id, "intervals are not ID keyed");
    fslogsl(file, "start=%05zu, end=%05zu | ", interval->start, interval->end);
  } else {
    fslogsl(file, "no associated interval | ");
  }

  switch (op->reg) {
  case NO_REG:
    fslogsl(file, "    (UNASSIGNED)");
    break;
  case REG_SPILLED:
    fslogsl(file, "    (SPILLED)");
    break;
  case DONT_GIVE_REG:
    fslogsl(file, "    (DONT)");
    break;
  case REG_FLAGS:
    fslogsl(file, "    (FLAGS)");
    break;
  default:
    fslogsl(file, "    register=%zu", op->reg);
    break;
  }
}

// walks across the blocks to determine the end range for a phi's dependency
static size_t walk_basicblock(struct ir_builder *irb, bool *basicblocks_visited,
                       struct ir_op *source_phi,
                       struct ir_basicblock *basicblock) {

  if (!basicblock || basicblocks_visited[basicblock->id]) {
    return 0;
  }

  basicblocks_visited[basicblock->id] = true;
  debug("now walking %zu", basicblock->id);

  size_t this = basicblock->last->last->id;
  size_t target_bb = source_phi->stmt->basicblock->id;

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_SPLIT: {
    // if (basicblock->split.true_target->id == target_bb) {
    //   size_t m_false = walk_basicblock(irb, basicblocks_visited, source_phi,
    //                                    basicblock->split.false_target);
    //   return MAX(this, m_false);
    // }
    // if (basicblock->split.false_target->id == target_bb) {
    //   size_t m_true = walk_basicblock(irb, basicblocks_visited, source_phi,
    //                                   basicblock->split.true_target);
    //   return MAX(this, m_true);
    // }

    size_t m_false = walk_basicblock(irb, basicblocks_visited, source_phi,
                                     basicblock->split.false_target);
    size_t m_true = walk_basicblock(irb, basicblocks_visited, source_phi,
                                    basicblock->split.true_target);
    return MAX(this, MAX(m_true, m_false));
  }
  case IR_BASICBLOCK_TY_MERGE:
    if (basicblock->merge.target->id == target_bb) {
      return this;
    }
    
    size_t target = walk_basicblock(irb, basicblocks_visited, source_phi,
                                     basicblock->merge.target);
    return MAX(this, target);
  case IR_BASICBLOCK_TY_RET:
    // this means this path did *not* reach the phi
    return 0;
  }
}

struct interval_data register_alloc_pass(struct ir_builder *irb,
                                         struct reg_info reg_info) {
  // first rebuild ids so they are sequential and increasing
  rebuild_ids(irb);

  // FIXME: *very* memory expensive |BBs|^2 space
  unsigned *basicblock_max_id = arena_alloc(irb->arena, sizeof(*basicblock_max_id) *
                                                      irb->basicblock_count *
                                                      irb->basicblock_count);

  memset(basicblock_max_id, 0,
         sizeof(*basicblock_max_id) * irb->basicblock_count * irb->basicblock_count);

  bool *basicblocks_visited = arena_alloc(
      irb->arena, sizeof(*basicblocks_visited) * irb->basicblock_count);

  // this calculates the maximum liveliness between two blocks
  // the liveliness of a phi-dependent is `basicblock_max_id[phi->bb->id * num_bb + dependent->bb->id]`
  // FIXME: this can be done MUCH more efficiently
  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        if (op->ty == IR_OP_TY_PHI) {
          for (size_t i = 0; i < op->phi.num_values; i++) {
            struct ir_op *value = op->phi.values[i];

            size_t len_id = (basicblock->id * irb->basicblock_count) + value->stmt->basicblock->id;

            size_t len;
            if (basicblock_max_id[len_id]) {
              len = basicblock_max_id[len_id];
            } else {
              memset(basicblocks_visited, 0,
                     sizeof(*basicblocks_visited) * irb->basicblock_count);

              len = walk_basicblock(irb, basicblocks_visited, op, value->stmt->basicblock);
              basicblock_max_id[len_id] = len;
            }
          }
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  struct interval_data data = construct_intervals(irb, basicblock_max_id);
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
    expire_old_intervals(intervals, interval, active, &num_active, &reg_pool);

    if (!op_produces_value(interval->op->ty)) {
      continue;
    }

    if (interval->op->reg == REG_FLAGS || interval->op->reg == DONT_GIVE_REG) {
      continue;
    }

    if (interval->op->flags & IR_OP_FLAG_MUST_SPILL ||
        num_active == reg_info.num_regs) {
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

  // removes intervals from last pass
  clear_metadata(irb);

  BEGIN_SUB_STAGE("SPILL HANDLING");

  // insert LOAD and STORE ops as needed
  alloc_fixup(irb, &data);

  if (log_enabled()) {
    // can't print intervals here, as `alloc_fixup` inserted new ops which don't
    // have valid intervals yet
    debug_print_ir(stderr, irb, NULL, NULL);
  }

  BEGIN_SUB_STAGE("SECOND-PASS REGALLOC");

  data = register_alloc_pass(irb, reg_info);

  qsort(data.intervals, data.num_intervals, sizeof(*data.intervals),
        compare_interval_id);

  if (log_enabled()) {
    debug_print_ir(stderr, irb, print_ir_intervals, NULL);
  }
}
