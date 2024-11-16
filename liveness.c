#include "liveness.h"

#include "alloc.h"
#include "bit_twiddle.h"
#include "bitset.h"
#include "ir/ir.h"
#include "log.h"
#include "util.h"

static void op_used_callback(struct ir_op **op, void *cb_metadata) {
  struct interval_callback_data *cb = cb_metadata;

  struct interval *interval = &cb->data->intervals[(*op)->id];

  interval->end = MAX(interval->end, cb->op->id);
}

// walks across the blocks to determine the end range for a phi's dependency
static size_t walk_basicblock(struct ir_func *irb, bool *basicblocks_visited,
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
  case IR_BASICBLOCK_TY_SWITCH: {
    size_t m = 0;
    for (size_t i = 0; i < basicblock->switch_case.num_cases; i++) {
      size_t m_case = walk_basicblock(irb, basicblocks_visited, source_phi,
                                      basicblock->switch_case.cases[i].target);
      m = MAX(m, m_case);
    }

    return MAX(this, m);
  }
  case IR_BASICBLOCK_TY_SPLIT: {
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

static unsigned *find_basicblock_ranges(struct ir_func *irb) {
  // FIXME: *very* memory expensive |BBs|^2 space
  unsigned *basicblock_max_id = arena_alloc(
      irb->arena, sizeof(*basicblock_max_id) * irb->basicblock_count *
                      irb->basicblock_count);

  memset(basicblock_max_id, 0,
         sizeof(*basicblock_max_id) * irb->basicblock_count *
             irb->basicblock_count);

  bool *basicblocks_visited = arena_alloc(
      irb->arena, sizeof(*basicblocks_visited) * irb->basicblock_count);

  // this calculates the maximum liveliness between two blocks
  // the liveliness of a phi-dependent is `basicblock_max_id[phi->bb->id *
  // num_bb + dependent->bb->id]`
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

            // HACK: this flag needs to enter the phi node so LSRA spills phis
            // maybe ir_lcl should have flag instead?
            if (value->flags & IR_OP_FLAG_MUST_SPILL) {
              op->flags |= IR_OP_FLAG_MUST_SPILL;
            }

            size_t len_id = (basicblock->id * irb->basicblock_count) +
                            value->stmt->basicblock->id;

            size_t len;
            if (basicblock_max_id[len_id]) {
              len = basicblock_max_id[len_id];
            } else {
              memset(basicblocks_visited, 0,
                     sizeof(*basicblocks_visited) * irb->basicblock_count);

              len = walk_basicblock(irb, basicblocks_visited, op,
                                    value->stmt->basicblock);
              basicblock_max_id[len_id] = (unsigned)len;
            }
          }
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  return basicblock_max_id;
}

/* Builds the intervals for each value in the SSA representation
     - IDs are rebuilt before calling this so that op ID can be used as an
   inreasing inex
     - indexes can be non-sequential but must be increasing
*/
struct interval_data construct_intervals(struct ir_func *irb) {
  // first rebuild ids so they are sequential and increasing
  rebuild_ids(irb);

  unsigned *bb_ranges = find_basicblock_ranges(irb);

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
        struct interval *interval = &data.intervals[op->id];

        if (op->ty == IR_OP_TY_MOV && op->mov.value == NULL) {
          op->reg =
              (struct ir_reg){.ty = IR_REG_TY_INTEGRAL, .idx = arg_regs++};
        } else {
          // // reset registers unless flags because flags is never allocated
          // if (op->reg != REG_FLAGS && !(op->flags &
          // IR_OP_FLAG_DONT_GIVE_SLOT)) {
          //   op->reg = NO_REG;
          // }
        }

        debug_assert(op->id < irb->op_count,
                     "out of range! (id %zu with opcount %zu)", op->id,
                     irb->op_count);

        interval->op = op;
        interval->start = op->id;

        // we can get intervals with an end before their start if the value is
        // unused fix them up to be valid
        if (interval->end < interval->start) {
          interval->end = interval->start;
        }

        debug_assert(op->metadata == NULL,
                     "metadata left over in op during liveness analysis, will "
                     "be overwritten");
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

  // now we use each phi to set it (and its dependent intervals) to the min/max
  // of the dependents
  basicblock = irb->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        if (op->ty == IR_OP_TY_PHI) {
          struct interval *interval = &data.intervals[op->id];
          size_t start = interval->start;
          size_t end = interval->end;

          for (size_t i = 0; i < op->phi.num_values; i++) {
            struct ir_op *dependent = op->phi.values[i];
            struct interval *dependent_interval =
                &data.intervals[dependent->id];

            size_t path_id = op->stmt->basicblock->id * irb->basicblock_count +
                             dependent->stmt->basicblock->id;

            debug_assert(bb_ranges[path_id], "bb_len was 0");
            size_t dependent_path_end = bb_ranges[path_id];

            start = MIN(start, dependent_interval->start);
            end = MAX(end, dependent_path_end);
          }

          interval->start = start;
          interval->end = end;

          for (size_t i = 0; i < op->phi.num_values; i++) {
            struct ir_op *dependent = op->phi.values[i];
            struct interval *dependent_interval =
                &data.intervals[dependent->id];

            dependent_interval->start =
                MIN(dependent_interval->start, interval->start);
            dependent_interval->end =
                MAX(dependent_interval->end, interval->end);
          }
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  return data;
}

void print_live_regs(FILE *file, const struct ir_reg_usage *reg_usage) {
  fslogsl(file, " - LIVE REGS (");

  struct bitset_iter gp_iter =
      bitset_iter(reg_usage->gp_registers_used, 0, true);
  struct bitset_iter fp_iter =
      bitset_iter(reg_usage->fp_registers_used, 0, true);

  size_t i;
  bool first = true;
  while (bitset_iter_next(&gp_iter, &i)) {
    if (first) {
      first = false;
      fslogsl(file, ", ");
    }

    fslogsl(file, "R%zu", i);
  }

  if (bitset_any(reg_usage->gp_registers_used, true) &&
      bitset_any(reg_usage->fp_registers_used, true)) {
    fslogsl(file, ", ");
  }

  first = true;
  while (bitset_iter_next(&fp_iter, &i)) {
    if (first) {
      first = false;
      fslogsl(file, ", ");
    }

    fslogsl(file, "F%zu", i);
  }
  fslogsl(file, ")");
}

void print_ir_intervals(FILE *file, struct ir_op *op,
                        UNUSED_ARG(void *metadata)) {
  struct interval *interval = op->metadata;
  if (interval) {
    invariant_assert(interval->op->id == op->id, "intervals are not ID keyed");
    fslogsl(file, "start=%05zu, end=%05zu | ", interval->start, interval->end);
  } else {
    fslogsl(file, "no associated interval | ");
  }

  switch (op->reg.ty) {
  case IR_REG_TY_NONE:
    fslogsl(file, "    (UNASSIGNED)");
    break;
  case IR_REG_TY_SPILLED:
    if (op->lcl) {
      fslogsl(file, "    (SPILLED), LCL=%zu", op->lcl->id);
    } else {
      fslogsl(file, "    (SPILLED), LCL=(UNASSIGNED)");
    }
    break;
  case IR_REG_TY_FLAGS:
    fslogsl(file, "    (FLAGS)");
    break;
  case IR_REG_TY_INTEGRAL:
    if (op->flags & IR_OP_FLAG_DONT_GIVE_REG) {
      fslogsl(file, "    (DONT)");
    } else {
      fslogsl(file, "    register=R%zu", op->reg.idx);
    }
    break;
  case IR_REG_TY_FP:
    if (op->flags & IR_OP_FLAG_DONT_GIVE_REG) {
      fslogsl(file, "    (DONT)");
    } else {
      fslogsl(file, "    register=F%zu", op->reg.idx);
    }
    break;
  }

  // if (interval && interval->op) {
  //   print_live_regs(file, &interval->op->reg_usage);
  // }
}
