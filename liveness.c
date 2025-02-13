#include "liveness.h"

#include "alloc.h"
#include "bitset.h"
#include "ir/ir.h"
#include "log.h"
#include "util.h"

static void op_used_callback(struct ir_op **op, void *cb_metadata) {
  struct interval_callback_data *cb = cb_metadata;

  if (cb->op->ty == IR_OP_TY_PHI) {
    return;
  }

  struct interval *interval = &cb->data->intervals[(*op)->id];

  size_t op_end = ((cb->op->flags | (*op)->flags) & IR_OP_FLAG_READS_DEST) ? cb->op->id + 1 : cb->op->id;
  interval->end = MAX(interval->end, op_end);
}

/* Builds the intervals for each value in the SSA representation
     - IDs are rebuilt before calling this so that op ID can be used as an
   inreasing inex
     - indexes can be non-sequential but must be increasing
*/
struct interval_data construct_intervals(struct ir_func *irb) {
  // first rebuild ids so they are sequential and increasing
  rebuild_ids(irb);

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

        DEBUG_ASSERT(op->id < irb->op_count,
                     "out of range! (id %zu with opcount %zu)", op->id,
                     irb->op_count);

        interval->op = op;
        interval->start = op->id;

        // we can get intervals with an end before their start if the value is
        // unused fix them up to be valid
        if (interval->end < interval->start) {
          interval->end = interval->start;
        }

        DEBUG_ASSERT(op->metadata == NULL,
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

          for (size_t i = 0; i < op->phi.num_values; i++) {
            struct ir_op *dependent = op->phi.values[i].value;
            struct interval *dependent_interval =
                &data.intervals[dependent->id];

            // force dependent to live until end of the bb
            // dependent_interval->end =
            //     op->phi.values[i].basicblock->last->last->id;
            dependent_interval->end = MAX(op->id, op->phi.values[i].basicblock->last->last->id);
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
    fslogsl(file, "    register=R%zu", op->reg.idx);
    break;
  case IR_REG_TY_FP:
    fslogsl(file, "    register=F%zu", op->reg.idx);
    break;
  }

  // if (interval && interval->op) {
  //   print_live_regs(file, &interval->op->reg_usage);
  // }
}


int sort_interval_by_start_point(const void *a, const void *b) {
  const struct interval *a_int = (const struct interval *)a;
  const struct interval *b_int = (const struct interval *)b;
  size_t a_start = a_int->start;
  size_t b_start = b_int->start;

  // put params at front, this is used for giving them the correct registers for
  // calling conv
  enum ir_op_flags a_flags = a_int->op->flags;
  enum ir_op_flags b_flags = b_int->op->flags;
  if ((a_flags & IR_OP_FLAG_PARAM) > (b_flags & IR_OP_FLAG_PARAM)) {
    return -1;
  } else if ((a_flags & IR_OP_FLAG_PARAM) < (b_flags & IR_OP_FLAG_PARAM)) {
    return 1;
  }

  if (a_start > b_start) {
    return 1;
  } else if (a_start < b_start) {
    return -1;
  }

  if (a_int->op->id > b_int->op->id) {
    return 1;
  } else if (a_int->op->id < b_int->op->id) {
    return -1;
  }

  return 0;
}
