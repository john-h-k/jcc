#include "liveness.h"

#include "alloc.h"
#include "bitset.h"
#include "ir/ir.h"
#include "ir/prettyprint.h"
#include "log.h"
#include "util.h"

#ifndef NDEBUG
struct validate_intervals_metadata {
  struct ir_func *func;
  struct ir_op *consumer;
  struct interval_data intervals;
};

static void validate_intervals_cb(struct ir_op **op,
                                  UNUSED enum ir_op_use_ty ty, void *metadata) {
  struct validate_intervals_metadata *data = metadata;

  struct interval *interval = &data->intervals.intervals[(*op)->id];

  if (data->consumer->ty == IR_OP_TY_PHI) {
    struct ir_op_phi phi = data->consumer->phi;

    for (size_t i = 0; i < phi.num_values; i++) {
      struct ir_phi_entry entry = phi.values[i];

      if (entry.value == *op) {
        if (interval->start > entry.basicblock->last->last->id ||
            interval->end < entry.basicblock->last->last->id) {
    debug_print_ir_func(stderr, data->func, print_ir_intervals,
                        data->intervals.intervals);
          BUG("op %zu is a phi with interval (%zu, %zu) but used by op %zu "
              "(expected it to live to end of pred block, op %zu)",
              data->consumer->id, interval->start, interval->end, (*op)->id,
              entry.basicblock->last->last->id);
        }

        break;
      }
    }

  } else if (interval->start > data->consumer->id ||
             interval->end < data->consumer->id) {

    debug_print_ir_func(stderr, data->func, print_ir_intervals,
                        data->intervals.intervals);
    BUG("op %zu interval (%zu, %zu) but used by op %zu", (*op)->id,
        interval->start, interval->end, data->consumer->id);
  }
}

static void validate_intervals(struct ir_func *func,
                               struct interval_data data) {
  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    struct validate_intervals_metadata metadata = {
        .func = func, .consumer = op, .intervals = data};

    ir_walk_op_uses(op, validate_intervals_cb, &metadata);
  }
}
#endif


static void op_used_callback(struct ir_op **op, UNUSED enum ir_op_use_ty use_ty,
                             void *cb_metadata) {
  struct interval_callback_data *cb = cb_metadata;

  // if (cb->op->ty == IR_OP_TY_PHI) {
  //   return;
  // }

  struct interval *interval = &cb->data->intervals[(*op)->id];

  size_t op_end = ((cb->op->flags | (*op)->flags) & IR_OP_FLAG_READS_DEST)
                      ? cb->op->id + 1
                      : cb->op->id;

  interval->end = MAX(interval->end, op_end);

  struct ir_op *consumer = cb->op;

  if ((*op)->stmt->basicblock != cb->op->stmt->basicblock) {
    struct ir_basicblock *basicblock = consumer->stmt->basicblock;

    if (basicblock->last && basicblock->last->last) {
      interval->end = MAX(interval->end, basicblock->last->last->id);
    }
    
    for (size_t i = 0; i < basicblock->num_preds; i++) {
      struct ir_basicblock *pred = basicblock->preds[i];
      if (pred->last && pred->last->last) {
        interval->end = MAX(interval->end, pred->last->last->id);
      }
    }
  }
}


/* Builds the intervals for each value in the SSA representation
     - IDs are rebuilt before calling this so that op ID can be used as an
   inreasing inex
     - indexes can be non-sequential but must be increasing
*/
struct interval_data construct_intervals(struct ir_func *irb) {
  // first rebuild ids so they are sequential and increasing
  ir_rebuild_ids(irb);

  struct interval_data data;
  data.intervals =
      arena_alloc(irb->arena, sizeof(*data.intervals) * irb->op_count);
  data.num_intervals = 0;

  memset(data.intervals, 0, sizeof(*data.intervals) * irb->op_count);

  struct ir_basicblock *basicblock = irb->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;
    while (stmt) {
      struct ir_op *op = stmt->first;
      while (op) {
        struct interval *interval = &data.intervals[op->id];

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

        // i originally had this, but i don't think its needed? the range is
        // still long enough its just that it will unnecessarily fill up a reg
        // spot?

        // removing causes `./tests/struct_arg.c` to fail
        if (op->flags & IR_OP_FLAG_ETERNAL) {
          interval->end = irb->op_count;
        }

        DEBUG_ASSERT(op->metadata == NULL,
                     "metadata left over in op during liveness analysis, will "
                     "be overwritten");
        op->metadata = interval;

        struct interval_callback_data cb_data = {.op = op, .data = &data};

        ir_walk_op_uses(op, op_used_callback, &cb_data);
        data.num_intervals++;

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  // NOTE: liveness assume that if a backward jump happens, any ops needed in that block are immediately used in a phi

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

            // FIXME: awkward scenario. Some tests pass using the first line
            // only, some only with the second line it also varies across arm64
            // vs x64...

            // dependent_interval->end =
            //     op->phi.values[i].basicblock->last->last->id;
            dependent_interval->end =
                MAX(dependent_interval->end,
                    MAX(op->id, op->phi.values[i].basicblock->last->last->id));
          }
        }

        op = op->succ;
      }

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

#ifndef NDEBUG
  validate_intervals(irb, data);
#endif

  return data;
}

void print_live_regs(FILE *file, const struct ir_reg_usage *reg_usage) {
  fslogsl(file, " - LIVE REGS (");

  for (size_t i = 0; i < reg_usage->num_nonvolatile_used; i++) {
    if (i + 1 != reg_usage->num_nonvolatile_used) {
      fslogsl(file, ", ");
    }

    debug_print_ir_reg(file, reg_usage->nonvolatile_used[i]);
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
