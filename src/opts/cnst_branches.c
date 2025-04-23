#include "cnst_branches.h"

#include "../hashtbl.h"
#include "../vector.h"
#include "opts.h"

struct phi_entry {
  struct ir_op *phi;
};

struct phi_info {
  struct hashtbl *bb_to_entry;
  struct ir_dominance_frontier df;
};

static struct ir_op *opts_follow_movs(struct ir_op *op) {
  while (op->ty == IR_OP_TY_MOV) {
    if (!op->mov.value) {
      return NULL;
    }

    op = op->mov.value;
  }

  return op;
}

static void opts_cnst_branches_func_end(UNUSED struct ir_func *func,
                                        void *data) {
  struct phi_info *info = data;

  struct hashtbl_iter *iter = hashtbl_iter(info->bb_to_entry);
  struct hashtbl_entry entry;
  while (hashtbl_iter_next(iter, &entry)) {
    struct vector **vector = entry.data;
    vector_free(vector);
  }

  // TODO: reuse hashtbl (have clear method)
  hashtbl_free(&info->bb_to_entry);
}

static void opts_cnst_branches_func_begin(struct ir_func *func, void *data) {
  struct phi_info *info = data;

  info->df = ir_compute_dominance_frontier(func);
  info->bb_to_entry = hashtbl_create(sizeof(struct ir_basicblock *),
                                     sizeof(struct vector *), NULL, NULL);

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);

  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    if (op->ty != IR_OP_TY_PHI) {
      continue;
    }

    for (size_t i = 0; i < op->phi.num_values; i++) {
      struct phi_entry e = {
          .phi = op,
      };

      struct vector *entries;
      struct vector **p =
          hashtbl_lookup(info->bb_to_entry, &op->phi.values[i].basicblock);
      if (p) {
        entries = *p;
      } else {
        entries = vector_create(sizeof(struct phi_entry));
        hashtbl_insert(info->bb_to_entry, &op->phi.values[i].basicblock,
                       &entries);
      }

      vector_push_back(entries, &e);
    }
  }
}

static void remove_dead_phi_entries(struct ir_basicblock *basicblock,
                                    struct phi_info *info) {
  struct vector **p = hashtbl_lookup(info->bb_to_entry, &basicblock);

  if (!p) {
    return;
  }

  struct vector *entries = *p;
  size_t num_entries = vector_length(entries);

  for (size_t i = 0; i < num_entries; i++) {
    struct phi_entry *phi_entry = vector_get(entries, i);
    struct ir_op *op = phi_entry->phi;

    for (size_t j = 0; j < op->phi.num_values;) {
      struct ir_phi_entry *entry = &op->phi.values[j];

      bool remove = false;
      if (entry->basicblock->id == DETACHED_BASICBLOCK ||
          !ir_basicblock_is_pred(op->stmt->basicblock, entry->basicblock)) {
        // must be dominated by another, so removing it is fine?
        remove = true;
      }

      if (remove) {
        size_t rem = op->phi.num_values - j - 1;
        memmove(&op->phi.values[j], &op->phi.values[j + 1],
                rem * sizeof(op->phi.values[j]));

        op->phi.num_values--;
      } else {
        j++;
      }
    }
  }
}

static bool opts_cnst_branches_op(struct ir_func *func, struct ir_op *op,
                                  void *data) {
  struct phi_info *info = data;

  switch (op->ty) {
  case IR_OP_TY_BR_COND: {
    struct ir_op_br_cond br_cond = op->br_cond;

    struct ir_op *cnst = opts_follow_movs(br_cond.cond);

    if (!cnst ||
        (cnst->ty != IR_OP_TY_CNST || !ir_var_ty_is_integral(&cnst->var_ty))) {
      return false;
    }

    // BUG: fails do_while.c
    // this can leave phis in BBs with only one pred. they need to be moved to
    // prior BBs

    if (cnst->cnst.int_value) {
      ir_make_basicblock_merge(func, op->stmt->basicblock,
                               op->stmt->basicblock->split.true_target);
      remove_dead_phi_entries(op->stmt->basicblock->split.false_target, info);
      remove_dead_phi_entries(op->stmt->basicblock->split.true_target, info);
    } else {
      ir_make_basicblock_merge(func, op->stmt->basicblock,
                               op->stmt->basicblock->split.false_target);
      remove_dead_phi_entries(op->stmt->basicblock->split.true_target, info);
      remove_dead_phi_entries(op->stmt->basicblock->split.false_target, info);
    }

    op->ty = IR_OP_TY_BR;

    return true;
  }
  default:
    return false;
  }
}

void opts_cnst_branches(struct ir_unit *unit) {
  struct phi_info data = {0};

  struct opts_op_pass pass = {.name = __func__,
                              .data = &data,
                              .begin_func_callback =
                                  opts_cnst_branches_func_begin,
                              .end_func_callback = opts_cnst_branches_func_end,
                              .op_callback = opts_cnst_branches_op};

  opts_run_op_pass(unit, &pass);
}
