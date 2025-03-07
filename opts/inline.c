#include "inline.h"

#include "../hashtbl.h"
#include "../ir/prettyprint.h"
#include "../log.h"
#include "../vector.h"
#include "opts.h"

enum opts_inline_stage {
  // this is about the CALLER not CALLEE
  // i.e if `foo() { return bar(); }`, stage INLINING means `foo` is currently undergoing inlining and may inline `bar`

  OPTS_INLINE_STAGE_INLINING,
  OPTS_INLINE_STAGE_INLINED,
};

struct opts_inline_info {
  // `struct ir_func * : enum opts_inline_stage`
  struct hashtbl *inlined;
};

static bool opts_can_inline(struct ir_func *func, struct ir_func *candidate, struct opts_inline_info *info) {
  if (func == candidate) {
    debug("cannot inline %s into %s due to recursion", candidate->name, func->name);
    return false;
  }

  enum opts_inline_stage *stage = hashtbl_lookup(info->inlined, candidate);

  if (stage && stage == OPTS_INLINE_STAGE_INLINING) {
    // cannot inline; mutually recursive
    debug("cannot inline %s into %s due to [mutual] recursion", candidate->name, func->name);
    return false;
  }

  // check for no inline attr etc
  return true;
}

static bool opts_should_inline(struct ir_func *func,
                               struct ir_func *candidate) {
  return func->op_count < 2048 && candidate->op_count < 128;
}

static struct ir_lcl *ir_clone_lcl(struct ir_func *func,
                                   const struct ir_lcl *lcl,
                                   struct hashtbl *cloned) {
  struct ir_lcl **lookup = hashtbl_lookup(cloned, &lcl);
  if (lookup) {
    return *lookup;
  }

  // doesn't clone metadata field

  struct ir_lcl *copy = ir_add_local(func, &lcl->var_ty);

  hashtbl_insert(cloned, &lcl, &copy);

  copy->func = func;
  copy->var_ty = lcl->var_ty;
  copy->flags = lcl->flags & ~IR_LCL_FLAG_PARAM;

  switch (lcl->alloc_ty) {
  case IR_LCL_ALLOC_TY_NONE:
    break;
  case IR_LCL_ALLOC_TY_NORMAL:
  case IR_LCL_ALLOC_TY_FIXED:
    copy->alloc = lcl->alloc;
    break;
  }

  return copy;
}

struct clone_uses_data {
  struct ir_func *func;
  struct hashtbl *cloned;
};

static struct ir_basicblock *
ir_clone_basicblock(struct ir_func *func,
                    const struct ir_basicblock *basicblock,
                    struct hashtbl *cloned);

static struct ir_stmt *ir_clone_stmt(struct ir_func *func, struct ir_stmt *stmt,
                                     struct hashtbl *cloned) {

  struct ir_stmt **lookup = hashtbl_lookup(cloned, &stmt);
  if (lookup) {
    return *lookup;
  }

  // doesn't copy metadata field

  struct ir_basicblock *cloned_bb =
      ir_clone_basicblock(func, stmt->basicblock, cloned);
  struct ir_stmt *copy = ir_alloc_stmt(func, cloned_bb);

  hashtbl_insert(cloned, &stmt, &copy);

  copy->flags = stmt->flags;
  return copy;
}

static struct ir_op *ir_clone_op(struct ir_func *func, const struct ir_op *op,
                                 struct hashtbl *cloned);

static void clone_uses(struct ir_op **op, UNUSED enum ir_op_use_ty ty,
                       void *cb_metadata) {
  struct clone_uses_data *data = cb_metadata;

  *op = ir_clone_op(data->func, *op, data->cloned);
}

static struct ir_op *ir_clone_op(struct ir_func *func, const struct ir_op *op,
                                 struct hashtbl *cloned) {
  struct ir_op **lookup = hashtbl_lookup(cloned, &op);
  if (lookup) {
    return *lookup;
  }

  // doesn't copy metadata field

  struct ir_stmt *cloned_stmt = ir_clone_stmt(func, op->stmt, cloned);
  struct ir_op *copy = ir_alloc_op(func, cloned_stmt);

  hashtbl_insert(cloned, &op, &copy);

  // well this is ugly
  copy->ty = op->ty;
#define CLONE_OP(hi, lo)                                                       \
  case IR_OP_TY_##hi:                                                          \
    copy->lo = op->lo;                                                         \
    break;

  switch (op->ty) {
    CLONE_OP(MOV, mov);
    CLONE_OP(CNST, cnst);
    CLONE_OP(BINARY_OP, binary_op);
    CLONE_OP(UNARY_OP, unary_op);
    CLONE_OP(CAST_OP, cast_op);
    CLONE_OP(BITFIELD_EXTRACT, bitfield_extract);
    CLONE_OP(BITFIELD_INSERT, bitfield_insert);
    CLONE_OP(MEM_SET, mem_set);
    CLONE_OP(MEM_COPY, mem_copy);
    CLONE_OP(ADDR_OFFSET, addr_offset);
    CLONE_OP(BR_COND, br_cond);
    CLONE_OP(BR_SWITCH, br_switch);
    CLONE_OP(RET, ret);
  case IR_OP_TY_CALL:
    copy->call = op->call;
    copy->call.num_args = op->call.num_args;
    copy->call.args = arena_alloc_init(
        func->arena, op->call.num_args * sizeof(struct ir_op *), op->call.args);
    break;
  case IR_OP_TY_PHI:
    copy->phi = (struct ir_op_phi){
        .num_values = op->phi.num_values,
        .values = arena_alloc_init(
            func->arena, op->phi.num_values * sizeof(op->phi.values[0]),
            op->phi.values)};

    // fix up the bb links
    for (size_t i = 0; i < copy->phi.num_values; i++) {
      copy->phi.values[i].basicblock =
          ir_clone_basicblock(func, copy->phi.values[i].basicblock, cloned);
    }
    break;
  case IR_OP_TY_GATHER:
    copy->gather = (struct ir_op_gather){
        .num_values = op->gather.num_values,
        .values = arena_alloc_init(
            func->arena, op->gather.num_values * sizeof(op->gather.values[0]),
            op->gather.values)};
    break;

  // may have lcl/glb references that must be cloned
  case IR_OP_TY_ADDR:
    copy->addr.ty = op->addr.ty;
    switch (op->addr.ty) {
    case IR_OP_ADDR_TY_LCL:
      copy->addr.lcl = ir_clone_lcl(func, op->addr.lcl, cloned);
      break;
    case IR_OP_ADDR_TY_GLB:
      copy->addr.glb = op->addr.glb;
      break;
    }
    break;
  case IR_OP_TY_STORE:
    copy->store.ty = op->store.ty;
    copy->store.value = op->store.value;
    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
      copy->store.lcl = ir_clone_lcl(func, op->store.lcl, cloned);
      break;
    case IR_OP_STORE_TY_GLB:
      copy->store.glb = op->store.glb;
      break;
    case IR_OP_STORE_TY_ADDR:
      copy->store.addr = op->store.addr;
      break;
    }
    break;
  case IR_OP_TY_LOAD:
    copy->load.ty = op->load.ty;
    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
      copy->load.lcl = ir_clone_lcl(func, op->load.lcl, cloned);
      break;
    case IR_OP_LOAD_TY_GLB:
      copy->load.glb = op->load.glb;
      break;
    case IR_OP_LOAD_TY_ADDR:
      copy->load.addr = op->load.addr;
      break;
    }
    break;
  case IR_OP_TY_STORE_BITFIELD:
    copy->store_bitfield.ty = op->store_bitfield.ty;
    copy->store_bitfield.value = op->store_bitfield.value;
    switch (op->store_bitfield.ty) {
    case IR_OP_STORE_TY_LCL:
      copy->store_bitfield.lcl =
          ir_clone_lcl(func, op->store_bitfield.lcl, cloned);
      break;
    case IR_OP_STORE_TY_GLB:
      copy->store_bitfield.glb = op->store_bitfield.glb;
      break;
    case IR_OP_STORE_TY_ADDR:
      copy->store_bitfield.addr = op->store_bitfield.addr;
      break;
    }
    break;
  case IR_OP_TY_LOAD_BITFIELD:
    copy->load_bitfield.ty = op->load_bitfield.ty;
    switch (op->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
      copy->load_bitfield.lcl =
          ir_clone_lcl(func, op->load_bitfield.lcl, cloned);
      break;
    case IR_OP_LOAD_TY_GLB:
      copy->load_bitfield.glb = op->load_bitfield.glb;
      break;
    case IR_OP_LOAD_TY_ADDR:
      copy->load_bitfield.addr = op->load_bitfield.addr;
      break;
    }
    break;
  default:
    break;
  }

  // we do NOT clone op uses here
  // just the ops themselves
  // we do the use-cloning after to preserve build order

#undef CLONE_OP

  if (op->lcl) {
    copy->lcl = ir_clone_lcl(func, op->lcl, cloned);
  }

  copy->reg = op->reg;
  copy->write_info = op->write_info;
  copy->flags = op->flags;
  copy->var_ty = op->var_ty;

  return copy;
}

static struct ir_basicblock *
ir_clone_basicblock(struct ir_func *func,
                    const struct ir_basicblock *basicblock,
                    struct hashtbl *cloned) {
  struct ir_basicblock **lookup = hashtbl_lookup(cloned, &basicblock);
  if (lookup) {
    return *lookup;
  }

  // doesn't copy metadata field

  struct ir_basicblock *copy = arena_alloc(func->arena, sizeof(*copy));

  ir_initialise_basicblock(copy, func->next_basicblock_id++);

  hashtbl_insert(cloned, &basicblock, &copy);

  if (basicblock->pred) {
    copy->pred = ir_clone_basicblock(func, basicblock->pred, cloned);
  }

  struct ir_stmt *stmt = basicblock->first;

  while (stmt) {
    ir_clone_stmt(func, stmt, cloned);

    stmt = stmt->succ;
  }

  if (basicblock->succ) {
    copy->succ = ir_clone_basicblock(func, basicblock->succ, cloned);
  }

  copy->num_preds = basicblock->num_preds;
  copy->preds = arena_alloc(func->arena, sizeof(struct ir_basicblock *));
  for (size_t i = 0; i < copy->num_preds; i++) {
    copy->preds[i] = ir_clone_basicblock(func, basicblock->preds[i], cloned);
  }

  copy->ty = basicblock->ty;
  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    copy->split = (struct ir_basicblock_split){
        .true_target =
            ir_clone_basicblock(func, basicblock->split.true_target, cloned),
        .false_target =
            ir_clone_basicblock(func, basicblock->split.false_target, cloned),
    };
    break;
  case IR_BASICBLOCK_TY_MERGE:
    copy->merge = (struct ir_basicblock_merge){
        .target = ir_clone_basicblock(func, basicblock->merge.target, cloned),
    };
    break;
  case IR_BASICBLOCK_TY_SWITCH:
    copy->switch_case = (struct ir_basicblock_switch){
        .num_cases = basicblock->switch_case.num_cases,
        .cases =
            arena_alloc(func->arena, sizeof(*copy->switch_case.cases) *
                                         basicblock->switch_case.num_cases),
        .default_target = ir_clone_basicblock(
            func, basicblock->switch_case.default_target, cloned),
    };

    for (size_t i = 0; i < basicblock->switch_case.num_cases; i++) {
      copy->switch_case.cases[i] = (struct ir_split_case){
          .value = basicblock->switch_case.cases[i].value,
          .target = ir_clone_basicblock(
              func, basicblock->switch_case.cases[i].target, cloned),
      };
    }
    break;
  }

  return copy;
}

static void opts_inline_func(struct ir_func *func, void *data);

static bool opts_inline_op(struct ir_func *func, struct ir_op *call,
                           void *data) {
  struct opts_inline_info *info = data;

  DEBUG_ASSERT(call->ty == IR_OP_TY_CALL, "op should have been call");

  struct ir_op *target = call->call.target;
  if (target->ty != IR_OP_TY_ADDR || target->addr.ty != IR_OP_ADDR_TY_GLB) {
    return false;
  }

  struct ir_glb *glb = target->addr.glb;

  if (glb->ty != IR_GLB_TY_FUNC || glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
    return false;
  }

  struct ir_func *candidate = glb->func;

  if (!opts_can_inline(func, candidate, info)) {
    return false;
  }

  if (!opts_should_inline(func, candidate)) {
    return false;
  }

  // first, make sure candidate is inlined
  opts_inline_func(candidate, info);

  debug("inlining %s into %s at callsite op %zu", candidate->name, func->name,
        call->id);

  ir_rebuild_ids(func);
  ir_rebuild_ids(candidate);

  // we effectively deep clone the IR into our IR

  struct hashtbl *cloned =
      hashtbl_create(sizeof(void *), sizeof(void *), NULL, NULL);

  struct ir_basicblock *basicblock = candidate->first;

  // first clone bbs
  struct ir_basicblock *copy = ir_clone_basicblock(func, basicblock, cloned);

  struct ir_basicblock *end = copy;
  while (end->succ) {
    end = end->succ;
  }

  struct ir_basicblock *end_bb =
      ir_insert_basicblocks_after_op(func, call, copy);
  struct ir_basicblock *orig_pred = copy->pred;

  // then clone ops (because they may reference things in weird directions and
  // this means all stmts/ops are dealt with)

  struct vector *returns =
      vector_create_in_arena(sizeof(struct ir_phi_entry), func->arena);

  // do a first pass
  struct ir_func_iter iter = ir_func_iter(candidate, IR_FUNC_ITER_FLAG_NONE);
  struct ir_op *to_clone;
  while (ir_func_iter_next(&iter, &to_clone)) {
    ir_clone_op(func, to_clone, cloned);
  }

  iter = ir_func_iter(candidate, IR_FUNC_ITER_FLAG_NONE);
  while (ir_func_iter_next(&iter, &to_clone)) {
    // just pulls from cache, but not efficient, should walk the new ops
    struct ir_op *op_copy = ir_clone_op(func, to_clone, cloned);

    struct clone_uses_data clone_data = {.func = func, .cloned = cloned};
    ir_walk_op_uses(op_copy, clone_uses, &clone_data);

    if (op_copy->ty == IR_OP_TY_RET) {
      ir_make_basicblock_merge(func, op_copy->stmt->basicblock, end_bb);

      struct ir_stmt *br_stmt = ir_alloc_stmt(func, op_copy->stmt->basicblock);
      struct ir_op *br_op = ir_alloc_op(func, br_stmt);
      br_op->ty = IR_OP_TY_BR;
      br_op->var_ty = IR_VAR_TY_NONE;

      if (op_copy->ret.value) {
        struct ir_op *value = op_copy->ret.value;
        struct ir_phi_entry entry = {.value = value,
                                     .basicblock = op_copy->stmt->basicblock};
        vector_push_back(returns, &entry);
        ir_detach_op(func, op_copy);
      } else {
        ir_detach_op(func, op_copy);
      }
    }
  }

  ir_rebuild_ids(func);

  struct ir_stmt *params = copy->first;
  if (params->flags & IR_STMT_FLAG_PARAM) {
    struct ir_op *param_op = params->first;
    for (size_t i = 0; i < call->call.num_args; i++) {
      switch (param_op->ty) {
      case IR_OP_TY_MOV:
        param_op->ty = IR_OP_TY_MOV;
        param_op->mov = (struct ir_op_mov){.value = call->call.args[i]};
        param_op->flags &= ~IR_OP_FLAG_PARAM;
        break;
      case IR_OP_TY_ADDR: {
        DEBUG_ASSERT(param_op->addr.ty == IR_OP_ADDR_TY_LCL,
                     "expected param addr op to be an addr lcl");

        param_op =
            ir_replace_op(func, param_op, IR_OP_TY_STORE, IR_VAR_TY_NONE);
        param_op->store = (struct ir_op_store){.ty = IR_OP_STORE_TY_LCL,
                                               .lcl = param_op->addr.lcl,
                                               .value = call->call.args[i]};
        break;
      }
      default:
        BUG("bad ty for param op");
      }

      param_op->flags &= ~IR_OP_FLAG_PARAM;
      param_op = param_op->succ;
    }
  }

  ir_detach_op(func, call);

  struct ir_stmt *end_stmt;
  if (end_bb->first) {
    end_stmt = end_bb->first;
  } else {
    end_stmt = ir_alloc_stmt(func, end_bb);
  }

  ir_attach_op(func, call, end_stmt, NULL, end_stmt->first);

  size_t num_rets = vector_length(returns);
  if (num_rets == 1) {
    call->ty = IR_OP_TY_MOV;
    call->mov = (struct ir_op_mov){
        .value = ((struct ir_phi_entry *)vector_head(returns))->value};
  } else {
    struct ir_op *phi = ir_insert_phi(func, call->stmt->basicblock, call->var_ty);
    phi->ty = IR_OP_TY_PHI;
    phi->phi = (struct ir_op_phi){.num_values = num_rets,
                                   .values = vector_head(returns)};

    // HACK: we can't remove the op because its used for iteration
    // so make it a pointless mov (which will get removed in next elim run)

    call->ty = IR_OP_TY_MOV;
    call->mov = (struct ir_op_mov){
        .value = phi
    };
  }

  // final stage
  // we may have originally had:
  //   BB 00:
  //     %5 = ...
  //   <- INLINING OCCURRED HERE ->
  //   BB 01:
  //     %6 = phi [ %5 (@0) ]
  //
  // so we need to fix up successors of the original bb

  struct ir_basicblock_succ_iter succ_iter = ir_basicblock_succ_iter(end_bb);
  struct ir_basicblock *succ;
  while (ir_basicblock_succ_iter_next(&succ_iter, &succ)) {
    struct ir_stmt *phi_stmt = succ->first;
    if (phi_stmt && phi_stmt->flags & IR_STMT_FLAG_PHI) {
      struct ir_op *phi = phi_stmt->first;

      while (phi) {
        for (size_t i = 0; i < phi->phi.num_values; i++) {
          struct ir_phi_entry *entry = &phi->phi.values[i];

          // FIXME: can we break at first find? for safety i am not right now,
          // but i think it is safe to assume (because `ir_simplify_phi`) has
          // been run
          if (entry->basicblock == orig_pred) {
            entry->basicblock = end_bb;
          }
        }

        phi = phi->succ;
      }
    }
  }

  ir_rebuild_ids(func);

  return true;
}


static void opts_inline_func(struct ir_func *func, void *data) {
  struct opts_inline_info *info = data;

  enum opts_inline_stage inlining = OPTS_INLINE_STAGE_INLINING;
  hashtbl_lookup_or_insert(info->inlined, &func, &inlining);

  struct ir_func_iter iter = ir_func_iter(func, IR_FUNC_ITER_FLAG_NONE);
  struct ir_op *op;
  while (ir_func_iter_next(&iter, &op)) {
    if (op->ty != IR_OP_TY_CALL) {
      continue;
    }

    opts_inline_op(func, op, data);
  }

}

void opts_inline(struct ir_unit *unit) {
  struct opts_inline_info inline_info = {
      .inlined = hashtbl_create(sizeof(struct ir_func *),
                                sizeof(enum opts_inline_stage), NULL, NULL)};

  struct opts_func_pass pass = {.name = __func__,
                           .data = &inline_info,
                           .func_callback = opts_inline_func};

  opts_run_func_pass(unit, &pass);
}
