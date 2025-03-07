#include "inline.h"

#include "../hashtbl.h"
#include "../vector.h"
#include "../ir/prettyprint.h"
#include "opts.h"

static bool opts_can_inline(UNUSED struct ir_func *func,
                            UNUSED struct ir_func *candidate) {
  // check for no inline attr etc
  return true;
}

static bool opts_should_inline(struct ir_func *func,
                               struct ir_func *candidate) {
  return func->op_count < 2048 && candidate->op_count < 128;
}

static struct ir_lcl *ir_clone_lcl(struct ir_func *func, struct ir_lcl *lcl,
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
  copy->flags = lcl->flags;

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
ir_clone_basicblock(struct ir_func *func, struct ir_basicblock *basicblock,
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

static struct ir_op *ir_clone_op(struct ir_func *func, struct ir_op *op,
                                 struct hashtbl *cloned);

static void clone_uses(struct ir_op **op, UNUSED enum ir_op_use_ty ty,
                       void *cb_metadata) {
  struct clone_uses_data *data = cb_metadata;

  *op = ir_clone_op(data->func, *op, data->cloned);
}

static struct ir_op *ir_clone_op(struct ir_func *func, struct ir_op *op,
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
    CLONE_OP(PHI, phi);
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
    CLONE_OP(CALL, call);
    CLONE_OP(GATHER, gather);

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
    default:
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
    default:
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
    default:
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
    default:
      break;
    }
    break;
  default:
    break;
  }

  struct clone_uses_data data = {.func = func, .cloned = cloned};
  ir_walk_op_uses(copy, clone_uses, &data);

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
ir_clone_basicblock(struct ir_func *func, struct ir_basicblock *basicblock,
                    struct hashtbl *cloned) {
  struct ir_basicblock **lookup = hashtbl_lookup(cloned, &basicblock);
  if (lookup) {
    return *lookup;
  }

  // doesn't copy metadata field

  struct ir_basicblock *copy = arena_alloc(func->arena, sizeof(*copy));

  ir_initialise_basicblock(copy, func->next_basicblock_id++);

  hashtbl_insert(cloned, &basicblock, &copy);

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

  struct ir_stmt *stmt = basicblock->first;

  while (stmt) {
    ir_clone_stmt(func, stmt, cloned);

    stmt = stmt->succ;
  }

  return copy;
}

static bool opts_inline_op(struct ir_func *func, struct ir_op *call) {
  if (call->ty != IR_OP_TY_CALL) {
    return false;
  }

  struct ir_op *target = call->call.target;
  if (target->ty != IR_OP_TY_ADDR || target->addr.ty != IR_OP_ADDR_TY_GLB) {
    return false;
  }

  struct ir_glb *glb = target->addr.glb;

  if (glb->ty != IR_GLB_TY_FUNC || glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
    return false;
  }

  struct ir_func *candidate = glb->func;

  if (!opts_can_inline(func, candidate)) {
    return false;
  }

  if (!opts_should_inline(func, candidate)) {
    return false;
  }

  ir_rebuild_ids(func);
  ir_rebuild_ids(candidate);

  // we effectively deep clone the IR into our IR

  struct hashtbl *cloned =
      hashtbl_create(sizeof(void *), sizeof(void *), NULL, NULL);

  struct ir_basicblock *basicblock = candidate->first;

  // first clone bbs
  struct ir_basicblock *copy = ir_clone_basicblock(func, basicblock, cloned);
  struct ir_basicblock *end_bb =
      ir_insert_basicblocks_after_op(func, call, copy);

  struct ir_stmt *br_stmt = ir_alloc_stmt(func, copy);
  struct ir_op *br = ir_alloc_op(func, br_stmt);
  br->ty = IR_OP_TY_BR;
  br->var_ty = IR_VAR_TY_NONE;

  // then clone ops (because they may reference things in weird directions and
  // this means all stmts/ops are dealt with)
  struct ir_func_iter iter = ir_func_iter(candidate, IR_FUNC_ITER_FLAG_NONE);

  struct vector *returns = vector_create_in_arena(sizeof(struct ir_phi_entry), func->arena);

  struct ir_op *to_clone;
  while (ir_func_iter_next(&iter, &to_clone)) {
    struct ir_op *op_copy = ir_clone_op(func, to_clone, cloned);

    if (op_copy->ty == IR_OP_TY_RET) {
      ir_make_basicblock_merge(func, op_copy->stmt->basicblock, end_bb);

      if (op_copy->var_ty.ty != IR_VAR_TY_TY_NONE) {
        struct ir_phi_entry entry = {.value = op_copy,
                                     .basicblock = op_copy->stmt->basicblock};

        struct ir_op *value = op_copy->ret.value;
        op_copy->ty = IR_OP_TY_MOV;
        op_copy->mov = (struct ir_op_mov) { .value = value };
        vector_push_back(returns, &entry);
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
      param_op->ty = IR_OP_TY_MOV;
      param_op->mov = (struct ir_op_mov) { .value = call->call.args[i] };
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
    call->mov = (struct ir_op_mov) { .value = ((struct ir_phi_entry *)vector_head(returns))->value };
  } else {
    call->ty = IR_OP_TY_PHI;
    call->phi = (struct ir_op_phi){
      .num_values = num_rets,
      .values = vector_head(returns)
    };
  }

  ir_rebuild_ids(func);

  // TODO: impl
  return false;
}

void opts_inline(struct ir_unit *unit) {
  struct opts_pass pass = {.name = __func__, .op_callback = opts_inline_op};

  opts_run_pass(unit, &pass);
}
