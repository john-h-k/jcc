#include "validate.h"

#include "../util.h"
#include "ir.h"

struct validate_op_order_metadata {
  struct ir_op *consumer;
};

static void validate_op_order(struct ir_op **ir, void *metadata) {
  struct validate_op_order_metadata *data = metadata;
  struct ir_op *consumer = data->consumer;

  if (consumer->ty == IR_OP_TY_PHI || (consumer->flags & IR_OP_FLAG_PHI_MOV)) {
    // these can work across time
    return;
  }

  struct ir_op *op = *ir;

  if (op->id > consumer->id) {
    BUG("op %zu uses op %zu which is ahead of it", consumer->id, op->id);
  }
}

static void ir_validate_op(UNUSED struct ir_func *func, struct ir_op *op) {
  struct validate_op_order_metadata metadata = {.consumer = op};
  walk_op_uses(op, validate_op_order, &metadata);

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("should not have unknown ops");
  case IR_OP_TY_PHI:
    break;
  case IR_OP_TY_UNDF:
    break;
  case IR_OP_TY_MOV:
    break;
  case IR_OP_TY_CNST:
    break;
  case IR_OP_TY_BINARY_OP:
    break;
  case IR_OP_TY_UNARY_OP:
    break;
  case IR_OP_TY_CAST_OP:
    break;
  case IR_OP_TY_LOAD:
    invariant_assert(!op->lcl, "op %zu: loads should not have locals", op->id);
    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
      invariant_assert(op->load.lcl, "op %zu: load ty lcl must have lcl",
                       op->id);
      break;
    case IR_OP_LOAD_TY_GLB:
      invariant_assert(op->load.glb, "op %zu: load ty glb must have glb",
                       op->id);
      break;
    case IR_OP_LOAD_TY_ADDR:
      invariant_assert(op->load.addr, "op %zu: load ty addr must have addr",
                       op->id);
      break;
    }
    break;
  case IR_OP_TY_STORE:
    invariant_assert(op->var_ty.ty == IR_VAR_TY_TY_NONE,
                     "op %zu: store ops should not have a var ty", op->id);

    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
      invariant_assert(op->store.lcl, "op %zu: store ty lcl must have lcl",
                       op->id);
      break;
    case IR_OP_STORE_TY_GLB:
      invariant_assert(op->store.glb, "op %zu: store ty glb must have glb",
                       op->id);
      break;
    case IR_OP_STORE_TY_ADDR:
      invariant_assert(op->store.addr, "op %zu: store ty addr must have addr",
                       op->id);
      break;
    }
    invariant_assert(!op->lcl, "op %zu: stores should not have locals", op->id);
    break;
  case IR_OP_TY_STORE_BITFIELD:
    break;
  case IR_OP_TY_LOAD_BITFIELD:
    break;
  case IR_OP_TY_ADDR:
    break;
  case IR_OP_TY_BR:
    break;
  case IR_OP_TY_BR_COND:
    break;
  case IR_OP_TY_BR_SWITCH:
    break;
  case IR_OP_TY_RET:
    break;
  case IR_OP_TY_CALL:
    break;
  case IR_OP_TY_CUSTOM:
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    break;
  case IR_OP_TY_BITFIELD_INSERT:
    break;
  case IR_OP_TY_MEM_SET:
    break;
  case IR_OP_TY_ADDR_OFFSET:
    break;
  }
}

static void ir_validate_stmt(struct ir_func *func, struct ir_stmt *stmt) {
  struct ir_op *op = stmt->first;

  while (op) {
    ir_validate_op(func, op);

    op = op->succ;
  }
}

static void ir_validate_basicblock(struct ir_func *func,
                                   struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;

  while (stmt) {
    ir_validate_stmt(func, stmt);

    stmt = stmt->succ;
  }
}

static void ir_validate_data(struct ir_unit *iru, struct ir_glb *glb) {}

static void ir_validate_func(struct ir_unit *iru, struct ir_glb *glb) {
  switch (glb->def_ty) {

  case IR_GLB_DEF_TY_DEFINED:
    if (!glb->func) {
      BUG("defined global should have func");
    }
    break;
  case IR_GLB_DEF_TY_UNDEFINED:
    if (glb->func) {
      BUG("undefined global should not have func");
    }
    return;
  case IR_GLB_DEF_TY_TENTATIVE:
    BUG("should not have tentative defs by now");
  }

  struct ir_func *func = glb->func;
  struct ir_basicblock *basicblock = func->first;

  rebuild_ids(func);

  while (basicblock) {
    ir_validate_basicblock(func, basicblock);

    basicblock = basicblock->succ;
  }
}

void ir_validate(struct ir_unit *iru) {
  struct ir_glb *glb = iru->first_global;

  while (glb) {
    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      ir_validate_data(iru, glb);
      break;
    case IR_GLB_TY_FUNC:
      ir_validate_func(iru, glb);
      break;
    }

    glb = glb->succ;
  }
}
