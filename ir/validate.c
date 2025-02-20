#include "validate.h"

#include "../log.h"
#include "../util.h"
#include "../vector.h"
#include "ir.h"
#include "prettyprint.h"

struct ir_validate_error {
  const char *err;
  struct ir_object object;
};

struct ir_validate_state {
  struct ir_unit *unit;
  struct vector *errors;
};

struct validate_op_order_metadata {
  struct ir_validate_state *state;
  struct ir_op *consumer;
};

#define VALIDATION_CHECKZ(cond, obj, msg)                                      \
  if (!(cond)) {                                                               \
    struct ir_validate_error error = {.err = msg,                              \
                                      .object = IR_MK_OBJECT((obj))};          \
    vector_push_back((state)->errors, &error);                                 \
  }

#define VALIDATION_CHECK(cond, obj, fmt, ...)                                  \
  if (!(cond)) {                                                               \
    const char *msg =                                                          \
        arena_alloc_snprintf(state->unit->arena, fmt, __VA_ARGS__);            \
    struct ir_validate_error error = {.err = msg,                              \
                                      .object = IR_MK_OBJECT((obj))};          \
    vector_push_back((state)->errors, &error);                                 \
  }

static void validate_op_order(struct ir_op **ir, void *metadata) {
  struct validate_op_order_metadata *data = metadata;
  struct ir_op *consumer = data->consumer;

  if (consumer->ty == IR_OP_TY_PHI || (consumer->flags & IR_OP_FLAG_PHI_MOV)) {
    // these can work across time
    return;
  }

  struct ir_op *op = *ir;

  struct ir_validate_state *state = data->state;
  VALIDATION_CHECK(consumer->id > op->id, consumer,
                   "uses op %zu which is ahead of it", op->id);
}

static void ir_validate_op(struct ir_validate_state *state,
                           struct ir_func *func, struct ir_op *op) {
  struct validate_op_order_metadata metadata = {.state = state, .consumer = op};
  walk_op_uses(op, validate_op_order, &metadata);

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("should not have unknown ops");
  case IR_OP_TY_PHI:
    break;
  case IR_OP_TY_UNDF:
    break;
  case IR_OP_TY_MOV:
    VALIDATION_CHECKZ(!(op->flags & IR_OP_FLAG_PARAM) || !op->mov.value, op,
                      "param mov must have null value");
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
    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
      VALIDATION_CHECKZ(op->load.lcl, op, "load ty lcl must have lcl");
      break;
    case IR_OP_LOAD_TY_GLB:
      VALIDATION_CHECKZ(op->load.glb, op, "load ty glb must have glb");
      break;
    case IR_OP_LOAD_TY_ADDR:
      VALIDATION_CHECKZ(op->load.addr, op, "load ty addr must have addr");
      break;
    }
    break;
  case IR_OP_TY_STORE:
    VALIDATION_CHECKZ(op->var_ty.ty == IR_VAR_TY_TY_NONE, op,
                      "store ops should not have a var ty");

    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
      VALIDATION_CHECKZ(op->store.lcl, op, "store ty lcl must have lcl");
      break;
    case IR_OP_STORE_TY_GLB:
      VALIDATION_CHECKZ(op->store.glb, op, "store ty glb must have glb");
      break;
    case IR_OP_STORE_TY_ADDR:
      VALIDATION_CHECKZ(op->store.addr, op, "tore ty addr must have addr");
      break;
    }
    VALIDATION_CHECKZ(!op->lcl, op, "stores should not have locals");
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
    VALIDATION_CHECKZ(func->flags & IR_FUNC_FLAG_MAKES_CALL, op,
                      "CALL op present but IR_FUNC_FLAG_MAKES_CALL not set");
    break;
  case IR_OP_TY_CUSTOM:
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    break;
  case IR_OP_TY_BITFIELD_INSERT:
    break;
  case IR_OP_TY_MEM_SET:
    break;
  case IR_OP_TY_MEM_COPY:
    break;
  case IR_OP_TY_ADDR_OFFSET:
    break;
  }
}

static void ir_validate_stmt(struct ir_validate_state *state,
                             struct ir_func *func, struct ir_stmt *stmt) {
  struct ir_op *op = stmt->first;

  while (op) {
    ir_validate_op(state, func, op);

    op = op->succ;
  }
}

static void ir_validate_basicblock(struct ir_validate_state *state,
                                   struct ir_func *func,
                                   struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;

  while (stmt) {
    ir_validate_stmt(state, func, stmt);

    stmt = stmt->succ;
  }

  printf("validating bb %zu\n", basicblock->id);

  struct ir_op *last = basicblock->last->last;

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    VALIDATION_CHECKZ(last->ty == IR_OP_TY_RET, basicblock, "IR_BASICBLOCK_TY_RET should end in `ret` op");
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    VALIDATION_CHECKZ(last->ty == IR_OP_TY_BR_COND, basicblock, "IR_BASICBLOCK_TY_SPLIT should end in `br.cond` op");
    break;
  case IR_BASICBLOCK_TY_MERGE:
    VALIDATION_CHECKZ(last->ty == IR_OP_TY_BR, basicblock, "IR_BASICBLOCK_TY_MERGE should end in `br` op");
    break;
  case IR_BASICBLOCK_TY_SWITCH:
    VALIDATION_CHECKZ(last->ty == IR_OP_TY_BR_SWITCH, basicblock, "IR_BASICBLOCK_TY_RET should end in `br.switch` op");
    break;
  }
}

static void ir_validate_data(struct ir_validate_state *state,
                             struct ir_glb *glb) {}

static void ir_validate_func(struct ir_validate_state *state,
                             struct ir_glb *glb) {
  switch (glb->def_ty) {
  case IR_GLB_DEF_TY_DEFINED:
    VALIDATION_CHECKZ(glb->func, glb, "defined global should have func");
    break;
  case IR_GLB_DEF_TY_UNDEFINED:
    VALIDATION_CHECKZ(!glb->func, glb, "undefined global should not have func");
    return;
  case IR_GLB_DEF_TY_TENTATIVE:
    VALIDATION_CHECKZ(false, glb, "should not have tentative defs by now");
  }

  struct ir_func *func = glb->func;
  struct ir_basicblock *basicblock = func->first;

  rebuild_ids(func);

  while (basicblock) {
    ir_validate_basicblock(state, func, basicblock);

    basicblock = basicblock->succ;
  }
}

void ir_validate(struct ir_unit *iru) {
  struct ir_glb *glb = iru->first_global;

  struct ir_validate_state state = {
      .unit = iru, .errors = vector_create(sizeof(struct ir_validate_error))};

  while (glb) {
    switch (glb->ty) {
    case IR_GLB_TY_DATA:
      ir_validate_data(&state, glb);
      break;
    case IR_GLB_TY_FUNC:
      ir_validate_func(&state, glb);
      break;
    }

    glb = glb->succ;
  }

  size_t num_errs = vector_length(state.errors);
  if (!num_errs) {
    return;
  }

  fprintf(stderr, "*** IR VALIDATION FAILED ****\n");

  for (size_t i = 0; i < num_errs; i++) {
    struct ir_validate_error *error = vector_get(state.errors, i);

    fprintf(stderr, "Validation error: %s\n", error->err);
    debug_print_ir_object(stderr, iru, &error->object);

    fprintf(stderr, "\n\n\n");
  }

  BUG("VALIDATION FAILED");
}
