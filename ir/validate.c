#include "validate.h"

#include "../alloc.h"
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
  enum ir_validate_flags flags;
  struct vector *errors;
};

struct validate_op_order_metadata {
  struct ir_validate_state *state;
  struct ir_op *consumer;
};

#define IR_OBJECT_NAME(obj, msg)                                               \
  _Generic((obj),                                                              \
      struct ir_glb *: "glb" msg,                                              \
      struct ir_lcl *: "lcl" msg,                                              \
      struct ir_func *: "func" msg,                                            \
      struct ir_var *: "var" msg,                                              \
      struct ir_basicblock *: "basicblock" msg,                                \
      struct ir_stmt *: "stmt" msg,                                            \
      struct ir_op *: "op" msg)

#define VALIDATION_ERRZ(obj, msg)                                              \
  do {                                                                         \
    struct ir_validate_error error = {.err = (msg),                            \
                                      .object = IR_MK_OBJECT((obj))};          \
    vector_push_back((state)->errors, &error);                                 \
  } while (0);

#define VALIDATION_ERR(obj, fmt, ...)                                          \
  do {                                                                         \
    const char *msg =                                                          \
        arena_alloc_snprintf(state->unit->arena, (fmt), __VA_ARGS__);          \
    struct ir_validate_error error = {.err = msg,                              \
                                      .object = IR_MK_OBJECT((obj))};          \
    vector_push_back((state)->errors, &error);                                 \
  } while (0);

#define VALIDATION_CHECK(cond, obj, fmt, ...)                                  \
  if (!(cond)) {                                                               \
    VALIDATION_ERR((obj), (fmt), __VA_ARGS__);                                 \
  }

#define VALIDATION_CHECKZ(cond, obj, msg)                                      \
  VALIDATION_CHECK((cond), (obj), IR_OBJECT_NAME((obj), " %zu: %s"),           \
                   (obj)->id, (msg))

static void validate_op_order(struct ir_op **ir, void *metadata) {
  struct validate_op_order_metadata *data = metadata;
  struct ir_validate_state *state = data->state;

  struct ir_op *consumer = data->consumer;

  VALIDATION_CHECKZ((*ir)->id != DETACHED_OP, *ir, "op is detached!");

  VALIDATION_CHECKZ((*ir)->stmt, *ir, "op has no stmt!");

  if (consumer->ty == IR_OP_TY_PHI || (consumer->flags & IR_OP_FLAG_PHI_MOV)) {
    // these can work across time
    return;
  }

  struct ir_op *op = *ir;

  VALIDATION_CHECK(consumer->id > op->id, consumer,
                   "uses op %zu which is ahead of it", op->id);
}

static void ir_validate_lcl(struct ir_validate_state *state,
                            struct ir_lcl *lcl) {
  VALIDATION_CHECKZ(lcl->id != DETACHED_LCL, lcl, "lcl is detached!");

  VALIDATION_CHECKZ(lcl->func, lcl, "lcl has no func!");
}

static void ir_validate_op(struct ir_validate_state *state,
                           struct ir_func *func, struct ir_op *op) {
  struct validate_op_order_metadata metadata = {.state = state, .consumer = op};
  walk_op_uses(op, validate_op_order, &metadata);

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("should not have unknown ops");
  case IR_OP_TY_PHI:
    for (size_t i = 0; i < op->phi.num_values; i++) {
      struct ir_phi_entry *entry = &op->phi.values[i];

      // FIXME: inefficient
      bool found = false;
      for (size_t j = 0; j < op->stmt->basicblock->num_preds; j++) {
        if (op->stmt->basicblock->preds[j] == entry->basicblock) {
          found = true;
          break;
        }
      }

      VALIDATION_CHECK(found, op, "op entry had bb %zu which was not a pred",
                       entry->basicblock->id);
    }
    break;
  case IR_OP_TY_GATHER: {
    struct ir_var_ty *var_ty = &op->var_ty;

    switch (var_ty->ty) {
    case IR_VAR_TY_TY_UNION:
    case IR_VAR_TY_TY_STRUCT:
      VALIDATION_CHECK(op->gather.num_values == var_ty->aggregate.num_fields,
                       op, "gather has %zu fields but type has %zu fields",
                       op->gather.num_values, var_ty->aggregate.num_fields);
      break;
    default:
      VALIDATION_ERRZ(
          op, "gather only makes sense when result is an aggregate type");
      break;
    }
    break;
  }
  case IR_OP_TY_UNDF:
    break;
  case IR_OP_TY_MOV:
    VALIDATION_CHECKZ(
        !(op->flags & IR_OP_FLAG_PARAM) != !(op->mov.value), op,
        "param mov must have null value, and param mov cannot have value");
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
      ir_validate_lcl(state, op->load.lcl);
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
      ir_validate_lcl(state, op->store.lcl);
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
    VALIDATION_CHECKZ(op->var_ty.ty == IR_VAR_TY_TY_NONE, op,
                      "store ops should not have a var ty");

    switch (op->store_bitfield.ty) {
    case IR_OP_STORE_TY_LCL:
      VALIDATION_CHECKZ(op->store_bitfield.lcl, op,
                        "store ty lcl must have lcl");
      ir_validate_lcl(state, op->store_bitfield.lcl);
      break;
    case IR_OP_STORE_TY_GLB:
      VALIDATION_CHECKZ(op->store_bitfield.glb, op,
                        "store ty glb must have glb");
      break;
    case IR_OP_STORE_TY_ADDR:
      VALIDATION_CHECKZ(op->store_bitfield.addr, op,
                        "tore ty addr must have addr");
      break;
    }
    VALIDATION_CHECKZ(!op->lcl, op, "stores should not have locals");
    break;
  case IR_OP_TY_LOAD_BITFIELD:
    switch (op->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
      VALIDATION_CHECKZ(op->load_bitfield.lcl, op, "load ty lcl must have lcl");
      ir_validate_lcl(state, op->load_bitfield.lcl);
      break;
    case IR_OP_LOAD_TY_GLB:
      VALIDATION_CHECKZ(op->load_bitfield.glb, op, "load ty glb must have glb");
      break;
    case IR_OP_LOAD_TY_ADDR:
      VALIDATION_CHECKZ(op->load_bitfield.addr, op,
                        "load ty addr must have addr");
      break;
    }
    break;
  case IR_OP_TY_ADDR:
    switch (op->addr.ty) {
    case IR_OP_ADDR_TY_LCL:
      VALIDATION_CHECKZ(op->addr.lcl, op, "addr ty lcl must have lcl");
      ir_validate_lcl(state, op->addr.lcl);
      break;
    case IR_OP_ADDR_TY_GLB:
      VALIDATION_CHECKZ(op->addr.glb, op, "addr ty glb must have glb");
      break;
    }
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
  VALIDATION_CHECKZ(stmt->id != DETACHED_STMT, stmt, "stmt is detached!");

  VALIDATION_CHECKZ(stmt->basicblock, stmt, "stmt has no basicblock!");
  VALIDATION_CHECKZ(!(stmt->flags & IR_STMT_FLAG_PHI), stmt,
                    "only phi stmt should be at start of bb");

  struct ir_op *op = stmt->first;

  if (op && op_is_branch(op->ty)) {
    VALIDATION_CHECKZ(!op->succ, op, "branch should be only op in stmt");
  }

  while (op) {
    if (op->ty == IR_OP_TY_PHI &&
        !(state->flags & IR_VALIDATE_FLAG_ALLOW_MIXED_PHIS)) {
      VALIDATION_ERRZ(
          op, "should not have phi except in phi-flagged at start of bb");
    }

    ir_validate_op(state, func, op);

    op = op->succ;
  }
}

static void ir_validate_basicblock(struct ir_validate_state *state,
                                   struct ir_func *func,
                                   struct ir_basicblock *basicblock) {
  VALIDATION_CHECKZ(basicblock->id != DETACHED_BASICBLOCK, basicblock,
                    "basicblock is detached!");

  VALIDATION_CHECKZ(basicblock->func, basicblock, "basicblock has no func!");

  struct ir_stmt *stmt = basicblock->first;

  if (stmt) {
    if (stmt->flags & IR_STMT_FLAG_PHI) {
      if (!(state->flags & IR_VALIDATE_FLAG_ALLOW_MIXED_PHIS)) {
        struct ir_op *phi = stmt->first;
        while (phi) {
          VALIDATION_CHECKZ(phi->ty == IR_OP_TY_PHI, phi,
                            "expected all phis in stmt to be phis");

          phi = phi->succ;
        }
      }

      stmt = stmt->succ;
    } else if (stmt->flags & IR_STMT_FLAG_PARAM) {
      // TODO: validate
      // struct ir_op *param = stmt->first;
      // while (param) {
      //   VALIDATION_CHECKZ(param->ty == IR_OP_TY_param, param,
      //                     "expected all params in stmt to be params");

      //   param = param->succ;
      // }
    }
  }

  while (stmt) {
    ir_validate_stmt(state, func, stmt);

    stmt = stmt->succ;
  }

  struct ir_op *last = basicblock->last->last;

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    VALIDATION_CHECKZ(last->ty == IR_OP_TY_RET, basicblock,
                      "IR_BASICBLOCK_TY_RET should end in `ret` op");
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    VALIDATION_CHECKZ(last->ty == IR_OP_TY_BR_COND, basicblock,
                      "IR_BASICBLOCK_TY_SPLIT should end in `br.cond` op");
    break;
  case IR_BASICBLOCK_TY_MERGE:
    VALIDATION_CHECKZ(last->ty == IR_OP_TY_BR, basicblock,
                      "IR_BASICBLOCK_TY_MERGE should end in `br` op");
    break;
  case IR_BASICBLOCK_TY_SWITCH:
    VALIDATION_CHECKZ(last->ty == IR_OP_TY_BR_SWITCH, basicblock,
                      "IR_BASICBLOCK_TY_RET should end in `br.switch` op");
    break;
  }
}

static void ir_validate_data(struct ir_validate_state *state,
                             struct ir_glb *glb) {
  switch (glb->def_ty) {
  case IR_GLB_DEF_TY_DEFINED:
    VALIDATION_CHECKZ(glb->var, glb, "defined global should have var");
    break;
  case IR_GLB_DEF_TY_UNDEFINED:
    // VALIDATION_CHECKZ(!glb->var, glb, "undefined global should not have
    // var");
    return;
  case IR_GLB_DEF_TY_TENTATIVE:
    VALIDATION_ERRZ(glb, "should not have tentative defs by now");
    return;
  }
}

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
    VALIDATION_ERRZ(glb, "should not have tentative defs by now");
    return;
  }

  struct ir_func *func = glb->func;
  struct ir_basicblock *basicblock = func->first;

  rebuild_ids(func);

  struct ir_lcl *lcl = func->first_lcl;

  size_t lcl_count = 0;
  while (lcl) {
    ir_validate_lcl(state, lcl);

    lcl = lcl->succ;
    lcl_count++;
  }

  VALIDATION_CHECK(func->lcl_count == lcl_count, func,
                   "lcl_count=%zu but found %zu", func->lcl_count, lcl_count);

  size_t bb_count = 0;
  while (basicblock) {
    ir_validate_basicblock(state, func, basicblock);

    basicblock = basicblock->succ;
    bb_count++;
  }

  VALIDATION_CHECK(func->basicblock_count == bb_count, func,
                   "basicblock_count=%zu but found %zu", func->basicblock_count,
                   bb_count);
}

void ir_validate(struct ir_unit *iru, enum ir_validate_flags flags) {
  struct ir_glb *glb = iru->first_global;

  struct ir_validate_state state = {
      .unit = iru,
      .flags = flags,
      .errors = vector_create(sizeof(struct ir_validate_error))};

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
    vector_free(&state.errors);
    return;
  }

  fprintf(stderr, "*** IR VALIDATION FAILED ****\n");

  for (size_t i = 0; i < num_errs; i++) {
    struct ir_validate_error *error = vector_get(state.errors, i);

    fprintf(stderr, "Validation error: %s\n", error->err);
    debug_print_ir_object(stderr, &error->object);

    fprintf(stderr, "\n\n\n");
  }

  vector_free(&state.errors);

  BUG("VALIDATION FAILED");
}
