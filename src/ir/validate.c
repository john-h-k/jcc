#include "validate.h"

#include "../alloc.h"
#include "../util.h"
#include "../vector.h"
#include "ir.h"
#include "prettyprint.h"

// TODO: certain failures will cause post-validation printing to fail (e.g
// basicblock not having a func) we should probably BUG on them instead

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
  struct ir_func *func;
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
  } while (0)

#define VALIDATION_ERR(obj, fmt, ...)                                          \
  do {                                                                         \
    const char *msg =                                                          \
        aralloc_snprintf(state->unit->arena, (fmt), __VA_ARGS__);              \
    struct ir_validate_error error = {.err = msg,                              \
                                      .object = IR_MK_OBJECT((obj))};          \
    vector_push_back((state)->errors, &error);                                 \
  } while (0)

#define VALIDATION_CHECK(cond, obj, fmt, ...)                                  \
  do {                                                                         \
    if (!(cond)) {                                                             \
      VALIDATION_ERR((obj), (fmt), __VA_ARGS__);                               \
    }                                                                          \
  } while (0)

#define VALIDATION_CHECKZ(cond, obj, msg)                                      \
  VALIDATION_CHECK((cond), (obj), IR_OBJECT_NAME((obj), " %zu: %s"),           \
                   (obj)->id, (msg))

#ifndef NDEBUG
static void validate_op_order(struct ir_op **ir,
                              UNUSED enum ir_op_use_ty use_ty, void *metadata) {
  struct validate_op_order_metadata *data = metadata;
  struct ir_validate_state *state = data->state;


  struct ir_func *func = NULL;
  if (*ir) {
    struct ir_stmt *stmt = (*ir)->stmt;

    if (stmt) {
      struct ir_basicblock *basicblock = stmt->basicblock;

      if (basicblock) {
        func = basicblock->func;
      }
    }
  }

  VALIDATION_CHECKZ(func, *ir, "consumed op has no func!");

  if (!func) {
    return;
  }

  if (func != data->func) {
    VALIDATION_ERR(
        *ir,
        "consumed op attached to func '%s' but should be attached to '%s'",
        func->name, data->func->name);

    return;
  }

  struct ir_op *consumer = data->consumer;

  VALIDATION_CHECKZ((*ir)->id != DETACHED_OP, consumer, "op uses detached op!");

  VALIDATION_CHECKZ((*ir)->stmt, consumer, "op uses op with no stmt!");

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
  VALIDATION_CHECKZ(!(lcl->flags & IR_OP_FLAG_PROMOTED) ||
                        (lcl->flags & IR_OP_FLAG_PROMOTED),
                    lcl, "promoted lcls must be params");
}

static bool validate_var_ty_is_pointer(struct ir_validate_state *state,
                                       struct ir_var_ty *var_ty) {
  // we have to always allower pointer-sized-int because typechk generates them
  // for e.g array access indices

  struct ir_var_ty pointer_ty = ir_var_ty_for_pointer_size(state->unit);
  if (ir_var_ty_eq(var_ty, &pointer_ty)) {
    return true;
  }

  if (!(state->flags & IR_VALIDATE_FLAG_LOWERED_POINTERS)) {
    return var_ty->ty == IR_VAR_TY_TY_POINTER;
  }

  return false;
}

struct walk_op_metadata {
  struct ir_func *func;
  struct ir_validate_state *state;
  struct ir_op *consumer;
};

static void walk_op_ty_callback(struct ir_op **op, enum ir_op_use_ty use_ty,
                                void *metadata) {
  struct walk_op_metadata *walk_state = metadata;
  struct ir_validate_state *state = walk_state->state;

  switch (use_ty) {
  case IR_OP_USE_TY_DEREF:
    VALIDATION_CHECK(
        validate_var_ty_is_pointer(state, &(*op)->var_ty), walk_state->consumer,
        "operand %zu with use type IR_OP_USE_TY_DEREF must have type pointer",
        (*op)->id);
    break;
  case IR_OP_USE_TY_READ: {
    if ((*op)->var_ty.ty == IR_VAR_TY_TY_POINTER &&
        validate_var_ty_is_pointer(state, &walk_state->consumer->var_ty)) {
      break;
    }
    if (walk_state->consumer->var_ty.ty == IR_VAR_TY_TY_POINTER &&
        validate_var_ty_is_pointer(state, &(*op)->var_ty)) {
      break;
    }
    VALIDATION_CHECK(
        ir_var_ty_eq(&(*op)->var_ty, &walk_state->consumer->var_ty),
        walk_state->consumer,
        "operand %zu with use type IR_OP_USE_TY_READ must have same type",
        (*op)->id);
    break;
  }
  }
}

static void ir_validate_operands_same_ty(struct ir_validate_state *state,
                                         struct ir_op *op) {
  struct ir_func *func = op->stmt->basicblock->func;

  struct walk_op_metadata metadata = {
    .func = func,
    .state = state, .consumer = op};

  ir_walk_op_uses(op, walk_op_ty_callback, &metadata);
}

static void ir_validate_mov_op(struct ir_validate_state *state,
                               struct ir_op *op) {
  struct ir_var_ty l = op->var_ty;

  VALIDATION_CHECKZ(l.ty != IR_VAR_TY_TY_NONE, op,
                    "mov op with none type makes no sense");

  if (!op->mov.value) {
    VALIDATION_CHECKZ(op->flags & IR_OP_FLAG_PARAM, op,
                      "mov op with no value should have flag .param");
    return;
  }

  struct ir_op *value = op->mov.value;
  VALIDATION_CHECKZ(value, op, "mov op without flag .param should have value");

  struct ir_var_ty r = value->var_ty;

  VALIDATION_CHECKZ(r.ty != IR_VAR_TY_TY_NONE, op,
                    "mov op with none type makes no sense");

  if (l.ty != IR_VAR_TY_TY_NONE && r.ty != IR_VAR_TY_TY_NONE) {
    struct ir_var_ty_info l_info = ir_var_ty_info(state->unit, &l);
    struct ir_var_ty_info r_info = ir_var_ty_info(state->unit, &r);

    VALIDATION_CHECKZ(l_info.size == r_info.size, op,
                      "mov op should have same-sized operands");
  }
}

static void ir_validate_cast_op(struct ir_validate_state *state,
                                struct ir_op *op) {

  struct ir_var_ty from = op->cast_op.value->var_ty;
  struct ir_var_ty to = op->var_ty;

  struct ir_var_ty_info from_info =
      ir_var_ty_info(state->unit, &op->cast_op.value->var_ty);
  struct ir_var_ty_info to_info = ir_var_ty_info(state->unit, &op->var_ty);

  switch (op->cast_op.ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    VALIDATION_CHECKZ(
        ir_var_ty_is_integral(&from) && ir_var_ty_is_integral(&to), op,
        "both sides of sext operator should be integral types");

    VALIDATION_CHECKZ(
        from_info.size < to_info.size, op,
        "source operand of sext should be smaller than result type");
    break;
  case IR_OP_CAST_OP_TY_ZEXT:
    VALIDATION_CHECKZ(
        ir_var_ty_is_integral(&from) && ir_var_ty_is_integral(&to), op,
        "both sides of zext operator should be integral types");

    VALIDATION_CHECKZ(
        from_info.size < to_info.size, op,
        "source operand of zext should be smaller than result type");
    break;
  case IR_OP_CAST_OP_TY_TRUNC:
    VALIDATION_CHECKZ(
        ir_var_ty_is_integral(&from) && ir_var_ty_is_integral(&to), op,
        "both sides of trunc operator should be integral types");

    VALIDATION_CHECKZ(
        from_info.size > to_info.size, op,
        "source operand of trunc should be bigger than result type");
    break;
  case IR_OP_CAST_OP_TY_CONV:
    VALIDATION_CHECKZ(ir_var_ty_is_fp(&from) && ir_var_ty_is_fp(&to), op,
                      "both sides of conv operator should be fp types");
    break;
  case IR_OP_CAST_OP_TY_UCONV:
    VALIDATION_CHECKZ(
        (ir_var_ty_is_fp(&from) && ir_var_ty_is_integral(&to)) ||
            (ir_var_ty_is_integral(&from) && ir_var_ty_is_fp(&to)),
        op, "both sides of uconv operator should be fp types");
    break;
  case IR_OP_CAST_OP_TY_SCONV:
    VALIDATION_CHECKZ(
        (ir_var_ty_is_fp(&from) && ir_var_ty_is_integral(&to)) ||
            (ir_var_ty_is_integral(&from) && ir_var_ty_is_fp(&to)),
        op, "both sides of sconv operator should be fp types");
    break;
  }
}

static void ir_validate_unary_op(struct ir_validate_state *state,
                                 struct ir_op *op) {

  enum ir_op_unary_op_ty ty = op->unary_op.ty;

  switch (ty) {
  case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
    VALIDATION_CHECKZ(ir_var_ty_is_integral(&op->var_ty), op,
                      "logical not (!) should have integral type");
    return;
  default:
    break;
  }

  ir_validate_operands_same_ty(state, op);
}

static void ir_validate_binary_op(struct ir_validate_state *state,
                                  struct ir_op *op) {
  enum ir_op_binary_op_ty ty = op->binary_op.ty;

  if (ir_binary_op_is_comparison(ty)) {
    VALIDATION_CHECKZ(ir_var_ty_is_integral(&op->var_ty), op,
                      "comparison binops should have integral type");
    return;
  }

  switch (ty) {
  case IR_OP_BINARY_OP_TY_SRSHIFT:
  case IR_OP_BINARY_OP_TY_URSHIFT:
  case IR_OP_BINARY_OP_TY_LSHIFT:
    // we could support floating point shifts in future but codegen will be
    // wrong for them so may as well catch it here
    VALIDATION_CHECKZ(ir_var_ty_is_integral(&op->binary_op.lhs->var_ty), op,
                      "shift binops should have integral lhs type");

    VALIDATION_CHECKZ(ir_var_ty_is_integral(&op->binary_op.rhs->var_ty), op,
                      "shift binops should have integral rhs type");
    return;
  default:
    break;
  }

  ir_validate_operands_same_ty(state, op);
}

static void ir_validate_op(struct ir_validate_state *state,
                           struct ir_func *func, struct ir_stmt *stmt,
                           struct ir_op *op) {
  struct validate_op_order_metadata metadata = {.func = func, .state = state, .consumer = op};
  ir_walk_op_uses(op, validate_op_order, &metadata);

  VALIDATION_CHECKZ(op->stmt, op, "op has no stmt");
  VALIDATION_CHECK(op->stmt == stmt, op,
                   "op attached to stmt %zu but should be attached to %zu",
                   op->stmt->id, stmt->id);

  switch (op->reg.ty) {
  case IR_REG_TY_NONE:
  case IR_REG_TY_SPILLED:
  case IR_REG_TY_FLAGS:
    break;
  case IR_REG_TY_INTEGRAL:
  case IR_REG_TY_FP:
    // can raise this if needed if we are somehow on a platform with more than
    // 1024 registers
    VALIDATION_CHECK(op->reg.idx < 1024, op,
                     "reg had type integral/fp but very large idx (%zu), "
                     "likely badly initialied",
                     op->reg.idx);
    break;
  }

  switch (op->ty) {
  case IR_OP_TY_UNKNOWN:
    BUG("should not have unknown ops");
  case IR_OP_TY_VA_START:
    break;
  case IR_OP_TY_VA_ARG:
    break;
  case IR_OP_TY_PHI:
    VALIDATION_CHECKZ(op->phi.num_values > 1, op, "phi with <2 operands");

    ir_validate_operands_same_ty(state, op);

    for (size_t i = 0; i < op->phi.num_values; i++) {
      struct ir_phi_entry *entry = &op->phi.values[i];

      VALIDATION_CHECKZ(ir_var_ty_eq(&op->var_ty, &entry->value->var_ty), op,
                        "op entry had different type to op!");

      VALIDATION_CHECK(
          ir_basicblock_is_pred(op->stmt->basicblock, entry->basicblock), op,
          "op entry had bb %zu which was not a pred", entry->basicblock->id);
    }
    break;
  case IR_OP_TY_GATHER: {
    // TODO: validate each entry is field ty

    struct ir_var_ty *var_ty = &op->var_ty;

    switch (var_ty->ty) {
    case IR_VAR_TY_TY_UNION:
    case IR_VAR_TY_TY_STRUCT:
      // TODO: needs to validate flattened type instead
      // VALIDATION_CHECK(op->gather.num_values == var_ty->aggregate.num_fields,
      //                  op, "gather has %zu fields but type has %zu fields",
      //                  op->gather.num_values, var_ty->aggregate.num_fields);
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
    ir_validate_mov_op(state, op);
    break;
  case IR_OP_TY_CNST:
    break;
  case IR_OP_TY_BINARY_OP:
    ir_validate_binary_op(state, op);
    break;
  case IR_OP_TY_UNARY_OP:
    ir_validate_unary_op(state, op);
    break;
  case IR_OP_TY_CAST_OP:
    ir_validate_cast_op(state, op);
    break;
  case IR_OP_TY_STORE:
    VALIDATION_CHECKZ(op->var_ty.ty == IR_VAR_TY_TY_NONE, op,
                      "store ops should not have a var ty");
    switch (op->store.ty) {
    case IR_OP_STORE_TY_LCL:
      // TODO: disabled, reenable (need to handle struct fields and stuff)
      // VALIDATION_CHECKZ(op->store.lcl, op, "store ty lcl must have lcl");

      // VALIDATION_CHECK(ir_var_ty_eq(state->unit, &op->store.value->var_ty,
      //                               &op->store.lcl->var_ty),
      //                  op, "lcl %zu must have same type", op->store.lcl->id);
      break;
    case IR_OP_STORE_TY_GLB:
      // VALIDATION_CHECKZ(op->store.glb, op, "store ty glb must have glb");

      // VALIDATION_CHECK(ir_var_ty_eq(state->unit, &op->store.value->var_ty,
      //                               &op->store.glb->var_ty),
      //                  op, "glb %zu must have same type", op->store.glb->id);
      break;
    case IR_OP_STORE_TY_ADDR: {
      VALIDATION_CHECKZ(op->store.addr, op, "store ty addr must have addr");
      // struct ir_op *addr = op->store.addr;
      // if (addr->ty == IR_OP_TY_ADDR) {
      //   switch (addr->addr.ty) {
      //   case IR_OP_ADDR_TY_LCL:
      //     VALIDATION_CHECK(
      //         (ir_var_ty_is_aggregate(&addr->addr.lcl->var_ty) ||
      //          addr->addr.lcl->var_ty.ty == IR_VAR_TY_TY_ARRAY) ||
      //             ir_var_ty_eq(state->unit, &op->store.value->var_ty,
      //                          &addr->addr.lcl->var_ty),
      //         op, "lcl %zu must have same type", addr->addr.lcl->id);
      //     break;
      //   case IR_OP_ADDR_TY_GLB:
      //     VALIDATION_CHECK(
      //         (ir_var_ty_is_aggregate(&addr->addr.glb->var_ty) ||
      //          addr->addr.glb->var_ty.ty == IR_VAR_TY_TY_ARRAY) ||
      //             ir_var_ty_eq(state->unit, &op->store.value->var_ty,
      //                          &addr->addr.glb->var_ty),
      //         op, "glb %zu must have same type", addr->addr.glb->id);
      //     break;
      //   }
      // }
      break;
    }
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

      VALIDATION_CHECK(ir_var_ty_eq(&op->store_bitfield.value->var_ty,
                                    &op->store_bitfield.lcl->var_ty),
                       op, "lcl %zu must have same type",
                       op->store_bitfield.lcl->id);
      break;
    case IR_OP_STORE_TY_GLB:
      VALIDATION_CHECKZ(op->store_bitfield.glb, op,
                        "store ty glb must have glb");
      VALIDATION_CHECK(ir_var_ty_eq(&op->store_bitfield.value->var_ty,
                                    &op->store_bitfield.glb->var_ty),
                       op, "glb %zu must have same type",
                       op->store_bitfield.glb->id);
      break;
    case IR_OP_STORE_TY_ADDR: {
      VALIDATION_CHECKZ(op->store_bitfield.addr, op,
                        "store ty addr must have addr");

      // struct ir_op *addr = op->store_bitfield.addr;
      // if (addr->ty == IR_OP_TY_ADDR) {
      //   switch (addr->addr.ty) {
      //   case IR_OP_ADDR_TY_LCL:
      //     VALIDATION_CHECK(
      //         (ir_var_ty_is_aggregate(&addr->addr.lcl->var_ty) ||
      //          addr->addr.lcl->var_ty.ty == IR_VAR_TY_TY_ARRAY) ||
      //             ir_var_ty_eq(state->unit,
      //             &op->store_bitfield.value->var_ty,
      //                          &addr->addr.lcl->var_ty),
      //         op, "lcl %zu must have same type", addr->addr.lcl->id);
      //     break;
      //   case IR_OP_ADDR_TY_GLB:
      //     VALIDATION_CHECK(
      //         (ir_var_ty_is_aggregate(&addr->addr.glb->var_ty) ||
      //          addr->addr.glb->var_ty.ty == IR_VAR_TY_TY_ARRAY) ||
      //             ir_var_ty_eq(state->unit,
      //             &op->store_bitfield.value->var_ty,
      //                          &addr->addr.glb->var_ty),
      //         op, "glb %zu must have same type", addr->addr.glb->id);
      //     break;
      //   }
      // }
      break;
    }
    }

    VALIDATION_CHECKZ(!op->lcl, op, "stores should not have locals");
    break;

  case IR_OP_TY_LOAD:
    ir_validate_operands_same_ty(state, op);

    switch (op->load.ty) {
    case IR_OP_LOAD_TY_LCL:
      // TODO: disabled, reenable (need to handle struct fields and stuff)
      //   VALIDATION_CHECKZ(op->load.lcl, op, "load ty lcl must have lcl");

      //   VALIDATION_CHECK(
      //       ir_var_ty_eq(state->unit, &op->var_ty, &op->load.lcl->var_ty),
      //       op, "lcl %zu must have same type", op->load.lcl->id);
      break;
    case IR_OP_LOAD_TY_GLB:
      //   VALIDATION_CHECKZ(op->load.glb, op, "load ty glb must have glb");

      //   VALIDATION_CHECK(
      //       ir_var_ty_eq(state->unit, &op->var_ty, &op->load.glb->var_ty),
      //       op, "glb %zu must have same type", op->load.glb->id);
      break;
    case IR_OP_LOAD_TY_ADDR: {
      VALIDATION_CHECKZ(op->load.addr, op, "load ty addr must have addr");

      // struct ir_op *addr = op->load.addr;
      // if (addr->ty == IR_OP_TY_ADDR) {
      //   switch (addr->addr.ty) {
      //   case IR_OP_ADDR_TY_LCL:
      //     VALIDATION_CHECK(
      //         (ir_var_ty_is_aggregate(&addr->addr.lcl->var_ty) ||
      //          addr->addr.lcl->var_ty.ty == IR_VAR_TY_TY_ARRAY) ||
      //             ir_var_ty_eq(state->unit, &op->var_ty,
      //                          &addr->addr.lcl->var_ty),
      //         op, "lcl %zu must have same type", addr->addr.lcl->id);
      //     break;
      //   case IR_OP_ADDR_TY_GLB:
      //     VALIDATION_CHECK(
      //         (ir_var_ty_is_aggregate(&addr->addr.glb->var_ty) ||
      //          addr->addr.glb->var_ty.ty == IR_VAR_TY_TY_ARRAY) ||
      //             ir_var_ty_eq(state->unit, &op->var_ty,
      //                          &addr->addr.glb->var_ty),
      //         op, "glb %zu must have same type", addr->addr.glb->id);
      //     break;
      //   }
      // }
      break;
    }
    }
    break;
  case IR_OP_TY_LOAD_BITFIELD:
    switch (op->load_bitfield.ty) {
    case IR_OP_LOAD_TY_LCL:
      VALIDATION_CHECKZ(op->load_bitfield.lcl, op, "load ty lcl must have lcl");

      VALIDATION_CHECK(
          ir_var_ty_eq(&op->var_ty, &op->load_bitfield.lcl->var_ty), op,
          "lcl %zu must have same type", op->load_bitfield.lcl->id);
      break;
    case IR_OP_LOAD_TY_GLB:
      VALIDATION_CHECKZ(op->load_bitfield.glb, op, "load ty glb must have glb");

      VALIDATION_CHECK(
          ir_var_ty_eq(&op->var_ty, &op->load_bitfield.glb->var_ty), op,
          "glb %zu must have same type", op->load_bitfield.glb->id);
      break;
    case IR_OP_LOAD_TY_ADDR: {
      VALIDATION_CHECKZ(op->load_bitfield.addr, op,
                        "load ty addr must have addr");

      // struct ir_op *addr = op->load_bitfield.addr;
      // if (addr->ty == IR_OP_TY_ADDR) {
      //   switch (addr->addr.ty) {
      //   case IR_OP_ADDR_TY_LCL:
      //     VALIDATION_CHECK(
      //         (ir_var_ty_is_aggregate(&addr->addr.lcl->var_ty) ||
      //          addr->addr.lcl->var_ty.ty == IR_VAR_TY_TY_ARRAY) ||
      //             ir_var_ty_eq(state->unit, &op->var_ty,
      //                          &addr->addr.lcl->var_ty),
      //         op, "lcl %zu must have same type", addr->addr.lcl->id);
      //     break;
      //   case IR_OP_ADDR_TY_GLB:
      //     VALIDATION_CHECK(
      //         (ir_var_ty_is_aggregate(&addr->addr.glb->var_ty) ||
      //          addr->addr.glb->var_ty.ty == IR_VAR_TY_TY_ARRAY) ||
      //             ir_var_ty_eq(state->unit, &op->var_ty,
      //                          &addr->addr.glb->var_ty),
      //         op, "glb %zu must have same type", addr->addr.glb->id);
      //     break;
      //   }
      // }
      break;
    }
    }
    break;
  case IR_OP_TY_ADDR:
    VALIDATION_CHECKZ(validate_var_ty_is_pointer(state, &op->var_ty), op,
                      "address must have type pointer");

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
    VALIDATION_CHECKZ(!op->succ && !op->stmt->succ, op, "br op should be at end of bb");
    VALIDATION_CHECKZ(op->var_ty.ty == IR_VAR_TY_TY_NONE, op,
                      "br ops should not have a var ty");
    break;
  case IR_OP_TY_BR_COND:
    VALIDATION_CHECKZ(!op->succ && !op->stmt->succ, op, "br.cond op should be at end of bb");
    VALIDATION_CHECKZ(op->var_ty.ty == IR_VAR_TY_TY_NONE, op,
                      "br.cond ops should not have a var ty");
    break;
  case IR_OP_TY_BR_SWITCH:
    VALIDATION_CHECKZ(!op->succ && !op->stmt->succ, op, "br.switch op should be at end of bb");
    VALIDATION_CHECKZ(op->var_ty.ty == IR_VAR_TY_TY_NONE, op,
                      "br.switch ops should not have a var ty");
    break;
  case IR_OP_TY_RET:
    VALIDATION_CHECKZ(!op->succ && !op->stmt->succ, op, "ret op should be at end of bb");
    // currently ret always returns none (do we want this?)
    // ir_validate_operands_same_ty(state, op);
    break;
  case IR_OP_TY_CALL:
    VALIDATION_CHECKZ(func->flags & IR_FUNC_FLAG_MAKES_CALL, op,
                      "CALL op present but IR_FUNC_FLAG_MAKES_CALL not set");
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    ir_validate_operands_same_ty(state, op);
    break;
  case IR_OP_TY_BITFIELD_INSERT:
    ir_validate_operands_same_ty(state, op);
    break;
  case IR_OP_TY_MEM_SET:
    VALIDATION_CHECKZ(op->var_ty.ty == IR_VAR_TY_TY_NONE, op,
                      "mem.set ops should not have a var ty");
    break;
  case IR_OP_TY_MEM_COPY:
    VALIDATION_CHECKZ(op->var_ty.ty == IR_VAR_TY_TY_NONE, op,
                      "mem.copy ops should not have a var ty");
    break;
  case IR_OP_TY_ADDR_OFFSET:
    VALIDATION_CHECKZ(validate_var_ty_is_pointer(state, &op->var_ty), op,
                      "address must have type pointer");
    VALIDATION_CHECKZ(
        validate_var_ty_is_pointer(state, &op->addr_offset.base->var_ty),
        op->addr_offset.base, "addr.off base address must have type pointer");
    VALIDATION_CHECKZ(
        !op->addr_offset.index ||
            validate_var_ty_is_pointer(state, &op->addr_offset.index->var_ty),
        op->addr_offset.index, "addr.off base address must have type pointer");
    break;
  }
}

static void ir_validate_stmt(struct ir_validate_state *state,
                             struct ir_func *func,
                             struct ir_basicblock *basicblock,
                             struct ir_stmt *stmt) {
  VALIDATION_CHECKZ(stmt->id != DETACHED_STMT, stmt, "stmt is detached!");

  VALIDATION_CHECKZ(stmt->basicblock, stmt, "stmt has no basicblock!");
  VALIDATION_CHECK(
      stmt->basicblock == basicblock, stmt,
      "stmt attached to basicblock %zu but should be attached to %zu",
      stmt->basicblock->id, basicblock->id);
  VALIDATION_CHECKZ(!(stmt->flags & IR_STMT_FLAG_PHI), stmt,
                    "only phi stmt should be at start of bb");

  VALIDATION_CHECKZ(
      (stmt->first == NULL) == (stmt->last == NULL), stmt,
      "either stmt->first and stmt->last should be NULL, or neither");

  struct ir_op *op = stmt->first;

  if (op && ir_op_is_branch(op->ty)) {
    VALIDATION_CHECKZ(!op->succ, op, "branch should be only op in stmt");
  }

  while (op) {
    if (op->ty == IR_OP_TY_PHI &&
        !(state->flags & IR_VALIDATE_FLAG_ALLOW_MIXED_PHIS)) {
      VALIDATION_ERRZ(
          op, "should not have phi except in phi-flagged at start of bb");
    }

    ir_validate_op(state, func, stmt, op);

    op = op->succ;
  }
}

static bool has_pred(struct ir_basicblock *pred,
                     struct ir_basicblock *basicblock) {
  for (size_t i = 0; i < basicblock->num_preds; i++) {
    if (basicblock->preds[i] == pred) {
      return true;
    }
  }

  return false;
}

static void ir_validate_basicblock(struct ir_validate_state *state,
                                   struct ir_func *func,
                                   struct ir_basicblock *basicblock) {
  VALIDATION_CHECKZ(basicblock->id != DETACHED_BASICBLOCK, basicblock,
                    "basicblock is detached!");

  VALIDATION_CHECKZ(basicblock->func, basicblock, "basicblock has no func!");

  if (!basicblock->func) {
    return;
  }

  VALIDATION_CHECKZ((basicblock->first == NULL) == (basicblock->last == NULL),
                    basicblock,
                    "either basicblock->first and basicblock->last should be "
                    "NULL, or neither");

  VALIDATION_CHECK(
      basicblock->func == func, basicblock,
      "basicblock attached to func '%s' but should be attached to '%s'",
      basicblock->func->name, func->name);

  for (size_t i = 0; i < basicblock->num_preds; i++) {
    struct ir_basicblock *pred = basicblock->preds[i];

    VALIDATION_CHECK(ir_basicblock_is_pred(basicblock, pred), basicblock,
                     "basicblock has incorrect pred %zu", pred->id);
  }

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    VALIDATION_CHECK(has_pred(basicblock, basicblock->split.true_target),
                     basicblock,
                     "basicblock has true_target %zu but it is not in preds "
                     "for that basicblock",
                     basicblock->split.true_target->id);
    VALIDATION_CHECK(has_pred(basicblock, basicblock->split.false_target),
                     basicblock,
                     "basicblock has false_target %zu but it is not in preds "
                     "for that basicblock",
                     basicblock->split.false_target->id);
    break;
  case IR_BASICBLOCK_TY_MERGE:
    VALIDATION_CHECK(has_pred(basicblock, basicblock->merge.target), basicblock,
                     "basicblock has target %zu but it is not in preds for "
                     "that basicblock",
                     basicblock->merge.target->id);
    break;
  case IR_BASICBLOCK_TY_SWITCH:
    for (size_t i = 0; i < basicblock->switch_case.num_cases; i++) {
      VALIDATION_CHECK(
          has_pred(basicblock, basicblock->switch_case.cases[i].target),
          basicblock,
          "basicblock has switch target %zu but it is not in preds for that "
          "basicblock",
          basicblock->switch_case.cases[i].target->id);
    }

    VALIDATION_CHECKZ(basicblock->switch_case.default_target, basicblock,
                      "basicblock has no default target! some code acts as if "
                      "this is legal, but it is not");
    VALIDATION_CHECK(
        has_pred(basicblock, basicblock->switch_case.default_target),
        basicblock,
        "basicblock has default_target %zu but it is not in preds for that "
        "basicblock",
        basicblock->switch_case.default_target->id);
    break;
  }

  struct ir_stmt *stmt = basicblock->first;

  if (stmt) {
    if (stmt->flags & IR_STMT_FLAG_PHI) {
      if (!(state->flags & IR_VALIDATE_FLAG_ALLOW_MIXED_PHIS)) {
        struct ir_op *phi = stmt->first;
        while (phi) {
          VALIDATION_CHECKZ(phi->ty == IR_OP_TY_PHI, phi,
                            "expected all phis in stmt to be phis");

          ir_validate_op(state, func, stmt, phi);

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
    ir_validate_stmt(state, func, basicblock, stmt);

    stmt = stmt->succ;
  }

  struct ir_op *last = basicblock->last ? basicblock->last->last : NULL;

  switch (basicblock->ty) {
  case IR_BASICBLOCK_TY_RET:
    VALIDATION_CHECKZ(last && last->ty == IR_OP_TY_RET, basicblock,
                      "IR_BASICBLOCK_TY_RET should end in `ret` op");
    break;
  case IR_BASICBLOCK_TY_SPLIT:
    VALIDATION_CHECKZ(last && last->ty == IR_OP_TY_BR_COND, basicblock,
                      "IR_BASICBLOCK_TY_SPLIT should end in `br.cond` op");
    break;
  case IR_BASICBLOCK_TY_MERGE:
    VALIDATION_CHECKZ(last && last->ty == IR_OP_TY_BR, basicblock,
                      "IR_BASICBLOCK_TY_MERGE should end in `br` op");
    break;
  case IR_BASICBLOCK_TY_SWITCH:
    VALIDATION_CHECKZ(last && last->ty == IR_OP_TY_BR_SWITCH, basicblock,
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

  if (!func->basicblock_count || !func->stmt_count || !func->op_count) {
    VALIDATION_ERRZ(
        func, "func has no basicblocks, stmts, or ops. should be impossible?");
  }

  ir_rebuild_func_ids(func);

  struct ir_lcl *lcl = func->first_lcl;

  size_t lcl_count = 0;
  while (lcl) {
    ir_validate_lcl(state, lcl);

    lcl = lcl->succ;
    lcl_count++;
  }

  VALIDATION_CHECK(func->lcl_count == lcl_count, func,
                   "lcl_count=%zu but found %zu", func->lcl_count, lcl_count);

  struct ir_basicblock *basicblock = func->first;
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
#endif

#ifdef NDEBUG
void ir_validate(UNUSED struct ir_unit *iru,
                 UNUSED enum ir_validate_flags flags) {}

#else
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

  if (num_errs) {
    fprintf(stderr, "***** IR VALIDATION FAILED *****\n\n");

    for (size_t i = 0; i < num_errs; i++) {
      struct ir_validate_error *error = vector_get(state.errors, i);

      fprintf(stderr, "Validation error: %s\n", error->err);
      debug_print_ir_object(stderr, &error->object);

      fprintf(stderr, "\n\n\n");
    }

    vector_free(&state.errors);

    // bug'ing out means we get a stacktrace (if possible) plus means we don't
    // need to consider validation failure as a path in the rest of the compiler
    // this is good, because it _isn't_, and failures are a bug within jcc
    BUG("VALIDATION FAILED");
  }

  vector_free(&state.errors);
}
#endif
