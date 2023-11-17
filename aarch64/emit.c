#include "emit.h"
#include "emitter.h"

#define RETURN_REG (0)

struct emit_state {
  struct arena_allocator *arena;
  struct aarch64_emitter *emitter;
  struct interval_data intervals;
};

size_t get_reg_for_op(struct emit_state *state, struct ir_op *op) {
  struct interval interval = state->intervals.intervals[op->id];
  debug_assert(interval.op_id == op->id, "intervals was not keyed by ID");
  return interval.reg;
}

void lower_binary_op(struct emit_state *state, struct ir_op *op) {
  debug_assert(op->ty == IR_OP_TY_BINARY_OP, "wrong ty op to `lower_binary_op`");
  size_t reg = get_reg_for_op(state, op);
  size_t lhs_reg = get_reg_for_op(state, op->binary_op.lhs);
  size_t rhs_reg = get_reg_for_op(state, op->binary_op.rhs);
  invariant_assert(lhs_reg != UINT32_MAX && rhs_reg != UINT32_MAX,
                   "bad IR, no reg");

  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_ADD:
    aarch64_emit_add_32(state->emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    aarch64_emit_sub_32(state->emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_MUL:
    aarch64_emit_mul_32(state->emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_SDIV:
    aarch64_emit_sdiv_32(state->emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_UDIV:
    aarch64_emit_udiv_32(state->emitter, lhs_reg, rhs_reg, reg);
    break;
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_UQUOT:
    bug("SQUOT and UQUOT should've been lowered to div-msub pair already");
  default:
    todo("unsupported op");
  }
}

const char *mangle(struct arena_allocator *arena, const char *name) {
  char *dest = alloc(arena, strlen(name) + /* null terminator + '_' char */ 2);

  dest[0] = '_';
  strcpy(dest + 1, name);

  return dest;
}

void emit_stmt(struct emit_state *state, struct ir_stmt *stmt);

struct compiled_function emit(struct ir_builder *func, struct interval_data intervals) {
  struct aarch64_emitter *emitter;
  create_aarch64_emitter(&emitter);


  struct emit_state state = {
    .arena = func->arena,
    .emitter = emitter,
    .intervals = intervals
  };

  struct ir_stmt *stmt = func->first;
  while (stmt) {
    emit_stmt(&state, stmt);

    stmt = stmt->succ;
  }

  size_t len = aarch64_emit_bytesize(emitter);
  void *data = alloc(func->arena, len);
  aarch64_emit_copy_to(emitter, data);

  free_aarch64_emitter(&emitter);

  struct compiled_function result = {
      .name = mangle(func->arena, func->name), .code = data, .len_code = len};

  return result;
}

void emit_stmt(struct emit_state *state, struct ir_stmt *stmt) {
  struct ir_op *op = stmt->first;
  while (op) {
    trace("lowering op with id %d, type %d", op->id, op->ty);
    switch (op->ty) {
    case IR_OP_TY_CNST: {
      size_t reg = get_reg_for_op(state, op);
      aarch64_emit_load_cnst_32(state->emitter, reg, op->cnst.value);
      break;
    }
    case IR_OP_TY_BINARY_OP: {
      lower_binary_op(state, op);
      break;
    }
    case IR_OP_TY_RET: {
      size_t value_reg = get_reg_for_op(state, op->ret.value);
      invariant_assert(value_reg != UINT32_MAX, "bad IR, no reg");

      if (value_reg != RETURN_REG) {
        aarch64_emit_mov_32(state->emitter, value_reg, RETURN_REG);
      }

      aarch64_emit_ret(state->emitter);
      break;
    }
    default: {
      todo("unsupported IR OP");
      break;
    }
    }

    op = op->succ;
  }
}

