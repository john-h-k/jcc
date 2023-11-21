#include "emit.h"
#include "emitter.h"
#include "isa.h"

struct current_op_state {
  // registers being used by the current instruction, so not possible to use
  unsigned long write_registers;
  unsigned long read_registers;
};

struct emit_state {
  struct arena_allocator *arena;
  struct aarch64_emitter *emitter;

  size_t num_extra_stack_slots;

  // registers that need to be reloaded
  unsigned long need_reload_registers;
  struct current_op_state cur_op_state;
};

unsigned short stack_slot_offset(size_t stack_slot) {
  // currently just uses 4 byte slots
  return (unsigned short)stack_slot * sizeof(int);
}

struct aarch64_reg get_reg_for_idx(size_t idx) {
  // [w|x]18 not available
  struct aarch64_reg reg = {idx < 18 ? idx : idx + 1};
  return reg;
}

bool is_return_reg(size_t idx) { return idx == 0; }

enum reg_usage { REG_USAGE_WRITE = 1, REG_USAGE_READ = 2 };

size_t get_reg_for_op(struct emit_state *state, struct ir_op *op,
                      enum reg_usage usage) {
  size_t reg = op->reg;

  if (usage & REG_USAGE_READ) {
    state->cur_op_state.read_registers |= reg;
  }
  if (usage & REG_USAGE_WRITE) {
    state->cur_op_state.write_registers |= reg;
  }

  invariant_assert(
      reg != REG_SPILLED,
      "spilled reg reached emitter; should've been handled by lower/regalloc");

  invariant_assert(reg < 18, "invalid reg for AArch64");

  return reg;
}

void lower_binary_op(struct emit_state *state, struct ir_op *op) {
  debug_assert(op->ty == IR_OP_TY_BINARY_OP,
               "wrong ty op to `lower_binary_op`");
  size_t reg = get_reg_for_op(state, op, REG_USAGE_WRITE);
  size_t lhs_reg = get_reg_for_op(state, op->binary_op.lhs, REG_USAGE_READ);
  size_t rhs_reg = get_reg_for_op(state, op->binary_op.rhs, REG_USAGE_READ);
  invariant_assert(lhs_reg != UINT32_MAX && rhs_reg != UINT32_MAX,
                   "bad IR, no reg");

  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_ADD:
    aarch64_emit_add_32(state->emitter, get_reg_for_idx(lhs_reg),
                        get_reg_for_idx(rhs_reg), get_reg_for_idx(reg));
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    aarch64_emit_sub_32(state->emitter, get_reg_for_idx(lhs_reg),
                        get_reg_for_idx(rhs_reg), get_reg_for_idx(reg));
    break;
  case IR_OP_BINARY_OP_TY_MUL:
    aarch64_emit_mul_32(state->emitter, get_reg_for_idx(lhs_reg),
                        get_reg_for_idx(rhs_reg), get_reg_for_idx(reg));
    break;
  case IR_OP_BINARY_OP_TY_SDIV:
    aarch64_emit_sdiv_32(state->emitter, get_reg_for_idx(lhs_reg),
                         get_reg_for_idx(rhs_reg), get_reg_for_idx(reg));
    break;
  case IR_OP_BINARY_OP_TY_UDIV:
    aarch64_emit_udiv_32(state->emitter, get_reg_for_idx(lhs_reg),
                         get_reg_for_idx(rhs_reg), get_reg_for_idx(reg));
    break;
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_UQUOT:
    bug("SQUOT and UQUOT should've been lowered to div-msub pair already");
  }
}

const char *mangle(struct arena_allocator *arena, const char *name) {
  char *dest = alloc(arena, strlen(name) + /* null terminator + '_' char */ 2);

  dest[0] = '_';
  strcpy(dest + 1, name);

  return dest;
}

void emit_stmt(struct emit_state *state, struct ir_stmt *stmt,
               size_t stack_size);

struct compiled_function emit(struct ir_builder *func) {
  struct aarch64_emitter *emitter;
  create_aarch64_emitter(&emitter);

  struct emit_state state = {.arena = func->arena,
                             .emitter = emitter,
                             .num_extra_stack_slots = 0,
                             .cur_op_state = {0}};

  size_t stack_size = ROUND_UP(16, func->total_locals_size);
  if (stack_size) {
    // spills, so we need stack space
    aarch64_emit_sub_64_imm(state.emitter, STACK_PTR_REG, stack_size,
                            STACK_PTR_REG);
  }

  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      emit_stmt(&state, stmt, stack_size);

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  size_t len = aarch64_emit_bytesize(emitter);
  void *data = alloc(func->arena, len);
  aarch64_emit_copy_to(emitter, data);

  free_aarch64_emitter(&emitter);

  struct compiled_function result = {
      .name = mangle(func->arena, func->name), .code = data, .len_code = len};

  return result;
}

unsigned get_lcl_stack_offset(struct emit_state *state, size_t lcl_idx) {
  // FIXME: only supports ints
  UNUSED_ARG(state);
  return lcl_idx;
}

void emit_stmt(struct emit_state *state, struct ir_stmt *stmt,
               size_t stack_size) {
  // NOTE: it is important, for branch offset calculations, that each IR operation emits exactly one instruction
  // any expansion needed other than this should have occured in lowering
  
  struct ir_op *op = stmt->first;
  while (op) {
    trace("lowering op with id %d, type %d", op->id, op->ty);
    switch (op->ty) {
    case IR_OP_TY_LOAD_LCL: {
      size_t reg = get_reg_for_op(state, op, REG_USAGE_WRITE);
      aarch64_emit_load_offset_32(
          state->emitter, STACK_PTR_REG, get_reg_for_idx(reg),
          get_lcl_stack_offset(state, op->load_lcl.lcl_idx));
      break;
    }
    case IR_OP_TY_STORE_LCL: {
      size_t reg = get_reg_for_op(state, op->store_lcl.value, REG_USAGE_READ);
      aarch64_emit_store_offset_32(
          state->emitter, STACK_PTR_REG, get_reg_for_idx(reg),
          get_lcl_stack_offset(state, op->store_lcl.lcl_idx));
      break;
    }
    case IR_OP_TY_BR_COND: {
      // we jump to the end of the block + skip this instruction
      size_t offset = op->br_cond.if_false->function_offset - aarch64_emitted_count(state->emitter);
      aarch64_emit_cbz_32_imm(state->emitter, get_reg_for_idx(op->br_cond.cond->reg), offset);
      break;
    }
    case IR_OP_TY_BR: {
      size_t offset = op->br.target->function_offset - aarch64_emitted_count(state->emitter);
      aarch64_emit_b(state->emitter, offset);
      break;
    }
    case IR_OP_TY_CNST: {
      size_t reg = get_reg_for_op(state, op, REG_USAGE_WRITE);
      aarch64_emit_load_cnst_32(state->emitter, get_reg_for_idx(reg),
                                op->cnst.value);
      break;
    }
    case IR_OP_TY_BINARY_OP: {
      lower_binary_op(state, op);
      break;
    }
    case IR_OP_TY_RET: {
      size_t value_reg = get_reg_for_op(state, op->ret.value, REG_USAGE_READ);
      invariant_assert(value_reg != UINT32_MAX, "bad IR, no reg");

      if (!is_return_reg(value_reg)) {
        aarch64_emit_mov_32(state->emitter, get_reg_for_idx(value_reg),
                            RETURN_REG);
      }

      // this should really be handled somewhere else for elegance
      // but this readjusts SP as needed (epilogue)
      if (stack_size) {
        aarch64_emit_add_64_imm(state->emitter, STACK_PTR_REG, stack_size,
                                STACK_PTR_REG);
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
