#include "emit.h"

#include "../aarch64.h"
#include "../ir/var_refs.h"
#include "emitter.h"
#include "isa.h"

struct current_op_state {
  // registers being used by the current instruction, so not possible to use
  unsigned long write_registers;
  unsigned long read_registers;
};

struct emit_state {
  struct ir_builder *irb;
  struct arena_allocator *arena;
  struct aarch64_emitter *emitter;

  size_t num_extra_stack_slots;

  // registers that need to be reloaded
  unsigned long need_reload_registers;
  struct current_op_state cur_op_state;
};

static struct aarch64_reg get_reg_for_idx(size_t idx) {
  // [w|x]18 not available
  struct aarch64_reg reg = {idx < 18 ? idx : idx + 1};
  invariant_assert(reg.idx <= 31, "invalid reg!");
  return reg;
}

enum reg_usage { REG_USAGE_WRITE = 1, REG_USAGE_READ = 2 };

static size_t get_reg_for_op(struct emit_state *state, struct ir_op *op,
                             enum reg_usage usage) {
  size_t reg = op->reg;

  if (reg == REG_FLAGS) {
    return reg;
  }

  if (usage & REG_USAGE_READ) {
    state->cur_op_state.read_registers |= reg;
  }
  if (usage & REG_USAGE_WRITE) {
    state->cur_op_state.write_registers |= reg;
  }

  invariant_assert(
      reg != REG_SPILLED,
      "spilled reg reached emitter; should've been handled by lower/regalloc");

  invariant_assert(reg < 31, "invalid reg '%zu' for AArch64", reg);

  return reg;
}

static bool is_64_bit(const struct ir_op *op) {
  invariant_assert(op->var_ty.ty == IR_OP_VAR_TY_TY_PRIMITIVE,
                   "non-primitive passed to `is_64_bit`");
  return op->var_ty.primitive == IR_OP_VAR_PRIMITIVE_TY_I64;
}

static void emit_call(struct emit_state *state, struct ir_op *op) {
  // FIXME: make a constant
  int offset;
  switch (op->call.target->ty) {
  case IR_OP_TY_GLB: {
    struct var_key key = {.name = op->call.target->glb.global,
                          .scope = SCOPE_GLOBAL};
    struct var_ref *ref = var_refs_get(state->irb->global_var_refs, &key);
    int pos = state->irb->offset + aarch64_emitted_count(state->emitter);
    offset = (int)ref->func->offset - (int)pos;
    break;
  }
  default:
    todo("non GLB calls");
    break;
  }

  aarch64_emit_bl(state->emitter, offset);
}

static void emit_cast_op(struct emit_state *state, struct ir_op *op) {
#define SEL_32_OR_64_BIT_OP(func, immr, imms)                                  \
  do {                                                                         \
    if (is_64_bit(op)) {                                                       \
      func##_64_imm(state->emitter, get_reg_for_idx(src_reg), immr, imms,      \
                    get_reg_for_idx(reg));                                     \
    } else {                                                                   \
      func##_32_imm(state->emitter, get_reg_for_idx(src_reg), immr, imms,      \
                    get_reg_for_idx(reg));                                     \
    }                                                                          \
  } while (0);

  size_t reg = get_reg_for_op(state, op, REG_USAGE_WRITE);
  size_t src_reg = get_reg_for_op(state, op->cast_op.value, REG_USAGE_READ);

  switch (op->cast_op.ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    invariant_assert(op->cast_op.value->var_ty.ty == IR_OP_VAR_TY_TY_PRIMITIVE,
                     "can't sext from non-primitive");

    switch (op->cast_op.value->var_ty.primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      SEL_32_OR_64_BIT_OP(aarch64_emit_sbfm, 0b000000, 0b000111);
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      SEL_32_OR_64_BIT_OP(aarch64_emit_sbfm, 0b000000, 0b001111);
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      SEL_32_OR_64_BIT_OP(aarch64_emit_sbfm, 0b000000, 0b011111);
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      bug("can't sext from I64");
    }
    break;
  case IR_OP_CAST_OP_TY_ZEXT:
    // `mov` zeroes top 32 bits
    aarch64_emit_mov_32(state->emitter, get_reg_for_idx(src_reg),
                        get_reg_for_idx(reg));
    break;
  case IR_OP_CAST_OP_TY_TRUNCATE:
    invariant_assert(op->var_ty.ty == IR_OP_VAR_TY_TY_PRIMITIVE,
                     "can't truncate non-primitive");

    // https://kddnewton.com/2022/08/11/aarch64-bitmask-immediates.html
    // for understanding the immediates
    switch (op->var_ty.primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      aarch64_emit_and_32_imm(state->emitter, get_reg_for_idx(src_reg), 0b0,
                              0b111, get_reg_for_idx(reg));
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      aarch64_emit_and_32_imm(state->emitter, get_reg_for_idx(src_reg), 0b0,
                              0b1111, get_reg_for_idx(reg));
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      // zeroes top 32 bits
      aarch64_emit_mov_32(state->emitter, get_reg_for_idx(src_reg),
                          get_reg_for_idx(reg));
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      break;
    }
  }

#undef SEL_32_OR_64_BIT_OP
}

static void emit_binary_op(struct emit_state *state, struct ir_op *op) {
#define SEL_32_OR_64_BIT_OP(func)                                              \
  do {                                                                         \
    if (is_64_bit(op)) {                                                       \
      func##_64(state->emitter, get_reg_for_idx(lhs_reg),                      \
                get_reg_for_idx(rhs_reg), get_reg_for_idx(reg));               \
    } else {                                                                   \
      func##_32(state->emitter, get_reg_for_idx(lhs_reg),                      \
                get_reg_for_idx(rhs_reg), get_reg_for_idx(reg));               \
    }                                                                          \
  } while (0);

  debug_assert(op->ty == IR_OP_TY_BINARY_OP,
               "wrong ty op to `lower_binary_op`");
  size_t reg = get_reg_for_op(state, op, REG_USAGE_WRITE);
  size_t lhs_reg = get_reg_for_op(state, op->binary_op.lhs, REG_USAGE_READ);
  size_t rhs_reg = get_reg_for_op(state, op->binary_op.rhs, REG_USAGE_READ);
  invariant_assert(lhs_reg != UINT32_MAX && rhs_reg != UINT32_MAX,
                   "bad IR, no reg");

  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_EQ:
  case IR_OP_BINARY_OP_TY_NEQ:
  case IR_OP_BINARY_OP_TY_UGT:
  case IR_OP_BINARY_OP_TY_SGT:
  case IR_OP_BINARY_OP_TY_UGTEQ:
  case IR_OP_BINARY_OP_TY_SGTEQ:
  case IR_OP_BINARY_OP_TY_ULT:
  case IR_OP_BINARY_OP_TY_SLT:
  case IR_OP_BINARY_OP_TY_ULTEQ:
  case IR_OP_BINARY_OP_TY_SLTEQ:
    if (is_64_bit(op)) {
      aarch64_emit_subs_64(state->emitter, get_reg_for_idx(lhs_reg),
                           get_reg_for_idx(rhs_reg), ZERO_REG);
    } else {
      aarch64_emit_subs_32(state->emitter, get_reg_for_idx(lhs_reg),
                           get_reg_for_idx(rhs_reg), ZERO_REG);
    }
    break;
  case IR_OP_BINARY_OP_TY_AND:
    if (is_64_bit(op)) {
      aarch64_emit_and_64(state->emitter, get_reg_for_idx(lhs_reg),
                          get_reg_for_idx(rhs_reg), get_reg_for_idx(reg),
                          SHIFT_LSL, 0);
    } else {
      aarch64_emit_and_32(state->emitter, get_reg_for_idx(lhs_reg),
                          get_reg_for_idx(rhs_reg), get_reg_for_idx(reg),
                          SHIFT_LSL, 0);
    }
    break;
  case IR_OP_BINARY_OP_TY_LSHIFT:
    SEL_32_OR_64_BIT_OP(aarch64_emit_lslv);
    break;
  case IR_OP_BINARY_OP_TY_URSHIFT:
    SEL_32_OR_64_BIT_OP(aarch64_emit_lsrv);
    break;
  case IR_OP_BINARY_OP_TY_SRSHIFT:
    SEL_32_OR_64_BIT_OP(aarch64_emit_asrv);
    break;
  case IR_OP_BINARY_OP_TY_ADD:
    SEL_32_OR_64_BIT_OP(aarch64_emit_add);
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    SEL_32_OR_64_BIT_OP(aarch64_emit_sub);
    break;
  case IR_OP_BINARY_OP_TY_MUL:
    SEL_32_OR_64_BIT_OP(aarch64_emit_mul);
    break;
  case IR_OP_BINARY_OP_TY_SDIV:
    SEL_32_OR_64_BIT_OP(aarch64_emit_sdiv);
    break;
  case IR_OP_BINARY_OP_TY_UDIV:
    SEL_32_OR_64_BIT_OP(aarch64_emit_udiv);
    break;
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_UQUOT:
    bug("SQUOT and UQUOT should've been lowered to div-msub pair already");
  }

#undef SEL_32_OR_64_BIT_OP
}

static const char *mangle(struct arena_allocator *arena, const char *name) {
  char *dest =
      arena_alloc(arena, strlen(name) + /* null terminator + '_' char */ 2);

  dest[0] = '_';
  strcpy(dest + 1, name);

  return dest;
}

void emit_br_op(struct emit_state *state, struct ir_op *op) {
  if (op->stmt->basicblock->ty == IR_BASICBLOCK_TY_MERGE) {
    struct ir_basicblock *target = op->stmt->basicblock->merge.target;
    ssize_t offset = (ssize_t)target->function_offset -
                     (ssize_t)aarch64_emitted_count(state->emitter);
    aarch64_emit_b(state->emitter, offset);
  } else {
    // otherwise, this is the false branch of a SPLIT
    struct ir_basicblock *false_target =
        op->stmt->basicblock->split.false_target;

    ssize_t false_offset = (ssize_t)false_target->function_offset -
                           (ssize_t)aarch64_emitted_count(state->emitter);
    aarch64_emit_b(state->emitter, false_offset);
  }
}

static enum aarch64_cond invert_cond(enum aarch64_cond cond) {
  return cond ^ 1;
}

static enum aarch64_cond get_cond_for_op(struct ir_op *op) {
  invariant_assert(op->ty == IR_OP_TY_BINARY_OP,
                   "`get_cond_for_op` expects a binary op");

  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_EQ:
    return AARCH64_COND_EQ;
  case IR_OP_BINARY_OP_TY_NEQ:
    return AARCH64_COND_NE;
  case IR_OP_BINARY_OP_TY_UGT:
    return AARCH64_COND_HI;
  case IR_OP_BINARY_OP_TY_SGT:
    return AARCH64_COND_GT;
  case IR_OP_BINARY_OP_TY_UGTEQ:
    return AARCH64_COND_HS;
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return AARCH64_COND_GE;
  case IR_OP_BINARY_OP_TY_ULT:
    return AARCH64_COND_LO;
  case IR_OP_BINARY_OP_TY_SLT:
    return AARCH64_COND_LT;
  case IR_OP_BINARY_OP_TY_ULTEQ:
    return AARCH64_COND_LS;
  case IR_OP_BINARY_OP_TY_SLTEQ:
    return AARCH64_COND_LE;
  default:
    bug("op was not a comparison");
  }
}

static void emit_mov_op(struct emit_state *state, struct ir_op *op) {
  size_t dest = get_reg_for_op(state, op, REG_USAGE_WRITE);

  if (op->mov.value->ty == IR_OP_TY_BINARY_OP &&
      binary_op_is_comparison(op->mov.value->binary_op.ty)) {

    // need to move from flags
    enum aarch64_cond cond = get_cond_for_op(op->mov.value);
    // 32 vs 64 bit doesn't matter
    aarch64_emit_csinc_32(state->emitter, invert_cond(cond), ZERO_REG, ZERO_REG,
                          get_reg_for_idx(dest));
  } else {
    size_t src = get_reg_for_op(state, op->mov.value, REG_USAGE_READ);
    aarch64_emit_mov_32(state->emitter, get_reg_for_idx(src),
                        get_reg_for_idx(dest));
  }
}

static void emit_br_cond_op(struct emit_state *state, struct ir_op *op) {
  // we jump to the end of the block + skip this
  // instruction
  struct ir_basicblock *true_target = op->stmt->basicblock->split.true_target;
  size_t true_offset =
      true_target->function_offset - aarch64_emitted_count(state->emitter);

  if (op->br_cond.cond->reg == REG_FLAGS) {
    // emit based on flags
    enum aarch64_cond cond = get_cond_for_op(op->br_cond.cond);
    aarch64_emit_b_cond(state->emitter, true_offset, cond);
  } else {
    // emit based on zero
    aarch64_emit_cbnz_32_imm(
        state->emitter, get_reg_for_idx(op->br_cond.cond->reg), true_offset);
  }
}

static void emit_stmt(struct emit_state *state, struct ir_stmt *stmt);

struct compiled_function aarch64_emit_function(struct ir_builder *func) {
  // the first step of emitting is that we need to ensure the `function_offset`
  // values are correct for all BBs as they may have been broken during various
  // opt/transforming passes
  {
    size_t opc = 0;

    struct ir_basicblock *basicblock = func->first;
    while (basicblock) {
      basicblock->function_offset = opc;
      struct ir_stmt *stmt = basicblock->first;

      while (stmt) {
        struct ir_op *op = stmt->first;
        while (op) {
          opc++;
          op = op->succ;
        }

        stmt = stmt->succ;
      }

      basicblock = basicblock->succ;
    }
  }

  struct aarch64_emitter *emitter;
  create_aarch64_emitter(&emitter);

  struct emit_state state = {.irb = func,
                             .arena = func->arena,
                             .emitter = emitter,
                             .num_extra_stack_slots = 0,
                             .cur_op_state = {0}};

  struct ir_basicblock *basicblock = func->first;
  while (basicblock) {
    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      emit_stmt(&state, stmt);

      stmt = stmt->succ;
    }

    basicblock = basicblock->succ;
  }

  size_t len = aarch64_emit_bytesize(emitter);
  void *data = arena_alloc(func->arena, len);
  aarch64_emit_copy_to(emitter, data);

  free_aarch64_emitter(&emitter);

  struct compiled_function result = {
      .name = mangle(func->arena, func->name), .code = data, .len_code = len};

  return result;
}

static unsigned get_lcl_stack_offset(struct emit_state *state, size_t lcl_idx) {
  UNUSED_ARG(state);
  // FIXME: only supports ints
  // offset by 4 because we store FP/LR at the top
  // this way it is offset enough for 32 bit ints and a bit too much for 64 bit ints
  return lcl_idx + 4;
}

static void emit_custom(struct emit_state *state, struct ir_op *op) {
  // FIXME: the pre-indexing here is causing segfaults for some reason? we
  // should be using it instead of stack when possible
  switch (op->custom.aarch64->ty) {
  case AARCH64_OP_TY_SAVE_LR:
    aarch64_emit_store_pair_pre_index_64(state->emitter, STACK_PTR_REG,
                                         FRAME_PTR_REG, RET_PTR_REG, 0);
    // also save stack pointer into frame pointer as required by ABI
    // `mov x29, sp` is illegal (encodes as `mov x29, xzr`)
    // so `add x29, sp, #0` is used instead
    aarch64_emit_add_64_imm(state->emitter, STACK_PTR_REG, 0, FRAME_PTR_REG);
    break;
  case AARCH64_OP_TY_RSTR_LR:
    aarch64_emit_load_pair_pre_index_64(state->emitter, STACK_PTR_REG,
                                        FRAME_PTR_REG, RET_PTR_REG, 0);
    break;
  case AARCH64_OP_TY_SUB_STACK:
    aarch64_emit_sub_64_imm(state->emitter, STACK_PTR_REG,
                            state->irb->total_locals_size * 4, STACK_PTR_REG);
    break;
  case AARCH64_OP_TY_ADD_STACK:
    aarch64_emit_add_64_imm(state->emitter, STACK_PTR_REG,
                            state->irb->total_locals_size * 4, STACK_PTR_REG);
    break;
  case AARCH64_OP_TY_SAVE_REG:
    aarch64_emit_store_offset_64(state->emitter, STACK_PTR_REG,
                                 get_reg_for_idx(op->reg),
                                 get_lcl_stack_offset(state, op->lcl_idx));
    break;
  case AARCH64_OP_TY_RSTR_REG:
    aarch64_emit_load_offset_64(state->emitter, STACK_PTR_REG,
                                get_reg_for_idx(op->reg),
                                get_lcl_stack_offset(state, op->lcl_idx));
    break;
  }
}

static void emit_stmt(struct emit_state *state, struct ir_stmt *stmt) {
  // NOTE: it is important, for branch offset calculations, that each IR
  // operation emits exactly one instruction any expansion needed other than
  // this should have occured in lowering

  struct ir_op *op = stmt->first;
  while (op) {
    trace("lowering op with id %d, type %d", op->id, op->ty);
    switch (op->ty) {
    case IR_OP_TY_CUSTOM: {
      emit_custom(state, op);
      break;
    }
    case IR_OP_TY_MOV: {
      if (op->flags & IR_OP_FLAG_PARAM) {
        // don't need to do anything, dummy instr
      } else {
        emit_mov_op(state, op);
      }
      break;
    }
    case IR_OP_TY_PHI: {
      // TODO: ensure everything is where it should be
      // currently we emit a `nop` to keep everything aligned
      // ideally we should remove the phi from IR entirely
      // earlier
      aarch64_emit_nop(state->emitter);
      break;
    }
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
      emit_br_cond_op(state, op);
      break;
    }
    case IR_OP_TY_BR: {
      emit_br_op(state, op);
      break;
    }
    case IR_OP_TY_CNST: {
      size_t reg = get_reg_for_op(state, op, REG_USAGE_WRITE);
      aarch64_emit_load_cnst_32(state->emitter, get_reg_for_idx(reg),
                                op->cnst.value);
      break;
    }
    case IR_OP_TY_BINARY_OP: {
      emit_binary_op(state, op);
      break;
    }
    case IR_OP_TY_CAST_OP: {
      emit_cast_op(state, op);
      break;
    }
    case IR_OP_TY_GLB: {
      // TODO:
      aarch64_emit_nop(state->emitter);
      break;
    }
    case IR_OP_TY_CALL: {
      emit_call(state, op);
      break;
    }
    case IR_OP_TY_RET: {
      size_t value_reg = get_reg_for_op(state, op->ret.value, REG_USAGE_READ);
      invariant_assert(value_reg != UINT32_MAX, "bad IR, no reg");

      aarch64_emit_ret(state->emitter);
      break;
    }
    default: {
      todo("unsupported IR OP '%zu'", op->ty);
      break;
    }
    }

    op = op->succ;
  }
}
