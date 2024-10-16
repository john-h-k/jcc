#include "emit.h"

#include "../aarch64.h"
#include "../bit_twiddle.h"
#include "emitter.h"
#include "isa.h"

struct emit_state {
  struct arena_allocator *arena;
  struct eep_emitter *emitter;

  size_t num_extra_stack_slots;
};

static bool is_return_reg(struct ir_reg reg) { return reg.idx == RETURN_REG.idx; }

struct eep_reg get_reg_for_idx(struct ir_reg reg) {
  return (struct eep_reg){.idx = reg.idx};
}

enum reg_usage { REG_USAGE_WRITE = 1, REG_USAGE_READ = 2 };

static struct ir_reg get_reg_for_op(struct emit_state *state, struct ir_op *op,
                             enum reg_usage usage) {
  UNUSED_ARG(state);
  UNUSED_ARG(usage);

  struct ir_reg reg = op->reg;

  if (reg.ty == IR_REG_TY_FLAGS) {
    return reg;
  }

  // if (usage & REG_USAGE_READ) {
  //   state->cur_op_state.read_registers |= reg;
  // }
  // if (usage & REG_USAGE_WRITE) {
  //   state->cur_op_state.write_registers |= reg;
  // }

  invariant_assert(
      reg.ty != IR_REG_TY_SPILLED,
      "spilled reg reached emitter; should've been handled by lower/regalloc");

  return reg;
}

static void emit_cast_op(struct emit_state *state, struct ir_op *op) {
  UNUSED_ARG(state);
  UNUSED_ARG(op);
  todo("cast on EEP");
}

static void emit_binary_op(struct emit_state *state, struct ir_op *op) {
#define EMIT_WITH_FN(func)                                                     \
  do {                                                                         \
    func(state->emitter, get_reg_for_idx(lhs_reg), get_reg_for_idx(rhs_reg),   \
         get_reg_for_idx(reg));                                                \
  } while (0);

  debug_assert(op->ty == IR_OP_TY_BINARY_OP,
               "wrong ty op to `lower_binary_op`");
  struct ir_reg reg = get_reg_for_op(state, op, REG_USAGE_WRITE);
  struct ir_reg lhs_reg = get_reg_for_op(state, op->binary_op.lhs, REG_USAGE_READ);
  struct ir_reg rhs_reg = get_reg_for_op(state, op->binary_op.rhs, REG_USAGE_READ);

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
    eep_emit_cmp(state->emitter, get_reg_for_idx(lhs_reg),
                 get_reg_for_idx(rhs_reg));
    break;
  case IR_OP_BINARY_OP_TY_LSHIFT:
  case IR_OP_BINARY_OP_TY_SRSHIFT:
  case IR_OP_BINARY_OP_TY_URSHIFT: {
    // FIXME: see lower.c, this is disabled bc sometimes one is needed
    // invariant_assert(rhs_reg == DONT_GIVE_REG, "shift RHS has been mistakenly
    // given a register by allocator even though it must be a constant");
    unsigned shift_imm = op->binary_op.rhs->cnst.int_value;
    if (!UNS_FITS_IN_BITS(shift_imm, 4)) {
      err("shift constant too large for eep! will be truncated");
    }

    switch (op->binary_op.ty) {
    case IR_OP_BINARY_OP_TY_LSHIFT:
      eep_emit_lsl(state->emitter, get_reg_for_idx(lhs_reg),
                   get_reg_for_idx(reg), shift_imm);
      break;
    case IR_OP_BINARY_OP_TY_SRSHIFT:
      eep_emit_asr(state->emitter, get_reg_for_idx(lhs_reg),
                   get_reg_for_idx(reg), shift_imm);
      break;
    case IR_OP_BINARY_OP_TY_URSHIFT:
      eep_emit_lsr(state->emitter, get_reg_for_idx(lhs_reg),
                   get_reg_for_idx(reg), shift_imm);
      break;
    default:
      unreachable("broken switch");
    }

    break;
  }
  case IR_OP_BINARY_OP_TY_AND:
    EMIT_WITH_FN(eep_emit_and);
    break;
  case IR_OP_BINARY_OP_TY_OR:
    todo("eep OR");
    break;
  case IR_OP_BINARY_OP_TY_XOR:
    todo("eep XOR");
    break;
  case IR_OP_BINARY_OP_TY_ADD:
    EMIT_WITH_FN(eep_emit_add);
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    EMIT_WITH_FN(eep_emit_sub);
    break;
  case IR_OP_BINARY_OP_TY_MUL:
  case IR_OP_BINARY_OP_TY_SDIV:
  case IR_OP_BINARY_OP_TY_UDIV:
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_UQUOT:
    bug("SQUOT, UQUOT, SDIV, UDIV, and MUL should've been lowered to already");
  }

#undef SEL_32_OR_64_BIT_OP
}

static void emit_br_op(struct emit_state *state, struct ir_op *op) {
  if (op->stmt->basicblock->ty == IR_BASICBLOCK_TY_MERGE) {
    struct ir_basicblock *target = op->stmt->basicblock->merge.target;
    ssize_t offset = (ssize_t)target->function_offset -
                     (ssize_t)eep_emitted_count(state->emitter);

    eep_emit_jump(state->emitter, EEP_COND_JMP, offset);
  } else {
    // otherwise, this is the false branch of a SPLIT
    struct ir_basicblock *false_target =
        op->stmt->basicblock->split.false_target;

    ssize_t false_offset = (ssize_t)false_target->function_offset -
                           (ssize_t)eep_emitted_count(state->emitter);
    eep_emit_jump(state->emitter, EEP_COND_JMP, false_offset);
  }
}

// not currently used
// static enum eep_cond invert_cond(enum eep_cond cond) {
//   invariant_assert(cond != EEP_COND_RET && cond != EEP_COND_JSR, "can't
//   invert JSR/RET"); return cond ^ 1;
// }

static enum eep_cond get_cond_for_op(struct ir_op *op) {
  invariant_assert(op->ty == IR_OP_TY_BINARY_OP,
                   "`get_cond_for_op` expects a binary op");

  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_EQ:
    return EEP_COND_JEQ;
  case IR_OP_BINARY_OP_TY_NEQ:
    return EEP_COND_JNE;
  case IR_OP_BINARY_OP_TY_UGT:
    return EEP_COND_JHI;
  case IR_OP_BINARY_OP_TY_SGT:
    return EEP_COND_JGT;
  case IR_OP_BINARY_OP_TY_UGTEQ:
    return EEP_COND_JCS;
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return EEP_COND_JGE;
  case IR_OP_BINARY_OP_TY_ULT:
    return EEP_COND_JCC;
  case IR_OP_BINARY_OP_TY_SLT:
    return EEP_COND_JLT;
  case IR_OP_BINARY_OP_TY_ULTEQ:
    return EEP_COND_JLS;
  case IR_OP_BINARY_OP_TY_SLTEQ:
    return EEP_COND_JLE;
  default:
    bug("op was not a comparison");
  }
}

static void emit_mov_op(struct emit_state *state, struct ir_op *op) {
  struct ir_reg dest = get_reg_for_op(state, op, REG_USAGE_WRITE);

  if (op->mov.value->ty == IR_OP_TY_BINARY_OP &&
      binary_op_is_comparison(op->mov.value->binary_op.ty)) {

    // need to move from flags
    enum eep_cond cond = get_cond_for_op(op->mov.value);
    UNUSED_ARG(cond);
    todo("don't know how to extract condition codes to reg");
  } else {
    struct ir_reg src = get_reg_for_op(state, op->mov.value, REG_USAGE_READ);
    eep_emit_mov(state->emitter, get_reg_for_idx(src), get_reg_for_idx(dest));
  }
}

static void emit_br_cond_op(struct emit_state *state, struct ir_op *op) {
  // we jump to the end of the block + skip this
  // instruction
  struct ir_basicblock *true_target = op->stmt->basicblock->split.true_target;
  size_t true_offset =
      true_target->function_offset - eep_emitted_count(state->emitter);

  if (op->br_cond.cond->reg.ty == IR_REG_TY_FLAGS) {
    // emit based on flags
    enum eep_cond cond = get_cond_for_op(op->br_cond.cond);
    eep_emit_jump(state->emitter, cond, true_offset);
  } else {
    invariant_assert(op->br_cond.cond->succ = op,
                     "can't use zeroness of reg unless it was last instr");
    // emit based on zero
    eep_emit_jump(state->emitter, EEP_COND_JNE, true_offset);
  }
}

static void emit_stmt(struct emit_state *state, struct ir_stmt *stmt,
                      size_t stack_size);

struct compiled_function eep_emit_function(struct ir_builder *func) {
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

  struct eep_emitter *emitter;
  create_eep_emitter(&emitter);

  struct emit_state state = {.arena = func->arena,
                             .emitter = emitter,
                             .num_extra_stack_slots = 0,
                             .cur_op_state = {0}};

  size_t stack_size =
      ROUND_UP(func->total_locals_size, AARCH64_STACK_ALIGNMENT);
  if (stack_size) {
    // spills, so we need stack space
    eep_emit_sub_imm(state.emitter, STACK_PTR_REG, stack_size);
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

  size_t len = eep_emit_bytesize(emitter);
  void *data = arena_alloc(func->arena, len);
  eep_emit_copy_to(emitter, data);

  free_eep_emitter(&emitter);

  struct compiled_function result = {
      .name = func->name, .code = data, .len_code = len};

  return result;
}

static unsigned get_lcl_stack_offset(struct emit_state *state,
                                     const struct ir_lcl *lcl) {
  // FIXME: only supports ints
  UNUSED_ARG(state);
  return lcl->id * EEP_REG_SIZE;
}

static void emit_stmt(struct emit_state *state, struct ir_stmt *stmt,
                      size_t stack_size) {
  // NOTE: it is important, for branch offset calculations, that each IR
  // operation emits exactly one instruction any expansion needed other than
  // this should have occured in lowering

  struct ir_op *op = stmt->first;
  while (op) {
    trace("lowering op with id %d, type %d", op->id, op->ty);
    switch (op->ty) {
    case IR_OP_TY_MOV: {
      emit_mov_op(state, op);
      break;
    }
    case IR_OP_TY_PHI: {
      // TODO: ensure everything is where it should be
      // currently we emit a `nop` to keep everything aligned
      // ideally we should remove the phi from IR entirely
      // earlier
      eep_emit_nop(state->emitter);
      break;
    }
    case IR_OP_TY_LOAD_LCL: {
      struct ir_reg reg = get_reg_for_op(state, op, REG_USAGE_WRITE);
      eep_emit_load_offset(state->emitter, STACK_PTR_REG,
                           get_lcl_stack_offset(state, op->lcl),
                           get_reg_for_idx(reg));
      break;
    }
    case IR_OP_TY_STORE_LCL: {
      struct ir_reg reg = get_reg_for_op(state, op->store_lcl.value, REG_USAGE_READ);
      size_t offset = get_lcl_stack_offset(state, op->lcl);
      invariant_assert(UNS_FITS_IN_BITS(offset, 4), "offset too big!");
      eep_emit_store_offset(state->emitter, get_reg_for_idx(reg), STACK_PTR_REG,
                            offset);
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
      struct ir_reg reg = get_reg_for_op(state, op, REG_USAGE_WRITE);

      switch (op->cnst.ty) {
      case IR_OP_CNST_TY_FLT:
        todo("floats");
      case IR_OP_CNST_TY_INT:
        if (reg.ty == IR_REG_TY_NONE) {
          eep_emit_nop(state->emitter);
        } else {
          eep_emit_mov_imm(state->emitter, op->cnst.int_value,
                           get_reg_for_idx(reg));
        }
        break;
      case IR_OP_CNST_TY_STR:
        todo("eep emit str cnst");
        break;
      }
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
    case IR_OP_TY_RET: {
      struct ir_reg value_reg = get_reg_for_op(state, op->ret.value, REG_USAGE_READ);

      if (!is_return_reg(value_reg)) {
        eep_emit_mov(state->emitter, get_reg_for_idx(value_reg), RETURN_REG);
      }

      // this should really be handled somewhere else for
      // elegance but this readjusts SP as needed (epilogue)
      if (stack_size) {
        eep_emit_add_imm(state->emitter, STACK_PTR_REG, stack_size);
      }

      // offset ignored by RET
      eep_emit_jump(state->emitter, EEP_COND_RET, 0);
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
