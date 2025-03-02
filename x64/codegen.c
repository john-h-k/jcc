#include "codegen.h"

#include "../util.h"
#include "../x64.h"

#include <stdio.h>

static void codegen_fprintf(FILE *file, const char *format, ...);

#define MOV_ALIAS(dest_reg, source_reg)                                        \
  (struct x64_instr) {                                                         \
    .ty = X64_INSTR_TY_MOV_REG, .mov_reg = {                                   \
      .dest = (dest_reg),                                                      \
      .source = (source_reg),                                                  \
    }                                                                          \
  }

#define FP_MOV_ALIAS(dest_reg, source_reg)                                     \
  (struct x64_instr) {                                                         \
    .ty = X64_INSTR_TY_MOVAPS, .movaps = {                                     \
      .source = (source_reg),                                                  \
      .dest = (dest_reg),                                                      \
    }                                                                          \
  }

// size_t reg_size(enum x64_reg_ty reg_ty) {
//   switch (reg_ty) {
//   case X64_REG_TY_NONE:
//     BUG("NONE reg ty has no size");
//   case X64_REG_TY_W:
//     return 4;
//   case X64_REG_TY_X:
//     return 8;
//   case X64_REG_TY_V:
//   case X64_REG_TY_Q:
//     return 16;
//   case X64_REG_TY_D:
//     return 8;
//   case X64_REG_TY_S:
//     return 4;
//   case X64_REG_TY_H:
//     return 2;
//   case X64_REG_TY_B:
//     return 1;
//   }
// }

static bool reg_eq(struct x64_reg l, struct x64_reg r) {
  if (l.idx != r.idx) {
    return false;
  }

  if (l.ty == r.ty) {
    return true;
  }

  if (x64_reg_ty_is_gp(l.ty) == x64_reg_ty_is_gp(r.ty)) {
    BUG("comparing two registers with same index and type but different size"
        "(e.g eax vs rax)");
  }

  return false;
}

bool x64_reg_ty_is_gp(enum x64_reg_ty ty) {
  switch (ty) {
  case X64_REG_TY_NONE:
    BUG("doesn't make sense");
  case X64_REG_TY_R:
  case X64_REG_TY_E:
  case X64_REG_TY_W:
  case X64_REG_TY_L:
    return true;
  case X64_REG_TY_XMM:
    return false;
  }
}

bool x64_reg_ty_is_fp(enum x64_reg_ty ty) {
  switch (ty) {
  case X64_REG_TY_NONE:
    BUG("makes no sense");

  case X64_REG_TY_R:
  case X64_REG_TY_E:
  case X64_REG_TY_W:
  case X64_REG_TY_L:
    return false;
  case X64_REG_TY_XMM:
    return true;
  }
}

// enum x64_instr_class instr_class(enum x64_instr_ty ty) {
//   switch (ty) {
//   case X64_INSTR_TY_NOP:
//     return X64_INSTR_CLASS_NOP;
//   case X64_INSTR_TY_AND:
//   case X64_INSTR_TY_ANDS:
//   case X64_INSTR_TY_ORR:
//   case X64_INSTR_TY_ORN:
//   case X64_INSTR_TY_EOR:
//   case X64_INSTR_TY_EON:
//     return X64_INSTR_CLASS_LOGICAL_REG;
//   case X64_INSTR_TY_AND_IMM:
//   case X64_INSTR_TY_ANDS_IMM:
//   case X64_INSTR_TY_ORR_IMM:
//   case X64_INSTR_TY_EOR_IMM:
//     return X64_INSTR_CLASS_LOGICAL_IMM;
//   case X64_INSTR_TY_ADR:
//   case X64_INSTR_TY_ADRP:
//     return X64_INSTR_CLASS_ADDR_IMM;
//   case X64_INSTR_TY_ADD_EXT:
//   case X64_INSTR_TY_ADDS_EXT:
//   case X64_INSTR_TY_SUB_EXT:
//   case X64_INSTR_TY_SUBS_EXT:
//     return X64_INSTR_CLASS_ADDSUB_EXT;
//   case X64_INSTR_TY_ADD:
//   case X64_INSTR_TY_ADDS:
//   case X64_INSTR_TY_SUB:
//   case X64_INSTR_TY_SUBS:
//     return X64_INSTR_CLASS_ADDSUB_REG;
//   case X64_INSTR_TY_ADD_IMM:
//   case X64_INSTR_TY_ADDS_IMM:
//   case X64_INSTR_TY_SUB_IMM:
//   case X64_INSTR_TY_SUBS_IMM:
//     return X64_INSTR_CLASS_ADDSUB_IMM;
//   case X64_INSTR_TY_FCMP:
//     return X64_INSTR_CLASS_FCMP;
//   case X64_INSTR_TY_FCMP_ZERO:
//     return X64_INSTR_CLASS_FCMP_ZERO;
//   case X64_INSTR_TY_FMOV:
//   case X64_INSTR_TY_FNEG:
//   case X64_INSTR_TY_FCVT:
//   case X64_INSTR_TY_UCVTF:
//   case X64_INSTR_TY_SCVTF:
//   case X64_INSTR_TY_FABS:
//   case X64_INSTR_TY_FSQRT:
//     return X64_INSTR_CLASS_REG_1_SOURCE;
//   case X64_INSTR_TY_ASRV:
//   case X64_INSTR_TY_LSLV:
//   case X64_INSTR_TY_LSRV:
//   case X64_INSTR_TY_RORV:
//   case X64_INSTR_TY_SDIV:
//   case X64_INSTR_TY_UDIV:
//   case X64_INSTR_TY_FADD:
//   case X64_INSTR_TY_FSUB:
//   case X64_INSTR_TY_FMUL:
//   case X64_INSTR_TY_FDIV:
//   case X64_INSTR_TY_FMAXNM:
//   case X64_INSTR_TY_FMINNM:
//     return X64_INSTR_CLASS_REG_2_SOURCE;
//   case X64_INSTR_TY_MADD:
//   case X64_INSTR_TY_MSUB:
//     return X64_INSTR_CLASS_FMA;
//   case X64_INSTR_TY_BFM:
//   case X64_INSTR_TY_SBFM:
//   case X64_INSTR_TY_UBFM:
//     return X64_INSTR_CLASS_BITFIELD;
//   case X64_INSTR_TY_CSEL:
//   case X64_INSTR_TY_CSINC:
//   case X64_INSTR_TY_CSINV:
//   case X64_INSTR_TY_CSNEG:
//     return X64_INSTR_CLASS_CONDITIONAL_SELECT;
//   case X64_INSTR_TY_B_COND:
//   case X64_INSTR_TY_BC_COND:
//     return X64_INSTR_CLASS_CONDITIONAL_BRANCH;
//   case X64_INSTR_TY_B:
//   case X64_INSTR_TY_BL:
//     return X64_INSTR_CLASS_BRANCH;
//   case X64_INSTR_TY_RET:
//   case X64_INSTR_TY_BR:
//   case X64_INSTR_TY_BLR:
//     return X64_INSTR_CLASS_BRANCH_REG;
//   case X64_INSTR_TY_CBZ:
//   case X64_INSTR_TY_CBNZ:
//     return X64_INSTR_CLASS_COMPARE_AND_BRANCH;
//   case X64_INSTR_TY_LOAD_IMM:
//   case X64_INSTR_TY_LOAD_BYTE_IMM:
//   case X64_INSTR_TY_LOAD_HALF_IMM:
//     return X64_INSTR_CLASS_LOAD_IMM;
//   case X64_INSTR_TY_STORE_IMM:
//   case X64_INSTR_TY_STORE_BYTE_IMM:
//   case X64_INSTR_TY_STORE_HALF_IMM:
//     return X64_INSTR_CLASS_STORE_IMM;
//   case X64_INSTR_TY_LOAD_PAIR_IMM:
//     return X64_INSTR_CLASS_LOAD_PAIR_IMM;
//   case X64_INSTR_TY_STORE_PAIR_IMM:
//     return X64_INSTR_CLASS_STORE_PAIR_IMM;
//   case X64_INSTR_TY_MOVZ:
//   case X64_INSTR_TY_MOVK:
//   case X64_INSTR_TY_MOVN:
//     return X64_INSTR_CLASS_MOV_IMM;
//   }
// }

// static enum x64_cond invert_cond(enum x64_cond cond) {
//   return cond ^ 1;
// }

struct x64_prologue_info {
  bool prologue_generated;
  size_t stack_size;
  size_t save_start;
};

static enum x64_cond get_cond_for_op(struct ir_op *op) {
  invariant_assert(op->ty == IR_OP_TY_BINARY_OP,
                   "`get_cond_for_op` expects a binary op");

  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_FEQ:
  case IR_OP_BINARY_OP_TY_EQ:
    return X64_COND_ZERO;
  case IR_OP_BINARY_OP_TY_FNEQ:
  case IR_OP_BINARY_OP_TY_NEQ:
    return X64_COND_NOT_ZERO;
  case IR_OP_BINARY_OP_TY_UGT:
    return X64_COND_NOT_BELOW_OR_EQUAL;
  case IR_OP_BINARY_OP_TY_FGT:
  case IR_OP_BINARY_OP_TY_SGT:
    return X64_COND_NOT_LESS_OR_EQUAL;
  case IR_OP_BINARY_OP_TY_UGTEQ:
    return X64_COND_NOT_BELOW;
  case IR_OP_BINARY_OP_TY_FGTEQ:
  case IR_OP_BINARY_OP_TY_SGTEQ:
    return X64_COND_NOT_LESS;
  case IR_OP_BINARY_OP_TY_ULT:
    return X64_COND_BELOW;
  case IR_OP_BINARY_OP_TY_FLT:
  case IR_OP_BINARY_OP_TY_SLT:
    return X64_COND_LESS;
  case IR_OP_BINARY_OP_TY_ULTEQ:
    return X64_COND_BELOW_OR_EQUAL;
  case IR_OP_BINARY_OP_TY_FLTEQ:
  case IR_OP_BINARY_OP_TY_SLTEQ:
    return X64_COND_LESS_OR_EQUAL;
  default:
    BUG("op was not a comparison");
  }
}

static ssize_t get_lcl_stack_offset(const struct codegen_state *state,
                                    const struct ir_lcl *lcl) {
  DEBUG_ASSERT(lcl->alloc_ty != IR_LCL_ALLOC_TY_NONE, "unallocated lcl");

  ssize_t offset = lcl->alloc.offset;

  if (lcl->alloc_ty == IR_LCL_ALLOC_TY_FIXED && offset <= 0 && lcl->flags & IR_LCL_FLAG_PARAM) {
    // add sizeof(void *) for the call address, which is implicitly pushed by `call`
    // and one more for stack pointer
    offset = 8 + state->x64_prologue_info->stack_size + -offset;
  }

  if (lcl->alloc_ty == IR_LCL_ALLOC_TY_NORMAL) {
    offset += state->ir->caller_stack_needed;
  }

  return offset;
}

static size_t translate_reg_idx(size_t idx, enum ir_reg_ty ty) {
  // don't modify without modifying the corresponding values in `x64/lower.c`

  static size_t reg_map[16] = {
      [0] = REG_IDX_DI, [1] = REG_IDX_SI, [2] = REG_IDX_DX,  [3] = REG_IDX_CX,
      [4] = 8,          [5] = 9,          [6] = REG_IDX_AX,  [7] = 10,
      [8] = 11,         [9] = REG_IDX_BX, [10] = 12,         [11] = 13,
      [12] = 14,        [13] = 15,        [14] = REG_IDX_BP, [15] = REG_IDX_SP,
  };

  switch (ty) {
  case IR_REG_TY_NONE:
  case IR_REG_TY_SPILLED:
  case IR_REG_TY_FLAGS:
    BUG("does not make sense for none/spilled/flags");
  case IR_REG_TY_INTEGRAL:
    DEBUG_ASSERT(idx < ARR_LENGTH(reg_map), "invalid reg");
    return reg_map[idx];
  case IR_REG_TY_FP:
    return idx;
  }
}

UNUSED static size_t get_ir_reg_idx(struct x64_reg reg) {
  static size_t reg_map[16] = {
      [REG_IDX_DI] = 0, [REG_IDX_SI] = 1, [REG_IDX_DX] = 2,  [REG_IDX_CX] = 3,
      [8] = 4,          [9] = 5,          [REG_IDX_AX] = 6,  [10] = 7,
      [11] = 8,         [REG_IDX_BX] = 9, [12] = 10,         [13] = 11,
      [14] = 12,        [15] = 13,        [REG_IDX_BP] = 14, [REG_IDX_SP] = 15,
  };

  switch (reg.ty) {
  case X64_REG_TY_NONE:
    BUG("does not make sense");
  case X64_REG_TY_R:
  case X64_REG_TY_E:
  case X64_REG_TY_W:
  case X64_REG_TY_L: {
    DEBUG_ASSERT(reg.idx < ARR_LENGTH(reg_map), "invalid reg");
    size_t idx = reg_map[reg.idx];
    return idx;
  }
  case X64_REG_TY_XMM:
    return reg.idx;
  }
}

// // this is useful for save/restores where you don't know what is live in that
// // reg
// struct x64_reg get_full_reg_for_ir_reg(struct ir_reg reg) {
//   switch (reg.ty) {
//   case IR_REG_TY_NONE:
//   case IR_REG_TY_SPILLED:
//   case IR_REG_TY_FLAGS:
//     BUG("doesn't make sense for none/spilled/flags");
//   case IR_REG_TY_INTEGRAL:
//     return (struct x64_reg){.ty = X64_REG_TY_X,
//                                 .idx = translate_reg_idx(reg.idx, reg.ty)};
//   case IR_REG_TY_FP:
//     // FIXME: this does not support vectors/quad floats
//     return (struct x64_reg){.ty = X64_REG_TY_D,
//                                 .idx = translate_reg_idx(reg.idx, reg.ty)};
//   }
// }

static enum x64_reg_ty reg_ty_for_var_ty(const struct ir_var_ty *var_ty) {
  switch (var_ty->primitive) {
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
    return X64_REG_TY_E;
  case IR_VAR_PRIMITIVE_TY_I64:
    return X64_REG_TY_R;
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    return X64_REG_TY_XMM;
  }
}

static struct x64_reg codegen_reg(struct ir_op *op) {
  size_t idx = translate_reg_idx(op->reg.idx, op->reg.ty);

  if (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE) {
    TODO("non primitives (op %zu)", op->id);
  }

  enum x64_reg_ty reg_ty = reg_ty_for_var_ty(&op->var_ty);

  switch (reg_ty) {
  case X64_REG_TY_NONE:
    BUG("doesn't make sense");
  case X64_REG_TY_R:
  case X64_REG_TY_E:
  case X64_REG_TY_W:
  case X64_REG_TY_L:
    invariant_assert(op->reg.ty == IR_REG_TY_INTEGRAL, "expected integral reg");
    break;
  case X64_REG_TY_XMM:
    invariant_assert(op->reg.ty == IR_REG_TY_FP, "expected fp reg");
    break;
  }

  return (struct x64_reg){.ty = reg_ty, .idx = idx};
}

static void codegen_mov_op(struct codegen_state *state, struct ir_op *op) {
  struct x64_reg dest = codegen_reg(op);

  if (op->mov.value->reg.ty == IR_REG_TY_FLAGS) {
    struct instr *set = alloc_instr(state->func);
    set->x64->ty = X64_INSTR_TY_SETCC;
    set->x64->setcc = (struct x64_conditional_select){
        .dest = dest, .cond = get_cond_for_op(op->mov.value)};

    // TODO: instead of this, we should generate a `xor` _before_ the
    // `cmp`/whatever but we need to make sure no instructions between the `cmp`
    // and this instruction affect flags also consider `movzx`
    struct instr *and = alloc_instr(state->func);
    and->x64->ty = X64_INSTR_TY_AND_IMM;
    and->x64->and_imm = (struct x64_alu_imm){.dest = dest, .imm = 0xF};
    return;
  }

  struct x64_reg source = codegen_reg(op->mov.value);

  struct instr *instr = alloc_instr(state->func);
  if (x64_reg_ty_is_gp(source.ty) && x64_reg_ty_is_gp(dest.ty)) {
    *instr->x64 = MOV_ALIAS(dest, source);
  } else if (x64_reg_ty_is_fp(source.ty) && x64_reg_ty_is_fp(dest.ty)) {
    *instr->x64 = FP_MOV_ALIAS(dest, source);
  } else {
    // one is floating

    switch (op->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_F32:
    case IR_VAR_PRIMITIVE_TY_I32:
      instr->x64->ty = X64_INSTR_TY_MOVD;
      instr->x64->movd = (struct x64_mov_reg){.dest = dest, .source = source};
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
    case IR_VAR_PRIMITIVE_TY_I64:
      instr->x64->ty = X64_INSTR_TY_MOVQ;
      instr->x64->movq = (struct x64_mov_reg){.dest = dest, .source = source};
      break;
    default:
      BUG("unsupported fp type for mov (f16 not available on x64 currently)");
    }
  }
}

// // TODO: we should remove load/store lcl/glb ops and lower all addressing
// // requirements earlier

static enum x64_instr_ty load_ty_for_op(struct ir_op *op) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
    return X64_INSTR_TY_MOVZX_LOAD_BYTE_IMM;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return X64_INSTR_TY_MOVZX_LOAD_HALF_IMM;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F32) {
    return X64_INSTR_TY_MOV_LOAD_SS_IMM;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64) {
    return X64_INSTR_TY_MOV_LOAD_SD_IMM;
  } else {
    return X64_INSTR_TY_MOV_LOAD_IMM;
  }
}

static enum x64_instr_ty store_ty_for_op(struct ir_op *op) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
    return X64_INSTR_TY_MOV_STORE_BYTE_IMM;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return X64_INSTR_TY_MOV_STORE_HALF_IMM;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F32) {
    return X64_INSTR_TY_MOV_STORE_SS_IMM;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64) {
    return X64_INSTR_TY_MOV_STORE_SD_IMM;
  } else {
    return X64_INSTR_TY_MOV_STORE_IMM;
  }
}

static void codegen_load_addr_op(struct codegen_state *state,
                                 struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct x64_reg dest = codegen_reg(op);

  if (op->load.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr = op->load.addr;

    simm_t imm;
    if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
      imm = get_lcl_stack_offset(state, addr->addr.lcl);
    } else {
      BUG("can't CONTAIN operand in load_addr node");
    }

    instr->x64->ty = load_ty_for_op(op);
    instr->x64->mov_load_imm = (struct x64_mov_load_imm){
        .dest = dest, .addr = STACK_PTR_REG, .imm = imm};
  } else {
    struct x64_reg addr = codegen_reg(op->load.addr);
    instr->x64->ty = load_ty_for_op(op);
    instr->x64->mov_load_imm =
        (struct x64_mov_load_imm){.dest = dest, .addr = addr, .imm = 0};
  }
}

static void codegen_store_addr_op(struct codegen_state *state,
                                  struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct x64_reg source = codegen_reg(op->store.value);

  if (op->store.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr = op->store.addr;

    simm_t imm;
    if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
      imm = get_lcl_stack_offset(state, addr->addr.lcl);
    } else {
      BUG("can't CONTAIN operand in store_addr node");
    }

    instr->x64->ty = store_ty_for_op(op->store.value);
    instr->x64->mov_store_imm = (struct x64_mov_store_imm){
        .source = source, .addr = STACK_PTR_REG, .imm = imm};
  } else {
    struct x64_reg addr = codegen_reg(op->store.addr);
    instr->x64->ty = store_ty_for_op(op->store.value);
    instr->x64->mov_store_imm =
        (struct x64_mov_store_imm){.source = source, .addr = addr, .imm = 0};
  }
}

static void codegen_load_op(struct codegen_state *state, struct ir_op *op) {
  DEBUG_ASSERT(op->load.ty == IR_OP_LOAD_TY_ADDR,
               "glb/lcl loads should have been lowered to addr load");

  codegen_load_addr_op(state, op);
}

static void codegen_store_op(struct codegen_state *state, struct ir_op *op) {
  DEBUG_ASSERT(op->store.ty == IR_OP_STORE_TY_ADDR,
               "glb/lcl stores should have been lowered to addr store");

  codegen_store_addr_op(state, op);
}

static void codegen_add_imm(struct codegen_state *state, struct x64_reg dest,
                            struct x64_reg source, unsigned long long value) {

  if (!reg_eq(dest, source)) {
    struct instr *mov = alloc_instr(state->func);
    mov->x64->ty = X64_INSTR_TY_MOV_REG;
    mov->x64->mov_reg = (struct x64_mov_reg){.dest = dest, .source = source};
  }

  struct instr *instr = alloc_instr(state->func);
  instr->x64->ty = X64_INSTR_TY_ADD_IMM;
  instr->x64->add_imm = (struct x64_alu_imm){.dest = dest, .imm = value};
}

static void codegen_sub_imm(struct codegen_state *state, struct x64_reg dest,
                            struct x64_reg source, unsigned long long value) {

  if (!reg_eq(dest, source)) {
    struct instr *mov = alloc_instr(state->func);
    mov->x64->ty = X64_INSTR_TY_MOV_REG;
    mov->x64->mov_reg = (struct x64_mov_reg){.dest = dest, .source = source};
  }

  struct instr *instr = alloc_instr(state->func);
  instr->x64->ty = X64_INSTR_TY_SUB_IMM;
  instr->x64->sub_imm = (struct x64_alu_imm){.dest = dest, .imm = value};
}

static void codegen_addr_offset_op(struct codegen_state *state,
                                   struct ir_op *op) {
  struct x64_reg dest = codegen_reg(op);

  struct ir_op_addr_offset *addr_offset = &op->addr_offset;

  struct x64_reg base = codegen_reg(addr_offset->base);

  struct instr *instr = alloc_instr(state->func);
  instr->x64->ty = X64_INSTR_TY_LEA;

  if (addr_offset->index) {
    DEBUG_ASSERT(popcntl(addr_offset->scale) <= 1,
                 "non pow2 addr offset op should have been lowered");

    instr->x64->lea = (struct x64_lea){.dest = dest,
                                       .base = base,
                                       .index = codegen_reg(addr_offset->index),
                                       .scale = addr_offset->scale,
                                       .offset = addr_offset->offset};
  } else {
    instr->x64->lea = (struct x64_lea){
        .dest = dest, .base = base, .scale = 0, .offset = addr_offset->offset};
  }
}
static void codegen_addr_op(struct codegen_state *state, struct ir_op *op) {
  struct x64_reg dest = codegen_reg(op);

  switch (op->addr.ty) {
  case IR_OP_ADDR_TY_LCL: {
    struct ir_lcl *lcl = op->addr.lcl;

    // op is NULL as we want the absolute offset
    size_t offset = get_lcl_stack_offset(state, lcl);

    codegen_add_imm(state, dest, STACK_PTR_REG, offset);

    break;
  }
  case IR_OP_ADDR_TY_GLB: {
    // TODO: generate one `mov rax, qword ptr [rip]` instr instead of lea/mov pair

    struct ir_glb *glb = op->addr.glb;

    struct instr *addr = alloc_instr(state->func);
    addr->x64->ty = X64_INSTR_TY_LEA_PCREL;
    addr->x64->lea_pcrel = (struct x64_lea_pcrel){.dest = dest, .offset = 0};

    addr->reloc = arena_alloc(state->func->unit->arena, sizeof(*addr->reloc));
    *addr->reloc = (struct relocation){
        .ty = glb->def_ty == IR_GLB_DEF_TY_DEFINED ? RELOCATION_TY_LOCAL_SINGLE
                                                   : RELOCATION_TY_UNDEF_SINGLE,
        .symbol_index = glb->id,
        // offset into current instr
        .address = 3,
        .size = 0};

    if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
      struct instr *load = alloc_instr(state->func);
      load->x64->ty = X64_INSTR_TY_MOV_LOAD_IMM;
      load->x64->mov_load_imm = (struct x64_mov_load_imm){.dest = dest, .addr = dest};
    }
  }
  }
}

static void codegen_br_cond_op(struct codegen_state *state, struct ir_op *op) {
  struct ir_basicblock *true_target = op->stmt->basicblock->split.true_target;
  struct ir_basicblock *false_target = op->stmt->basicblock->split.false_target;

  enum x64_cond cond;
  if (op->br_cond.cond->reg.ty != IR_REG_TY_FLAGS) {
    struct x64_reg cmp_reg = codegen_reg(op->br_cond.cond);

    struct instr *test = alloc_instr(state->func);
    test->x64->ty = X64_INSTR_TY_TEST;
    test->x64->test = (struct x64_cmp){.lhs = cmp_reg, .rhs = cmp_reg};

    cond = X64_COND_NOT_ZERO;
  } else {
    cond = get_cond_for_op(op->br_cond.cond);
  }

  struct instr *instr = alloc_instr(state->func);
  instr->x64->ty = X64_INSTR_TY_JCC;
  instr->x64->jcc =
      (struct x64_conditional_branch){.cond = cond, .target = true_target};

  // now generate the `jmp`
  struct instr *jmp = alloc_instr(state->func);
  jmp->x64->ty = X64_INSTR_TY_JMP;
  jmp->x64->jmp = (struct x64_branch){.target = false_target};
}

static void codegen_br_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  instr->x64->ty = X64_INSTR_TY_JMP;
  instr->x64->jmp =
      (struct x64_branch){.target = op->stmt->basicblock->merge.target};
}

static_assert((sizeof(unsigned long long) == 8) & (sizeof(unsigned short) == 2),
              "type sizes not expected");

union b64 {
  unsigned long long ull;
  double d;
  unsigned short b[4];
};

static void codegen_int(struct codegen_state *state, struct x64_reg dest,
                        union b64 value) {

  struct instr *lo = alloc_instr(state->func);
  lo->x64->ty = X64_INSTR_TY_MOV_IMM;
  lo->x64->mov_imm = (struct x64_mov_imm){.dest = dest, .imm = value.ull};
}

static void codegen_cnst_op(struct codegen_state *state, struct ir_op *op) {
  DEBUG_ASSERT(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
               "expects primitive type");

  struct x64_reg dest = codegen_reg(op);

  switch (op->cnst.ty) {
  case IR_OP_CNST_TY_FLT:
    // currently all constants are lowered to an integer load and `fmov`
    // but lots of constants can be loaded directly, so do that here
    TODO("simple float constants (not lowered)");
  case IR_OP_CNST_TY_INT:
    switch (op->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I8:
    case IR_VAR_PRIMITIVE_TY_I16:
    case IR_VAR_PRIMITIVE_TY_I32:
    case IR_VAR_PRIMITIVE_TY_I64:
      codegen_int(state, dest,
                  (union b64){.ull = (unsigned long long)op->cnst.int_value});
      break;
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_F32:
    case IR_VAR_PRIMITIVE_TY_F64:
      unreachable();
    };
  }
}

#define GET_FP_OP(var_ty, base)                                                \
  (var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64 ? (base##SD) : (base##SS))

static void codegen_unary_op(struct codegen_state *state, struct ir_op *op) {
  struct x64_reg dest = codegen_reg(op);
  struct x64_reg source = codegen_reg(op->unary_op.value);

  if (!reg_eq(source, dest)) {
    struct instr *mov = alloc_instr(state->func);
    if (x64_reg_ty_is_gp(dest.ty)) {
      *mov->x64 =
          (struct x64_instr){.ty = X64_INSTR_TY_MOV_REG,
                             .mov_reg = {.dest = dest, .source = source}};
    } else {
      *mov->x64 = (struct x64_instr){
          .ty = op->unary_op.value->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64
                    ? X64_INSTR_TY_MOVAPD
                    : X64_INSTR_TY_MOVAPS,
          .mov_reg = {.dest = dest, .source = source}};
    }
  }

  struct instr *instr = alloc_instr(state->func);

  switch (op->unary_op.ty) {
  case IR_OP_UNARY_OP_TY_FABS:
  case IR_OP_UNARY_OP_TY_FNEG:
    BUG("should have been removed in lowering");
  case IR_OP_UNARY_OP_TY_FSQRT:
    switch (op->unary_op.value->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_F32:
      instr->x64->ty = X64_INSTR_TY_SQRTSS;
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      instr->x64->ty = X64_INSTR_TY_SQRTSD;
      break;
    default:
      unreachable();
    }

    instr->x64->sqrtss = (struct x64_2_reg_unary){
        .dest = dest,
        .source = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_NEG:
    instr->x64->ty = X64_INSTR_TY_NEG;
    instr->x64->neg = (struct x64_1_reg){
        .dest = dest,
    };
    return;
  case IR_OP_UNARY_OP_TY_NOT:
    instr->x64->ty = X64_INSTR_TY_NOT;
    instr->x64->not = (struct x64_1_reg){
        .dest = dest,
    };
    return;
  default:
    TODO("other x64 unary ops");
  }
}

static void codegen_binary_op(struct codegen_state *state, struct ir_op *op) {
  struct x64_reg dest = {0};

  if (!ir_binary_op_is_comparison(op->binary_op.ty)) {
    dest = codegen_reg(op);
  }

  struct x64_reg lhs = codegen_reg(op->binary_op.lhs);
  struct x64_reg rhs = codegen_reg(op->binary_op.rhs);

  bool is_fp = ir_var_ty_is_fp(&op->var_ty);

  enum ir_op_binary_op_ty ty = op->binary_op.ty;
  DEBUG_ASSERT(
      ty == IR_OP_BINARY_OP_TY_FADD || ty == IR_OP_BINARY_OP_TY_FSUB ||
          ty == IR_OP_BINARY_OP_TY_FMUL || ty == IR_OP_BINARY_OP_TY_FDIV ||
          ty == IR_OP_BINARY_OP_TY_OR || ty == IR_OP_BINARY_OP_TY_AND ||
          ty == IR_OP_BINARY_OP_TY_XOR || !is_fp,
      "floating point with invalid binary op");

  switch (ty) {
  case IR_OP_BINARY_OP_TY_MUL: {
    DEBUG_ASSERT(lhs.idx == REG_IDX_AX, "expected lhs to be in an AX register");
    DEBUG_ASSERT(dest.idx == REG_IDX_AX,
                 "expected dest to be in an DX register");

    struct instr *instr = alloc_instr(state->func);
    instr->x64->ty = X64_INSTR_TY_IMUL;
    instr->x64->mul = (struct x64_mul){
        .rhs = rhs,
    };
    return;
  }
  case IR_OP_BINARY_OP_TY_SDIV: {
    DEBUG_ASSERT(lhs.idx == REG_IDX_AX, "expected lhs to be in an AX register");
    DEBUG_ASSERT(dest.idx == REG_IDX_AX,
                 "expected dest to be in an DX register");

    struct instr *clear_hi = alloc_instr(state->func);
    clear_hi->x64->ty = X64_INSTR_TY_XOR;
    clear_hi->x64->xor = (struct x64_alu_reg){
                           .dest = {.ty = lhs.ty, .idx = REG_IDX_DX},
                           .rhs = {.ty = lhs.ty, .idx = REG_IDX_DX},
                       };

    struct instr *instr = alloc_instr(state->func);
    instr->x64->ty = X64_INSTR_TY_IDIV;
    instr->x64->div = (struct x64_div){
        .rhs = rhs,
    };
    return;
  }
  case IR_OP_BINARY_OP_TY_UDIV: {
    DEBUG_ASSERT(lhs.idx == REG_IDX_AX, "expected lhs to be in an AX register");
    DEBUG_ASSERT(dest.idx == REG_IDX_AX,
                 "expected dest to be in an DX register");

    struct instr *clear_hi = alloc_instr(state->func);
    clear_hi->x64->ty = X64_INSTR_TY_XOR;
    clear_hi->x64->xor = (struct x64_alu_reg){
                           .dest = {.ty = lhs.ty, .idx = REG_IDX_DX},
                           .rhs = {.ty = lhs.ty, .idx = REG_IDX_DX},
                       };

    struct instr *instr = alloc_instr(state->func);
    instr->x64->ty = X64_INSTR_TY_DIV;
    instr->x64->idiv = (struct x64_div){
        .rhs = rhs,
    };
    return;
  }
  case IR_OP_BINARY_OP_TY_SQUOT: {
    DEBUG_ASSERT(lhs.idx == REG_IDX_AX, "expected lhs to be in an AX register");
    DEBUG_ASSERT(dest.idx == REG_IDX_DX,
                 "expected dest to be in an DX register");

    struct instr *clear_hi = alloc_instr(state->func);
    clear_hi->x64->ty = X64_INSTR_TY_XOR;
    clear_hi->x64->xor = (struct x64_alu_reg){
                           .dest = {.ty = X64_REG_TY_R, .idx = REG_IDX_DX},
                           .rhs = {.ty = X64_REG_TY_R, .idx = REG_IDX_DX},
                       };

    struct instr *instr = alloc_instr(state->func);
    instr->x64->ty = X64_INSTR_TY_IDIV;
    instr->x64->div = (struct x64_div){
        .rhs = rhs,
    };
    return;
  }
  case IR_OP_BINARY_OP_TY_UQUOT: {
    DEBUG_ASSERT(lhs.idx == REG_IDX_AX, "expected lhs to be in an AX register");
    DEBUG_ASSERT(dest.idx == REG_IDX_DX,
                 "expected dest to be in an DX register");

    struct instr *clear_hi = alloc_instr(state->func);
    clear_hi->x64->ty = X64_INSTR_TY_XOR;
    clear_hi->x64->xor = (struct x64_alu_reg){
                           .dest = {.ty = X64_REG_TY_R, .idx = REG_IDX_DX},
                           .rhs = {.ty = X64_REG_TY_R, .idx = REG_IDX_DX},
                       };

    struct instr *instr = alloc_instr(state->func);
    instr->x64->ty = X64_INSTR_TY_DIV;
    instr->x64->idiv = (struct x64_div){
        .rhs = rhs,
    };
    return;
  }
  default:
    break;
  }

  if (!ir_binary_op_is_comparison(ty) && !reg_eq(lhs, dest)) {
    struct instr *mov = alloc_instr(state->func);

    if (x64_reg_ty_is_gp(dest.ty)) {
      *mov->x64 = (struct x64_instr){.ty = X64_INSTR_TY_MOV_REG,
                                     .mov_reg = {.dest = dest, .source = lhs}};
    } else {
      *mov->x64 = (struct x64_instr){
          .ty = op->binary_op.lhs->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64
                    ? X64_INSTR_TY_MOVAPD
                    : X64_INSTR_TY_MOVAPS,
          .mov_reg = {.dest = dest, .source = lhs}};
    }
  }

  struct instr *instr = alloc_instr(state->func);

  enum ir_var_primitive_ty primitive = op->binary_op.lhs->var_ty.primitive;
#define GET_BITWISE(integral)                                                  \
  (primitive == IR_VAR_PRIMITIVE_TY_F64   ? (integral##PD)                     \
   : primitive == IR_VAR_PRIMITIVE_TY_F32 ? (integral##PS)                     \
                                          : (integral))
  switch (ty) {
  case IR_OP_BINARY_OP_TY_FEQ:
  case IR_OP_BINARY_OP_TY_FNEQ:
  case IR_OP_BINARY_OP_TY_FGT:
  case IR_OP_BINARY_OP_TY_FGTEQ:
  case IR_OP_BINARY_OP_TY_FLT:
  case IR_OP_BINARY_OP_TY_FLTEQ:
    instr->x64->ty = GET_FP_OP(op->binary_op.lhs->var_ty, X64_INSTR_TY_UCOMI);
    instr->x64->cmp = (struct x64_cmp){
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
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
    instr->x64->ty = X64_INSTR_TY_CMP;
    instr->x64->cmp = (struct x64_cmp){
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_LSHIFT:
    instr->x64->ty = X64_INSTR_TY_SHL;
    instr->x64->shl = (struct x64_shift){
        .dest = dest,
    };
    break;
  case IR_OP_BINARY_OP_TY_SRSHIFT:
    instr->x64->ty = X64_INSTR_TY_SAR;
    instr->x64->sar = (struct x64_shift){
        .dest = dest,
    };
    break;
  case IR_OP_BINARY_OP_TY_URSHIFT:
    instr->x64->ty = X64_INSTR_TY_SHR;
    instr->x64->shr = (struct x64_shift){
        .dest = dest,
    };
    break;
  case IR_OP_BINARY_OP_TY_AND:
    instr->x64->ty = GET_BITWISE(X64_INSTR_TY_AND);
    instr->x64->and = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_OR:
    instr->x64->ty = GET_BITWISE(X64_INSTR_TY_OR);
    instr->x64->or = (struct x64_alu_reg){
                       .dest = dest,
                       .rhs = rhs,
                   };
    break;
  case IR_OP_BINARY_OP_TY_XOR:
    instr->x64->ty = GET_BITWISE(X64_INSTR_TY_XOR);
    instr->x64->xor = (struct x64_alu_reg){
                        .dest = dest,
                        .rhs = rhs,
                    };
    break;
  case IR_OP_BINARY_OP_TY_ADD:
    instr->x64->ty = X64_INSTR_TY_ADD;
    instr->x64->add = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    instr->x64->ty = X64_INSTR_TY_SUB;
    instr->x64->sub = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
    //   case IR_OP_BINARY_OP_TY_FMAX:
    //     instr->x64->ty = X64_INSTR_TY_FMAXNM;
    //     instr->x64->fmaxnm = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_FMIN:
    //     instr->x64->ty = X64_INSTR_TY_FMINNM;
    //     instr->x64->fminnm = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
  case IR_OP_BINARY_OP_TY_FADD:
    instr->x64->ty = GET_FP_OP(op->var_ty, X64_INSTR_TY_ADD);
    instr->x64->alu_reg = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FSUB:
    instr->x64->ty = GET_FP_OP(op->var_ty, X64_INSTR_TY_SUB);
    instr->x64->alu_reg = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMUL:
    instr->x64->ty = GET_FP_OP(op->var_ty, X64_INSTR_TY_MUL);
    instr->x64->alu_reg = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FDIV:
    instr->x64->ty = GET_FP_OP(op->var_ty, X64_INSTR_TY_DIV);
    instr->x64->alu_reg = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
  default:
    TODO("x64 other binops");
  }

#undef GET_BITWISE
}

static void codegen_sext_op(struct codegen_state *state, struct ir_op *op,
                            struct x64_reg source, struct x64_reg dest) {
  struct instr *instr = alloc_instr(state->func);

  invariant_assert(op->cast_op.value->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't sext from non-primitive");

  switch (op->cast_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I8:
    instr->x64->ty = X64_INSTR_TY_MOVSX;
    instr->x64->movsx = (struct x64_mov_reg){.dest = dest, .source = source};
    break;
  case IR_VAR_PRIMITIVE_TY_I16:
    instr->x64->ty = X64_INSTR_TY_MOVSX;
    instr->x64->movsx = (struct x64_mov_reg){.dest = dest, .source = source};
    break;
  case IR_VAR_PRIMITIVE_TY_I32:
    instr->x64->ty = X64_INSTR_TY_MOVSX;
    instr->x64->movsx = (struct x64_mov_reg){.dest = dest, .source = source};
    break;
  case IR_VAR_PRIMITIVE_TY_I64:
    BUG("can't sext from I64");
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    BUG("todo cast floats");
  }
}

static void codegen_zext_op(struct codegen_state *state, struct x64_reg source,
                            struct x64_reg dest) {
  // `mov` will zero top 32 bits
  // do we ever need to explicitly zero extend?

  struct instr *instr = alloc_instr(state->func);
  instr->x64->ty = X64_INSTR_TY_MOV_REG;
  instr->x64->mov_reg = (struct x64_mov_reg){.dest = dest, .source = source};
}

static void codegen_trunc_op(struct codegen_state *state, struct ir_op *op,
                             struct x64_reg source, struct x64_reg dest) {
  invariant_assert(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't truncate non-primitive");

  switch (op->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I8: {
    if (dest.idx != source.idx) {
      struct instr *mov = alloc_instr(state->func);
      mov->x64->ty = X64_INSTR_TY_MOV_REG;
      mov->x64->mov_reg = (struct x64_mov_reg){.dest = dest, .source = source};
    }

    struct instr *instr = alloc_instr(state->func);
    instr->x64->ty = X64_INSTR_TY_AND_IMM;
    instr->x64->and_imm = (struct x64_alu_imm){
        .dest = dest,
        .imm = 0xFF,
    };
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I16: {
    if (dest.idx != source.idx) {
      struct instr *mov = alloc_instr(state->func);
      mov->x64->ty = X64_INSTR_TY_MOV_REG;
      mov->x64->mov_reg = (struct x64_mov_reg){.dest = dest, .source = source};
    }

    struct instr *instr = alloc_instr(state->func);
    instr->x64->ty = X64_INSTR_TY_AND_IMM;
    instr->x64->and_imm = (struct x64_alu_imm){
        .dest = dest,
        .imm = 0xFFFF,
    };
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I32: {
    struct instr *mov = alloc_instr(state->func);
    mov->x64->ty = X64_INSTR_TY_MOV_REG;
    mov->x64->mov_reg = (struct x64_mov_reg){.dest = dest, .source = source};
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I64:
    break;
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    BUG("todo cast floats");
  }
}

static void codegen_conv_op(struct codegen_state *state, struct ir_op *op,
                            struct x64_reg source, struct x64_reg dest) {
  struct instr *instr = alloc_instr(state->func);
  instr->x64->ty = op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64
                       ? X64_INSTR_TY_CVTSS2SD
                       : X64_INSTR_TY_CVTSD2SS;
  instr->x64->two_reg_unary =
      (struct x64_2_reg_unary){.dest = dest, .source = source};
}

static void codegen_uconv_op(struct codegen_state *state, struct ir_op *op,
                             struct x64_reg source, struct x64_reg dest) {
  // FIXME: need to manually handle sign

  struct instr *instr = alloc_instr(state->func);
  if (x64_reg_ty_is_fp(dest.ty)) {
    instr->x64->ty = GET_FP_OP(op->var_ty, X64_INSTR_TY_CVTSI2);
  } else {
    instr->x64->ty =
        op->unary_op.value->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64
            ? X64_INSTR_TY_CVTTSD2SI
            : X64_INSTR_TY_CVTTSS2SI;
  }
  instr->x64->two_reg_unary =
      (struct x64_2_reg_unary){.dest = dest, .source = source};
}

static void codegen_sconv_op(struct codegen_state *state, struct ir_op *op,
                             struct x64_reg source, struct x64_reg dest) {
  struct instr *instr = alloc_instr(state->func);
  if (x64_reg_ty_is_fp(dest.ty)) {
    instr->x64->ty = GET_FP_OP(op->var_ty, X64_INSTR_TY_CVTSI2);
  } else {
    instr->x64->ty =
        op->unary_op.value->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64
            ? X64_INSTR_TY_CVTTSD2SI
            : X64_INSTR_TY_CVTTSS2SI;
  }
  instr->x64->two_reg_unary =
      (struct x64_2_reg_unary){.dest = dest, .source = source};
}

static void codegen_cast_op(struct codegen_state *state, struct ir_op *op) {
  struct x64_reg dest = codegen_reg(op);
  struct x64_reg source = codegen_reg(op->cast_op.value);

  // NOTE: for the integer casts (sext/zext/trunc) we promote the source reg
  // to
  // the same type as the dest reg (mixed regs make no sense in an integer
  // instruction)

  switch (op->cast_op.ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    codegen_sext_op(state, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_ZEXT:
    codegen_zext_op(state, source, dest);
    break;
  case IR_OP_CAST_OP_TY_TRUNC:
    codegen_trunc_op(state, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_CONV:
    codegen_conv_op(state, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_UCONV:
    codegen_uconv_op(state, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_SCONV:
    codegen_sconv_op(state, op, source, dest);
    break;
  }
}

// when generating reg moves, assume memory slots never conflict
// use incrementing values from 128 onwards
#define FIRST_MEM_LOC 128
#define IS_MEM_LOC(v) ((v.idx) >= FIRST_MEM_LOC)
#define MEM_LOC() (last_mem_loc++)

struct mem_loc {
  struct x64_reg base;
  size_t offset;
  size_t size;
};
struct mem_copy {
  struct mem_loc src, dest;
};

enum x64_reg_attr_flags reg_attr_flags(struct x64_reg reg) {
  switch (reg.ty) {
  case X64_REG_TY_NONE:
    return X64_REG_ATTR_FLAG_NONE;
  case X64_REG_TY_R:
  case X64_REG_TY_E:
  case X64_REG_TY_W:
  case X64_REG_TY_L:
    if (!reg.idx) {
      return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_RET_REG;
    }

    switch (reg.idx) {
    case REG_IDX_DI:
    case REG_IDX_SI:
    case REG_IDX_DX:
    case REG_IDX_CX:
    case 8:
    case 9:
      return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG;
    case 10:
    case 11:
      return X64_REG_ATTR_FLAG_VOLATILE;
    case REG_IDX_SP:
    case REG_IDX_BP:
      return X64_REG_ATTR_FLAG_RESERVED;
    }

    return X64_REG_ATTR_FLAG_NONE;
  case X64_REG_TY_XMM:
    if (!reg.idx) {
      return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG |
             X64_REG_ATTR_FLAG_RET_REG;
    }

    if (reg.idx <= 7) {
      return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG;
    }

    return X64_REG_ATTR_FLAG_VOLATILE;
  }
}

// reg 9 is not part of calling convention
// and all registers have already been saved
// so this is always safe
// same with reg 10, which we use if a register branch target is needed for
// `blr`
#define GP_TMP_REG_IDX ((size_t)9)
#define CALL_REG_IDX ((size_t)0)
#define ADDR_REG_IDX ((size_t)11)
#define FP_TMP_REG_IDX ((size_t)16)

// FIXME: apple do things differently!!!

#define INTEGRAL_OR_PTRLIKE(var_ty)                                            \
  (var_ty_is_integral((var_ty)) || (var_ty)->ty == IR_VAR_TY_TY_POINTER ||     \
   (var_ty)->ty == IR_VAR_TY_TY_ARRAY)

static void codegen_call_op(struct codegen_state *state, struct ir_op *op) {
  invariant_assert(op->call.func_ty.ty == IR_VAR_TY_TY_FUNC, "non-func");

  // now we generate the actual call

  struct instr *instr = alloc_instr(state->func);
  if (op->call.target->flags & IR_OP_FLAG_CONTAINED) {
    instr->x64->ty = X64_INSTR_TY_CALL;
    instr->x64->call = (struct x64_branch){.target = NULL};

    instr->reloc = arena_alloc(state->func->unit->arena, sizeof(*instr->reloc));
    *instr->reloc = (struct relocation){
        .ty = RELOCATION_TY_CALL,
        .symbol_index = op->call.target->addr.glb->id,
        .size = 2,
        .address = 1,
    };
  } else {
    instr->x64->ty = X64_INSTR_TY_CALL_REG;
    instr->x64->call_reg =
        (struct x64_branch_reg){.target = codegen_reg(op->call.target)};
  }
}

static void codegen_prologue(struct codegen_state *state) {
  struct ir_func *ir = state->ir;

  size_t num_nonvolatile_used = ir->reg_usage.num_nonvolatile_used;
  bool leaf = !(num_nonvolatile_used || ir->lcl_count ||
                ir->flags & IR_FUNC_FLAG_MAKES_CALL);

  size_t stack_size =
      state->ir->total_locals_size + state->ir->caller_stack_needed;
  stack_size = ROUND_UP(stack_size, X64_STACK_ALIGNMENT);

  struct x64_prologue_info info = {.prologue_generated = !leaf,
                                   .save_start = stack_size,
                                   .stack_size = stack_size};

  state->x64_prologue_info = arena_alloc(state->arena, sizeof(*state->x64_prologue_info));

  if (!info.prologue_generated) {
    *state->x64_prologue_info = info;
    return;
  }

  // save nonvol
  info.stack_size += num_nonvolatile_used * 8;

  // need to save rbp
  info.stack_size += 8;

  struct instr *save_rbp = alloc_instr(state->func);
  save_rbp->x64->ty = X64_INSTR_TY_PUSH;
  save_rbp->x64->push = (struct x64_push){.source = FRAME_PTR_REG};

  // for alignment, we also need to consider the call address pushed, but we don't need to actually sub for this
  info.stack_size = ROUND_UP(info.stack_size + 8, X64_STACK_ALIGNMENT) - 8;

  size_t stack_to_sub = info.stack_size - 8 /* push rbp does 8 bytes for us */;
  if (stack_to_sub) {
    if (stack_to_sub > MAX_IMM_SIZE) {
      codegen_sub_imm(state, STACK_PTR_REG, STACK_PTR_REG, stack_to_sub);
    } else {
      struct instr *sub_stack = alloc_instr(state->func);
      sub_stack->x64->ty = X64_INSTR_TY_SUB_IMM;
      sub_stack->x64->sub_imm =
          (struct x64_alu_imm){.dest = STACK_PTR_REG, .imm = stack_to_sub};
    }

    for (size_t i = 0; i < num_nonvolatile_used; i++) {
      struct ir_reg reg = ir->reg_usage.nonvolatile_used[i];

      // guaranteed to be mod 8
      size_t offset = (info.save_start / 8) + i;

      switch (reg.ty) {
      case IR_REG_TY_INTEGRAL: {
        struct instr *save = alloc_instr(state->func);
        save->x64->ty = X64_INSTR_TY_MOV_STORE_IMM;
        save->x64->mov_store_imm = (struct x64_mov_store_imm){
            .imm = offset * 8,
            .source = (struct x64_reg){.ty = X64_REG_TY_R,
                                       .idx = translate_reg_idx(
                                           reg.idx, IR_REG_TY_INTEGRAL)},
            .addr = STACK_PTR_REG,
        };
        break;
      }
      case IR_REG_TY_FP: {
        struct instr *save = alloc_instr(state->func);
        save->x64->ty = X64_INSTR_TY_MOV_STORE_SD_IMM;
        save->x64->mov_store_sd_imm = (struct x64_mov_store_imm){
            .imm = offset,
            .source = (struct x64_reg){.ty = X64_REG_TY_XMM,
                                       .idx = translate_reg_idx(
                                           reg.idx, IR_REG_TY_INTEGRAL)},
            .addr = STACK_PTR_REG,
        };
        break;
      }
      default:
        BUG("can't save this reg ty");
      }
    }
  }

    *state->x64_prologue_info = info;
}

static void codegen_epilogue(struct codegen_state *state) {
  const struct x64_prologue_info *prologue_info = state->x64_prologue_info;

  if (!prologue_info->prologue_generated) {
    return;
  }

  size_t num_nonvolatile_used = state->ir->reg_usage.num_nonvolatile_used;

  for (size_t i = 0; i < num_nonvolatile_used; i++) {
    struct ir_reg reg = state->ir->reg_usage.nonvolatile_used[i];

    // guaranteed to be mod 8
    size_t offset = (prologue_info->save_start / 8) + i;

    switch (reg.ty) {
    case IR_REG_TY_INTEGRAL: {
      struct instr *restore = alloc_instr(state->func);
      restore->x64->ty = X64_INSTR_TY_MOV_LOAD_IMM;
      restore->x64->mov_load_imm = (struct x64_mov_load_imm){
          .imm = offset * 8,
          .dest =
              (struct x64_reg){.ty = X64_REG_TY_R,
                               .idx = translate_reg_idx(reg.idx, IR_REG_TY_INTEGRAL)},
          .addr = STACK_PTR_REG,
      };
      break;
    }
    case IR_REG_TY_FP: {
      struct instr *restore = alloc_instr(state->func);
      restore->x64->ty = X64_INSTR_TY_MOV_LOAD_SD_IMM;
      restore->x64->mov_load_sd_imm = (struct x64_mov_load_imm){
          .imm = offset,
          .dest =
              (struct x64_reg){.ty = X64_REG_TY_XMM,
                               .idx = translate_reg_idx(reg.idx, IR_REG_TY_INTEGRAL)},
          .addr = STACK_PTR_REG,
      };
      break;
    }
    default:
      BUG("can't restore this reg ty");
    }
  }

  size_t stack_to_add = prologue_info->stack_size - 8 /* pop does 8 bytes for us */;
  if (stack_to_add) {
    codegen_add_imm(state, STACK_PTR_REG, STACK_PTR_REG, stack_to_add);
  }

  struct instr *restore_rbp = alloc_instr(state->func);
  restore_rbp->x64->ty = X64_INSTR_TY_POP;
  restore_rbp->x64->pop = (struct x64_pop){
      .dest = FRAME_PTR_REG,
  };
}

#define INTEGRAL_OR_PTRLIKE(var_ty)                                            \
  (var_ty_is_integral((var_ty)) || (var_ty)->ty == IR_VAR_TY_TY_POINTER ||     \
   (var_ty)->ty == IR_VAR_TY_TY_ARRAY)

static void codegen_ret_op(struct codegen_state *state,
                           UNUSED struct ir_op *op) {
  codegen_epilogue(state);

  struct instr *instr = alloc_instr(state->func);

  instr->x64->ty = X64_INSTR_TY_RET;
}

static void codegen_op(struct codegen_state *state, struct ir_op *op) {
  trace("lowering op with id %zu, type %d", op->id, op->ty);
  switch (op->ty) {
  case IR_OP_TY_UNDF:
  case IR_OP_TY_PHI:
    break;
  case IR_OP_TY_MOV: {
    if (op->flags & IR_OP_FLAG_PARAM) {
      // don't need to do anything
    } else {
      codegen_mov_op(state, op);
    }
    break;
  }
    //   case IR_OP_TY_BITFIELD_INSERT:
    //     codegen_bitfield_insert(state, op);
    //     break;
    //   case IR_OP_TY_BITFIELD_EXTRACT:
    //     codegen_bitfield_extract(state, op);
    //     break;
  case IR_OP_TY_LOAD:
    codegen_load_op(state, op);
    break;
  case IR_OP_TY_STORE:
    codegen_store_op(state, op);
    break;
  case IR_OP_TY_ADDR:
    codegen_addr_op(state, op);
    break;
  case IR_OP_TY_ADDR_OFFSET:
    codegen_addr_offset_op(state, op);
    break;
  case IR_OP_TY_BR_COND:
    codegen_br_cond_op(state, op);
    break;
  case IR_OP_TY_BR:
    codegen_br_op(state, op);
    break;
  case IR_OP_TY_CNST:
    codegen_cnst_op(state, op);
    break;

  case IR_OP_TY_UNARY_OP:
    codegen_unary_op(state, op);
    break;

  case IR_OP_TY_BINARY_OP:
    codegen_binary_op(state, op);
    break;

  case IR_OP_TY_CAST_OP:
    codegen_cast_op(state, op);
    break;
  case IR_OP_TY_CALL:
    codegen_call_op(state, op);
    break;
  case IR_OP_TY_RET:
    codegen_ret_op(state, op);
    break;

  default: {
    TODO("unsupported IR OP '%d'", op->ty);
  }
  }
}

// // static void codegen_nop(struct codegen_state *state) {
// //   struct instr *instr = alloc_instr(state->func);

// //   instr->x64->ty = X64_INSTR_TY_NOP;
// // }

static void codegen_stmt(struct codegen_state *state,
                         const struct ir_stmt *stmt) {
  struct ir_op *op = stmt->first;
  while (op) {
    struct instr *prev = state->func->last;

    if (!(op->flags & IR_OP_FLAG_CONTAINED)) {
      codegen_op(state, op);

      struct instr *start = prev ? prev->succ : NULL;

      while (start) {
        start->op = op;
        start = start->succ;
      }
    }

    op = op->succ;
  }
}

void x64_codegen_start(struct codegen_state *state) {
  codegen_prologue(state);
}

void x64_codegen_basicblock(struct codegen_state *state, struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;

  while (stmt) {
    codegen_stmt(state, stmt);

    stmt = stmt->succ;
  }
}

void x64_codegen_end(UNUSED struct codegen_state *state) {  
}

static void codegen_fprintf(FILE *file, const char *format, ...) {
  static const char *reg_names[] = {"ax", "cx", "dx", "bx",
                                    "sp", "bp", "si", "di"};

  va_list list;
  va_start(list, format);
  while (format[0] != '\0') {
    if (format[0] != '%') {
      fputc(format[0], file);
      format++;
      continue;
    }

    format++;

    if (strncmp(format, "sz_imm_addr", 11) == 0) {
      size_t sz = va_arg(list, size_t);
      struct x64_reg addr = va_arg(list, struct x64_reg);
      imm_t imm = va_arg(list, imm_t);

      switch (sz) {
      case 8:
        fprintf(file, "qword ptr [");
        break;
      case 4:
        fprintf(file, "dword ptr [");
        break;
      case 2:
        fprintf(file, "word ptr [");
        break;
      case 1:
        fprintf(file, "byte ptr [");
        break;
      default:
        BUG("bad size");
      }

      codegen_fprintf(file, "%reg", addr);
      if (imm) {
        codegen_fprintf(file, " + %imm", imm);
      }

      fprintf(file, "]");

      format += 11;
    } else if (strncmp(format, "instr", 5) == 0) {
      struct instr *instr = va_arg(list, struct instr *);
      if (instr) {
        fprintf(file, "%%%zx", instr->id);
      } else {
        fprintf(file, "%%(null)");
      }

      format += 5;
    } else if (strncmp(format, "cond", 4) == 0) {
      enum x64_cond cond = va_arg(list, enum x64_cond);
      switch (cond) {
      case X64_COND_OVERFLOW:
        fprintf(file, "o");
        break;
      case X64_COND_NOT_OVERFLOW:
        fprintf(file, "no");
        break;
      case X64_COND_BELOW:
        fprintf(file, "b");
        break;
      case X64_COND_NOT_BELOW:
        fprintf(file, "nb");
        break;
      case X64_COND_ZERO:
        fprintf(file, "z");
        break;
      case X64_COND_NOT_ZERO:
        fprintf(file, "nz");
        break;
      case X64_COND_BELOW_OR_EQUAL:
        fprintf(file, "be");
        break;
      case X64_COND_NOT_BELOW_OR_EQUAL:
        fprintf(file, "nbe");
        break;
      case X64_COND_SIGN:
        fprintf(file, "s");
        break;
      case X64_COND_NOT_SIGN:
        fprintf(file, "ns");
        break;
      case X64_COND_PARITY:
        fprintf(file, "p");
        break;
      case X64_COND_NOT_PARITY:
        fprintf(file, "np");
        break;
      case X64_COND_LESS:
        fprintf(file, "l");
        break;
      case X64_COND_NOT_LESS:
        fprintf(file, "nl");
        break;
      case X64_COND_LESS_OR_EQUAL:
        fprintf(file, "le");
        break;
      case X64_COND_NOT_LESS_OR_EQUAL:
        fprintf(file, "nle");
        break;
      }

      format += 4;
    } else if (strncmp(format, "reg", 3) == 0) {
      struct x64_reg reg = va_arg(list, struct x64_reg);

      if (format[3] == '8') {
        reg.ty = X64_REG_TY_L;
      }

      switch (reg.ty) {
      case X64_REG_TY_NONE:
        BUG("doesn't make sense");
        break;
      case X64_REG_TY_R:
        if (reg.idx > 7) {
          fprintf(file, "r%zu", reg.idx);
        } else {
          const char *name = reg_names[reg.idx];
          fprintf(file, "r%s", name);
        }
        break;
      case X64_REG_TY_E: {
        if (reg.idx < ARR_LENGTH(reg_names)) {
          const char *name = reg_names[reg.idx];
          fprintf(file, "e%s", name);
        } else {
          fprintf(file, "r%zud", reg.idx);
        }
        break;
      }
      case X64_REG_TY_W: {
        if (reg.idx < ARR_LENGTH(reg_names)) {
          const char *name = reg_names[reg.idx];
          fprintf(file, "%s", name);
        } else {
          fprintf(file, "r%zul", reg.idx);
        }
        break;
      }
      case X64_REG_TY_L: {
        if (reg.idx < ARR_LENGTH(reg_names)) {
          const char *name = reg_names[reg.idx];
          fprintf(file, "%cl", name[0]);
        } else {
          fprintf(file, "r%zub", reg.idx);
        }
        break;
      }
      case X64_REG_TY_XMM: {
        fprintf(file, "xmm%zu", reg.idx);
        break;
      }
      }

      format += format[3] == '8' ? 4 : 3;
    } else if (strncmp(format, "rimm", 4) == 0) {
      ssize_t imm = va_arg(list, ssize_t);
      fprintf(file, "%zd", imm);

      format += 4;
    } else if (strncmp(format, "imm", 3) == 0) {
      ssize_t imm = va_arg(list, ssize_t);
      fprintf(file, "#%zd", imm);

      format += 3;
    } else if (format[0] == '%') {
      fputc('%', file);
      format++;
    } else {
      BUG("unrecognised format starting '%%%s'", format);
    }
  }
}

static void debug_print_shift(FILE *file, const struct x64_shift *shift) {
  codegen_fprintf(file, " %reg, cl", shift->dest);
}

static void debug_print_1_reg(FILE *file, const struct x64_1_reg *one_reg) {
  codegen_fprintf(file, " %reg", one_reg->dest);
}

static void
debug_print_2_reg_unary(FILE *file,
                        const struct x64_2_reg_unary *two_reg_unary) {
  codegen_fprintf(file, " %reg, %reg", two_reg_unary->dest,
                  two_reg_unary->source);
}

static void debug_print_alu_reg(FILE *file, const struct x64_alu_reg *alu_reg) {
  codegen_fprintf(file, " %reg, %reg", alu_reg->dest, alu_reg->rhs);
}

static void debug_print_alu_imm(FILE *file, const struct x64_alu_imm *alu_imm) {
  codegen_fprintf(file, " %reg, %imm", alu_imm->dest, alu_imm->imm);
}

static void debug_print_div(FILE *file, const struct x64_div *div) {
  codegen_fprintf(file, " %reg", div->rhs, div->rhs);
}

static void debug_print_mul(FILE *file, const struct x64_mul *mul) {
  codegen_fprintf(file, " %reg", mul->rhs);
}

static void
debug_print_mov_load_imm(FILE *file, size_t sz,
                         const struct x64_mov_load_imm *mov_load_imm) {
  codegen_fprintf(file, " %reg, %sz_imm_addr", mov_load_imm->dest, sz,
                  mov_load_imm->addr, mov_load_imm->imm);
}

static void
debug_print_mov_store_imm(FILE *file, size_t sz,
                          const struct x64_mov_store_imm *mov_store_imm) {
  codegen_fprintf(file, " %sz_imm_addr, %reg", sz, mov_store_imm->addr,
                  mov_store_imm->imm, mov_store_imm->source);
}

static void debug_print_mov_imm(FILE *file, const struct x64_mov_imm *mov_imm) {
  codegen_fprintf(file, " %reg, %imm", mov_imm->dest, mov_imm->imm);
}

static void debug_print_mov_reg(FILE *file, const struct x64_mov_reg *mov_reg) {
  codegen_fprintf(file, " %reg, %reg", mov_reg->dest, mov_reg->source);
}

static void debug_print_lea_pcrel(FILE *file,
                                  const struct x64_lea_pcrel *lea_pcrel) {
  if (lea_pcrel->offset) {
    codegen_fprintf(file, " %reg, [rip + %rimm]", lea_pcrel->dest,
                    lea_pcrel->offset);
  } else {
    codegen_fprintf(file, " %reg, [rip]", lea_pcrel->dest);
  }
}

static void debug_print_lea(FILE *file, const struct x64_lea *lea) {
  codegen_fprintf(file, " %reg, [%reg", lea->dest, lea->base);

  if (lea->scale == 1) {
    codegen_fprintf(file, " + %reg", lea->index);
  } else if (lea->scale > 1) {
    codegen_fprintf(file, " + %reg*%rimm", lea->index, lea->scale);
  }

  if (lea->offset) {
    codegen_fprintf(file, " + %rimm", lea->offset);
  }

  codegen_fprintf(file, "]");
}

static void debug_print_push(FILE *file, const struct x64_push *push) {
  codegen_fprintf(file, " %reg", push->source);
}

static void debug_print_pop(FILE *file, const struct x64_pop *pop) {
  codegen_fprintf(file, " %reg", pop->dest);
}

static void debug_print_branch(FILE *file, const struct x64_branch *branch) {
  codegen_fprintf(file, " %instr", branch->target->first_instr);
}

static void debug_print_branch_reg(FILE *file,
                                   const struct x64_branch_reg *branch_reg) {
  codegen_fprintf(file, " %reg", branch_reg->target);
}

static void debug_print_conditional_branch(
    FILE *file, const struct x64_conditional_branch *conditional_branch) {
  codegen_fprintf(file, "%cond %instr", conditional_branch->cond,
                  conditional_branch->target->first_instr);
}

static void debug_print_conditional_select(
    FILE *file, const struct x64_conditional_select *conditional_select) {
  codegen_fprintf(file, "%cond %reg8", conditional_select->cond,
                  conditional_select->dest);
}

static void debug_print_cmp(FILE *file, const struct x64_cmp *cmp) {
  codegen_fprintf(file, " %reg, %reg", cmp->lhs, cmp->rhs);
}

static void debug_print_cmp_imm(FILE *file, const struct x64_cmp_imm *cmp_imm) {
  codegen_fprintf(file, " %reg, %imm", cmp_imm->lhs, cmp_imm->imm);
}

void x64_debug_print_instr(FILE *file,
                           UNUSED_ARG(const struct codegen_function *func),
                           const struct instr *instr) {

  switch (instr->x64->ty) {
  case X64_INSTR_TY_MOV_LOAD_SS_IMM:
    fprintf(file, "movss");
    debug_print_mov_load_imm(file, 4, &instr->x64->mov_load_ss_imm);
    break;
  case X64_INSTR_TY_MOV_LOAD_SD_IMM:
    fprintf(file, "movsd");
    debug_print_mov_load_imm(file, 8, &instr->x64->mov_load_sd_imm);
    break;
  case X64_INSTR_TY_MOV_STORE_SS_IMM:
    fprintf(file, "movss");
    debug_print_mov_store_imm(file, 4, &instr->x64->mov_store_ss_imm);
    break;
  case X64_INSTR_TY_MOV_STORE_SD_IMM:
    fprintf(file, "movsd");
    debug_print_mov_store_imm(file, 8, &instr->x64->mov_store_sd_imm);
    break;

  case X64_INSTR_TY_CVTSI2SS:
    fprintf(file, "cvtsi2ss");
    debug_print_2_reg_unary(file, &instr->x64->cvtsi2ss);
    break;
  case X64_INSTR_TY_CVTSI2SD:
    fprintf(file, "cvtsi2sd");
    debug_print_2_reg_unary(file, &instr->x64->cvtsi2sd);
    break;
  case X64_INSTR_TY_CVTTSS2SI:
    fprintf(file, "cvttss2si");
    debug_print_2_reg_unary(file, &instr->x64->cvttss2si);
    break;
  case X64_INSTR_TY_CVTTSD2SI:
    fprintf(file, "cvttsd2si");
    debug_print_2_reg_unary(file, &instr->x64->cvttsd2si);
    break;
  case X64_INSTR_TY_CVTSS2SI:
    fprintf(file, "cvtss2si");
    debug_print_2_reg_unary(file, &instr->x64->cvtss2si);
    break;
  case X64_INSTR_TY_CVTSD2SI:
    fprintf(file, "cvtsd2si");
    debug_print_2_reg_unary(file, &instr->x64->cvtsd2si);
    break;
  case X64_INSTR_TY_CVTSS2SD:
    fprintf(file, "cvtss2sd");
    debug_print_2_reg_unary(file, &instr->x64->cvtss2sd);
    break;
  case X64_INSTR_TY_CVTSD2SS:
    fprintf(file, "cvtsd2ss");
    debug_print_2_reg_unary(file, &instr->x64->cvtsd2ss);
    break;
  case X64_INSTR_TY_ADDSS:
    fprintf(file, "addss");
    debug_print_alu_reg(file, &instr->x64->addss);
    break;
  case X64_INSTR_TY_ADDSD:
    fprintf(file, "addsd");
    debug_print_alu_reg(file, &instr->x64->addsd);
    break;
  case X64_INSTR_TY_SUBSS:
    fprintf(file, "subss");
    debug_print_alu_reg(file, &instr->x64->subss);
    break;
  case X64_INSTR_TY_SUBSD:
    fprintf(file, "subsd");
    debug_print_alu_reg(file, &instr->x64->subsd);
    break;
  case X64_INSTR_TY_MULSS:
    fprintf(file, "mulss");
    debug_print_alu_reg(file, &instr->x64->mulss);
    break;
  case X64_INSTR_TY_MULSD:
    fprintf(file, "mulsd");
    debug_print_alu_reg(file, &instr->x64->mulsd);
    break;
  case X64_INSTR_TY_DIVSS:
    fprintf(file, "divss");
    debug_print_alu_reg(file, &instr->x64->divss);
    break;
  case X64_INSTR_TY_DIVSD:
    fprintf(file, "divsd");
    debug_print_alu_reg(file, &instr->x64->divsd);
    break;
  case X64_INSTR_TY_MOVAPS:
    fprintf(file, "movaps");
    debug_print_2_reg_unary(file, &instr->x64->movaps);
    break;
  case X64_INSTR_TY_MOVAPD:
    fprintf(file, "movapd");
    debug_print_2_reg_unary(file, &instr->x64->movapd);
    break;
  case X64_INSTR_TY_SQRTSS:
    fprintf(file, "sqrtss");
    debug_print_2_reg_unary(file, &instr->x64->sqrtss);
    break;
  case X64_INSTR_TY_SQRTSD:
    fprintf(file, "sqrtsd");
    debug_print_2_reg_unary(file, &instr->x64->sqrtsd);
    break;
  case X64_INSTR_TY_UCOMISS:
    fprintf(file, "ucomiss");
    debug_print_cmp(file, &instr->x64->ucomiss);
    break;
  case X64_INSTR_TY_UCOMISD:
    fprintf(file, "ucomisd");
    debug_print_cmp(file, &instr->x64->ucomisd);
    break;
  case X64_INSTR_TY_ANDPS:
    fprintf(file, "andps");
    debug_print_alu_reg(file, &instr->x64->andps);
    break;
  case X64_INSTR_TY_ANDPD:
    fprintf(file, "andpd");
    debug_print_alu_reg(file, &instr->x64->andpd);
    break;
  case X64_INSTR_TY_XORPS:
    fprintf(file, "xorps");
    debug_print_alu_reg(file, &instr->x64->xorps);
    break;
  case X64_INSTR_TY_XORPD:
    fprintf(file, "xorpd");
    debug_print_alu_reg(file, &instr->x64->xorpd);
    break;
  case X64_INSTR_TY_ORPS:
    fprintf(file, "orps");
    debug_print_alu_reg(file, &instr->x64->orps);
    break;
  case X64_INSTR_TY_ORPD:
    fprintf(file, "orpd");
    debug_print_alu_reg(file, &instr->x64->orpd);
    break;
  case X64_INSTR_TY_SHL:
    fprintf(file, "shl");
    debug_print_shift(file, &instr->x64->shl);
    break;
  case X64_INSTR_TY_SHR:
    fprintf(file, "shr");
    debug_print_shift(file, &instr->x64->shr);
    break;
  case X64_INSTR_TY_SAR:
    fprintf(file, "sar");
    debug_print_shift(file, &instr->x64->sar);
    break;
  case X64_INSTR_TY_MOV_IMM:
    fprintf(file, "mov");
    debug_print_mov_imm(file, &instr->x64->mov_imm);
    break;
  case X64_INSTR_TY_MOV_STORE_IMM:
    fprintf(file, "mov");
    debug_print_mov_store_imm(
        file, instr->x64->mov_store_imm.source.ty == X64_REG_TY_R ? 8 : 4,
        &instr->x64->mov_store_imm);
    break;
  case X64_INSTR_TY_MOV_STORE_HALF_IMM:
    fprintf(file, "mov");
    debug_print_mov_store_imm(file, 2, &instr->x64->mov_store_half_imm);
    break;
  case X64_INSTR_TY_MOV_STORE_BYTE_IMM:
    fprintf(file, "mov");
    debug_print_mov_store_imm(file, 1, &instr->x64->mov_store_byte_imm);
    break;
  case X64_INSTR_TY_MOV_LOAD_IMM:
    fprintf(file, "mov");
    debug_print_mov_load_imm(
        file, instr->x64->mov_load_imm.dest.ty == X64_REG_TY_R ? 8 : 4,
        &instr->x64->mov_load_imm);
    break;
  case X64_INSTR_TY_MOVZX_LOAD_HALF_IMM:
    fprintf(file, "movzx");
    debug_print_mov_load_imm(file, 2, &instr->x64->movzx_load_half_imm);
    break;
  case X64_INSTR_TY_MOVZX_LOAD_BYTE_IMM:
    fprintf(file, "movzx");
    debug_print_mov_load_imm(file, 1, &instr->x64->movzx_load_byte_imm);
    break;
  case X64_INSTR_TY_MOV_REG:
    fprintf(file, "mov");
    debug_print_mov_reg(file, &instr->x64->mov_reg);
    break;
  case X64_INSTR_TY_MOVQ:
    fprintf(file, "movq");
    debug_print_mov_reg(file, &instr->x64->movq);
    break;
  case X64_INSTR_TY_MOVD:
    fprintf(file, "movd");
    debug_print_mov_reg(file, &instr->x64->movd);
    break;
  case X64_INSTR_TY_MOVSX:
    fprintf(file, "movsx");
    debug_print_mov_reg(file, &instr->x64->movsx);
    break;
  case X64_INSTR_TY_ADD:
    fprintf(file, "add");
    debug_print_alu_reg(file, &instr->x64->add);
    break;
  case X64_INSTR_TY_SUB:
    fprintf(file, "sub");
    debug_print_alu_reg(file, &instr->x64->sub);
    break;
  case X64_INSTR_TY_IDIV:
    fprintf(file, "idiv");
    debug_print_div(file, &instr->x64->idiv);
    break;
  case X64_INSTR_TY_DIV:
    fprintf(file, "div");
    debug_print_div(file, &instr->x64->div);
    break;
  case X64_INSTR_TY_IMUL:
    fprintf(file, "imul");
    debug_print_mul(file, &instr->x64->imul);
    break;
  case X64_INSTR_TY_OR:
    fprintf(file, "or");
    debug_print_alu_reg(file, &instr->x64->or);
    break;
  case X64_INSTR_TY_AND:
    fprintf(file, "and");
    debug_print_alu_reg(file, &instr->x64->and);
    break;
  case X64_INSTR_TY_XOR:
    fprintf(file, "xor");
    debug_print_alu_reg(file, &instr->x64->xor);
    break;
  case X64_INSTR_TY_NOT:
    fprintf(file, "not");
    debug_print_1_reg(file, &instr->x64->not );
    break;
  case X64_INSTR_TY_NEG:
    fprintf(file, "neg");
    debug_print_1_reg(file, &instr->x64->neg);
    break;
  case X64_INSTR_TY_PUSH:
    fprintf(file, "push");
    debug_print_push(file, &instr->x64->push);
    break;
  case X64_INSTR_TY_LEA:
    fprintf(file, "lea");
    debug_print_lea(file, &instr->x64->lea);
    break;
  case X64_INSTR_TY_LEA_PCREL:
    fprintf(file, "lea");
    debug_print_lea_pcrel(file, &instr->x64->lea_pcrel);
    break;
  case X64_INSTR_TY_POP:
    fprintf(file, "pop");
    debug_print_pop(file, &instr->x64->pop);
    break;
  case X64_INSTR_TY_ADD_IMM:
    fprintf(file, "add");
    debug_print_alu_imm(file, &instr->x64->add_imm);
    break;
  case X64_INSTR_TY_SUB_IMM:
    fprintf(file, "sub");
    debug_print_alu_imm(file, &instr->x64->sub_imm);
    break;
  case X64_INSTR_TY_AND_IMM:
    fprintf(file, "and");
    debug_print_alu_imm(file, &instr->x64->and_imm);
    break;
  case X64_INSTR_TY_OR_IMM:
    fprintf(file, "or");
    debug_print_alu_imm(file, &instr->x64->or_imm);
    break;
  case X64_INSTR_TY_XOR_IMM:
    fprintf(file, "eor");
    debug_print_alu_imm(file, &instr->x64->xor_imm);
    break;
  case X64_INSTR_TY_RET:
    fprintf(file, "ret");
    break;
  case X64_INSTR_TY_JMP:
    fprintf(file, "jmp");
    debug_print_branch(file, &instr->x64->jmp);
    break;
  case X64_INSTR_TY_CALL:
    fprintf(file, "call");

    if (instr->x64->call.target) {
      debug_print_branch(file, &instr->x64->call);
    } else {
      fprintf(file, " <unknown>");
    }
    break;
  case X64_INSTR_TY_JMP_REG:
    fprintf(file, "jmp");
    debug_print_branch_reg(file, &instr->x64->jmp_reg);
    break;
  case X64_INSTR_TY_CALL_REG:
    fprintf(file, "call");
    debug_print_branch_reg(file, &instr->x64->call_reg);
    break;
  case X64_INSTR_TY_JCC:
    fprintf(file, "j");
    debug_print_conditional_branch(file, &instr->x64->jcc);
    break;
  case X64_INSTR_TY_SETCC:
    fprintf(file, "set");
    debug_print_conditional_select(file, &instr->x64->setcc);
    break;
  case X64_INSTR_TY_CMP:
    fprintf(file, "cmp");
    debug_print_cmp(file, &instr->x64->cmp);
    break;
  case X64_INSTR_TY_TEST:
    fprintf(file, "test");
    debug_print_cmp(file, &instr->x64->test);
    break;
  case X64_INSTR_TY_CMP_IMM:
    fprintf(file, "cmp");
    debug_print_cmp_imm(file, &instr->x64->cmp_imm);
    break;
  case X64_INSTR_TY_TEST_IMM:
    fprintf(file, "test");
    debug_print_cmp_imm(file, &instr->x64->test_imm);
    break;
  }
}

void x64_debug_print_codegen(FILE *file, struct codegen_unit *unit) {
  DEBUG_ASSERT(unit->ty == CODEGEN_UNIT_TY_X64, "expected x64");

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct codegen_entry *entry = &unit->entries[i];

    if (entry->ty != CODEGEN_ENTRY_TY_FUNC) {
      fprintf(file, "DATA: %s\n\n", entry->name);
      continue;
    }

    struct codegen_function *func = &entry->func;

    fprintf(file, "\nFUNCTION: %s\n", entry->name);
    fprintf(file, "  prologue: %s\n", entry->func.prologue ? "true" : "false");
    fprintf(file, "  stack_size: %zu\n", entry->func.stack_size);
    fprintf(file, "\n");

    int op_pad = /* guess */ 50;

    size_t offset = 0;
    struct instr *instr = func->first;
    while (instr) {
      long pos = ftell(file);

      fprintf(file, "%04zu: ", offset++);
      x64_debug_print_instr(file, func, instr);

      if (ftell(file) == pos) {
        // no line was written
        continue;
      }

      long width = ftell(file) - pos;
      long pad = op_pad - width;

      if (pad > 0) {
        fprintf(file, "%*s", (int)pad, "");
      }

      if (instr->op) {
        fprintf(file, "| op = %%%zu", instr->op->id);
      }
      fprintf(file, "\n");

      instr = instr->succ;
    }

    fprintf(file, "\n");
  }
}
