#include "codegen.h"

#include "../aarch64.h"
#include "../bit_twiddle.h"
#include "../log.h"
#include "../util.h"

#include <assert.h>
#include <stdio.h>

#define NOT_SP(reg)                                                            \
  (DEBUG_ASSERT(!reg_eq(reg, STACK_PTR_REG),                                   \
                "invalid place for stack ptr reg"),                            \
   reg)

#define MOV_ALIAS(dest_reg, source_reg)                                        \
  (reg_eq((source_reg), STACK_PTR_REG)                                         \
       ? (struct aarch64_instr){.ty = AARCH64_INSTR_TY_ADD_IMM,                \
                                .add_imm = {.source = (source_reg),            \
                                            .dest = (dest_reg),                \
                                            .imm = 0,                          \
                                            .shift =                           \
                                                AARCH64_BINARY_SHIFT_LSL_0}}   \
       : (struct aarch64_instr){.ty = AARCH64_INSTR_TY_ORR,                    \
                                .orr = {.lhs = zero_reg_for_ty(dest_reg.ty),   \
                                        .rhs = NOT_SP((source_reg)),           \
                                        .dest = (dest_reg),                    \
                                        .imm6 = 0}})

#define FP_MOV_ALIAS(dest_reg, source_reg)                                     \
  (struct aarch64_instr) {                                                     \
    .ty = AARCH64_INSTR_TY_FMOV, .fmov = {                                     \
      .source = (source_reg),                                                  \
      .dest = (dest_reg),                                                      \
    }                                                                          \
  }

static enum aarch64_reg_ty gp_reg_ty_for_size(size_t size) {
  switch (size) {
  case 8:
    return AARCH64_REG_TY_X;
  case 4:
    return AARCH64_REG_TY_W;
  default:
    BUG("bad size %zu", size);
  }
}

static enum aarch64_reg_ty fp_reg_ty_for_size(size_t size) {
  switch (size) {
  case 16:
    return AARCH64_REG_TY_V;
  case 8:
    return AARCH64_REG_TY_D;
  case 4:
    return AARCH64_REG_TY_S;
  case 2:
    return AARCH64_REG_TY_H;
  case 1:
    return AARCH64_REG_TY_B;
  default:
    BUG("bad size %zu", size);
  }
}

UNUSED static enum aarch64_reg_ty
reg_ty_for_size(enum aarch64_reg_class reg_class, size_t size) {
  switch (reg_class) {
  case AARCH64_REG_CLASS_GP:
    return gp_reg_ty_for_size(size);
  case AARCH64_REG_CLASS_FP:
    return fp_reg_ty_for_size(size);
  }
}

size_t aarch64_reg_size(enum aarch64_reg_ty reg_ty) {
  switch (reg_ty) {
  case AARCH64_REG_TY_NONE:
    BUG("NONE reg ty has no size");
  case AARCH64_REG_TY_W:
    return 4;
  case AARCH64_REG_TY_X:
    return 8;
  case AARCH64_REG_TY_V:
  case AARCH64_REG_TY_Q:
    return 16;
  case AARCH64_REG_TY_D:
    return 8;
  case AARCH64_REG_TY_S:
    return 4;
  case AARCH64_REG_TY_H:
    return 2;
  case AARCH64_REG_TY_B:
    return 1;
  }
}

static bool is_zero_reg(struct aarch64_reg reg) {
  // this is a bit dodgy as it can also be SP in this context
  return aarch64_reg_ty_is_gp(reg.ty) && reg.idx == 31;
}

static bool reg_eq(struct aarch64_reg l, struct aarch64_reg r) {
  if (l.idx != r.idx) {
    return false;
  }

  if (l.ty == r.ty) {
    return true;
  }

  if (aarch64_reg_ty_is_gp(l.ty) == aarch64_reg_ty_is_gp(r.ty)) {
    BUG("comparing two registers with same index and type but different size "
        "(e.g w0 vs x0)");
  }

  return false;
}

static struct aarch64_reg zero_reg_for_ty(enum aarch64_reg_ty reg_ty) {
  return (struct aarch64_reg){.ty = reg_ty, .idx = 31};
}

bool aarch64_reg_ty_is_gp(enum aarch64_reg_ty ty) {
  switch (ty) {
  case AARCH64_REG_TY_NONE:
    BUG("makes no sense");

  case AARCH64_REG_TY_W:
  case AARCH64_REG_TY_X:
    return true;
  case AARCH64_REG_TY_V:
  case AARCH64_REG_TY_Q:
  case AARCH64_REG_TY_D:
  case AARCH64_REG_TY_S:
  case AARCH64_REG_TY_H:
  case AARCH64_REG_TY_B:
    return false;
  }
}

bool aarch64_reg_ty_is_fp(enum aarch64_reg_ty ty) {
  switch (ty) {
  case AARCH64_REG_TY_NONE:
    BUG("makes no sense");

  case AARCH64_REG_TY_W:
  case AARCH64_REG_TY_X:
    return false;
  case AARCH64_REG_TY_V:
  case AARCH64_REG_TY_Q:
  case AARCH64_REG_TY_D:
  case AARCH64_REG_TY_S:
  case AARCH64_REG_TY_H:
  case AARCH64_REG_TY_B:
    return true;
  }
}

enum aarch64_instr_class instr_class(enum aarch64_instr_ty ty) {
  switch (ty) {
  case AARCH64_INSTR_TY_NOP:
    return AARCH64_INSTR_CLASS_NOP;
  case AARCH64_INSTR_TY_AND:
  case AARCH64_INSTR_TY_ANDS:
  case AARCH64_INSTR_TY_ORR:
  case AARCH64_INSTR_TY_ORN:
  case AARCH64_INSTR_TY_EOR:
  case AARCH64_INSTR_TY_EON:
    return AARCH64_INSTR_CLASS_LOGICAL_REG;
  case AARCH64_INSTR_TY_AND_IMM:
  case AARCH64_INSTR_TY_ANDS_IMM:
  case AARCH64_INSTR_TY_ORR_IMM:
  case AARCH64_INSTR_TY_EOR_IMM:
    return AARCH64_INSTR_CLASS_LOGICAL_IMM;
  case AARCH64_INSTR_TY_ADR:
  case AARCH64_INSTR_TY_ADRP:
    return AARCH64_INSTR_CLASS_ADDR_IMM;
  case AARCH64_INSTR_TY_ADD_EXT:
  case AARCH64_INSTR_TY_ADDS_EXT:
  case AARCH64_INSTR_TY_SUB_EXT:
  case AARCH64_INSTR_TY_SUBS_EXT:
    return AARCH64_INSTR_CLASS_ADDSUB_EXT;
  case AARCH64_INSTR_TY_ADD:
  case AARCH64_INSTR_TY_ADDS:
  case AARCH64_INSTR_TY_SUB:
  case AARCH64_INSTR_TY_SUBS:
    return AARCH64_INSTR_CLASS_ADDSUB_REG;
  case AARCH64_INSTR_TY_ADD_IMM:
  case AARCH64_INSTR_TY_ADDS_IMM:
  case AARCH64_INSTR_TY_SUB_IMM:
  case AARCH64_INSTR_TY_SUBS_IMM:
    return AARCH64_INSTR_CLASS_ADDSUB_IMM;
  case AARCH64_INSTR_TY_FCMP:
    return AARCH64_INSTR_CLASS_FCMP;
  case AARCH64_INSTR_TY_FCMP_ZERO:
    return AARCH64_INSTR_CLASS_FCMP_ZERO;
  case AARCH64_INSTR_TY_FMOV:
  case AARCH64_INSTR_TY_FNEG:
  case AARCH64_INSTR_TY_FCVT:
  case AARCH64_INSTR_TY_UCVTF:
  case AARCH64_INSTR_TY_SCVTF:
  case AARCH64_INSTR_TY_FABS:
  case AARCH64_INSTR_TY_FSQRT:
  case AARCH64_INSTR_TY_CLZ:
  case AARCH64_INSTR_TY_RBIT:
  case AARCH64_INSTR_TY_REV:
  case AARCH64_INSTR_TY_REV16:
    return AARCH64_INSTR_CLASS_REG_1_SOURCE;
  case AARCH64_INSTR_TY_CNT:
  case AARCH64_INSTR_TY_ADDV:
    return AARCH64_INSTR_CLASS_VREG_1_SOURCE;
  case AARCH64_INSTR_TY_ASRV:
  case AARCH64_INSTR_TY_LSLV:
  case AARCH64_INSTR_TY_LSRV:
  case AARCH64_INSTR_TY_RORV:
  case AARCH64_INSTR_TY_SDIV:
  case AARCH64_INSTR_TY_UDIV:
  case AARCH64_INSTR_TY_FADD:
  case AARCH64_INSTR_TY_FSUB:
  case AARCH64_INSTR_TY_FMUL:
  case AARCH64_INSTR_TY_FDIV:
  case AARCH64_INSTR_TY_FMAXNM:
  case AARCH64_INSTR_TY_FMINNM:
    return AARCH64_INSTR_CLASS_REG_2_SOURCE;
  case AARCH64_INSTR_TY_MADD:
  case AARCH64_INSTR_TY_MSUB:
    return AARCH64_INSTR_CLASS_FMA;
  case AARCH64_INSTR_TY_BFM:
  case AARCH64_INSTR_TY_SBFM:
  case AARCH64_INSTR_TY_UBFM:
    return AARCH64_INSTR_CLASS_BITFIELD;
  case AARCH64_INSTR_TY_CSEL:
  case AARCH64_INSTR_TY_CSINC:
  case AARCH64_INSTR_TY_CSINV:
  case AARCH64_INSTR_TY_CSNEG:
    return AARCH64_INSTR_CLASS_CONDITIONAL_SELECT;
  case AARCH64_INSTR_TY_B_COND:
  case AARCH64_INSTR_TY_BC_COND:
    return AARCH64_INSTR_CLASS_CONDITIONAL_BRANCH;
  case AARCH64_INSTR_TY_B:
  case AARCH64_INSTR_TY_BL:
    return AARCH64_INSTR_CLASS_BRANCH;
  case AARCH64_INSTR_TY_RET:
  case AARCH64_INSTR_TY_BR:
  case AARCH64_INSTR_TY_BLR:
    return AARCH64_INSTR_CLASS_BRANCH_REG;
  case AARCH64_INSTR_TY_CBZ:
  case AARCH64_INSTR_TY_CBNZ:
    return AARCH64_INSTR_CLASS_COMPARE_AND_BRANCH;
  case AARCH64_INSTR_TY_LOAD_IMM:
  case AARCH64_INSTR_TY_LOAD_BYTE_IMM:
  case AARCH64_INSTR_TY_LOAD_HALF_IMM:
    return AARCH64_INSTR_CLASS_LOAD_IMM;
  case AARCH64_INSTR_TY_STORE_IMM:
  case AARCH64_INSTR_TY_STORE_BYTE_IMM:
  case AARCH64_INSTR_TY_STORE_HALF_IMM:
    return AARCH64_INSTR_CLASS_STORE_IMM;
  case AARCH64_INSTR_TY_LOAD:
  case AARCH64_INSTR_TY_LOAD_BYTE:
  case AARCH64_INSTR_TY_LOAD_HALF:
    return AARCH64_INSTR_CLASS_LOAD;
  case AARCH64_INSTR_TY_STORE:
  case AARCH64_INSTR_TY_STORE_BYTE:
  case AARCH64_INSTR_TY_STORE_HALF:
    return AARCH64_INSTR_CLASS_STORE;
  case AARCH64_INSTR_TY_LOAD_PAIR_IMM:
    return AARCH64_INSTR_CLASS_LOAD_PAIR_IMM;
  case AARCH64_INSTR_TY_STORE_PAIR_IMM:
    return AARCH64_INSTR_CLASS_STORE_PAIR_IMM;
  case AARCH64_INSTR_TY_MOVZ:
  case AARCH64_INSTR_TY_MOVK:
  case AARCH64_INSTR_TY_MOVN:
    return AARCH64_INSTR_CLASS_MOV_IMM;
  }
}

static enum aarch64_cond invert_cond(enum aarch64_cond cond) {
  return cond ^ 1;
}

struct aarch64_prologue_info {
  bool prologue_generated;
  size_t stack_size;
  size_t lr_offset;
  size_t save_start;
};

static enum aarch64_cond get_cond_for_op(struct ir_op *op) {
  invariant_assert(op->ty == IR_OP_TY_BINARY_OP,
                   "`get_cond_for_op` expects a binary op");

  switch (op->binary_op.ty) {
  case IR_OP_BINARY_OP_TY_FEQ:
  case IR_OP_BINARY_OP_TY_EQ:
    return AARCH64_COND_EQ;
  case IR_OP_BINARY_OP_TY_FNEQ:
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
  case IR_OP_BINARY_OP_TY_FLT:
    return AARCH64_COND_MI;
  case IR_OP_BINARY_OP_TY_FGT:
    return AARCH64_COND_GT;
  case IR_OP_BINARY_OP_TY_FLTEQ:
    return AARCH64_COND_LS;
  case IR_OP_BINARY_OP_TY_FGTEQ:
    return AARCH64_COND_GE;
  default:
    BUG("op was not a comparison");
  }
}

static ssize_t get_rel_stack_offset(const struct cg_state *state,
                                    ssize_t offset) {
  offset += state->ir->caller_stack_needed;

  if (!state->aarch64_prologue_info->prologue_generated) {
    // using red zone
    offset = (ssize_t)state->aarch64_prologue_info->stack_size - offset;
  }

  return offset;
}

static ssize_t get_lcl_stack_offset(const struct cg_state *state,
                                    const struct ir_op *op,
                                    const struct ir_lcl *lcl) {
  DEBUG_ASSERT(lcl->alloc_ty != IR_LCL_ALLOC_TY_NONE, "unallocated lcl");

  ssize_t offset = lcl->alloc.offset;

  if (lcl->alloc_ty == IR_LCL_ALLOC_TY_FIXED && offset <= 0 &&
      lcl->flags & IR_LCL_FLAG_PARAM) {
    offset = state->aarch64_prologue_info->stack_size + -offset;
  }

  if (lcl->alloc_ty == IR_LCL_ALLOC_TY_NORMAL) {
    offset = get_rel_stack_offset(state, offset);
  }

  if (!op) {
    return offset;
  }

  struct ir_var_ty_info info = ir_var_ty_info(state->ir->unit, &op->var_ty);
  DEBUG_ASSERT(offset % info.size == 0,
               "stack offset not divisible by type size");

  return offset / (ssize_t)info.size;
}

static size_t translate_reg_idx(size_t idx, enum ir_reg_ty ty) {
  switch (ty) {
  case IR_REG_TY_NONE:
  case IR_REG_TY_SPILLED:
  case IR_REG_TY_FLAGS:
    BUG("does not make sense for none/spilled/flags");
  case IR_REG_TY_INTEGRAL:
    return idx < 18 ? idx : idx + 1;
  case IR_REG_TY_FP:
    return idx >= 24 ? (idx - 24 + 8) : idx;
  }
}

// this is useful for save/restores where you don't know what is live in that
// reg
UNUSED static struct aarch64_reg get_full_reg_for_ir_reg(struct ir_reg reg) {
  switch (reg.ty) {
  case IR_REG_TY_NONE:
  case IR_REG_TY_SPILLED:
  case IR_REG_TY_FLAGS:
    BUG("doesn't make sense for none/spilled/flags");
  case IR_REG_TY_INTEGRAL:
    return (struct aarch64_reg){.ty = AARCH64_REG_TY_X,
                                .idx = translate_reg_idx(reg.idx, reg.ty)};
  case IR_REG_TY_FP:
    // FIXME: this does not support vectors/quad floats
    return (struct aarch64_reg){.ty = AARCH64_REG_TY_D,
                                .idx = translate_reg_idx(reg.idx, reg.ty)};
  }
}

static enum aarch64_reg_ty reg_ty_for_var_ty(const struct ir_var_ty *var_ty) {
  switch (var_ty->primitive) {
  case IR_VAR_PRIMITIVE_TY_I1:
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
    return AARCH64_REG_TY_W;
  case IR_VAR_PRIMITIVE_TY_I64:
    return AARCH64_REG_TY_X;
  case IR_VAR_PRIMITIVE_TY_F16:
    return AARCH64_REG_TY_H;
  case IR_VAR_PRIMITIVE_TY_F32:
    return AARCH64_REG_TY_S;
  case IR_VAR_PRIMITIVE_TY_F64:
    return AARCH64_REG_TY_D;
  case IR_VAR_PRIMITIVE_TY_I128:
    TODO("codegen uint128");
  }
}

static struct aarch64_reg codegen_reg(struct ir_op *op) {
  if (op->ty == IR_OP_TY_CNST && (op->flags & IR_OP_FLAG_CONTAINED) &&
      op->cnst.int_value == 0) {
    return zero_reg_for_ty(reg_ty_for_var_ty(&op->var_ty));
  }

  DEBUG_ASSERT(!(op->flags & IR_OP_FLAG_CONTAINED),
               "contained ops have no reg");

  size_t idx = translate_reg_idx(op->reg.idx, op->reg.ty);

  if (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE) {
    TODO("non primitives (op %zu)", op->id);
  }

  enum aarch64_reg_ty reg_ty = reg_ty_for_var_ty(&op->var_ty);

  switch (reg_ty) {
  case AARCH64_REG_TY_W:
  case AARCH64_REG_TY_X:
    invariant_assert(op->reg.ty == IR_REG_TY_INTEGRAL, "expected integral reg");
    break;
  case AARCH64_REG_TY_H:
  case AARCH64_REG_TY_S:
  case AARCH64_REG_TY_D:
    invariant_assert(op->reg.ty == IR_REG_TY_FP, "expected fp reg");
    break;
  default:
    TODO("other reg tys (Q/V)");
  }

  return (struct aarch64_reg){.ty = reg_ty, .idx = idx};
}

static bool cond_is_com(enum aarch64_cond cond) {
  switch (cond) {
  case AARCH64_COND_EQ:
  case AARCH64_COND_NE:
    return true;
  case AARCH64_COND_AL:
  case AARCH64_COND_AL_ALT:
  case AARCH64_COND_GE:
  case AARCH64_COND_LT:
  case AARCH64_COND_GT:
  case AARCH64_COND_LE:
  case AARCH64_COND_VS:
  case AARCH64_COND_VC:
  case AARCH64_COND_HI:
  case AARCH64_COND_LS:
  case AARCH64_COND_HS:
  case AARCH64_COND_LO:
  case AARCH64_COND_MI:
  case AARCH64_COND_PL:
    return false;
  }
}

static void codegen_mov_op(struct cg_state *state,
                           struct cg_basicblock *basicblock, struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);

  if (op->mov.value->reg.ty == IR_REG_TY_FLAGS) {
    struct aarch64_reg zero_reg = zero_reg_for_ty(dest.ty);

    enum aarch64_cond cond = get_cond_for_op(op->mov.value);
    if (op->mov.value->ty == IR_OP_TY_BINARY_OP &&
        (op->br_cond.cond->binary_op.lhs->flags & IR_OP_FLAG_CONTAINED &&
         !cond_is_com(cond))) {
      // because of immediate, we did `subs rhs, lhs_imm`, so invert cond
      cond = invert_cond(cond);
    }

    struct cg_instr *csinc = cg_alloc_instr(state->func, basicblock);
    csinc->aarch64->ty = AARCH64_INSTR_TY_CSINC;
    csinc->aarch64->csinc = (struct aarch64_conditional_select){
        .dest = dest,
        .cond = invert_cond(cond),
        .true_source = zero_reg,
        .false_source = zero_reg,
    };

    return;
  }

  struct aarch64_reg source = codegen_reg(op->mov.value);

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  if (aarch64_reg_ty_is_gp(source.ty) && aarch64_reg_ty_is_gp(dest.ty)) {
    if (op->flags & IR_OP_FLAG_PHI_MOV) {
      dest.ty = AARCH64_REG_TY_X;
      source.ty = AARCH64_REG_TY_X;
    }

    *instr->aarch64 = MOV_ALIAS(dest, source);
  } else {
    if (op->flags & IR_OP_FLAG_PHI_MOV) {
      dest.ty = AARCH64_REG_TY_D;
      source.ty = AARCH64_REG_TY_D;
    }
    // one is floating
    *instr->aarch64 = FP_MOV_ALIAS(dest, source);
  }
}

// TODO: we should remove load/store lcl/glb ops and lower all addressing
// requirements earlier

enum addr_mode {
  ADDR_MODE_IMM,
  ADDR_MODE_REG,
};

static enum aarch64_instr_ty load_ty_for_op(struct ir_op *op,
                                            enum addr_mode mode) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      (op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8 ||
       op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I1)) {
    return mode == ADDR_MODE_IMM ? AARCH64_INSTR_TY_LOAD_BYTE_IMM
                                 : AARCH64_INSTR_TY_LOAD_BYTE;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return mode == ADDR_MODE_IMM ? AARCH64_INSTR_TY_LOAD_HALF_IMM
                                 : AARCH64_INSTR_TY_LOAD_HALF;
  } else {
    return mode == ADDR_MODE_IMM ? AARCH64_INSTR_TY_LOAD_IMM
                                 : AARCH64_INSTR_TY_LOAD;
  }
}

static enum aarch64_instr_ty store_ty_for_op(struct ir_op *op,
                                             enum addr_mode mode) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      (op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8 ||
       op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I1)) {
    return mode == ADDR_MODE_IMM ? AARCH64_INSTR_TY_STORE_BYTE_IMM
                                 : AARCH64_INSTR_TY_STORE_BYTE;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return mode == ADDR_MODE_IMM ? AARCH64_INSTR_TY_STORE_HALF_IMM
                                 : AARCH64_INSTR_TY_STORE_HALF;
  } else {
    return mode == ADDR_MODE_IMM ? AARCH64_INSTR_TY_STORE_IMM
                                 : AARCH64_INSTR_TY_STORE;
  }
}

enum folded_addr_op_ty {
  FOLDED_ADDR_OP_TY_IMM,
  FOLDED_ADDR_OP_TY_INDEX,
};

struct folded_addr_index {
  // either `base + index` or `base + (index * op_size)`

  struct aarch64_reg index;
  bool shift_op_size;
};

struct folded_addr_op {
  enum folded_addr_op_ty ty;
  struct aarch64_reg base;

  union {
    size_t imm;
    struct folded_addr_index index;
  };
};

static struct folded_addr_op fold_addr_op(struct cg_state *state,
                                          struct ir_op *op,
                                          struct ir_var_ty *var_ty) {
  DEBUG_ASSERT(op->flags & IR_OP_FLAG_CONTAINED, "expected contained op");

  switch (op->ty) {
  case IR_OP_TY_ADDR_OFFSET: {
    // either:
    //   * `addr.offset %addr + #offset`
    //   * `addr.offset %addr + %index`
    //   * `addr.offset %addr + (%index * sizeof_result)`

    struct ir_op_addr_offset addr_offset = op->addr_offset;

    struct ir_op *base = addr_offset.base;
    struct aarch64_reg base_reg;

    size_t offset = 0;
    if (base->flags & IR_OP_FLAG_CONTAINED) {
      DEBUG_ASSERT(base->ty == IR_OP_TY_ADDR &&
                       base->addr.ty == IR_OP_ADDR_TY_LCL,
                   "expected base to be an `addr LCL` node if contained");

      ssize_t lcl_offset = get_lcl_stack_offset(state, NULL, base->addr.lcl);
      DEBUG_ASSERT(!addr_offset.index || !lcl_offset,
                   "for contained `addr.offset` with an index, expected "
                   "contained `addr` to have offset 0");

      offset += lcl_offset;
      base_reg = STACK_PTR_REG;
    } else {
      base_reg = codegen_reg(base);
    }

    struct aarch64_reg index;

    if (addr_offset.index) {
      DEBUG_ASSERT(ISPOW2(addr_offset.scale) &&
                       (addr_offset.scale == 1 ||
                        addr_offset.scale ==
                            ir_var_ty_info(state->ir->unit, var_ty).size),
                   "must have scale of 1 or same size as store type");

      index = codegen_reg(addr_offset.index);

      return (struct folded_addr_op){
          .ty = FOLDED_ADDR_OP_TY_INDEX,
          .base = base_reg,
          .index = {.index = index, .shift_op_size = addr_offset.scale != 1}};
    } else {
      DEBUG_ASSERT(!addr_offset.index, "cannot have offset and index here");

      return (struct folded_addr_op){.ty = FOLDED_ADDR_OP_TY_IMM,
                                     .base = base_reg,
                                     .imm = offset + addr_offset.offset};
    }
  }
  case IR_OP_TY_ADDR: {
    DEBUG_ASSERT(op->addr.ty == IR_OP_ADDR_TY_LCL,
                 "must be ADDR LCL op for contain");
    ssize_t offset = get_lcl_stack_offset(state, NULL, op->addr.lcl);

    return (struct folded_addr_op){
        .ty = FOLDED_ADDR_OP_TY_IMM, .base = STACK_PTR_REG, .imm = offset};
  }
  default:
    BUG("bad op for contained store");
  }
}

static void codegen_load_addr_op(struct cg_state *state,
                                 struct cg_basicblock *basicblock,
                                 struct ir_op *op) {
  // if (op->id == 206) BREAKPOINT();
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);

  struct aarch64_reg dest = codegen_reg(op);

  if (op->load.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct folded_addr_op fold =
        fold_addr_op(state, op->load.addr, &op->var_ty);

    switch (fold.ty) {
    case FOLDED_ADDR_OP_TY_IMM: {
      size_t imm = fold.imm;
      struct ir_var_ty_info info = ir_var_ty_info(state->ir->unit, &op->var_ty);
      DEBUG_ASSERT(imm % info.size == 0,
                   "%s: %zu expected imm %zu to be multiple of size %zu",
                   state->entry->name, op->id, imm, info.size);
      imm /= info.size;

      instr->aarch64->ty = load_ty_for_op(op, ADDR_MODE_IMM);
      instr->aarch64->ldr_imm =
          (struct aarch64_load_imm){.dest = dest,
                                    .addr = fold.base,
                                    .imm = imm,
                                    .mode = AARCH64_ADDRESSING_MODE_OFFSET};
      break;
    }
    case FOLDED_ADDR_OP_TY_INDEX:
      instr->aarch64->ty = load_ty_for_op(op, ADDR_MODE_REG);
      instr->aarch64->ldr = (struct aarch64_load){
          .dest = dest,
          .addr = fold.base,
          .offset = fold.index.index,
          .extend = AARCH64_EXTEND_LSL,
          .amount =
              fold.index.shift_op_size ? AARCH64_LSL_OPSZ : AARCH64_LSL_0};
      break;
    }
  } else {
    struct aarch64_reg addr = codegen_reg(op->load.addr);
    instr->aarch64->ty = load_ty_for_op(op, ADDR_MODE_IMM);
    instr->aarch64->ldr_imm =
        (struct aarch64_load_imm){.dest = dest,
                                  .addr = addr,
                                  .imm = 0,
                                  .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  }
}
static void codegen_store_addr_op(struct cg_state *state,
                                  struct cg_basicblock *basicblock,
                                  struct ir_op *op) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);

  struct aarch64_reg source = codegen_reg(op->store.value);

  if (op->store.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct folded_addr_op fold =
        fold_addr_op(state, op->store.addr, &op->store.value->var_ty);

    switch (fold.ty) {
    case FOLDED_ADDR_OP_TY_IMM: {
      size_t imm = fold.imm;
      struct ir_var_ty_info info =
          ir_var_ty_info(state->ir->unit, &op->store.value->var_ty);
      DEBUG_ASSERT(imm % info.size == 0,
                   "%s: %zu expected imm %zu to be multiple of size %zu",
                   state->entry->name, op->id, imm, info.size);
      imm /= info.size;

      instr->aarch64->ty = store_ty_for_op(op->store.value, ADDR_MODE_IMM);
      instr->aarch64->str_imm =
          (struct aarch64_store_imm){.source = source,
                                     .addr = fold.base,
                                     .imm = imm,
                                     .mode = AARCH64_ADDRESSING_MODE_OFFSET};
      break;
    }
    case FOLDED_ADDR_OP_TY_INDEX:
      instr->aarch64->ty = store_ty_for_op(op->store.value, ADDR_MODE_REG);
      instr->aarch64->str = (struct aarch64_store){
          .source = source,
          .addr = fold.base,
          .offset = fold.index.index,
          .extend = AARCH64_EXTEND_LSL,
          .amount =
              fold.index.shift_op_size ? AARCH64_LSL_OPSZ : AARCH64_LSL_0};
      break;
    }
  } else {
    struct aarch64_reg addr = codegen_reg(op->store.addr);
    instr->aarch64->ty = store_ty_for_op(op->store.value, ADDR_MODE_IMM);
    instr->aarch64->str_imm =
        (struct aarch64_store_imm){.source = source,
                                   .addr = addr,
                                   .imm = 0,
                                   .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  }
}

static void codegen_bitfield_insert(struct cg_state *state,
                                    struct cg_basicblock *basicblock,
                                    struct ir_op *op) {
  struct ir_bitfield bitfield = op->bitfield_insert.bitfield;

  struct aarch64_reg value_reg = codegen_reg(op->bitfield_insert.value);
  struct aarch64_reg target_reg = codegen_reg(op->bitfield_insert.target);
  struct aarch64_reg dest = codegen_reg(op);

  if (target_reg.idx != dest.idx) {
    struct cg_instr *mov = cg_alloc_instr(state->func, basicblock);
    *mov->aarch64 = MOV_ALIAS(dest, target_reg);
  }

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->aarch64->ty = AARCH64_INSTR_TY_BFM;
  instr->aarch64->bfm = (struct aarch64_bitfield){
      .dest = dest,
      .source = value_reg,
      .imms = bitfield.width - 1,
      .immr = bitfield.offset,
  };
}

static void codegen_bitfield_extract(struct cg_state *state,
                                     struct cg_basicblock *basicblock,
                                     struct ir_op *op) {
  struct ir_bitfield bitfield = op->bitfield_extract.bitfield;

  struct aarch64_reg value_reg = codegen_reg(op->bitfield_extract.value);
  struct aarch64_reg dest = codegen_reg(op);

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->aarch64->ty = AARCH64_INSTR_TY_UBFM;
  instr->aarch64->ubfm = (struct aarch64_bitfield){.dest = dest,
                                                   .source = value_reg,
                                                   .imms = bitfield.width - 1,
                                                   .immr = bitfield.offset};
}

static void codegen_load_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  DEBUG_ASSERT(op->load.ty == IR_OP_LOAD_TY_ADDR,
               "glb/lcl loads should have been lowered to addr load");

  codegen_load_addr_op(state, basicblock, op);
}

static void codegen_store_op(struct cg_state *state,
                             struct cg_basicblock *basicblock,
                             struct ir_op *op) {
  DEBUG_ASSERT(op->store.ty == IR_OP_STORE_TY_ADDR,
               "glb/lcl stores should have been lowered to addr store");

  codegen_store_addr_op(state, basicblock, op);
}

// this method assumes it can safely you any non-argument volatile registers
UNUSED static void codegen_mem_copy_volatile(
    struct cg_state *state, struct cg_basicblock *basicblock,
    struct aarch64_reg source, size_t source_offset, struct aarch64_reg dest,
    size_t dest_offset, size_t num_bytes) {
  DEBUG_ASSERT(num_bytes < 4096,
               "doesn't support >4096 copies due to immediates");

  if (source_offset % 32 || dest_offset % 32) {
    TODO("handle");
  }

  size_t remaining = num_bytes;
  size_t head = 0;

  // use v10/11 as the intermediates

  while (remaining >= 32) {
    struct cg_instr *load = cg_alloc_instr(state->func, basicblock);
    load->aarch64->ty = AARCH64_INSTR_TY_LOAD_PAIR_IMM;
    load->aarch64->load_pair_imm =
        (struct aarch64_load_pair_imm){.mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                       .imm = (head + source_offset) / 16,
                                       .addr = source,
                                       .dest = {
                                           {.ty = AARCH64_REG_TY_Q, .idx = 10},
                                           {.ty = AARCH64_REG_TY_Q, .idx = 11},
                                       }};

    struct cg_instr *store = cg_alloc_instr(state->func, basicblock);
    store->aarch64->ty = AARCH64_INSTR_TY_STORE_PAIR_IMM;
    store->aarch64->store_pair_imm =
        (struct aarch64_store_pair_imm){.mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                        .imm = (head + dest_offset) / 16,
                                        .addr = dest,
                                        .source = {
                                            {.ty = AARCH64_REG_TY_Q, .idx = 10},
                                            {.ty = AARCH64_REG_TY_Q, .idx = 11},
                                        }};

    head += 32;
    remaining -= 32;
  }

  while (remaining) {
    size_t chunk = ilog2(remaining);
    enum aarch64_reg_ty ty = fp_reg_ty_for_size(chunk);

    struct cg_instr *load = cg_alloc_instr(state->func, basicblock);
    load->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
    load->aarch64->load_imm = (struct aarch64_load_imm){
        .mode = AARCH64_ADDRESSING_MODE_OFFSET,
        .imm = (head + source_offset) / chunk,
        .addr = source,
        .dest = {.ty = ty, .idx = 10},
    };

    struct cg_instr *store = cg_alloc_instr(state->func, basicblock);
    store->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
    store->aarch64->store_imm = (struct aarch64_store_imm){
        .mode = AARCH64_ADDRESSING_MODE_OFFSET,
        .imm = (head + dest_offset) / chunk,
        .addr = dest,
        .source = {.ty = ty, .idx = 10},
    };

    head += chunk;
    remaining -= chunk;
  }
}

#define IMM_BITS (12)

static void codegen_sub_imm(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct aarch64_reg dest, struct aarch64_reg source,
                            long long value);

static void codegen_add_imm(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct aarch64_reg dest, struct aarch64_reg source,
                            long long value) {
  if (value < 0) {
    codegen_sub_imm(state, basicblock, dest, source, -value);
    return;
  }

  if (value & ~MASK_LO(unsigned long long, 24)) {
    TODO("imm >24 bits");
  }

  unsigned long long lo = value & MASK_LO(unsigned long long, 12);
  unsigned long long hi = (value >> 12) & MASK_LO(unsigned long long, 12);

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
  instr->aarch64->add_imm = (struct aarch64_addsub_imm){
      .dest = dest,
      .source = source,
      .imm = lo,
      .shift = 0,
  };

  if (hi) {
    instr = cg_alloc_instr(state->func, basicblock);
    instr->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
    instr->aarch64->add_imm = (struct aarch64_addsub_imm){
        .dest = dest,
        .source = dest,
        .imm = hi,
        .shift = 1,
    };
  }
}

static void codegen_sub_imm(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct aarch64_reg dest, struct aarch64_reg source,
                            long long value) {
  if (value < 0) {
    codegen_add_imm(state, basicblock, dest, source, -value);
    return;
  }

  if (value & ~MASK_LO(unsigned long long, 24)) {
    TODO("imm >24 bits");
  }

  unsigned long long lo = value & MASK_LO(unsigned long long, 12);
  unsigned long long hi = (value >> 12) & MASK_LO(unsigned long long, 12);

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->aarch64->ty = AARCH64_INSTR_TY_SUB_IMM;
  instr->aarch64->add_imm = (struct aarch64_addsub_imm){
      .dest = dest,
      .source = source,
      .imm = lo,
      .shift = 0,
  };

  if (hi) {
    instr = cg_alloc_instr(state->func, basicblock);
    instr->aarch64->ty = AARCH64_INSTR_TY_SUB_IMM;
    instr->aarch64->add_imm = (struct aarch64_addsub_imm){
        .dest = dest,
        .source = dest,
        .imm = hi,
        .shift = 1,
    };
  }
}

union b32 {
  unsigned u;
  float f;
  unsigned short b[2];
};

static void codegen_32_bit_int(struct cg_state *state,
                               struct cg_basicblock *basicblock,
                               struct aarch64_reg dest, union b32 value);

static void codegen_addr_offset_op(struct cg_state *state,
                                   struct cg_basicblock *basicblock,
                                   struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);

  struct aarch64_reg base;
  bool base_sp;

  struct ir_op_addr_offset *addr_offset = &op->addr_offset;
  ssize_t offset = addr_offset->offset;

  if (addr_offset->base->flags & IR_OP_FLAG_CONTAINED) {
    DEBUG_ASSERT(addr_offset->base->ty == IR_OP_TY_ADDR &&
                     addr_offset->base->addr.ty == IR_OP_ADDR_TY_LCL,
                 "can only contain `addr LCL`");

    base = STACK_PTR_REG;
    base_sp = true;
    offset += get_lcl_stack_offset(state, NULL, addr_offset->base->addr.lcl);
  } else {
    base_sp = false;
    base = codegen_reg(addr_offset->base);
  }

  DEBUG_ASSERT(!addr_offset->index || popcntl(addr_offset->scale) == 1,
               "non pow2 addr offset op should have been lowered");

  struct aarch64_reg reg = base;
  if (addr_offset->index) {
    reg = dest;

    struct aarch64_reg index = codegen_reg(addr_offset->index);
    size_t shift = tzcnt(addr_offset->scale);

    struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
    if (base_sp) {
      instr->aarch64->ty = AARCH64_INSTR_TY_ADD_EXT;
      instr->aarch64->add_ext =
          (struct aarch64_addsub_ext){.dest = dest,
                                      .lhs = base,
                                      .rhs = index,
                                      .imm3 = shift,
                                      .extend = AARCH64_EXTEND_LSL};
    } else {
      instr->aarch64->ty = AARCH64_INSTR_TY_ADD;
      instr->aarch64->add =
          (struct aarch64_addsub_reg){.dest = dest,
                                      .lhs = base,
                                      .rhs = index,
                                      .imm6 = shift,
                                      .shift = AARCH64_SHIFT_LSL};
    }
  }

  if (offset) {
    codegen_add_imm(state, basicblock, dest, reg, offset);
  }

  if (!addr_offset->index && !offset) {
    // just a mov
    struct cg_instr *mov = cg_alloc_instr(state->func, basicblock);
    *mov->aarch64 = MOV_ALIAS(dest, base);
  }
}

static void codegen_addr_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);

  switch (op->addr.ty) {
  case IR_OP_ADDR_TY_LCL: {
    struct ir_lcl *lcl = op->addr.lcl;

    // op is NULL as we want the absolute offset
    ssize_t offset = get_lcl_stack_offset(state, NULL, lcl);

    codegen_add_imm(state, basicblock, dest, STACK_PTR_REG, offset);

    break;
  }
  case IR_OP_ADDR_TY_GLB: {
    struct ir_glb *glb = op->addr.glb;

    struct cg_instr *adrp = cg_alloc_instr(state->func, basicblock);
    adrp->aarch64->ty = AARCH64_INSTR_TY_ADRP;
    adrp->aarch64->adrp = (struct aarch64_addr_imm){.dest = dest, .imm = 0};

    adrp->reloc = aralloc(state->func->unit->arena, sizeof(*adrp->reloc));
    *adrp->reloc = (struct relocation){
        .ty = glb->def_ty == IR_GLB_DEF_TY_DEFINED ? RELOCATION_TY_LOCAL_PAIR
                                                   : RELOCATION_TY_UNDEF_PAIR,
        .symbol_index = glb->id,
        .address = 0,
        .size = 0};

    if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
      struct cg_instr *add = cg_alloc_instr(state->func, basicblock);
      add->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
      add->aarch64->add_imm = (struct aarch64_addsub_imm){
          .dest = dest,
          .source = dest,
          .imm = 0,
      };
    } else {
      struct cg_instr *ldr = cg_alloc_instr(state->func, basicblock);
      ldr->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
      ldr->aarch64->load_imm = (struct aarch64_load_imm){
          .mode = AARCH64_ADDRESSING_MODE_OFFSET,
          .dest = dest,
          .addr = dest,
          .imm = 0,
      };
    }
  }
  }
}

static void codegen_select_op(struct cg_state *state,
                              struct cg_basicblock *basicblock,
                              struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg cond = codegen_reg(op->select.cond);
  struct aarch64_reg true_op = codegen_reg(op->select.true_op);
  struct aarch64_reg false_op = codegen_reg(op->select.false_op);

  struct cg_instr *cmp = cg_alloc_instr(state->func, basicblock);
  cmp->aarch64->ty = AARCH64_INSTR_TY_SUBS;
  cmp->aarch64->subs = (struct aarch64_addsub_reg){
      .dest = zero_reg_for_ty(cond.ty),
      .lhs = cond,
      .rhs = zero_reg_for_ty(cond.ty),
  };

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->aarch64->ty = AARCH64_INSTR_TY_CSEL;
  instr->aarch64->csel =
      (struct aarch64_conditional_select){.dest = dest,
                                          .cond = AARCH64_COND_NE,
                                          .true_source = true_op,
                                          .false_source = false_op};
}

static void codegen_br_cond_op(struct cg_state *state,
                               struct cg_basicblock *basicblock,
                               struct ir_op *op) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);

  // AArch64 requires turning `br.cond <true> <false>` into 2 instructions
  // we represent this as just the `true` part of the `br.cond`, and then a
  // `br` after branching to the false target

  struct ir_basicblock *true_target = op->stmt->basicblock->split.true_target;
  struct ir_basicblock *false_target = op->stmt->basicblock->split.false_target;

  if (op->br_cond.cond->reg.ty == IR_REG_TY_FLAGS) {
    // emit based on flags
    enum aarch64_cond cond = get_cond_for_op(op->br_cond.cond);

    if (op->br_cond.cond->ty == IR_OP_TY_BINARY_OP &&
        (op->br_cond.cond->binary_op.lhs->flags & IR_OP_FLAG_CONTAINED &&
         !cond_is_com(cond))) {
      // because of immediate, we did `subs rhs, lhs_imm`, so invert cond
      cond = invert_cond(cond);
    }

    instr->aarch64->ty = AARCH64_INSTR_TY_B_COND;
    instr->aarch64->b_cond = (struct aarch64_conditional_branch){
        .cond = cond, .target = true_target};
  } else {
    struct aarch64_reg cmp_reg = codegen_reg(op->br_cond.cond);

    instr->aarch64->ty = AARCH64_INSTR_TY_CBNZ;
    instr->aarch64->cbnz = (struct aarch64_compare_and_branch){
        .cmp = cmp_reg, .target = true_target};
  }

  if (op->stmt->basicblock->succ != false_target) {
    // now generate the `br`
    struct cg_instr *br = cg_alloc_instr(state->func, basicblock);
    br->aarch64->ty = AARCH64_INSTR_TY_B;
    br->aarch64->b = (struct aarch64_branch){.target = false_target};
  }
}

static void codegen_br_op(struct cg_state *state,
                          struct cg_basicblock *basicblock, struct ir_op *op) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);

  instr->aarch64->ty = AARCH64_INSTR_TY_B;
  instr->aarch64->b =
      (struct aarch64_branch){.target = op->stmt->basicblock->merge.target};
}

static_assert((sizeof(unsigned long long) == 8) & (sizeof(unsigned short) == 2),
              "type sizes not expected");

union b64 {
  unsigned long long ull;
  double d;
  unsigned short b[4];
};

static void codegen_64_bit_int(struct cg_state *state,
                               struct cg_basicblock *basicblock,
                               struct aarch64_reg dest, union b64 value) {
  bool first = true;
  if (value.b[0] || !value.ull) {
    struct cg_instr *lo = cg_alloc_instr(state->func, basicblock);
    lo->aarch64->ty = AARCH64_INSTR_TY_MOVZ;
    lo->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[0]};

    first = false;
  }

  if (value.b[1]) {
    struct cg_instr *mid_lo = cg_alloc_instr(state->func, basicblock);
    mid_lo->aarch64->ty = first ? AARCH64_INSTR_TY_MOVZ : AARCH64_INSTR_TY_MOVK;
    mid_lo->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[1], .shift = 1};

    first = false;
  }

  if (value.b[2]) {
    struct cg_instr *mid_hi = cg_alloc_instr(state->func, basicblock);
    mid_hi->aarch64->ty = first ? AARCH64_INSTR_TY_MOVZ : AARCH64_INSTR_TY_MOVK;
    mid_hi->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[2], .shift = 2};

    first = false;
  }

  if (value.b[3]) {
    struct cg_instr *hi = cg_alloc_instr(state->func, basicblock);
    hi->aarch64->ty = first ? AARCH64_INSTR_TY_MOVZ : AARCH64_INSTR_TY_MOVK;
    hi->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[3], .shift = 3};

    first = false;
  }
}

static_assert((sizeof(unsigned) == 4) & (sizeof(unsigned short) == 2),
              "type sizes not expected");

static void codegen_32_bit_int(struct cg_state *state,
                               struct cg_basicblock *basicblock,
                               struct aarch64_reg dest, union b32 value) {

  struct cg_instr *lo = cg_alloc_instr(state->func, basicblock);
  lo->aarch64->ty = AARCH64_INSTR_TY_MOVZ;
  lo->aarch64->movz = (struct aarch64_mov_imm){.dest = dest, .imm = value.b[0]};

  if (value.b[1]) {
    struct cg_instr *hi = cg_alloc_instr(state->func, basicblock);
    hi->aarch64->ty = AARCH64_INSTR_TY_MOVK;
    hi->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[1], .shift = 1};
  }
}

static void codegen_cnst_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  DEBUG_ASSERT(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
               "expects primitive type");

  struct aarch64_reg dest = codegen_reg(op);

  switch (op->cnst.ty) {
  case IR_OP_CNST_TY_FLT:
    // currently all constants are lowered to an integer load and `fmov`
    // but lots of constants can be loaded directly, so do that here
    TODO("simple float constants (not lowered)");
  case IR_OP_CNST_TY_INT:
    switch (op->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I1:
    case IR_VAR_PRIMITIVE_TY_I8:
    case IR_VAR_PRIMITIVE_TY_I16:
    case IR_VAR_PRIMITIVE_TY_I32:
      codegen_32_bit_int(state, basicblock, dest,
                         (union b32){.u = (unsigned)op->cnst.int_value});
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      codegen_64_bit_int(state, basicblock, dest,
                         (union b64){.ull = op->cnst.int_value});
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      TODO("codegen uint128");
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_F32:
    case IR_VAR_PRIMITIVE_TY_F64:
      unreachable();
    };
  }
}

static void codegen_unary_op(struct cg_state *state,
                             struct cg_basicblock *basicblock,
                             struct ir_op *op) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);

  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg source = codegen_reg(op->unary_op.value);

  switch (op->unary_op.ty) {
  case IR_OP_UNARY_OP_TY_REV:
    switch (op->unary_op.value->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I1:
    case IR_VAR_PRIMITIVE_TY_I8:
      BUG("i1/i8 rev makes no sense");
    case IR_VAR_PRIMITIVE_TY_I16:
      instr->aarch64->ty = AARCH64_INSTR_TY_REV16;
      instr->aarch64->rev16 = (struct aarch64_reg_1_source){
          .dest = dest,
          .source = source,
      };
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
    case IR_VAR_PRIMITIVE_TY_I64:
      instr->aarch64->ty = AARCH64_INSTR_TY_REV;
      instr->aarch64->rev = (struct aarch64_reg_1_source){
          .dest = dest,
          .source = source,
      };
      break;
    case IR_VAR_PRIMITIVE_TY_I128:
      TODO("rev i128");
    default:
      unreachable();
    }
    break;
  case IR_OP_UNARY_OP_TY_CLZ:
    if (op->unary_op.value->var_ty.primitive < IR_VAR_PRIMITIVE_TY_I32) {
      TODO("i8/i16 clz");
    }

    instr->aarch64->ty = AARCH64_INSTR_TY_CLZ;
    instr->aarch64->clz = (struct aarch64_reg_1_source){
        .dest = dest,
        .source = source,
    };
    break;
  case IR_OP_UNARY_OP_TY_CTZ:
    if (op->unary_op.value->var_ty.primitive < IR_VAR_PRIMITIVE_TY_I32) {
      TODO("i8/i16 ctz");
    }

    instr->aarch64->ty = AARCH64_INSTR_TY_RBIT;
    instr->aarch64->rbit = (struct aarch64_reg_1_source){
        .dest = dest,
        .source = source,
    };

    struct cg_instr *clz = cg_alloc_instr(state->func, basicblock);
    clz->aarch64->ty = AARCH64_INSTR_TY_CLZ;
    clz->aarch64->clz = (struct aarch64_reg_1_source){
        .dest = dest,
        .source = dest,
    };
    break;
  case IR_OP_UNARY_OP_TY_POPCNT:
    instr->aarch64->ty = AARCH64_INSTR_TY_CNT;
    instr->aarch64->cnt = (struct aarch64_vreg_1_source){
        .dest = dest,
        .source = source,
        .arrangement = AARCH64_V_ARRANGMENT_8B,
    };

    struct cg_instr *addv = cg_alloc_instr(state->func, basicblock);
    addv->aarch64->ty = AARCH64_INSTR_TY_ADDV;
    addv->aarch64->addv = (struct aarch64_vreg_1_source){
        .dest = dest,
        .source = dest,
        .arrangement = AARCH64_V_ARRANGMENT_8B,
    };
    return;
  case IR_OP_UNARY_OP_TY_FABS:
    instr->aarch64->ty = AARCH64_INSTR_TY_FABS;
    instr->aarch64->fabs = (struct aarch64_reg_1_source){
        .dest = dest,
        .source = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_FSQRT:
    instr->aarch64->ty = AARCH64_INSTR_TY_FSQRT;
    instr->aarch64->fsqrt = (struct aarch64_reg_1_source){
        .dest = dest,
        .source = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_FNEG:
    instr->aarch64->ty = AARCH64_INSTR_TY_FNEG;
    instr->aarch64->fneg = (struct aarch64_reg_1_source){
        .dest = dest,
        .source = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_NEG:
    instr->aarch64->ty = AARCH64_INSTR_TY_SUB;
    instr->aarch64->sub = (struct aarch64_addsub_reg){
        .dest = dest,
        .lhs = zero_reg_for_ty(source.ty),
        .rhs = source,
        .shift = 0,
        .imm6 = 0,
    };
    return;
  case IR_OP_UNARY_OP_TY_NOT:
    instr->aarch64->ty = AARCH64_INSTR_TY_ORN;
    instr->aarch64->orn = (struct aarch64_logical_reg){
        .dest = dest,
        .lhs = zero_reg_for_ty(source.ty),
        .rhs = source,
        .shift = 0,
        .imm6 = 0,
    };
    return;
  case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
    BUG("logical not should never reach emitter, should be converted in "
        "lower");
  }
}

static void codegen_binary_op(struct cg_state *state,
                              struct cg_basicblock *basicblock,
                              struct ir_op *op) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);

  struct aarch64_reg dest = {0};

  if (ir_binary_op_is_comparison(op->binary_op.ty)) {
    dest = zero_reg_for_ty(reg_ty_for_var_ty(&op->binary_op.lhs->var_ty));
  } else {
    dest = codegen_reg(op);
  }

  struct ir_op *lhs_op = op->binary_op.lhs;
  struct ir_op *rhs_op = op->binary_op.rhs;

  bool is_fp = ir_var_ty_is_fp(&op->var_ty);

  enum ir_op_binary_op_ty ty = op->binary_op.ty;
  DEBUG_ASSERT(ty == IR_OP_BINARY_OP_TY_FADD || ty == IR_OP_BINARY_OP_TY_FSUB ||
                   ty == IR_OP_BINARY_OP_TY_FMUL ||
                   ty == IR_OP_BINARY_OP_TY_FDIV ||
                   ty == IR_OP_BINARY_OP_TY_FMAX ||
                   ty == IR_OP_BINARY_OP_TY_FMIN || !is_fp,
               "floating point with invalid binary op");

#define CONTAINED_OP(str, ins_up, ins)                                         \
  if ((lhs_op->flags & IR_OP_FLAG_CONTAINED) && lhs_op->cnst.int_value) {      \
    instr->aarch64->ty = AARCH64_INSTR_TY_##ins_up##_IMM;                      \
    instr->aarch64->ins##_imm =                                                \
        (struct aarch64_##str##_imm){.dest = dest,                             \
                                     .source = codegen_reg(rhs_op),            \
                                     .imm = lhs_op->cnst.int_value};           \
  } else if ((rhs_op->flags & IR_OP_FLAG_CONTAINED) &&                         \
             rhs_op->cnst.int_value) {                                         \
    instr->aarch64->ty = AARCH64_INSTR_TY_##ins_up##_IMM;                      \
    instr->aarch64->ins##_imm =                                                \
        (struct aarch64_##str##_imm){.dest = dest,                             \
                                     .source = codegen_reg(lhs_op),            \
                                     .imm = rhs_op->cnst.int_value};           \
  } else {                                                                     \
    instr->aarch64->ty = AARCH64_INSTR_TY_##ins_up;                            \
    instr->aarch64->ins = (struct aarch64_##str##_reg){                        \
        .dest = dest,                                                          \
        .lhs = codegen_reg(lhs_op),                                            \
        .rhs = codegen_reg(rhs_op),                                            \
    };                                                                         \
  }

  switch (ty) {
  case IR_OP_BINARY_OP_TY_FEQ:
  case IR_OP_BINARY_OP_TY_FNEQ:
  case IR_OP_BINARY_OP_TY_FGT:
  case IR_OP_BINARY_OP_TY_FGTEQ:
  case IR_OP_BINARY_OP_TY_FLT:
  case IR_OP_BINARY_OP_TY_FLTEQ:
    if (lhs_op->flags & IR_OP_FLAG_CONTAINED) {
      instr->aarch64->ty = AARCH64_INSTR_TY_FCMP_ZERO;
      instr->aarch64->fcmp_zero = (struct aarch64_fcmp_zero){
          .lhs = codegen_reg(rhs_op),
      };
    } else if (rhs_op->flags & IR_OP_FLAG_CONTAINED) {
      instr->aarch64->ty = AARCH64_INSTR_TY_FCMP_ZERO;
      instr->aarch64->fcmp_zero = (struct aarch64_fcmp_zero){
          .lhs = codegen_reg(lhs_op),
      };
    } else {
      instr->aarch64->ty = AARCH64_INSTR_TY_FCMP;
      instr->aarch64->fcmp = (struct aarch64_fcmp){
          .lhs = codegen_reg(lhs_op),
          .rhs = codegen_reg(rhs_op),
      };
    }
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
    CONTAINED_OP(addsub, SUBS, subs);
    break;
  case IR_OP_BINARY_OP_TY_LSHIFT:
    if (rhs_op->flags & IR_OP_FLAG_CONTAINED) {
      size_t shift = rhs_op->cnst.int_value;

      if (shift) {
        size_t size = ir_var_ty_info(state->ir->unit, &op->var_ty).size;
        size_t imms = (size * 8 - 1) - shift;
        instr->aarch64->ty = AARCH64_INSTR_TY_UBFM;
        instr->aarch64->ubfm =
            (struct aarch64_bitfield){.dest = dest,
                                      .source = codegen_reg(lhs_op),
                                      .immr = imms + 1,
                                      .imms = imms};
      } else {
        *instr->aarch64 = MOV_ALIAS(dest, codegen_reg(lhs_op));
      }
    } else {
      instr->aarch64->ty = AARCH64_INSTR_TY_LSLV;
      instr->aarch64->lslv = (struct aarch64_reg_2_source){
          .dest = dest,
          .lhs = codegen_reg(lhs_op),
          .rhs = codegen_reg(rhs_op),
      };
    }
    break;
  case IR_OP_BINARY_OP_TY_SRSHIFT:
    if (rhs_op->flags & IR_OP_FLAG_CONTAINED) {
      size_t shift = rhs_op->cnst.int_value;

      if (shift) {
        size_t size = ir_var_ty_info(state->ir->unit, &op->var_ty).size;
        size_t immr = shift;
        instr->aarch64->ty = AARCH64_INSTR_TY_SBFM;
        instr->aarch64->sbfm =
            (struct aarch64_bitfield){.dest = dest,
                                      .source = codegen_reg(lhs_op),
                                      .immr = immr,
                                      .imms = (size * 8) - 1};
      } else {
        *instr->aarch64 = MOV_ALIAS(dest, codegen_reg(lhs_op));
      }
    } else {
      instr->aarch64->ty = AARCH64_INSTR_TY_ASRV;
      instr->aarch64->asrv = (struct aarch64_reg_2_source){
          .dest = dest,
          .lhs = codegen_reg(lhs_op),
          .rhs = codegen_reg(rhs_op),
      };
    }
    break;
  case IR_OP_BINARY_OP_TY_URSHIFT:
    if (rhs_op->flags & IR_OP_FLAG_CONTAINED) {
      size_t shift = rhs_op->cnst.int_value;

      if (shift) {

        size_t size = ir_var_ty_info(state->ir->unit, &op->var_ty).size;
        size_t immr = shift;
        instr->aarch64->ty = AARCH64_INSTR_TY_UBFM;
        instr->aarch64->ubfm =
            (struct aarch64_bitfield){.dest = dest,
                                      .source = codegen_reg(lhs_op),
                                      .immr = immr,
                                      .imms = (size * 8) - 1};
      } else {
        *instr->aarch64 = MOV_ALIAS(dest, codegen_reg(lhs_op));
      }
    } else {
      instr->aarch64->ty = AARCH64_INSTR_TY_LSRV;
      instr->aarch64->lsrv = (struct aarch64_reg_2_source){
          .dest = dest,
          .lhs = codegen_reg(lhs_op),
          .rhs = codegen_reg(rhs_op),
      };
    }
    break;
  case IR_OP_BINARY_OP_TY_AND:
    instr->aarch64->ty = AARCH64_INSTR_TY_AND;
    instr->aarch64->and = (struct aarch64_logical_reg){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_OR:
    instr->aarch64->ty = AARCH64_INSTR_TY_ORR;
    instr->aarch64->orr = (struct aarch64_logical_reg){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_XOR:
    instr->aarch64->ty = AARCH64_INSTR_TY_EOR;
    instr->aarch64->eor = (struct aarch64_logical_reg){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_ADD:
    CONTAINED_OP(addsub, ADD, add);
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    CONTAINED_OP(addsub, SUB, sub);
    break;
  case IR_OP_BINARY_OP_TY_MUL:
    instr->aarch64->ty = AARCH64_INSTR_TY_MADD;
    instr->aarch64->madd =
        (struct aarch64_fma){.dest = dest,
                             .lhs = codegen_reg(lhs_op),
                             .rhs = codegen_reg(rhs_op),
                             .addsub = zero_reg_for_ty(dest.ty)};
    break;
  case IR_OP_BINARY_OP_TY_SDIV:
    instr->aarch64->ty = AARCH64_INSTR_TY_SDIV;
    instr->aarch64->sdiv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_UDIV:
    instr->aarch64->ty = AARCH64_INSTR_TY_UDIV;
    instr->aarch64->udiv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_FMAX:
    instr->aarch64->ty = AARCH64_INSTR_TY_FMAXNM;
    instr->aarch64->fmaxnm = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_FMIN:
    instr->aarch64->ty = AARCH64_INSTR_TY_FMINNM;
    instr->aarch64->fminnm = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_FADD:
    instr->aarch64->ty = AARCH64_INSTR_TY_FADD;
    instr->aarch64->fadd = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_FSUB:
    instr->aarch64->ty = AARCH64_INSTR_TY_FSUB;
    instr->aarch64->fsub = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_FMUL:
    instr->aarch64->ty = AARCH64_INSTR_TY_FMUL;
    instr->aarch64->fmul = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_FDIV:
    instr->aarch64->ty = AARCH64_INSTR_TY_FDIV;
    instr->aarch64->fdiv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = codegen_reg(lhs_op),
        .rhs = codegen_reg(rhs_op),
    };
    break;
  case IR_OP_BINARY_OP_TY_SMOD:
  case IR_OP_BINARY_OP_TY_UMOD:
    BUG("smod/umod shoud have been lowered");
  }
}

static void codegen_sext_op(struct cg_state *state,
                            struct cg_basicblock *basicblock, struct ir_op *op,
                            struct aarch64_reg source,
                            struct aarch64_reg dest) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);

  invariant_assert(op->cast_op.value->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't sext from non-primitive");

  switch (op->cast_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I1:
    BUG("sext i1 makes no sense (never negative)");
  case IR_VAR_PRIMITIVE_TY_I8:
    instr->aarch64->ty = AARCH64_INSTR_TY_SBFM;
    instr->aarch64->sbfm = (struct aarch64_bitfield){
        .dest = dest, .source = source, .immr = 0b000000, .imms = 0b000111};
    break;
  case IR_VAR_PRIMITIVE_TY_I16:
    instr->aarch64->ty = AARCH64_INSTR_TY_SBFM;
    instr->aarch64->sbfm = (struct aarch64_bitfield){
        .dest = dest, .source = source, .immr = 0b000000, .imms = 0b001111};
    break;
  case IR_VAR_PRIMITIVE_TY_I32:
    instr->aarch64->ty = AARCH64_INSTR_TY_SBFM;
    instr->aarch64->sbfm = (struct aarch64_bitfield){
        .dest = dest, .source = source, .immr = 0b000000, .imms = 0b011111};
    break;
  case IR_VAR_PRIMITIVE_TY_I64:
    BUG("can't sext from I64");
  case IR_VAR_PRIMITIVE_TY_I128:
    TODO("codegen uint128");
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    BUG("todo cast floats");
  }
}

static void codegen_zext_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct aarch64_reg source,
                            struct aarch64_reg dest) {
  // `mov`/`orr` with 32 bit operands zeroes top 32 bits

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  *instr->aarch64 = MOV_ALIAS(dest, source);
}

static void codegen_trunc_op(struct cg_state *state,
                             struct cg_basicblock *basicblock, struct ir_op *op,
                             struct aarch64_reg source,
                             struct aarch64_reg dest) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  invariant_assert(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't truncate non-primitive");

  // https://kddnewton.com/2022/08/11/aarch64-bitmask-immediates.html
  // for understanding the immediates
  switch (op->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I1:
    instr->aarch64->ty = AARCH64_INSTR_TY_AND_IMM;
    instr->aarch64->and_imm = (struct aarch64_logical_imm){
        .dest = dest,
        .source = source,
        .immr = 0b0,
        .imms = 0b0,
    };
    break;
  case IR_VAR_PRIMITIVE_TY_I8:
    instr->aarch64->ty = AARCH64_INSTR_TY_AND_IMM;
    instr->aarch64->and_imm = (struct aarch64_logical_imm){
        .dest = dest,
        .source = source,
        .immr = 0b0,
        .imms = 0b111,
    };
    break;
  case IR_VAR_PRIMITIVE_TY_I16:
    instr->aarch64->ty = AARCH64_INSTR_TY_AND_IMM;
    instr->aarch64->and_imm = (struct aarch64_logical_imm){
        .dest = dest,
        .source = source,
        .immr = 0b0,
        .imms = 0b1111,
    };
    break;
  case IR_VAR_PRIMITIVE_TY_I32:
    *instr->aarch64 = MOV_ALIAS(dest, source);
    break;
  case IR_VAR_PRIMITIVE_TY_I64:
    break;
  case IR_VAR_PRIMITIVE_TY_I128:
    TODO("codegen uint128");
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    BUG("todo cast floats");
  }
}

static void codegen_conv_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct aarch64_reg source,
                            struct aarch64_reg dest) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->aarch64->ty = AARCH64_INSTR_TY_FCVT;
  instr->aarch64->fcvt =
      (struct aarch64_reg_1_source){.dest = dest, .source = source};
}

static void codegen_uconv_op(struct cg_state *state,
                             struct cg_basicblock *basicblock,
                             struct aarch64_reg source,
                             struct aarch64_reg dest) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->aarch64->ty = AARCH64_INSTR_TY_UCVTF;
  instr->aarch64->fcvt =
      (struct aarch64_reg_1_source){.dest = dest, .source = source};
}

static void codegen_sconv_op(struct cg_state *state,
                             struct cg_basicblock *basicblock,
                             struct aarch64_reg source,
                             struct aarch64_reg dest) {
  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->aarch64->ty = AARCH64_INSTR_TY_SCVTF;
  instr->aarch64->fcvt =
      (struct aarch64_reg_1_source){.dest = dest, .source = source};
}

static void codegen_cast_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg source = codegen_reg(op->cast_op.value);

  // NOTE: for the integer casts (sext/zext/trunc) we promote the source reg
  // to the same type as the dest reg (mixed regs make no sense in an integer
  // instruction)

  switch (op->cast_op.ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    source.ty = dest.ty;
    codegen_sext_op(state, basicblock, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_ZEXT:
    source.ty = dest.ty;
    codegen_zext_op(state, basicblock, source, dest);
    break;
  case IR_OP_CAST_OP_TY_TRUNC:
    source.ty = dest.ty;
    codegen_trunc_op(state, basicblock, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_CONV:
    codegen_conv_op(state, basicblock, source, dest);
    break;
  case IR_OP_CAST_OP_TY_UCONV:
    codegen_uconv_op(state, basicblock, source, dest);
    break;
  case IR_OP_CAST_OP_TY_SCONV:
    codegen_sconv_op(state, basicblock, source, dest);
    break;
  }
}

// when generating reg moves, assume memory slots never conflict
// use incrementing values from 128 onwards
#define FIRST_MEM_LOC 128
#define IS_MEM_LOC(v) ((v.idx) >= FIRST_MEM_LOC)
#define MEM_LOC() (last_mem_loc++)

struct mem_loc {
  struct aarch64_reg base;
  size_t offset;
  size_t size;
};
struct mem_copy {
  struct mem_loc src, dest;
};

UNUSED static enum aarch64_reg_attr_flags
reg_attr_flags(struct aarch64_reg reg) {
  switch (reg.ty) {
  case AARCH64_REG_TY_NONE:
    return AARCH64_REG_ATTR_FLAG_NONE;
  case AARCH64_REG_TY_W:
  case AARCH64_REG_TY_X:
    if (!reg.idx) {
      return AARCH64_REG_ATTR_FLAG_VOLATILE | AARCH64_REG_ATTR_FLAG_ARG_REG |
             AARCH64_REG_ATTR_FLAG_RET_REG;
    }

    if (reg.idx <= 7) {
      return AARCH64_REG_ATTR_FLAG_VOLATILE | AARCH64_REG_ATTR_FLAG_ARG_REG;
    }

    if (reg.idx < 18) {
      return AARCH64_REG_ATTR_FLAG_VOLATILE;
    }

    if (reg.idx == 18) {
      return AARCH64_REG_ATTR_FLAG_RESERVED;
    }

    if (reg.idx < 29) {
      return AARCH64_REG_ATTR_FLAG_NONE;
    }

    return AARCH64_REG_ATTR_FLAG_RESERVED;
  case AARCH64_REG_TY_V:
  case AARCH64_REG_TY_Q:
    if (!reg.idx) {
      return AARCH64_REG_ATTR_FLAG_VOLATILE | AARCH64_REG_ATTR_FLAG_ARG_REG |
             AARCH64_REG_ATTR_FLAG_RET_REG;
    }

    if (reg.idx <= 7) {
      return AARCH64_REG_ATTR_FLAG_VOLATILE | AARCH64_REG_ATTR_FLAG_ARG_REG;
    }

    if (reg.idx <= 15) {
      return AARCH64_REG_ATTR_FLAG_VOLATILE;
    }

    return AARCH64_REG_ATTR_FLAG_NONE;
  case AARCH64_REG_TY_D:
  case AARCH64_REG_TY_S:
  case AARCH64_REG_TY_H:
  case AARCH64_REG_TY_B:
    if (!reg.idx) {
      return AARCH64_REG_ATTR_FLAG_VOLATILE | AARCH64_REG_ATTR_FLAG_ARG_REG |
             AARCH64_REG_ATTR_FLAG_RET_REG;
    }

    if (reg.idx <= 7) {
      return AARCH64_REG_ATTR_FLAG_VOLATILE | AARCH64_REG_ATTR_FLAG_ARG_REG;
    }

    return AARCH64_REG_ATTR_FLAG_NONE;
  }
}

// reg 9 is not part of calling convention
// and all registers have already been saved
// so this is always safe
// same with reg 10, which we use if a register branch target is needed for
// `blr`
#define GP_TMP_REG_IDX ((size_t)9)
#define BLR_REG_IDX ((size_t)10)
#define ADDR_REG_IDX ((size_t)11)
#define FP_TMP_REG_IDX ((size_t)16)

// FIXME: apple do things differently!!!

#define INTEGRAL_OR_PTRLIKE(var_ty)                                            \
  (var_ty_is_integral((var_ty)) || (var_ty)->ty == IR_VAR_TY_TY_POINTER ||     \
   (var_ty)->ty == IR_VAR_TY_TY_ARRAY)

static void codegen_call_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  invariant_assert(op->call.func_ty.ty == IR_VAR_TY_TY_FUNC, "non-func");

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);
  if (op->call.target->flags & IR_OP_FLAG_CONTAINED) {
    instr->aarch64->ty = AARCH64_INSTR_TY_BL;
    instr->aarch64->bl = (struct aarch64_branch){.target = NULL};

    instr->reloc = aralloc(state->func->unit->arena, sizeof(*instr->reloc));
    *instr->reloc = (struct relocation){
        .ty = RELOCATION_TY_CALL,
        .symbol_index = op->call.target->addr.glb->id,
        .size = 2,
        .address = 0,
    };
  } else {
    // NOTE: `blr` seems to segfault on linux aarch64
    instr->aarch64->ty = AARCH64_INSTR_TY_BLR;
    instr->aarch64->blr =
        (struct aarch64_branch_reg){.target = codegen_reg(op->call.target)};
  }
}

static void codegen_epilogue(struct cg_state *state,
                             struct cg_basicblock *basicblock) {
  const struct aarch64_prologue_info *prologue_info =
      state->aarch64_prologue_info;

  if (!prologue_info->prologue_generated) {
    return;
  }

  for (size_t i = 0; i < state->ir->reg_usage.num_nonvolatile_used; i++) {
    struct ir_reg reg = state->ir->reg_usage.nonvolatile_used[i];

    // guaranteed to be mod 8
    size_t offset = (prologue_info->save_start / 8) + i;

    switch (reg.ty) {
    case IR_REG_TY_INTEGRAL: {
      struct cg_instr *restore = cg_alloc_instr(state->func, basicblock);
      restore->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
      restore->aarch64->ldr_imm = (struct aarch64_load_imm){
          .mode = AARCH64_ADDRESSING_MODE_OFFSET,
          .imm = offset,
          .dest = (struct aarch64_reg){.ty = AARCH64_REG_TY_X,
                                       .idx = translate_reg_idx(
                                           reg.idx, IR_REG_TY_INTEGRAL)},
          .addr = STACK_PTR_REG,
      };
      break;
    }
    case IR_REG_TY_FP: {
      struct cg_instr *restore = cg_alloc_instr(state->func, basicblock);
      restore->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
      restore->aarch64->ldr_imm = (struct aarch64_load_imm){
          .mode = AARCH64_ADDRESSING_MODE_OFFSET,
          .imm = offset,
          .dest = (struct aarch64_reg){.ty = AARCH64_REG_TY_D,
                                       .idx = translate_reg_idx(reg.idx,
                                                                IR_REG_TY_FP)},
          .addr = STACK_PTR_REG,
      };
      break;
    }
    default:
      BUG("can't restore this reg ty");
    }
  }

  size_t stack_to_add = prologue_info->stack_size - 16;
  if (stack_to_add) {
    if (stack_to_add > MAX_IMM_SIZE) {
      codegen_add_imm(state, basicblock, STACK_PTR_REG, STACK_PTR_REG,
                      stack_to_add);
    } else {
      struct cg_instr *add_stack = cg_alloc_instr(state->func, basicblock);
      add_stack->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
      add_stack->aarch64->add_imm =
          (struct aarch64_addsub_imm){.dest = STACK_PTR_REG,
                                      .source = STACK_PTR_REG,
                                      .imm = stack_to_add,
                                      .shift = 0};
    }
  }

  struct cg_instr *restore_lr_x30 = cg_alloc_instr(state->func, basicblock);
  restore_lr_x30->aarch64->ty = AARCH64_INSTR_TY_LOAD_PAIR_IMM;
  restore_lr_x30->aarch64->ldp_imm = (struct aarch64_load_pair_imm){
      .mode = AARCH64_ADDRESSING_MODE_POSTINDEX,
      .imm = prologue_info->lr_offset,
      .dest = {FRAME_PTR_REG, RET_PTR_REG},
      .addr = STACK_PTR_REG,
  };
}

static void codegen_ret_op(struct cg_state *state,
                           struct cg_basicblock *basicblock,
                           UNUSED struct ir_op *op) {
  codegen_epilogue(state, basicblock);

  struct cg_instr *instr = cg_alloc_instr(state->func, basicblock);

  instr->aarch64->ty = AARCH64_INSTR_TY_RET;
  instr->aarch64->ret = (struct aarch64_branch_reg){.target = RET_PTR_REG};
}

static void codegen_op(struct cg_state *state, struct cg_basicblock *basicblock,
                       struct ir_op *op) {
  switch (op->ty) {
  case IR_OP_TY_UNDF:
  case IR_OP_TY_PHI:
    break;
  case IR_OP_TY_MOV: {
    if (op->flags & IR_OP_FLAG_PARAM) {
      // don't need to do anything
    } else {
      codegen_mov_op(state, basicblock, op);
    }
    break;
  }
  case IR_OP_TY_BITFIELD_INSERT:
    codegen_bitfield_insert(state, basicblock, op);
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    codegen_bitfield_extract(state, basicblock, op);
    break;
  case IR_OP_TY_LOAD:
    codegen_load_op(state, basicblock, op);
    break;
  case IR_OP_TY_STORE:
    codegen_store_op(state, basicblock, op);
    break;
  case IR_OP_TY_ADDR: {
    codegen_addr_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_BR_COND: {
    codegen_br_cond_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_BR: {
    codegen_br_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_CNST: {
    codegen_cnst_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_UNARY_OP: {
    codegen_unary_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_BINARY_OP: {
    codegen_binary_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_CAST_OP: {
    codegen_cast_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_CALL: {
    codegen_call_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_RET: {
    codegen_ret_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_ADDR_OFFSET: {
    codegen_addr_offset_op(state, basicblock, op);
    break;
  }
  case IR_OP_TY_SELECT: {
    codegen_select_op(state, basicblock, op);
    break;
  }
  default: {
    TODO("unsupported IR OP '%u'", op->ty);
  }
  }
}

// static void codegen_nop(struct codegen_state *state) {
//   struct cg_instr *instr = alloc_instr(state->func);

//   instr->aarch64->ty = AARCH64_INSTR_TY_NOP;
// }

static void codegen_stmt(struct cg_state *state,
                         struct cg_basicblock *basicblock,
                         const struct ir_stmt *stmt) {
  struct ir_op *op = stmt->first;
  while (op) {
    if (!(op->flags & IR_OP_FLAG_CONTAINED)) {
      codegen_op(state, basicblock, op);
    }

    op = op->succ;
  }
}

#define LEAF_STACK_SIZE (128)

static void codegen_prologue(struct cg_state *state) {
  struct ir_func *ir = state->ir;

  size_t stack_size =
      state->ir->total_locals_size + state->ir->caller_stack_needed;
  stack_size = ROUND_UP(stack_size, AARCH64_STACK_ALIGNMENT);

  const size_t LR_OFFSET = 2;

  size_t save_start = stack_size;

  size_t num_nonvolatile_used = ir->reg_usage.num_nonvolatile_used;

  // save nonvol
  stack_size += num_nonvolatile_used * 8;
  stack_size = ROUND_UP(stack_size, AARCH64_STACK_ALIGNMENT);

  // TODO: implement red zone. requires _subtracting_ from `sp` instead of
  // adding for all local addressing bool leaf =
  //     !(stack_size > LEAF_STACK_SIZE || ir->flags &
  //     IR_FUNC_FLAG_MAKES_CALL);
  bool leaf = !(stack_size || ir->flags & IR_FUNC_FLAG_MAKES_CALL);

  struct aarch64_prologue_info info = {.prologue_generated = !leaf,
                                       .save_start = save_start,
                                       .lr_offset = LR_OFFSET,
                                       .stack_size = stack_size};

  state->aarch64_prologue_info =
      aralloc(state->arena, sizeof(*state->aarch64_prologue_info));

  if (!info.prologue_generated) {
    *state->aarch64_prologue_info = info;
    return;
  }

  // need to save x29 and x30
  info.stack_size += 16;

  struct cg_basicblock *basicblock = cg_alloc_basicblock(state->func, NULL);

  struct cg_instr *save_lr_x30 = cg_alloc_instr(state->func, basicblock);
  save_lr_x30->aarch64->ty = AARCH64_INSTR_TY_STORE_PAIR_IMM;
  save_lr_x30->aarch64->stp_imm = (struct aarch64_store_pair_imm){
      .mode = AARCH64_ADDRESSING_MODE_PREINDEX,
      .imm = -info.lr_offset,
      .source = {FRAME_PTR_REG, RET_PTR_REG},
      .addr = STACK_PTR_REG,
  };

  info.stack_size = ROUND_UP(info.stack_size, AARCH64_STACK_ALIGNMENT);

  // also save stack pointer into frame pointer as required by ABI
  // `mov x29, sp` is illegal (encodes as `mov x29, xzr`)
  // so `add x29, sp, #x` is used instead
  struct cg_instr *save_x29 = cg_alloc_instr(state->func, basicblock);
  save_x29->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
  save_x29->aarch64->add_imm = (struct aarch64_addsub_imm){
      .dest = (struct aarch64_reg){.ty = AARCH64_REG_TY_X, .idx = 29},
      .source = STACK_PTR_REG,
      .imm = info.lr_offset * 8,
      .shift = 0};

  size_t stack_to_sub = info.stack_size - 16; // from the pre-index lr
  if (stack_to_sub) {
    if (stack_to_sub > MAX_IMM_SIZE) {
      codegen_sub_imm(state, basicblock, STACK_PTR_REG, STACK_PTR_REG,
                      stack_to_sub);
    } else {
      struct cg_instr *sub_stack = cg_alloc_instr(state->func, basicblock);
      sub_stack->aarch64->ty = AARCH64_INSTR_TY_SUB_IMM;
      sub_stack->aarch64->sub_imm =
          (struct aarch64_addsub_imm){.dest = STACK_PTR_REG,
                                      .source = STACK_PTR_REG,
                                      .imm = stack_to_sub,
                                      .shift = 0};
    }

    for (size_t i = 0; i < num_nonvolatile_used; i++) {
      struct ir_reg reg = ir->reg_usage.nonvolatile_used[i];

      // guaranteed to be mod 8
      size_t offset = (info.save_start / 8) + i;

      switch (reg.ty) {
      case IR_REG_TY_INTEGRAL: {
        struct cg_instr *save = cg_alloc_instr(state->func, basicblock);
        save->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
        save->aarch64->str_imm = (struct aarch64_store_imm){
            .mode = AARCH64_ADDRESSING_MODE_OFFSET,
            .imm = offset,
            .source = (struct aarch64_reg){.ty = AARCH64_REG_TY_X,
                                           .idx = translate_reg_idx(
                                               reg.idx, IR_REG_TY_INTEGRAL)},
            .addr = STACK_PTR_REG,
        };
        break;
      }
      case IR_REG_TY_FP: {
        struct cg_instr *save = cg_alloc_instr(state->func, basicblock);
        save->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
        save->aarch64->str_imm = (struct aarch64_store_imm){
            .mode = AARCH64_ADDRESSING_MODE_OFFSET,
            .imm = offset,
            .source = (struct aarch64_reg){.ty = AARCH64_REG_TY_D,
                                           .idx = translate_reg_idx(
                                               reg.idx, IR_REG_TY_FP)},
            .addr = STACK_PTR_REG,
        };
        break;
      }
      default:
        BUG("can't save this reg ty");
      }
    }
  }

  *state->aarch64_prologue_info = info;
}

void aarch64_codegen_start(struct cg_state *state) { codegen_prologue(state); }

void aarch64_codegen_basicblock(struct cg_state *state,
                                struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;
  struct cg_basicblock *cg_basicblock =
      cg_alloc_basicblock(state->func, basicblock);

  while (stmt) {
    codegen_stmt(state, cg_basicblock, stmt);

    stmt = stmt->succ;
  }
}

struct check_reg_type_data {
  // used so we know when the instruction changes
  const struct cg_state *state;
  struct cg_instr *last;

  enum aarch64_reg_ty reg_ty;
};

static void check_reg_type_callback(struct cg_instr *instr,
                                    struct aarch64_reg reg,
                                    enum aarch64_reg_usage_ty usage_ty,
                                    void *metadata) {
  struct check_reg_type_data *data = metadata;

  if (usage_ty == AARCH64_REG_USAGE_TY_DEREF) {
    // deref only makes sense on an X register, and is ignored for comparisons
    // to other registers so back out early
    invariant_assert(reg.ty == AARCH64_REG_TY_X,
                     "usage DEREF only makes sense with REG_TY_X in %zu",
                     instr->id);
    return;
  }

  if (instr != data->last) {
    // new
    data->reg_ty = reg.ty;
  } else {
    // check reg types are the same within instructions
    // FMOV is an exception as it can move GP<->FP

    if (instr->aarch64->ty == AARCH64_INSTR_TY_FMOV) {
      size_t cur_size = aarch64_reg_size(reg.ty);
      size_t last_size = aarch64_reg_size(data->reg_ty);
      invariant_assert(cur_size == last_size ||
                           (cur_size < 4 && last_size == 4),
                       "expected `fmov` %zu to have same size registers "
                       "(expected %zu found %zu)",
                       instr->id, cur_size, last_size);
    } else if (instr->aarch64->ty == AARCH64_INSTR_TY_FCVT) {
      invariant_assert(aarch64_reg_ty_is_fp(reg.ty) &&
                           aarch64_reg_ty_is_fp(data->reg_ty),
                       "expected `fcvt` %zu to have all registers "
                       "floating-point",
                       instr->id);
    } else if (instr->aarch64->ty == AARCH64_INSTR_TY_UCVTF ||
               instr->aarch64->ty == AARCH64_INSTR_TY_SCVTF) {
      invariant_assert(aarch64_reg_ty_is_fp(reg.ty) !=
                           aarch64_reg_ty_is_fp(data->reg_ty),
                       "expected `ucvtf`/`scvtf` %zu to have one fp register "
                       "and one gp register",
                       instr->id);
    } else {
      // invariant_assert(
      //     reg.ty == data->reg_ty,
      //     "reg ty mismatch in %zu (expected %d found %d)",
      //     instr->id, data->reg_ty, reg.ty);
    }
  }

  data->last = instr;
}

void aarch64_codegen_end(struct cg_state *state) {
  // codegen is now done
  // do some basic sanity checks

  struct check_reg_type_data data = {.state = state, .last = NULL, .reg_ty = 0};
  walk_regs(state->func, check_reg_type_callback, &data);
}

void walk_regs(const struct cg_func *func, walk_regs_callback *cb,
               void *metadata) {
  struct cg_basicblock *basicblock = func->first;
  while (basicblock) {
    struct cg_instr *instr = basicblock->first;

    while (instr) {
      enum aarch64_instr_class class = instr_class(instr->aarch64->ty);
      switch (class) {
      case AARCH64_INSTR_CLASS_NOP:
        break;
      case AARCH64_INSTR_CLASS_LOGICAL_REG: {
        struct aarch64_logical_reg logical_reg = instr->aarch64->logical_reg;
        cb(instr, logical_reg.lhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, logical_reg.rhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, logical_reg.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_LOGICAL_IMM: {
        struct aarch64_logical_imm logical_imm = instr->aarch64->logical_imm;
        cb(instr, logical_imm.source, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, logical_imm.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_ADDSUB_REG: {
        struct aarch64_addsub_reg addsub_reg = instr->aarch64->addsub_reg;
        cb(instr, addsub_reg.lhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, addsub_reg.rhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, addsub_reg.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_ADDSUB_EXT: {
        struct aarch64_addsub_ext addsub_ext = instr->aarch64->addsub_ext;
        cb(instr, addsub_ext.lhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, addsub_ext.rhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, addsub_ext.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_ADDSUB_IMM: {
        struct aarch64_addsub_imm addsub_imm = instr->aarch64->addsub_imm;
        cb(instr, addsub_imm.source, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, addsub_imm.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_ADDR_IMM: {
        struct aarch64_addr_imm addr_imm = instr->aarch64->addr_imm;
        cb(instr, addr_imm.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_BITFIELD: {
        struct aarch64_bitfield bitfield = instr->aarch64->bitfield;
        cb(instr, bitfield.source, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, bitfield.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_FCMP: {
        struct aarch64_fcmp fcmp = instr->aarch64->fcmp;
        cb(instr, fcmp.lhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, fcmp.rhs, AARCH64_REG_USAGE_TY_READ, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_FCMP_ZERO: {
        struct aarch64_fcmp_zero fcmp_zero = instr->aarch64->fcmp_zero;
        cb(instr, fcmp_zero.lhs, AARCH64_REG_USAGE_TY_READ, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_REG_1_SOURCE: {
        struct aarch64_reg_1_source reg_1_source = instr->aarch64->reg_1_source;
        cb(instr, reg_1_source.source, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, reg_1_source.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_VREG_1_SOURCE: {
        struct aarch64_vreg_1_source reg_1_source =
            instr->aarch64->vreg_1_source;
        cb(instr, reg_1_source.source, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, reg_1_source.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_REG_2_SOURCE: {
        struct aarch64_reg_2_source reg_2_source = instr->aarch64->reg_2_source;
        cb(instr, reg_2_source.lhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, reg_2_source.rhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, reg_2_source.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_MOV_IMM: {
        struct aarch64_mov_imm mov_imm = instr->aarch64->mov_imm;
        cb(instr, mov_imm.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_FMA: {
        struct aarch64_fma fma = instr->aarch64->fma;
        cb(instr, fma.lhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, fma.rhs, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, fma.addsub, AARCH64_REG_USAGE_TY_READ, metadata);
        cb(instr, fma.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_CONDITIONAL_SELECT: {
        struct aarch64_conditional_select conditional_select =
            instr->aarch64->conditional_select;
        cb(instr, conditional_select.false_source, AARCH64_REG_USAGE_TY_READ,
           metadata);
        cb(instr, conditional_select.true_source, AARCH64_REG_USAGE_TY_READ,
           metadata);
        cb(instr, conditional_select.dest, AARCH64_REG_USAGE_TY_WRITE,
           metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_CONDITIONAL_BRANCH: {
        break;
      }
      case AARCH64_INSTR_CLASS_BRANCH: {
        break;
      }
      case AARCH64_INSTR_CLASS_BRANCH_REG: {
        struct aarch64_branch_reg branch_reg = instr->aarch64->branch_reg;
        cb(instr, branch_reg.target, AARCH64_REG_USAGE_TY_READ, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_COMPARE_AND_BRANCH: {
        struct aarch64_compare_and_branch compare_and_branch =
            instr->aarch64->compare_and_branch;
        cb(instr, compare_and_branch.cmp, AARCH64_REG_USAGE_TY_READ, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_LOAD_IMM: {
        struct aarch64_load_imm load_imm = instr->aarch64->load_imm;
        cb(instr, load_imm.addr, AARCH64_REG_USAGE_TY_DEREF, metadata);
        cb(instr, load_imm.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_STORE_IMM: {
        struct aarch64_store_imm store_imm = instr->aarch64->store_imm;
        cb(instr, store_imm.addr, AARCH64_REG_USAGE_TY_DEREF, metadata);
        cb(instr, store_imm.source, AARCH64_REG_USAGE_TY_READ, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_LOAD: {
        struct aarch64_load load = instr->aarch64->load;
        cb(instr, load.addr, AARCH64_REG_USAGE_TY_DEREF, metadata);
        cb(instr, load.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_STORE: {
        struct aarch64_store store = instr->aarch64->store;
        cb(instr, store.addr, AARCH64_REG_USAGE_TY_DEREF, metadata);
        cb(instr, store.source, AARCH64_REG_USAGE_TY_READ, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_LOAD_PAIR_IMM: {
        struct aarch64_load_pair_imm load_pair_imm =
            instr->aarch64->load_pair_imm;
        cb(instr, load_pair_imm.addr, AARCH64_REG_USAGE_TY_DEREF, metadata);
        cb(instr, load_pair_imm.dest[0], AARCH64_REG_USAGE_TY_WRITE, metadata);
        cb(instr, load_pair_imm.dest[1], AARCH64_REG_USAGE_TY_WRITE, metadata);
        break;
      }
      case AARCH64_INSTR_CLASS_STORE_PAIR_IMM: {
        struct aarch64_store_pair_imm store_pair_imm =
            instr->aarch64->store_pair_imm;
        cb(instr, store_pair_imm.addr, AARCH64_REG_USAGE_TY_DEREF, metadata);
        cb(instr, store_pair_imm.source[0], AARCH64_REG_USAGE_TY_READ,
           metadata);
        cb(instr, store_pair_imm.source[1], AARCH64_REG_USAGE_TY_READ,
           metadata);
        break;
      }
      }

      instr = instr->succ;
    }
    basicblock = basicblock->succ;
  }
}

#ifndef __JCC__
static char reg_prefix(struct aarch64_reg reg) {
  switch (reg.ty) {
  case AARCH64_REG_TY_NONE:
    return '!';
  case AARCH64_REG_TY_W:
    return 'w';
  case AARCH64_REG_TY_X:
    return 'x';
  case AARCH64_REG_TY_V:
    return 'b';
  case AARCH64_REG_TY_Q:
    return 'q';
  case AARCH64_REG_TY_D:
    return 'd';
  case AARCH64_REG_TY_S:
    return 's';
  case AARCH64_REG_TY_H:
    return 'h';
  case AARCH64_REG_TY_B:
    return 'b';
  }
}
#endif

static void codegen_fprintf(FILE *file, const char *format, ...) {
#ifdef __JCC__
  (void)file;
  (void)format;
  TODO("jcc va_arg");
#else

  va_list list;
  va_start(list, format);
  while (format[0] != '\0') {
    if (format[0] != '%') {
      fputc(format[0], file);
      format++;
      continue;
    }

    format++;

    if (strncmp(format, "addr_imm", 8) == 0) {
      // expects addressing mode + register + immediate
      enum aarch64_addressing_mode addressing_mode =
          va_arg(list, enum aarch64_addressing_mode);
      struct aarch64_reg reg = va_arg(list, struct aarch64_reg);
      ssize_t imm = va_arg(list, ssize_t);

      size_t size = aarch64_reg_size(reg.ty);
      char prefix = reg_prefix(reg);

      if (!imm) {
        fprintf(file, "[%c%zu]", prefix, reg.idx);
      } else {
        imm *= size;
        switch (addressing_mode) {
        case AARCH64_ADDRESSING_MODE_OFFSET:
          fprintf(file, "[%c%zu, #%zd]", prefix, reg.idx, imm);
          break;
        case AARCH64_ADDRESSING_MODE_PREINDEX:
          fprintf(file, "[%c%zu, #%zd]!", prefix, reg.idx, imm);
          break;
        case AARCH64_ADDRESSING_MODE_POSTINDEX:
          fprintf(file, "[%c%zu], #%zd", prefix, reg.idx, imm);
          break;
        }
      }

      format += 8;
    } else if (strncmp(format, "addr", 4) == 0) {
      // expects addr + offset + extend + amount + operand size
      struct aarch64_reg addr = va_arg(list, struct aarch64_reg);
      struct aarch64_reg offset = va_arg(list, struct aarch64_reg);
      enum aarch64_extend extend = va_arg(list, enum aarch64_extend);
      enum aarch64_lsl amount = va_arg(list, enum aarch64_lsl);
      enum aarch64_op_size op_size = va_arg(list, enum aarch64_op_size);

      fprintf(file, "[%c%zu, %c%zu", reg_prefix(addr), addr.idx,
              reg_prefix(offset), offset.idx);

      size_t imm = 0;
      if (amount && extend == AARCH64_EXTEND_LSL) {
        switch (op_size) {
        case AARCH64_OP_SIZE_DWORD:
          imm = 3;
          break;
        case AARCH64_OP_SIZE_WORD:
          imm = 2;
          break;
        case AARCH64_OP_SIZE_HALF:
          imm = 1;
          break;
        case AARCH64_OP_SIZE_BYTE:
          imm = 0;
          break;
        }
      }

      switch (extend) {
      case AARCH64_EXTEND_LSL:
        if (imm) {
          fprintf(file, ", lsl %zu", imm);
        }
        break;
      case AARCH64_EXTEND_UXTB:
        fprintf(file, ", uxtb");
        break;
      case AARCH64_EXTEND_UXTH:
        fprintf(file, ", uxth");
        break;
      case AARCH64_EXTEND_UXTW:
        fprintf(file, ", uxtw");
        break;
      case AARCH64_EXTEND_SXTB:
        fprintf(file, ", sxtb");
        break;
      case AARCH64_EXTEND_SXTH:
        fprintf(file, ", sxth");
        break;
      case AARCH64_EXTEND_SXTW:
        fprintf(file, ", sxtw");
        break;
      case AARCH64_EXTEND_SXTX:
        fprintf(file, ", sxtx");
        break;
      }

      fprintf(file, "]");

      format += 4;
    } else if (strncmp(format, "ext_imm", 7) == 0) {
      // expects a shift + an immediate, and prints if shift != LSL or imm !=
      // 0
      enum aarch64_extend extend = va_arg(list, enum aarch64_extend);
      size_t imm = va_arg(list, size_t);

      if (extend != AARCH64_EXTEND_UXTW || imm) {
        switch (extend) {
        case AARCH64_EXTEND_UXTB:
          fprintf(file, ", uxtb #%zu", imm);
          break;
        case AARCH64_EXTEND_UXTH:
          fprintf(file, ", uxth #%zu", imm);
          break;
        case AARCH64_EXTEND_UXTW:
          fprintf(file, ", uxtw #%zu", imm);
          break;
        case AARCH64_EXTEND_UXTX:
          fprintf(file, ", uxtx #%zu", imm);
          break;
        case AARCH64_EXTEND_SXTB:
          fprintf(file, ", sxtb #%zu", imm);
          break;
        case AARCH64_EXTEND_SXTH:
          fprintf(file, ", sxth #%zu", imm);
          break;
        case AARCH64_EXTEND_SXTW:
          fprintf(file, ", sxtw #%zu", imm);
          break;
        case AARCH64_EXTEND_SXTX:
          fprintf(file, ", sxtx #%zu", imm);
          break;
        }
      }

      format += 7;
    } else if (strncmp(format, "shift_imm", 9) == 0) {
      // expects a shift + an immediate, and prints if shift != LSL or imm !=
      // 0
      enum aarch64_shift shift = va_arg(list, enum aarch64_shift);
      size_t imm = va_arg(list, size_t);

      if (shift != AARCH64_SHIFT_LSL || imm) {
        switch (shift) {
        case AARCH64_SHIFT_LSL:
          fprintf(file, ", lsl #%zu", imm);
          break;
        case AARCH64_SHIFT_LSR:
          fprintf(file, ", lsr #%zu", imm);
          break;
        case AARCH64_SHIFT_ASR:
          fprintf(file, ", asr #%zu", imm);
          break;
        case AARCH64_SHIFT_RESERVED:
          fprintf(file, ", RESERVED");
          break;
        }
      }

      format += 9;
    } else if (strncmp(format, "log_imm", 7) == 0) {
      // logical immediate are weird
      size_t n = va_arg(list, size_t);
      size_t immr = va_arg(list, size_t);
      size_t imms = va_arg(list, size_t);
      if (!n && !immr && !imms) {
        fprintf(file, "#0");
      } else {
        fprintf(file,
                "(N=%zu, immr=%zu, imms=%zu) - TODO: logical immediate "
                "formatting",
                n, immr, imms);
      }

      format += 7;
    } else if (strncmp(format, "reg", 3) == 0) {
      struct aarch64_reg reg = va_arg(list, struct aarch64_reg);
      char prefix = reg_prefix(reg);
      fputc(prefix, file);

      if (is_zero_reg(reg)) {
        fprintf(file, "zr");
      } else {
        fprintf(file, "%zu", reg.idx);
      }

      format += 3;
    } else if (strncmp(format, "instr", 5) == 0) {
      struct cg_instr *instr = va_arg(list, struct cg_instr *);
      if (instr) {
        fprintf(file, "%%%zx", instr->id);
      } else {
        fprintf(file, "%%(null)");
      }

      format += 5;
    } else if (strncmp(format, "cond", 4) == 0) {
      enum aarch64_cond cond = va_arg(list, enum aarch64_cond);
      switch (cond) {
      case AARCH64_COND_AL:
        fprintf(file, "al");
        break;
      case AARCH64_COND_AL_ALT:
        fprintf(file, "alt");
        break;
      case AARCH64_COND_EQ:
        fprintf(file, "eq");
        break;
      case AARCH64_COND_NE:
        fprintf(file, "ne");
        break;
      case AARCH64_COND_GE:
        fprintf(file, "ge");
        break;
      case AARCH64_COND_LT:
        fprintf(file, "lt");
        break;
      case AARCH64_COND_GT:
        fprintf(file, "gt");
        break;
      case AARCH64_COND_LE:
        fprintf(file, "le");
        break;
      case AARCH64_COND_VS:
        fprintf(file, "vs");
        break;
      case AARCH64_COND_VC:
        fprintf(file, "vc");
        break;
      case AARCH64_COND_CS:
        fprintf(file, "cs");
        break;
      case AARCH64_COND_CC:
        fprintf(file, "cc");
        break;
      case AARCH64_COND_HI:
        fprintf(file, "hi");
        break;
      case AARCH64_COND_LS:
        fprintf(file, "ls");
        break;
      case AARCH64_COND_MI:
        fprintf(file, "mi");
        break;
      case AARCH64_COND_PL:
        fprintf(file, "pl");
        break;
        // prints the other form (CS/CC)
        // case AARCH64_COND_HS:
        //   fprintf(file, "hs");
        //   break;
        // case AARCH64_COND_LO:
        //   fprintf(file, "lo");
        //   break;
      }

      format += 4;
    } else if (strncmp(format, "shift", 5) == 0) {
      enum aarch64_shift shift = va_arg(list, enum aarch64_shift);
      switch (shift) {
      case AARCH64_SHIFT_LSL:
        fprintf(file, "lsl");
        break;
      case AARCH64_SHIFT_LSR:
        fprintf(file, "lsr");
        break;
      case AARCH64_SHIFT_ASR:
        fprintf(file, "asr");
        break;
      case AARCH64_SHIFT_RESERVED:
        fprintf(file, "RESERVED");
        break;
      }

      format += 5;
    } else if (strncmp(format, "imm", 3) == 0) {
      ssize_t imm = va_arg(list, ssize_t);
      fprintf(file, "#%zd", imm);

      format += 3;
    } else if (strncmp(format, "ar", 2) == 0) {
      enum aarch64_v_arrangment ar = va_arg(list, enum aarch64_v_arrangment);

      switch (ar) {
      case AARCH64_V_ARRANGMENT_8B:
        fprintf(file, ".8b");
        break;
      case AARCH64_V_ARRANGMENT_16B:
        fprintf(file, ".16b");
        break;
      }

      format += 2;
    } else if (format[0] == '%') {
      fputc('%', file);
      format++;
    } else {
      BUG("unrecognised format starting '%%%s'", format);
    }
  }

  va_end(list);
#endif
}

static void
debug_print_logical_reg(FILE *file,
                        const struct aarch64_logical_reg *logical_reg) {
  codegen_fprintf(file, " %reg, %reg, %reg%shift_imm", logical_reg->dest,
                  logical_reg->lhs, logical_reg->rhs, logical_reg->shift,
                  logical_reg->imm6);
}

static void
debug_print_logical_imm(FILE *file,
                        const struct aarch64_logical_imm *logical_imm) {
  codegen_fprintf(file, " %reg, %reg, %log_imm", logical_imm->dest,
                  logical_imm->source, logical_imm->n, logical_imm->immr,
                  logical_imm->imms);
}

static void debug_print_addr_imm(FILE *file,
                                 const struct aarch64_addr_imm *addr_imm) {
  codegen_fprintf(file, " %reg, %imm", addr_imm->dest, addr_imm->imm);
}

static void
debug_print_addsub_reg(FILE *file,
                       const struct aarch64_addsub_reg *addsub_reg) {
  codegen_fprintf(file, " %reg, %reg, %reg%shift_imm", addsub_reg->dest,
                  addsub_reg->lhs, addsub_reg->rhs, addsub_reg->shift,
                  addsub_reg->imm6);
}

static void
debug_print_addsub_ext(FILE *file,
                       const struct aarch64_addsub_ext *addsub_ext) {
  codegen_fprintf(file, " %reg, %reg, %reg%ext_imm", addsub_ext->dest,
                  addsub_ext->lhs, addsub_ext->rhs, addsub_ext->extend,
                  addsub_ext->imm3);
}

static void
debug_print_addsub_imm(FILE *file,
                       const struct aarch64_addsub_imm *addsub_imm) {
  if (addsub_imm->shift) {
    codegen_fprintf(file, " %reg, %reg, %imm, lsl %imm", addsub_imm->dest,
                    addsub_imm->source, addsub_imm->imm,
                    addsub_imm->shift * 12);
  } else {
    codegen_fprintf(file, " %reg, %reg, %imm", addsub_imm->dest,
                    addsub_imm->source, addsub_imm->imm);
  }
}

static void
debug_print_reg_1_source(FILE *file,
                         const struct aarch64_reg_1_source *reg_1_source) {
  codegen_fprintf(file, " %reg, %reg", reg_1_source->dest,
                  reg_1_source->source);
}

static void
debug_print_vreg_1_source(FILE *file,
                          const struct aarch64_vreg_1_source *reg_1_source) {
  codegen_fprintf(file, "%ar %reg, %reg", reg_1_source->arrangement,
                  reg_1_source->dest, reg_1_source->source);
}

static void debug_print_fcmp(FILE *file, const struct aarch64_fcmp *fcmp) {
  codegen_fprintf(file, " %reg, %reg", fcmp->lhs, fcmp->rhs);
}

static void debug_print_fcmp_zero(FILE *file,
                                  const struct aarch64_fcmp_zero *fcmp_zero) {
  codegen_fprintf(file, " %reg, #0.0", fcmp_zero->lhs);
}

static void
debug_print_reg_2_source(FILE *file,
                         const struct aarch64_reg_2_source *reg_2_source) {
  codegen_fprintf(file, " %reg, %reg, %reg", reg_2_source->dest,
                  reg_2_source->lhs, reg_2_source->rhs);
}

static void
debug_print_bitfield_imm(FILE *file,
                         const struct aarch64_bitfield *bitfield_imm) {
  codegen_fprintf(file, " %reg, %reg, %imm, %imm", bitfield_imm->dest,
                  bitfield_imm->source, bitfield_imm->immr, bitfield_imm->imms);
}

static void debug_print_conditional_select(
    FILE *file, const struct aarch64_conditional_select *conditional_select) {
  codegen_fprintf(file, " %reg, %reg, %reg, %cond", conditional_select->dest,
                  conditional_select->false_source,
                  conditional_select->true_source, conditional_select->cond);
}

static void debug_print_conditional_branch(
    FILE *file, const struct aarch64_conditional_branch *conditional_branch) {
  codegen_fprintf(file, ".%cond %instr", conditional_branch->cond,
                  conditional_branch->target->cg_basicblock->first);
}

static void debug_print_branch(FILE *file,
                               const struct aarch64_branch *branch) {
  codegen_fprintf(file, " %instr", branch->target->cg_basicblock->first);
}

static void
debug_print_branch_reg(FILE *file,
                       const struct aarch64_branch_reg *branch_reg) {
  codegen_fprintf(file, " %reg", branch_reg->target);
}

static void debug_print_compare_and_branch(
    FILE *file, const struct aarch64_compare_and_branch *compare_and_branch) {
  codegen_fprintf(file, " %reg, %instr", compare_and_branch->cmp,
                  compare_and_branch->target->cg_basicblock->first);
}

static void debug_print_load(FILE *file, const struct aarch64_load *load,
                             enum aarch64_op_size op_size) {
  codegen_fprintf(file, " %reg, %addr", load->dest, load->addr, load->offset,
                  load->extend, load->amount, op_size);
}

static void debug_print_store(FILE *file, const struct aarch64_store *store,
                              enum aarch64_op_size op_size) {
  codegen_fprintf(file, " %reg, %addr", store->source, store->addr,
                  store->offset, store->extend, store->amount, op_size);
}

static void debug_print_load_imm(FILE *file,
                                 const struct aarch64_load_imm *load_imm) {
  codegen_fprintf(file, " %reg, %addr_imm", load_imm->dest, load_imm->mode,
                  load_imm->addr, load_imm->imm);
}

static void debug_print_store_imm(FILE *file,
                                  const struct aarch64_store_imm *store_imm) {
  codegen_fprintf(file, " %reg, %addr_imm", store_imm->source, store_imm->mode,
                  store_imm->addr, store_imm->imm);
}

static void
debug_print_load_pair_imm(FILE *file,
                          const struct aarch64_load_pair_imm *load_pair_imm) {
  codegen_fprintf(file, " %reg, %reg, %addr_imm", load_pair_imm->dest[0],
                  load_pair_imm->dest[1], load_pair_imm->mode,
                  load_pair_imm->addr, load_pair_imm->imm);
}

static void debug_print_store_pair_imm(
    FILE *file, const struct aarch64_store_pair_imm *store_pair_imm) {
  codegen_fprintf(file, " %reg, %reg, %addr_imm", store_pair_imm->source[0],
                  store_pair_imm->source[1], store_pair_imm->mode,
                  store_pair_imm->addr, store_pair_imm->imm);
}

static void debug_print_mov_imm(FILE *file,
                                const struct aarch64_mov_imm *mov_imm) {
  if (mov_imm->shift) {
    codegen_fprintf(file, " %reg, %imm, lsl %imm", mov_imm->dest, mov_imm->imm,
                    mov_imm->shift);
  } else {
    codegen_fprintf(file, " %reg, %imm", mov_imm->dest, mov_imm->imm);
  }
}

static void debug_print_fma(FILE *file, const struct aarch64_fma *fma) {
  codegen_fprintf(file, " %reg, %reg, %reg, %reg", fma->dest, fma->lhs,
                  fma->rhs, fma->addsub);
}

static void print_instr(FILE *file, UNUSED_ARG(const struct cg_func *func),
                        const struct cg_instr *instr) {

  switch (instr->aarch64->ty) {
  case AARCH64_INSTR_TY_NOP:
    fprintf(file, "nop");
    break;
  case AARCH64_INSTR_TY_REV:
    fprintf(file, "rev");
    debug_print_reg_1_source(file, &instr->aarch64->rev);
    break;
  case AARCH64_INSTR_TY_REV16:
    fprintf(file, "rev16");
    debug_print_reg_1_source(file, &instr->aarch64->rev16);
    break;
  case AARCH64_INSTR_TY_CLZ:
    fprintf(file, "clz");
    debug_print_reg_1_source(file, &instr->aarch64->clz);
    break;
  case AARCH64_INSTR_TY_RBIT:
    fprintf(file, "rbit");
    debug_print_reg_1_source(file, &instr->aarch64->rbit);
    break;
  case AARCH64_INSTR_TY_SBFM:
    fprintf(file, "sbfm");
    debug_print_bitfield_imm(file, &instr->aarch64->sbfm);
    break;
  case AARCH64_INSTR_TY_BFM:
    fprintf(file, "bfm");
    debug_print_bitfield_imm(file, &instr->aarch64->bfm);
    break;
  case AARCH64_INSTR_TY_UBFM:
    fprintf(file, "ubfm");
    debug_print_bitfield_imm(file, &instr->aarch64->ubfm);
    break;
  case AARCH64_INSTR_TY_AND:
    fprintf(file, "and");
    debug_print_logical_reg(file, &instr->aarch64->and);
    break;
  case AARCH64_INSTR_TY_ANDS:
    fprintf(file, "ands");
    debug_print_logical_reg(file, &instr->aarch64->ands);
    break;
  case AARCH64_INSTR_TY_ORR:
    if ((is_zero_reg(instr->aarch64->orr.lhs) ||
         is_zero_reg(instr->aarch64->orr.rhs)) &&
        instr->aarch64->orr.shift == 0) {
      codegen_fprintf(file, "mov %reg, %reg", instr->aarch64->orr.dest,
                      is_zero_reg(instr->aarch64->orr.lhs)
                          ? instr->aarch64->orr.rhs
                          : instr->aarch64->orr.lhs);
    } else {
      fprintf(file, "orr");
      debug_print_logical_reg(file, &instr->aarch64->orr);
    }
    break;
  case AARCH64_INSTR_TY_ORN:
    fprintf(file, "orn");
    debug_print_logical_reg(file, &instr->aarch64->orn);
    break;
  case AARCH64_INSTR_TY_EOR:
    fprintf(file, "eor");
    debug_print_logical_reg(file, &instr->aarch64->eor);
    break;
  case AARCH64_INSTR_TY_EON:
    fprintf(file, "eon");
    debug_print_logical_reg(file, &instr->aarch64->eon);
    break;
  case AARCH64_INSTR_TY_EOR_IMM:
    fprintf(file, "eor");
    debug_print_logical_imm(file, &instr->aarch64->eor_imm);
    break;
  case AARCH64_INSTR_TY_ORR_IMM:
    fprintf(file, "orr");
    debug_print_logical_imm(file, &instr->aarch64->orr_imm);
    break;
  case AARCH64_INSTR_TY_ANDS_IMM:
    fprintf(file, "ands");
    debug_print_logical_imm(file, &instr->aarch64->ands_imm);
    break;
  case AARCH64_INSTR_TY_AND_IMM:
    fprintf(file, "and");
    debug_print_logical_imm(file, &instr->aarch64->and_imm);
    break;
  case AARCH64_INSTR_TY_ADDS:
    fprintf(file, "adds");
    debug_print_addsub_reg(file, &instr->aarch64->adds);
    break;
  case AARCH64_INSTR_TY_ADDS_EXT:
    fprintf(file, "adds");
    debug_print_addsub_ext(file, &instr->aarch64->adds_ext);
    break;
  case AARCH64_INSTR_TY_ADD_EXT:
    fprintf(file, "add");
    debug_print_addsub_ext(file, &instr->aarch64->add_ext);
    break;
  case AARCH64_INSTR_TY_ADD:
    fprintf(file, "add");
    debug_print_addsub_reg(file, &instr->aarch64->add);
    break;
  case AARCH64_INSTR_TY_ADD_IMM:
    fprintf(file, "add");
    debug_print_addsub_imm(file, &instr->aarch64->add_imm);
    break;
  case AARCH64_INSTR_TY_ADR:
    fprintf(file, "adr");
    debug_print_addr_imm(file, &instr->aarch64->adr);
    break;
  case AARCH64_INSTR_TY_ADRP:
    fprintf(file, "adrp");
    debug_print_addr_imm(file, &instr->aarch64->adrp);
    break;
  case AARCH64_INSTR_TY_ASRV:
    fprintf(file, "asrv");
    debug_print_reg_2_source(file, &instr->aarch64->asrv);
    break;
  case AARCH64_INSTR_TY_B:
    fprintf(file, "b");
    debug_print_branch(file, &instr->aarch64->b);
    break;
  case AARCH64_INSTR_TY_BL:
    fprintf(file, "bl");

    if (instr->aarch64->bl.target) {
      debug_print_branch(file, &instr->aarch64->bl);
    } else {
      fprintf(file, " <unknown>");
    }
    break;
  case AARCH64_INSTR_TY_BC_COND:
    fprintf(file, "bc");
    debug_print_conditional_branch(file, &instr->aarch64->bc_cond);
    break;
  case AARCH64_INSTR_TY_B_COND:
    fprintf(file, "b");
    debug_print_conditional_branch(file, &instr->aarch64->b_cond);
    break;
  case AARCH64_INSTR_TY_CBNZ:
    fprintf(file, "cbnz");
    debug_print_compare_and_branch(file, &instr->aarch64->cbnz);
    break;
  case AARCH64_INSTR_TY_CBZ:
    fprintf(file, "cbz");
    debug_print_compare_and_branch(file, &instr->aarch64->cbz);
    break;
  case AARCH64_INSTR_TY_CSEL:
    fprintf(file, "csel");
    debug_print_conditional_select(file, &instr->aarch64->csel);
    break;
  case AARCH64_INSTR_TY_CSINC:
    fprintf(file, "csinc");
    debug_print_conditional_select(file, &instr->aarch64->csinc);
    break;
  case AARCH64_INSTR_TY_CSINV:
    fprintf(file, "csinv");
    debug_print_conditional_select(file, &instr->aarch64->csinv);
    break;
  case AARCH64_INSTR_TY_CSNEG:
    fprintf(file, "csneg");
    debug_print_conditional_select(file, &instr->aarch64->csneg);
    break;
  case AARCH64_INSTR_TY_LOAD_IMM:
    fprintf(file, "ldr");
    debug_print_load_imm(file, &instr->aarch64->ldr_imm);
    break;
  case AARCH64_INSTR_TY_LOAD_BYTE_IMM:
    fprintf(file, "ldrb");
    debug_print_load_imm(file, &instr->aarch64->ldrb_imm);
    break;
  case AARCH64_INSTR_TY_LOAD_HALF_IMM:
    fprintf(file, "ldrh");
    debug_print_load_imm(file, &instr->aarch64->ldrh_imm);
    break;
  case AARCH64_INSTR_TY_LOAD:
    fprintf(file, "ldr");
    debug_print_load(file, &instr->aarch64->ldr,
                     instr->aarch64->ldr.dest.ty == AARCH64_REG_TY_X
                         ? AARCH64_OP_SIZE_DWORD
                         : AARCH64_OP_SIZE_WORD);
    break;
  case AARCH64_INSTR_TY_LOAD_BYTE:
    fprintf(file, "ldrb");
    debug_print_load(file, &instr->aarch64->ldrb, AARCH64_OP_SIZE_BYTE);
    break;
  case AARCH64_INSTR_TY_LOAD_HALF:
    fprintf(file, "ldrh");
    debug_print_load(file, &instr->aarch64->ldrh, AARCH64_OP_SIZE_HALF);
    break;
  case AARCH64_INSTR_TY_LOAD_PAIR_IMM:
    fprintf(file, "ldp");
    debug_print_load_pair_imm(file, &instr->aarch64->ldp_imm);
    break;
  case AARCH64_INSTR_TY_LSLV:
    fprintf(file, "lslv");
    debug_print_reg_2_source(file, &instr->aarch64->lslv);
    break;
  case AARCH64_INSTR_TY_LSRV:
    fprintf(file, "lsrv");
    debug_print_reg_2_source(file, &instr->aarch64->lsrv);
    break;
  case AARCH64_INSTR_TY_MADD:
    if (is_zero_reg(instr->aarch64->madd.addsub)) {
      codegen_fprintf(file, "mul %reg, %reg, %reg", instr->aarch64->madd.dest,
                      instr->aarch64->madd.lhs, instr->aarch64->madd.rhs);
    } else {
      fprintf(file, "madd");
      debug_print_fma(file, &instr->aarch64->madd);
    }
    break;
  case AARCH64_INSTR_TY_MOVN:
    fprintf(file, "movn");
    debug_print_mov_imm(file, &instr->aarch64->movn);
    break;
  case AARCH64_INSTR_TY_MOVZ:
    fprintf(file, "movz");
    debug_print_mov_imm(file, &instr->aarch64->movz);
    break;
  case AARCH64_INSTR_TY_MOVK:
    fprintf(file, "movk");
    debug_print_mov_imm(file, &instr->aarch64->movk);
    break;
  case AARCH64_INSTR_TY_CNT:
    fprintf(file, "cnt");
    debug_print_vreg_1_source(file, &instr->aarch64->cnt);
    break;
  case AARCH64_INSTR_TY_ADDV:
    fprintf(file, "addv");
    debug_print_vreg_1_source(file, &instr->aarch64->addv);
    break;
  case AARCH64_INSTR_TY_FMOV:
    fprintf(file, "fmov");
    debug_print_reg_1_source(file, &instr->aarch64->fmov);
    break;
  case AARCH64_INSTR_TY_FNEG:
    fprintf(file, "fneg");
    debug_print_reg_1_source(file, &instr->aarch64->fneg);
    break;
  case AARCH64_INSTR_TY_FCVT:
    fprintf(file, "fcvt");
    debug_print_reg_1_source(file, &instr->aarch64->fcvt);
    break;
  case AARCH64_INSTR_TY_UCVTF:
    fprintf(file, "ucvtf");
    debug_print_reg_1_source(file, &instr->aarch64->ucvtf);
    break;
  case AARCH64_INSTR_TY_SCVTF:
    fprintf(file, "scvtf");
    debug_print_reg_1_source(file, &instr->aarch64->scvtf);
    break;
  case AARCH64_INSTR_TY_MSUB:
    fprintf(file, "msub");
    debug_print_fma(file, &instr->aarch64->msub);
    break;
  case AARCH64_INSTR_TY_BR:
    fprintf(file, "br");
    debug_print_branch_reg(file, &instr->aarch64->br);
    break;
  case AARCH64_INSTR_TY_BLR:
    fprintf(file, "blr");
    debug_print_branch_reg(file, &instr->aarch64->blr);
    break;
  case AARCH64_INSTR_TY_RET:
    fprintf(file, "ret");
    if (!reg_eq(instr->aarch64->ret.target, RET_PTR_REG)) {
      debug_print_branch_reg(file, &instr->aarch64->ret);
    }
    break;
  case AARCH64_INSTR_TY_RORV:
    fprintf(file, "rorv");
    debug_print_reg_2_source(file, &instr->aarch64->rorv);
    break;
  case AARCH64_INSTR_TY_SDIV:
    fprintf(file, "sdiv");
    debug_print_reg_2_source(file, &instr->aarch64->sdiv);
    break;
  case AARCH64_INSTR_TY_STORE_IMM:
    fprintf(file, "str");
    debug_print_store_imm(file, &instr->aarch64->str_imm);
    break;
  case AARCH64_INSTR_TY_STORE_BYTE_IMM:
    fprintf(file, "strb");
    debug_print_store_imm(file, &instr->aarch64->strb_imm);
    break;
  case AARCH64_INSTR_TY_STORE_HALF_IMM:
    fprintf(file, "strh");
    debug_print_store_imm(file, &instr->aarch64->strh_imm);
    break;
  case AARCH64_INSTR_TY_STORE:
    fprintf(file, "str");
    debug_print_store(file, &instr->aarch64->str,
                      instr->aarch64->str.source.ty == AARCH64_REG_TY_X
                          ? AARCH64_OP_SIZE_DWORD
                          : AARCH64_OP_SIZE_WORD);
    break;
  case AARCH64_INSTR_TY_STORE_BYTE:
    fprintf(file, "strb");
    debug_print_store(file, &instr->aarch64->strb, AARCH64_OP_SIZE_BYTE);
    break;
  case AARCH64_INSTR_TY_STORE_HALF:
    fprintf(file, "strh");
    debug_print_store(file, &instr->aarch64->strh, AARCH64_OP_SIZE_HALF);
    break;
  case AARCH64_INSTR_TY_STORE_PAIR_IMM:
    fprintf(file, "stp");
    debug_print_store_pair_imm(file, &instr->aarch64->stp_imm);
    break;
  case AARCH64_INSTR_TY_SUBS:
    fprintf(file, "subs");
    debug_print_addsub_reg(file, &instr->aarch64->subs);
    break;
  case AARCH64_INSTR_TY_SUBS_EXT:
    fprintf(file, "subs");
    debug_print_addsub_ext(file, &instr->aarch64->subs_ext);
    break;
  case AARCH64_INSTR_TY_SUB_EXT:
    fprintf(file, "sub");
    debug_print_addsub_ext(file, &instr->aarch64->sub_ext);
    break;
  case AARCH64_INSTR_TY_SUB:
    fprintf(file, "sub");
    debug_print_addsub_reg(file, &instr->aarch64->sub);
    break;
  case AARCH64_INSTR_TY_SUB_IMM:
    fprintf(file, "sub");
    debug_print_addsub_imm(file, &instr->aarch64->sub_imm);
    break;
  case AARCH64_INSTR_TY_SUBS_IMM:
    fprintf(file, "subs");
    debug_print_addsub_imm(file, &instr->aarch64->subs_imm);
    break;
  case AARCH64_INSTR_TY_UDIV:
    fprintf(file, "udiv");
    debug_print_reg_2_source(file, &instr->aarch64->udiv);
    break;
  case AARCH64_INSTR_TY_ADDS_IMM:
    fprintf(file, "adds");
    debug_print_addsub_imm(file, &instr->aarch64->adds_imm);
    break;
  case AARCH64_INSTR_TY_FCMP:
    fprintf(file, "fcmp");
    debug_print_fcmp(file, &instr->aarch64->fcmp);
    break;
  case AARCH64_INSTR_TY_FCMP_ZERO:
    fprintf(file, "fcmp");
    debug_print_fcmp_zero(file, &instr->aarch64->fcmp_zero);
    break;
  case AARCH64_INSTR_TY_FADD:
    fprintf(file, "fadd");
    debug_print_reg_2_source(file, &instr->aarch64->fadd);
    break;
  case AARCH64_INSTR_TY_FMUL:
    fprintf(file, "fmul");
    debug_print_reg_2_source(file, &instr->aarch64->fmul);
    break;
  case AARCH64_INSTR_TY_FDIV:
    fprintf(file, "fdiv");
    debug_print_reg_2_source(file, &instr->aarch64->fdiv);
    break;
  case AARCH64_INSTR_TY_FSUB:
    fprintf(file, "fsub");
    debug_print_reg_2_source(file, &instr->aarch64->fsub);
    break;
  case AARCH64_INSTR_TY_FABS:
    fprintf(file, "fabs");
    debug_print_reg_1_source(file, &instr->aarch64->fabs);
    break;
  case AARCH64_INSTR_TY_FSQRT:
    fprintf(file, "fsqrt");
    debug_print_reg_1_source(file, &instr->aarch64->fsqrt);
    break;
  case AARCH64_INSTR_TY_FMINNM:
    fprintf(file, "fminnm");
    debug_print_reg_2_source(file, &instr->aarch64->fminnm);
    break;
  case AARCH64_INSTR_TY_FMAXNM:
    fprintf(file, "fmaxnm");
    debug_print_reg_2_source(file, &instr->aarch64->fmaxnm);
    break;
  }
}

static void aarch64_debug_print_func(FILE *file, struct cg_entry *entry) {
  struct cg_func *func = &entry->func;

  fprintf(file, "\nFUNCTION: %s\n", entry->name);
  fprintf(file, "  prologue: %s\n", entry->func.prologue ? "true" : "false");
  fprintf(file, "  stack_size: %zu\n", entry->func.stack_size);
  fprintf(file, "\n");

  int op_pad = /* guess */ 50;

  bool supports_pos = ftell(file) != -1;

  size_t offset = 0;
  struct cg_basicblock *basicblock = func->first;
  while (basicblock) {
    fprintf(file, "\nBB @ %03zu", basicblock->id);
    if (basicblock->ir_basicblock) {
      fprintf(file, " (ir_basicblock %zu)", basicblock->ir_basicblock->id);
    }
    fprintf(file, "\n");

    struct cg_instr *instr = basicblock->first;
    while (instr) {
      long pos = ftell(file);

      fprintf(file, "%04zu: ", offset++);
      print_instr(file, func, instr);

      if (supports_pos && ftell(file) == pos) {
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

    basicblock = basicblock->succ;
  }
}

void aarch64_debug_print_codegen(FILE *file, struct cg_unit *unit) {
  DEBUG_ASSERT(unit->ty == CODEGEN_UNIT_TY_AARCH64, "expected aarch64");

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct cg_entry *entry = &unit->entries[i];

    if (entry->ty != CG_ENTRY_TY_FUNC) {
      fprintf(file, "DATA: %s\n\n", entry->name);
      continue;
    }

    aarch64_debug_print_func(file, entry);

    fprintf(file, "\n");
  }
}
