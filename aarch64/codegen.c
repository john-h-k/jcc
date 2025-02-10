#include "codegen.h"

#include "../aarch64.h"
#include "../bit_twiddle.h"
#include "../bitset.h"
#include "../util.h"
#include "../vector.h"
#include "isa.h"

#include <stdio.h>

#define MOV_ALIAS(dest_reg, source_reg)                                        \
  (struct aarch64_instr) {                                                     \
    .ty = AARCH64_INSTR_TY_ORR, .orr = {                                       \
      .lhs = zero_reg_for_ty(dest_reg.ty),                                     \
      .rhs = (source_reg),                                                     \
      .dest = (dest_reg),                                                      \
      .imm6 = 0                                                                \
    }                                                                          \
  }

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
  }

  BUG("bad size %zu", size);
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
  }

  BUG("bad size %zu", size);
}

static enum aarch64_reg_ty reg_ty_for_size(enum aarch64_reg_class reg_class,
                                           size_t size) {
  switch (reg_class) {
  case AARCH64_REG_CLASS_GP:
    return gp_reg_ty_for_size(size);
  case AARCH64_REG_CLASS_FP:
    return fp_reg_ty_for_size(size);
  }
}

size_t reg_size(enum aarch64_reg_ty reg_ty) {
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

bool is_return_reg(struct aarch64_reg reg) { return reg.idx == 0; }

bool is_zero_reg(struct aarch64_reg reg) {
  // this is a bit dodgy as it can also be SP in this context
  return aarch64_reg_ty_is_gp(reg.ty) && reg.idx == 31;
}

bool reg_eq(struct aarch64_reg l, struct aarch64_reg r) {
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

static struct aarch64_reg return_reg_for_ty(enum aarch64_reg_ty reg_ty) {
  return (struct aarch64_reg){.ty = reg_ty, .idx = 0};
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
    return AARCH64_INSTR_CLASS_REG_1_SOURCE;
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
  unsigned long long saved_gp_registers;
  unsigned long long saved_fp_registers;
};

struct codegen_state {
  struct arena_allocator *arena;

  const struct target *target;
  struct codegen_function *func;
  struct ir_func *ir;
  struct aarch64_prologue_info prologue_info;

  struct aarch64_reg ssp;

  size_t stack_args_size;
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

static ssize_t get_lcl_stack_offset(const struct codegen_state *state,
                                    const struct ir_op *op,
                                    const struct ir_lcl *lcl) {
  ssize_t offset = state->stack_args_size + lcl->offset;

  if (!op) {
    return offset;
  }

  struct ir_var_ty_info info = var_ty_info(state->ir->unit, &op->var_ty);
  DEBUG_ASSERT(offset % info.size == 0,
               "stack offset not divisible by type size");

  return offset / info.size;
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
struct aarch64_reg get_full_reg_for_ir_reg(struct ir_reg reg) {
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
  }
}

static struct aarch64_reg codegen_reg(struct ir_op *op) {
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

static void codegen_mov_op(struct codegen_state *state, struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);

  if (op->mov.value->reg.ty == IR_REG_TY_FLAGS) {
    struct aarch64_reg zero_reg = zero_reg_for_ty(dest.ty);

    struct instr *csinc = alloc_instr(state->func);
    csinc->aarch64->ty = AARCH64_INSTR_TY_CSINC;
    csinc->aarch64->csinc = (struct aarch64_conditional_select){
        .dest = dest,
        .cond = invert_cond(get_cond_for_op(op->mov.value)),
        .true_source = zero_reg,
        .false_source = zero_reg,
    };

    return;
  }

  struct aarch64_reg source = codegen_reg(op->mov.value);

  struct instr *instr = alloc_instr(state->func);
  if (aarch64_reg_ty_is_gp(source.ty) && aarch64_reg_ty_is_gp(dest.ty)) {
    *instr->aarch64 = MOV_ALIAS(dest, source);
  } else {
    // one is floating
    *instr->aarch64 = FP_MOV_ALIAS(dest, source);
  }
}

// TODO: we should remove load/store lcl/glb ops and lower all addressing
// requirements earlier

static enum aarch64_instr_ty load_ty_for_op(struct ir_op *op) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
    return AARCH64_INSTR_TY_LOAD_BYTE_IMM;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return AARCH64_INSTR_TY_LOAD_HALF_IMM;
  } else {
    return AARCH64_INSTR_TY_LOAD_IMM;
  }
}

static enum aarch64_instr_ty store_ty_for_op(struct ir_op *op) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
    return AARCH64_INSTR_TY_STORE_BYTE_IMM;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return AARCH64_INSTR_TY_STORE_HALF_IMM;
  } else {
    return AARCH64_INSTR_TY_STORE_IMM;
  }
}

static void codegen_add_imm(struct codegen_state *state,
                            struct aarch64_reg dest, struct aarch64_reg source,
                            unsigned long long value);

static void codegen_load_lcl_op(struct codegen_state *state, struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);
  struct ir_lcl *lcl = op->load.lcl;

  simm_t offset = get_lcl_stack_offset(state, op, lcl);

  if (offset > MAX_IMM_SIZE) {
    size_t raw_offset = get_lcl_stack_offset(state, NULL, lcl);
    codegen_add_imm(state, state->ssp, STACK_PTR_REG, raw_offset);

    struct instr *instr = alloc_instr(state->func);
    instr->aarch64->ty = load_ty_for_op(op);
    instr->aarch64->load_imm =
        (struct aarch64_load_imm){.dest = dest,
                                  .addr = state->ssp,
                                  .imm = 0,
                                  .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  } else {
    struct instr *instr = alloc_instr(state->func);
    instr->aarch64->ty = load_ty_for_op(op);
    instr->aarch64->load_imm =
        (struct aarch64_load_imm){.dest = dest,
                                  .addr = STACK_PTR_REG,
                                  .imm = offset,
                                  .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  }
}

static void codegen_store_lcl_op(struct codegen_state *state,
                                 struct ir_op *op) {
  struct aarch64_reg source = codegen_reg(op->store.value);
  struct ir_lcl *lcl = op->store.lcl;

  size_t offset = get_lcl_stack_offset(state, op->store.value, lcl);

  if (offset > MAX_IMM_SIZE) {
    size_t raw_offset = get_lcl_stack_offset(state, NULL, lcl);
    codegen_add_imm(state, state->ssp, STACK_PTR_REG, raw_offset);

    struct instr *instr = alloc_instr(state->func);
    instr->aarch64->ty = load_ty_for_op(op);
    instr->aarch64->str_imm =
        (struct aarch64_store_imm){.source = source,
                                   .addr = state->ssp,
                                   .imm = 0,
                                   .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  } else {
    struct instr *instr = alloc_instr(state->func);
    instr->aarch64->ty = store_ty_for_op(op->store.value);
    instr->aarch64->str_imm =
        (struct aarch64_store_imm){.source = source,
                                   .addr = STACK_PTR_REG,
                                   .imm = offset,
                                   .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  }
}

static void codegen_load_addr_op(struct codegen_state *state,
                                 struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct aarch64_reg dest = codegen_reg(op);

  if (op->load.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr = op->load.addr;

    simm_t imm;
    if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
      imm = get_lcl_stack_offset(state, op, addr->addr.lcl);
    } else {
      BUG("can't CONTAIN operand in load_addr node");
    }

    instr->aarch64->ty = load_ty_for_op(op);
    instr->aarch64->ldr_imm =
        (struct aarch64_load_imm){.dest = dest,
                                  .addr = STACK_PTR_REG,
                                  .imm = imm,
                                  .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  } else {
    struct aarch64_reg addr = codegen_reg(op->load.addr);
    instr->aarch64->ty = load_ty_for_op(op);
    instr->aarch64->ldr_imm =
        (struct aarch64_load_imm){.dest = dest,
                                  .addr = addr,
                                  .imm = 0,
                                  .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  }
}

static void codegen_store_addr_op(struct codegen_state *state,
                                  struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct aarch64_reg source = codegen_reg(op->store.value);

  if (op->store.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr = op->store.addr;

    simm_t imm;
    if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
      imm = get_lcl_stack_offset(state, op->store.value, addr->addr.lcl);
    } else {
      BUG("can't CONTAIN operand in store_addr node");
    }

    instr->aarch64->ty = store_ty_for_op(op->store.value);
    instr->aarch64->str_imm =
        (struct aarch64_store_imm){.source = source,
                                   .addr = STACK_PTR_REG,
                                   .imm = imm,
                                   .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  } else {
    struct aarch64_reg addr = codegen_reg(op->store.addr);
    instr->aarch64->ty = store_ty_for_op(op->store.value);
    instr->aarch64->str_imm =
        (struct aarch64_store_imm){.source = source,
                                   .addr = addr,
                                   .imm = 0,
                                   .mode = AARCH64_ADDRESSING_MODE_OFFSET};
  }
}

static void codegen_bitfield_insert(struct codegen_state *state,
                                    struct ir_op *op) {
  struct ir_bitfield bitfield = op->bitfield_insert.bitfield;

  struct aarch64_reg value_reg = codegen_reg(op->bitfield_insert.value);
  struct aarch64_reg target_reg = codegen_reg(op->bitfield_insert.target);
  struct aarch64_reg dest = codegen_reg(op);

  if (target_reg.idx != dest.idx) {
    struct instr *mov = alloc_instr(state->func);
    *mov->aarch64 = MOV_ALIAS(dest, target_reg);
  }

  struct instr *instr = alloc_instr(state->func);
  instr->aarch64->ty = AARCH64_INSTR_TY_BFM;
  instr->aarch64->bfm = (struct aarch64_bitfield){
      .dest = dest,
      .source = value_reg,
      .imms = bitfield.width - 1,
      .immr = bitfield.offset,
  };
}

static void codegen_bitfield_extract(struct codegen_state *state,
                                     struct ir_op *op) {
  struct ir_bitfield bitfield = op->bitfield_extract.bitfield;

  struct aarch64_reg value_reg = codegen_reg(op->bitfield_extract.value);
  struct aarch64_reg dest = codegen_reg(op);

  struct instr *instr = alloc_instr(state->func);
  instr->aarch64->ty = AARCH64_INSTR_TY_UBFM;
  instr->aarch64->ubfm = (struct aarch64_bitfield){.dest = dest,
                                                   .source = value_reg,
                                                   .imms = bitfield.width - 1,
                                                   .immr = bitfield.offset};
}

static void codegen_load_op(struct codegen_state *state, struct ir_op *op) {
  switch (op->load.ty) {
  case IR_OP_LOAD_TY_LCL:
    codegen_load_lcl_op(state, op);
    break;
  case IR_OP_LOAD_TY_ADDR:
    codegen_load_addr_op(state, op);
    break;
  case IR_OP_LOAD_TY_GLB:
    BUG("load.glb should have been lowered");
  }
}

static void codegen_store_op(struct codegen_state *state, struct ir_op *op) {
  switch (op->load.ty) {
  case IR_OP_STORE_TY_LCL:
    codegen_store_lcl_op(state, op);
    break;
  case IR_OP_STORE_TY_ADDR:
    codegen_store_addr_op(state, op);
    break;
  case IR_OP_STORE_TY_GLB:
    BUG("store.glb should have been lowered");
  }
}

// this method assumes it can safely you any non-argument volatile registers
static void codegen_mem_copy_volatile(struct codegen_state *state,
                                      struct aarch64_reg source,
                                      size_t source_offset,
                                      struct aarch64_reg dest,
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
    struct instr *load = alloc_instr(state->func);
    load->aarch64->ty = AARCH64_INSTR_TY_LOAD_PAIR_IMM;
    load->aarch64->load_pair_imm =
        (struct aarch64_load_pair_imm){.mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                       .imm = (head + source_offset) / 16,
                                       .addr = source,
                                       .dest = {
                                           {.ty = AARCH64_REG_TY_Q, .idx = 10},
                                           {.ty = AARCH64_REG_TY_Q, .idx = 11},
                                       }};

    struct instr *store = alloc_instr(state->func);
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

    struct instr *load = alloc_instr(state->func);
    load->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
    load->aarch64->load_imm = (struct aarch64_load_imm){
        .mode = AARCH64_ADDRESSING_MODE_OFFSET,
        .imm = (head + source_offset) / chunk,
        .addr = source,
        .dest = {.ty = ty, .idx = 10},
    };

    struct instr *store = alloc_instr(state->func);
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

static void codegen_add_imm(struct codegen_state *state,
                            struct aarch64_reg dest, struct aarch64_reg source,
                            unsigned long long value) {

  if (value & ~MASK_LO(unsigned long long, 24)) {
    TODO("imm >24 bits");
  }

  unsigned long long lo = value & MASK_LO(unsigned long long, 12);
  unsigned long long hi = (value >> 12) & MASK_LO(unsigned long long, 12);

  struct instr *instr = alloc_instr(state->func);
  instr->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
  instr->aarch64->add_imm = (struct aarch64_addsub_imm){
      .dest = dest,
      .source = source,
      .imm = lo,
      .shift = 0,
  };

  if (hi) {
    instr = alloc_instr(state->func);
    instr->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
    instr->aarch64->add_imm = (struct aarch64_addsub_imm){
        .dest = dest,
        .source = dest,
        .imm = hi,
        .shift = 1,
    };
  }
}

static void codegen_sub_imm(struct codegen_state *state,
                            struct aarch64_reg dest, struct aarch64_reg source,
                            unsigned long long value) {

  if (value & ~MASK_LO(unsigned long long, 24)) {
    TODO("imm >24 bits");
  }

  unsigned long long lo = value & MASK_LO(unsigned long long, 12);
  unsigned long long hi = (value >> 12) & MASK_LO(unsigned long long, 12);

  struct instr *instr = alloc_instr(state->func);
  instr->aarch64->ty = AARCH64_INSTR_TY_SUB_IMM;
  instr->aarch64->add_imm = (struct aarch64_addsub_imm){
      .dest = dest,
      .source = dest,
      .imm = lo,
      .shift = 0,
  };

  if (hi) {
    instr = alloc_instr(state->func);
    instr->aarch64->ty = AARCH64_INSTR_TY_SUB_IMM;
    instr->aarch64->add_imm = (struct aarch64_addsub_imm){
        .dest = dest,
        .source = source,
        .imm = hi,
        .shift = 1,
    };
  }
}
static void codegen_addr_op(struct codegen_state *state, struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);

  switch (op->addr.ty) {
  case IR_OP_ADDR_TY_LCL: {
    struct ir_lcl *lcl = op->addr.lcl;

    // op is NULL as we want the absolute offset
    size_t offset = get_lcl_stack_offset(state, NULL, lcl);

    codegen_add_imm(state, dest, STACK_PTR_REG, offset);

    break;
  }
  case IR_OP_ADDR_TY_GLB: {
    struct ir_glb *glb = op->addr.glb;

    struct instr *adrp = alloc_instr(state->func);
    adrp->aarch64->ty = AARCH64_INSTR_TY_ADRP;
    adrp->aarch64->adrp = (struct aarch64_addr_imm){.dest = dest, .imm = 0};

    adrp->reloc = arena_alloc(state->func->unit->arena, sizeof(*adrp->reloc));
    *adrp->reloc = (struct relocation){
        .ty = glb->def_ty == IR_GLB_DEF_TY_DEFINED ? RELOCATION_TY_LOCAL_PAIR
                                                   : RELOCATION_TY_UNDEF_PAIR,
        .symbol_index = glb->id,
        .address = 0,
        .size = 0};

    if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
      struct instr *add = alloc_instr(state->func);
      add->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
      add->aarch64->add_imm = (struct aarch64_addsub_imm){
          .dest = dest,
          .source = dest,
          .imm = 0,
      };
    } else {
      struct instr *ldr = alloc_instr(state->func);
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

static void codegen_br_cond_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  // AArch64 requires turning `br.cond <true> <false>` into 2 instructions
  // we represent this as just the `true` part of the `br.cond`, and then a `br`
  // after branching to the false target

  struct ir_basicblock *true_target = op->stmt->basicblock->split.true_target;
  struct ir_basicblock *false_target = op->stmt->basicblock->split.false_target;

  if (op->br_cond.cond->reg.ty == IR_REG_TY_FLAGS) {
    // emit based on flags
    enum aarch64_cond cond = get_cond_for_op(op->br_cond.cond);
    instr->aarch64->ty = AARCH64_INSTR_TY_B_COND;
    instr->aarch64->b_cond = (struct aarch64_conditional_branch){
        .cond = cond, .target = true_target};
  } else {
    struct aarch64_reg cmp_reg = codegen_reg(op->br_cond.cond);

    instr->aarch64->ty = AARCH64_INSTR_TY_CBNZ;
    instr->aarch64->cbnz = (struct aarch64_compare_and_branch){
        .cmp = cmp_reg, .target = true_target};
  }

  // now generate the `br`
  struct instr *br = alloc_instr(state->func);
  br->aarch64->ty = AARCH64_INSTR_TY_B;
  br->aarch64->b = (struct aarch64_branch){.target = false_target};
}

static void codegen_br_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

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

static void codegen_64_bit_int(struct codegen_state *state,
                               struct aarch64_reg dest, union b64 value) {
  struct instr *lo = alloc_instr(state->func);
  lo->aarch64->ty = AARCH64_INSTR_TY_MOVZ;
  lo->aarch64->movz = (struct aarch64_mov_imm){.dest = dest, .imm = value.b[0]};

  if (value.b[1]) {
    struct instr *mid_lo = alloc_instr(state->func);
    mid_lo->aarch64->ty = AARCH64_INSTR_TY_MOVK;
    mid_lo->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[1], .shift = 1};
  }

  if (value.b[2]) {
    struct instr *mid_hi = alloc_instr(state->func);
    mid_hi->aarch64->ty = AARCH64_INSTR_TY_MOVK;
    mid_hi->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[2], .shift = 2};
  }

  if (value.b[3]) {
    struct instr *hi = alloc_instr(state->func);
    hi->aarch64->ty = AARCH64_INSTR_TY_MOVK;
    hi->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[3], .shift = 3};
  }
}

static_assert((sizeof(unsigned) == 4) & (sizeof(unsigned short) == 2),
              "type sizes not expected");

union b32 {
  unsigned u;
  float f;
  unsigned short b[2];
};

static void codegen_32_bit_int(struct codegen_state *state,
                               struct aarch64_reg dest, union b32 value) {

  struct instr *lo = alloc_instr(state->func);
  lo->aarch64->ty = AARCH64_INSTR_TY_MOVZ;
  lo->aarch64->movz = (struct aarch64_mov_imm){.dest = dest, .imm = value.b[0]};

  if (value.b[1]) {
    struct instr *hi = alloc_instr(state->func);
    hi->aarch64->ty = AARCH64_INSTR_TY_MOVK;
    hi->aarch64->movz =
        (struct aarch64_mov_imm){.dest = dest, .imm = value.b[1], .shift = 1};
  }
}

static void codegen_cnst_op(struct codegen_state *state, struct ir_op *op) {
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
    case IR_VAR_PRIMITIVE_TY_I8:
    case IR_VAR_PRIMITIVE_TY_I16:
    case IR_VAR_PRIMITIVE_TY_I32:
      codegen_32_bit_int(state, dest,
                         (union b32){.u = (unsigned)op->cnst.int_value});
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      codegen_64_bit_int(state, dest, (union b64){.ull = op->cnst.int_value});
      break;
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_F32:
    case IR_VAR_PRIMITIVE_TY_F64:
      unreachable();
    };
  }
}

static void codegen_unary_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg source = codegen_reg(op->unary_op.value);

  switch (op->unary_op.ty) {
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
    BUG("logical not should never reach emitter, should be converted in lower");
  }
}

static void codegen_binary_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct aarch64_reg dest = {0};

  if (!binary_op_is_comparison(op->binary_op.ty)) {
    dest = codegen_reg(op);
  }

  struct aarch64_reg lhs = codegen_reg(op->binary_op.lhs);
  struct aarch64_reg rhs = codegen_reg(op->binary_op.rhs);

  bool is_fp = var_ty_is_fp(&op->var_ty);

  enum ir_op_binary_op_ty ty = op->binary_op.ty;
  DEBUG_ASSERT(ty == IR_OP_BINARY_OP_TY_FADD || ty == IR_OP_BINARY_OP_TY_FSUB ||
                   ty == IR_OP_BINARY_OP_TY_FMUL ||
                   ty == IR_OP_BINARY_OP_TY_FDIV || !is_fp,
               "floating point with invalid binary op");

  switch (ty) {
  case IR_OP_BINARY_OP_TY_FEQ:
  case IR_OP_BINARY_OP_TY_FNEQ:
  case IR_OP_BINARY_OP_TY_FGT:
  case IR_OP_BINARY_OP_TY_FGTEQ:
  case IR_OP_BINARY_OP_TY_FLT:
  case IR_OP_BINARY_OP_TY_FLTEQ:
    instr->aarch64->ty = AARCH64_INSTR_TY_FCMP;
    instr->aarch64->fcmp = (struct aarch64_fcmp){
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
    instr->aarch64->ty = AARCH64_INSTR_TY_SUBS;
    instr->aarch64->subs = (struct aarch64_addsub_reg){
        .dest = zero_reg_for_ty(lhs.ty),
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_LSHIFT:
    instr->aarch64->ty = AARCH64_INSTR_TY_LSLV;
    instr->aarch64->lslv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_SRSHIFT:
    instr->aarch64->ty = AARCH64_INSTR_TY_ASRV;
    instr->aarch64->asrv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_URSHIFT:
    instr->aarch64->ty = AARCH64_INSTR_TY_LSRV;
    instr->aarch64->lsrv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_AND:
    instr->aarch64->ty = AARCH64_INSTR_TY_AND;
    instr->aarch64->and = (struct aarch64_logical_reg){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_OR:
    instr->aarch64->ty = AARCH64_INSTR_TY_ORR;
    instr->aarch64->orr = (struct aarch64_logical_reg){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_XOR:
    instr->aarch64->ty = AARCH64_INSTR_TY_EOR;
    instr->aarch64->eor = (struct aarch64_logical_reg){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_ADD:
    instr->aarch64->ty = AARCH64_INSTR_TY_ADD;
    instr->aarch64->add = (struct aarch64_addsub_reg){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    instr->aarch64->ty = AARCH64_INSTR_TY_SUB;
    instr->aarch64->sub = (struct aarch64_addsub_reg){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_MUL:
    instr->aarch64->ty = AARCH64_INSTR_TY_MADD;
    instr->aarch64->madd =
        (struct aarch64_fma){.dest = dest,
                             .lhs = lhs,
                             .rhs = rhs,
                             .addsub = zero_reg_for_ty(lhs.ty)};
    break;
  case IR_OP_BINARY_OP_TY_SDIV:
    instr->aarch64->ty = AARCH64_INSTR_TY_SDIV;
    instr->aarch64->sdiv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_UDIV:
    instr->aarch64->ty = AARCH64_INSTR_TY_UDIV;
    instr->aarch64->udiv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMAX:
    instr->aarch64->ty = AARCH64_INSTR_TY_FMAXNM;
    instr->aarch64->fmaxnm = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMIN:
    instr->aarch64->ty = AARCH64_INSTR_TY_FMINNM;
    instr->aarch64->fminnm = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FADD:
    instr->aarch64->ty = AARCH64_INSTR_TY_FADD;
    instr->aarch64->fadd = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FSUB:
    instr->aarch64->ty = AARCH64_INSTR_TY_FSUB;
    instr->aarch64->fsub = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMUL:
    instr->aarch64->ty = AARCH64_INSTR_TY_FMUL;
    instr->aarch64->fmul = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FDIV:
    instr->aarch64->ty = AARCH64_INSTR_TY_FDIV;
    instr->aarch64->fdiv = (struct aarch64_reg_2_source){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_UQUOT:
    BUG("squot/uquot shoud have been lowered");
  }
}

static void codegen_sext_op(struct codegen_state *state, struct ir_op *op,
                            struct aarch64_reg source,
                            struct aarch64_reg dest) {
  struct instr *instr = alloc_instr(state->func);

  invariant_assert(op->cast_op.value->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't sext from non-primitive");

  switch (op->cast_op.value->var_ty.primitive) {
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
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    BUG("todo cast floats");
  }
}

static void codegen_zext_op(struct codegen_state *state,
                            struct aarch64_reg source,
                            struct aarch64_reg dest) {
  // `mov`/`orr` with 32 bit operands zeroes top 32 bits

  struct instr *instr = alloc_instr(state->func);
  *instr->aarch64 = MOV_ALIAS(dest, source);
}

static void codegen_trunc_op(struct codegen_state *state, struct ir_op *op,
                             struct aarch64_reg source,
                             struct aarch64_reg dest) {
  struct instr *instr = alloc_instr(state->func);
  invariant_assert(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't truncate non-primitive");

  // https://kddnewton.com/2022/08/11/aarch64-bitmask-immediates.html
  // for understanding the immediates
  switch (op->var_ty.primitive) {
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
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    BUG("todo cast floats");
  }
}

static void codegen_conv_op(struct codegen_state *state,
                            struct aarch64_reg source,
                            struct aarch64_reg dest) {
  struct instr *instr = alloc_instr(state->func);
  instr->aarch64->ty = AARCH64_INSTR_TY_FCVT;
  instr->aarch64->fcvt =
      (struct aarch64_reg_1_source){.dest = dest, .source = source};
}

static void codegen_uconv_op(struct codegen_state *state,
                             struct aarch64_reg source,
                             struct aarch64_reg dest) {
  struct instr *instr = alloc_instr(state->func);
  instr->aarch64->ty = AARCH64_INSTR_TY_UCVTF;
  instr->aarch64->fcvt =
      (struct aarch64_reg_1_source){.dest = dest, .source = source};
}

static void codegen_sconv_op(struct codegen_state *state,
                             struct aarch64_reg source,
                             struct aarch64_reg dest) {
  struct instr *instr = alloc_instr(state->func);
  instr->aarch64->ty = AARCH64_INSTR_TY_SCVTF;
  instr->aarch64->fcvt =
      (struct aarch64_reg_1_source){.dest = dest, .source = source};
}

static void codegen_cast_op(struct codegen_state *state, struct ir_op *op) {
  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg source = codegen_reg(op->cast_op.value);

  // NOTE: for the integer casts (sext/zext/trunc) we promote the source reg to
  // the same type as the dest reg (mixed regs make no sense in an integer
  // instruction)

  switch (op->cast_op.ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    source.ty = dest.ty;
    codegen_sext_op(state, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_ZEXT:
    source.ty = dest.ty;
    codegen_zext_op(state, source, dest);
    break;
  case IR_OP_CAST_OP_TY_TRUNC:
    source.ty = dest.ty;
    codegen_trunc_op(state, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_CONV:
    codegen_conv_op(state, source, dest);
    break;
  case IR_OP_CAST_OP_TY_UCONV:
    codegen_uconv_op(state, source, dest);
    break;
  case IR_OP_CAST_OP_TY_SCONV:
    codegen_sconv_op(state, source, dest);
    break;
  }
}

static bool try_get_hfa_info(struct codegen_state *state,
                             const struct ir_var_ty *var_ty,
                             size_t *num_members, size_t *member_size) {
  if (var_ty->ty != IR_VAR_TY_TY_UNION && var_ty->ty != IR_VAR_TY_TY_STRUCT) {
    return false;
  }

  if (var_ty->ty == IR_VAR_TY_TY_UNION) {
    TODO("union hfa handling");
  }

  if (!var_ty->struct_ty.num_fields) {
    return false;
  }

  struct ir_var_ty *field_ty = &var_ty->struct_ty.fields[0];

  if (!var_ty_is_fp(field_ty)) {
    return false;
  }

  if (var_ty->struct_ty.num_fields > 4) {
    return false;
  }

  for (size_t i = 1; i < var_ty->struct_ty.num_fields; i++) {
    if (!var_ty_eq(state->ir, field_ty, &var_ty->struct_ty.fields[i])) {
      return false;
    }
  }

  switch (field_ty->primitive) {
  case IR_VAR_PRIMITIVE_TY_F16:
    *member_size = 2;
    break;
  case IR_VAR_PRIMITIVE_TY_F32:
    *member_size = 4;
    break;
  case IR_VAR_PRIMITIVE_TY_F64:
    *member_size = 8;
    break;
  default:
    unreachable();
  }

  *num_members = var_ty->struct_ty.num_fields;
  return true;
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

static void codegen_moves(struct codegen_state *state, struct move_set moves,
                          enum aarch64_reg_class reg_class) {
  for (size_t i = 0; i < moves.num_moves; i++) {
    struct move move = moves.moves[i];

    if (IS_MEM_LOC(move.from)) {
      DEBUG_ASSERT(!IS_MEM_LOC(move.to), "mem -> mem but not in memcpy list");

      struct mem_loc *from = move.from.metadata;
      struct aarch64_reg to = {.ty = reg_ty_for_size(reg_class, from->size),
                               .idx = move.to.idx};

      DEBUG_ASSERT(from->offset % from->size == 0,
                   "offset was not a multiple of size");
      size_t offset = from->offset / from->size;

      struct instr *load = alloc_instr(state->func);
      load->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
      load->aarch64->load_imm =
          (struct aarch64_load_imm){.addr = from->base,
                                    .imm = offset,
                                    .dest = to,
                                    .mode = AARCH64_ADDRESSING_MODE_OFFSET};
    } else if (IS_MEM_LOC(move.to)) {
      DEBUG_ASSERT(!IS_MEM_LOC(move.from), "mem -> mem but not in memcpy list");

      struct mem_loc *to = move.to.metadata;
      struct aarch64_reg from = {.ty = reg_ty_for_size(reg_class, to->size),
                                 .idx = move.from.idx};

      DEBUG_ASSERT(to->offset % to->size == 0,
                   "offset was not a multiple of size");
      size_t offset = to->offset / to->size;

      struct instr *store = alloc_instr(state->func);
      store->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
      store->aarch64->store_imm =
          (struct aarch64_store_imm){.addr = to->base,
                                     .imm = offset,
                                     .source = from,
                                     .mode = AARCH64_ADDRESSING_MODE_OFFSET};
    } else {
      // we move more of register than necessary here

      struct instr *mov = alloc_instr(state->func);

      switch (reg_class) {
      case AARCH64_REG_CLASS_GP: {
        struct aarch64_reg source = {.ty = AARCH64_REG_TY_X,
                                     .idx = move.from.idx};
        struct aarch64_reg dest = {.ty = AARCH64_REG_TY_X, .idx = move.to.idx};
        *mov->aarch64 = MOV_ALIAS(dest, source);
        break;
      }
      case AARCH64_REG_CLASS_FP: {
        struct aarch64_reg source = {.ty = AARCH64_REG_TY_D,
                                     .idx = move.from.idx};
        struct aarch64_reg dest = {.ty = AARCH64_REG_TY_D, .idx = move.to.idx};
        *mov->aarch64 = FP_MOV_ALIAS(dest, source);
        break;
      }
      }
    }
  }
}

enum aarch64_reg_attr_flags reg_attr_flags(struct aarch64_reg reg) {
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

static void codegen_call_op(struct codegen_state *state, struct ir_op *op) {
  invariant_assert(op->call.func_ty.ty == IR_VAR_TY_TY_FUNC, "non-func");

  const struct ir_var_func_ty *func_ty = &op->call.func_ty.func;
  const struct ir_var_ty *param_tys;

  // note, it is important we use the func ty as the reference here for
  // types as aggregates and similar will already have been turned into
  // pointers. this does mean we need to explicitly handle pointers
  // in the case of unspecified functions, we use `arg_var_tys` which is
  // preserved
  if (func_ty->num_params == op->call.num_args ||
      (func_ty->flags & IR_VAR_FUNC_TY_FLAG_VARIADIC)) {
    param_tys = func_ty->params;
  } else {
    param_tys = op->call.arg_var_tys;
  }

  invariant_assert(func_ty->num_params <= 8,
                   "`%s` doesn't support more than 8 args yet", __func__);

  // now we need to move each argument into its correct register
  // it is possible there are no spare registers for this, and so we may need

  bool variadics_on_stack;
  switch (state->target->target_id) {
  case TARGET_ID_AARCH64_MACOS:
    variadics_on_stack = true;
    break;
  default:
    variadics_on_stack = false;
    break;
  }

  struct vector *gp_move_from = vector_create(sizeof(struct location));
  struct vector *gp_move_to = vector_create(sizeof(struct location));

  struct vector *fp_move_from = vector_create(sizeof(struct location));
  struct vector *fp_move_to = vector_create(sizeof(struct location));

  struct vector *mem_copies = vector_create(sizeof(struct mem_copy));

  size_t ngrn = 0;
  size_t nsrn = 0;
  size_t nsaa = 0;

  if (!(op->call.target->flags & IR_OP_FLAG_CONTAINED)) {
    struct aarch64_reg source = codegen_reg(op->call.target);

    struct location from = {.idx = source.idx};

    struct location to = {.idx = BLR_REG_IDX};

    // need to move it into reg 10
    vector_push_back(gp_move_from, &from);
    vector_push_back(gp_move_to, &to);
  }

  size_t last_mem_loc = FIRST_MEM_LOC;

  if (op->call.num_args) {
    size_t num_normal_args = func_ty->num_params;

    for (size_t i = 0; i < op->call.num_args; i++) {
      struct aarch64_reg source = codegen_reg(op->call.args[i]);

      enum aarch64_reg_attr_flags flags = reg_attr_flags(source);
      if (flags & AARCH64_REG_ATTR_FLAG_ARG_REG) {
        // bad, might be overwritten during param preparation
        // preemptively add 9 so it is not an arg register
        // TODO: is this safe, and can it be improved?
        struct aarch64_reg dest = {.ty = source.ty, .idx = source.idx + 9};
        struct instr *mov = alloc_instr(state->func);

        if (aarch64_reg_ty_is_gp(dest.ty)) {
          *mov->aarch64 = MOV_ALIAS(dest, source);
        } else {
          *mov->aarch64 = FP_MOV_ALIAS(dest, source);
        }

        source = dest;
      }
    }

    for (size_t i = 0; i < op->call.num_args; i++) {
      struct aarch64_reg source = codegen_reg(op->call.args[i]);

      enum aarch64_reg_attr_flags flags = reg_attr_flags(source);
      if (flags & AARCH64_REG_ATTR_FLAG_ARG_REG) {
        source.idx += 9;
      }

      // hmm, we pretend all memory locations are different but they are not
      // can gen_move_order accidentally overwrite things because of this?

      if (!variadics_on_stack || i < num_normal_args ||
          !(func_ty->flags & IR_VAR_FUNC_TY_FLAG_VARIADIC)) {

        const struct ir_var_ty *var_ty = i < num_normal_args ? &param_tys[i] : &op->call.args[i]->var_ty;

        struct ir_var_ty_info info = var_ty_info(
            state->ir->unit,
            var_ty->ty == IR_VAR_TY_TY_ARRAY ? &IR_VAR_TY_POINTER : var_ty);

        size_t num_hfa_members;
        size_t hfa_member_size;

        if (var_ty_is_fp(var_ty) && nsrn < 8) {
          struct location from = {.idx = source.idx};
          struct location to = {.idx = nsrn};
          vector_push_back(fp_move_from, &from);
          vector_push_back(fp_move_to, &to);
          nsrn++;
          continue;
        } else if (try_get_hfa_info(state, var_ty, &num_hfa_members,
                                    &hfa_member_size)) {
          if (nsrn + num_hfa_members <= 8) {
            for (size_t j = 0; j < num_hfa_members; j++) {
              // given this is a composite, we assume `source` contains a
              // pointer to it

              struct mem_loc mem_loc = {.base = source,
                                        .offset = hfa_member_size * j,
                                        .size = hfa_member_size};

              struct location loc = {
                  .idx = MEM_LOC(),
                  .metadata = arena_alloc_init(
                      state->arena, sizeof(struct mem_loc), &mem_loc)};

              struct location to = {.idx = nsrn++};
              vector_push_back(fp_move_from, &loc);
              vector_push_back(fp_move_to, &to);
            }

            continue;
          }

          nsrn = 8;
          size_t nsaa_align = MAX(8, info.alignment);
          size_t size = ROUND_UP(info.size, nsaa_align);

          // TODO: overaligned HFAs and vectors
          struct mem_loc mem_src = {.base = source, .offset = 0, .size = size};

          struct mem_loc mem_dest = {
              .base = STACK_PTR_REG, .offset = nsaa, .size = size};

          struct mem_copy copy = {.src = mem_src, .dest = mem_dest};

          vector_push_back(mem_copies, &copy);

          nsaa += size;

          continue;

        } else if ((INTEGRAL_OR_PTRLIKE(var_ty)) && info.size <= 8 &&
                   ngrn <= 8) {
          struct location from = {.idx = source.idx};
          struct location to = {.idx = ngrn};
          vector_push_back(gp_move_from, &from);
          vector_push_back(gp_move_to, &to);
          ngrn++;
          continue;
        }

        if (info.alignment == 16) {
          ngrn = (ngrn + 1) & 1;
        }

        if (var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
          TODO("128 bit reg alloc");
          // // lo to ngrn, hi to ngrn+1
          // ngrn += 2;
          // continue;
        }

        size_t dw_size = info.size / 8;
        if (var_ty_is_aggregate(var_ty) && dw_size <= (8 - ngrn)) {
          for (size_t j = 0; j <= dw_size; j++) {
            // given this is a composite, we assume `source` contains a
            // pointer to it

            struct mem_loc mem_loc = {
                .base = source, .offset = 8 * j, .size = 8};

            struct location loc = {
                .idx = MEM_LOC(),
                .metadata = arena_alloc_init(state->arena,
                                             sizeof(struct mem_loc), &mem_loc)};

            struct location to = {.idx = ngrn++};
            vector_push_back(gp_move_from, &loc);
            vector_push_back(gp_move_to, &to);
          }

          continue;
        }

        ngrn = 8;
        size_t nsaa_align = MAX(8, info.alignment);
        nsaa = ROUND_UP(nsaa, nsaa_align);

        if (var_ty_is_aggregate(var_ty)) {
          struct mem_loc mem_src = {
              .base = source, .offset = 0, .size = info.size};

          struct mem_loc mem_dest = {
              .base = STACK_PTR_REG, .offset = nsaa, .size = info.size};

          struct mem_copy copy = {.src = mem_src, .dest = mem_dest};

          vector_push_back(mem_copies, &copy);

          nsaa += info.size;
          continue;
        }

        size_t size = MAX(8, info.size);

        struct mem_loc mem_loc = {
            .base = STACK_PTR_REG, .offset = nsaa, .size = size};

        nsaa += size;

        struct location from = {.idx = source.idx};

        struct location to = {
            .idx = MEM_LOC(),
            .metadata = arena_alloc_init(state->arena, sizeof(struct mem_loc),
                                         &mem_loc)};

        if (INTEGRAL_OR_PTRLIKE(var_ty)) {
          vector_push_back(gp_move_from, &from);
          vector_push_back(gp_move_to, &to);
        } else if (var_ty_is_fp(var_ty)) {
          vector_push_back(fp_move_from, &from);
          vector_push_back(fp_move_to, &to);
        } else {
          BUG("bad type");
        }
      } else {
        struct ir_var_ty *var_ty = &op->call.args[i]->var_ty;

        // this argument is variadic
        // the stack slot this local must live in, in terms of 8 byte slots
        size_t variadic_arg_idx = i - num_normal_args;

        if (source.ty == AARCH64_REG_TY_W) {
          // because offsets are in terms of reg size, we need to double it
          // for the 8 byte slots
          variadic_arg_idx *= 2;
        } else {
          invariant_assert(var_ty_info(state->ir->unit, var_ty).size >= 8,
                           "variadic arg with size < 8 has not been promoted");
        }

        struct instr *store = alloc_instr(state->func);

        store->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
        store->aarch64->str_imm =
            (struct aarch64_store_imm){.source = source,
                                       .addr = STACK_PTR_REG,
                                       .imm = variadic_arg_idx,
                                       .mode = AARCH64_ADDRESSING_MODE_OFFSET};
      }
    }
  }

  struct move_set gp_arg_moves = gen_move_order(
      state->ir->arena, vector_head(gp_move_from), vector_head(gp_move_to),
      vector_length(gp_move_from), GP_TMP_REG_IDX);

  struct move_set fp_arg_moves = gen_move_order(
      state->ir->arena, vector_head(fp_move_from), vector_head(fp_move_to),
      vector_length(fp_move_from), FP_TMP_REG_IDX);

  codegen_moves(state, gp_arg_moves, AARCH64_REG_CLASS_GP);
  codegen_moves(state, fp_arg_moves, AARCH64_REG_CLASS_FP);

  // then handle mem copies
  size_t num_copies = vector_length(mem_copies);
  for (size_t i = 0; i < num_copies; i++) {
    struct mem_copy *copy = vector_get(mem_copies, i);
    struct mem_loc *from = &copy->src;
    struct mem_loc *to = &copy->dest;

    DEBUG_ASSERT(from->size == to->size, "mem cpy with different sizes");

    codegen_mem_copy_volatile(state, from->base, from->offset, to->base,
                              to->offset, to->size);
  }

  // now we generate the actual call

  struct instr *instr = alloc_instr(state->func);
  if (op->call.target->flags & IR_OP_FLAG_CONTAINED) {
    instr->aarch64->ty = AARCH64_INSTR_TY_BL;
    instr->aarch64->bl = (struct aarch64_branch){.target = NULL};

    instr->reloc = arena_alloc(state->func->unit->arena, sizeof(*instr->reloc));
    *instr->reloc = (struct relocation){
        .ty = RELOCATION_TY_SINGLE,
        .symbol_index = op->call.target->addr.glb->id,
        .size = 2,
        .address = 0,
    };
  } else {
    // NOTE: `blr` seems to segfault on linux aarch64
    instr->aarch64->ty = AARCH64_INSTR_TY_BLR;
    instr->aarch64->blr = (struct aarch64_branch_reg){
        .target = {.ty = AARCH64_REG_TY_X, .idx = BLR_REG_IDX}};
  }

  if (func_ty->ret_ty->ty == IR_VAR_TY_TY_NONE) {
    return;
  }

  struct aarch64_reg dest = codegen_reg(op);

  struct ir_var_ty *var_ty = func_ty->ret_ty;
  struct ir_var_ty_info info = var_ty_info(state->ir->unit, var_ty);

  if (INTEGRAL_OR_PTRLIKE(var_ty)) {
    if (!is_return_reg(dest)) {
      struct instr *mov = alloc_instr(state->func);

      *mov->aarch64 = MOV_ALIAS(dest, return_reg_for_ty(dest.ty));
    }
  } else if (var_ty_is_fp(var_ty)) {
    if (!is_return_reg(dest)) {
      struct instr *mov = alloc_instr(state->func);

      *mov->aarch64 = FP_MOV_ALIAS(dest, return_reg_for_ty(dest.ty));
    }
  } else {
    struct ir_lcl *lcl = op->lcl;
    size_t offset = lcl->offset;

    // aggregate ty
    size_t num_members;
    size_t member_size;
    if (lcl && try_get_hfa_info(state, var_ty, &num_members, &member_size) &&
        num_members <= 4) {
      enum aarch64_reg_ty reg_ty;
      switch (member_size) {
      case 8:
        reg_ty = AARCH64_REG_TY_D;
        break;
      case 4:
        reg_ty = AARCH64_REG_TY_S;
        break;
      case 2:
        reg_ty = AARCH64_REG_TY_H;
        break;
      }

      for (size_t i = 0; i < num_members; i++) {
        struct aarch64_reg source = {.ty = reg_ty, .idx = i};

        struct instr *mov = alloc_instr(state->func);

        mov->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
        mov->aarch64->store_imm =
            (struct aarch64_store_imm){.addr = STACK_PTR_REG,
                                       .imm = (offset / member_size) + i,
                                       .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                       .source = source};
      }
    } else if (info.size == 1) {
      struct aarch64_reg source = {.ty = AARCH64_REG_TY_W, .idx = 0};

      struct instr *mov = alloc_instr(state->func);

      mov->aarch64->ty = AARCH64_INSTR_TY_STORE_BYTE_IMM;
      mov->aarch64->strb_imm =
          (struct aarch64_store_imm){.addr = STACK_PTR_REG,
                                     .imm = offset,
                                     .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                     .source = source};
    } else if (info.size == 2) {
      struct aarch64_reg source = {.ty = AARCH64_REG_TY_W, .idx = 0};

      struct instr *mov = alloc_instr(state->func);

      mov->aarch64->ty = AARCH64_INSTR_TY_STORE_HALF_IMM;
      mov->aarch64->strh_imm =
          (struct aarch64_store_imm){.addr = STACK_PTR_REG,
                                     .imm = offset / 2,
                                     .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                     .source = source};
    } else if (info.size <= 4) {
      struct aarch64_reg source = {.ty = AARCH64_REG_TY_W, .idx = 0};

      struct instr *mov = alloc_instr(state->func);

      mov->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
      mov->aarch64->str_imm =
          (struct aarch64_store_imm){.addr = STACK_PTR_REG,
                                     .imm = offset / 4,
                                     .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                     .source = source};
    } else if (info.size <= 8) {
      struct aarch64_reg source = {.ty = AARCH64_REG_TY_X, .idx = 0};

      struct instr *mov = alloc_instr(state->func);

      mov->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
      mov->aarch64->str_imm =
          (struct aarch64_store_imm){.addr = STACK_PTR_REG,
                                     .imm = offset / 8,
                                     .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                     .source = source};
    } else if (info.size <= 16) {
      struct aarch64_reg source0 = {.ty = AARCH64_REG_TY_X, .idx = 0};
      struct aarch64_reg source1 = {.ty = AARCH64_REG_TY_X, .idx = 1};

      struct instr *mov = alloc_instr(state->func);

      mov->aarch64->ty = AARCH64_INSTR_TY_STORE_PAIR_IMM;
      mov->aarch64->store_pair_imm = (struct aarch64_store_pair_imm){
          .addr = STACK_PTR_REG,
          .imm = offset / 8,
          .mode = AARCH64_ADDRESSING_MODE_OFFSET,
          .source = {source0, source1}};
    } else {
      TODO("larger than 16 byte types");
    }
  }
}

static void codegen_params(struct codegen_state *state) {
  // we effectively do the reverse of what we do in `codegen_call_op`
  // putting params in correct registers
  // and spilling aggregates to stack
  // we could try and merge the code in some way?
  // we also do it again, below, to calculate how much arg space is needed for
  // prologue

  // important note:
  // to prevent overwriting things, we put all our locals beneath the arguments
  // for the method

  // FIXME: this currently relies heavily on IR build
  // specifically, each aggregate must produce exactly one local
  // and do it in order of params

  struct ir_var_func_ty func_ty = state->ir->func_ty;

  size_t ngrn = 0;
  size_t nsrn = 0;
  size_t nsaa = 0;

  struct vector *gp_move_from = vector_create(sizeof(struct location));
  struct vector *gp_move_to = vector_create(sizeof(struct location));

  struct vector *fp_move_from = vector_create(sizeof(struct location));
  struct vector *fp_move_to = vector_create(sizeof(struct location));

  struct vector *mem_copies = vector_create(sizeof(struct mem_copy));

  struct ir_stmt *param_stmt = state->ir->first->first;
  struct ir_op *param_op = NULL;
  if (param_stmt && param_stmt->first &&
      param_stmt->first->ty == IR_OP_TY_MOV &&
      (param_stmt->first->flags & IR_OP_FLAG_PARAM)) {
    param_op = param_stmt->first;
  }

  struct ir_lcl *lcl = state->ir->first_local;

  size_t last_mem_loc = FIRST_MEM_LOC;

  for (size_t i = 0; i < func_ty.num_params; i++) {
    const struct ir_var_ty *var_ty = &func_ty.params[i];
    struct ir_var_ty_info info = var_ty_info(
        state->ir->unit,
        var_ty->ty == IR_VAR_TY_TY_ARRAY ? &IR_VAR_TY_POINTER : var_ty);

    size_t offset;
    struct aarch64_reg source;

    if (lcl) {
      offset = lcl->offset;
    }

    if (param_op) {
      source = codegen_reg(param_op);
    }

    size_t num_hfa_members;
    size_t hfa_member_size;

    if (var_ty_is_fp(var_ty) && nsrn < 8) {
      struct location from = {.idx = nsrn};
      struct location to = {.idx = source.idx};
      vector_push_back(fp_move_from, &from);
      vector_push_back(fp_move_to, &to);
      nsrn++;

      param_op = param_op->succ;
      continue;
    } else if (try_get_hfa_info(state, var_ty, &num_hfa_members,
                                &hfa_member_size)) {
      if (nsrn + num_hfa_members <= 8) {
        for (size_t j = 0; j < num_hfa_members; j++) {
          // given this is a composite, we assume `source` contains a
          // pointer to it

          struct location from = {.idx = nsrn++};

          struct mem_loc mem_loc = {.base = STACK_PTR_REG,
                                    .offset = hfa_member_size * j,
                                    .size = hfa_member_size};

          struct location to = {
              .idx = MEM_LOC(),
              .metadata = arena_alloc_init(state->arena, sizeof(struct mem_loc),
                                           &mem_loc)};

          vector_push_back(fp_move_from, &from);
          vector_push_back(fp_move_to, &to);
        }

        lcl = lcl->succ;
        continue;
      }

      nsrn = 8;
      size_t nsaa_align = MAX(8, info.alignment);
      size_t size = ROUND_UP(info.size, nsaa_align);

      lcl->offset = state->prologue_info.stack_size + nsaa;

      nsaa += size;
      lcl = lcl->succ;
      continue;

    } else if ((INTEGRAL_OR_PTRLIKE(var_ty)) && info.size <= 8 && ngrn <= 8) {
      struct location from = {.idx = ngrn};
      struct location to = {.idx = source.idx};
      vector_push_back(gp_move_from, &from);
      vector_push_back(gp_move_to, &to);
      ngrn++;
      param_op = param_op->succ;
      continue;
    }

    if (info.alignment == 16) {
      ngrn = (ngrn + 1) & 1;
    }

    if (var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
      TODO("128 bit reg alloc");
      // // lo to ngrn, hi to ngrn+1
      // ngrn += 2;
      // continue;
    }

    size_t dw_size = (info.size + 7) / 8;
    if (var_ty_is_aggregate(var_ty) && dw_size <= (8 - ngrn)) {
      for (size_t j = 0; j < dw_size; j++) {
        // given this is a composite, we assume `source` contains a
        // pointer to it

        struct mem_loc mem_loc = {
            .base = STACK_PTR_REG, .offset = offset + (8 * j), .size = 8};

        struct location to = {
            .idx = MEM_LOC(),
            .metadata = arena_alloc_init(state->arena, sizeof(struct mem_loc),
                                         &mem_loc)};

        struct location from = {.idx = ngrn++};
        vector_push_back(gp_move_from, &from);
        vector_push_back(gp_move_to, &to);
      }

      lcl = lcl->succ;
      continue;
    }

    ngrn = 8;
    size_t nsaa_align = MAX(8, info.alignment);
    nsaa = ROUND_UP(nsaa, nsaa_align);

    if (var_ty_is_aggregate(var_ty)) {
      lcl->offset = state->prologue_info.stack_size + nsaa;

      nsaa += info.size;

      lcl = lcl->succ;
      continue;
    }

    size_t size = MAX(8, info.size);

    struct mem_loc mem_loc = {
        .base = STACK_PTR_REG, .offset = nsaa, .size = size};

    nsaa += size;

    struct location to = {.idx = source.idx};

    struct location from = {
        .idx = MEM_LOC(),
        .metadata =
            arena_alloc_init(state->arena, sizeof(struct mem_loc), &mem_loc)};

    param_op = param_op->succ;

    if (INTEGRAL_OR_PTRLIKE(var_ty)) {
      vector_push_back(gp_move_from, &from);
      vector_push_back(gp_move_to, &to);
    } else if (var_ty_is_fp(var_ty)) {
      vector_push_back(fp_move_from, &from);
      vector_push_back(fp_move_to, &to);
    } else {
      BUG("bad type");
    }
  }

  struct move_set gp_arg_moves = gen_move_order(
      state->ir->arena, vector_head(gp_move_from), vector_head(gp_move_to),
      vector_length(gp_move_from), GP_TMP_REG_IDX);

  struct move_set fp_arg_moves = gen_move_order(
      state->ir->arena, vector_head(fp_move_from), vector_head(fp_move_to),
      vector_length(fp_move_from), FP_TMP_REG_IDX);

  codegen_moves(state, gp_arg_moves, AARCH64_REG_CLASS_GP);
  codegen_moves(state, fp_arg_moves, AARCH64_REG_CLASS_FP);

  // then handle mem copies
  size_t num_copies = vector_length(mem_copies);
  for (size_t i = 0; i < num_copies; i++) {
    struct mem_copy *copy = vector_get(mem_copies, i);
    struct mem_loc *from = &copy->src;
    struct mem_loc *to = &copy->dest;

    DEBUG_ASSERT(from->size == to->size, "mem cpy with different sizes");

    codegen_mem_copy_volatile(state, from->base, from->offset, to->base,
                              to->offset, to->size);
  }
}

static size_t calc_arg_stack_space(struct codegen_state *state,
                                   struct ir_var_func_ty func_ty,
                                   struct ir_op *op) {
  size_t ngrn = 0;
  size_t nsrn = 0;
  size_t nsaa = 0;

  for (size_t i = 0; i < func_ty.num_params; i++) {
    const struct ir_var_ty *var_ty = &func_ty.params[i];
    struct ir_var_ty_info info = var_ty_info(state->ir->unit, var_ty);

    size_t num_hfa_members;
    size_t hfa_member_size;

    if (var_ty_is_fp(var_ty) && nsrn < 8) {
      nsrn++;
      continue;
    } else if (try_get_hfa_info(state, var_ty, &num_hfa_members,
                                &hfa_member_size)) {
      if (nsrn + num_hfa_members <= 8) {
        nsrn += num_hfa_members;
        continue;
      }

      nsrn = 8;
      size_t nsaa_align = MAX(8, info.alignment);
      size_t size = ROUND_UP(info.size, nsaa_align);

      nsaa += size;
      continue;

    } else if ((INTEGRAL_OR_PTRLIKE(var_ty)) && info.size <= 8 && ngrn <= 8) {
      ngrn++;
      continue;
    }

    if (info.alignment == 16) {
      ngrn = (ngrn + 1) & 1;
    }

    if (var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
      ngrn += 2;
      continue;
    }

    size_t dw_size = info.size / 8;
    if (var_ty_is_aggregate(var_ty) && dw_size <= (8 - ngrn)) {
      ngrn += dw_size;
      continue;
    }

    ngrn = 8;
    size_t nsaa_align = MAX(8, info.alignment);
    nsaa = ROUND_UP(nsaa, nsaa_align);

    if (var_ty_is_aggregate(var_ty)) {
      nsaa += info.size;
      continue;
    }

    size_t size = MAX(8, info.size);

    nsaa += size;
  }

  if (op) {
    DEBUG_ASSERT(op->ty == IR_OP_TY_CALL, "needs call");
    for (size_t i = func_ty.num_params; i < op->call.num_args; i++) {
      struct ir_var_ty var_ty = op->call.args[i]->var_ty;
      struct ir_var_ty_info info = var_ty_info(state->ir->unit, &var_ty);

      size_t size = ROUND_UP(info.size, 8);
      nsaa += size;
    }
  }

  return nsaa;
}

// FIXME: do things more smarter
#define AARCH64_TARGET (AARCH64_MACOS_TARGET)

static void codegen_prologue(struct codegen_state *state) {
  struct ir_func *ir = state->ir;

  // TODO: super inefficient
  bool nonvolatile_registers_used =
      (bitset_lzcnt(ir->reg_usage.gp_registers_used) <
       AARCH64_TARGET.reg_info.gp_registers.num_nonvolatile) ||
      (bitset_lzcnt(ir->reg_usage.fp_registers_used) <
       AARCH64_TARGET.reg_info.fp_registers.num_nonvolatile);

  bool leaf = !(nonvolatile_registers_used || ir->num_locals ||
                ir->flags & IR_FUNC_FLAG_MAKES_CALL);

  size_t stack_size = state->stack_args_size;
  stack_size =
      ROUND_UP(stack_size + ir->total_locals_size, AARCH64_STACK_ALIGNMENT);

  const size_t LR_OFFSET = 2;
  struct aarch64_prologue_info info = {.prologue_generated = !leaf,
                                       .saved_gp_registers = 0,
                                       .saved_fp_registers = 0,
                                       .save_start = stack_size,
                                       .lr_offset = LR_OFFSET,
                                       .stack_size = stack_size};

  if (!info.prologue_generated) {
    state->prologue_info = info;
    return;
  }

  struct bitset_iter gp_iter =
      bitset_iter(ir->reg_usage.gp_registers_used,
                  AARCH64_TARGET.reg_info.gp_registers.num_nonvolatile, true);

  size_t i;
  while (bitset_iter_next(&gp_iter, &i)) {
    info.saved_gp_registers |= (1 << i);
    info.stack_size += 8;
  }

  struct bitset_iter fp_iter =
      bitset_iter(ir->reg_usage.fp_registers_used,
                  AARCH64_TARGET.reg_info.fp_registers.num_nonvolatile, true);

  while (bitset_iter_next(&fp_iter, &i)) {
    info.saved_fp_registers |= (1 << i);
    info.stack_size += 8;
  }

  // need to save x29 and x30
  info.stack_size += 16;

  struct instr *save_lr_x30 = alloc_instr(state->func);
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
  struct instr *save_x29 = alloc_instr(state->func);
  save_x29->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
  save_x29->aarch64->add_imm = (struct aarch64_addsub_imm){
      .dest = (struct aarch64_reg){.ty = AARCH64_REG_TY_X, .idx = 29},
      .source = STACK_PTR_REG,
      .imm = info.lr_offset * 8,
      .shift = 0};

  size_t stack_to_sub = info.stack_size - 16; // from the pre-index lr
  if (stack_to_sub) {
    if (stack_to_sub > MAX_IMM_SIZE) {
      codegen_sub_imm(state, STACK_PTR_REG, STACK_PTR_REG, stack_to_sub);
    } else {
      struct instr *sub_stack = alloc_instr(state->func);
      sub_stack->aarch64->ty = AARCH64_INSTR_TY_SUB_IMM;
      sub_stack->aarch64->sub_imm =
          (struct aarch64_addsub_imm){.dest = STACK_PTR_REG,
                                      .source = STACK_PTR_REG,
                                      .imm = stack_to_sub,
                                      .shift = 0};
    }

    size_t save_idx = 0;

    struct bitset_iter gp_reg_iter =
        bitset_iter(ir->reg_usage.gp_registers_used,
                    AARCH64_TARGET.reg_info.gp_registers.num_nonvolatile, true);

    size_t idx;
    while (bitset_iter_next(&gp_reg_iter, &idx)) {
      // guaranteed to be mod 8
      size_t offset = (info.save_start / 8) + save_idx++;

      struct instr *save = alloc_instr(state->func);
      save->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
      save->aarch64->str_imm = (struct aarch64_store_imm){
          .mode = AARCH64_ADDRESSING_MODE_OFFSET,
          .imm = offset,
          .source = (struct aarch64_reg){.ty = AARCH64_REG_TY_X,
                                         .idx = translate_reg_idx(
                                             idx, IR_REG_TY_INTEGRAL)},
          .addr = STACK_PTR_REG,
      };
    }

    struct bitset_iter fp_reg_iter =
        bitset_iter(ir->reg_usage.fp_registers_used,
                    AARCH64_TARGET.reg_info.fp_registers.num_nonvolatile, true);

    while (bitset_iter_next(&fp_reg_iter, &idx)) {
      // guaranteed to be mod 8
      size_t offset = (info.save_start / 8) + save_idx++;

      struct instr *save = alloc_instr(state->func);
      save->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
      save->aarch64->str_imm = (struct aarch64_store_imm){
          .mode = AARCH64_ADDRESSING_MODE_OFFSET,
          .imm = offset,
          .source = (struct aarch64_reg){.ty = AARCH64_REG_TY_D,
                                         .idx = translate_reg_idx(
                                             idx, IR_REG_TY_INTEGRAL)},
          .addr = STACK_PTR_REG,
      };
    }
  }

  state->prologue_info = info;
}

static void codegen_epilogue(struct codegen_state *state) {
  const struct aarch64_prologue_info *prologue_info = &state->prologue_info;

  if (!prologue_info->prologue_generated) {
    return;
  }

  unsigned long max_gp_saved = sizeof(prologue_info->saved_gp_registers) * 8 -
                               lzcnt(prologue_info->saved_gp_registers);

  size_t save_idx = 0;
  for (size_t i = 0; i < max_gp_saved; i++) {
    // FIXME: loop should start at i=first non volatile
    if (!NTH_BIT(prologue_info->saved_gp_registers, i)) {
      continue;
    }

    size_t offset = (prologue_info->save_start / 8) + save_idx++;

    struct instr *restore = alloc_instr(state->func);
    restore->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
    restore->aarch64->ldr_imm = (struct aarch64_load_imm){
        .mode = AARCH64_ADDRESSING_MODE_OFFSET,
        .imm = offset,
        .dest = (struct aarch64_reg){.ty = AARCH64_REG_TY_X,
                                     .idx = translate_reg_idx(
                                         i, IR_REG_TY_INTEGRAL)},
        .addr = STACK_PTR_REG,
    };
  }

  unsigned long max_fp_saved = sizeof(prologue_info->saved_fp_registers) * 8 -
                               lzcnt(prologue_info->saved_fp_registers);

  save_idx = 0;
  for (size_t i = 0; i < max_fp_saved; i++) {
    // FIXME: loop should start at i=first non volatile
    if (!NTH_BIT(prologue_info->saved_fp_registers, i)) {
      continue;
    }

    size_t offset = (prologue_info->save_start / 8) + save_idx++;

    struct instr *restore = alloc_instr(state->func);
    restore->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
    restore->aarch64->ldr_imm = (struct aarch64_load_imm){
        .mode = AARCH64_ADDRESSING_MODE_OFFSET,
        .imm = offset,
        .dest = (struct aarch64_reg){.ty = AARCH64_REG_TY_D,
                                     .idx = translate_reg_idx(
                                         i, IR_REG_TY_INTEGRAL)},
        .addr = STACK_PTR_REG,
    };
  }

  size_t stack_to_add = prologue_info->stack_size - 16;
  if (stack_to_add) {
    if (stack_to_add > MAX_IMM_SIZE) {
      codegen_add_imm(state, STACK_PTR_REG, STACK_PTR_REG, stack_to_add);
    } else {
      struct instr *add_stack = alloc_instr(state->func);
      add_stack->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
      add_stack->aarch64->add_imm =
          (struct aarch64_addsub_imm){.dest = STACK_PTR_REG,
                                      .source = STACK_PTR_REG,
                                      .imm = stack_to_add,
                                      .shift = 0};
    }
  }

  struct instr *restore_lr_x30 = alloc_instr(state->func);
  restore_lr_x30->aarch64->ty = AARCH64_INSTR_TY_LOAD_PAIR_IMM;
  restore_lr_x30->aarch64->ldp_imm = (struct aarch64_load_pair_imm){
      .mode = AARCH64_ADDRESSING_MODE_POSTINDEX,
      .imm = prologue_info->lr_offset,
      .dest = {FRAME_PTR_REG, RET_PTR_REG},
      .addr = STACK_PTR_REG,
  };
}

static void codegen_ret_op(struct codegen_state *state, struct ir_op *op) {
  if (op->ret.value && op->ret.value->ty != IR_OP_TY_CALL) {
    struct aarch64_reg source = codegen_reg(op->ret.value);

    struct ir_var_ty *var_ty = state->ir->func_ty.ret_ty;
    struct ir_var_ty_info info = var_ty_info(state->ir->unit, var_ty);

    if (INTEGRAL_OR_PTRLIKE(var_ty)) {
      if (!is_return_reg(source)) {
        struct instr *mov = alloc_instr(state->func);

        *mov->aarch64 = MOV_ALIAS(return_reg_for_ty(source.ty), source);
      }
    } else if (var_ty_is_fp(var_ty)) {
      if (!is_return_reg(source)) {
        struct instr *mov = alloc_instr(state->func);

        *mov->aarch64 = FP_MOV_ALIAS(return_reg_for_ty(source.ty), source);
      }
    } else {
      // aggregate ty
      size_t num_members;
      size_t member_size;
      if (try_get_hfa_info(state, var_ty, &num_members, &member_size) &&
          num_members <= 4) {
        enum aarch64_reg_ty reg_ty;
        switch (member_size) {
        case 8:
          reg_ty = AARCH64_REG_TY_D;
          break;
        case 4:
          reg_ty = AARCH64_REG_TY_S;
          break;
        case 2:
          reg_ty = AARCH64_REG_TY_H;
          break;
        }

        for (size_t i = 0; i < num_members; i++) {
          struct aarch64_reg dest = {.ty = reg_ty, .idx = i};

          struct instr *mov = alloc_instr(state->func);

          mov->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
          mov->aarch64->load_imm =
              (struct aarch64_load_imm){.addr = source,
                                        .imm = i,
                                        .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                        .dest = dest};
        }
      } else if (info.size == 1) {
        struct aarch64_reg dest = {.ty = AARCH64_REG_TY_W, .idx = 0};

        struct instr *mov = alloc_instr(state->func);

        mov->aarch64->ty = AARCH64_INSTR_TY_LOAD_BYTE_IMM;
        mov->aarch64->ldrb_imm =
            (struct aarch64_load_imm){.addr = source,
                                      .imm = 0,
                                      .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                      .dest = dest};
      } else if (info.size == 2) {
        struct aarch64_reg dest = {.ty = AARCH64_REG_TY_W, .idx = 0};

        struct instr *mov = alloc_instr(state->func);

        mov->aarch64->ty = AARCH64_INSTR_TY_LOAD_HALF_IMM;
        mov->aarch64->ldrb_imm =
            (struct aarch64_load_imm){.addr = source,
                                      .imm = 0,
                                      .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                      .dest = dest};
      } else if (info.size <= 4) {
        struct aarch64_reg dest = {.ty = AARCH64_REG_TY_W, .idx = 0};

        struct instr *mov = alloc_instr(state->func);

        mov->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
        mov->aarch64->ldrb_imm =
            (struct aarch64_load_imm){.addr = source,
                                      .imm = 0,
                                      .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                      .dest = dest};
      } else if (info.size <= 8) {
        struct aarch64_reg dest = {.ty = AARCH64_REG_TY_X, .idx = 0};

        struct instr *mov = alloc_instr(state->func);

        mov->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
        mov->aarch64->ldrb_imm =
            (struct aarch64_load_imm){.addr = source,
                                      .imm = 0,
                                      .mode = AARCH64_ADDRESSING_MODE_OFFSET,
                                      .dest = dest};
      } else if (info.size <= 16) {
        struct aarch64_reg dest0 = {.ty = AARCH64_REG_TY_X, .idx = 0};
        struct aarch64_reg dest1 = {.ty = AARCH64_REG_TY_X, .idx = 1};

        struct instr *mov = alloc_instr(state->func);

        mov->aarch64->ty = AARCH64_INSTR_TY_LOAD_PAIR_IMM;
        mov->aarch64->load_pair_imm = (struct aarch64_load_pair_imm){
            .addr = source,
            .imm = 0,
            .mode = AARCH64_ADDRESSING_MODE_OFFSET,
            .dest = {dest0, dest1}};
      } else {
        TODO("larger than 16 byte types");
      }
    }
  }

  codegen_epilogue(state);

  struct instr *instr = alloc_instr(state->func);

  instr->aarch64->ty = AARCH64_INSTR_TY_RET;
  instr->aarch64->ret = (struct aarch64_branch_reg){.target = RET_PTR_REG};
}

static void codegen_op(struct codegen_state *state, struct ir_op *op) {
  trace("lowering op with id %zu, type %d", op->id, op->ty);
  switch (op->ty) {
  case IR_OP_TY_UNDF:
  case IR_OP_TY_PHI:
    break;
  case IR_OP_TY_CUSTOM: {
    BUG("custom");
  }
  case IR_OP_TY_MOV: {
    if (op->flags & IR_OP_FLAG_PARAM) {
      // don't need to do anything
    } else {
      codegen_mov_op(state, op);
    }
    break;
  }
  case IR_OP_TY_BITFIELD_INSERT:
    codegen_bitfield_insert(state, op);
    break;
  case IR_OP_TY_BITFIELD_EXTRACT:
    codegen_bitfield_extract(state, op);
    break;
  case IR_OP_TY_LOAD:
    codegen_load_op(state, op);
    break;
  case IR_OP_TY_STORE:
    codegen_store_op(state, op);
    break;
  case IR_OP_TY_ADDR: {
    codegen_addr_op(state, op);
    break;
  }
  case IR_OP_TY_BR_COND: {
    codegen_br_cond_op(state, op);
    break;
  }
  case IR_OP_TY_BR: {
    codegen_br_op(state, op);
    break;
  }
  case IR_OP_TY_CNST: {
    codegen_cnst_op(state, op);
    break;
  }
  case IR_OP_TY_UNARY_OP: {
    codegen_unary_op(state, op);
    break;
  }
  case IR_OP_TY_BINARY_OP: {
    codegen_binary_op(state, op);
    break;
  }
  case IR_OP_TY_CAST_OP: {
    codegen_cast_op(state, op);
    break;
  }
  case IR_OP_TY_CALL: {
    codegen_call_op(state, op);
    break;
  }
  case IR_OP_TY_RET: {
    codegen_ret_op(state, op);
    break;
  }
  default: {
    TODO("unsupported IR OP '%d'", op->ty);
  }
  }
}

// static void codegen_nop(struct codegen_state *state) {
//   struct instr *instr = alloc_instr(state->func);

//   instr->aarch64->ty = AARCH64_INSTR_TY_NOP;
// }

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

struct check_reg_type_data {
  // used so we know when the instruction changes
  const struct codegen_entry *entry;
  struct instr *last;

  enum aarch64_reg_ty reg_ty;
};

static void check_reg_type_callback(struct instr *instr, struct aarch64_reg reg,
                                    enum aarch64_reg_usage_ty usage_ty,
                                    void *metadata) {
  struct check_reg_type_data *data = metadata;

  if (usage_ty == AARCH64_REG_USAGE_TY_DEREF) {
    // deref only makes sense on an X register, and is ignored for comparisons
    // to other registers so back out early
    invariant_assert(
        reg.ty == AARCH64_REG_TY_X,
        "entry %zu: usage DEREF only makes sense with REG_TY_X in %zu",
        data->entry->glb_id, instr->id);
    return;
  }

  if (instr != data->last) {
    // new
    data->reg_ty = reg.ty;
  } else {
    // check reg types are the same within instructions
    // FMOV is an exception as it can move GP<->FP

    if (instr->aarch64->ty == AARCH64_INSTR_TY_FMOV) {
      size_t cur_size = reg_size(reg.ty);
      size_t last_size = reg_size(data->reg_ty);
      invariant_assert(
          cur_size == last_size,
          "entry %zu: expected `fmov` %zu to have same size registers "
          "(expected %zu found %zu)",
          data->entry->glb_id, instr->id, cur_size, last_size);
    } else if (instr->aarch64->ty == AARCH64_INSTR_TY_FCVT) {
      invariant_assert(aarch64_reg_ty_is_fp(reg.ty) &&
                           aarch64_reg_ty_is_fp(data->reg_ty),
                       "entry %zu: expected `fcvt` %zu to have all registers "
                       "floating-point",
                       data->entry->glb_id, instr->id);
    } else if (instr->aarch64->ty == AARCH64_INSTR_TY_UCVTF ||
               instr->aarch64->ty == AARCH64_INSTR_TY_SCVTF) {
      invariant_assert(
          aarch64_reg_ty_is_fp(reg.ty) != aarch64_reg_ty_is_fp(data->reg_ty),
          "entry %zu: expected `ucvtf`/`scvtf` %zu to have one fp register "
          "and one gp register",
          data->entry->glb_id, instr->id);
    } else {
      invariant_assert(
          reg.ty == data->reg_ty,
          "entry %zu: reg ty mismatch in %zu (expected %d found %d)",
          data->entry->glb_id, instr->id, data->reg_ty, reg.ty);
    }
  }

  data->last = instr;
}

static int sort_entries_by_id(const void *a, const void *b) {
  const struct codegen_entry *l = a;
  const struct codegen_entry *r = b;

  if (l->glb_id > r->glb_id) {
    return 1;
  } else if (l->glb_id == r->glb_id) {
    return 0;
  } else {
    return -1;
  }
}

static void codegen_write_var_value(struct ir_unit *iru, struct vector *relocs,
                                    size_t offset, struct ir_var_value *value,
                                    char *data) {
  if (!value || value->ty == IR_VAR_VALUE_TY_ZERO) {
    return;
  }

  switch (value->var_ty.ty) {
  case IR_VAR_TY_TY_NONE:
  case IR_VAR_TY_TY_VARIADIC:
    break;
  case IR_VAR_TY_TY_PRIMITIVE: {
    switch (value->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I8:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      memcpy(data, &value->int_value, 1);
      break;
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_I16:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
                       value->ty == IR_VAR_VALUE_TY_FLT,
                   "expected int/flt");
      memcpy(data, &value->int_value, 2);
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
    case IR_VAR_PRIMITIVE_TY_F32:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
                       value->ty == IR_VAR_VALUE_TY_FLT,
                   "expected int/flt");
      memcpy(data, &value->int_value, 4);
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
    case IR_VAR_PRIMITIVE_TY_F64:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
                       value->ty == IR_VAR_VALUE_TY_FLT,
                   "expected int/flt");
      memcpy(data, &value->int_value, sizeof(unsigned long));
      break;
    }
    break;
  }

  case IR_VAR_TY_TY_FUNC:
    BUG("func can not have data as a global var");

    // FIXME: some bugs here around using compiler-ptr-size when we mean to use
    // target-ptr-size

  case IR_VAR_TY_TY_POINTER:
  case IR_VAR_TY_TY_ARRAY:
    switch (value->ty) {
    case IR_VAR_VALUE_TY_ZERO:
    case IR_VAR_VALUE_TY_FLT:
      BUG("doesn't make sense");
    case IR_VAR_VALUE_TY_ADDR: {
      struct relocation reloc = {.ty = RELOCATION_TY_POINTER,
                                 .size = 3,
                                 .address = offset,
                                 .offset = value->addr.offset,
                                 .symbol_index = value->addr.glb->id};

      vector_push_back(relocs, &reloc);

      memcpy(data, &value->addr.offset, sizeof(void *));
      break;
    }
    case IR_VAR_VALUE_TY_INT:
      memcpy(data, &value->int_value, sizeof(void *));
      break;
    case IR_VAR_VALUE_TY_STR:
      // FIXME: !!!! doesn't work with string literals containing null char
      strcpy(data, value->str_value);
      break;
    case IR_VAR_VALUE_TY_VALUE_LIST:
      for (size_t i = 0; i < value->value_list.num_values; i++) {
        size_t value_offset = value->value_list.offsets[i];
        codegen_write_var_value(iru, relocs, offset + value_offset,
                                &value->value_list.values[i],
                                &data[value_offset]);
      }
      break;
    }
    break;

  case IR_VAR_TY_TY_STRUCT:
  case IR_VAR_TY_TY_UNION:
    DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_VALUE_LIST,
                 "expected value list");
    for (size_t i = 0; i < value->value_list.num_values; i++) {
      size_t field_offset = value->value_list.offsets[i];
      codegen_write_var_value(iru, relocs, offset + field_offset,
                              &value->value_list.values[i],
                              &data[field_offset]);
    }
  }
}

static struct codegen_entry codegen_var_data(struct ir_unit *ir, size_t id,
                                             const char *name,
                                             struct ir_var *var) {
  switch (var->ty) {
  case IR_VAR_TY_STRING_LITERAL: {
    BUG("str literal should have been lowered seperately");
  }
  case IR_VAR_TY_CONST_DATA:
  case IR_VAR_TY_DATA: {
    struct ir_var_ty_info info = var_ty_info(ir, &var->var_ty);

    // TODO: this leak
    struct vector *relocs = vector_create(sizeof(struct relocation));

    size_t len = info.size;

    char *data = arena_alloc(ir->arena, len);
    memset(data, 0, len);

    codegen_write_var_value(ir, relocs, 0, &var->value, data);

    // TODO: handle const data
    return (struct codegen_entry){
        .ty = CODEGEN_ENTRY_TY_DATA,
        .glb_id = id,
        .alignment = info.alignment,
        .name = name,
        .data = (struct codegen_data){.data = data,
                                      .len_data = len,
                                      .relocs = vector_head(relocs),
                                      .num_relocs = vector_length(relocs)}};
  }
  }
}

struct codegen_unit *aarch64_codegen(struct ir_unit *ir) {
  struct codegen_unit *unit = arena_alloc(ir->arena, sizeof(*unit));
  *unit = (struct codegen_unit){
      .ty = CODEGEN_UNIT_TY_AARCH64,
      .instr_size = sizeof(struct aarch64_instr),
      .num_entries = ir->num_globals,
      .entries = arena_alloc(ir->arena,
                             ir->num_globals * sizeof(struct codeen_entry *))};

  arena_allocator_create(&unit->arena);

  struct ir_glb *glb = ir->first_global;

  {
    size_t i = 0;
    while (glb) {
      if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
        unit->entries[i] = (struct codegen_entry){
            .ty = CODEGEN_ENTRY_TY_DECL,
            .alignment = 0,
            .glb_id = glb->id,
            .name = ir->target->mangle(ir->arena, glb->name)};

        i++;
        glb = glb->succ;
        continue;
      }

      switch (glb->ty) {
      case IR_GLB_TY_DATA: {
        // TODO: non string literals
        const char *name =
            glb->name ? ir->target->mangle(ir->arena, glb->name)
                      : mangle_str_cnst_name(ir->arena, "todo", glb->id);
        switch (glb->var->ty) {
        case IR_VAR_TY_STRING_LITERAL:
          unit->entries[i] =
              (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_STRING,
                                     .alignment = 1,
                                     .glb_id = glb->id,
                                     .name = name,
                                     .str = glb->var->value.str_value};
          break;
        case IR_VAR_TY_CONST_DATA:
        case IR_VAR_TY_DATA:
          unit->entries[i] = codegen_var_data(ir, glb->id, name, glb->var);
          break;
        }
        break;
      }
      case IR_GLB_TY_FUNC: {
        struct ir_func *ir_func = glb->func;

        clear_metadata(ir_func);

        unit->entries[i] = (struct codegen_entry){
            .ty = CODEGEN_ENTRY_TY_FUNC,
            .alignment = AARCH64_FUNCTION_ALIGNMENT,
            .glb_id = glb->id,
            .name = ir->target->mangle(ir->arena, ir_func->name),
            .func = {
                .unit = unit, .first = NULL, .last = NULL, .instr_count = 0}};

        struct arena_allocator *arena;
        arena_allocator_create(&arena);

        struct codegen_function *func = &unit->entries[i].func;
        struct codegen_state state = {
            .arena = arena,
            .target = ir->target,
            .func = func,
            .ir = ir_func,
            .ssp = {.ty = AARCH64_REG_TY_X, .idx = 28}};

        state.stack_args_size = 0;

        struct ir_basicblock *basicblock = ir_func->first;
        while (basicblock) {
          struct ir_stmt *stmt = basicblock->first;

          while (stmt) {
            struct ir_op *op = stmt->first;

            while (op) {
              if (op->ty == IR_OP_TY_CALL) {
                size_t call_args_size =
                    calc_arg_stack_space(&state, op->call.func_ty.func, op);
                state.stack_args_size =
                    MAX(state.stack_args_size, call_args_size);
              }

              op = op->succ;
            }

            stmt = stmt->succ;
          }

          basicblock = basicblock->succ;
        }

        codegen_prologue(&state);
        codegen_params(&state);

        func->prologue = state.prologue_info.prologue_generated;
        func->stack_size = state.prologue_info.stack_size;

        basicblock = ir_func->first;
        while (basicblock) {
          struct instr *first_pred = func->last;

          struct ir_stmt *stmt = basicblock->first;

          while (stmt) {
            codegen_stmt(&state, stmt);

            stmt = stmt->succ;
          }

          basicblock->first_instr = first_pred ? first_pred->succ : func->first;
          basicblock->last_instr = func->last;

          basicblock = basicblock->succ;
        }

        break;
      }
      }

      i++;
      glb = glb->succ;
    }
  }

  qsort(unit->entries, unit->num_entries, sizeof(struct codegen_entry),
        sort_entries_by_id);

  if (log_enabled()) {
    aarch64_debug_print_codegen(stderr, unit);
  }

  // now do sanity checks
  for (size_t i = 0; i < unit->num_entries; i++) {
    const struct codegen_entry *entry = &unit->entries[i];

    if (entry->ty == CODEGEN_ENTRY_TY_FUNC) {
      // codegen is now done
      // do some basic sanity checks
      struct check_reg_type_data data = {
          .entry = entry, .last = NULL, .reg_ty = 0};
      walk_regs(&entry->func, check_reg_type_callback, &data);
    }
  }

  return unit;
}

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

void walk_regs(const struct codegen_function *func, walk_regs_callback *cb,
               void *metadata) {
  struct instr *instr = func->first;

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
      cb(instr, conditional_select.dest, AARCH64_REG_USAGE_TY_WRITE, metadata);
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
      cb(instr, store_pair_imm.source[0], AARCH64_REG_USAGE_TY_READ, metadata);
      cb(instr, store_pair_imm.source[1], AARCH64_REG_USAGE_TY_READ, metadata);
      break;
    }
    }

    instr = instr->succ;
  }
}

static void codegen_fprintf(FILE *file, const char *format, ...) {
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

      size_t size = reg_size(reg.ty);
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

      if (reg.idx == 31) {
        fprintf(file, "zr");
      } else {
        fprintf(file, "%zu", reg.idx);
      }

      format += 3;
    } else if (strncmp(format, "instr", 5) == 0) {
      struct instr *instr = va_arg(list, struct instr *);
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
    } else if (format[0] == '%') {
      fputc('%', file);
      format++;
    } else {
      BUG("unrecognised format starting '%%%s'", format);
    }
  }
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
                  conditional_branch->target->first_instr);
}

static void debug_print_branch(FILE *file,
                               const struct aarch64_branch *branch) {
  codegen_fprintf(file, " %instr", branch->target->first_instr);
}

static void
debug_print_branch_reg(FILE *file,
                       const struct aarch64_branch_reg *branch_reg) {
  codegen_fprintf(file, " %reg", branch_reg->target);
}

static void debug_print_compare_and_branch(
    FILE *file, const struct aarch64_compare_and_branch *compare_and_branch) {
  codegen_fprintf(file, " %reg, %instr", compare_and_branch->cmp,
                  compare_and_branch->target->first_instr);
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
  codegen_fprintf(file, " %reg, %reg, %reg", fma->lhs, fma->rhs, fma->addsub);
}

static void debug_print_instr(FILE *file,
                              UNUSED_ARG(const struct codegen_function *func),
                              const struct instr *instr) {

  switch (instr->aarch64->ty) {
  case AARCH64_INSTR_TY_NOP:
    fprintf(file, "nop");
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
    fprintf(file, "madd");
    debug_print_fma(file, &instr->aarch64->madd);
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

void aarch64_debug_print_codegen(FILE *file, struct codegen_unit *unit) {
  DEBUG_ASSERT(unit->ty == CODEGEN_UNIT_TY_AARCH64, "expected aarch64");

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
      debug_print_instr(file, func, instr);

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
