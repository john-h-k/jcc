#include "codegen.h"

#include "../alloc.h"
#include "../bit_twiddle.h"
#include "../bitset.h"
#include "../log.h"
#include "../rv32i.h"
#include "../vector.h"

#define NOP_ALIAS                                                              \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_ADDI, .addi = {                                       \
      .dest = (GP_ZERO_REG),                                                   \
      .source = (GP_ZERO_REG),                                                 \
      .imm = 0,                                                                \
    }                                                                          \
  }

#define MOV_ALIAS(dest_reg, source_reg)                                        \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_ADDI, .addi = {                                       \
      .dest = (dest_reg),                                                      \
      .source = (source_reg),                                                  \
      .imm = 0,                                                                \
    }                                                                          \
  }

#define FP32_MOV_ALIAS(dest_reg, source_reg)                                   \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_FSGNJ_S, .add = {                                     \
      .lhs = (source_reg),                                                     \
      .rhs = (source_reg),                                                     \
      .dest = (dest_reg),                                                      \
    }                                                                          \
  }

#define FP64_MOV_ALIAS(dest_reg, source_reg)                                   \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_FSGNJ_D, .add = {                                     \
      .lhs = (source_reg),                                                     \
      .rhs = (source_reg),                                                     \
      .dest = (dest_reg),                                                      \
    }                                                                          \
  }

const char *rv32i_mangle(UNUSED struct arena_allocator *arena,
                         const char *name) {
  return name;
}

struct rv32i_prologue_info {
  bool prologue_generated;
  size_t stack_size;
  size_t save_start;
};

static ssize_t get_lcl_stack_offset(const struct cg_state *state,
                                    UNUSED const struct ir_op *op,
                                    const struct ir_lcl *lcl) {
  DEBUG_ASSERT(lcl->alloc_ty != IR_LCL_ALLOC_TY_NONE, "unallocated lcl");

  ssize_t offset = lcl->alloc.offset;

  if (lcl->alloc_ty == IR_LCL_ALLOC_TY_FIXED && offset <= 0 &&
      lcl->flags & IR_LCL_FLAG_PARAM) {
    offset = state->rv32i_prologue_info->stack_size + -offset;
  }

  if (lcl->alloc_ty == IR_LCL_ALLOC_TY_NORMAL) {
    offset += state->ir->caller_stack_needed;
  }

  return offset;
}

static enum rv32i_reg_ty reg_ty_for_var_ty(const struct ir_var_ty *var_ty) {
  switch (var_ty->primitive) {
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
    return RV32I_REG_TY_W;
  case IR_VAR_PRIMITIVE_TY_I64:
    TODO("i64");
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    return RV32I_REG_TY_F;
  }
}

static size_t translate_reg_idx(size_t idx, enum ir_reg_ty ty) {
  switch (ty) {
  case IR_REG_TY_NONE:
  case IR_REG_TY_SPILLED:
  case IR_REG_TY_FLAGS:
    BUG("does not make sense for none/spilled/flags");
  case IR_REG_TY_INTEGRAL:
    if (idx >= 27) {
      BUG("invalid idx");
    } else if (idx >= 17) {
      return 18 + (idx - 17);
    } else if (idx >= 15) {
      return 8 + (idx - 15);
    } else if (idx >= 11) {
      return 28 + (idx - 11);
    } else if (idx >= 8) {
      return 5 + (idx - 8);
    } else {
      return 10 + idx;
    }
  case IR_REG_TY_FP:
    if (idx >= 22) {
      return 18 + (idx - 22);
    } else if (idx >= 20) {
      return 8 + (idx - 20);
    } else if (idx >= 16) {
      return 28 + (idx - 16);
    } else if (idx >= 8) {
      return idx - 8;
    } else {
      return 10 + idx;
    }
  }
}

static struct rv32i_reg codegen_reg(struct ir_op *op) {
  size_t idx = translate_reg_idx(op->reg.idx, op->reg.ty);

  DEBUG_ASSERT(idx < 32, "got invalid reg idx (ty=%d, idx=%zu)", op->reg.ty,
               op->reg.idx);

  if (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE) {
    TODO("non primitives (op %zu)", op->id);
  }

  enum rv32i_reg_ty reg_ty = reg_ty_for_var_ty(&op->var_ty);

  switch (reg_ty) {
  case RV32I_REG_TY_W:
    invariant_assert(op->reg.ty == IR_REG_TY_INTEGRAL, "expected integral reg");
    break;
  case RV32I_REG_TY_F:
    invariant_assert(op->reg.ty == IR_REG_TY_FP, "expected fp reg");
    break;
  default:
    TODO("other reg tys (Q/V)");
  }

  return (struct rv32i_reg){.ty = reg_ty, .idx = idx};
}

static bool rv32i_reg_ty_is_gp(enum rv32i_reg_ty ty) {
  return ty == RV32I_REG_TY_W;
}

static void codegen_mov_op(struct cg_state *state,
                           struct cg_basicblock *basicblock, struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg source = codegen_reg(op->mov.value);

  struct instr *instr = cg_alloc_instr(state->func, basicblock);
  if (rv32i_reg_ty_is_gp(source.ty) && rv32i_reg_ty_is_gp(dest.ty)) {
    *instr->rv32i = MOV_ALIAS(dest, source);
  } else if (!rv32i_reg_ty_is_gp(source.ty) && !rv32i_reg_ty_is_gp(dest.ty)) {
    switch (op->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_F32:
      *instr->rv32i = FP32_MOV_ALIAS(dest, source);
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      *instr->rv32i = FP64_MOV_ALIAS(dest, source);
      break;
    default:
      BUG("unsupported");
    }
  } else {
    DEBUG_ASSERT(op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F32,
                 "rv32i doesn't support x<->f for double");
    instr->rv32i->ty = RV32I_INSTR_TY_FMV_S;
    instr->rv32i->fmv = (struct rv32i_op_mov){.source = source, .dest = dest};
  }
}

static enum rv32i_instr_ty invert_cond(enum rv32i_instr_ty ty) {
  DEBUG_ASSERT(ty <= RV32I_INSTR_TY_BGEU, "not a condition instruction!");

  return ty ^ 1;
}

#define SIGNED_IMM_FITS(imm, sz)                                               \
  (((imm) < 0 && ((simm_t)(imm) >> ((sz)-1)) == -1) ||                         \
   ((imm) >= 0 && ((simm_t)(imm) >> ((sz)-1)) == 0))

static void codegen_br_cond_op(struct cg_state *state,
                               struct cg_basicblock *basicblock,
                               struct ir_op *op) {
  struct ir_basicblock *true_target = op->stmt->basicblock->split.true_target;
  struct ir_basicblock *false_target = op->stmt->basicblock->split.false_target;

  struct ir_op *cond = op->br_cond.cond;

  enum rv32i_instr_ty br_ty;
  bool invert_ops;
  struct rv32i_reg lhs, rhs;

  if (cond->flags & IR_OP_FLAG_CONTAINED) {
    lhs = codegen_reg(cond->binary_op.lhs);
    rhs = codegen_reg(cond->binary_op.rhs);

    switch (cond->binary_op.ty) {
    case IR_OP_BINARY_OP_TY_EQ:
      br_ty = RV32I_INSTR_TY_BEQ;
      invert_ops = false;
      break;
    case IR_OP_BINARY_OP_TY_NEQ:
      br_ty = RV32I_INSTR_TY_BNE;
      invert_ops = false;
      break;
    case IR_OP_BINARY_OP_TY_UGT:
      br_ty = RV32I_INSTR_TY_BLTU;
      invert_ops = true;
      break;
    case IR_OP_BINARY_OP_TY_SGT:
      br_ty = RV32I_INSTR_TY_BLT;
      invert_ops = true;
      break;
    case IR_OP_BINARY_OP_TY_UGTEQ:
      br_ty = RV32I_INSTR_TY_BGEU;
      invert_ops = false;
      break;
    case IR_OP_BINARY_OP_TY_SGTEQ:
      br_ty = RV32I_INSTR_TY_BGE;
      invert_ops = false;
      break;
    case IR_OP_BINARY_OP_TY_ULT:
      br_ty = RV32I_INSTR_TY_BLTU;
      invert_ops = false;
      break;
    case IR_OP_BINARY_OP_TY_SLT:
      br_ty = RV32I_INSTR_TY_BLT;
      invert_ops = false;
      break;
    case IR_OP_BINARY_OP_TY_ULTEQ:
      br_ty = RV32I_INSTR_TY_BGEU;
      invert_ops = true;
      break;
    case IR_OP_BINARY_OP_TY_SLTEQ:
      br_ty = RV32I_INSTR_TY_BGE;
      invert_ops = true;
      break;
    default:
      BUG("floating point etc");
    }

  } else {
    br_ty = RV32I_INSTR_TY_BNE;
    invert_ops = false;
    lhs = codegen_reg(cond);
    rhs = GP_ZERO_REG;
  }

  struct instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->rv32i->ty = br_ty;
  instr->rv32i->conditional_branch = (struct rv32i_conditional_branch){
      .lhs = invert_ops ? rhs : lhs,
      .rhs = invert_ops ? lhs : rhs,
      .target = RV32I_BASICBLOCK_TARGET(true_target)};

  // rv32i requires turning `br.cond <true> <false>` into 2 instructions
  // we represent this as just the `true` part of the `br.cond`, and then a
  // `br`
  // after branching to the false target

  // now generate the `br`
  struct instr *br = cg_alloc_instr(state->func, basicblock);
  br->rv32i->ty = RV32I_INSTR_TY_JAL;
  br->rv32i->jal = (struct rv32i_jal){
      .ret_addr = GP_ZERO_REG, .target = RV32I_BASICBLOCK_TARGET(false_target)};
}

static void codegen_br_op(struct cg_state *state,
                          struct cg_basicblock *basicblock, struct ir_op *op) {
  struct instr *instr = cg_alloc_instr(state->func, basicblock);
  instr->rv32i->ty = RV32I_INSTR_TY_JAL;
  instr->rv32i->jal = (struct rv32i_jal){
      .ret_addr = GP_ZERO_REG,
      .target = RV32I_BASICBLOCK_TARGET(op->stmt->basicblock->merge.target)};
}

static void codegen_cnst_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  DEBUG_ASSERT(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
               "expects primitive type");

  struct rv32i_reg dest = codegen_reg(op);

  switch (op->cnst.ty) {
  case IR_OP_CNST_TY_FLT:
    // currently all constants are lowered to an integer load and `fmov`
    // but lots of constants can be loaded directly, so do that here
    TODO("simple float constants (not lowered)");
  case IR_OP_CNST_TY_INT:
    switch (op->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I8:
    case IR_VAR_PRIMITIVE_TY_I16:
    case IR_VAR_PRIMITIVE_TY_I32: {
      unsigned long long cnst = op->cnst.int_value;

      int hi = (int)(((unsigned)cnst + 0x800) >> 12);
      if ((hi >> 19) & 1) {
        // immediate validation needs it to be the desired sign
        hi = (uint32_t)hi | (0xFFFu << 20);
      }

      int lo = (int)((unsigned)cnst - ((unsigned)hi << 12));

      struct rv32i_reg source_reg = GP_ZERO_REG;

      if (hi) {
        struct instr *instr = cg_alloc_instr(state->func, basicblock);
        instr->rv32i->ty = RV32I_INSTR_TY_LUI;
        instr->rv32i->lui = (struct rv32i_u){.dest = dest, .imm = hi};

        source_reg = dest;
      }

      struct instr *instr = cg_alloc_instr(state->func, basicblock);
      instr->rv32i->ty = RV32I_INSTR_TY_ADDI;
      instr->rv32i->addi =
          (struct rv32i_op_imm){.dest = dest, .source = source_reg, .imm = lo};

      break;
    }
    case IR_VAR_PRIMITIVE_TY_I64:
      TODO("i64");
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_F32:
    case IR_VAR_PRIMITIVE_TY_F64:
      unreachable();
    };
  }
}

static void codegen_add_imm(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct rv32i_reg dest, struct rv32i_reg source,
                            long long value);

#define RV32I_STACK_ALIGNMENT (16)

static void codegen_prologue(struct cg_state *state) {
  struct ir_func *ir = state->ir;

  size_t stack_size =
      state->ir->total_locals_size + state->ir->caller_stack_needed;
  stack_size = ROUND_UP(stack_size, RV32I_STACK_ALIGNMENT);

  size_t num_nonvolatile_used = ir->reg_usage.num_nonvolatile_used;

  size_t save_start = stack_size;

  // save nonvol
  stack_size += num_nonvolatile_used * 8;
  stack_size = ROUND_UP(stack_size, RV32I_STACK_ALIGNMENT);

  bool leaf = !(stack_size || ir->flags & IR_FUNC_FLAG_MAKES_CALL);

  struct rv32i_prologue_info info = {.prologue_generated = !leaf,
                                     .save_start = save_start,
                                     .stack_size = stack_size};

  state->rv32i_prologue_info =
      arena_alloc(state->arena, sizeof(*state->rv32i_prologue_info));

  if (!info.prologue_generated) {
    *state->rv32i_prologue_info = info;
    return;
  }

  info.stack_size += 8;

  info.stack_size = ROUND_UP(info.stack_size, RV32I_STACK_ALIGNMENT);

  struct cg_basicblock *basicblock = cg_alloc_basicblock(state->func, NULL);

  struct instr *save_ra = cg_alloc_instr(state->func, basicblock);
  save_ra->rv32i->ty = RV32I_INSTR_TY_SW;
  save_ra->rv32i->sw = (struct rv32i_store){
      .source = RET_PTR_REG, .addr = STACK_PTR_REG, .imm = -4};

  struct instr *save_sp = cg_alloc_instr(state->func, basicblock);
  save_sp->rv32i->ty = RV32I_INSTR_TY_SW;
  save_sp->rv32i->sw = (struct rv32i_store){
      .source = STACK_PTR_REG, .addr = STACK_PTR_REG, .imm = -8};

  ssize_t stack_to_sub = info.stack_size;
  codegen_add_imm(state, basicblock, STACK_PTR_REG, STACK_PTR_REG,
                  -stack_to_sub);

  for (size_t i = 0; i < num_nonvolatile_used; i++) {
    struct ir_reg reg = ir->reg_usage.nonvolatile_used[i];

    size_t offset = info.save_start + (i * 8);

    switch (reg.ty) {
    case IR_REG_TY_INTEGRAL: {

      struct instr *save = cg_alloc_instr(state->func, basicblock);
      save->rv32i->ty = RV32I_INSTR_TY_SW;
      save->rv32i->sw = (struct rv32i_store){
          .source = (struct rv32i_reg){.ty = RV32I_REG_TY_W,
                                       .idx = translate_reg_idx(
                                           reg.idx, IR_REG_TY_INTEGRAL)},
          .addr = STACK_PTR_REG,
          .imm = offset};
      break;
    }
    case IR_REG_TY_FP: {
      struct instr *save = cg_alloc_instr(state->func, basicblock);
      save->rv32i->ty = RV32I_INSTR_TY_FSD;
      save->rv32i->fsw = (struct rv32i_store){
          .source = (struct rv32i_reg){.ty = RV32I_REG_TY_F,
                                       .idx = translate_reg_idx(reg.idx,
                                                                IR_REG_TY_FP)},
          .addr = STACK_PTR_REG,
          .imm = offset};
      break;
    }
    default:
      BUG("can't save this reg ty");
    }
  }

  *state->rv32i_prologue_info = info;
}

#define IMM_BITS (12)

static void codegen_add_imm(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct rv32i_reg dest, struct rv32i_reg source,
                            long long value) {
  long long abs_val = value >= 0 ? value : -value;

  do {
    ssize_t chunk = MIN(0x7FF, abs_val);

    struct instr *instr = cg_alloc_instr(state->func, basicblock);
    instr->rv32i->ty = RV32I_INSTR_TY_ADDI;
    instr->rv32i->addi = (struct rv32i_op_imm){
        .dest = dest,
        .source = source,
        .imm = value >= 0 ? chunk : -chunk,
    };

    source = dest;

    abs_val -= chunk;
  } while (abs_val);
}

static void codegen_epilogue(struct cg_state *state,
                             struct cg_basicblock *basicblock) {
  const struct rv32i_prologue_info *prologue_info = state->rv32i_prologue_info;

  if (!prologue_info->prologue_generated) {
    return;
  }

  for (size_t i = 0; i < state->ir->reg_usage.num_nonvolatile_used; i++) {
    struct ir_reg reg = state->ir->reg_usage.nonvolatile_used[i];

    size_t offset = prologue_info->save_start + (i * 8);

    switch (reg.ty) {
    case IR_REG_TY_INTEGRAL: {
      struct instr *restore = cg_alloc_instr(state->func, basicblock);
      restore->rv32i->ty = RV32I_INSTR_TY_LW;
      restore->rv32i->lw = (struct rv32i_load){
          .imm = offset,
          .dest = (struct rv32i_reg){.ty = RV32I_REG_TY_W,
                                     .idx = translate_reg_idx(
                                         reg.idx, IR_REG_TY_INTEGRAL)},
          .addr = STACK_PTR_REG,
      };
      break;
    }
    case IR_REG_TY_FP: {
      struct instr *restore = cg_alloc_instr(state->func, basicblock);
      restore->rv32i->ty = RV32I_INSTR_TY_FLW;
      restore->rv32i->flw = (struct rv32i_load){
          .imm = offset,
          .dest = (struct rv32i_reg){.ty = RV32I_REG_TY_F,
                                     .idx = translate_reg_idx(reg.idx,
                                                              IR_REG_TY_FP)},
          .addr = STACK_PTR_REG,
      };
      break;
    }
    default:
      BUG("can't save this reg ty");
    }
  }

  size_t stack_to_add = prologue_info->stack_size;
  codegen_add_imm(state, basicblock, STACK_PTR_REG, STACK_PTR_REG,
                  stack_to_add);

  struct instr *restore_ra = cg_alloc_instr(state->func, basicblock);
  restore_ra->rv32i->ty = RV32I_INSTR_TY_LW;
  restore_ra->rv32i->lw = (struct rv32i_load){
      .dest = RET_PTR_REG, .addr = STACK_PTR_REG, .imm = -4};

  struct instr *restore_sp = cg_alloc_instr(state->func, basicblock);
  restore_sp->rv32i->ty = RV32I_INSTR_TY_LW;
  restore_sp->rv32i->lw = (struct rv32i_load){
      .dest = STACK_PTR_REG, .addr = STACK_PTR_REG, .imm = -8};
}

static void codegen_ret_op(struct cg_state *state,
                           struct cg_basicblock *basicblock,
                           UNUSED struct ir_op *op) {
  codegen_epilogue(state, basicblock);

  struct instr *instr = cg_alloc_instr(state->func, basicblock);

  instr->rv32i->ty = RV32I_INSTR_TY_JALR;
  instr->rv32i->jalr = (struct rv32i_jalr){
      .ret_addr = GP_ZERO_REG, .target = RET_PTR_REG, .imm = 0};
}

static void codegen_unary_op(struct cg_state *state,
                             struct cg_basicblock *basicblock,
                             struct ir_op *op) {
  struct instr *instr = cg_alloc_instr(state->func, basicblock);

  struct rv32i_reg dest = codegen_reg(op);
  struct rv32i_reg source = codegen_reg(op->unary_op.value);

  bool fp64 = op->unary_op.value->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64;
#define SEL_FP_INSTR(ty) (fp64 ? ty##_D : ty##_S)
  switch (op->unary_op.ty) {
  case IR_OP_UNARY_OP_TY_FNEG:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FSGNJN);
    instr->rv32i->fsgnjn = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = source,
        .rhs = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_FABS:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FSGNJX);
    instr->rv32i->fsgnjx = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = source,
        .rhs = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_FSQRT:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FSQRT);
    instr->rv32i->fsqrt =
        (struct rv32i_op_unary_fp){.dest = dest, .source = source};
    return;
  case IR_OP_UNARY_OP_TY_NEG:
    instr->rv32i->ty = RV32I_INSTR_TY_SUB;
    instr->rv32i->sub = (struct rv32i_op){
        .dest = dest,
        .lhs = GP_ZERO_REG,
        .rhs = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_NOT:
    instr->rv32i->ty = RV32I_INSTR_TY_XORI;
    instr->rv32i->xori =
        (struct rv32i_op_imm){.dest = dest, .source = source, .imm = -1};
    return;
  case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
    instr->rv32i->ty = RV32I_INSTR_TY_SLTIU;
    instr->rv32i->sltiu =
        (struct rv32i_op_imm){.dest = dest, .source = source, .imm = 1};
    return;
  }
#undef SEL_FP_INSTR
}

static void codegen_binary_op(struct cg_state *state,
                              struct cg_basicblock *basicblock,
                              struct ir_op *op) {
  struct instr *instr = cg_alloc_instr(state->func, basicblock);

  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg lhs = codegen_reg(op->binary_op.lhs);
  struct rv32i_reg rhs = codegen_reg(op->binary_op.rhs);

  enum ir_op_binary_op_ty ty = op->binary_op.ty;

  enum rv32i_instr_ty instr_ty;
  bool invert_cond, invert_res;

  bool fp64 = op->binary_op.lhs->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64;
#define SEL_FP_INSTR(ty) (fp64 ? ty##_D : ty##_S)

  // floating point has different ops (eq + lt + le) to integral so they need to
  // have different logic
  switch (ty) {
  case IR_OP_BINARY_OP_TY_FEQ:
    instr_ty = SEL_FP_INSTR(RV32I_INSTR_TY_FEQ);
    invert_cond = false;
    invert_res = false;
    goto set_fp_cmp;

  case IR_OP_BINARY_OP_TY_FNEQ:
    instr_ty = SEL_FP_INSTR(RV32I_INSTR_TY_FEQ);
    invert_cond = false;
    invert_res = true;
    goto set_fp_cmp;

  case IR_OP_BINARY_OP_TY_FGT:
    instr_ty = SEL_FP_INSTR(RV32I_INSTR_TY_FLE);
    invert_cond = true;
    invert_res = false;
    goto set_fp_cmp;

  case IR_OP_BINARY_OP_TY_FGTEQ:
    instr_ty = SEL_FP_INSTR(RV32I_INSTR_TY_FLT);
    invert_cond = true;
    invert_res = false;
    goto set_fp_cmp;

  case IR_OP_BINARY_OP_TY_FLT:
    instr_ty = SEL_FP_INSTR(RV32I_INSTR_TY_FLT);
    invert_cond = false;
    invert_res = false;
    goto set_fp_cmp;

  case IR_OP_BINARY_OP_TY_FLTEQ:
    instr_ty = SEL_FP_INSTR(RV32I_INSTR_TY_FLE);
    invert_cond = false;
    invert_res = false;
    goto set_fp_cmp;

  set_fp_cmp: {
    instr->rv32i->ty = instr_ty;
    instr->rv32i->op_fp = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = invert_cond ? rhs : lhs,
        .rhs = invert_cond ? lhs : rhs,
    };

    if (invert_res) {
      struct instr *xori = cg_alloc_instr(state->func, basicblock);
      xori->rv32i->ty = RV32I_INSTR_TY_XORI;
      xori->rv32i->xori =
          (struct rv32i_op_imm){.dest = dest, .source = dest, .imm = 1};
    }
    break;
  }

  case IR_OP_BINARY_OP_TY_EQ: {
    instr->rv32i->ty = RV32I_INSTR_TY_XOR;
    instr->rv32i->xor = (struct rv32i_op){
                          .dest = dest,
                          .lhs = lhs,
                          .rhs = rhs,
                      };

    struct instr *sltiu = cg_alloc_instr(state->func, basicblock);
    sltiu->rv32i->ty = RV32I_INSTR_TY_SLTIU;
    sltiu->rv32i->sltiu =
        (struct rv32i_op_imm){.dest = dest, .source = dest, .imm = 1};
    break;
  }

  case IR_OP_BINARY_OP_TY_NEQ: {
    instr->rv32i->ty = RV32I_INSTR_TY_XOR;
    instr->rv32i->xor = (struct rv32i_op){
                          .dest = dest,
                          .lhs = lhs,
                          .rhs = rhs,
                      };

    struct instr *sltu = cg_alloc_instr(state->func, basicblock);
    sltu->rv32i->ty = RV32I_INSTR_TY_SLTU;
    sltu->rv32i->sltu =
        (struct rv32i_op){.dest = dest, .lhs = GP_ZERO_REG, .rhs = dest};
    break;
  }

  case IR_OP_BINARY_OP_TY_UGT:
    instr_ty = RV32I_INSTR_TY_SLTU;
    invert_cond = true;
    invert_res = false;
    goto set_cmp;

  case IR_OP_BINARY_OP_TY_SGT:
    instr_ty = RV32I_INSTR_TY_SLT;
    invert_cond = true;
    invert_res = false;
    goto set_cmp;

  case IR_OP_BINARY_OP_TY_UGTEQ:
    instr_ty = RV32I_INSTR_TY_SLTU;
    invert_cond = false;
    invert_res = true;
    goto set_cmp;

  case IR_OP_BINARY_OP_TY_SGTEQ:
    instr_ty = RV32I_INSTR_TY_SLT;
    invert_cond = false;
    invert_res = true;
    goto set_cmp;

  case IR_OP_BINARY_OP_TY_ULT:
    instr_ty = RV32I_INSTR_TY_SLTU;
    invert_cond = false;
    invert_res = false;
    goto set_cmp;

  case IR_OP_BINARY_OP_TY_SLT:
    instr_ty = RV32I_INSTR_TY_SLT;
    invert_cond = false;
    invert_res = false;
    goto set_cmp;

  case IR_OP_BINARY_OP_TY_ULTEQ:
    instr_ty = RV32I_INSTR_TY_SLTU;
    invert_cond = true;
    invert_res = true;
    goto set_cmp;

  case IR_OP_BINARY_OP_TY_SLTEQ:
    instr_ty = RV32I_INSTR_TY_SLT;
    invert_cond = true;
    invert_res = true;
    goto set_cmp;

  set_cmp: {
    instr->rv32i->ty = instr_ty;
    instr->rv32i->op = (struct rv32i_op){
        .dest = dest,
        .lhs = invert_cond ? rhs : lhs,
        .rhs = invert_cond ? lhs : rhs,
    };

    if (invert_res) {
      struct instr *xori = cg_alloc_instr(state->func, basicblock);
      xori->rv32i->ty = RV32I_INSTR_TY_XORI;
      xori->rv32i->xori =
          (struct rv32i_op_imm){.dest = dest, .source = dest, .imm = 1};
    }
    break;
  }
  case IR_OP_BINARY_OP_TY_LSHIFT:
    instr->rv32i->ty = RV32I_INSTR_TY_SLL;
    instr->rv32i->sll = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_SRSHIFT:
    instr->rv32i->ty = RV32I_INSTR_TY_SRA;
    instr->rv32i->sra = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_URSHIFT:
    instr->rv32i->ty = RV32I_INSTR_TY_SRL;
    instr->rv32i->srl = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_AND:
    instr->rv32i->ty = RV32I_INSTR_TY_AND;
    instr->rv32i->and = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_OR:
    instr->rv32i->ty = RV32I_INSTR_TY_OR;
    instr->rv32i->or = (struct rv32i_op){
                         .dest = dest,
                         .lhs = lhs,
                         .rhs = rhs,
                     };
    break;
  case IR_OP_BINARY_OP_TY_XOR:
    instr->rv32i->ty = RV32I_INSTR_TY_XOR;
    instr->rv32i->xor = (struct rv32i_op){
                          .dest = dest,
                          .lhs = lhs,
                          .rhs = rhs,
                      };
    break;
  case IR_OP_BINARY_OP_TY_ADD:
    instr->rv32i->ty = RV32I_INSTR_TY_ADD;
    instr->rv32i->add = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_SUB:
    instr->rv32i->ty = RV32I_INSTR_TY_SUB;
    instr->rv32i->sub = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_MUL:
    instr->rv32i->ty = RV32I_INSTR_TY_MUL;
    instr->rv32i->mul = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_SDIV:
    instr->rv32i->ty = RV32I_INSTR_TY_DIV;
    instr->rv32i->div = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_UDIV:
    instr->rv32i->ty = RV32I_INSTR_TY_DIVU;
    instr->rv32i->divu = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_SQUOT:
    instr->rv32i->ty = RV32I_INSTR_TY_REM;
    instr->rv32i->rem = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_UQUOT:
    instr->rv32i->ty = RV32I_INSTR_TY_REMU;
    instr->rv32i->remu = (struct rv32i_op){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FADD:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FADD);
    instr->rv32i->fadd = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FSUB:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FSUB);
    instr->rv32i->fsub = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMUL:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FMUL);
    instr->rv32i->fmul = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FDIV:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FDIV);
    instr->rv32i->fdiv = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMAX:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FMAX);
    instr->rv32i->fmax = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMIN:
    instr->rv32i->ty = SEL_FP_INSTR(RV32I_INSTR_TY_FMIN);
    instr->rv32i->fmin = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  }
#undef SEL_FP_INSTR
}

static enum rv32i_instr_ty load_ty_for_op(struct ir_op *op) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
    return RV32I_INSTR_TY_LBU;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return RV32I_INSTR_TY_LHU;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I32) {
    return RV32I_INSTR_TY_LW;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F32) {
    return RV32I_INSTR_TY_FLW;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64) {
    return RV32I_INSTR_TY_FLD;
  } else {
    BUG("unknown load ty");
  }
}

static enum rv32i_instr_ty store_ty_for_op(struct ir_op *op) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
    return RV32I_INSTR_TY_SB;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return RV32I_INSTR_TY_SH;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I32) {
    return RV32I_INSTR_TY_SW;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F32) {
    return RV32I_INSTR_TY_FSW;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_F64) {
    return RV32I_INSTR_TY_FSD;
  } else {
    BUG("unknown store ty");
  }
}

static void codegen_load_addr_op(struct cg_state *state,
                                 struct cg_basicblock *basicblock,
                                 struct ir_op *op) {
  struct instr *instr = cg_alloc_instr(state->func, basicblock);

  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg addr;
  size_t offset;

  if (op->load.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr_op = op->load.addr;

    if (addr_op->ty == IR_OP_TY_ADDR && addr_op->addr.ty == IR_OP_ADDR_TY_LCL) {
      addr = STACK_PTR_REG;
      offset = get_lcl_stack_offset(state, op, addr_op->addr.lcl);
    } else if (addr_op->ty == IR_OP_TY_ADDR_OFFSET) {
      struct ir_op *base = addr_op->addr_offset.base;

      offset = addr_op->addr_offset.offset;

      if (base->flags & IR_OP_FLAG_CONTAINED) {
        addr = STACK_PTR_REG;
        offset = get_lcl_stack_offset(state, op, base->addr.lcl) +
                 addr_op->addr_offset.offset;
      } else {
        addr = codegen_reg(base);
      }
    } else {
      BUG("can't CONTAIN operand in load_addr node");
    }
  } else {
    addr = codegen_reg(op->load.addr);
    offset = 0;
  }

  instr->rv32i->ty = load_ty_for_op(op);
  instr->rv32i->load =
      (struct rv32i_load){.dest = dest, .addr = addr, .imm = offset};
}

static void codegen_store_addr_op(struct cg_state *state,
                                  struct cg_basicblock *basicblock,
                                  struct ir_op *op) {
  struct instr *instr = cg_alloc_instr(state->func, basicblock);

  struct rv32i_reg source = codegen_reg(op->store.value);

  struct rv32i_reg addr;
  size_t offset;

  if (op->store.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr_op = op->store.addr;

    if (addr_op->ty == IR_OP_TY_ADDR && addr_op->addr.ty == IR_OP_ADDR_TY_LCL) {
      addr = STACK_PTR_REG;
      offset = get_lcl_stack_offset(state, op, addr_op->addr.lcl);
    } else if (addr_op->ty == IR_OP_TY_ADDR_OFFSET) {
      struct ir_op *base = addr_op->addr_offset.base;

      offset = addr_op->addr_offset.offset;

      if (base->flags & IR_OP_FLAG_CONTAINED) {
        addr = STACK_PTR_REG;
        offset = get_lcl_stack_offset(state, op, base->addr.lcl) +
                 addr_op->addr_offset.offset;
      } else {
        addr = codegen_reg(base);
      }
    } else {
      BUG("can't CONTAIN operand in store_addr node");
    }
  } else {
    addr = codegen_reg(op->store.addr);
    offset = 0;
  }

  instr->rv32i->ty = store_ty_for_op(op->store.value);
  instr->rv32i->store =
      (struct rv32i_store){.source = source, .addr = addr, .imm = offset};
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

static void codegen_addr_offset_op(struct cg_state *state,
                                   struct cg_basicblock *basicblock,
                                   struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);

  // we could just lower this to add. it is a rather pointless op on rv32i

  DEBUG_ASSERT(!op->addr_offset.index, "rv32i doesn't support index");

  struct ir_op_addr_offset *addr_offset = &op->addr_offset;
  ssize_t offset = addr_offset->offset;

  struct rv32i_reg base;

  if (addr_offset->base->flags & IR_OP_FLAG_CONTAINED) {
    DEBUG_ASSERT(addr_offset->base->ty == IR_OP_TY_ADDR &&
                     addr_offset->base->addr.ty == IR_OP_ADDR_TY_LCL,
                 "can only contain `addr LCL`");

    base = STACK_PTR_REG;
    offset += get_lcl_stack_offset(state, NULL, addr_offset->base->addr.lcl);
  } else {
    base = codegen_reg(addr_offset->base);
  }

  codegen_add_imm(state, basicblock, dest, base, offset);
}

static void codegen_addr_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);

  switch (op->addr.ty) {
  case IR_OP_ADDR_TY_LCL: {
    struct ir_lcl *lcl = op->addr.lcl;

    // op is NULL as we want the absolute offset
    size_t offset = get_lcl_stack_offset(state, NULL, lcl);

    codegen_add_imm(state, basicblock, dest, STACK_PTR_REG, offset);

    break;
  }
  case IR_OP_ADDR_TY_GLB: {
    struct ir_glb *glb = op->addr.glb;

    struct instr *adrp = cg_alloc_instr(state->func, basicblock);
    adrp->rv32i->ty = RV32I_INSTR_TY_LUI;
    adrp->rv32i->lui = (struct rv32i_u){.dest = dest, .imm = 0};

    adrp->reloc = arena_alloc(state->func->unit->arena, sizeof(*adrp->reloc));
    *adrp->reloc = (struct relocation){
        .ty = glb->def_ty == IR_GLB_DEF_TY_DEFINED ? RELOCATION_TY_LOCAL_PAIR
                                                   : RELOCATION_TY_UNDEF_PAIR,
        .symbol_index = glb->id,
        .address = 0,
        .size = 0};

    struct instr *add = cg_alloc_instr(state->func, basicblock);
    add->rv32i->ty = RV32I_INSTR_TY_ADDI;
    add->rv32i->addi = (struct rv32i_op_imm){
        .dest = dest,
        .source = dest,
        .imm = 0,
    };
  }
  }
}

static void codegen_sext_op(struct cg_state *state,
                            struct cg_basicblock *basicblock, struct ir_op *op,
                            struct rv32i_reg source, struct rv32i_reg dest) {
  invariant_assert(op->cast_op.value->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't sext from non-primitive");

  size_t sh_sz;
  switch (op->cast_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I8:
    sh_sz = 24;
    goto sext;
  case IR_VAR_PRIMITIVE_TY_I16:
    sh_sz = 16;
    goto sext;

  sext: {
    struct instr *sl = cg_alloc_instr(state->func, basicblock);
    sl->rv32i->ty = RV32I_INSTR_TY_SLLI;
    sl->rv32i->slli =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = sh_sz};
    struct instr *sr = cg_alloc_instr(state->func, basicblock);
    sr->rv32i->ty = RV32I_INSTR_TY_SRAI;
    sr->rv32i->srai =
        (struct rv32i_op_imm){.source = dest, .dest = dest, .imm = sh_sz};
    break;
  }

  case IR_VAR_PRIMITIVE_TY_I32:
    TODO("rv32i i64");
  case IR_VAR_PRIMITIVE_TY_I64:
    BUG("can't sext from I64");
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    BUG("can't sext float");
  }
}

static void codegen_zext_op(struct cg_state *state,
                            struct cg_basicblock *basicblock, struct ir_op *op,
                            struct rv32i_reg source, struct rv32i_reg dest) {

  invariant_assert(op->cast_op.value->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't sext from non-primitive");

  switch (op->cast_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I8: {
    struct instr *and = cg_alloc_instr(state->func, basicblock);
    and->rv32i->ty = RV32I_INSTR_TY_ANDI;
    and->rv32i->andi =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 0xFF};
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I16: {
    struct instr *sl = cg_alloc_instr(state->func, basicblock);
    sl->rv32i->ty = RV32I_INSTR_TY_SLLI;
    sl->rv32i->slli =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 16};
    struct instr *sr = cg_alloc_instr(state->func, basicblock);
    sr->rv32i->ty = RV32I_INSTR_TY_SRLI;
    sr->rv32i->srli =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 16};
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I32:
    TODO("rv32i i64");
  case IR_VAR_PRIMITIVE_TY_I64:
    BUG("can't zext from I64");
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    BUG("can't zext float");
  }
}

static void codegen_trunc_op(struct cg_state *state,
                             struct cg_basicblock *basicblock, struct ir_op *op,
                             struct rv32i_reg source, struct rv32i_reg dest) {
  switch (op->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I8: {
    struct instr *and = cg_alloc_instr(state->func, basicblock);
    and->rv32i->ty = RV32I_INSTR_TY_ANDI;
    and->rv32i->andi =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 0xFF};
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I16: {
    struct instr *sl = cg_alloc_instr(state->func, basicblock);
    sl->rv32i->ty = RV32I_INSTR_TY_SLLI;
    sl->rv32i->slli =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 16};
    struct instr *sr = cg_alloc_instr(state->func, basicblock);
    sr->rv32i->ty = RV32I_INSTR_TY_SRLI;
    sr->rv32i->srli =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 16};
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I64:
    break;
  default:
    BUG("can't zext");
  }
}

static void codegen_conv_op(struct cg_state *state,
                            struct cg_basicblock *basicblock, struct ir_op *op,
                            struct rv32i_reg source, struct rv32i_reg dest) {
  struct instr *instr = cg_alloc_instr(state->func, basicblock);
  switch (op->unary_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_F32:
    instr->rv32i->ty = RV32I_INSTR_TY_FCVT_S;
    break;
  case IR_VAR_PRIMITIVE_TY_F64:
    instr->rv32i->ty = RV32I_INSTR_TY_FCVT_D;
    break;
  default:
    BUG("unsupported type");
  }
  instr->rv32i->fmv = (struct rv32i_op_mov){.dest = dest, .source = source};
}

static void codegen_uconv_op(struct cg_state *state,
                             struct cg_basicblock *basicblock, struct ir_op *op,
                             struct rv32i_reg source, struct rv32i_reg dest) {
  struct instr *instr = cg_alloc_instr(state->func, basicblock);

  enum ir_var_primitive_ty ty;
  if (ir_var_ty_is_fp(&op->var_ty)) {
    ty = op->var_ty.primitive;
  } else {
    ty = op->unary_op.value->var_ty.primitive;
  }

  switch (ty) {
  case IR_VAR_PRIMITIVE_TY_F32:
    instr->rv32i->ty = RV32I_INSTR_TY_FCVTU_S;
    break;
  case IR_VAR_PRIMITIVE_TY_F64:
    instr->rv32i->ty = RV32I_INSTR_TY_FCVTU_D;
    break;
  default:
    BUG("unsupported type");
  }
  instr->rv32i->fmv = (struct rv32i_op_mov){.dest = dest, .source = source};
}

static void codegen_sconv_op(struct cg_state *state,
                             struct cg_basicblock *basicblock, struct ir_op *op,
                             struct rv32i_reg source, struct rv32i_reg dest) {
  struct instr *instr = cg_alloc_instr(state->func, basicblock);

  enum ir_var_primitive_ty ty;
  if (ir_var_ty_is_fp(&op->var_ty)) {
    ty = op->var_ty.primitive;
  } else {
    ty = op->unary_op.value->var_ty.primitive;
  }

  switch (ty) {
  case IR_VAR_PRIMITIVE_TY_F32:
    instr->rv32i->ty = RV32I_INSTR_TY_FCVT_S;
    break;
  case IR_VAR_PRIMITIVE_TY_F64:
    instr->rv32i->ty = RV32I_INSTR_TY_FCVT_D;
    break;
  default:
    BUG("unsupported type");
  }
  instr->rv32i->fmv = (struct rv32i_op_mov){.dest = dest, .source = source};
}

static void codegen_cast_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);
  struct rv32i_reg source = codegen_reg(op->cast_op.value);

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
    codegen_zext_op(state, basicblock, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_TRUNC:
    source.ty = dest.ty;
    codegen_trunc_op(state, basicblock, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_CONV:
    codegen_conv_op(state, basicblock, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_UCONV:
    codegen_uconv_op(state, basicblock, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_SCONV:
    codegen_sconv_op(state, basicblock, op, source, dest);
    break;
  }
}

static void codegen_call_op(struct cg_state *state,
                            struct cg_basicblock *basicblock,
                            struct ir_op *op) {
  invariant_assert(op->call.func_ty.ty == IR_VAR_TY_TY_FUNC, "non-func");

  const struct ir_var_func_ty *func_ty = &op->call.func_ty.func;

  invariant_assert(func_ty->num_params <= 8,
                   "`%s` doesn't support more than 8 args yet", __func__);

  if (op->call.target->flags & IR_OP_FLAG_CONTAINED) {
    DEBUG_ASSERT(op->call.target->ty == IR_OP_TY_ADDR &&
                     op->call.target->addr.ty == IR_OP_ADDR_TY_GLB,
                 "expected addr GLB");
    struct ir_glb *glb = op->call.target->addr.glb;

    struct cg_entry *entry = &state->func->unit->entries[glb->id];

    if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
      struct instr *instr = cg_alloc_instr(state->func, basicblock);
      instr->rv32i->ty = RV32I_INSTR_TY_JAL;
      instr->rv32i->jal = (struct rv32i_jal){
          .target = RV32I_SYMBOL_TARGET(entry), .ret_addr = RET_PTR_REG};

      instr->reloc =
          arena_alloc(state->func->unit->arena, sizeof(*instr->reloc));
      *instr->reloc = (struct relocation){
          .ty = RELOCATION_TY_CALL,
          .symbol_index = op->call.target->addr.glb->id,
          .size = 2,
          .address = 0,
      };
    } else {
      // generate `auipc` + `jalr` with fake reg (`ra`) that the linker will
      // reloc into a `jal`
      struct instr *auipc = cg_alloc_instr(state->func, basicblock);
      auipc->rv32i->ty = RV32I_INSTR_TY_AUIPC;
      auipc->rv32i->auipc = (struct rv32i_u){.dest = RET_PTR_REG, .imm = 0};

      auipc->reloc =
          arena_alloc(state->func->unit->arena, sizeof(*auipc->reloc));
      *auipc->reloc = (struct relocation){
          .ty = RELOCATION_TY_CALL,
          .symbol_index = op->call.target->addr.glb->id,
          .size = 2,
          .address = 0,
      };

      struct instr *jalr = cg_alloc_instr(state->func, basicblock);
      jalr->rv32i->ty = RV32I_INSTR_TY_JALR;
      jalr->rv32i->jalr = (struct rv32i_jalr){
          .target = RET_PTR_REG, .ret_addr = RET_PTR_REG, .imm = 0};
    }
  } else {
    // NOTE: `blr` seems to segfault on linux rv32i
    struct instr *instr = cg_alloc_instr(state->func, basicblock);
    instr->rv32i->ty = RV32I_INSTR_TY_JALR;
    instr->rv32i->jalr = (struct rv32i_jalr){
        .target = codegen_reg(op->call.target), .ret_addr = RET_PTR_REG};
  }
}

static void codegen_op(struct cg_state *state, struct cg_basicblock *basicblock,
                       struct ir_op *op) {
  trace("lowering op with id %zu, type %d", op->id, op->ty);
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
  case IR_OP_TY_ADDR_OFFSET: {
    codegen_addr_offset_op(state, basicblock, op);
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
  default: {
    TODO("unsupported IR OP '%d'", op->ty);
  }
  }
}

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

void rv32i_codegen_start(struct cg_state *state) { codegen_prologue(state); }

void rv32i_codegen_basicblock(struct cg_state *state,
                              struct ir_basicblock *basicblock) {
  struct ir_stmt *stmt = basicblock->first;

  struct cg_basicblock *cg_basicblock =
      cg_alloc_basicblock(state->func, basicblock);

  while (stmt) {
    codegen_stmt(state, cg_basicblock, stmt);

    stmt = stmt->succ;
  }
}

static bool rv32i_instr_is_branch(const struct rv32i_instr *instr) {
  switch (instr->ty) {
  case RV32I_INSTR_TY_BEQ:
  case RV32I_INSTR_TY_BNE:
  case RV32I_INSTR_TY_BLT:
  case RV32I_INSTR_TY_BGE:
  case RV32I_INSTR_TY_BLTU:
  case RV32I_INSTR_TY_BGEU:
  case RV32I_INSTR_TY_JALR:
    return true;
  case RV32I_INSTR_TY_JAL:
    // note: JAL is not always a branch (also can be call)
    return instr->jal.ret_addr.ty == RV32I_REG_TY_W &&
           instr->jal.ret_addr.idx == 0;
  default:
    return false;
  }
}

static unsigned long long pessimistic_bb_distance(struct instr *from,
                                                  struct cg_basicblock *to) {
  // we never insert more 4 instructions for a jump
  // the 4 instr case is:
  //     b<cond> true, false
  //
  // into:
  //     b<cond> true_jmp, false_jmp
  //   true_jmp:
  //     auipc
  //     jalr
  //   false_jmp:
  //     auipc
  //     jalr
  //
  // as such, we know the distance between two instructions is bounded by 4 *
  // num_bbs_between we should try and calculate this more accurately to not
  // falsely generate large jumps, but this can also just be fixed by using
  // linker relaxation

  return llabs((long long)cg_get_next_instr(to)->id - (long long)from->id) * 4 *
         /* instr size */ 4;
}

#define MAX_B_TYPE_BR (4094)
#define MIN_B_TYPE_BR (-4096)

#define MAX_J_TYPE_BR (524286)
#define MIN_J_TYPE_BR (-524288)

void rv32i_codegen_end(struct cg_state *state) {
  struct cg_basicblock *basicblock = state->func->first;

  while (basicblock) {
    struct instr *last = basicblock->last;

    while (last) {
      if (!rv32i_instr_is_branch(last->rv32i)) {
        goto done;
        // can be eliminated because consective BBs
        // BUG("expected cg_basicblock to end in branch");
      }

      switch (last->rv32i->ty) {
      case RV32I_INSTR_TY_JALR:
        // return, no offset concern
        goto done;
      case RV32I_INSTR_TY_JAL: {
        // may be preceded by a branch conditional instr, so keep going

        struct rv32i_target target = last->rv32i->jal.target;

        if (target.ty != RV32I_TARGET_TY_BASICBLOCK) {
          break;
        }

        long long offset =
            pessimistic_bb_distance(last, target.basicblock->cg_basicblock);

        if (offset >= MIN_J_TYPE_BR && offset <= MAX_J_TYPE_BR) {
          // valid offset, nothing needed
          break;
        }

        BUG("jal > 2.5mb");
        break;
      }
      default: {
        // conditional branch
        struct rv32i_target target = last->rv32i->conditional_branch.target;

        if (target.ty != RV32I_TARGET_TY_BASICBLOCK) {
          goto done;
        }

        long long offset =
            pessimistic_bb_distance(last, target.basicblock->cg_basicblock);

        if (offset >= MIN_B_TYPE_BR && offset <= MAX_B_TYPE_BR) {
          // valid offset, nothing needed
          goto done;
        }

        if (offset < MIN_J_TYPE_BR || offset > MAX_J_TYPE_BR) {
          BUG("cond br > 2.5mb");
        }

        // generate jalr instead
        struct instr *jal;
        if (last->succ) {
          DEBUG_ASSERT(last->succ->rv32i->ty == RV32I_INSTR_TY_JAL,
                       "expected jal after cond branch");
          DEBUG_ASSERT(last->succ->rv32i->jal.target.ty |=
                       RV32I_TARGET_TY_OFFSET == RV32I_INSTR_TY_JAL,
                       "branch rewriting will break offset");
          jal = last->succ;

          struct instr *new_jal = cg_alloc_instr(state->func, basicblock);
          *new_jal->rv32i = *jal->rv32i;
        } else {
          jal = cg_alloc_instr(state->func, basicblock);
        }

        // jump over jalr if true
        last->rv32i->ty = invert_cond(last->rv32i->ty);
        last->rv32i->conditional_branch.target = RV32I_OFFSET_TARGET(8);

        jal->rv32i->ty = RV32I_INSTR_TY_JAL;
        jal->rv32i->jal =
            (struct rv32i_jal){.ret_addr = GP_ZERO_REG, .target = target};

        goto done;
      }
      }

      last = last->pred;
    }

  done:
    DEBUG_ASSERT(!last || !last->pred ||
                     !rv32i_instr_is_branch(last->pred->rv32i),
                 "more branches than expected in bb");

    basicblock = basicblock->succ;
  }

  // now rebuild ids so offsets are correct for emitting
  cg_rebuild_ids(state->func);
}

static const char *GP_REG_ABI_NAMES[] = {
    "zero",

    "ra",   "sp", "gp", "tp",

    "t0",   "t1", "t2",

    "fp", // also `s0`
    "s1",

    "a0",   "a1", "a2", "a3", "a4", "a5", "a6", "a7",

    "s2",   "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",

    "t3",   "t4", "t5", "t6",
};

static const char *FP_REG_ABI_NAMES[] = {
    "ft0", "ft1", "ft2",  "ft3",  "ft4", "ft5", "ft6", "ft7",

    "fs0", "fs1",

    "fa0", "fa1", "fa2",  "fa3",  "fa4", "fa5", "fa6", "fa7",

    "fs2", "fs3", "fs4",  "fs5",  "fs6", "fs7", "fs8", "fs9", "fs10", "fs11",

    "ft8", "ft9", "ft10", "ft11",
};

struct codegen_debug_state {
  FILE *file;
  struct cg_func *func;
  struct instr *instr;
};

static void print_bb_symbol(FILE *file, struct ir_basicblock *basicblock) {
  fprintf(file, ".L_%s_BB_%zu", basicblock->func->name, basicblock->id);
}

static void codegen_fprintf(const struct codegen_debug_state *state,
                            const char *format, ...) {
  FILE *file = state->file;

  va_list list;
  va_start(list, format);
  while (format[0] != '\0') {
    if (format[0] != '%') {
      fputc(format[0], file);
      format++;
      continue;
    }

    format++;

    if (strncmp(format, "reg", 3) == 0) {
      struct rv32i_reg reg = va_arg(list, struct rv32i_reg);

      switch (reg.ty) {
      case RV32I_REG_TY_NONE:
        BUG("none reg makes no sense");
        break;
      case RV32I_REG_TY_W:
        fprintf(file, "%s", GP_REG_ABI_NAMES[reg.idx]);
        break;
      case RV32I_REG_TY_F:
        fprintf(file, "%s", FP_REG_ABI_NAMES[reg.idx]);
      }

      format += 3;
    } else if (strncmp(format, "target", 6) == 0) {
      struct rv32i_target target = va_arg(list, struct rv32i_target);

      switch (target.ty) {
      case RV32I_TARGET_TY_OFFSET:
        fprintf(file, ".%s%lld", target.offset > 0 ? "+" : "", target.offset);
        break;
      case RV32I_TARGET_TY_BASICBLOCK: {
        print_bb_symbol(file, target.basicblock);
        break;
      }
      case RV32I_TARGET_TY_SYMBOL:
        fprintf(file, "<%s>", target.symbol->name);
        break;
      }

      format += 6;
    } else if (strncmp(format, "immu", 4) == 0) {
      simm_t imm = va_arg(list, simm_t);
      fprintf(file, "%llu",
              imm >= 0 ? imm
                       : (unsigned long long)(1048574ll + (long long)imm));

      format += 4;
    } else if (strncmp(format, "imm", 3) == 0) {
      simm_t imm = va_arg(list, simm_t);
      fprintf(file, "%lld", imm);

      format += 3;
    } else if (format[0] == 's') {
      const char *str = va_arg(list, const char *);
      fprintf(file, "%s", str);
      format++;
    } else if (format[0] == '%') {
      fputc('%', file);
      format++;
    } else {
      BUG("unrecognised format starting '%%%s'", format);
    }
  }
}

static void debug_print_op_imm(const struct codegen_debug_state *state,
                               const struct rv32i_op_imm *op_imm) {
  codegen_fprintf(state, " %reg, %reg, %imm", op_imm->dest, op_imm->source,
                  op_imm->imm);
}

static void debug_print_op_fp(const struct codegen_debug_state *state,
                              const struct rv32i_op_fp *op_fp) {
  codegen_fprintf(state, " %reg, %reg, %reg", op_fp->dest, op_fp->lhs,
                  op_fp->rhs);
}

static void
debug_print_op_unary_fp(const struct codegen_debug_state *state,
                        const struct rv32i_op_unary_fp *op_unary_fp) {
  codegen_fprintf(state, " %reg, %reg", op_unary_fp->dest, op_unary_fp->source);
}

static void debug_print_op(const struct codegen_debug_state *state,
                           const struct rv32i_op *op) {
  codegen_fprintf(state, " %reg, %reg, %reg", op->dest, op->lhs, op->rhs);
}

static void debug_print_op_mov(const struct codegen_debug_state *state,
                               const struct rv32i_op_mov *op_mov) {
  codegen_fprintf(state, " %reg, %reg", op_mov->dest, op_mov->source);
}

static void debug_print_lui(const struct codegen_debug_state *state,
                            const struct rv32i_u *lui) {
  codegen_fprintf(state, " %reg, %immu", lui->dest, lui->imm);
}

static void debug_print_jalr(const struct codegen_debug_state *state,
                             const struct rv32i_jalr *jalr) {
  codegen_fprintf(state, " %reg, %reg, %imm", jalr->ret_addr, jalr->target,
                  jalr->imm);
}

static void debug_print_jal(const struct codegen_debug_state *state,
                            const struct rv32i_jal *jal) {
  codegen_fprintf(state, " %reg, %target", jal->ret_addr, jal->target);
}

static void debug_print_load(const struct codegen_debug_state *state,
                             const struct rv32i_load *load) {
  codegen_fprintf(state, " %reg, %imm(%reg)", load->dest, load->imm,
                  load->addr);
}

static void debug_print_store(const struct codegen_debug_state *state,
                              const struct rv32i_store *store) {
  codegen_fprintf(state, " %reg, %imm(%reg)", store->source, store->imm,
                  store->addr);
}

static void debug_print_conditional_branch(
    const struct codegen_debug_state *state,
    const struct rv32i_conditional_branch *conditional_branch) {
  codegen_fprintf(state, " %reg, %reg, %target", conditional_branch->lhs,
                  conditional_branch->rhs, conditional_branch->target);
}

static struct relocation *get_instr_reloc(const struct instr *instr) {
  if (instr->reloc) {
    return instr->reloc;
  }

  if (!instr->pred || !instr->pred->reloc) {
    return NULL;
  }

  struct relocation *reloc = instr->pred->reloc;

  switch (reloc->ty) {
  case RELOCATION_TY_POINTER:
  case RELOCATION_TY_LOCAL_SINGLE:
  case RELOCATION_TY_UNDEF_SINGLE:
    return NULL;
  case RELOCATION_TY_CALL:
    return instr->rv32i->ty == RV32I_INSTR_TY_JALR &&
                   instr->pred->rv32i->ty == RV32I_INSTR_TY_AUIPC
               ? reloc
               : NULL;
  case RELOCATION_TY_LOCAL_PAIR:
  case RELOCATION_TY_UNDEF_PAIR:
    return reloc;
  }
}

static const char *get_reloc_symbol(const struct codegen_debug_state *state,
                                    const struct relocation *reloc) {
  return state->func->unit->entries[reloc->symbol_index].name;
}

static void print_instr(const struct codegen_debug_state *state,
                                    enum codegen_flags flags,
                        const struct instr *instr) {

  FILE *file = state->file;

  switch (instr->rv32i->ty) {
  case RV32I_INSTR_TY_LUI: {
    struct relocation *reloc = instr->reloc;

    struct rv32i_u *lui = &instr->rv32i->lui;
    if (reloc) {
      codegen_fprintf(state, "lui %reg, %%hi(%s+%imm)", lui->dest,
                      get_reloc_symbol(state, reloc), lui->imm);

    } else {
      fprintf(file, "lui");
      debug_print_lui(state, lui);
    }
    break;
  }
  case RV32I_INSTR_TY_AUIPC: {
    struct relocation *reloc = instr->reloc;

    struct rv32i_u *auipc = &instr->rv32i->auipc;
    if (reloc) {
      codegen_fprintf(state, "%imm: auipc %reg, %%pcrel_hi(%s+%imm)", instr->id,
                      auipc->dest, get_reloc_symbol(state, reloc), auipc->imm);
    } else {
      fprintf(file, "auipc");
      debug_print_lui(state, auipc);
    }
    break;
  }
  case RV32I_INSTR_TY_ADDI: {
    struct relocation *reloc = get_instr_reloc(instr);

    struct rv32i_op_imm *addi = &instr->rv32i->addi;
    if (reloc) {
      codegen_fprintf(state, "addi %reg, %reg, %%lo(%s+%imm)", addi->dest,
                      addi->source, get_reloc_symbol(state, reloc), addi->imm);

    } else if ((flags & CODEGEN_FLAG_MNEMONICS) && addi->source.idx == 0 &&
               addi->dest.idx == 0 && addi->imm == 0) {
      codegen_fprintf(state, "nop");
    } else if ((flags & CODEGEN_FLAG_MNEMONICS) && addi->source.idx == 0) {
      codegen_fprintf(state, "li %reg, %imm", addi->dest, addi->imm);
    } else if ((flags & CODEGEN_FLAG_MNEMONICS) && addi->imm == 0) {
      codegen_fprintf(state, "mv %reg, %reg", addi->dest, addi->source);
    } else {
      fprintf(file, "addi");
      debug_print_op_imm(state, addi);
    }

    break;
  }
  case RV32I_INSTR_TY_JAL: {
    struct relocation *reloc = get_instr_reloc(instr);

    struct rv32i_jal *jal = &instr->rv32i->jal;

    if (reloc) {
      codegen_fprintf(state, "jal %reg, %s", jal->ret_addr,
                      get_reloc_symbol(state, reloc));
    } else if ((flags & CODEGEN_FLAG_MNEMONICS) && jal->ret_addr.idx == 0) {
      codegen_fprintf(state, "j %target", jal->target);
    } else if ((flags & CODEGEN_FLAG_MNEMONICS) && jal->ret_addr.idx == 1) {
      codegen_fprintf(state, "jal %target", jal->target);
    } else {
      fprintf(file, "jal");
      debug_print_jal(state, jal);
    }
    break;
  }
  case RV32I_INSTR_TY_JALR: {
    struct relocation *reloc = get_instr_reloc(instr);

    struct rv32i_jalr *jalr = &instr->rv32i->jalr;

    if (reloc) {
      codegen_fprintf(state, "jalr %reg, %reg, %%pcrel_lo(%immb)",
                      jalr->ret_addr, jalr->target, instr->pred->id);
    } else if ((flags & CODEGEN_FLAG_MNEMONICS) && jalr->ret_addr.idx == 0 && jalr->target.idx == 1 && jalr->imm == 0) {
      codegen_fprintf(state, "ret");
    } else if ((flags & CODEGEN_FLAG_MNEMONICS) && jalr->ret_addr.idx == 0 && jalr->imm == 0) {
      codegen_fprintf(state, "jr %reg", jalr->target);
    } else if ((flags & CODEGEN_FLAG_MNEMONICS) && jalr->ret_addr.idx == 1 && jalr->imm == 0) {
      codegen_fprintf(state, "jalr %reg", jalr->target);
    } else {
      fprintf(file, "jalr");
      debug_print_jalr(state, jalr);
    }
    break;
  }
  case RV32I_INSTR_TY_XORI: {
    struct rv32i_op_imm *xori = &instr->rv32i->xori;
    if ((flags & CODEGEN_FLAG_MNEMONICS) && xori->imm == -1) {
      codegen_fprintf(state, "not %reg, %reg", xori->dest, xori->source);
    } else {
      fprintf(file, "xori");
      debug_print_op_imm(state, xori);
    }
    break;
  }
  case RV32I_INSTR_TY_ADD: {
    struct rv32i_op *add = &instr->rv32i->add;
    fprintf(file, "add");
    debug_print_op(state, add);
    break;
  }
  case RV32I_INSTR_TY_SUB: {
    struct rv32i_op *sub = &instr->rv32i->sub;
    if ((flags & CODEGEN_FLAG_MNEMONICS) && sub->lhs.idx == 0) {
      codegen_fprintf(state, "neg %reg, %reg", sub->dest, sub->rhs);
    } else {
    fprintf(file, "sub");
    debug_print_op(state, sub);
    }
    break;
  }
  case RV32I_INSTR_TY_MUL: {
    struct rv32i_op *mul = &instr->rv32i->mul;
    fprintf(file, "mul");
    debug_print_op(state, mul);
    break;
  }
  case RV32I_INSTR_TY_DIV: {
    struct rv32i_op *div = &instr->rv32i->div;
    fprintf(file, "div");
    debug_print_op(state, div);
    break;
  }
  case RV32I_INSTR_TY_DIVU: {
    struct rv32i_op *divu = &instr->rv32i->divu;
    fprintf(file, "divu");
    debug_print_op(state, divu);
    break;
  }
  case RV32I_INSTR_TY_REM: {
    struct rv32i_op *rem = &instr->rv32i->rem;
    fprintf(file, "rem");
    debug_print_op(state, rem);
    break;
  }
  case RV32I_INSTR_TY_REMU: {
    struct rv32i_op *remu = &instr->rv32i->remu;
    fprintf(file, "remu");
    debug_print_op(state, remu);
    break;
  }
  case RV32I_INSTR_TY_SB: {
    struct rv32i_store *sb = &instr->rv32i->sb;
    fprintf(file, "sb");
    debug_print_store(state, sb);
    break;
  }
  case RV32I_INSTR_TY_SH: {
    struct rv32i_store *sh = &instr->rv32i->sh;
    fprintf(file, "sh");
    debug_print_store(state, sh);
    break;
  }
  case RV32I_INSTR_TY_SW: {
    struct rv32i_store *sw = &instr->rv32i->sw;
    fprintf(file, "sw");
    debug_print_store(state, sw);
    break;
  }
  case RV32I_INSTR_TY_LB: {
    struct rv32i_load *lb = &instr->rv32i->lb;
    fprintf(file, "lb");
    debug_print_load(state, lb);
    break;
  }
  case RV32I_INSTR_TY_LBU: {
    struct rv32i_load *lbu = &instr->rv32i->lbu;
    fprintf(file, "lbu");
    debug_print_load(state, lbu);
    break;
  }
  case RV32I_INSTR_TY_LH: {
    struct rv32i_load *lh = &instr->rv32i->lh;
    fprintf(file, "lh");
    debug_print_load(state, lh);
    break;
  }
  case RV32I_INSTR_TY_LHU: {
    struct rv32i_load *lhu = &instr->rv32i->lhu;
    fprintf(file, "lhu");
    debug_print_load(state, lhu);
    break;
  }
  case RV32I_INSTR_TY_LW: {
    struct rv32i_load *lw = &instr->rv32i->lw;
    fprintf(file, "lw");
    debug_print_load(state, lw);
    break;
  }
  case RV32I_INSTR_TY_FSW: {
    struct rv32i_store *fsw = &instr->rv32i->fsw;
    fprintf(file, "fsw");
    debug_print_store(state, fsw);
    break;
  }
  case RV32I_INSTR_TY_FLW: {
    struct rv32i_load *flw = &instr->rv32i->flw;
    fprintf(file, "flw");
    debug_print_load(state, flw);
    break;
  }
  case RV32I_INSTR_TY_FSD: {
    struct rv32i_store *fsd = &instr->rv32i->fsd;
    fprintf(file, "fsd");
    debug_print_store(state, fsd);
    break;
  }
  case RV32I_INSTR_TY_FLD: {
    struct rv32i_load *fld = &instr->rv32i->fld;
    fprintf(file, "fld");
    debug_print_load(state, fld);
    break;
  }
  case RV32I_INSTR_TY_BEQ: {
    struct rv32i_conditional_branch *beq = &instr->rv32i->beq;
    if ((flags & CODEGEN_FLAG_MNEMONICS) && beq->rhs.idx == 0) {
    codegen_fprintf(state, "beqz %reg, %target", beq->lhs,
                    beq->target);
    } else {
      fprintf(file, "beq");
      debug_print_conditional_branch(state, beq);
    }
    break;
  }
  case RV32I_INSTR_TY_BNE: {
    struct rv32i_conditional_branch *bne = &instr->rv32i->bne;
    if ((flags & CODEGEN_FLAG_MNEMONICS) && bne->rhs.idx == 0) {
    codegen_fprintf(state, "bnez %reg, %target", bne->lhs,
                    bne->target);
    } else {
    fprintf(file, "bne");
    debug_print_conditional_branch(state, bne);
    }
    break;
  }
  case RV32I_INSTR_TY_BLT: {
    struct rv32i_conditional_branch *blt = &instr->rv32i->blt;
    if ((flags & CODEGEN_FLAG_MNEMONICS) && blt->rhs.idx == 0) {
    codegen_fprintf(state, "bltz %reg, %target", blt->lhs,
                    blt->target);
    } else {
    fprintf(file, "blt");
    debug_print_conditional_branch(state, blt);
    }
    break;
  }
  case RV32I_INSTR_TY_BGE: {
    struct rv32i_conditional_branch *bge = &instr->rv32i->bge;
    if ((flags & CODEGEN_FLAG_MNEMONICS) && bge->rhs.idx == 0) {
    codegen_fprintf(state, "bgez %reg, %target", bge->lhs,
                    bge->target);
    } else {
    fprintf(file, "bge");
    debug_print_conditional_branch(state, bge);
    }
    break;
  }
  case RV32I_INSTR_TY_BLTU: {
    struct rv32i_conditional_branch *bltu = &instr->rv32i->bltu;
    fprintf(file, "bltu");
    debug_print_conditional_branch(state, bltu);
    break;
  }
  case RV32I_INSTR_TY_BGEU: {
    struct rv32i_conditional_branch *bgeu = &instr->rv32i->bgeu;
    fprintf(file, "bgeu");
    debug_print_conditional_branch(state, bgeu);
    break;
  }
  case RV32I_INSTR_TY_OR: {
    struct rv32i_op * or = &instr->rv32i->or ;
    fprintf(file, "or");
    debug_print_op(state, or);
    break;
  }
  case RV32I_INSTR_TY_AND: {
    struct rv32i_op *and = &instr->rv32i->and;
    fprintf(file, "and");
    debug_print_op(state, and);
    break;
  }
  case RV32I_INSTR_TY_XOR: {
    struct rv32i_op * xor = &instr->rv32i->xor ;
    fprintf(file, "xor");
    debug_print_op(state, xor);
    break;
  }
  case RV32I_INSTR_TY_SLL: {
    struct rv32i_op *sll = &instr->rv32i->sll;
    fprintf(file, "sll");
    debug_print_op(state, sll);
    break;
  }
  case RV32I_INSTR_TY_SRL: {
    struct rv32i_op *srl = &instr->rv32i->srl;
    fprintf(file, "srl");
    debug_print_op(state, srl);
    break;
  }
  case RV32I_INSTR_TY_SRA: {
    struct rv32i_op *sra = &instr->rv32i->sra;
    fprintf(file, "sra");
    debug_print_op(state, sra);
    break;
  }
  case RV32I_INSTR_TY_FMV_S: {
    struct rv32i_op_mov *fmv = &instr->rv32i->fmv;

    // old names but seems the langproc assembler wants them
    switch (fmv->source.ty) {
    case RV32I_REG_TY_NONE:
      BUG("none dest");
    case RV32I_REG_TY_W:
      fprintf(file, "fmv.s.x");
      break;
    case RV32I_REG_TY_F:
      fprintf(file, "fmv.x.s");
      break;
    }

    debug_print_op_mov(state, fmv);
    break;
  }
  case RV32I_INSTR_TY_FMV_D: {
    struct rv32i_op_mov *fmv = &instr->rv32i->fmv;

    switch (fmv->source.ty) {
    case RV32I_REG_TY_NONE:
      BUG("none dest");
    case RV32I_REG_TY_W:
      fprintf(file, "fmv.w.d");
      break;
    case RV32I_REG_TY_F:
      fprintf(file, "fmv.d.w");
      break;
    }

    debug_print_op_mov(state, fmv);
    break;
  }
  case RV32I_INSTR_TY_FADD_S: {
    struct rv32i_op_fp *fadd = &instr->rv32i->fadd;
    fprintf(file, "fadd.s");
    debug_print_op_fp(state, fadd);
    break;
  }
  case RV32I_INSTR_TY_FADD_D: {
    struct rv32i_op_fp *fadd = &instr->rv32i->fadd;
    fprintf(file, "fadd.d");
    debug_print_op_fp(state, fadd);
    break;
  }
  case RV32I_INSTR_TY_FSUB_S: {
    struct rv32i_op_fp *fsub = &instr->rv32i->fsub;
    fprintf(file, "fsub.s");
    debug_print_op_fp(state, fsub);
    break;
  }
  case RV32I_INSTR_TY_FSUB_D: {
    struct rv32i_op_fp *fsub = &instr->rv32i->fsub;
    fprintf(file, "fsub.d");
    debug_print_op_fp(state, fsub);
    break;
  }
  case RV32I_INSTR_TY_FSGNJ_S: {
    if ((flags & CODEGEN_FLAG_MNEMONICS) &&
        instr->rv32i->fsgnj.lhs.idx == instr->rv32i->fsgnj.rhs.idx) {
      codegen_fprintf(state, "fmv.s %reg, %reg", instr->rv32i->fsgnj.dest,
                      instr->rv32i->fsgnj.lhs);
    } else {
      struct rv32i_op_fp *fsgnj = &instr->rv32i->fsgnj;
      fprintf(file, "fsgnj.s");
      debug_print_op_fp(state, fsgnj);
    }
    break;
  }
  case RV32I_INSTR_TY_FSGNJ_D: {
    if ((flags & CODEGEN_FLAG_MNEMONICS) &&
        instr->rv32i->fsgnj.lhs.idx == instr->rv32i->fsgnj.rhs.idx) {
      codegen_fprintf(state, "fmv.d %reg, %reg", instr->rv32i->fsgnj.dest,
                      instr->rv32i->fsgnj.lhs);
    } else {
      struct rv32i_op_fp *fsgnj = &instr->rv32i->fsgnj;
      fprintf(file, "fsgnj.d");
      debug_print_op_fp(state, fsgnj);
    }
    break;
  }
  case RV32I_INSTR_TY_FMUL_S: {
    struct rv32i_op_fp *fmul = &instr->rv32i->fmul;
    fprintf(file, "fmul.s");
    debug_print_op_fp(state, fmul);
    break;
  }
  case RV32I_INSTR_TY_FMUL_D: {
    struct rv32i_op_fp *fmul = &instr->rv32i->fmul;
    fprintf(file, "fmul.d");
    debug_print_op_fp(state, fmul);
    break;
  }

  case RV32I_INSTR_TY_FDIV_S: {
    struct rv32i_op_fp *fdiv = &instr->rv32i->fdiv;
    fprintf(file, "fdiv.s");
    debug_print_op_fp(state, fdiv);
    break;
  }
  case RV32I_INSTR_TY_FDIV_D: {
    struct rv32i_op_fp *fdiv = &instr->rv32i->fdiv;
    fprintf(file, "fdiv.d");
    debug_print_op_fp(state, fdiv);
    break;
  }

  case RV32I_INSTR_TY_FSGNJN_S: {
    struct rv32i_op_fp *fsgnjn = &instr->rv32i->fsgnjn;
    fprintf(file, "fsgnjn.s");
    debug_print_op_fp(state, fsgnjn);
    break;
  }
  case RV32I_INSTR_TY_FSGNJN_D: {
    struct rv32i_op_fp *fsgnjn = &instr->rv32i->fsgnjn;
    fprintf(file, "fsgnjn.d");
    debug_print_op_fp(state, fsgnjn);
    break;
  }

  case RV32I_INSTR_TY_FSGNJX_S: {
    struct rv32i_op_fp *fsgnjx = &instr->rv32i->fsgnjx;
    fprintf(file, "fsgnjx.s");
    debug_print_op_fp(state, fsgnjx);
    break;
  }
  case RV32I_INSTR_TY_FSGNJX_D: {
    struct rv32i_op_fp *fsgnjx = &instr->rv32i->fsgnjx;
    fprintf(file, "fsgnjx.d");
    debug_print_op_fp(state, fsgnjx);
    break;
  }

  case RV32I_INSTR_TY_FMAX_S: {
    struct rv32i_op_fp *fmax = &instr->rv32i->fmax;
    fprintf(file, "fmax.s");
    debug_print_op_fp(state, fmax);
    break;
  }
  case RV32I_INSTR_TY_FMAX_D: {
    struct rv32i_op_fp *fmax = &instr->rv32i->fmax;
    fprintf(file, "fmax.d");
    debug_print_op_fp(state, fmax);
    break;
  }

  case RV32I_INSTR_TY_FMIN_S: {
    struct rv32i_op_fp *fmin = &instr->rv32i->fmin;
    fprintf(file, "fmin.s");
    debug_print_op_fp(state, fmin);
    break;
  }
  case RV32I_INSTR_TY_FMIN_D: {
    struct rv32i_op_fp *fmin = &instr->rv32i->fmin;
    fprintf(file, "fmin.d");
    debug_print_op_fp(state, fmin);
    break;
  }

  case RV32I_INSTR_TY_FSQRT_S: {
    struct rv32i_op_unary_fp *fsqrt = &instr->rv32i->fsqrt;
    fprintf(file, "fsqrt.s");
    debug_print_op_unary_fp(state, fsqrt);
    break;
  }
  case RV32I_INSTR_TY_FSQRT_D: {
    struct rv32i_op_unary_fp *fsqrt = &instr->rv32i->fsqrt;
    fprintf(file, "fsqrt.d");
    debug_print_op_unary_fp(state, fsqrt);
    break;
  }

  case RV32I_INSTR_TY_ORI: {
    struct rv32i_op_imm *ori = &instr->rv32i->ori;
    fprintf(file, "ori");
    debug_print_op_imm(state, ori);
    break;
  }
  case RV32I_INSTR_TY_ANDI: {
    struct rv32i_op_imm *andi = &instr->rv32i->andi;
    fprintf(file, "andi");
    debug_print_op_imm(state, andi);
    break;
  }
  case RV32I_INSTR_TY_SLLI: {
    struct rv32i_op_imm *slli = &instr->rv32i->slli;
    fprintf(file, "slli");
    debug_print_op_imm(state, slli);
    break;
  }
  case RV32I_INSTR_TY_SRLI: {
    struct rv32i_op_imm *srli = &instr->rv32i->srli;
    fprintf(file, "srli");
    debug_print_op_imm(state, srli);
    break;
  }
  case RV32I_INSTR_TY_SRAI: {
    struct rv32i_op_imm *srai = &instr->rv32i->srai;
    fprintf(file, "srai");
    debug_print_op_imm(state, srai);
    break;
  }
  case RV32I_INSTR_TY_SLT: {
    struct rv32i_op *slt = &instr->rv32i->slt;
    fprintf(file, "slt");
    debug_print_op(state, slt);
    break;
  }
  case RV32I_INSTR_TY_SLTU: {
    struct rv32i_op *sltu = &instr->rv32i->sltu;
    fprintf(file, "sltu");
    debug_print_op(state, sltu);
    break;
  }

  case RV32I_INSTR_TY_SLTI: {
    struct rv32i_op_imm *slti = &instr->rv32i->slti;
    fprintf(file, "slti");
    debug_print_op_imm(state, slti);
    break;
  }
  case RV32I_INSTR_TY_SLTIU: {
    struct rv32i_op_imm *sltiu = &instr->rv32i->sltiu;
    fprintf(file, "sltiu");
    debug_print_op_imm(state, sltiu);
    break;
  }
  case RV32I_INSTR_TY_MULH: {
    struct rv32i_op *mulh = &instr->rv32i->mulh;
    fprintf(file, "mulh");
    debug_print_op(state, mulh);
    break;
  }
  case RV32I_INSTR_TY_MULHU: {
    struct rv32i_op *mulhu = &instr->rv32i->mulhu;
    fprintf(file, "mulhu");
    debug_print_op(state, mulhu);
    break;
  }
  case RV32I_INSTR_TY_MULHSU: {
    struct rv32i_op *mulhsu = &instr->rv32i->mulhsu;
    fprintf(file, "mulhsu");
    debug_print_op(state, mulhsu);
    break;
  }
  case RV32I_INSTR_TY_FCVT_S: {
    struct rv32i_op_mov *fcvt = &instr->rv32i->fcvt;
    switch (fcvt->source.ty) {
    case RV32I_REG_TY_NONE:
      BUG("makes no sense");
    case RV32I_REG_TY_W:
      fprintf(file, "fcvt.s.w");
      break;
    case RV32I_REG_TY_F:
      switch (fcvt->dest.ty) {
      case RV32I_REG_TY_NONE:
        BUG("makes no sense");
      case RV32I_REG_TY_W:
        fprintf(file, "fcvt.w.s");
        break;
      case RV32I_REG_TY_F:
        fprintf(file, "fcvt.d.s");
        break;
      }
    }

    debug_print_op_mov(state, fcvt);
    break;
  }
  case RV32I_INSTR_TY_FCVT_D: {
    struct rv32i_op_mov *fcvt = &instr->rv32i->fcvt;
    switch (fcvt->source.ty) {
    case RV32I_REG_TY_NONE:
      BUG("makes no sense");
    case RV32I_REG_TY_W:
      fprintf(file, "fcvt.d.w");
      break;
    case RV32I_REG_TY_F:
      switch (fcvt->dest.ty) {
      case RV32I_REG_TY_NONE:
        BUG("makes no sense");
      case RV32I_REG_TY_W:
        fprintf(file, "fcvt.w.d");
        break;
      case RV32I_REG_TY_F:
        fprintf(file, "fcvt.s.d");
        break;
      }
    }

    debug_print_op_mov(state, fcvt);
    break;
  }
  case RV32I_INSTR_TY_FCVTU_S: {
    struct rv32i_op_mov *fcvtu = &instr->rv32i->fcvtu;
    switch (fcvtu->source.ty) {
    case RV32I_REG_TY_NONE:
      BUG("makes no sense");
    case RV32I_REG_TY_W:
      DEBUG_ASSERT(fcvtu->dest.ty == RV32I_REG_TY_F,
                   "fcvtu can only be used for f<->w/x moves");

      fprintf(file, "fcvt.s.wu");
      break;
    case RV32I_REG_TY_F:
      DEBUG_ASSERT(fcvtu->dest.ty == RV32I_REG_TY_W,
                   "fcvtu can only be used for f<->w/x moves");
      fprintf(file, "fcvt.wu.s");
      break;
    }
    debug_print_op_mov(state, fcvtu);
    break;
  }
  case RV32I_INSTR_TY_FCVTU_D: {
    struct rv32i_op_mov *fcvtu = &instr->rv32i->fcvtu;
    switch (fcvtu->source.ty) {
    case RV32I_REG_TY_NONE:
      BUG("makes no sense");
    case RV32I_REG_TY_W:
      DEBUG_ASSERT(fcvtu->dest.ty == RV32I_REG_TY_F,
                   "fcvtu can only be used for f<->w/x moves");

      fprintf(file, "fcvt.d.wu");
      break;
    case RV32I_REG_TY_F:
      DEBUG_ASSERT(fcvtu->dest.ty == RV32I_REG_TY_W,
                   "fcvtu can only be used for f<->w/x moves");
      fprintf(file, "fcvt.wu.d");
      break;
    }
    debug_print_op_mov(state, fcvtu);
    break;
  }
  case RV32I_INSTR_TY_FEQ_S: {
    struct rv32i_op_fp *feq = &instr->rv32i->feq;
    fprintf(file, "feq.s");
    debug_print_op_fp(state, feq);
    break;
  }
  case RV32I_INSTR_TY_FEQ_D: {
    struct rv32i_op_fp *feq = &instr->rv32i->feq;
    fprintf(file, "feq.d");
    debug_print_op_fp(state, feq);
    break;
  }
  case RV32I_INSTR_TY_FLT_S: {
    struct rv32i_op_fp *flt = &instr->rv32i->flt;
    fprintf(file, "flt.s");
    debug_print_op_fp(state, flt);
    break;
  }
  case RV32I_INSTR_TY_FLT_D: {
    struct rv32i_op_fp *flt = &instr->rv32i->flt;
    fprintf(file, "flt.d");
    debug_print_op_fp(state, flt);
    break;
  }
  case RV32I_INSTR_TY_FLE_S: {
    struct rv32i_op_fp *fle = &instr->rv32i->fle;
    fprintf(file, "fle.s");
    debug_print_op_fp(state, fle);
    break;
  }
  case RV32I_INSTR_TY_FLE_D: {
    struct rv32i_op_fp *fle = &instr->rv32i->fle;
    fprintf(file, "fle.d");
    debug_print_op_fp(state, fle);
    break;
  }
  case RV32I_INSTR_TY_FCLASS_S: {
    struct rv32i_op_unary_fp *fclass = &instr->rv32i->fclass;
    fprintf(file, "fclass.s");
    debug_print_op_unary_fp(state, fclass);
    break;
  }
  case RV32I_INSTR_TY_FCLASS_D: {
    struct rv32i_op_unary_fp *fclass = &instr->rv32i->fclass;
    fprintf(file, "fclass.d");
    debug_print_op_unary_fp(state, fclass);
    break;
  }
  case RV32I_INSTR_TY_FENCE:
  case RV32I_INSTR_TY_FENCE_TSO:
  case RV32I_INSTR_TY_PAUSE:
  case RV32I_INSTR_TY_ECALL:
  case RV32I_INSTR_TY_EBREAK:
    TODO("debug print other instrs");
  }
}

void rv32i_debug_print_codegen(FILE *file, struct cg_unit *unit) {
  DEBUG_ASSERT(unit->ty == CODEGEN_UNIT_TY_RV32I, "expected rv32i");

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct cg_entry *entry = &unit->entries[i];

    if (entry->ty != CG_ENTRY_TY_FUNC) {
      continue;
    }

    struct cg_func *func = &entry->func;

    fprintf(file, "\nFUNCTION: %s\n", entry->name);
    fprintf(file, "  prologue: %s\n", entry->func.prologue ? "true" : "false");
    fprintf(file, "  stack_size: %zu\n", entry->func.stack_size);
    fprintf(file, "\n");

    size_t offset = 0;
    struct cg_basicblock *basicblock = func->first;
    while (basicblock) {
      struct instr *instr = basicblock->first;
      while (instr) {
        struct codegen_debug_state state = {
            .file = file, .func = func, .instr = instr};

        fprintf(file, "%04zu: ", offset++ * 4);
        print_instr(&state, CODEGEN_FLAG_NONE, instr);
        fprintf(file, "\n");

        instr = instr->succ;
      }
      basicblock = basicblock->succ;
    }

    fprintf(file, "\n");
  }
}

void rv32i_emit_asm(FILE *file, struct cg_unit *unit, enum codegen_flags flags) {
  DEBUG_ASSERT(unit->ty == CODEGEN_UNIT_TY_RV32I, "expected rv32i");

  fprintf(file, ".data\n");

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct cg_entry *entry = &unit->entries[i];

    switch (entry->ty) {
    case CG_ENTRY_TY_FUNC:
      // done later
      break;
    case CG_ENTRY_TY_STRING: {
      // FIXME: escape
      fprintf(file, "\n%s:\n", entry->name);
      fprintf(file, "        .string ");
      fprint_str(file, entry->data.data, entry->data.len_data);
      break;
    }
    case CG_ENTRY_TY_CONST_DATA:
    case CG_ENTRY_TY_DATA:
      // FIXME: escape
      fprintf(file, "\n%s:\n", entry->name);
      fprintf(file, "        .byte ");
      for (size_t j = 0; j < entry->data.len_data; j++) {
        fprintf(file, "%d", ((char *)entry->data.data)[j]);

        if (j + 1 != entry->data.len_data) {
          fprintf(file, ", ");
        }
      }
      fprintf(file, "\n");

      for (size_t j = 0; j < entry->data.num_relocs; j++) {
        struct relocation *reloc = &entry->data.relocs[j];

        DEBUG_ASSERT(reloc->ty == RELOCATION_TY_POINTER,
                     "non pointer reloc in .data");

        struct cg_entry *target = &unit->entries[reloc->symbol_index];
        fprintf(file, "        .reloc %s + %zu, R_RISCV_32, %s + %zu",
                entry->name, reloc->address, target->name, reloc->offset);
      }
      fprintf(file, "\n");
      break;
    case CG_ENTRY_TY_DECL: {
      continue;
    }
    }

    fprintf(file, "\n");
  }

  fprintf(file, ".text\n");

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct cg_entry *entry = &unit->entries[i];

    switch (entry->ty) {
    case CG_ENTRY_TY_FUNC: {
      struct cg_func *func = &entry->func;

      if (entry->symbol.visibility == SYMBOL_VISIBILITY_GLOBAL) {
        fprintf(file, ".globl %s\n", entry->name);
      }

      fprintf(file,
              ".align 4\n");
      fprintf(file, "\n%s:\n", entry->name);

      struct cg_basicblock *basicblock = func->first;
      while (basicblock) {
        if (basicblock->ir_basicblock) {
          print_bb_symbol(file, basicblock->ir_basicblock);
          fprintf(file, ":\n");
        }

        struct instr *instr = basicblock->first;
        while (instr) {
          struct codegen_debug_state state = {
              .file = file, .func = func, .instr = instr};

          fprintf(file, "        ");
          print_instr(&state, flags, instr);
          fprintf(file, "\n");

          instr = instr->succ;
        }

        basicblock = basicblock->succ;
      }

      break;
    }
    case CG_ENTRY_TY_STRING:
    case CG_ENTRY_TY_CONST_DATA:
    case CG_ENTRY_TY_DATA:
      break;
    case CG_ENTRY_TY_DECL:
      continue;
    }

    fprintf(file, "\n");
  }
}
