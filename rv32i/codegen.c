#include "codegen.h"

#include "../alloc.h"
#include "../bit_twiddle.h"
#include "../bitset.h"
#include "../log.h"
#include "../rv32i.h"
#include "../vector.h"

#define MOV_ALIAS(dest_reg, source_reg)                                        \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_ADD, .add = {                                         \
      .lhs = (source_reg),                                                     \
      .rhs = GP_ZERO_REG,                                                      \
      .dest = (dest_reg),                                                      \
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

const char *rv32i_mangle(struct arena_allocator *arena, const char *name) {
  return name;
}

struct rv32i_prologue_info {
  bool prologue_generated;
  size_t stack_size;
  size_t save_start;
  unsigned long long saved_gp_registers;
  unsigned long long saved_fp_registers;
};

struct codegen_state {
  struct arena_allocator *arena;

  struct codegen_function *func;
  struct ir_func *ir;
  struct rv32i_prologue_info prologue_info;

  size_t stack_args_size;
};

static ssize_t get_lcl_stack_offset(const struct codegen_state *state,
                                    UNUSED const struct ir_op *op,
                                    const struct ir_lcl *lcl) {
  ssize_t offset = state->stack_args_size + lcl->offset;

  return offset;
}

static struct rv32i_reg return_reg_for_ty(enum rv32i_reg_ty reg_ty) {
  return (struct rv32i_reg){.ty = reg_ty, .idx = 10};
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
    if (idx >= 11) {
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

static void codegen_mov_op(struct codegen_state *state, struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg source = codegen_reg(op->mov.value);

  struct instr *instr = alloc_instr(state->func);
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

static void codegen_br_cond_op(struct codegen_state *state, struct ir_op *op) {
  struct ir_basicblock *true_target = op->stmt->basicblock->split.true_target;
  struct ir_basicblock *false_target = op->stmt->basicblock->split.false_target;

  struct ir_op *cond = op->br_cond.cond;

  if (cond->flags & IR_OP_FLAG_CONTAINED) {
    struct rv32i_reg lhs_reg = codegen_reg(cond->binary_op.lhs);
    struct rv32i_reg rhs_reg = codegen_reg(cond->binary_op.rhs);

    enum rv32i_instr_ty br_ty;
    bool invert;
    switch (cond->binary_op.ty) {
    case IR_OP_BINARY_OP_TY_EQ:
      br_ty = RV32I_INSTR_TY_BEQ;
      invert = false;
      break;
    case IR_OP_BINARY_OP_TY_NEQ:
      br_ty = RV32I_INSTR_TY_BNE;
      invert = false;
      break;
    case IR_OP_BINARY_OP_TY_UGT:
      br_ty = RV32I_INSTR_TY_BLTU;
      invert = true;
      break;
    case IR_OP_BINARY_OP_TY_SGT:
      br_ty = RV32I_INSTR_TY_BLT;
      invert = true;
      break;
    case IR_OP_BINARY_OP_TY_UGTEQ:
      br_ty = RV32I_INSTR_TY_BGEU;
      invert = false;
      break;
    case IR_OP_BINARY_OP_TY_SGTEQ:
      br_ty = RV32I_INSTR_TY_BGE;
      invert = false;
      break;
    case IR_OP_BINARY_OP_TY_ULT:
      br_ty = RV32I_INSTR_TY_BLTU;
      invert = false;
      break;
    case IR_OP_BINARY_OP_TY_SLT:
      br_ty = RV32I_INSTR_TY_BLT;
      invert = false;
      break;
    case IR_OP_BINARY_OP_TY_ULTEQ:
      br_ty = RV32I_INSTR_TY_BGEU;
      invert = true;
      break;
    case IR_OP_BINARY_OP_TY_SLTEQ:
      br_ty = RV32I_INSTR_TY_BGE;
      invert = true;
      break;
    default:
      BUG("floating point etc");
    }

    struct instr *instr = alloc_instr(state->func);
    instr->rv32i->ty = br_ty;
    instr->rv32i->conditional_branch =
        (struct rv32i_conditional_branch){.lhs = invert ? rhs_reg : lhs_reg,
                                          .rhs = invert ? lhs_reg : rhs_reg,
                                          .target = true_target};
  } else {
    struct rv32i_reg cmp_reg = codegen_reg(cond);

    struct instr *instr = alloc_instr(state->func);
    instr->rv32i->ty = RV32I_INSTR_TY_BNE;
    instr->rv32i->bne = (struct rv32i_conditional_branch){
        .lhs = cmp_reg, .rhs = GP_ZERO_REG, .target = true_target};
  }

  // rv32i requires turning `br.cond <true> <false>` into 2 instructions
  // we represent this as just the `true` part of the `br.cond`, and then a
  // `br`
  // after branching to the false target

  // now generate the `br`
  struct instr *br = alloc_instr(state->func);
  br->rv32i->ty = RV32I_INSTR_TY_JAL;
  br->rv32i->jal =
      (struct rv32i_jal){.ret_addr = GP_ZERO_REG, .target = false_target};
}

static void codegen_br_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);
  instr->rv32i->ty = RV32I_INSTR_TY_JAL;
  instr->rv32i->jal = (struct rv32i_jal){
      .ret_addr = GP_ZERO_REG, .target = op->stmt->basicblock->merge.target};
}

static void codegen_cnst_op(struct codegen_state *state, struct ir_op *op) {
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
      int lo = (int)((unsigned)cnst - ((unsigned)hi << 12));

      struct rv32i_reg source_reg = GP_ZERO_REG;

      if (hi) {
        struct instr *instr = alloc_instr(state->func);
        instr->rv32i->ty = RV32I_INSTR_TY_LUI;
        instr->rv32i->lui = (struct rv32i_u){.dest = dest, .imm = hi};

        source_reg = dest;
      }

      struct instr *instr = alloc_instr(state->func);
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


static void codegen_add_imm(struct codegen_state *state,
                            struct rv32i_reg dest, struct rv32i_reg source,
                            long long value);

#define RV32I_STACK_ALIGNMENT (4)

static void codegen_prologue(struct codegen_state *state) {
  struct ir_func *ir = state->ir;

  size_t stack_size =
      state->ir->total_locals_size + state->ir->caller_stack_needed;
  stack_size =
      ROUND_UP(stack_size + ir->total_locals_size, RV32I_STACK_ALIGNMENT);

  unsigned long long saved_gp_registers = 0;
  unsigned long long saved_fp_registers = 0;

  struct bitset_iter gp_iter =
      bitset_iter(ir->reg_usage.gp_registers_used,
                  RV32I_LINUX_TARGET.reg_info.gp_registers.num_volatile, true);

  size_t i;
  while (bitset_iter_next(&gp_iter, &i)) {
    saved_gp_registers |= (1 << i);
    stack_size += 8;
  }

  struct bitset_iter fp_iter =
      bitset_iter(ir->reg_usage.fp_registers_used,
                  RV32I_LINUX_TARGET.reg_info.fp_registers.num_volatile, true);

  while (bitset_iter_next(&fp_iter, &i)) {
    saved_fp_registers |= (1 << i);
    stack_size += 8;
  }

  // TODO: implement red zone. requires _subtracting_ from `sp` instead of
  // adding for all local addressing bool leaf =
  //     !(stack_size > LEAF_STACK_SIZE || ir->flags & IR_FUNC_FLAG_MAKES_CALL);
  bool leaf = !(stack_size || ir->flags & IR_FUNC_FLAG_MAKES_CALL);

  struct rv32i_prologue_info info = {.prologue_generated = !leaf,
                                     .saved_gp_registers = saved_gp_registers,
                                     .saved_fp_registers = saved_fp_registers,
                                     .save_start = stack_size,
                                     .stack_size = stack_size};

  if (!info.prologue_generated) {
    return;
  }

  info.stack_size += 8;

  info.stack_size = ROUND_UP(info.stack_size, RV32I_STACK_ALIGNMENT);

  struct instr *save_ra = alloc_instr(state->func);
  save_ra->rv32i->ty = RV32I_INSTR_TY_SW;
  save_ra->rv32i->sw = (struct rv32i_store){
      .source = RET_PTR_REG, .addr = STACK_PTR_REG, .imm = -4};

  struct instr *save_sp = alloc_instr(state->func);
  save_sp->rv32i->ty = RV32I_INSTR_TY_SW;
  save_sp->rv32i->sw = (struct rv32i_store){
      .source = STACK_PTR_REG, .addr = STACK_PTR_REG, .imm = -8};

  ssize_t stack_to_sub = info.stack_size;
  codegen_add_imm(state, STACK_PTR_REG, STACK_PTR_REG, -stack_to_sub);

  size_t save_idx = 0;

  struct bitset_iter gp_reg_iter =
      bitset_iter(ir->reg_usage.gp_registers_used,
                  RV32I_LINUX_TARGET.reg_info.gp_registers.num_volatile, true);

  size_t idx;
  while (bitset_iter_next(&gp_reg_iter, &idx)) {
    // guaranteed to be mod 8
    size_t offset = (info.save_start / 8) + save_idx++;

    struct instr *save = alloc_instr(state->func);
    save->rv32i->ty = RV32I_INSTR_TY_SW;
    save->rv32i->sw = (struct rv32i_store){
        .source = (struct rv32i_reg){.ty = RV32I_REG_TY_W,
                                     .idx = translate_reg_idx(
                                         idx, IR_REG_TY_INTEGRAL)},
        .addr = STACK_PTR_REG,
        .imm = offset};
  }

  struct bitset_iter fp_reg_iter =
      bitset_iter(ir->reg_usage.fp_registers_used,
                  RV32I_LINUX_TARGET.reg_info.fp_registers.num_volatile, true);

  while (bitset_iter_next(&fp_reg_iter, &idx)) {
    // guaranteed to be mod 8
    size_t offset = (info.save_start / 8) + save_idx++;

    struct instr *save = alloc_instr(state->func);
    save->rv32i->ty = RV32I_INSTR_TY_FSW;
    save->rv32i->fsw = (struct rv32i_store){
        .source = (struct rv32i_reg){.ty = RV32I_REG_TY_F,
                                     .idx = translate_reg_idx(
                                         idx, IR_REG_TY_INTEGRAL)},
        .addr = STACK_PTR_REG,
        .imm = offset};
  }

  state->prologue_info = info;
}

#define IMM_BITS (12)

static void codegen_add_imm(struct codegen_state *state,
                            struct rv32i_reg dest, struct rv32i_reg source,
                            long long value) {
  long long abs_val = value >= 0 ? value : -value;

  do {
    ssize_t chunk = MIN(0x7FF, abs_val);

    struct instr *instr = alloc_instr(state->func);
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

static void codegen_epilogue(struct codegen_state *state) {
  const struct rv32i_prologue_info *prologue_info = &state->prologue_info;

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
    restore->rv32i->ty = RV32I_INSTR_TY_LW;
    restore->rv32i->lw = (struct rv32i_load){
        .imm = offset,
        .dest =
            (struct rv32i_reg){.ty = RV32I_REG_TY_W,
                               .idx = translate_reg_idx(i, IR_REG_TY_INTEGRAL)},
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
    restore->rv32i->ty = RV32I_INSTR_TY_FLW;
    restore->rv32i->flw = (struct rv32i_load){
        .imm = offset,
        .dest =
            (struct rv32i_reg){.ty = RV32I_REG_TY_F,
                               .idx = translate_reg_idx(i, IR_REG_TY_INTEGRAL)},
        .addr = STACK_PTR_REG,
    };
  }

  size_t stack_to_add = prologue_info->stack_size;
  codegen_add_imm(state, STACK_PTR_REG, STACK_PTR_REG, stack_to_add);

  struct instr *restore_ra = alloc_instr(state->func);
  restore_ra->rv32i->ty = RV32I_INSTR_TY_LW;
  restore_ra->rv32i->lw = (struct rv32i_load){
      .dest = RET_PTR_REG, .addr = STACK_PTR_REG, .imm = -4};

  struct instr *restore_sp = alloc_instr(state->func);
  restore_sp->rv32i->ty = RV32I_INSTR_TY_LW;
  restore_sp->rv32i->lw = (struct rv32i_load){
      .dest = STACK_PTR_REG, .addr = STACK_PTR_REG, .imm = -8};
}

static void codegen_ret_op(struct codegen_state *state, struct ir_op *op) {
  if (op->ret.value && op->ret.value->ty != IR_OP_TY_CALL) {
    struct rv32i_reg source = codegen_reg(op->ret.value);

    if (source.idx != return_reg_for_ty(source.ty).idx) {
      struct instr *instr = alloc_instr(state->func);

      if (source.ty == RV32I_REG_TY_W) {
        *instr->rv32i = MOV_ALIAS(return_reg_for_ty(source.ty), source);
      } else {
        DEBUG_ASSERT(var_ty_is_fp(&op->var_ty), "expected fp");

        switch (op->var_ty.primitive) {
        case IR_VAR_PRIMITIVE_TY_F32:
          *instr->rv32i = FP32_MOV_ALIAS(return_reg_for_ty(source.ty), source);
          break;
        case IR_VAR_PRIMITIVE_TY_F64:
          *instr->rv32i = FP64_MOV_ALIAS(return_reg_for_ty(source.ty), source);
          break;
        default:
          BUG("unsupported");
        }
      }
    }
  }

  codegen_epilogue(state);

  struct instr *instr = alloc_instr(state->func);

  instr->rv32i->ty = RV32I_INSTR_TY_JALR;
  instr->rv32i->jalr = (struct rv32i_jalr){
      .ret_addr = GP_ZERO_REG, .target = RET_PTR_REG, .imm = 0};
}

static void codegen_unary_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

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

static void codegen_binary_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

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
      struct instr *xori = alloc_instr(state->func);
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

    struct instr *sltiu = alloc_instr(state->func);
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

    struct instr *sltu = alloc_instr(state->func);
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
      struct instr *xori = alloc_instr(state->func);
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

static void codegen_load_addr_op(struct codegen_state *state,
                                 struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg dest = codegen_reg(op);

  if (op->load.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr = op->load.addr;

    simm_t imm;
    if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
      imm = get_lcl_stack_offset(state, op, addr->addr.lcl);
    } else {
      BUG("can't CONTAIN operand in load_addr node");
    }

    instr->rv32i->ty = load_ty_for_op(op);
    instr->rv32i->load =
        (struct rv32i_load){.dest = dest, .addr = STACK_PTR_REG, .imm = imm};
  } else {
    struct rv32i_reg addr = codegen_reg(op->load.addr);
    instr->rv32i->ty = load_ty_for_op(op);
    instr->rv32i->load =
        (struct rv32i_load){.dest = dest, .addr = addr, .imm = 0};
  }
}

static void codegen_load_lcl_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg dest = codegen_reg(op);
  struct ir_lcl *lcl = op->load.lcl;

  simm_t offset = get_lcl_stack_offset(state, op, lcl);

  instr->rv32i->ty = load_ty_for_op(op);
  instr->rv32i->load =
      (struct rv32i_load){.dest = dest, .addr = STACK_PTR_REG, .imm = offset};
}

static void codegen_store_lcl_op(struct codegen_state *state,
                                 struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg source = codegen_reg(op->store.value);
  struct ir_lcl *lcl = op->store.lcl;

  instr->rv32i->ty = store_ty_for_op(op->store.value);
  instr->rv32i->store = (struct rv32i_store){
      .source = source,
      .addr = STACK_PTR_REG,
      .imm = get_lcl_stack_offset(state, op->store.value, lcl)};
}

static void codegen_store_addr_op(struct codegen_state *state,
                                  struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg source = codegen_reg(op->store.value);

  if (op->store.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr = op->store.addr;

    simm_t imm;
    if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
      imm = get_lcl_stack_offset(state, op->store.value, addr->addr.lcl);
    } else {
      BUG("can't CONTAIN operand in store_addr node");
    }

    instr->rv32i->ty = store_ty_for_op(op->store.value);
    instr->rv32i->store = (struct rv32i_store){
        .source = source, .addr = STACK_PTR_REG, .imm = imm};
  } else {
    struct rv32i_reg addr = codegen_reg(op->store.addr);
    instr->rv32i->ty = store_ty_for_op(op->store.value);
    instr->rv32i->store =
        (struct rv32i_store){.source = source, .addr = addr, .imm = 0};
  }
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

static void codegen_addr_op(struct codegen_state *state, struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);

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
    adrp->rv32i->ty = RV32I_INSTR_TY_LUI;
    adrp->rv32i->lui = (struct rv32i_u){.dest = dest, .imm = 0};

    adrp->reloc = arena_alloc(state->func->unit->arena, sizeof(*adrp->reloc));
    *adrp->reloc = (struct relocation){
        .ty = glb->def_ty == IR_GLB_DEF_TY_DEFINED ? RELOCATION_TY_LOCAL_PAIR
                                                   : RELOCATION_TY_UNDEF_PAIR,
        .symbol_index = glb->id,
        .address = 0,
        .size = 0};

    if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
      struct instr *add = alloc_instr(state->func);
      add->rv32i->ty = RV32I_INSTR_TY_ADDI;
      add->rv32i->addi = (struct rv32i_op_imm){
          .dest = dest,
          .source = dest,
          .imm = 0,
      };
    } else {
      struct instr *lw = alloc_instr(state->func);
      lw->rv32i->ty = RV32I_INSTR_TY_LW;
      lw->rv32i->lw = (struct rv32i_load){
          .dest = dest,
          .addr = dest,
          .imm = 0,
      };
    }
  }
  }
}

static void codegen_sext_op(struct codegen_state *state, struct ir_op *op,
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
    struct instr *sl = alloc_instr(state->func);
    sl->rv32i->ty = RV32I_INSTR_TY_SLLI;
    sl->rv32i->slli =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = sh_sz};
    struct instr *sr = alloc_instr(state->func);
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

static void codegen_zext_op(struct codegen_state *state, struct ir_op *op,
                            struct rv32i_reg source, struct rv32i_reg dest) {

  invariant_assert(op->cast_op.value->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
                   "can't sext from non-primitive");

  switch (op->cast_op.value->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I8: {
    struct instr *and = alloc_instr(state->func);
    and->rv32i->ty = RV32I_INSTR_TY_ANDI;
    and->rv32i->andi =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 0xFF};
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I16: {
    struct instr *sl = alloc_instr(state->func);
    sl->rv32i->ty = RV32I_INSTR_TY_SLLI;
    sl->rv32i->slli =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 16};
    struct instr *sr = alloc_instr(state->func);
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

static void codegen_trunc_op(struct codegen_state *state, struct ir_op *op,
                             struct rv32i_reg source, struct rv32i_reg dest) {
  switch (op->var_ty.primitive) {
  case IR_VAR_PRIMITIVE_TY_I8: {
    struct instr *and = alloc_instr(state->func);
    and->rv32i->ty = RV32I_INSTR_TY_ANDI;
    and->rv32i->andi =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 0xFF};
    break;
  }
  case IR_VAR_PRIMITIVE_TY_I16: {
    struct instr *sl = alloc_instr(state->func);
    sl->rv32i->ty = RV32I_INSTR_TY_SLLI;
    sl->rv32i->slli =
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = 16};
    struct instr *sr = alloc_instr(state->func);
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

static void codegen_conv_op(struct codegen_state *state, struct ir_op *op,
                            struct rv32i_reg source, struct rv32i_reg dest) {
  struct instr *instr = alloc_instr(state->func);
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

static void codegen_uconv_op(struct codegen_state *state, struct ir_op *op,
                             struct rv32i_reg source, struct rv32i_reg dest) {
  struct instr *instr = alloc_instr(state->func);

  enum ir_var_primitive_ty ty;
  if (var_ty_is_fp(&op->var_ty)) {
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

static void codegen_sconv_op(struct codegen_state *state, struct ir_op *op,
                             struct rv32i_reg source, struct rv32i_reg dest) {
  struct instr *instr = alloc_instr(state->func);

  enum ir_var_primitive_ty ty;
  if (var_ty_is_fp(&op->var_ty)) {
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

static void codegen_cast_op(struct codegen_state *state, struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);
  struct rv32i_reg source = codegen_reg(op->cast_op.value);

  // NOTE: for the integer casts (sext/zext/trunc) we promote the source reg
  // to the same type as the dest reg (mixed regs make no sense in an integer
  // instruction)

  switch (op->cast_op.ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    source.ty = dest.ty;
    codegen_sext_op(state, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_ZEXT:
    source.ty = dest.ty;
    codegen_zext_op(state, op, source, dest);
    break;
  case IR_OP_CAST_OP_TY_TRUNC:
    source.ty = dest.ty;
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

static void codegen_call_op(struct codegen_state *state, struct ir_op *op) {
  invariant_assert(op->call.func_ty.ty == IR_VAR_TY_TY_FUNC, "non-func");

  const struct ir_var_func_ty *func_ty = &op->call.func_ty.func;

  invariant_assert(func_ty->num_params <= 8,
                   "`%s` doesn't support more than 8 args yet", __func__);

  if (op->call.target->flags & IR_OP_FLAG_CONTAINED) {
    DEBUG_ASSERT(op->call.target->ty == IR_OP_TY_ADDR &&
                     op->call.target->addr.ty == IR_OP_ADDR_TY_GLB,
                 "expected addr GLB");
    struct ir_glb *glb = op->call.target->addr.glb;

    if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
      struct instr *instr = alloc_instr(state->func);
      instr->rv32i->ty = RV32I_INSTR_TY_JAL;
      instr->rv32i->jal =
          (struct rv32i_jal){.target = NULL, .ret_addr = RET_PTR_REG};

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
      struct instr *auipc = alloc_instr(state->func);
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

      struct instr *jalr = alloc_instr(state->func);
      jalr->rv32i->ty = RV32I_INSTR_TY_JALR;
      jalr->rv32i->jalr = (struct rv32i_jalr){
          .target = RET_PTR_REG, .ret_addr = RET_PTR_REG, .imm = 0};
    }
  } else {
    // NOTE: `blr` seems to segfault on linux rv32i
    struct instr *instr = alloc_instr(state->func);
    instr->rv32i->ty = RV32I_INSTR_TY_JALR;
    instr->rv32i->jalr = (struct rv32i_jalr){
        .target = codegen_reg(op->call.target), .ret_addr = RET_PTR_REG};
  }
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

static void codegen_write_var_value(struct ir_unit *iru, struct vector *relocs,
                                    size_t offset, struct ir_var_value *value,
                                    char *data) {
  if (!value || value->ty == IR_VAR_VALUE_TY_ZERO) {
    return;
  }

#define COPY(ty, fld)                                                          \
  ty tmp##ty = (ty)value->fld;                                                 \
  memcpy(data, &tmp##ty, sizeof(tmp##ty))
  switch (value->var_ty.ty) {
  case IR_VAR_TY_TY_NONE:
  case IR_VAR_TY_TY_VARIADIC:
    break;
  case IR_VAR_TY_TY_PRIMITIVE: {
    switch (value->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I8:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint8_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F16:
    case IR_VAR_PRIMITIVE_TY_I16:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
                       value->ty == IR_VAR_VALUE_TY_FLT,
                   "expected int/flt");
      COPY(uint16_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I32:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint32_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_I64:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
      COPY(uint64_t, int_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F32:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_FLT, "expected flt");
      COPY(float, flt_value);
      break;
    case IR_VAR_PRIMITIVE_TY_F64:
      DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_FLT, "expected flt");
      COPY(double, flt_value);
      break;
    }
    break;
  }
#undef COPY

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
struct codegen_unit *rv32i_codegen(struct ir_unit *ir) {
  struct codegen_unit *unit = arena_alloc(ir->arena, sizeof(*unit));
  *unit = (struct codegen_unit){
      .ty = CODEGEN_UNIT_TY_RV32I,
      .instr_size = sizeof(struct rv32i_instr),
      .num_entries = ir->num_globals,
      .entries = arena_alloc(ir->arena,
                             ir->num_globals * sizeof(struct codeen_entry *))};

  arena_allocator_create(&unit->arena);

  struct ir_glb *glb = ir->first_global;

  {
    size_t i = 0;
    while (glb) {
      if (glb->def_ty == IR_GLB_DEF_TY_UNDEFINED) {
        unit->entries[i] =
            (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_DECL,
                                   .glb_id = glb->id,
                                   .name = rv32i_mangle(ir->arena, glb->name)};

        i++;
        glb = glb->succ;
        continue;
      }

      switch (glb->ty) {
      case IR_GLB_TY_DATA: {
        // TODO: non string literals

        const char *name =
            glb->name ? rv32i_mangle(ir->arena, glb->name)
                      : mangle_str_cnst_name(ir->arena, "todo", glb->id);
        switch (glb->var->ty) {
        case IR_VAR_TY_STRING_LITERAL:
          unit->entries[i] =
              (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_STRING,
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
            .glb_id = glb->id,
            .name = rv32i_mangle(ir->arena, ir_func->name),
            .func = {
                .unit = unit, .first = NULL, .last = NULL, .instr_count = 0}};

        struct arena_allocator *arena;
        arena_allocator_create(&arena);

        struct codegen_function *func = &unit->entries[i].func;
        struct codegen_state state = {
            .arena = arena, .func = func, .ir = ir_func};

        state.stack_args_size = 0;

        codegen_prologue(&state);

        func->prologue = state.prologue_info.prologue_generated;
        func->stack_size = state.prologue_info.stack_size;

        struct ir_basicblock *basicblock = ir_func->first;
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
        codegen_sort_entries_by_id);

  if (log_enabled()) {
    rv32i_debug_print_codegen(stderr, unit);
  }

  // now do sanity checks
  // for (size_t i = 0; i < unit->num_entries; i++) {
  //   const struct codegen_entry *entry = &unit->entries[i];

  //   if (entry->ty == CODEGEN_ENTRY_TY_FUNC) {
  //     // codegen is now done
  //     // do some basic sanity checks
  //     struct check_reg_type_data data = {
  //         .entry = entry, .last = NULL, .reg_ty = 0};
  //     walk_regs(&entry->func, check_reg_type_callback, &data);
  //   }
  // }

  return unit;
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
    } else if (strncmp(format, "instr", 5) == 0) {
      struct ir_basicblock *basicblock = va_arg(list, struct ir_basicblock *);
      if (basicblock && basicblock->first_instr) {
        fprintf(file, "%%%zx", basicblock->first_instr->id);
      } else {
        fprintf(file, "%%(null)");
      }

      format += 5;
    } else if (strncmp(format, "imm", 3) == 0) {
      simm_t imm = va_arg(list, simm_t);
      fprintf(file, "%lld", imm);

      format += 3;
    } else if (format[0] == '%') {
      fputc('%', file);
      format++;
    } else {
      BUG("unrecognised format starting '%%%s'", format);
    }
  }
}

static void debug_print_op_imm(FILE *file, const struct rv32i_op_imm *op_imm) {
  codegen_fprintf(file, " %reg, %reg, %imm", op_imm->dest, op_imm->source,
                  op_imm->imm);
}

static void debug_print_op_fp(FILE *file, const struct rv32i_op_fp *op_fp) {
  codegen_fprintf(file, " %reg, %reg, %reg", op_fp->dest, op_fp->lhs,
                  op_fp->rhs);
}

static void
debug_print_op_unary_fp(FILE *file,
                        const struct rv32i_op_unary_fp *op_unary_fp) {
  codegen_fprintf(file, " %reg, %reg", op_unary_fp->dest, op_unary_fp->source);
}

static void debug_print_op(FILE *file, const struct rv32i_op *op) {
  codegen_fprintf(file, " %reg, %reg, %reg", op->dest, op->lhs, op->rhs);
}

static void debug_print_op_mov(FILE *file, const struct rv32i_op_mov *op_mov) {
  codegen_fprintf(file, " %reg, %reg", op_mov->dest, op_mov->source);
}

static void debug_print_lui(FILE *file, const struct rv32i_u *lui) {
  codegen_fprintf(file, " %reg, %imm", lui->dest, lui->imm);
}

static void debug_print_jalr(FILE *file, const struct rv32i_jalr *jalr) {
  codegen_fprintf(file, " %reg, %reg, %imm", jalr->ret_addr, jalr->target,
                  jalr->imm);
}

static void debug_print_jal(FILE *file, const struct rv32i_jal *jal) {
  codegen_fprintf(file, " %reg, %%%instr", jal->ret_addr, jal->target);
}

static void debug_print_load(FILE *file, const struct rv32i_load *load) {
  if (load->imm) {
    codegen_fprintf(file, " %reg, %imm(%reg)", load->dest, load->imm,
                    load->addr);
  } else {
    codegen_fprintf(file, " %reg, %reg", load->dest, load->addr);
  }
}

static void debug_print_store(FILE *file, const struct rv32i_store *store) {
  if (store->imm) {
    codegen_fprintf(file, " %reg, %imm(%reg)", store->source, store->imm,
                    store->addr);
  } else {
    codegen_fprintf(file, " %reg, %reg", store->source, store->addr);
  }
}

static void debug_print_conditional_branch(
    FILE *file, const struct rv32i_conditional_branch *conditional_branch) {
  codegen_fprintf(file, " %reg, %reg, %%%instr", conditional_branch->lhs,
                  conditional_branch->rhs, conditional_branch->target);
}

static void debug_print_instr(FILE *file,
                              UNUSED_ARG(const struct codegen_function *func),
                              const struct instr *instr) {

  switch (instr->rv32i->ty) {
  case RV32I_INSTR_TY_ADDI: {
    struct rv32i_op_imm *addi = &instr->rv32i->addi;
    fprintf(file, "addi");
    debug_print_op_imm(file, addi);
    break;
  }
  case RV32I_INSTR_TY_XORI: {
    struct rv32i_op_imm *addi = &instr->rv32i->addi;
    fprintf(file, "xori");
    debug_print_op_imm(file, addi);
    break;
  }
  case RV32I_INSTR_TY_ADD: {
    struct rv32i_op *add = &instr->rv32i->add;

    if (add->rhs.idx == 0) {
      codegen_fprintf(file, "mov %reg, %reg", instr->rv32i->add.dest,
                      instr->rv32i->add.lhs);
    } else {
      fprintf(file, "add");
      debug_print_op(file, add);
    }
    break;
  }
  case RV32I_INSTR_TY_SUB: {
    struct rv32i_op *sub = &instr->rv32i->sub;
    fprintf(file, "sub");
    debug_print_op(file, sub);
    break;
  }
  case RV32I_INSTR_TY_MUL: {
    struct rv32i_op *mul = &instr->rv32i->mul;
    fprintf(file, "mul");
    debug_print_op(file, mul);
    break;
  }
  case RV32I_INSTR_TY_DIV: {
    struct rv32i_op *div = &instr->rv32i->div;
    fprintf(file, "div");
    debug_print_op(file, div);
    break;
  }
  case RV32I_INSTR_TY_DIVU: {
    struct rv32i_op *divu = &instr->rv32i->divu;
    fprintf(file, "divu");
    debug_print_op(file, divu);
    break;
  }
  case RV32I_INSTR_TY_REM: {
    struct rv32i_op *rem = &instr->rv32i->rem;
    fprintf(file, "rem");
    debug_print_op(file, rem);
    break;
  }
  case RV32I_INSTR_TY_REMU: {
    struct rv32i_op *remu = &instr->rv32i->remu;
    fprintf(file, "remu");
    debug_print_op(file, remu);
    break;
  }
  case RV32I_INSTR_TY_LUI: {
    struct rv32i_u *lui = &instr->rv32i->lui;
    fprintf(file, "lui");
    debug_print_lui(file, lui);
    break;
  }
  case RV32I_INSTR_TY_AUIPC: {
    struct rv32i_u *auipc = &instr->rv32i->auipc;
    fprintf(file, "auipc");
    debug_print_lui(file, auipc);
    break;
  }
  case RV32I_INSTR_TY_JALR: {
    struct rv32i_jalr *jalr = &instr->rv32i->jalr;
    fprintf(file, "jalr");
    debug_print_jalr(file, jalr);
    break;
  }
  case RV32I_INSTR_TY_SB: {
    struct rv32i_store *sb = &instr->rv32i->sb;
    fprintf(file, "sb");
    debug_print_store(file, sb);
    break;
  }
  case RV32I_INSTR_TY_SH: {
    struct rv32i_store *sh = &instr->rv32i->sh;
    fprintf(file, "sh");
    debug_print_store(file, sh);
    break;
  }
  case RV32I_INSTR_TY_SW: {
    struct rv32i_store *sw = &instr->rv32i->sw;
    fprintf(file, "sw");
    debug_print_store(file, sw);
    break;
  }
  case RV32I_INSTR_TY_LB: {
    struct rv32i_load *lb = &instr->rv32i->lb;
    fprintf(file, "lb");
    debug_print_load(file, lb);
    break;
  }
  case RV32I_INSTR_TY_LBU: {
    struct rv32i_load *lbu = &instr->rv32i->lbu;
    fprintf(file, "lbu");
    debug_print_load(file, lbu);
    break;
  }
  case RV32I_INSTR_TY_LH: {
    struct rv32i_load *lh = &instr->rv32i->lh;
    fprintf(file, "lh");
    debug_print_load(file, lh);
    break;
  }
  case RV32I_INSTR_TY_LHU: {
    struct rv32i_load *lhu = &instr->rv32i->lhu;
    fprintf(file, "lhu");
    debug_print_load(file, lhu);
    break;
  }
  case RV32I_INSTR_TY_LW: {
    struct rv32i_load *lw = &instr->rv32i->lw;
    fprintf(file, "lw");
    debug_print_load(file, lw);
    break;
  }
  case RV32I_INSTR_TY_FSW: {
    struct rv32i_store *fsw = &instr->rv32i->fsw;
    fprintf(file, "fsw");
    debug_print_store(file, fsw);
    break;
  }
  case RV32I_INSTR_TY_FLW: {
    struct rv32i_load *flw = &instr->rv32i->flw;
    fprintf(file, "flw");
    debug_print_load(file, flw);
    break;
  }
  case RV32I_INSTR_TY_FSD: {
    struct rv32i_store *fsd = &instr->rv32i->fsd;
    fprintf(file, "fsd");
    debug_print_store(file, fsd);
    break;
  }
  case RV32I_INSTR_TY_FLD: {
    struct rv32i_load *fld = &instr->rv32i->fld;
    fprintf(file, "fld");
    debug_print_load(file, fld);
    break;
  }
  case RV32I_INSTR_TY_JAL: {
    struct rv32i_jal *jal = &instr->rv32i->jal;
    fprintf(file, "jal");
    debug_print_jal(file, jal);
    break;
  }
  case RV32I_INSTR_TY_BEQ: {
    struct rv32i_conditional_branch *beq = &instr->rv32i->beq;
    fprintf(file, "beq");
    debug_print_conditional_branch(file, beq);
    break;
  }
  case RV32I_INSTR_TY_BNE: {
    struct rv32i_conditional_branch *bne = &instr->rv32i->bne;
    fprintf(file, "bne");
    debug_print_conditional_branch(file, bne);
    break;
  }
  case RV32I_INSTR_TY_BLT: {
    struct rv32i_conditional_branch *blt = &instr->rv32i->blt;
    fprintf(file, "blt");
    debug_print_conditional_branch(file, blt);
    break;
  }
  case RV32I_INSTR_TY_BGE: {
    struct rv32i_conditional_branch *bge = &instr->rv32i->bge;
    fprintf(file, "bge");
    debug_print_conditional_branch(file, bge);
    break;
  }
  case RV32I_INSTR_TY_BLTU: {
    struct rv32i_conditional_branch *bltu = &instr->rv32i->bltu;
    fprintf(file, "bltu");
    debug_print_conditional_branch(file, bltu);
    break;
  }
  case RV32I_INSTR_TY_BGEU: {
    struct rv32i_conditional_branch *bgeu = &instr->rv32i->bgeu;
    fprintf(file, "bgeu");
    debug_print_conditional_branch(file, bgeu);
    break;
  }
  case RV32I_INSTR_TY_OR: {
    struct rv32i_op * or = &instr->rv32i->or ;
    fprintf(file, "or");
    debug_print_op(file, or);
    break;
  }
  case RV32I_INSTR_TY_AND: {
    struct rv32i_op *and = &instr->rv32i->and;
    fprintf(file, "and");
    debug_print_op(file, and);
    break;
  }
  case RV32I_INSTR_TY_XOR: {
    struct rv32i_op * xor = &instr->rv32i->xor ;
    fprintf(file, "xor");
    debug_print_op(file, xor);
    break;
  }
  case RV32I_INSTR_TY_SLL: {
    struct rv32i_op *sll = &instr->rv32i->sll;
    fprintf(file, "sll");
    debug_print_op(file, sll);
    break;
  }
  case RV32I_INSTR_TY_SRL: {
    struct rv32i_op *srl = &instr->rv32i->srl;
    fprintf(file, "srl");
    debug_print_op(file, srl);
    break;
  }
  case RV32I_INSTR_TY_SRA: {
    struct rv32i_op *sra = &instr->rv32i->sra;
    fprintf(file, "sra");
    debug_print_op(file, sra);
    break;
  }
  case RV32I_INSTR_TY_FMV_S: {
    struct rv32i_op_mov *fmv = &instr->rv32i->fmv;

    switch (fmv->source.ty) {
    case RV32I_REG_TY_NONE:
      BUG("none dest");
    case RV32I_REG_TY_W:
      fprintf(file, "fmv.w.s");
      break;
    case RV32I_REG_TY_F:
      fprintf(file, "fmv.s.w");
      break;
    }

    debug_print_op_mov(file, fmv);
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

    debug_print_op_mov(file, fmv);
    break;
  }
  case RV32I_INSTR_TY_FADD_S: {
    struct rv32i_op_fp *fadd = &instr->rv32i->fadd;
    fprintf(file, "fadd.s");
    debug_print_op_fp(file, fadd);
    break;
  }
  case RV32I_INSTR_TY_FADD_D: {
    struct rv32i_op_fp *fadd = &instr->rv32i->fadd;
    fprintf(file, "fadd.d");
    debug_print_op_fp(file, fadd);
    break;
  }
  case RV32I_INSTR_TY_FSUB_S: {
    struct rv32i_op_fp *fsub = &instr->rv32i->fsub;
    fprintf(file, "fsub.s");
    debug_print_op_fp(file, fsub);
    break;
  }
  case RV32I_INSTR_TY_FSUB_D: {
    struct rv32i_op_fp *fsub = &instr->rv32i->fsub;
    fprintf(file, "fsub.d");
    debug_print_op_fp(file, fsub);
    break;
  }
  case RV32I_INSTR_TY_FSGNJ_S: {
    if (instr->rv32i->fsgnj.lhs.idx == instr->rv32i->fsgnj.rhs.idx) {
      codegen_fprintf(file, "fmv %reg, %reg", instr->rv32i->fsgnj.dest,
                      instr->rv32i->fsgnj.lhs);
    } else {
      struct rv32i_op_fp *fsgnj = &instr->rv32i->fsgnj;
      fprintf(file, "fsgnj.s");
      debug_print_op_fp(file, fsgnj);
    }
    break;
  }
  case RV32I_INSTR_TY_FSGNJ_D: {
    if (instr->rv32i->fsgnj.lhs.idx == instr->rv32i->fsgnj.rhs.idx) {
      codegen_fprintf(file, "fmv %reg, %reg", instr->rv32i->fsgnj.dest,
                      instr->rv32i->fsgnj.lhs);
    } else {
      struct rv32i_op_fp *fsgnj = &instr->rv32i->fsgnj;
      fprintf(file, "fsgnj.d");
      debug_print_op_fp(file, fsgnj);
    }
    break;
  }
  case RV32I_INSTR_TY_FMUL_S: {
    struct rv32i_op_fp *fmul = &instr->rv32i->fmul;
    fprintf(file, "fmul.s");
    debug_print_op_fp(file, fmul);
    break;
  }
  case RV32I_INSTR_TY_FMUL_D: {
    struct rv32i_op_fp *fmul = &instr->rv32i->fmul;
    fprintf(file, "fmul.d");
    debug_print_op_fp(file, fmul);
    break;
  }

  case RV32I_INSTR_TY_FDIV_S: {
    struct rv32i_op_fp *fdiv = &instr->rv32i->fdiv;
    fprintf(file, "fdiv.s");
    debug_print_op_fp(file, fdiv);
    break;
  }
  case RV32I_INSTR_TY_FDIV_D: {
    struct rv32i_op_fp *fdiv = &instr->rv32i->fdiv;
    fprintf(file, "fdiv.d");
    debug_print_op_fp(file, fdiv);
    break;
  }

  case RV32I_INSTR_TY_FSGNJN_S: {
    struct rv32i_op_fp *fsgnjn = &instr->rv32i->fsgnjn;
    fprintf(file, "fsgnjn.s");
    debug_print_op_fp(file, fsgnjn);
    break;
  }
  case RV32I_INSTR_TY_FSGNJN_D: {
    struct rv32i_op_fp *fsgnjn = &instr->rv32i->fsgnjn;
    fprintf(file, "fsgnjn.d");
    debug_print_op_fp(file, fsgnjn);
    break;
  }

  case RV32I_INSTR_TY_FSGNJX_S: {
    struct rv32i_op_fp *fsgnjx = &instr->rv32i->fsgnjx;
    fprintf(file, "fsgnjx.s");
    debug_print_op_fp(file, fsgnjx);
    break;
  }
  case RV32I_INSTR_TY_FSGNJX_D: {
    struct rv32i_op_fp *fsgnjx = &instr->rv32i->fsgnjx;
    fprintf(file, "fsgnjx.d");
    debug_print_op_fp(file, fsgnjx);
    break;
  }

  case RV32I_INSTR_TY_FMAX_S: {
    struct rv32i_op_fp *fmax = &instr->rv32i->fmax;
    fprintf(file, "fmax.s");
    debug_print_op_fp(file, fmax);
    break;
  }
  case RV32I_INSTR_TY_FMAX_D: {
    struct rv32i_op_fp *fmax = &instr->rv32i->fmax;
    fprintf(file, "fmax.d");
    debug_print_op_fp(file, fmax);
    break;
  }

  case RV32I_INSTR_TY_FMIN_S: {
    struct rv32i_op_fp *fmin = &instr->rv32i->fmin;
    fprintf(file, "fmin.s");
    debug_print_op_fp(file, fmin);
    break;
  }
  case RV32I_INSTR_TY_FMIN_D: {
    struct rv32i_op_fp *fmin = &instr->rv32i->fmin;
    fprintf(file, "fmin.d");
    debug_print_op_fp(file, fmin);
    break;
  }

  case RV32I_INSTR_TY_FSQRT_S: {
    struct rv32i_op_unary_fp *fsqrt = &instr->rv32i->fsqrt;
    fprintf(file, "fsqrt.s");
    debug_print_op_unary_fp(file, fsqrt);
    break;
  }
  case RV32I_INSTR_TY_FSQRT_D: {
    struct rv32i_op_unary_fp *fsqrt = &instr->rv32i->fsqrt;
    fprintf(file, "fsqrt.d");
    debug_print_op_unary_fp(file, fsqrt);
    break;
  }

  case RV32I_INSTR_TY_ORI: {
    struct rv32i_op_imm *ori = &instr->rv32i->ori;
    fprintf(file, "ori");
    debug_print_op_imm(file, ori);
    break;
  }
  case RV32I_INSTR_TY_ANDI: {
    struct rv32i_op_imm *andi = &instr->rv32i->andi;
    fprintf(file, "andi");
    debug_print_op_imm(file, andi);
    break;
  }
  case RV32I_INSTR_TY_SLLI: {
    struct rv32i_op_imm *slli = &instr->rv32i->slli;
    fprintf(file, "slli");
    debug_print_op_imm(file, slli);
    break;
  }
  case RV32I_INSTR_TY_SRLI: {
    struct rv32i_op_imm *srli = &instr->rv32i->srli;
    fprintf(file, "srli");
    debug_print_op_imm(file, srli);
    break;
  }
  case RV32I_INSTR_TY_SRAI: {
    struct rv32i_op_imm *srai = &instr->rv32i->srai;
    fprintf(file, "srai");
    debug_print_op_imm(file, srai);
    break;
  }
  case RV32I_INSTR_TY_SLT: {
    struct rv32i_op *slt = &instr->rv32i->slt;
    fprintf(file, "slt");
    debug_print_op(file, slt);
    break;
  }
  case RV32I_INSTR_TY_SLTU: {
    struct rv32i_op *sltu = &instr->rv32i->sltu;
    fprintf(file, "sltu");
    debug_print_op(file, sltu);
    break;
  }

  case RV32I_INSTR_TY_SLTI: {
    struct rv32i_op_imm *slti = &instr->rv32i->slti;
    fprintf(file, "slti");
    debug_print_op_imm(file, slti);
    break;
  }
  case RV32I_INSTR_TY_SLTIU: {
    struct rv32i_op_imm *sltiu = &instr->rv32i->sltiu;
    fprintf(file, "sltiu");
    debug_print_op_imm(file, sltiu);
    break;
  }
  case RV32I_INSTR_TY_MULH: {
    struct rv32i_op *mulh = &instr->rv32i->mulh;
    fprintf(file, "mulh");
    debug_print_op(file, mulh);
    break;
  }
  case RV32I_INSTR_TY_MULHU: {
    struct rv32i_op *mulhu = &instr->rv32i->mulhu;
    fprintf(file, "mulhu");
    debug_print_op(file, mulhu);
    break;
  }
  case RV32I_INSTR_TY_MULHSU: {
    struct rv32i_op *mulhsu = &instr->rv32i->mulhsu;
    fprintf(file, "mulhsu");
    debug_print_op(file, mulhsu);
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

    debug_print_op_mov(file, fcvt);
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

    debug_print_op_mov(file, fcvt);
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
    debug_print_op_mov(file, fcvtu);
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
    debug_print_op_mov(file, fcvtu);
    break;
  }
  case RV32I_INSTR_TY_FEQ_S: {
    struct rv32i_op_fp *feq = &instr->rv32i->feq;
    fprintf(file, "feq.s");
    debug_print_op_fp(file, feq);
    break;
  }
  case RV32I_INSTR_TY_FEQ_D: {
    struct rv32i_op_fp *feq = &instr->rv32i->feq;
    fprintf(file, "feq.d");
    debug_print_op_fp(file, feq);
    break;
  }
  case RV32I_INSTR_TY_FLT_S: {
    struct rv32i_op_fp *flt = &instr->rv32i->flt;
    fprintf(file, "flt.s");
    debug_print_op_fp(file, flt);
    break;
  }
  case RV32I_INSTR_TY_FLT_D: {
    struct rv32i_op_fp *flt = &instr->rv32i->flt;
    fprintf(file, "flt.d");
    debug_print_op_fp(file, flt);
    break;
  }
  case RV32I_INSTR_TY_FLE_S: {
    struct rv32i_op_fp *fle = &instr->rv32i->fle;
    fprintf(file, "fle.s");
    debug_print_op_fp(file, fle);
    break;
  }
  case RV32I_INSTR_TY_FLE_D: {
    struct rv32i_op_fp *fle = &instr->rv32i->fle;
    fprintf(file, "fle.d");
    debug_print_op_fp(file, fle);
    break;
  }
  case RV32I_INSTR_TY_FCLASS_S: {
    struct rv32i_op_unary_fp *fclass = &instr->rv32i->fclass;
    fprintf(file, "fclass.s");
    debug_print_op_unary_fp(file, fclass);
    break;
  }
  case RV32I_INSTR_TY_FCLASS_D: {
    struct rv32i_op_unary_fp *fclass = &instr->rv32i->fclass;
    fprintf(file, "fclass.d");
    debug_print_op_unary_fp(file, fclass);
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

void rv32i_debug_print_codegen(FILE *file, struct codegen_unit *unit) {
  DEBUG_ASSERT(unit->ty == CODEGEN_UNIT_TY_RV32I, "expected rv32i");

  for (size_t i = 0; i < unit->num_entries; i++) {
    struct codegen_entry *entry = &unit->entries[i];

    if (entry->ty != CODEGEN_ENTRY_TY_FUNC) {
      continue;
    }

    struct codegen_function *func = &entry->func;

    fprintf(file, "\nFUNCTION: %s\n", entry->name);
    fprintf(file, "  prologue: %s\n", entry->func.prologue ? "true" : "false");
    fprintf(file, "  stack_size: %zu\n", entry->func.stack_size);
    fprintf(file, "\n");

    size_t offset = 0;
    struct instr *instr = func->first;
    while (instr) {
      fprintf(file, "%04zu: ", offset++);
      debug_print_instr(file, func, instr);
      fprintf(file, "\n");

      instr = instr->succ;
    }

    fprintf(file, "\n");
  }
}
