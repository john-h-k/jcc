#include "codegen.h"

#include "../alloc.h"
#include "../log.h"
#include "../vector.h"

#define MOV_ALIAS(dest_reg, source_reg)                                        \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_ADD, .add = {                                         \
      .lhs = (source_reg),                                                     \
      .rhs = zero_reg_for_ty(dest_reg.ty),                                     \
      .dest = (dest_reg),                                                      \
    }                                                                          \
  }

#define FP_MOV_ALIAS(dest_reg, source_reg)                                     \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_FSGNJ, .add = {                                       \
      .lhs = (source_reg),                                                     \
      .rhs = (source_reg),                                                     \
      .dest = (dest_reg),                                                      \
    }                                                                          \
  }

const char *rv32i_mangle(struct arena_allocator *arena,
                                const char *name) {
  return name;
}

static struct rv32i_reg zero_reg_for_ty(enum rv32i_reg_ty reg_ty) {
  return (struct rv32i_reg){.ty = reg_ty, .idx = 0};
}

struct rv32i_prologue_info {
  bool prologue_generated;
  size_t stack_size;
  size_t lr_offset;
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
    return RV32I_REG_TY_GP;
  case IR_VAR_PRIMITIVE_TY_I64:
    TODO("i64");
  case IR_VAR_PRIMITIVE_TY_F16:
  case IR_VAR_PRIMITIVE_TY_F32:
  case IR_VAR_PRIMITIVE_TY_F64:
    return RV32I_REG_TY_FP;
  }
}

static size_t translate_reg_idx(size_t idx, enum ir_reg_ty ty) {
  switch (ty) {
  case IR_REG_TY_NONE:
  case IR_REG_TY_SPILLED:
  case IR_REG_TY_FLAGS:
    BUG("does not make sense for none/spilled/flags");
  case IR_REG_TY_INTEGRAL:
    if (idx >= 17) {
      return 18 + idx;
    } else if (idx >= 14) {
      return 28 + (idx - 14);
    } else {
      return 5 + idx;
    }
  case IR_REG_TY_FP:
    if (idx >= 22) {
      return 18 + idx;
    } else if (idx >= 20) {
      return 8 + idx;
    } else if (idx >= 8) {
      return 2 + idx;
    } else {
      return 1 + idx;
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
  case RV32I_REG_TY_GP:
    invariant_assert(op->reg.ty == IR_REG_TY_INTEGRAL, "expected integral reg");
    break;
  case RV32I_REG_TY_FP:
    invariant_assert(op->reg.ty == IR_REG_TY_FP, "expected fp reg");
    break;
  default:
    TODO("other reg tys (Q/V)");
  }

  return (struct rv32i_reg){.ty = reg_ty, .idx = idx};
}

static bool rv32i_reg_ty_is_gp(enum rv32i_reg_ty ty) {
  return ty == RV32I_REG_TY_GP;
}

static void codegen_mov_op(struct codegen_state *state, struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg source = codegen_reg(op->mov.value);

  struct instr *instr = alloc_instr(state->func);
  if (rv32i_reg_ty_is_gp(source.ty) && rv32i_reg_ty_is_gp(dest.ty)) {
    *instr->rv32i = MOV_ALIAS(dest, source);
  } else {
    instr->rv32i->ty = RV32I_INSTR_TY_FMV;
    instr->rv32i->fmv = (struct rv32i_op_mov){.source = source, .dest = dest};
  }
}

static void codegen_br_cond_op(struct codegen_state *state, struct ir_op *op) {
  struct ir_basicblock *true_target = op->stmt->basicblock->split.true_target;
  struct ir_basicblock *false_target = op->stmt->basicblock->split.false_target;

  struct ir_op *cond = op->br_cond.cond;

  if (cond->ty == IR_OP_TY_BINARY_OP &&
      binary_op_is_comparison(cond->binary_op.ty)) {
    DEBUG_ASSERT(cond->flags & IR_OP_FLAG_CONTAINED,
                 "expected to be contained");

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
        .lhs = cmp_reg,
        .rhs = zero_reg_for_ty(RV32I_REG_TY_GP),
        .target = true_target};
  }

  // rv32i requires turning `br.cond <true> <false>` into 2 instructions
  // we represent this as just the `true` part of the `br.cond`, and then a
  // `br`
  // after branching to the false target

  // now generate the `br`
  struct instr *br = alloc_instr(state->func);
  br->rv32i->ty = RV32I_INSTR_TY_JAL;
  br->rv32i->jal = (struct rv32i_jal){
      .ret_addr = zero_reg_for_ty(RV32I_REG_TY_GP), .target = false_target};
}

static void codegen_br_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);
  instr->rv32i->ty = RV32I_INSTR_TY_JAL;
  instr->rv32i->jal =
      (struct rv32i_jal){.ret_addr = zero_reg_for_ty(RV32I_REG_TY_GP),
                         .target = op->stmt->basicblock->merge.target};
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

      unsigned long lo;
      unsigned long hi;
      if ((cnst >> 11) & 1) {
        lo = -(4096 - (cnst & 0xFFF));
        hi = (cnst >> 12) + 1;
      } else {
        lo = cnst & 0xFFF;
        hi = cnst >> 12;
      }

      struct rv32i_reg source_reg = zero_reg_for_ty(RV32I_REG_TY_GP);

      if (hi) {
        struct instr *instr = alloc_instr(state->func);
        instr->rv32i->ty = RV32I_INSTR_TY_LUI;
        instr->rv32i->lui = (struct rv32i_lui){.dest = dest, .imm = hi};

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

static void codegen_ret_op(struct codegen_state *state, struct ir_op *op) {
  if (op->ret.value && op->ret.value->ty != IR_OP_TY_CALL) {
    struct rv32i_reg source = codegen_reg(op->ret.value);

    if (source.idx != return_reg_for_ty(source.ty).idx) {
      struct instr *instr = alloc_instr(state->func);

      if (source.ty == RV32I_REG_TY_GP) {
        *instr->rv32i = MOV_ALIAS(return_reg_for_ty(source.ty), source);
      } else {
        *instr->rv32i = FP_MOV_ALIAS(return_reg_for_ty(source.ty), source);
      }
    }
  }

  // codegen_epilogue(state);

  struct instr *instr = alloc_instr(state->func);

  instr->rv32i->ty = RV32I_INSTR_TY_JALR;
  instr->rv32i->jalr =
      (struct rv32i_jalr){.ret_addr = zero_reg_for_ty(RV32I_REG_TY_GP),
                          .target = RET_PTR_REG,
                          .imm = 0};
}

static void codegen_unary_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg dest = codegen_reg(op);
  struct rv32i_reg source = codegen_reg(op->unary_op.value);

  switch (op->unary_op.ty) {
  case IR_OP_UNARY_OP_TY_FNEG:
    instr->rv32i->ty = RV32I_INSTR_TY_FSGNJN;
    instr->rv32i->fsgnjn = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = source,
        .rhs = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_FABS:
    instr->rv32i->ty = RV32I_INSTR_TY_FSGNJX;
    instr->rv32i->fsgnjx = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = source,
        .rhs = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_FSQRT:
    instr->rv32i->ty = RV32I_INSTR_TY_FSQRT;
    instr->rv32i->fsqrt =
        (struct rv32i_op_unary_fp){.dest = dest, .source = source};
    return;
  case IR_OP_UNARY_OP_TY_NEG:
    instr->rv32i->ty = RV32I_INSTR_TY_SUB;
    instr->rv32i->sub = (struct rv32i_op){
        .dest = dest,
        .lhs = zero_reg_for_ty(source.ty),
        .rhs = source,
    };
    return;
  case IR_OP_UNARY_OP_TY_NOT:
    instr->rv32i->ty = RV32I_INSTR_TY_XORI;
    instr->rv32i->xori =
        (struct rv32i_op_imm){.dest = dest, .source = source, .imm = -1};
    return;
  case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
    BUG("logical not should never reach emitter, should be converted in lower");
  }
}

static void codegen_binary_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg lhs = codegen_reg(op->binary_op.lhs);
  struct rv32i_reg rhs = codegen_reg(op->binary_op.rhs);

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
    // instr->rv32i->ty = rv32i_INSTR_TY_FCMP;
    // instr->rv32i->fcmp = (struct rv32i_fcmp){
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;

    TODO("float comp binops");
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
    // instr->rv32i->ty = rv32i_INSTR_TY_SUBS;
    // instr->rv32i->subs = (struct rv32i_addsub_reg){
    //     .dest = zero_reg_for_ty(lhs.ty),
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
    TODO("comp binops");
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
    instr->rv32i->ty = RV32I_INSTR_TY_FADD;
    instr->rv32i->fadd = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FSUB:
    instr->rv32i->ty = RV32I_INSTR_TY_FSUB;
    instr->rv32i->fsub = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMUL:
    instr->rv32i->ty = RV32I_INSTR_TY_FMUL;
    instr->rv32i->fmul = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FDIV:
    instr->rv32i->ty = RV32I_INSTR_TY_FDIV;
    instr->rv32i->fdiv = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMAX:
    instr->rv32i->ty = RV32I_INSTR_TY_FMAX;
    instr->rv32i->fmax = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_FMIN:
    instr->rv32i->ty = RV32I_INSTR_TY_FMIN;
    instr->rv32i->fmin = (struct rv32i_op_fp){
        .dest = dest,
        .lhs = lhs,
        .rhs = rhs,
    };
    break;
  }
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

static void codegen_add_imm(struct codegen_state *state, struct rv32i_reg dest,
                            struct rv32i_reg source, unsigned long long value) {
  struct instr *instr = alloc_instr(state->func);
  instr->rv32i->ty = RV32I_INSTR_TY_ADDI;
  instr->rv32i->addi = (struct rv32i_op_imm){
      .dest = dest,
      .source = source,
      .imm = MIN(value, 4095),
  };

  if (value > 4095) {
    value -= 4095;

    while (value) {
      unsigned long long imm = MIN(value, 4095);

      struct instr *add = alloc_instr(state->func);
      add->rv32i->ty = RV32I_INSTR_TY_ADDI;
      add->rv32i->addi = (struct rv32i_op_imm){
          .dest = dest,
          .source = dest,
          .imm = imm,
      };

      value -= imm;
    }
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
    TODO("rv32i global addr");
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
        (struct rv32i_op_imm){.source = source, .dest = dest, .imm = sh_sz};
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

static void codegen_conv_op(struct codegen_state *state,
                            struct rv32i_reg source, struct rv32i_reg dest) {
  TODO("rv32i conv");
}

static void codegen_uconv_op(struct codegen_state *state,
                             struct rv32i_reg source, struct rv32i_reg dest) {
  TODO("rv32i uconv");
}

static void codegen_sconv_op(struct codegen_state *state,
                             struct rv32i_reg source, struct rv32i_reg dest) {
  // todo("rv32i sconv");

  // temporarily just move bits
  struct instr *instr = alloc_instr(state->func);
  instr->rv32i->ty = RV32I_INSTR_TY_FMV;
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
  (void)param_tys;

  invariant_assert(func_ty->num_params <= 8,
                   "`%s` doesn't support more than 8 args yet", __func__);

  struct instr *instr = alloc_instr(state->func);
  if (op->call.target->flags & IR_OP_FLAG_CONTAINED) {
    instr->rv32i->ty = RV32I_INSTR_TY_JAL;
    instr->rv32i->jal = (struct rv32i_jal){.target = NULL};

    instr->reloc = arena_alloc(state->func->unit->arena, sizeof(*instr->reloc));
    *instr->reloc = (struct relocation){
        .ty = RELOCATION_TY_CALL,
        .symbol_index = op->call.target->addr.glb->id,
        .size = 2,
        .address = 0,
    };
  } else {
    // NOTE: `blr` seems to segfault on linux rv32i
    instr->rv32i->ty = RV32I_INSTR_TY_JALR;
    instr->rv32i->jalr = (struct rv32i_jalr){
        .target = codegen_reg(op->call.target) };
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
    if (!(op->flags & IR_OP_FLAG_CONTAINED)) {
      codegen_op(state, op);
    }

    op = op->succ;
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
          TODO("rv32i cnst data");
          // unit->entries[i] =
          //     (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_CONST_DATA,
          //                            .glb_id = glb->id,
          //                            .name = name,
          //                            .data = codegen_var_data(ir,
          //                            glb->var)};
          // break;
        case IR_VAR_TY_DATA:
          TODO("rv32i data");
          // unit->entries[i] =
          //     (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_DATA,
          //                            .glb_id = glb->id,
          //                            .name = name,
          //                            .data = codegen_var_data(ir,
          //                            glb->var)};
          // break;
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

        struct ir_basicblock *basicblock = ir_func->first;
        while (basicblock) {
          struct ir_stmt *stmt = basicblock->first;

          while (stmt) {
            struct ir_op *op = stmt->first;

            while (op) {
              if (op->ty == IR_OP_TY_CALL) {
                TODO("rv32i calls");
                // size_t call_args_size =
                //     calc_arg_stack_space(&state, op->call.func_ty.func,
                //     op);
                // state.stack_args_size =
                //     MAX(state.stack_args_size, call_args_size);
              }

              op = op->succ;
            }

            stmt = stmt->succ;
          }

          basicblock = basicblock->succ;
        }

        // codegen_prologue(&state);

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

static void debug_print_reg(FILE *file, const struct rv32i_reg *reg) {
  switch (reg->ty) {
  case RV32I_REG_TY_NONE:
    fprintf(file, "NONE");
    break;
  case RV32I_REG_TY_GP:
    fprintf(file, "x%zu", reg->idx);
    break;
  case RV32I_REG_TY_FP:
    fprintf(file, "f%zu", reg->idx);
    break;
  }
}

static void debug_print_op_imm(FILE *file, const struct rv32i_op_imm *op_imm) {
  fprintf(file, " x%zu, x%zu, %lld", op_imm->dest.idx, op_imm->source.idx,
          op_imm->imm);
}

static void debug_print_op_fp(FILE *file, const struct rv32i_op_fp *op_fp) {
  fprintf(file, " f%zu, f%zu, f%zu", op_fp->dest.idx, op_fp->lhs.idx,
          op_fp->rhs.idx);
}

static void
debug_print_op_unary_fp(FILE *file,
                        const struct rv32i_op_unary_fp *op_unary_fp) {
  fprintf(file, " f%zu, f%zu", op_unary_fp->dest.idx, op_unary_fp->source.idx);
}

static void debug_print_op(FILE *file, const struct rv32i_op *op) {
  fprintf(file, " x%zu, x%zu, x%zu", op->dest.idx, op->lhs.idx, op->rhs.idx);
}

static void debug_print_op_mov(FILE *file, const struct rv32i_op_mov *op_mov) {
  fprintf(file, " ");
  debug_print_reg(file, &op_mov->dest);
  fprintf(file, ", ");
  debug_print_reg(file, &op_mov->source);
}

static void debug_print_lui(FILE *file, const struct rv32i_lui *lui) {
  fprintf(file, " x%zu, %lld", lui->dest.idx, lui->imm);
}

static void debug_print_jalr(FILE *file, const struct rv32i_jalr *jalr) {
  fprintf(file, " x%zu, x%zu, %lld", jalr->ret_addr.idx, jalr->target.idx,
          jalr->imm);
}

static void debug_print_jal(FILE *file, const struct rv32i_jal *jal) {
  fprintf(file, " x%zu, %%%zu", jal->ret_addr.idx, jal->target->id);
}

static void debug_print_load(FILE *file, const struct rv32i_load *load) {
  fprintf(file, " ");
  debug_print_reg(file, &load->dest);
  if (load->imm) {
    fprintf(file, ", %lld(x%zu)", load->imm, load->addr.idx);
  } else {
    fprintf(file, ", x%zu", load->addr.idx);
  }
}

static void debug_print_store(FILE *file, const struct rv32i_store *store) {
  fprintf(file, " ");
  debug_print_reg(file, &store->source);
  if (store->imm) {
    fprintf(file, ", %lld(x%zu)", store->imm, store->addr.idx);
  } else {
    fprintf(file, ", x%zu", store->addr.idx);
  }
}

static void debug_print_conditional_branch(
    FILE *file, const struct rv32i_conditional_branch *conditional_branch) {
  fprintf(file, " x%zu, x%zu, %%%zu", conditional_branch->lhs.idx,
          conditional_branch->rhs.idx, conditional_branch->target->id);
}

static void debug_print_instr(FILE *file,
                              UNUSED_ARG(const struct codegen_function *func),
                              const struct instr *instr) {

  switch (instr->rv32i->ty) {
  case RV32I_INSTR_TY_ADDI:
    fprintf(file, "addi");
    debug_print_op_imm(file, &instr->rv32i->addi);
    break;
  case RV32I_INSTR_TY_XORI:
    fprintf(file, "xori");
    debug_print_op_imm(file, &instr->rv32i->addi);
    break;
  case RV32I_INSTR_TY_ADD:
    fprintf(file, "add");
    debug_print_op(file, &instr->rv32i->add);
    break;
  case RV32I_INSTR_TY_SUB:
    fprintf(file, "sub");
    debug_print_op(file, &instr->rv32i->sub);
    break;
  case RV32I_INSTR_TY_MUL:
    fprintf(file, "mul");
    debug_print_op(file, &instr->rv32i->mul);
    break;
  case RV32I_INSTR_TY_DIV:
    fprintf(file, "div");
    debug_print_op(file, &instr->rv32i->div);
    break;
  case RV32I_INSTR_TY_DIVU:
    fprintf(file, "divu");
    debug_print_op(file, &instr->rv32i->divu);
    break;
  case RV32I_INSTR_TY_REM:
    fprintf(file, "rem");
    debug_print_op(file, &instr->rv32i->rem);
    break;
  case RV32I_INSTR_TY_REMU:
    fprintf(file, "remu");
    debug_print_op(file, &instr->rv32i->remu);
    break;
  case RV32I_INSTR_TY_LUI:
    fprintf(file, "lui");
    debug_print_lui(file, &instr->rv32i->lui);
    break;
  case RV32I_INSTR_TY_JALR:
    fprintf(file, "jalr");
    debug_print_jalr(file, &instr->rv32i->jalr);
    break;
  case RV32I_INSTR_TY_SB:
    fprintf(file, "sb");
    debug_print_store(file, &instr->rv32i->sb);
    break;
  case RV32I_INSTR_TY_SH:
    fprintf(file, "sh");
    debug_print_store(file, &instr->rv32i->sh);
    break;
  case RV32I_INSTR_TY_SW:
    fprintf(file, "sw");
    debug_print_store(file, &instr->rv32i->sw);
    break;
  case RV32I_INSTR_TY_LB:
    fprintf(file, "lb");
    debug_print_load(file, &instr->rv32i->lb);
    break;
  case RV32I_INSTR_TY_LBU:
    fprintf(file, "lbu");
    debug_print_load(file, &instr->rv32i->lbu);
    break;
  case RV32I_INSTR_TY_LH:
    fprintf(file, "lh");
    debug_print_load(file, &instr->rv32i->lh);
    break;
  case RV32I_INSTR_TY_LHU:
    fprintf(file, "lhu");
    debug_print_load(file, &instr->rv32i->lhu);
    break;
  case RV32I_INSTR_TY_LW:
    fprintf(file, "lw");
    debug_print_load(file, &instr->rv32i->lw);
    break;
  case RV32I_INSTR_TY_FSW:
    fprintf(file, "fsw");
    debug_print_store(file, &instr->rv32i->fsw);
    break;
  case RV32I_INSTR_TY_FLW:
    fprintf(file, "flw");
    debug_print_load(file, &instr->rv32i->flw);
    break;
  case RV32I_INSTR_TY_JAL:
    fprintf(file, "jal");
    debug_print_jal(file, &instr->rv32i->jal);
    break;
  case RV32I_INSTR_TY_BEQ:
    fprintf(file, "beq");
    debug_print_conditional_branch(file, &instr->rv32i->beq);
    break;
  case RV32I_INSTR_TY_BNE:
    fprintf(file, "bne");
    debug_print_conditional_branch(file, &instr->rv32i->bne);
    break;
  case RV32I_INSTR_TY_BLT:
    fprintf(file, "blt");
    debug_print_conditional_branch(file, &instr->rv32i->blt);
    break;
  case RV32I_INSTR_TY_BGE:
    fprintf(file, "bge");
    debug_print_conditional_branch(file, &instr->rv32i->bge);
    break;
  case RV32I_INSTR_TY_BLTU:
    fprintf(file, "bltu");
    debug_print_conditional_branch(file, &instr->rv32i->bltu);
    break;
  case RV32I_INSTR_TY_BGEU:
    fprintf(file, "bgeu");
    debug_print_conditional_branch(file, &instr->rv32i->bgeu);
    break;
  case RV32I_INSTR_TY_OR:
    fprintf(file, "or");
    debug_print_op(file, &instr->rv32i->or);
    break;
  case RV32I_INSTR_TY_AND:
    fprintf(file, "and");
    debug_print_op(file, &instr->rv32i->and);
    break;
  case RV32I_INSTR_TY_XOR:
    fprintf(file, "xor");
    debug_print_op(file, &instr->rv32i->xor);
    break;
  case RV32I_INSTR_TY_SLL:
    fprintf(file, "sll");
    debug_print_op(file, &instr->rv32i->sll);
    break;
  case RV32I_INSTR_TY_SRL:
    fprintf(file, "srl");
    debug_print_op(file, &instr->rv32i->srl);
    break;
  case RV32I_INSTR_TY_SRA:
    fprintf(file, "sra");
    debug_print_op(file, &instr->rv32i->sra);
    break;
  case RV32I_INSTR_TY_FMV:
    switch (instr->rv32i->fmv.source.ty) {
    case RV32I_REG_TY_NONE:
      BUG("none dest");
    case RV32I_REG_TY_GP:
      fprintf(file, "fmv.w.x");
      break;
    case RV32I_REG_TY_FP:
      fprintf(file, "fmv.x.w");
      break;
    }

    debug_print_op_mov(file, &instr->rv32i->fmv);
    break;
  case RV32I_INSTR_TY_FADD:
    fprintf(file, "fadd");
    debug_print_op_fp(file, &instr->rv32i->fadd);
    break;
  case RV32I_INSTR_TY_FSUB:
    fprintf(file, "fsub");
    debug_print_op_fp(file, &instr->rv32i->fsub);
    break;
  case RV32I_INSTR_TY_FSGNJ:
    if (instr->rv32i->fsgnj.lhs.idx == instr->rv32i->fsgnj.rhs.idx) {
      fprintf(file, "fmv f%zu, f%zu", instr->rv32i->fsgnj.dest.idx,
              instr->rv32i->fsgnj.lhs.idx);
    } else {
      fprintf(file, "fsgnj");
      debug_print_op_fp(file, &instr->rv32i->fsgnj);
    }
    break;
  case RV32I_INSTR_TY_FMUL:
    fprintf(file, "fmul");
    debug_print_op_fp(file, &instr->rv32i->fmul);
    break;
  case RV32I_INSTR_TY_FDIV:
    fprintf(file, "fdiv");
    debug_print_op_fp(file, &instr->rv32i->fdiv);
    break;
  case RV32I_INSTR_TY_FSGNJN:
    fprintf(file, "fsgnjn");
    debug_print_op_fp(file, &instr->rv32i->fsgnjn);
    break;
  case RV32I_INSTR_TY_FSGNJX:
    fprintf(file, "fsgnjnx");
    debug_print_op_fp(file, &instr->rv32i->fsgnjx);
    break;
  case RV32I_INSTR_TY_FMAX:
    fprintf(file, "fmax");
    debug_print_op_fp(file, &instr->rv32i->fmax);
    break;
  case RV32I_INSTR_TY_FMIN:
    fprintf(file, "fmin");
    debug_print_op_fp(file, &instr->rv32i->fmin);
    break;
  case RV32I_INSTR_TY_FSQRT:
    fprintf(file, "fsqrt");
    debug_print_op_unary_fp(file, &instr->rv32i->fsqrt);
    break;
  case RV32I_INSTR_TY_ORI:
    fprintf(file, "ori");
    debug_print_op_imm(file, &instr->rv32i->ori);
    break;
  case RV32I_INSTR_TY_ANDI:
    fprintf(file, "andi");
    debug_print_op_imm(file, &instr->rv32i->andi);
    break;
  case RV32I_INSTR_TY_SLLI:
    fprintf(file, "slli");
    debug_print_op_imm(file, &instr->rv32i->addi);
    break;
  case RV32I_INSTR_TY_SRLI:
    fprintf(file, "srli");
    debug_print_op_imm(file, &instr->rv32i->srli);
    break;
  case RV32I_INSTR_TY_SRAI:
    fprintf(file, "srai");
    debug_print_op_imm(file, &instr->rv32i->srai);
    break;
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
