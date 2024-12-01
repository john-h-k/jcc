#include "codegen.h"

#include "../alloc.h"
#include "../log.h"
#include "../vector.h"

#define MOV_ALIAS(dest_reg, source_reg)                                        \
  (struct rv32i_instr) {                                                       \
    .ty = RV32I_INSTR_TY_ADD, .add = {                                         \
      .lhs = zero_reg_for_ty(dest_reg.ty),                                     \
      .rhs = (source_reg),                                                     \
      .dest = (dest_reg),                                                      \
    }                                                                          \
  }

static const char *rv32i_mangle(struct arena_allocator *arena,
                                const char *name) {
  char *dest =
      arena_alloc(arena, strlen(name) + /* null terminator + '_' char */ 2);

  dest[0] = '_';
  strcpy(dest + 1, name);

  return dest;
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
    todo("i64");
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
    bug("does not make sense for none/spilled/flags");
  case IR_REG_TY_INTEGRAL:
    if (idx >= 17) {
      return 18 + idx;
    } else if (idx >= 14) {
      return 28 + (idx - 14);
    } else {
      return 5 + idx;
    }
  case IR_REG_TY_FP:
    return idx >= 24 ? 8 + (idx - 24) : idx;
  }
}

static struct rv32i_reg codegen_reg(struct ir_op *op) {
  size_t idx = translate_reg_idx(op->reg.idx, op->reg.ty);

  if (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE) {
    todo("non primitives (op %zu)", op->id);
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
    todo("other reg tys (Q/V)");
  }

  return (struct rv32i_reg){.ty = reg_ty, .idx = idx};
}

static void codegen_mov_op(struct codegen_state *state, struct ir_op *op) {
  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg source = codegen_reg(op->mov.value);

  struct instr *instr = alloc_instr(state->func);
  // if (rv32i_reg_ty_is_gp(source.ty) && rv32i_reg_ty_is_gp(dest.ty)) {
  *instr->rv32i = MOV_ALIAS(dest, source);
  // } else {
  //   todo("");
  // }
}

static void codegen_br_cond_op(struct codegen_state *state, struct ir_op *op) {
  todo("cond br");
  // struct instr *instr = alloc_instr(state->func);

  // // rv32i requires turning `br.cond <true> <false>` into 2 instructions
  // // we represent this as just the `true` part of the `br.cond`, and then a
  // `br`
  // // after branching to the false target

  // struct ir_basicblock *true_target =
  // op->stmt->basicblock->split.true_target; struct ir_basicblock *false_target
  // = op->stmt->basicblock->split.false_target;

  // if (op->br_cond.cond->reg.ty == IR_REG_TY_FLAGS) {
  //   // emit based on flags
  //   enum rv32i_cond cond = get_cond_for_op(op->br_cond.cond);
  //   instr->rv32i->ty = rv32i_INSTR_TY_B_COND;
  //   instr->rv32i->b_cond = (struct rv32i_conditional_branch){
  //       .cond = cond, .target = true_target};
  // } else {
  //   struct rv32i_reg cmp_reg = codegen_reg(op->br_cond.cond);

  //   instr->rv32i->ty = rv32i_INSTR_TY_CBNZ;
  //   instr->rv32i->cbnz = (struct rv32i_compare_and_branch){
  //       .cmp = cmp_reg, .target = true_target};
  // }

  // // now generate the `br`
  // struct instr *br = alloc_instr(state->func);
  // br->rv32i->ty = rv32i_INSTR_TY_B;
  // br->rv32i->b = (struct rv32i_branch){.target = false_target};
}

static void codegen_br_op(struct codegen_state *state, struct ir_op *op) {
  todo("br");
  // struct instr *instr = alloc_instr(state->func);
}

static void codegen_cnst_op(struct codegen_state *state, struct ir_op *op) {
  debug_assert(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
               "expects primitive type");

  struct rv32i_reg dest = codegen_reg(op);

  switch (op->cnst.ty) {
  case IR_OP_CNST_TY_FLT:
    // currently all constants are lowered to an integer load and `fmov`
    // but lots of constants can be loaded directly, so do that here
    todo("simple float constants (not lowered)");
  case IR_OP_CNST_TY_INT:
    switch (op->var_ty.primitive) {
    case IR_VAR_PRIMITIVE_TY_I8:
    case IR_VAR_PRIMITIVE_TY_I16:
    case IR_VAR_PRIMITIVE_TY_I32: {
      unsigned long long cnst = op->cnst.int_value;
      if (cnst >> 12) {
        struct instr *instr = alloc_instr(state->func);
        instr->rv32i->ty = RV32I_INSTR_TY_LUI;
        instr->rv32i->lui = (struct rv32i_lui){.dest = dest, .imm = cnst >> 12};
      }

      struct instr *instr = alloc_instr(state->func);
      instr->rv32i->ty = RV32I_INSTR_TY_ADDI;
      instr->rv32i->addi =
          (struct rv32i_op_imm){.dest = dest,
                                .source = zero_reg_for_ty(dest.ty),
                                .imm = cnst & ((1ul << 12) - 1)};

      break;
    }
    case IR_VAR_PRIMITIVE_TY_I64:
      todo("i64");
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
      *instr->rv32i = MOV_ALIAS(return_reg_for_ty(source.ty), source);
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

static void codegen_binary_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg dest = codegen_reg(op);

  struct rv32i_reg lhs = codegen_reg(op->binary_op.lhs);
  struct rv32i_reg rhs = codegen_reg(op->binary_op.rhs);

  bool is_fp = var_ty_is_fp(&op->var_ty);

  enum ir_op_binary_op_ty ty = op->binary_op.ty;
  debug_assert(ty == IR_OP_BINARY_OP_TY_FADD || ty == IR_OP_BINARY_OP_TY_FSUB ||
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
  case IR_OP_BINARY_OP_TY_LSHIFT:
    // instr->rv32i->ty = rv32i_INSTR_TY_LSLV;
    // instr->rv32i->lslv = (struct rv32i_reg_2_source){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
  case IR_OP_BINARY_OP_TY_SRSHIFT:
    // instr->rv32i->ty = rv32i_INSTR_TY_ASRV;
    // instr->rv32i->asrv = (struct rv32i_reg_2_source){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
  case IR_OP_BINARY_OP_TY_URSHIFT:
    // instr->rv32i->ty = rv32i_INSTR_TY_LSRV;
    // instr->rv32i->lsrv = (struct rv32i_reg_2_source){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
  case IR_OP_BINARY_OP_TY_AND:
    // instr->rv32i->ty = rv32i_INSTR_TY_AND;
    // instr->rv32i->and = (struct rv32i_logical_reg){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
  case IR_OP_BINARY_OP_TY_OR:
    // instr->rv32i->ty = rv32i_INSTR_TY_ORR;
    // instr->rv32i->orr = (struct rv32i_logical_reg){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
  case IR_OP_BINARY_OP_TY_XOR:
    // instr->rv32i->ty = rv32i_INSTR_TY_EOR;
    // instr->rv32i->eor = (struct rv32i_logical_reg){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
    todo("other binops");
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
    // instr->rv32i->ty = rv32i_INSTR_TY_FADD;
    // instr->rv32i->fadd = (struct rv32i_reg_2_source){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
  case IR_OP_BINARY_OP_TY_FSUB:
    // instr->rv32i->ty = rv32i_INSTR_TY_FSUB;
    // instr->rv32i->fsub = (struct rv32i_reg_2_source){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
  case IR_OP_BINARY_OP_TY_FMUL:
    // instr->rv32i->ty = rv32i_INSTR_TY_FMUL;
    // instr->rv32i->fmul = (struct rv32i_reg_2_source){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
  case IR_OP_BINARY_OP_TY_FDIV:
    // instr->rv32i->ty = rv32i_INSTR_TY_FDIV;
    // instr->rv32i->fdiv = (struct rv32i_reg_2_source){
    //     .dest = dest,
    //     .lhs = lhs,
    //     .rhs = rhs,
    // };
    // break;
    todo("other binops");
  }
}

static enum rv32i_instr_ty load_ty_for_op(struct ir_op *op) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
    return RV32I_INSTR_TY_LBU;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return RV32I_INSTR_TY_LHU;
  } else {
    return RV32I_INSTR_TY_LW;
  }
}

static enum rv32i_instr_ty store_ty_for_op(struct ir_op *op) {
  if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
      op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
    return RV32I_INSTR_TY_SB;
  } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
             op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
    return RV32I_INSTR_TY_SH;
  } else {
    return RV32I_INSTR_TY_SW;
  }
}


static void codegen_load_addr_op(struct codegen_state *state,
                                 struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg dest = codegen_reg(op);

  if (op->load_addr.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr = op->load_addr.addr;

    simm_t imm;
    if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
      imm = get_lcl_stack_offset(state, op, addr->addr.lcl);
    } else {
      bug("can't CONTAIN operand in load_addr node");
    }

    instr->rv32i->ty = load_ty_for_op(op);
    instr->rv32i->load =
        (struct rv32i_load){.dest = dest,
                                  .addr = STACK_PTR_REG,
                                  .imm = imm};
  } else {
    struct rv32i_reg addr = codegen_reg(op->load_addr.addr);
    instr->rv32i->ty = load_ty_for_op(op);
    instr->rv32i->load =
        (struct rv32i_load){.dest = dest,
                                  .addr = addr,
                                  .imm = 0};
  }
}

static void codegen_load_lcl_op(struct codegen_state *state, struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg dest = codegen_reg(op);
  struct ir_lcl *lcl = op->load_lcl.lcl;

  simm_t offset = get_lcl_stack_offset(state, op, lcl);

  instr->rv32i->ty = load_ty_for_op(op);
  instr->rv32i->load =
      (struct rv32i_load){.dest = dest,
                                .addr = STACK_PTR_REG,
                                .imm = offset};
}

static void codegen_store_lcl_op(struct codegen_state *state,
                                 struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg source = codegen_reg(op->store_lcl.value);
  struct ir_lcl *lcl = op->lcl;

  instr->rv32i->ty = store_ty_for_op(op->store_addr.value);
  instr->rv32i->store = (struct rv32i_store){
      .source = source,
      .addr = STACK_PTR_REG,
      .imm = get_lcl_stack_offset(state, op->store_lcl.value, lcl)};
}


static void codegen_store_addr_op(struct codegen_state *state,
                                  struct ir_op *op) {
  struct instr *instr = alloc_instr(state->func);

  struct rv32i_reg source = codegen_reg(op->store_addr.value);

  if (op->store_addr.addr->flags & IR_OP_FLAG_CONTAINED) {
    struct ir_op *addr = op->store_addr.addr;

    simm_t imm;
    if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
      imm = get_lcl_stack_offset(state, op->store_addr.value, addr->addr.lcl);
    } else {
      bug("can't CONTAIN operand in store_addr node");
    }

    instr->rv32i->ty = store_ty_for_op(op->store_addr.value);
    instr->rv32i->store =
        (struct rv32i_store){.source = source,
                                   .addr = STACK_PTR_REG,
                                   .imm = imm};
  } else {
    struct rv32i_reg addr = codegen_reg(op->store_addr.addr);
    instr->rv32i->ty = store_ty_for_op(op->store_addr.value);
    instr->rv32i->store =
        (struct rv32i_store){.source = source,
                                   .addr = addr,
                                   .imm = 0};
  }
}

static void codegen_add_imm(struct codegen_state *state,
                            struct rv32i_reg dest, struct rv32i_reg source,
                            unsigned long long value) {
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
      todo("rv32i global addr");
    }
  }
}

static void codegen_op(struct codegen_state *state, struct ir_op *op) {
  trace("lowering op with id %zu, type %d", op->id, op->ty);
  switch (op->ty) {
  case IR_OP_TY_UNDF:
  case IR_OP_TY_PHI:
    break;
  case IR_OP_TY_CUSTOM: {
    bug("custom");
  }
  case IR_OP_TY_MOV: {
    if (op->flags & IR_OP_FLAG_PARAM) {
      // don't need to do anything
    } else {
      codegen_mov_op(state, op);
    }
    break;
  }
  case IR_OP_TY_LOAD_GLB:
  case IR_OP_TY_STORE_GLB: {
    bug("load/store glb should have been lowered");
  }
  case IR_OP_TY_LOAD_LCL: {
    codegen_load_lcl_op(state, op);
    break;
  }
  case IR_OP_TY_STORE_LCL: {
    codegen_store_lcl_op(state, op);
    break;
  }
  case IR_OP_TY_LOAD_ADDR: {
    codegen_load_addr_op(state, op);
    break;
  }
  case IR_OP_TY_STORE_ADDR: {
    codegen_store_addr_op(state, op);
    break;
  }
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
    todo("");
    // codegen_unary_op(state, op);
    // break;
  }
  case IR_OP_TY_BINARY_OP: {
    codegen_binary_op(state, op);
    break;
  }
  case IR_OP_TY_CAST_OP: {
    todo("");
    // codegen_cast_op(state, op);
    // break;
  }
  case IR_OP_TY_CALL: {
    todo("");
    // codegen_call_op(state, op);
    // break;
  }
  case IR_OP_TY_RET: {
    codegen_ret_op(state, op);
    break;
  }
  default: {
    todo("unsupported IR OP '%d'", op->ty);
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
          todo("");
          // unit->entries[i] =
          //     (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_CONST_DATA,
          //                            .glb_id = glb->id,
          //                            .name = name,
          //                            .data = codegen_var_data(ir, glb->var)};
          // break;
        case IR_VAR_TY_DATA:
          todo("");
          // unit->entries[i] =
          //     (struct codegen_entry){.ty = CODEGEN_ENTRY_TY_DATA,
          //                            .glb_id = glb->id,
          //                            .name = name,
          //                            .data = codegen_var_data(ir, glb->var)};
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
                todo("");
                // size_t call_args_size =
                //     calc_arg_stack_space(&state, op->call.func_ty.func, op);
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
        // codegen_params(&state);

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

static void debug_print_op_imm(FILE *file, const struct rv32i_op_imm *op_imm) {
  fprintf(file, " x%zu, x%zu, %llu", op_imm->dest.idx, op_imm->source.idx,
          op_imm->imm);
}

static void debug_print_op(FILE *file, const struct rv32i_op *op) {
  fprintf(file, " x%zu, x%zu, x%zu", op->dest.idx, op->lhs.idx, op->rhs.idx);
}

static void debug_print_lui(FILE *file, const struct rv32i_lui *lui) {
  fprintf(file, " x%zu, %llu", lui->dest.idx, lui->imm);
}

static void debug_print_jalr(FILE *file, const struct rv32i_jalr *jalr) {
  fprintf(file, " x%zu, x%zu, %llu", jalr->ret_addr.idx, jalr->target.idx,
          jalr->imm);
}

static void debug_print_load(FILE *file, const struct rv32i_load *load) {
  if (load->imm) {
    fprintf(file, " x%zu, %llu(x%zu)", load->dest.idx, load->imm,
            load->addr.idx);
  } else {
    fprintf(file, " x%zu, x%zu", load->dest.idx, load->addr.idx);
  }
}

static void debug_print_store(FILE *file, const struct rv32i_store *store) {
  if (store->imm) {
    fprintf(file, " x%zu, %llu(x%zu)", store->source.idx, store->imm,
            store->addr.idx);
  } else {
    fprintf(file, " x%zu, x%zu", store->source.idx, store->addr.idx);
  }
}

static void debug_print_instr(FILE *file,
                              UNUSED_ARG(const struct codegen_function *func),
                              const struct instr *instr) {

  switch (instr->rv32i->ty) {
  case RV32I_INSTR_TY_ADDI:
    fprintf(file, "addi");
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
    fprintf(file, "lui x%zu, %llu", instr->rv32i->lui.dest.idx,
            instr->rv32i->lui.imm);
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
  }
}

void rv32i_debug_print_codegen(FILE *file, struct codegen_unit *unit) {
  debug_assert(unit->ty == CODEGEN_UNIT_TY_RV32I, "expected rv32i");

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
