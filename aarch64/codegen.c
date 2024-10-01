#include "codegen.h"

#include "../util.h"
#include "isa.h"

#include <stdio.h>
#include <sys/stat.h>

// static enum aarch64_cond invert_cond(enum aarch64_cond cond) {
//   return cond ^ 1;
// }

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

static unsigned get_lcl_stack_offset(const struct codegen_function *func,
                                     const struct ir_lcl *lcl) {
  // FIXME: wrongly assumes everything is 8 byte
  return func->max_variadic_args * 8 + lcl->offset;
}

// static unsigned get_lcl_stack_offset_32(const struct codegen_function *func,
// const struct ir_lcl *lcl) {
//   unsigned abs_offset = get_lcl_stack_offset(func, lcl);
//   debug_assert(abs_offset % 4 == 0, "stack offset not divisible by 4");
//   return abs_offset / 4;
// }

// static unsigned get_lcl_stack_offset_64(const struct codegen_function *func,
// const struct ir_lcl *lcl) {
//   unsigned abs_offset = get_lcl_stack_offset(func, lcl);
//   debug_assert(abs_offset % 8 == 0, "stack offset not divisible by 8");
//   return abs_offset / 8;
// }

static struct aarch64_reg codegen_reg(struct ir_op *op) {
  if (op->var_ty.ty != IR_OP_VAR_TY_TY_PRIMITIVE) {
    todo("non primitives");
  }

  switch (op->var_ty.primitive) {
  case IR_OP_VAR_PRIMITIVE_TY_I8:
  case IR_OP_VAR_PRIMITIVE_TY_I16:
  case IR_OP_VAR_PRIMITIVE_TY_I32:
    invariant_assert(op->reg.ty == IR_REG_TY_INTEGRAL, "expected integral reg");
    return (struct aarch64_reg){.ty = AARCH64_REG_TY_W, .idx = op->reg.idx};
  case IR_OP_VAR_PRIMITIVE_TY_I64:
    invariant_assert(op->reg.ty == IR_REG_TY_INTEGRAL, "expected integral reg");
    return (struct aarch64_reg){.ty = AARCH64_REG_TY_X, .idx = op->reg.idx};
  case IR_OP_VAR_PRIMITIVE_TY_F32:
    invariant_assert(op->reg.ty == IR_REG_TY_FP, "expected fp reg");
    return (struct aarch64_reg){.ty = AARCH64_REG_TY_S, .idx = op->reg.idx};
  case IR_OP_VAR_PRIMITIVE_TY_F64:
    invariant_assert(op->reg.ty == IR_REG_TY_FP, "expected fp reg");
    return (struct aarch64_reg){.ty = AARCH64_REG_TY_D, .idx = op->reg.idx};
  }
}

static void codegen_mov_op(struct codegen_function *func, struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg source = codegen_reg(op->mov.value);

  instr->aarch64->ty = AARCH64_INSTR_TY_ORR;
  instr->aarch64->orr = (struct aarch64_logical_reg){
      .dest = dest, .lhs = source, .rhs = ZERO_REG};
}

static void codegen_load_lcl_op(struct codegen_function *func,
                                struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg dest = codegen_reg(op);
  struct ir_lcl *lcl = op->load_lcl.lcl;

  instr->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
  instr->aarch64->ldr_imm =
      (struct aarch64_load_imm){.dest = dest,
                                .addr = STACK_PTR_REG,
                                .imm = get_lcl_stack_offset(func, lcl),
                                .mode = AARCH64_ADDRESSING_MODE_OFFSET};
}

static void codegen_store_lcl_op(struct codegen_function *func,
                                 struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg source = codegen_reg(op->store_lcl.value);
  struct ir_lcl *lcl = op->lcl;

  instr->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
  instr->aarch64->str_imm =
      (struct aarch64_store_imm){.source = source,
                                 .addr = STACK_PTR_REG,
                                 .imm = get_lcl_stack_offset(func, lcl),
                                 .mode = AARCH64_ADDRESSING_MODE_OFFSET};
}

static void codegen_load_addr_op(struct codegen_function *func,
                                 struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg addr = codegen_reg(op->load_addr.addr);

  instr->aarch64->ty = AARCH64_INSTR_TY_LOAD_IMM;
  instr->aarch64->ldr_imm =
      (struct aarch64_load_imm){.dest = dest,
                                .addr = addr,
                                .imm = 0,
                                .mode = AARCH64_ADDRESSING_MODE_OFFSET};
}

static void codegen_store_addr_op(struct codegen_function *func,
                                  struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg source = codegen_reg(op->store_addr.value);
  struct aarch64_reg addr = codegen_reg(op->store_addr.addr);

  instr->aarch64->ty = AARCH64_INSTR_TY_STORE_IMM;
  instr->aarch64->str_imm =
      (struct aarch64_store_imm){.source = source,
                                 .addr = addr,
                                 .imm = 0,
                                 .mode = AARCH64_ADDRESSING_MODE_OFFSET};
}

static void codegen_addr_op(struct codegen_function *func, struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg dest = codegen_reg(op);

  switch (op->addr.ty) {
  case IR_OP_ADDR_TY_LCL: {
    struct ir_lcl *lcl = op->addr.lcl;
    size_t offset = get_lcl_stack_offset(func, lcl);

    instr->aarch64->ty = AARCH64_INSTR_TY_ADD_IMM;
    instr->aarch64->add_imm = (struct aarch64_addsub_imm){
        .dest = dest,
        .source = STACK_PTR_REG,
        .imm = offset,
        .shift = 0,
    };
    break;
  }
  }
}

static void codegen_br_cond_op(struct codegen_function *func,
                               struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

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
  struct instr *br = alloc_instr(func);
  br->aarch64->ty = AARCH64_INSTR_TY_B;
  br->aarch64->b = (struct aarch64_branch){.target = false_target};
}

static void codegen_br_op(struct codegen_function *func, struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  instr->aarch64->ty = AARCH64_INSTR_TY_B;
  instr->aarch64->b =
      (struct aarch64_branch){.target = op->stmt->basicblock->merge.target};
}

static void codegen_cnst_op(struct codegen_function *func, struct ir_op *op) {
  debug_assert(op->var_ty.ty == IR_OP_VAR_TY_TY_PRIMITIVE,
               "expects primitive type");

  struct instr *instr = alloc_instr(func);

  switch (op->var_ty.primitive) {
  case IR_OP_VAR_PRIMITIVE_TY_I8:
  case IR_OP_VAR_PRIMITIVE_TY_I16:
  case IR_OP_VAR_PRIMITIVE_TY_I32:
  case IR_OP_VAR_PRIMITIVE_TY_I64:
    instr->aarch64->ty = AARCH64_INSTR_TY_MOV_IMM;
    instr->aarch64->mov_imm = (struct aarch64_mov_imm){
        .dest = codegen_reg(op), .imm = op->cnst.int_value};
    return;
  case IR_OP_VAR_PRIMITIVE_TY_F32:
  case IR_OP_VAR_PRIMITIVE_TY_F64:
    todo("float cnsts");
  };
}

static void codegen_unary_op(struct codegen_function *func, struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg source = codegen_reg(op->unary_op.value);

  switch (op->unary_op.ty) {
  case IR_OP_UNARY_OP_TY_NEG:
    instr->aarch64->ty = AARCH64_INSTR_TY_SUB;
    instr->aarch64->sub = (struct aarch64_addsub_reg){
        .dest = dest,
        .lhs = ZERO_REG,
        .rhs = source,
        .shift = 0,
        .imm6 = 0,
    };
    return;
  case IR_OP_UNARY_OP_TY_NOT:
    instr->aarch64->ty = AARCH64_INSTR_TY_MVN;
    instr->aarch64->mvn = (struct aarch64_reg_1_source_with_shift){
        .dest = dest,
        .source = source,
        .shift = 0,
        .imm6 = 0,
    };
    return;
  case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
    bug("logical not should never reach emitter, should be converted in lower");
  }
}

static void codegen_binary_op(struct codegen_function *func, struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg lhs = codegen_reg(op->binary_op.lhs);
  struct aarch64_reg rhs = codegen_reg(op->binary_op.rhs);

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
    instr->aarch64->ty = AARCH64_INSTR_TY_SUBS;
    instr->aarch64->subs = (struct aarch64_addsub_reg){
        .dest = ZERO_REG,
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
    instr->aarch64->madd = (struct aarch64_fma){
        .dest = dest, .lhs = lhs, .rhs = rhs, .addsub = ZERO_REG};
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
  case IR_OP_BINARY_OP_TY_SQUOT:
  case IR_OP_BINARY_OP_TY_UQUOT:
    bug("squot/uquot shoud have been lowered");
  }
}

static void codegen_cast_op(struct codegen_function *func, struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  struct aarch64_reg dest = codegen_reg(op);
  struct aarch64_reg source = codegen_reg(op->cast_op.value);

  switch (op->cast_op.ty) {
  case IR_OP_CAST_OP_TY_SEXT:
    invariant_assert(op->cast_op.value->var_ty.ty == IR_OP_VAR_TY_TY_PRIMITIVE,
                     "can't sext from non-primitive");

    switch (op->cast_op.value->var_ty.primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      instr->aarch64->ty = AARCH64_INSTR_TY_SBFM_IMM;
      instr->aarch64->sbfm = (struct aarch64_bitfield_imm){
          .dest = dest, .source = source, .immr = 0b000000, .imms = 0b000111};
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      instr->aarch64->ty = AARCH64_INSTR_TY_SBFM_IMM;
      instr->aarch64->sbfm = (struct aarch64_bitfield_imm){
          .dest = dest, .source = source, .immr = 0b000000, .imms = 0b001111};
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      instr->aarch64->ty = AARCH64_INSTR_TY_SBFM_IMM;
      instr->aarch64->sbfm = (struct aarch64_bitfield_imm){
          .dest = dest, .source = source, .immr = 0b000000, .imms = 0b011111};
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      bug("can't sext from I64");
    case IR_OP_VAR_PRIMITIVE_TY_F32:
    case IR_OP_VAR_PRIMITIVE_TY_F64:
      bug("todo cast floats");
    }
    break;
  case IR_OP_CAST_OP_TY_ZEXT:
    // `mov`/`orr` with 32 bit operands zeroes top 32 bits
    instr->aarch64->ty = AARCH64_INSTR_TY_ORR;
    instr->aarch64->orr = (struct aarch64_logical_reg){
        .dest = dest, .lhs = source, .rhs = ZERO_REG, .imm6 = 0, .shift = 0};
    break;
  case IR_OP_CAST_OP_TY_TRUNCATE:
    invariant_assert(op->var_ty.ty == IR_OP_VAR_TY_TY_PRIMITIVE,
                     "can't truncate non-primitive");

    // https://kddnewton.com/2022/08/11/aarch64-bitmask-immediates.html
    // for understanding the immediates
    switch (op->var_ty.primitive) {
    case IR_OP_VAR_PRIMITIVE_TY_I8:
      instr->aarch64->ty = AARCH64_INSTR_TY_AND_IMM;
      instr->aarch64->and_imm = (struct aarch64_logical_imm){
          .dest = dest,
          .source = source,
          .immr = 0b0,
          .imms = 0b111,
      };
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I16:
      instr->aarch64->ty = AARCH64_INSTR_TY_AND_IMM;
      instr->aarch64->and_imm = (struct aarch64_logical_imm){
          .dest = dest,
          .source = source,
          .immr = 0b0,
          .imms = 0b1111,
      };
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I32:
      instr->aarch64->ty = AARCH64_INSTR_TY_ORR;
      instr->aarch64->orr = (struct aarch64_logical_reg){
          .dest = dest, .lhs = source, .rhs = ZERO_REG, .imm6 = 0, .shift = 0};
      break;
    case IR_OP_VAR_PRIMITIVE_TY_I64:
      break;
    case IR_OP_VAR_PRIMITIVE_TY_F32:
    case IR_OP_VAR_PRIMITIVE_TY_F64:
      bug("todo cast floats");
    }
  }
}

static void codegen_call_op(struct codegen_function *func, struct ir_op *op) {
  todo(__func__);
}

static void codegen_ret_op(struct codegen_function *func, struct ir_op *op) {
  struct instr *instr = alloc_instr(func);

  instr->aarch64->ty = AARCH64_INSTR_TY_RET;
  instr->aarch64->ret = (struct aarch64_ret){.target = RET_PTR_REG};
}

static void codegen_op(struct codegen_function *func, struct ir_op *op) {
  trace("lowering op with id %d, type %d", op->id, op->ty);
  switch (op->ty) {
  case IR_OP_TY_UNDF:
  case IR_OP_TY_PHI:
    break;
  case IR_OP_TY_CUSTOM: {
    bug("custom");
    break;
  }
  case IR_OP_TY_MOV: {
    if (op->flags & IR_OP_FLAG_PARAM) {
      // don't need to do anything
    } else {
      codegen_mov_op(func, op);
    }
    break;
  }
  case IR_OP_TY_LOAD_LCL: {
    codegen_load_lcl_op(func, op);
    break;
  }
  case IR_OP_TY_STORE_LCL: {
    codegen_store_lcl_op(func, op);
    break;
  }
  case IR_OP_TY_LOAD_ADDR: {
    codegen_load_addr_op(func, op);
    break;
  }
  case IR_OP_TY_STORE_ADDR: {
    codegen_store_addr_op(func, op);
    break;
  }
  case IR_OP_TY_ADDR: {
    codegen_addr_op(func, op);
    break;
  }

  case IR_OP_TY_BR_COND: {
    codegen_br_cond_op(func, op);
    break;
  }
  case IR_OP_TY_BR: {
    codegen_br_op(func, op);
    break;
  }
  case IR_OP_TY_CNST: {
    codegen_cnst_op(func, op);
    break;
  }
  case IR_OP_TY_UNARY_OP: {
    codegen_unary_op(func, op);
    break;
  }
  case IR_OP_TY_BINARY_OP: {
    codegen_binary_op(func, op);
    break;
  }
  case IR_OP_TY_CAST_OP: {
    codegen_cast_op(func, op);
    break;
  }
  case IR_OP_TY_GLB_REF: {
    break;
  }
  case IR_OP_TY_CALL: {
    codegen_call_op(func, op);
    break;
  }
  case IR_OP_TY_RET: {
    codegen_ret_op(func, op);
    break;
  }
  default: {
    todo("unsupported IR OP '%zu'", op->ty);
    break;
  }
  }
}

// static void codegen_nop(struct codegen_function *func) {
//   struct instr *instr = alloc_instr(func);

//   instr->aarch64->ty = AARCH64_INSTR_TY_NOP;
// }

static void codegen_stmt(struct codegen_function *func,
                         const struct ir_stmt *stmt) {
  struct ir_op *op = stmt->first;
  while (op) {
    codegen_op(func, op);

    op = op->succ;
  }
}

struct codegen_function *aarch64_codegen(struct ir_builder *ir) {
  struct codegen_function *func = arena_alloc(ir->arena, sizeof(*func));
  *func = (struct codegen_function){.ty = CODEGEN_FUNCTION_TY_AARCH64,
                                    .name = ir->name,
                                    .first = NULL,
                                    .last = NULL,
                                    .instr_count = 0,
                                    .instr_size = sizeof(struct aarch64_instr),
                                    .max_variadic_args = 0};

  arena_allocator_create(&func->arena);

  clear_metadata(ir);

  struct ir_basicblock *basicblock = ir->first;
  while (basicblock) {
    struct instr *first_pred = func->last;

    struct ir_stmt *stmt = basicblock->first;

    while (stmt) {
      codegen_stmt(func, stmt);

      stmt = stmt->succ;
    }

    basicblock->first_instr = first_pred ? first_pred->succ : func->first;
    basicblock->last_instr = func->last;

    basicblock = basicblock->succ;
  }

  return func;
}

char reg_prefix(struct aarch64_reg reg) {
  switch (reg.ty) {
  case AARCH64_REG_TY_W:
    return 'w';
  case AARCH64_REG_TY_X:
    return 'w';
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

void codegen_fprintf(FILE *file, const char *format, ...) {
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
      size_t imm = va_arg(list, size_t);

      char prefix = reg_prefix(reg);

      if (!imm) {
        fprintf(file, "[%c%zu]", prefix, reg.idx);
      } else {
        switch (addressing_mode) {
        case AARCH64_ADDRESSING_MODE_OFFSET:
          fprintf(file, "[%c%zu, #%zu]", prefix, reg.idx, imm);
          break;
        case AARCH64_ADDRESSING_MODE_PREINDEX:
          fprintf(file, "[%c%zu, #%zu]!", prefix, reg.idx, imm);
          break;
        case AARCH64_ADDRESSING_MODE_POSTINDEX:
          fprintf(file, "[%c%zu], #%zu", prefix, reg.idx, imm);
          break;
        }
      }

      format += 8;
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
        todo("logical immediates");
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
        fprintf(file, "%%%zu", instr->id);
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
      size_t imm = va_arg(list, size_t);
      fprintf(file, "#%zu", imm);

      format += 3;
    } else if (format[0] == '%') {
      fputc('%', file);
      format++;
    } else {
      bug("unrecognised format starting '%%%s'", format);
    }
  }
}

void debug_print_logical_reg(FILE *file,
                             const struct aarch64_logical_reg *logical_reg) {
  codegen_fprintf(file, " %reg, %reg, %reg%shift_imm", logical_reg->dest,
                  logical_reg->lhs, logical_reg->rhs, logical_reg->shift,
                  logical_reg->imm6);
}

void debug_print_logical_imm(FILE *file,
                             const struct aarch64_logical_imm *logical_imm) {
  codegen_fprintf(file, " %reg, %reg, %log_imm", logical_imm->dest,
                  logical_imm->source, logical_imm->n, logical_imm->immr,
                  logical_imm->imms);
}

void debug_print_addr_imm(FILE *file, const struct aarch64_addr_imm *addr_imm) {
  codegen_fprintf(file, " %reg, %imm", addr_imm->dest, addr_imm->imm);
}

void debug_print_addsub_reg(FILE *file,
                            const struct aarch64_addsub_reg *addsub_reg) {
  codegen_fprintf(file, " %reg, %reg, %reg%shift_imm", addsub_reg->dest,
                  addsub_reg->lhs, addsub_reg->rhs, addsub_reg->shift,
                  addsub_reg->imm6);
}

void debug_print_addsub_imm(FILE *file,
                            const struct aarch64_addsub_imm *addsub_imm) {
  if (addsub_imm->shift) {
    codegen_fprintf(file, " %reg, %reg, %imm, lsl %imm", addsub_imm->dest,
                    addsub_imm->source, addsub_imm->imm, addsub_imm->shift);
  } else {
    codegen_fprintf(file, " %reg, %reg, %imm", addsub_imm->dest,
                    addsub_imm->source, addsub_imm->imm);
  }
}

void debug_print_reg_1_source(FILE *file,
                              const struct aarch64_reg_1_source *reg_1_source) {
  codegen_fprintf(file, " %reg, %reg", reg_1_source->dest,
                  reg_1_source->source);
}

void debug_print_reg_1_source_with_shift(
    FILE *file,
    const struct aarch64_reg_1_source_with_shift *reg_1_source_with_shift) {
  codegen_fprintf(file, " %reg, %reg%shift_imm", reg_1_source_with_shift->dest,
                  reg_1_source_with_shift->source,
                  reg_1_source_with_shift->shift,
                  reg_1_source_with_shift->imm6);
}

void debug_print_reg_2_source(FILE *file,
                              const struct aarch64_reg_2_source *reg_2_source) {
  codegen_fprintf(file, " %reg, %reg, %reg", reg_2_source->dest,
                  reg_2_source->lhs, reg_2_source->rhs);
}

void debug_print_bitfield_imm(FILE *file,
                              const struct aarch64_bitfield_imm *bitfield_imm) {
  codegen_fprintf(file, " %reg, %reg, %imm, %imm", bitfield_imm->dest,
                  bitfield_imm->source, bitfield_imm->immr, bitfield_imm->imms);
}

void debug_print_conditional_select(
    FILE *file, const struct aarch64_conditional_select *conditional_select) {
  codegen_fprintf(file, "% reg, %reg, %reg, %cond", conditional_select->dest,
                  conditional_select->false_source,
                  conditional_select->true_source, conditional_select->cond);
}

void debug_print_conditional_branch(
    FILE *file, const struct aarch64_conditional_branch *conditional_branch) {
  codegen_fprintf(file, ".%cond %instr", conditional_branch->cond,
                  conditional_branch->target->first_instr);
}

void debug_print_branch(FILE *file, const struct aarch64_branch *branch) {
  codegen_fprintf(file, " %instr", branch->target->first_instr);
}

void debug_print_ret(FILE *file, const struct aarch64_ret *ret) {
  codegen_fprintf(file, " %reg", ret->target);
}

void debug_print_compare_and_branch(
    FILE *file, const struct aarch64_compare_and_branch *compare_and_branch) {
  codegen_fprintf(file, " %reg, %instr", compare_and_branch->cmp,
                  compare_and_branch->target->first_instr);
}

void debug_print_load_imm(FILE *file, const struct aarch64_load_imm *load_imm) {
  codegen_fprintf(file, " %reg, %addr_imm", load_imm->dest, load_imm->mode,
                  load_imm->addr, load_imm->imm);
}

void debug_print_store_imm(FILE *file,
                           const struct aarch64_store_imm *store_imm) {
  codegen_fprintf(file, " %reg, %addr_imm", store_imm->source, store_imm->mode,
                  store_imm->addr, store_imm->imm);
}

void debug_print_load_pair_imm(
    FILE *file, const struct aarch64_load_pair_imm *load_pair_imm) {
  codegen_fprintf(file, " %reg, %reg, %addr_imm", load_pair_imm->dest[0],
                  load_pair_imm->dest[1], load_pair_imm->mode,
                  load_pair_imm->addr, load_pair_imm->imm);
}

void debug_print_store_pair_imm(
    FILE *file, const struct aarch64_store_pair_imm *store_pair_imm) {
  codegen_fprintf(file, " %reg, %reg, %addr_imm", store_pair_imm->source[0],
                  store_pair_imm->source[1], store_pair_imm->mode,
                  store_pair_imm->addr, store_pair_imm->imm);
}

void debug_print_mov_imm(FILE *file, const struct aarch64_mov_imm *mov_imm) {
  if (mov_imm->shift) {
    codegen_fprintf(file, " %reg, %imm, lsl %imm", mov_imm->dest, mov_imm->imm,
                    mov_imm->shift);
  } else {
    codegen_fprintf(file, " %reg, %imm", mov_imm->dest, mov_imm->imm);
  }
}

void debug_print_fma(FILE *file, const struct aarch64_fma *fma) {
  codegen_fprintf(file, " %reg, %reg, %reg", fma->lhs, fma->rhs, fma->addsub);
}

void debug_print_instr(FILE *file, const struct codegen_function *func,
                       const struct instr *instr) {

  switch (instr->aarch64->ty) {
  case AARCH64_INSTR_TY_NOP:
    fprintf(file, "nop");
    break;
  case AARCH64_INSTR_TY_SBFM_IMM:
    fprintf(file, "sbfm");
    debug_print_bitfield_imm(file, &instr->aarch64->sbfm);
    break;
  case AARCH64_INSTR_TY_BFM_IMM:
    fprintf(file, "bfm");
    debug_print_bitfield_imm(file, &instr->aarch64->bfm);
    break;
  case AARCH64_INSTR_TY_UBFM_IMM:
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
    fprintf(file, "orr");
    debug_print_logical_reg(file, &instr->aarch64->orr);
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
    debug_print_branch(file, &instr->aarch64->bl);
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
  case AARCH64_INSTR_TY_MOVN_IMM:
    fprintf(file, "movn");
    debug_print_mov_imm(file, &instr->aarch64->movn_imm);
    break;
  case AARCH64_INSTR_TY_MVN:
    fprintf(file, "mvn");
    debug_print_reg_1_source_with_shift(file, &instr->aarch64->mvn);
    break;
  case AARCH64_INSTR_TY_MOV_IMM:
    fprintf(file, "mov");
    debug_print_mov_imm(file, &instr->aarch64->mov_imm);
    break;
  case AARCH64_INSTR_TY_MSUB:
    fprintf(file, "msub");
    debug_print_fma(file, &instr->aarch64->msub);
    break;
  case AARCH64_INSTR_TY_RET:
    fprintf(file, "ret");
    debug_print_ret(file, &instr->aarch64->ret);
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
  case AARCH64_INSTR_TY_STORE_PAIR_IMM:
    fprintf(file, "stp");
    debug_print_store_pair_imm(file, &instr->aarch64->stp_imm);
    break;
  case AARCH64_INSTR_TY_SUBS:
    fprintf(file, "subs");
    debug_print_addsub_reg(file, &instr->aarch64->subs);
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
  }
}

void aarch64_debug_print_codegen(FILE *file, struct codegen_function *func) {
  debug_assert(func->ty == CODEGEN_FUNCTION_TY_AARCH64, "expected aarch64");

  int offset = 0;
  struct instr *instr = func->first;
  while (instr) {
    fprintf(file, "0x%04X: ", offset++);
    debug_print_instr(file, func, instr);
    fprintf(file, "\n");

    instr = instr->succ;
  }
}
