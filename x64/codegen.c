#include "codegen.h"

#include "../bit_twiddle.h"
#include "../bitset.h"
#include "../util.h"
#include "../vector.h"
#include "../x64.h"
#include "isa.h"

#include <stdio.h>

// #define MOV_ALIAS(dest_reg, source_reg)                                        \
//   (struct x64_instr) {                                                     \
//     .ty = X64_INSTR_TY_ORR, .orr = {                                       \
//       .lhs = zero_reg_for_ty(dest_reg.ty),                                     \
//       .rhs = (source_reg),                                                     \
//       .dest = (dest_reg),                                                      \
//       .imm6 = 0                                                                \
//     }                                                                          \
//   }

// #define FP_MOV_ALIAS(dest_reg, source_reg)                                     \
//   (struct x64_instr) {                                                     \
//     .ty = X64_INSTR_TY_FMOV, .fmov = {                                     \
//       .source = (source_reg),                                                  \
//       .dest = (dest_reg),                                                      \
//     }                                                                          \
//   }



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

// bool is_return_reg(struct x64_reg reg) { return reg.idx == 0; }

// bool is_zero_reg(struct x64_reg reg) {
//   // this is a bit dodgy as it can also be SP in this context
//   return x64_reg_ty_is_gp(reg.ty) && reg.idx == 31;
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
        "(e.g w0 vs x0)");
  }

  return false;
}

static struct x64_reg return_reg_for_ty(enum x64_reg_ty reg_ty) {
  return (struct x64_reg){.ty = reg_ty, .idx = 0};
}

// static struct x64_reg zero_reg_for_ty(enum x64_reg_ty reg_ty) {
//   return (struct x64_reg){.ty = reg_ty, .idx = 31};
// }

bool x64_reg_ty_is_gp(enum x64_reg_ty ty) {
  switch (ty) {
  case X64_REG_TY_R:
  case X64_REG_TY_RD:
  case X64_REG_TY_E:
    return true;
    // case X64_REG_TY_V:
    // case X64_REG_TY_Q:
    // case X64_REG_TY_D:
    // case X64_REG_TY_S:
    // case X64_REG_TY_H:
    // case X64_REG_TY_B:
    //   return false;
  }
}

// bool x64_reg_ty_is_fp(enum x64_reg_ty ty) {
//   switch (ty) {
//   case X64_REG_TY_NONE:
//     BUG("makes no sense");

//   case X64_REG_TY_W:
//   case X64_REG_TY_X:
//     return false;
//   case X64_REG_TY_V:
//   case X64_REG_TY_Q:
//   case X64_REG_TY_D:
//   case X64_REG_TY_S:
//   case X64_REG_TY_H:
//   case X64_REG_TY_B:
//     return true;
//   }
// }

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

// struct x64_prologue_info {
//   bool prologue_generated;
//   size_t stack_size;
//   size_t lr_offset;
//   size_t save_start;
//   unsigned long long saved_gp_registers;
//   unsigned long long saved_fp_registers;
// };

struct codegen_state {
  struct arena_allocator *arena;

  const struct target *target;
  struct codegen_function *func;
  struct ir_func *ir;
  // struct x64_prologue_info prologue_info;
  // struct x64_reg ssp;
  // size_t stack_args_size;
};

// static enum x64_cond get_cond_for_op(struct ir_op *op) {
//   invariant_assert(op->ty == IR_OP_TY_BINARY_OP,
//                    "`get_cond_for_op` expects a binary op");

//   switch (op->binary_op.ty) {
//   case IR_OP_BINARY_OP_TY_FEQ:
//   case IR_OP_BINARY_OP_TY_EQ:
//     return X64_COND_EQ;
//   case IR_OP_BINARY_OP_TY_FNEQ:
//   case IR_OP_BINARY_OP_TY_NEQ:
//     return X64_COND_NE;
//   case IR_OP_BINARY_OP_TY_UGT:
//     return X64_COND_HI;
//   case IR_OP_BINARY_OP_TY_SGT:
//     return X64_COND_GT;
//   case IR_OP_BINARY_OP_TY_UGTEQ:
//     return X64_COND_HS;
//   case IR_OP_BINARY_OP_TY_SGTEQ:
//     return X64_COND_GE;
//   case IR_OP_BINARY_OP_TY_ULT:
//     return X64_COND_LO;
//   case IR_OP_BINARY_OP_TY_SLT:
//     return X64_COND_LT;
//   case IR_OP_BINARY_OP_TY_ULTEQ:
//     return X64_COND_LS;
//   case IR_OP_BINARY_OP_TY_SLTEQ:
//     return X64_COND_LE;
//   case IR_OP_BINARY_OP_TY_FLT:
//     return X64_COND_MI;
//   case IR_OP_BINARY_OP_TY_FGT:
//     return X64_COND_GT;
//   case IR_OP_BINARY_OP_TY_FLTEQ:
//     return X64_COND_LS;
//   case IR_OP_BINARY_OP_TY_FGTEQ:
//     return X64_COND_GE;
//   default:
//     BUG("op was not a comparison");
//   }
// }

// static ssize_t get_lcl_stack_offset(const struct codegen_state *state,
//                                     const struct ir_op *op,
//                                     const struct ir_lcl *lcl) {
//   ssize_t offset = state->stack_args_size + lcl->offset;

//   if (!op) {
//     return offset;
//   }

//   struct ir_var_ty_info info = var_ty_info(state->ir->unit, &op->var_ty);
//   DEBUG_ASSERT(offset % info.size == 0,
//                "stack offset not divisible by type size");

//   return offset / info.size;
// }

static size_t translate_reg_idx(size_t idx, enum ir_reg_ty ty) {
  switch (ty) {
  case IR_REG_TY_NONE:
  case IR_REG_TY_SPILLED:
  case IR_REG_TY_FLAGS:
    BUG("does not make sense for none/spilled/flags");
  case IR_REG_TY_INTEGRAL:
    return idx < 4 ? idx : idx + 2;
  case IR_REG_TY_FP:
    return idx;
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

static enum x64_reg_ty reg_ty_for_var_ty(const struct ir_var_ty *var_ty, size_t idx) {
  switch (var_ty->primitive) {
  case IR_VAR_PRIMITIVE_TY_I8:
  case IR_VAR_PRIMITIVE_TY_I16:
  case IR_VAR_PRIMITIVE_TY_I32:
    return idx < 8 ? X64_REG_TY_E : X64_REG_TY_RD;
  case IR_VAR_PRIMITIVE_TY_I64:
    return X64_REG_TY_R;
  // case IR_VAR_PRIMITIVE_TY_F16:
  //   return X64_REG_TY_H;
  // case IR_VAR_PRIMITIVE_TY_F32:
  //   return X64_REG_TY_S;
  // case IR_VAR_PRIMITIVE_TY_F64:
  //   return X64_REG_TY_D;
  default:
    TODO("other reg tys");
  }
}

static struct x64_reg codegen_reg(struct ir_op *op) {
  size_t idx = translate_reg_idx(op->reg.idx, op->reg.ty);

  if (op->var_ty.ty != IR_VAR_TY_TY_PRIMITIVE) {
    TODO("non primitives (op %zu)", op->id);
  }

  enum x64_reg_ty reg_ty = reg_ty_for_var_ty(&op->var_ty, idx);

  switch (reg_ty) {
  case X64_REG_TY_R:
  case X64_REG_TY_RD:
  case X64_REG_TY_E:
    invariant_assert(op->reg.ty == IR_REG_TY_INTEGRAL, "expected integral reg");
    break;
    // case X64_REG_TY_:
    // case X64_REG_TY_S:
    // case X64_REG_TY_D:
    //   invariant_assert(op->reg.ty == IR_REG_TY_FP, "expected fp reg");
    //   break;
    // default:
    //   TODO("other reg tys (Q/V)");
  }

  return (struct x64_reg){.ty = reg_ty, .idx = idx};
}

static void codegen_mov_op(struct codegen_state *state, struct ir_op *op) {
  struct x64_reg dest = codegen_reg(op);

  if (op->mov.value->reg.ty == IR_REG_TY_FLAGS) {
    TODO("flags mov x64");

    return;
  }

  struct x64_reg source = codegen_reg(op->mov.value);

  struct instr *instr = alloc_instr(state->func);
  if (x64_reg_ty_is_gp(source.ty) && x64_reg_ty_is_gp(dest.ty)) {
    *instr->x64 =
        (struct x64_instr){.ty = X64_INSTR_TY_MOV_REG,
                           .mov_reg = {.dest = dest, .source = source}};
  } else {
    // one is floating
    TODO("fp move x64");
  }
}

// // TODO: we should remove load/store lcl/glb ops and lower all addressing
// // requirements earlier

// static enum x64_instr_ty load_ty_for_op(struct ir_op *op) {
//   if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
//       op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
//     return X64_INSTR_TY_LOAD_BYTE_IMM;
//   } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
//              op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
//     return X64_INSTR_TY_LOAD_HALF_IMM;
//   } else {
//     return X64_INSTR_TY_LOAD_IMM;
//   }
// }

// static enum x64_instr_ty store_ty_for_op(struct ir_op *op) {
//   if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
//       op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I8) {
//     return X64_INSTR_TY_STORE_BYTE_IMM;
//   } else if (op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE &&
//              op->var_ty.primitive == IR_VAR_PRIMITIVE_TY_I16) {
//     return X64_INSTR_TY_STORE_HALF_IMM;
//   } else {
//     return X64_INSTR_TY_STORE_IMM;
//   }
// }

// static void codegen_add_imm(struct codegen_state *state,
//                             struct x64_reg dest, struct x64_reg source,
//                             unsigned long long value);

// static void codegen_load_lcl_op(struct codegen_state *state, struct ir_op
// *op) {
//   struct x64_reg dest = codegen_reg(op);
//   struct ir_lcl *lcl = op->load.lcl;

//   simm_t offset = get_lcl_stack_offset(state, op, lcl);

//   if (offset > MAX_IMM_SIZE) {
//     size_t raw_offset = get_lcl_stack_offset(state, NULL, lcl);
//     codegen_add_imm(state, state->ssp, STACK_PTR_REG, raw_offset);

//     struct instr *instr = alloc_instr(state->func);
//     instr->x64->ty = load_ty_for_op(op);
//     instr->x64->load_imm =
//         (struct x64_load_imm){.dest = dest,
//                                   .addr = state->ssp,
//                                   .imm = 0,
//                                   .mode = X64_ADDRESSING_MODE_OFFSET};
//   } else {
//     struct instr *instr = alloc_instr(state->func);
//     instr->x64->ty = load_ty_for_op(op);
//     instr->x64->load_imm =
//         (struct x64_load_imm){.dest = dest,
//                                   .addr = STACK_PTR_REG,
//                                   .imm = offset,
//                                   .mode = X64_ADDRESSING_MODE_OFFSET};
//   }
// }

// static void codegen_store_lcl_op(struct codegen_state *state,
//                                  struct ir_op *op) {
//   struct x64_reg source = codegen_reg(op->store.value);
//   struct ir_lcl *lcl = op->store.lcl;

//   size_t offset = get_lcl_stack_offset(state, op->store.value, lcl);

//   if (offset > MAX_IMM_SIZE) {
//     size_t raw_offset = get_lcl_stack_offset(state, NULL, lcl);
//     codegen_add_imm(state, state->ssp, STACK_PTR_REG, raw_offset);

//     struct instr *instr = alloc_instr(state->func);
//     instr->x64->ty = load_ty_for_op(op);
//     instr->x64->str_imm =
//         (struct x64_store_imm){.source = source,
//                                    .addr = state->ssp,
//                                    .imm = 0,
//                                    .mode = X64_ADDRESSING_MODE_OFFSET};
//   } else {
//     struct instr *instr = alloc_instr(state->func);
//     instr->x64->ty = store_ty_for_op(op->store.value);
//     instr->x64->str_imm =
//         (struct x64_store_imm){.source = source,
//                                    .addr = STACK_PTR_REG,
//                                    .imm = offset,
//                                    .mode = X64_ADDRESSING_MODE_OFFSET};
//   }
// }

// static void codegen_load_addr_op(struct codegen_state *state,
//                                  struct ir_op *op) {
//   struct instr *instr = alloc_instr(state->func);

//   struct x64_reg dest = codegen_reg(op);

//   if (op->load.addr->flags & IR_OP_FLAG_CONTAINED) {
//     struct ir_op *addr = op->load.addr;

//     simm_t imm;
//     if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
//       imm = get_lcl_stack_offset(state, op, addr->addr.lcl);
//     } else {
//       BUG("can't CONTAIN operand in load_addr node");
//     }

//     instr->x64->ty = load_ty_for_op(op);
//     instr->x64->ldr_imm =
//         (struct x64_load_imm){.dest = dest,
//                                   .addr = STACK_PTR_REG,
//                                   .imm = imm,
//                                   .mode = X64_ADDRESSING_MODE_OFFSET};
//   } else {
//     struct x64_reg addr = codegen_reg(op->load.addr);
//     instr->x64->ty = load_ty_for_op(op);
//     instr->x64->ldr_imm =
//         (struct x64_load_imm){.dest = dest,
//                                   .addr = addr,
//                                   .imm = 0,
//                                   .mode = X64_ADDRESSING_MODE_OFFSET};
//   }
// }

// static void codegen_store_addr_op(struct codegen_state *state,
//                                   struct ir_op *op) {
//   struct instr *instr = alloc_instr(state->func);

//   struct x64_reg source = codegen_reg(op->store.value);

//   if (op->store.addr->flags & IR_OP_FLAG_CONTAINED) {
//     struct ir_op *addr = op->store.addr;

//     simm_t imm;
//     if (addr->ty == IR_OP_TY_ADDR && addr->addr.ty == IR_OP_ADDR_TY_LCL) {
//       imm = get_lcl_stack_offset(state, op->store.value, addr->addr.lcl);
//     } else {
//       BUG("can't CONTAIN operand in store_addr node");
//     }

//     instr->x64->ty = store_ty_for_op(op->store.value);
//     instr->x64->str_imm =
//         (struct x64_store_imm){.source = source,
//                                    .addr = STACK_PTR_REG,
//                                    .imm = imm,
//                                    .mode = X64_ADDRESSING_MODE_OFFSET};
//   } else {
//     struct x64_reg addr = codegen_reg(op->store.addr);
//     instr->x64->ty = store_ty_for_op(op->store.value);
//     instr->x64->str_imm =
//         (struct x64_store_imm){.source = source,
//                                    .addr = addr,
//                                    .imm = 0,
//                                    .mode = X64_ADDRESSING_MODE_OFFSET};
//   }
// }

// static void codegen_bitfield_insert(struct codegen_state *state,
//                                     struct ir_op *op) {
//   struct ir_bitfield bitfield = op->bitfield_insert.bitfield;

//   struct x64_reg value_reg = codegen_reg(op->bitfield_insert.value);
//   struct x64_reg target_reg = codegen_reg(op->bitfield_insert.target);
//   struct x64_reg dest = codegen_reg(op);

//   if (target_reg.idx != dest.idx) {
//     struct instr *mov = alloc_instr(state->func);
//     *mov->x64 = MOV_ALIAS(dest, target_reg);
//   }

//   struct instr *instr = alloc_instr(state->func);
//   instr->x64->ty = X64_INSTR_TY_BFM;
//   instr->x64->bfm = (struct x64_bitfield){
//       .dest = dest,
//       .source = value_reg,
//       .imms = bitfield.width - 1,
//       .immr = bitfield.offset,
//   };
// }

// static void codegen_bitfield_extract(struct codegen_state *state,
//                                      struct ir_op *op) {
//   struct ir_bitfield bitfield = op->bitfield_extract.bitfield;

//   struct x64_reg value_reg = codegen_reg(op->bitfield_extract.value);
//   struct x64_reg dest = codegen_reg(op);

//   struct instr *instr = alloc_instr(state->func);
//   instr->x64->ty = X64_INSTR_TY_UBFM;
//   instr->x64->ubfm = (struct x64_bitfield){.dest = dest,
//                                                    .source = value_reg,
//                                                    .imms = bitfield.width -
//                                                    1, .immr =
//                                                    bitfield.offset};
// }

// static void codegen_load_op(struct codegen_state *state, struct ir_op *op) {
//   switch (op->load.ty) {
//   case IR_OP_LOAD_TY_LCL:
//     codegen_load_lcl_op(state, op);
//     break;
//   case IR_OP_LOAD_TY_ADDR:
//     codegen_load_addr_op(state, op);
//     break;
//   case IR_OP_LOAD_TY_GLB:
//     BUG("load.glb should have been lowered");
//   }
// }

// static void codegen_store_op(struct codegen_state *state, struct ir_op *op) {
//   switch (op->load.ty) {
//   case IR_OP_STORE_TY_LCL:
//     codegen_store_lcl_op(state, op);
//     break;
//   case IR_OP_STORE_TY_ADDR:
//     codegen_store_addr_op(state, op);
//     break;
//   case IR_OP_STORE_TY_GLB:
//     BUG("store.glb should have been lowered");
//   }
// }

// // this method assumes it can safely you any non-argument volatile registers
// static void codegen_mem_copy_volatile(struct codegen_state *state,
//                                       struct x64_reg source,
//                                       size_t source_offset,
//                                       struct x64_reg dest,
//                                       size_t dest_offset, size_t num_bytes) {
//   DEBUG_ASSERT(num_bytes < 4096,
//                "doesn't support >4096 copies due to immediates");

//   if (source_offset % 32 || dest_offset % 32) {
//     TODO("handle");
//   }

//   size_t remaining = num_bytes;
//   size_t head = 0;

//   // use v10/11 as the intermediates

//   while (remaining >= 32) {
//     struct instr *load = alloc_instr(state->func);
//     load->x64->ty = X64_INSTR_TY_LOAD_PAIR_IMM;
//     load->x64->load_pair_imm =
//         (struct x64_load_pair_imm){.mode = X64_ADDRESSING_MODE_OFFSET,
//                                        .imm = (head + source_offset) / 16,
//                                        .addr = source,
//                                        .dest = {
//                                            {.ty = X64_REG_TY_Q, .idx = 10},
//                                            {.ty = X64_REG_TY_Q, .idx = 11},
//                                        }};

//     struct instr *store = alloc_instr(state->func);
//     store->x64->ty = X64_INSTR_TY_STORE_PAIR_IMM;
//     store->x64->store_pair_imm =
//         (struct x64_store_pair_imm){.mode = X64_ADDRESSING_MODE_OFFSET,
//                                         .imm = (head + dest_offset) / 16,
//                                         .addr = dest,
//                                         .source = {
//                                             {.ty = X64_REG_TY_Q, .idx = 10},
//                                             {.ty = X64_REG_TY_Q, .idx = 11},
//                                         }};

//     head += 32;
//     remaining -= 32;
//   }

//   while (remaining) {
//     size_t chunk = ilog2(remaining);
//     enum x64_reg_ty ty = fp_reg_ty_for_size(chunk);

//     struct instr *load = alloc_instr(state->func);
//     load->x64->ty = X64_INSTR_TY_LOAD_IMM;
//     load->x64->load_imm = (struct x64_load_imm){
//         .mode = X64_ADDRESSING_MODE_OFFSET,
//         .imm = (head + source_offset) / chunk,
//         .addr = source,
//         .dest = {.ty = ty, .idx = 10},
//     };

//     struct instr *store = alloc_instr(state->func);
//     store->x64->ty = X64_INSTR_TY_STORE_IMM;
//     store->x64->store_imm = (struct x64_store_imm){
//         .mode = X64_ADDRESSING_MODE_OFFSET,
//         .imm = (head + dest_offset) / chunk,
//         .addr = dest,
//         .source = {.ty = ty, .idx = 10},
//     };

//     head += chunk;
//     remaining -= chunk;
//   }
// }

// #define IMM_BITS (12)

// static void codegen_add_imm(struct codegen_state *state,
//                             struct x64_reg dest, struct x64_reg source,
//                             unsigned long long value) {

//   if (value & ~MASK_LO(unsigned long long, 24)) {
//     TODO("imm >24 bits");
//   }

//   unsigned long long lo = value & MASK_LO(unsigned long long, 12);
//   unsigned long long hi = (value >> 12) & MASK_LO(unsigned long long, 12);

//   struct instr *instr = alloc_instr(state->func);
//   instr->x64->ty = X64_INSTR_TY_ADD_IMM;
//   instr->x64->add_imm = (struct x64_addsub_imm){
//       .dest = dest,
//       .source = source,
//       .imm = lo,
//       .shift = 0,
//   };

//   if (hi) {
//     instr = alloc_instr(state->func);
//     instr->x64->ty = X64_INSTR_TY_ADD_IMM;
//     instr->x64->add_imm = (struct x64_addsub_imm){
//         .dest = dest,
//         .source = dest,
//         .imm = hi,
//         .shift = 1,
//     };
//   }
// }

// static void codegen_sub_imm(struct codegen_state *state,
//                             struct x64_reg dest, struct x64_reg source,
//                             unsigned long long value) {

//   if (value & ~MASK_LO(unsigned long long, 24)) {
//     TODO("imm >24 bits");
//   }

//   unsigned long long lo = value & MASK_LO(unsigned long long, 12);
//   unsigned long long hi = (value >> 12) & MASK_LO(unsigned long long, 12);

//   struct instr *instr = alloc_instr(state->func);
//   instr->x64->ty = X64_INSTR_TY_SUB_IMM;
//   instr->x64->add_imm = (struct x64_addsub_imm){
//       .dest = dest,
//       .source = dest,
//       .imm = lo,
//       .shift = 0,
//   };

//   if (hi) {
//     instr = alloc_instr(state->func);
//     instr->x64->ty = X64_INSTR_TY_SUB_IMM;
//     instr->x64->add_imm = (struct x64_addsub_imm){
//         .dest = dest,
//         .source = source,
//         .imm = hi,
//         .shift = 1,
//     };
//   }
// }
// static void codegen_addr_op(struct codegen_state *state, struct ir_op *op) {
//   struct x64_reg dest = codegen_reg(op);

//   switch (op->addr.ty) {
//   case IR_OP_ADDR_TY_LCL: {
//     struct ir_lcl *lcl = op->addr.lcl;

//     // op is NULL as we want the absolute offset
//     size_t offset = get_lcl_stack_offset(state, NULL, lcl);

//     codegen_add_imm(state, dest, STACK_PTR_REG, offset);

//     break;
//   }
//   case IR_OP_ADDR_TY_GLB: {
//     struct ir_glb *glb = op->addr.glb;

//     struct instr *adrp = alloc_instr(state->func);
//     adrp->x64->ty = X64_INSTR_TY_ADRP;
//     adrp->x64->adrp = (struct x64_addr_imm){.dest = dest, .imm = 0};

//     adrp->reloc = arena_alloc(state->func->unit->arena,
//     sizeof(*adrp->reloc)); *adrp->reloc = (struct relocation){
//         .ty = glb->def_ty == IR_GLB_DEF_TY_DEFINED ? RELOCATION_TY_LOCAL_PAIR
//                                                    :
//                                                    RELOCATION_TY_UNDEF_PAIR,
//         .symbol_index = glb->id,
//         .address = 0,
//         .size = 0};

//     if (glb->def_ty == IR_GLB_DEF_TY_DEFINED) {
//       struct instr *add = alloc_instr(state->func);
//       add->x64->ty = X64_INSTR_TY_ADD_IMM;
//       add->x64->add_imm = (struct x64_addsub_imm){
//           .dest = dest,
//           .source = dest,
//           .imm = 0,
//       };
//     } else {
//       struct instr *ldr = alloc_instr(state->func);
//       ldr->x64->ty = X64_INSTR_TY_LOAD_IMM;
//       ldr->x64->load_imm = (struct x64_load_imm){
//           .mode = X64_ADDRESSING_MODE_OFFSET,
//           .dest = dest,
//           .addr = dest,
//           .imm = 0,
//       };
//     }
//   }
//   }
// }

// static void codegen_br_cond_op(struct codegen_state *state, struct ir_op *op)
// {
//   struct instr *instr = alloc_instr(state->func);

//   // AArch64 requires turning `br.cond <true> <false>` into 2 instructions
//   // we represent this as just the `true` part of the `br.cond`, and then a
//   `br`
//   // after branching to the false target

//   struct ir_basicblock *true_target =
//   op->stmt->basicblock->split.true_target; struct ir_basicblock *false_target
//   = op->stmt->basicblock->split.false_target;

//   if (op->br_cond.cond->reg.ty == IR_REG_TY_FLAGS) {
//     // emit based on flags
//     enum x64_cond cond = get_cond_for_op(op->br_cond.cond);
//     instr->x64->ty = X64_INSTR_TY_B_COND;
//     instr->x64->b_cond = (struct x64_conditional_branch){
//         .cond = cond, .target = true_target};
//   } else {
//     struct x64_reg cmp_reg = codegen_reg(op->br_cond.cond);

//     instr->x64->ty = X64_INSTR_TY_CBNZ;
//     instr->x64->cbnz = (struct x64_compare_and_branch){
//         .cmp = cmp_reg, .target = true_target};
//   }

//   // now generate the `br`
//   struct instr *br = alloc_instr(state->func);
//   br->x64->ty = X64_INSTR_TY_B;
//   br->x64->b = (struct x64_branch){.target = false_target};
// }

// static void codegen_br_op(struct codegen_state *state, struct ir_op *op) {
//   struct instr *instr = alloc_instr(state->func);

//   instr->x64->ty = X64_INSTR_TY_B;
//   instr->x64->b =
//       (struct x64_branch){.target = op->stmt->basicblock->merge.target};
// }

// static_assert((sizeof(unsigned long long) == 8) & (sizeof(unsigned short) ==
// 2),
//               "type sizes not expected");

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

static void codegen_unary_op(struct codegen_state *state, struct ir_op *op) {
  struct x64_reg dest = codegen_reg(op);
  struct x64_reg source = codegen_reg(op->unary_op.value);

  if (!reg_eq(source, dest)) {
    struct instr *mov = alloc_instr(state->func);
    *mov->x64 = (struct x64_instr){
      .ty = X64_INSTR_TY_MOV_REG,
      .mov_reg = {
        .dest = dest,
        .source = source
      }
    };
  }

  struct instr *instr = alloc_instr(state->func);

  switch (op->unary_op.ty) {
//   case IR_OP_UNARY_OP_TY_FABS:
//     instr->x64->ty = X64_INSTR_TY_FABS;
//     instr->x64->fabs = (struct x64_reg_1_source){
//         .dest = dest,
//         .source = source,
//     };
//     return;
//   case IR_OP_UNARY_OP_TY_FSQRT:
//     instr->x64->ty = X64_INSTR_TY_FSQRT;
//     instr->x64->fsqrt = (struct x64_reg_1_source){
//         .dest = dest,
//         .source = source,
//     };
//     return;
//   case IR_OP_UNARY_OP_TY_FNEG:
//     instr->x64->ty = X64_INSTR_TY_FNEG;
//     instr->x64->fneg = (struct x64_reg_1_source){
//         .dest = dest,
//         .source = source,
//     };
//     return;
  case IR_OP_UNARY_OP_TY_NEG:
    instr->x64->ty = X64_INSTR_TY_NEG;
    instr->x64->neg = (struct x64_alu_unary){
        .dest = dest,
    };
    return;
  case IR_OP_UNARY_OP_TY_NOT:
    instr->x64->ty = X64_INSTR_TY_NOT;
    instr->x64->not = (struct x64_alu_unary){
        .dest = dest,
    };
    return;
  default:
    TODO("other x64 unary ops");
//   case IR_OP_UNARY_OP_TY_LOGICAL_NOT:
//     BUG("logical not should never reach emitter, should be converted in
//     lower");
  }
}

static void codegen_binary_op(struct codegen_state *state, struct ir_op *op) {
  struct x64_reg dest = {0};

  if (!binary_op_is_comparison(op->binary_op.ty)) {
    dest = codegen_reg(op);
  }

  struct x64_reg lhs = codegen_reg(op->binary_op.lhs);
  struct x64_reg rhs = codegen_reg(op->binary_op.rhs);

  if (!reg_eq(lhs, dest)) {
    struct instr *mov = alloc_instr(state->func);
    *mov->x64 = (struct x64_instr){
      .ty = X64_INSTR_TY_MOV_REG,
      .mov_reg = {
        .dest = dest,
        .source = lhs
      }
    };
  }

  struct instr *instr = alloc_instr(state->func);

  bool is_fp = var_ty_is_fp(&op->var_ty);

  enum ir_op_binary_op_ty ty = op->binary_op.ty;
  DEBUG_ASSERT(ty == IR_OP_BINARY_OP_TY_FADD || ty == IR_OP_BINARY_OP_TY_FSUB ||
                   ty == IR_OP_BINARY_OP_TY_FMUL ||
                   ty == IR_OP_BINARY_OP_TY_FDIV || !is_fp,
               "floating point with invalid binary op");

  switch (ty) {
    //   case IR_OP_BINARY_OP_TY_FEQ:
    //   case IR_OP_BINARY_OP_TY_FNEQ:
    //   case IR_OP_BINARY_OP_TY_FGT:
    //   case IR_OP_BINARY_OP_TY_FGTEQ:
    //   case IR_OP_BINARY_OP_TY_FLT:
    //   case IR_OP_BINARY_OP_TY_FLTEQ:
    //     instr->x64->ty = X64_INSTR_TY_FCMP;
    //     instr->x64->fcmp = (struct x64_fcmp){
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_EQ:
    //   case IR_OP_BINARY_OP_TY_NEQ:
    //   case IR_OP_BINARY_OP_TY_UGT:
    //   case IR_OP_BINARY_OP_TY_SGT:
    //   case IR_OP_BINARY_OP_TY_UGTEQ:
    //   case IR_OP_BINARY_OP_TY_SGTEQ:
    //   case IR_OP_BINARY_OP_TY_ULT:
    //   case IR_OP_BINARY_OP_TY_SLT:
    //   case IR_OP_BINARY_OP_TY_ULTEQ:
    //   case IR_OP_BINARY_OP_TY_SLTEQ:
    //     instr->x64->ty = X64_INSTR_TY_SUBS;
    //     instr->x64->subs = (struct x64_addsub_reg){
    //         .dest = zero_reg_for_ty(lhs.ty),
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_LSHIFT:
    //     instr->x64->ty = X64_INSTR_TY_LSLV;
    //     instr->x64->lslv = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_SRSHIFT:
    //     instr->x64->ty = X64_INSTR_TY_ASRV;
    //     instr->x64->asrv = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_URSHIFT:
    //     instr->x64->ty = X64_INSTR_TY_LSRV;
    //     instr->x64->lsrv = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
  case IR_OP_BINARY_OP_TY_AND:
    instr->x64->ty = X64_INSTR_TY_AND;
    instr->x64->and = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_OR:
    instr->x64->ty = X64_INSTR_TY_OR;
    instr->x64->or = (struct x64_alu_reg){
        .dest = dest,
        .rhs = rhs,
    };
    break;
  case IR_OP_BINARY_OP_TY_XOR:
    instr->x64->ty = X64_INSTR_TY_EOR;
    instr->x64->eor = (struct x64_alu_reg){
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
    //   case IR_OP_BINARY_OP_TY_MUL:
    //     instr->x64->ty = X64_INSTR_TY_MADD;
    //     instr->x64->madd =
    //         (struct x64_fma){.dest = dest,
    //                              .lhs = lhs,
    //                              .rhs = rhs,
    //                              .addsub = zero_reg_for_ty(lhs.ty)};
    //     break;
    //   case IR_OP_BINARY_OP_TY_SDIV:
    //     instr->x64->ty = X64_INSTR_TY_SDIV;
    //     instr->x64->sdiv = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_UDIV:
    //     instr->x64->ty = X64_INSTR_TY_UDIV;
    //     instr->x64->udiv = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
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
    //   case IR_OP_BINARY_OP_TY_FADD:
    //     instr->x64->ty = X64_INSTR_TY_FADD;
    //     instr->x64->fadd = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_FSUB:
    //     instr->x64->ty = X64_INSTR_TY_FSUB;
    //     instr->x64->fsub = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_FMUL:
    //     instr->x64->ty = X64_INSTR_TY_FMUL;
    //     instr->x64->fmul = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_FDIV:
    //     instr->x64->ty = X64_INSTR_TY_FDIV;
    //     instr->x64->fdiv = (struct x64_reg_2_source){
    //         .dest = dest,
    //         .lhs = lhs,
    //         .rhs = rhs,
    //     };
    //     break;
    //   case IR_OP_BINARY_OP_TY_SQUOT:
    //   case IR_OP_BINARY_OP_TY_UQUOT:
    //     BUG("squot/uquot shoud have been lowered");
  default:
    TODO("x64 other binops");
  }
}

// static void codegen_sext_op(struct codegen_state *state, struct ir_op *op,
//                             struct x64_reg source,
//                             struct x64_reg dest) {
//   struct instr *instr = alloc_instr(state->func);

//   invariant_assert(op->cast_op.value->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
//                    "can't sext from non-primitive");

//   switch (op->cast_op.value->var_ty.primitive) {
//   case IR_VAR_PRIMITIVE_TY_I8:
//     instr->x64->ty = X64_INSTR_TY_SBFM;
//     instr->x64->sbfm = (struct x64_bitfield){
//         .dest = dest, .source = source, .immr = 0b000000, .imms = 0b000111};
//     break;
//   case IR_VAR_PRIMITIVE_TY_I16:
//     instr->x64->ty = X64_INSTR_TY_SBFM;
//     instr->x64->sbfm = (struct x64_bitfield){
//         .dest = dest, .source = source, .immr = 0b000000, .imms = 0b001111};
//     break;
//   case IR_VAR_PRIMITIVE_TY_I32:
//     instr->x64->ty = X64_INSTR_TY_SBFM;
//     instr->x64->sbfm = (struct x64_bitfield){
//         .dest = dest, .source = source, .immr = 0b000000, .imms = 0b011111};
//     break;
//   case IR_VAR_PRIMITIVE_TY_I64:
//     BUG("can't sext from I64");
//   case IR_VAR_PRIMITIVE_TY_F16:
//   case IR_VAR_PRIMITIVE_TY_F32:
//   case IR_VAR_PRIMITIVE_TY_F64:
//     BUG("todo cast floats");
//   }
// }

// static void codegen_zext_op(struct codegen_state *state,
//                             struct x64_reg source,
//                             struct x64_reg dest) {
//   // `mov`/`orr` with 32 bit operands zeroes top 32 bits

//   struct instr *instr = alloc_instr(state->func);
//   *instr->x64 = MOV_ALIAS(dest, source);
// }

// static void codegen_trunc_op(struct codegen_state *state, struct ir_op *op,
//                              struct x64_reg source,
//                              struct x64_reg dest) {
//   struct instr *instr = alloc_instr(state->func);
//   invariant_assert(op->var_ty.ty == IR_VAR_TY_TY_PRIMITIVE,
//                    "can't truncate non-primitive");

//   // https://kddnewton.com/2022/08/11/x64-bitmask-immediates.html
//   // for understanding the immediates
//   switch (op->var_ty.primitive) {
//   case IR_VAR_PRIMITIVE_TY_I8:
//     instr->x64->ty = X64_INSTR_TY_AND_IMM;
//     instr->x64->and_imm = (struct x64_logical_imm){
//         .dest = dest,
//         .source = source,
//         .immr = 0b0,
//         .imms = 0b111,
//     };
//     break;
//   case IR_VAR_PRIMITIVE_TY_I16:
//     instr->x64->ty = X64_INSTR_TY_AND_IMM;
//     instr->x64->and_imm = (struct x64_logical_imm){
//         .dest = dest,
//         .source = source,
//         .immr = 0b0,
//         .imms = 0b1111,
//     };
//     break;
//   case IR_VAR_PRIMITIVE_TY_I32:
//     *instr->x64 = MOV_ALIAS(dest, source);
//     break;
//   case IR_VAR_PRIMITIVE_TY_I64:
//     break;
//   case IR_VAR_PRIMITIVE_TY_F16:
//   case IR_VAR_PRIMITIVE_TY_F32:
//   case IR_VAR_PRIMITIVE_TY_F64:
//     BUG("todo cast floats");
//   }
// }

// static void codegen_conv_op(struct codegen_state *state,
//                             struct x64_reg source,
//                             struct x64_reg dest) {
//   struct instr *instr = alloc_instr(state->func);
//   instr->x64->ty = X64_INSTR_TY_FCVT;
//   instr->x64->fcvt =
//       (struct x64_reg_1_source){.dest = dest, .source = source};
// }

// static void codegen_uconv_op(struct codegen_state *state,
//                              struct x64_reg source,
//                              struct x64_reg dest) {
//   struct instr *instr = alloc_instr(state->func);
//   instr->x64->ty = X64_INSTR_TY_UCVTF;
//   instr->x64->fcvt =
//       (struct x64_reg_1_source){.dest = dest, .source = source};
// }

// static void codegen_sconv_op(struct codegen_state *state,
//                              struct x64_reg source,
//                              struct x64_reg dest) {
//   struct instr *instr = alloc_instr(state->func);
//   instr->x64->ty = X64_INSTR_TY_SCVTF;
//   instr->x64->fcvt =
//       (struct x64_reg_1_source){.dest = dest, .source = source};
// }

// static void codegen_cast_op(struct codegen_state *state, struct ir_op *op) {
//   struct x64_reg dest = codegen_reg(op);
//   struct x64_reg source = codegen_reg(op->cast_op.value);

//   // NOTE: for the integer casts (sext/zext/trunc) we promote the source reg
//   to
//   // the same type as the dest reg (mixed regs make no sense in an integer
//   // instruction)

//   switch (op->cast_op.ty) {
//   case IR_OP_CAST_OP_TY_SEXT:
//     source.ty = dest.ty;
//     codegen_sext_op(state, op, source, dest);
//     break;
//   case IR_OP_CAST_OP_TY_ZEXT:
//     source.ty = dest.ty;
//     codegen_zext_op(state, source, dest);
//     break;
//   case IR_OP_CAST_OP_TY_TRUNC:
//     source.ty = dest.ty;
//     codegen_trunc_op(state, op, source, dest);
//     break;
//   case IR_OP_CAST_OP_TY_CONV:
//     codegen_conv_op(state, source, dest);
//     break;
//   case IR_OP_CAST_OP_TY_UCONV:
//     codegen_uconv_op(state, source, dest);
//     break;
//   case IR_OP_CAST_OP_TY_SCONV:
//     codegen_sconv_op(state, source, dest);
//     break;
//   }
// }

// static bool try_get_hfa_info(struct codegen_state *state,
//                              const struct ir_var_ty *var_ty,
//                              size_t *num_members, size_t *member_size) {
//   if (var_ty->ty != IR_VAR_TY_TY_UNION && var_ty->ty != IR_VAR_TY_TY_STRUCT)
//   {
//     return false;
//   }

//   if (var_ty->ty == IR_VAR_TY_TY_UNION) {
//     TODO("union hfa handling");
//   }

//   if (!var_ty->struct_ty.num_fields) {
//     return false;
//   }

//   struct ir_var_ty *field_ty = &var_ty->struct_ty.fields[0];

//   if (!var_ty_is_fp(field_ty)) {
//     return false;
//   }

//   if (var_ty->struct_ty.num_fields > 4) {
//     return false;
//   }

//   for (size_t i = 1; i < var_ty->struct_ty.num_fields; i++) {
//     if (!var_ty_eq(state->ir, field_ty, &var_ty->struct_ty.fields[i])) {
//       return false;
//     }
//   }

//   switch (field_ty->primitive) {
//   case IR_VAR_PRIMITIVE_TY_F16:
//     *member_size = 2;
//     break;
//   case IR_VAR_PRIMITIVE_TY_F32:
//     *member_size = 4;
//     break;
//   case IR_VAR_PRIMITIVE_TY_F64:
//     *member_size = 8;
//     break;
//   default:
//     unreachable();
//   }

//   *num_members = var_ty->struct_ty.num_fields;
//   return true;
// }

// // when generating reg moves, assume memory slots never conflict
// // use incrementing values from 128 onwards
// #define FIRST_MEM_LOC 128
// #define IS_MEM_LOC(v) ((v.idx) >= FIRST_MEM_LOC)
// #define MEM_LOC() (last_mem_loc++)

// struct mem_loc {
//   struct x64_reg base;
//   size_t offset;
//   size_t size;
// };
// struct mem_copy {
//   struct mem_loc src, dest;
// };

// static void codegen_moves(struct codegen_state *state, struct move_set moves,
//                           enum x64_reg_class reg_class) {
//   for (size_t i = 0; i < moves.num_moves; i++) {
//     struct move move = moves.moves[i];

//     if (IS_MEM_LOC(move.from)) {
//       DEBUG_ASSERT(!IS_MEM_LOC(move.to), "mem -> mem but not in memcpy
//       list");

//       struct mem_loc *from = move.from.metadata;
//       struct x64_reg to = {.ty = reg_ty_for_size(reg_class, from->size),
//                                .idx = move.to.idx};

//       DEBUG_ASSERT(from->offset % from->size == 0,
//                    "offset was not a multiple of size");
//       size_t offset = from->offset / from->size;

//       struct instr *load = alloc_instr(state->func);
//       load->x64->ty = X64_INSTR_TY_LOAD_IMM;
//       load->x64->load_imm =
//           (struct x64_load_imm){.addr = from->base,
//                                     .imm = offset,
//                                     .dest = to,
//                                     .mode = X64_ADDRESSING_MODE_OFFSET};
//     } else if (IS_MEM_LOC(move.to)) {
//       DEBUG_ASSERT(!IS_MEM_LOC(move.from), "mem -> mem but not in memcpy
//       list");

//       struct mem_loc *to = move.to.metadata;
//       struct x64_reg from = {.ty = reg_ty_for_size(reg_class, to->size),
//                                  .idx = move.from.idx};

//       DEBUG_ASSERT(to->offset % to->size == 0,
//                    "offset was not a multiple of size");
//       size_t offset = to->offset / to->size;

//       struct instr *store = alloc_instr(state->func);
//       store->x64->ty = X64_INSTR_TY_STORE_IMM;
//       store->x64->store_imm =
//           (struct x64_store_imm){.addr = to->base,
//                                      .imm = offset,
//                                      .source = from,
//                                      .mode = X64_ADDRESSING_MODE_OFFSET};
//     } else {
//       // we move more of register than necessary here

//       struct instr *mov = alloc_instr(state->func);

//       switch (reg_class) {
//       case X64_REG_CLASS_GP: {
//         struct x64_reg source = {.ty = X64_REG_TY_X,
//                                      .idx = move.from.idx};
//         struct x64_reg dest = {.ty = X64_REG_TY_X, .idx = move.to.idx};
//         *mov->x64 = MOV_ALIAS(dest, source);
//         break;
//       }
//       case X64_REG_CLASS_FP: {
//         struct x64_reg source = {.ty = X64_REG_TY_D,
//                                      .idx = move.from.idx};
//         struct x64_reg dest = {.ty = X64_REG_TY_D, .idx = move.to.idx};
//         *mov->x64 = FP_MOV_ALIAS(dest, source);
//         break;
//       }
//       }
//     }
//   }
// }

// enum x64_reg_attr_flags reg_attr_flags(struct x64_reg reg) {
//   switch (reg.ty) {
//   case X64_REG_TY_NONE:
//     return X64_REG_ATTR_FLAG_NONE;
//   case X64_REG_TY_W:
//   case X64_REG_TY_X:
//     if (!reg.idx) {
//       return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG |
//              X64_REG_ATTR_FLAG_RET_REG;
//     }

//     if (reg.idx <= 7) {
//       return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG;
//     }

//     if (reg.idx < 18) {
//       return X64_REG_ATTR_FLAG_VOLATILE;
//     }

//     if (reg.idx == 18) {
//       return X64_REG_ATTR_FLAG_RESERVED;
//     }

//     if (reg.idx < 29) {
//       return X64_REG_ATTR_FLAG_NONE;
//     }

//     return X64_REG_ATTR_FLAG_RESERVED;
//   case X64_REG_TY_V:
//   case X64_REG_TY_Q:
//     if (!reg.idx) {
//       return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG |
//              X64_REG_ATTR_FLAG_RET_REG;
//     }

//     if (reg.idx <= 7) {
//       return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG;
//     }

//     if (reg.idx <= 15) {
//       return X64_REG_ATTR_FLAG_VOLATILE;
//     }

//     return X64_REG_ATTR_FLAG_NONE;
//   case X64_REG_TY_D:
//   case X64_REG_TY_S:
//   case X64_REG_TY_H:
//   case X64_REG_TY_B:
//     if (!reg.idx) {
//       return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG |
//              X64_REG_ATTR_FLAG_RET_REG;
//     }

//     if (reg.idx <= 7) {
//       return X64_REG_ATTR_FLAG_VOLATILE | X64_REG_ATTR_FLAG_ARG_REG;
//     }

//     return X64_REG_ATTR_FLAG_NONE;
//   }
// }

// // reg 9 is not part of calling convention
// // and all registers have already been saved
// // so this is always safe
// // same with reg 10, which we use if a register branch target is needed for
// // `blr`
// #define GP_TMP_REG_IDX ((size_t)9)
// #define BLR_REG_IDX ((size_t)10)
// #define ADDR_REG_IDX ((size_t)11)
// #define FP_TMP_REG_IDX ((size_t)16)

// // FIXME: apple do things differently!!!

// #define INTEGRAL_OR_PTRLIKE(var_ty)                                            \
//   (var_ty_is_integral((var_ty)) || (var_ty)->ty == IR_VAR_TY_TY_POINTER ||     \
//    (var_ty)->ty == IR_VAR_TY_TY_ARRAY)

// static void codegen_call_op(struct codegen_state *state, struct ir_op *op) {
//   invariant_assert(op->call.func_ty.ty == IR_VAR_TY_TY_FUNC, "non-func");

//   const struct ir_var_func_ty *func_ty = &op->call.func_ty.func;
//   const struct ir_var_ty *param_tys;

//   // note, it is important we use the func ty as the reference here for
//   // types as aggregates and similar will already have been turned into
//   // pointers. this does mean we need to explicitly handle pointers
//   // in the case of unspecified functions, we use `arg_var_tys` which is
//   // preserved
//   if (func_ty->num_params == op->call.num_args ||
//       (func_ty->flags & IR_VAR_FUNC_TY_FLAG_VARIADIC)) {
//     param_tys = func_ty->params;
//   } else {
//     param_tys = op->call.arg_var_tys;
//   }

//   invariant_assert(func_ty->num_params <= 8,
//                    "`%s` doesn't support more than 8 args yet", __func__);

//   // now we need to move each argument into its correct register
//   // it is possible there are no spare registers for this, and so we may need

//   bool variadics_on_stack;
//   switch (state->target->target_id) {
//   case TARGET_ID_X64_MACOS:
//     variadics_on_stack = true;
//     break;
//   case TARGET_ID_X64_LINUX:
//   case TARGET_ID_RV32I_UNKNOWN:
//     variadics_on_stack = false;
//     break;
//   }

//   struct vector *gp_move_from = vector_create(sizeof(struct location));
//   struct vector *gp_move_to = vector_create(sizeof(struct location));

//   struct vector *fp_move_from = vector_create(sizeof(struct location));
//   struct vector *fp_move_to = vector_create(sizeof(struct location));

//   struct vector *mem_copies = vector_create(sizeof(struct mem_copy));

//   size_t ngrn = 0;
//   size_t nsrn = 0;
//   size_t nsaa = 0;

//   if (!(op->call.target->flags & IR_OP_FLAG_CONTAINED)) {
//     struct x64_reg source = codegen_reg(op->call.target);

//     struct location from = {.idx = source.idx};

//     struct location to = {.idx = BLR_REG_IDX};

//     // need to move it into reg 10
//     vector_push_back(gp_move_from, &from);
//     vector_push_back(gp_move_to, &to);
//   }

//   size_t last_mem_loc = FIRST_MEM_LOC;

//   if (op->call.num_args) {
//     size_t num_normal_args = func_ty->num_params;

//     for (size_t i = 0; i < op->call.num_args; i++) {
//       struct x64_reg source = codegen_reg(op->call.args[i]);

//       enum x64_reg_attr_flags flags = reg_attr_flags(source);
//       if (flags & X64_REG_ATTR_FLAG_ARG_REG) {
//         // bad, might be overwritten during param preparation
//         // preemptively add 9 so it is not an arg register
//         // TODO: is this safe, and can it be improved?
//         struct x64_reg dest = {.ty = source.ty, .idx = source.idx + 9};
//         struct instr *mov = alloc_instr(state->func);

//         if (x64_reg_ty_is_gp(dest.ty)) {
//           *mov->x64 = MOV_ALIAS(dest, source);
//         } else {
//           *mov->x64 = FP_MOV_ALIAS(dest, source);
//         }

//         source = dest;
//       }
//     }

//     for (size_t i = 0; i < op->call.num_args; i++) {
//       struct x64_reg source = codegen_reg(op->call.args[i]);

//       enum x64_reg_attr_flags flags = reg_attr_flags(source);
//       if (flags & X64_REG_ATTR_FLAG_ARG_REG) {
//         source.idx += 9;
//       }

//       // hmm, we pretend all memory locations are different but they are not
//       // can gen_move_order accidentally overwrite things because of this?

//       if (!variadics_on_stack || i < num_normal_args ||
//           !(func_ty->flags & IR_VAR_FUNC_TY_FLAG_VARIADIC)) {

//         const struct ir_var_ty *var_ty = i < num_normal_args ? &param_tys[i]
//         : &op->call.args[i]->var_ty;

//         struct ir_var_ty_info info = var_ty_info(
//             state->ir->unit,
//             var_ty->ty == IR_VAR_TY_TY_ARRAY ? &IR_VAR_TY_POINTER : var_ty);

//         size_t num_hfa_members;
//         size_t hfa_member_size;

//         if (var_ty_is_fp(var_ty) && nsrn < 8) {
//           struct location from = {.idx = source.idx};
//           struct location to = {.idx = nsrn};
//           vector_push_back(fp_move_from, &from);
//           vector_push_back(fp_move_to, &to);
//           nsrn++;
//           continue;
//         } else if (try_get_hfa_info(state, var_ty, &num_hfa_members,
//                                     &hfa_member_size)) {
//           if (nsrn + num_hfa_members <= 8) {
//             for (size_t j = 0; j < num_hfa_members; j++) {
//               // given this is a composite, we assume `source` contains a
//               // pointer to it

//               struct mem_loc mem_loc = {.base = source,
//                                         .offset = hfa_member_size * j,
//                                         .size = hfa_member_size};

//               struct location loc = {
//                   .idx = MEM_LOC(),
//                   .metadata = arena_alloc_init(
//                       state->arena, sizeof(struct mem_loc), &mem_loc)};

//               struct location to = {.idx = nsrn++};
//               vector_push_back(fp_move_from, &loc);
//               vector_push_back(fp_move_to, &to);
//             }

//             continue;
//           }

//           nsrn = 8;
//           size_t nsaa_align = MAX(8, info.alignment);
//           size_t size = ROUND_UP(info.size, nsaa_align);

//           // TODO: overaligned HFAs and vectors
//           struct mem_loc mem_src = {.base = source, .offset = 0, .size =
//           size};

//           struct mem_loc mem_dest = {
//               .base = STACK_PTR_REG, .offset = nsaa, .size = size};

//           struct mem_copy copy = {.src = mem_src, .dest = mem_dest};

//           vector_push_back(mem_copies, &copy);

//           nsaa += size;

//           continue;

//         } else if ((INTEGRAL_OR_PTRLIKE(var_ty)) && info.size <= 8 &&
//                    ngrn <= 8) {
//           struct location from = {.idx = source.idx};
//           struct location to = {.idx = ngrn};
//           vector_push_back(gp_move_from, &from);
//           vector_push_back(gp_move_to, &to);
//           ngrn++;
//           continue;
//         }

//         if (info.alignment == 16) {
//           ngrn = (ngrn + 1) & 1;
//         }

//         if (var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
//           TODO("128 bit reg alloc");
//           // // lo to ngrn, hi to ngrn+1
//           // ngrn += 2;
//           // continue;
//         }

//         size_t dw_size = info.size / 8;
//         if (var_ty_is_aggregate(var_ty) && dw_size <= (8 - ngrn)) {
//           for (size_t j = 0; j <= dw_size; j++) {
//             // given this is a composite, we assume `source` contains a
//             // pointer to it

//             struct mem_loc mem_loc = {
//                 .base = source, .offset = 8 * j, .size = 8};

//             struct location loc = {
//                 .idx = MEM_LOC(),
//                 .metadata = arena_alloc_init(state->arena,
//                                              sizeof(struct mem_loc),
//                                              &mem_loc)};

//             struct location to = {.idx = ngrn++};
//             vector_push_back(gp_move_from, &loc);
//             vector_push_back(gp_move_to, &to);
//           }

//           continue;
//         }

//         ngrn = 8;
//         size_t nsaa_align = MAX(8, info.alignment);
//         nsaa = ROUND_UP(nsaa, nsaa_align);

//         if (var_ty_is_aggregate(var_ty)) {
//           struct mem_loc mem_src = {
//               .base = source, .offset = 0, .size = info.size};

//           struct mem_loc mem_dest = {
//               .base = STACK_PTR_REG, .offset = nsaa, .size = info.size};

//           struct mem_copy copy = {.src = mem_src, .dest = mem_dest};

//           vector_push_back(mem_copies, &copy);

//           nsaa += info.size;
//           continue;
//         }

//         size_t size = MAX(8, info.size);

//         struct mem_loc mem_loc = {
//             .base = STACK_PTR_REG, .offset = nsaa, .size = size};

//         nsaa += size;

//         struct location from = {.idx = source.idx};

//         struct location to = {
//             .idx = MEM_LOC(),
//             .metadata = arena_alloc_init(state->arena, sizeof(struct
//             mem_loc),
//                                          &mem_loc)};

//         if (INTEGRAL_OR_PTRLIKE(var_ty)) {
//           vector_push_back(gp_move_from, &from);
//           vector_push_back(gp_move_to, &to);
//         } else if (var_ty_is_fp(var_ty)) {
//           vector_push_back(fp_move_from, &from);
//           vector_push_back(fp_move_to, &to);
//         } else {
//           BUG("bad type");
//         }
//       } else {
//         struct ir_var_ty *var_ty = &op->call.args[i]->var_ty;

//         // this argument is variadic
//         // the stack slot this local must live in, in terms of 8 byte slots
//         size_t variadic_arg_idx = i - num_normal_args;

//         if (source.ty == X64_REG_TY_W) {
//           // because offsets are in terms of reg size, we need to double it
//           // for the 8 byte slots
//           variadic_arg_idx *= 2;
//         } else {
//           invariant_assert(var_ty_info(state->ir->unit, var_ty).size >= 8,
//                            "variadic arg with size < 8 has not been
//                            promoted");
//         }

//         struct instr *store = alloc_instr(state->func);

//         store->x64->ty = X64_INSTR_TY_STORE_IMM;
//         store->x64->str_imm =
//             (struct x64_store_imm){.source = source,
//                                        .addr = STACK_PTR_REG,
//                                        .imm = variadic_arg_idx,
//                                        .mode = X64_ADDRESSING_MODE_OFFSET};
//       }
//     }
//   }

//   struct move_set gp_arg_moves = gen_move_order(
//       state->ir->arena, vector_head(gp_move_from), vector_head(gp_move_to),
//       vector_length(gp_move_from), GP_TMP_REG_IDX);

//   struct move_set fp_arg_moves = gen_move_order(
//       state->ir->arena, vector_head(fp_move_from), vector_head(fp_move_to),
//       vector_length(fp_move_from), FP_TMP_REG_IDX);

//   codegen_moves(state, gp_arg_moves, X64_REG_CLASS_GP);
//   codegen_moves(state, fp_arg_moves, X64_REG_CLASS_FP);

//   // then handle mem copies
//   size_t num_copies = vector_length(mem_copies);
//   for (size_t i = 0; i < num_copies; i++) {
//     struct mem_copy *copy = vector_get(mem_copies, i);
//     struct mem_loc *from = &copy->src;
//     struct mem_loc *to = &copy->dest;

//     DEBUG_ASSERT(from->size == to->size, "mem cpy with different sizes");

//     codegen_mem_copy_volatile(state, from->base, from->offset, to->base,
//                               to->offset, to->size);
//   }

//   // now we generate the actual call

//   struct instr *instr = alloc_instr(state->func);
//   if (op->call.target->flags & IR_OP_FLAG_CONTAINED) {
//     instr->x64->ty = X64_INSTR_TY_BL;
//     instr->x64->bl = (struct x64_branch){.target = NULL};

//     instr->reloc = arena_alloc(state->func->unit->arena,
//     sizeof(*instr->reloc)); *instr->reloc = (struct relocation){
//         .ty = RELOCATION_TY_SINGLE,
//         .symbol_index = op->call.target->addr.glb->id,
//         .size = 2,
//         .address = 0,
//     };
//   } else {
//     // NOTE: `blr` seems to segfault on linux x64
//     instr->x64->ty = X64_INSTR_TY_BLR;
//     instr->x64->blr = (struct x64_branch_reg){
//         .target = {.ty = X64_REG_TY_X, .idx = BLR_REG_IDX}};
//   }

//   if (func_ty->ret_ty->ty == IR_VAR_TY_TY_NONE) {
//     return;
//   }

//   struct x64_reg dest = codegen_reg(op);

//   struct ir_var_ty *var_ty = func_ty->ret_ty;
//   struct ir_var_ty_info info = var_ty_info(state->ir->unit, var_ty);

//   if (INTEGRAL_OR_PTRLIKE(var_ty)) {
//     if (!is_return_reg(dest)) {
//       struct instr *mov = alloc_instr(state->func);

//       *mov->x64 = MOV_ALIAS(dest, return_reg_for_ty(dest.ty));
//     }
//   } else if (var_ty_is_fp(var_ty)) {
//     if (!is_return_reg(dest)) {
//       struct instr *mov = alloc_instr(state->func);

//       *mov->x64 = FP_MOV_ALIAS(dest, return_reg_for_ty(dest.ty));
//     }
//   } else {
//     struct ir_lcl *lcl = op->lcl;
//     size_t offset = lcl->offset;

//     // aggregate ty
//     size_t num_members;
//     size_t member_size;
//     if (lcl && try_get_hfa_info(state, var_ty, &num_members, &member_size) &&
//         num_members <= 4) {
//       enum x64_reg_ty reg_ty;
//       switch (member_size) {
//       case 8:
//         reg_ty = X64_REG_TY_D;
//         break;
//       case 4:
//         reg_ty = X64_REG_TY_S;
//         break;
//       case 2:
//         reg_ty = X64_REG_TY_H;
//         break;
//       }

//       for (size_t i = 0; i < num_members; i++) {
//         struct x64_reg source = {.ty = reg_ty, .idx = i};

//         struct instr *mov = alloc_instr(state->func);

//         mov->x64->ty = X64_INSTR_TY_STORE_IMM;
//         mov->x64->store_imm =
//             (struct x64_store_imm){.addr = STACK_PTR_REG,
//                                        .imm = (offset / member_size) + i,
//                                        .mode = X64_ADDRESSING_MODE_OFFSET,
//                                        .source = source};
//       }
//     } else if (info.size == 1) {
//       struct x64_reg source = {.ty = X64_REG_TY_W, .idx = 0};

//       struct instr *mov = alloc_instr(state->func);

//       mov->x64->ty = X64_INSTR_TY_STORE_BYTE_IMM;
//       mov->x64->strb_imm =
//           (struct x64_store_imm){.addr = STACK_PTR_REG,
//                                      .imm = offset,
//                                      .mode = X64_ADDRESSING_MODE_OFFSET,
//                                      .source = source};
//     } else if (info.size == 2) {
//       struct x64_reg source = {.ty = X64_REG_TY_W, .idx = 0};

//       struct instr *mov = alloc_instr(state->func);

//       mov->x64->ty = X64_INSTR_TY_STORE_HALF_IMM;
//       mov->x64->strh_imm =
//           (struct x64_store_imm){.addr = STACK_PTR_REG,
//                                      .imm = offset / 2,
//                                      .mode = X64_ADDRESSING_MODE_OFFSET,
//                                      .source = source};
//     } else if (info.size <= 4) {
//       struct x64_reg source = {.ty = X64_REG_TY_W, .idx = 0};

//       struct instr *mov = alloc_instr(state->func);

//       mov->x64->ty = X64_INSTR_TY_STORE_IMM;
//       mov->x64->str_imm =
//           (struct x64_store_imm){.addr = STACK_PTR_REG,
//                                      .imm = offset / 4,
//                                      .mode = X64_ADDRESSING_MODE_OFFSET,
//                                      .source = source};
//     } else if (info.size <= 8) {
//       struct x64_reg source = {.ty = X64_REG_TY_X, .idx = 0};

//       struct instr *mov = alloc_instr(state->func);

//       mov->x64->ty = X64_INSTR_TY_STORE_IMM;
//       mov->x64->str_imm =
//           (struct x64_store_imm){.addr = STACK_PTR_REG,
//                                      .imm = offset / 8,
//                                      .mode = X64_ADDRESSING_MODE_OFFSET,
//                                      .source = source};
//     } else if (info.size <= 16) {
//       struct x64_reg source0 = {.ty = X64_REG_TY_X, .idx = 0};
//       struct x64_reg source1 = {.ty = X64_REG_TY_X, .idx = 1};

//       struct instr *mov = alloc_instr(state->func);

//       mov->x64->ty = X64_INSTR_TY_STORE_PAIR_IMM;
//       mov->x64->store_pair_imm = (struct x64_store_pair_imm){
//           .addr = STACK_PTR_REG,
//           .imm = offset / 8,
//           .mode = X64_ADDRESSING_MODE_OFFSET,
//           .source = {source0, source1}};
//     } else {
//       TODO("larger than 16 byte types");
//     }
//   }
// }

// static void codegen_params(struct codegen_state *state) {
//   // we effectively do the reverse of what we do in `codegen_call_op`
//   // putting params in correct registers
//   // and spilling aggregates to stack
//   // we could try and merge the code in some way?
//   // we also do it again, below, to calculate how much arg space is needed
//   for
//   // prologue

//   // important note:
//   // to prevent overwriting things, we put all our locals beneath the
//   arguments
//   // for the method

//   // FIXME: this currently relies heavily on IR build
//   // specifically, each aggregate must produce exactly one local
//   // and do it in order of params

//   struct ir_var_func_ty func_ty = state->ir->func_ty;

//   size_t ngrn = 0;
//   size_t nsrn = 0;
//   size_t nsaa = 0;

//   struct vector *gp_move_from = vector_create(sizeof(struct location));
//   struct vector *gp_move_to = vector_create(sizeof(struct location));

//   struct vector *fp_move_from = vector_create(sizeof(struct location));
//   struct vector *fp_move_to = vector_create(sizeof(struct location));

//   struct vector *mem_copies = vector_create(sizeof(struct mem_copy));

//   struct ir_stmt *param_stmt = state->ir->first->first;
//   struct ir_op *param_op = NULL;
//   if (param_stmt && param_stmt->first &&
//       param_stmt->first->ty == IR_OP_TY_MOV &&
//       (param_stmt->first->flags & IR_OP_FLAG_PARAM)) {
//     param_op = param_stmt->first;
//   }

//   struct ir_lcl *lcl = state->ir->first_local;

//   size_t last_mem_loc = FIRST_MEM_LOC;

//   for (size_t i = 0; i < func_ty.num_params; i++) {
//     const struct ir_var_ty *var_ty = &func_ty.params[i];
//     struct ir_var_ty_info info = var_ty_info(
//         state->ir->unit,
//         var_ty->ty == IR_VAR_TY_TY_ARRAY ? &IR_VAR_TY_POINTER : var_ty);

//     size_t offset;
//     struct x64_reg source;

//     if (lcl) {
//       offset = lcl->offset;
//     }

//     if (param_op) {
//       source = codegen_reg(param_op);
//     }

//     size_t num_hfa_members;
//     size_t hfa_member_size;

//     if (var_ty_is_fp(var_ty) && nsrn < 8) {
//       struct location from = {.idx = nsrn};
//       struct location to = {.idx = source.idx};
//       vector_push_back(fp_move_from, &from);
//       vector_push_back(fp_move_to, &to);
//       nsrn++;

//       param_op = param_op->succ;
//       continue;
//     } else if (try_get_hfa_info(state, var_ty, &num_hfa_members,
//                                 &hfa_member_size)) {
//       if (nsrn + num_hfa_members <= 8) {
//         for (size_t j = 0; j < num_hfa_members; j++) {
//           // given this is a composite, we assume `source` contains a
//           // pointer to it

//           struct location from = {.idx = nsrn++};

//           struct mem_loc mem_loc = {.base = STACK_PTR_REG,
//                                     .offset = hfa_member_size * j,
//                                     .size = hfa_member_size};

//           struct location to = {
//               .idx = MEM_LOC(),
//               .metadata = arena_alloc_init(state->arena, sizeof(struct
//               mem_loc),
//                                            &mem_loc)};

//           vector_push_back(fp_move_from, &from);
//           vector_push_back(fp_move_to, &to);
//         }

//         lcl = lcl->succ;
//         continue;
//       }

//       nsrn = 8;
//       size_t nsaa_align = MAX(8, info.alignment);
//       size_t size = ROUND_UP(info.size, nsaa_align);

//       lcl->offset = state->prologue_info.stack_size + nsaa;

//       nsaa += size;
//       lcl = lcl->succ;
//       continue;

//     } else if ((INTEGRAL_OR_PTRLIKE(var_ty)) && info.size <= 8 && ngrn <= 8)
//     {
//       struct location from = {.idx = ngrn};
//       struct location to = {.idx = source.idx};
//       vector_push_back(gp_move_from, &from);
//       vector_push_back(gp_move_to, &to);
//       ngrn++;
//       param_op = param_op->succ;
//       continue;
//     }

//     if (info.alignment == 16) {
//       ngrn = (ngrn + 1) & 1;
//     }

//     if (var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
//       TODO("128 bit reg alloc");
//       // // lo to ngrn, hi to ngrn+1
//       // ngrn += 2;
//       // continue;
//     }

//     size_t dw_size = (info.size + 7) / 8;
//     if (var_ty_is_aggregate(var_ty) && dw_size <= (8 - ngrn)) {
//       for (size_t j = 0; j < dw_size; j++) {
//         // given this is a composite, we assume `source` contains a
//         // pointer to it

//         struct mem_loc mem_loc = {
//             .base = STACK_PTR_REG, .offset = offset + (8 * j), .size = 8};

//         struct location to = {
//             .idx = MEM_LOC(),
//             .metadata = arena_alloc_init(state->arena, sizeof(struct
//             mem_loc),
//                                          &mem_loc)};

//         struct location from = {.idx = ngrn++};
//         vector_push_back(gp_move_from, &from);
//         vector_push_back(gp_move_to, &to);
//       }

//       lcl = lcl->succ;
//       continue;
//     }

//     ngrn = 8;
//     size_t nsaa_align = MAX(8, info.alignment);
//     nsaa = ROUND_UP(nsaa, nsaa_align);

//     if (var_ty_is_aggregate(var_ty)) {
//       lcl->offset = state->prologue_info.stack_size + nsaa;

//       nsaa += info.size;

//       lcl = lcl->succ;
//       continue;
//     }

//     size_t size = MAX(8, info.size);

//     struct mem_loc mem_loc = {
//         .base = STACK_PTR_REG, .offset = nsaa, .size = size};

//     nsaa += size;

//     struct location to = {.idx = source.idx};

//     struct location from = {
//         .idx = MEM_LOC(),
//         .metadata =
//             arena_alloc_init(state->arena, sizeof(struct mem_loc),
//             &mem_loc)};

//     param_op = param_op->succ;

//     if (INTEGRAL_OR_PTRLIKE(var_ty)) {
//       vector_push_back(gp_move_from, &from);
//       vector_push_back(gp_move_to, &to);
//     } else if (var_ty_is_fp(var_ty)) {
//       vector_push_back(fp_move_from, &from);
//       vector_push_back(fp_move_to, &to);
//     } else {
//       BUG("bad type");
//     }
//   }

//   struct move_set gp_arg_moves = gen_move_order(
//       state->ir->arena, vector_head(gp_move_from), vector_head(gp_move_to),
//       vector_length(gp_move_from), GP_TMP_REG_IDX);

//   struct move_set fp_arg_moves = gen_move_order(
//       state->ir->arena, vector_head(fp_move_from), vector_head(fp_move_to),
//       vector_length(fp_move_from), FP_TMP_REG_IDX);

//   codegen_moves(state, gp_arg_moves, X64_REG_CLASS_GP);
//   codegen_moves(state, fp_arg_moves, X64_REG_CLASS_FP);

//   // then handle mem copies
//   size_t num_copies = vector_length(mem_copies);
//   for (size_t i = 0; i < num_copies; i++) {
//     struct mem_copy *copy = vector_get(mem_copies, i);
//     struct mem_loc *from = &copy->src;
//     struct mem_loc *to = &copy->dest;

//     DEBUG_ASSERT(from->size == to->size, "mem cpy with different sizes");

//     codegen_mem_copy_volatile(state, from->base, from->offset, to->base,
//                               to->offset, to->size);
//   }
// }

// static size_t calc_arg_stack_space(struct codegen_state *state,
//                                    struct ir_var_func_ty func_ty,
//                                    struct ir_op *op) {
//   size_t ngrn = 0;
//   size_t nsrn = 0;
//   size_t nsaa = 0;

//   for (size_t i = 0; i < func_ty.num_params; i++) {
//     const struct ir_var_ty *var_ty = &func_ty.params[i];
//     struct ir_var_ty_info info = var_ty_info(state->ir->unit, var_ty);

//     size_t num_hfa_members;
//     size_t hfa_member_size;

//     if (var_ty_is_fp(var_ty) && nsrn < 8) {
//       nsrn++;
//       continue;
//     } else if (try_get_hfa_info(state, var_ty, &num_hfa_members,
//                                 &hfa_member_size)) {
//       if (nsrn + num_hfa_members <= 8) {
//         nsrn += num_hfa_members;
//         continue;
//       }

//       nsrn = 8;
//       size_t nsaa_align = MAX(8, info.alignment);
//       size_t size = ROUND_UP(info.size, nsaa_align);

//       nsaa += size;
//       continue;

//     } else if ((INTEGRAL_OR_PTRLIKE(var_ty)) && info.size <= 8 && ngrn <= 8)
//     {
//       ngrn++;
//       continue;
//     }

//     if (info.alignment == 16) {
//       ngrn = (ngrn + 1) & 1;
//     }

//     if (var_ty_is_integral(var_ty) && info.size == 16 && ngrn < 7) {
//       ngrn += 2;
//       continue;
//     }

//     size_t dw_size = info.size / 8;
//     if (var_ty_is_aggregate(var_ty) && dw_size <= (8 - ngrn)) {
//       ngrn += dw_size;
//       continue;
//     }

//     ngrn = 8;
//     size_t nsaa_align = MAX(8, info.alignment);
//     nsaa = ROUND_UP(nsaa, nsaa_align);

//     if (var_ty_is_aggregate(var_ty)) {
//       nsaa += info.size;
//       continue;
//     }

//     size_t size = MAX(8, info.size);

//     nsaa += size;
//   }

//   if (op) {
//     DEBUG_ASSERT(op->ty == IR_OP_TY_CALL, "needs call");
//     for (size_t i = func_ty.num_params; i < op->call.num_args; i++) {
//       struct ir_var_ty var_ty = op->call.args[i]->var_ty;
//       struct ir_var_ty_info info = var_ty_info(state->ir->unit, &var_ty);

//       size_t size = ROUND_UP(info.size, 8);
//       nsaa += size;
//     }
//   }

//   return nsaa;
// }

// // FIXME: do things more smarter
// #define X64_TARGET (X64_MACOS_TARGET)

// static void codegen_prologue(struct codegen_state *state) {
//   struct ir_func *ir = state->ir;

//   // TODO: super inefficient
//   bool nonvolatile_registers_used =
//       (bitset_lzcnt(ir->reg_usage.gp_registers_used) <
//        X64_TARGET.reg_info.gp_registers.num_nonvolatile) ||
//       (bitset_lzcnt(ir->reg_usage.fp_registers_used) <
//        X64_TARGET.reg_info.fp_registers.num_nonvolatile);

//   bool leaf = !(nonvolatile_registers_used || ir->num_locals ||
//                 ir->flags & IR_FUNC_FLAG_MAKES_CALL);

//   size_t stack_size = state->stack_args_size;
//   stack_size =
//       ROUND_UP(stack_size + ir->total_locals_size, X64_STACK_ALIGNMENT);

//   const size_t LR_OFFSET = 2;
//   struct x64_prologue_info info = {.prologue_generated = !leaf,
//                                        .saved_gp_registers = 0,
//                                        .saved_fp_registers = 0,
//                                        .save_start = stack_size,
//                                        .lr_offset = LR_OFFSET,
//                                        .stack_size = stack_size};

//   if (!info.prologue_generated) {
//     state->prologue_info = info;
//     return;
//   }

//   struct bitset_iter gp_iter =
//       bitset_iter(ir->reg_usage.gp_registers_used,
//                   X64_TARGET.reg_info.gp_registers.num_nonvolatile, true);

//   size_t i;
//   while (bitset_iter_next(&gp_iter, &i)) {
//     info.saved_gp_registers |= (1 << i);
//     info.stack_size += 8;
//   }

//   struct bitset_iter fp_iter =
//       bitset_iter(ir->reg_usage.fp_registers_used,
//                   X64_TARGET.reg_info.fp_registers.num_nonvolatile, true);

//   while (bitset_iter_next(&fp_iter, &i)) {
//     info.saved_fp_registers |= (1 << i);
//     info.stack_size += 8;
//   }

//   // need to save x29 and x30
//   info.stack_size += 16;

//   struct instr *save_lr_x30 = alloc_instr(state->func);
//   save_lr_x30->x64->ty = X64_INSTR_TY_STORE_PAIR_IMM;
//   save_lr_x30->x64->stp_imm = (struct x64_store_pair_imm){
//       .mode = X64_ADDRESSING_MODE_PREINDEX,
//       .imm = -info.lr_offset,
//       .source = {FRAME_PTR_REG, RET_PTR_REG},
//       .addr = STACK_PTR_REG,
//   };

//   info.stack_size = ROUND_UP(info.stack_size, X64_STACK_ALIGNMENT);

//   // also save stack pointer into frame pointer as required by ABI
//   // `mov x29, sp` is illegal (encodes as `mov x29, xzr`)
//   // so `add x29, sp, #x` is used instead
//   struct instr *save_x29 = alloc_instr(state->func);
//   save_x29->x64->ty = X64_INSTR_TY_ADD_IMM;
//   save_x29->x64->add_imm = (struct x64_addsub_imm){
//       .dest = (struct x64_reg){.ty = X64_REG_TY_X, .idx = 29},
//       .source = STACK_PTR_REG,
//       .imm = info.lr_offset * 8,
//       .shift = 0};

//   size_t stack_to_sub = info.stack_size - 16; // from the pre-index lr
//   if (stack_to_sub) {
//     if (stack_to_sub > MAX_IMM_SIZE) {
//       codegen_sub_imm(state, STACK_PTR_REG, STACK_PTR_REG, stack_to_sub);
//     } else {
//       struct instr *sub_stack = alloc_instr(state->func);
//       sub_stack->x64->ty = X64_INSTR_TY_SUB_IMM;
//       sub_stack->x64->sub_imm =
//           (struct x64_addsub_imm){.dest = STACK_PTR_REG,
//                                       .source = STACK_PTR_REG,
//                                       .imm = stack_to_sub,
//                                       .shift = 0};
//     }

//     size_t save_idx = 0;

//     struct bitset_iter gp_reg_iter =
//         bitset_iter(ir->reg_usage.gp_registers_used,
//                     X64_TARGET.reg_info.gp_registers.num_nonvolatile, true);

//     size_t idx;
//     while (bitset_iter_next(&gp_reg_iter, &idx)) {
//       // guaranteed to be mod 8
//       size_t offset = (info.save_start / 8) + save_idx++;

//       struct instr *save = alloc_instr(state->func);
//       save->x64->ty = X64_INSTR_TY_STORE_IMM;
//       save->x64->str_imm = (struct x64_store_imm){
//           .mode = X64_ADDRESSING_MODE_OFFSET,
//           .imm = offset,
//           .source = (struct x64_reg){.ty = X64_REG_TY_X,
//                                          .idx = translate_reg_idx(
//                                              idx, IR_REG_TY_INTEGRAL)},
//           .addr = STACK_PTR_REG,
//       };
//     }

//     struct bitset_iter fp_reg_iter =
//         bitset_iter(ir->reg_usage.fp_registers_used,
//                     X64_TARGET.reg_info.fp_registers.num_nonvolatile, true);

//     while (bitset_iter_next(&fp_reg_iter, &idx)) {
//       // guaranteed to be mod 8
//       size_t offset = (info.save_start / 8) + save_idx++;

//       struct instr *save = alloc_instr(state->func);
//       save->x64->ty = X64_INSTR_TY_STORE_IMM;
//       save->x64->str_imm = (struct x64_store_imm){
//           .mode = X64_ADDRESSING_MODE_OFFSET,
//           .imm = offset,
//           .source = (struct x64_reg){.ty = X64_REG_TY_D,
//                                          .idx = translate_reg_idx(
//                                              idx, IR_REG_TY_INTEGRAL)},
//           .addr = STACK_PTR_REG,
//       };
//     }
//   }

//   state->prologue_info = info;
// }

// static void codegen_epilogue(struct codegen_state *state) {
//   const struct x64_prologue_info *prologue_info = &state->prologue_info;

//   if (!prologue_info->prologue_generated) {
//     return;
//   }

//   unsigned long max_gp_saved = sizeof(prologue_info->saved_gp_registers) * 8
//   -
//                                lzcnt(prologue_info->saved_gp_registers);

//   size_t save_idx = 0;
//   for (size_t i = 0; i < max_gp_saved; i++) {
//     // FIXME: loop should start at i=first non volatile
//     if (!NTH_BIT(prologue_info->saved_gp_registers, i)) {
//       continue;
//     }

//     size_t offset = (prologue_info->save_start / 8) + save_idx++;

//     struct instr *restore = alloc_instr(state->func);
//     restore->x64->ty = X64_INSTR_TY_LOAD_IMM;
//     restore->x64->ldr_imm = (struct x64_load_imm){
//         .mode = X64_ADDRESSING_MODE_OFFSET,
//         .imm = offset,
//         .dest = (struct x64_reg){.ty = X64_REG_TY_X,
//                                      .idx = translate_reg_idx(
//                                          i, IR_REG_TY_INTEGRAL)},
//         .addr = STACK_PTR_REG,
//     };
//   }

//   unsigned long max_fp_saved = sizeof(prologue_info->saved_fp_registers) * 8
//   -
//                                lzcnt(prologue_info->saved_fp_registers);

//   save_idx = 0;
//   for (size_t i = 0; i < max_fp_saved; i++) {
//     // FIXME: loop should start at i=first non volatile
//     if (!NTH_BIT(prologue_info->saved_fp_registers, i)) {
//       continue;
//     }

//     size_t offset = (prologue_info->save_start / 8) + save_idx++;

//     struct instr *restore = alloc_instr(state->func);
//     restore->x64->ty = X64_INSTR_TY_LOAD_IMM;
//     restore->x64->ldr_imm = (struct x64_load_imm){
//         .mode = X64_ADDRESSING_MODE_OFFSET,
//         .imm = offset,
//         .dest = (struct x64_reg){.ty = X64_REG_TY_D,
//                                      .idx = translate_reg_idx(
//                                          i, IR_REG_TY_INTEGRAL)},
//         .addr = STACK_PTR_REG,
//     };
//   }

//   size_t stack_to_add = prologue_info->stack_size - 16;
//   if (stack_to_add) {
//     if (stack_to_add > MAX_IMM_SIZE) {
//       codegen_add_imm(state, STACK_PTR_REG, STACK_PTR_REG, stack_to_add);
//     } else {
//       struct instr *add_stack = alloc_instr(state->func);
//       add_stack->x64->ty = X64_INSTR_TY_ADD_IMM;
//       add_stack->x64->add_imm =
//           (struct x64_addsub_imm){.dest = STACK_PTR_REG,
//                                       .source = STACK_PTR_REG,
//                                       .imm = stack_to_add,
//                                       .shift = 0};
//     }
//   }

//   struct instr *restore_lr_x30 = alloc_instr(state->func);
//   restore_lr_x30->x64->ty = X64_INSTR_TY_LOAD_PAIR_IMM;
//   restore_lr_x30->x64->ldp_imm = (struct x64_load_pair_imm){
//       .mode = X64_ADDRESSING_MODE_POSTINDEX,
//       .imm = prologue_info->lr_offset,
//       .dest = {FRAME_PTR_REG, RET_PTR_REG},
//       .addr = STACK_PTR_REG,
//   };
// }

#define INTEGRAL_OR_PTRLIKE(var_ty)                                            \
  (var_ty_is_integral((var_ty)) || (var_ty)->ty == IR_VAR_TY_TY_POINTER ||     \
   (var_ty)->ty == IR_VAR_TY_TY_ARRAY)

static void codegen_ret_op(struct codegen_state *state, struct ir_op *op) {
  if (op->ret.value && op->ret.value->ty != IR_OP_TY_CALL) {
    struct x64_reg source = codegen_reg(op->ret.value);

    struct ir_var_ty *var_ty = state->ir->func_ty.ret_ty;
    // struct ir_var_ty_info info = var_ty_info(state->ir->unit, var_ty);

    if (INTEGRAL_OR_PTRLIKE(var_ty)) {
      if (!is_return_reg(source)) {
        struct instr *mov = alloc_instr(state->func);

        *mov->x64 = (struct x64_instr){.ty = X64_INSTR_TY_MOV_REG,
                                       .mov_reg = {
                                           .dest = return_reg_for_ty(source.ty),
                                           .source = source
                                       }};
      }
    } else if (var_ty_is_fp(var_ty)) {
      TODO("FP return x64");
      // if (!is_return_reg(source)) {
      //   struct instr *mov = alloc_instr(state->func);

      //   *mov->x64 = FP_MOV_ALIAS(return_reg_for_ty(source.ty), source);
      // }
    } else {
      TODO("big return x64");
    }
  }

  // codegen_epilogue(state);

  struct instr *instr = alloc_instr(state->func);

  instr->x64->ty = X64_INSTR_TY_RET;
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
    //   case IR_OP_TY_BITFIELD_INSERT:
    //     codegen_bitfield_insert(state, op);
    //     break;
    //   case IR_OP_TY_BITFIELD_EXTRACT:
    //     codegen_bitfield_extract(state, op);
    //     break;
    //   case IR_OP_TY_LOAD:
    //     codegen_load_op(state, op);
    //     break;
    //   case IR_OP_TY_STORE:
    //     codegen_store_op(state, op);
    //     break;
    //   case IR_OP_TY_ADDR: {
    //     codegen_addr_op(state, op);
    //     break;
    //   }
    //   case IR_OP_TY_BR_COND: {
    //     codegen_br_cond_op(state, op);
    //     break;
    //   }
    //   case IR_OP_TY_BR: {
    //     codegen_br_op(state, op);
    //     break;
    //   }
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
    //   case IR_OP_TY_CAST_OP: {
    //     codegen_cast_op(state, op);
    //     break;
    //   }
    //   case IR_OP_TY_CALL: {
    //     codegen_call_op(state, op);
    //     break;
    //   }
  case IR_OP_TY_RET: {
    codegen_ret_op(state, op);
    break;
  }
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

// struct check_reg_type_data {
//   // used so we know when the instruction changes
//   const struct codegen_entry *entry;
//   struct instr *last;

//   enum x64_reg_ty reg_ty;
// };

// static void check_reg_type_callback(struct instr *instr, struct x64_reg reg,
//                                     enum x64_reg_usage_ty usage_ty,
//                                     void *metadata) {
//   struct check_reg_type_data *data = metadata;

//   if (usage_ty == X64_REG_USAGE_TY_DEREF) {
//     // deref only makes sense on an X register, and is ignored for
//     comparisons
//     // to other registers so back out early
//     invariant_assert(
//         reg.ty == X64_REG_TY_X,
//         "entry %zu: usage DEREF only makes sense with REG_TY_X in %zu",
//         data->entry->glb_id, instr->id);
//     return;
//   }

//   if (instr != data->last) {
//     // new
//     data->reg_ty = reg.ty;
//   } else {
//     // check reg types are the same within instructions
//     // FMOV is an exception as it can move GP<->FP

//     if (instr->x64->ty == X64_INSTR_TY_FMOV) {
//       size_t cur_size = reg_size(reg.ty);
//       size_t last_size = reg_size(data->reg_ty);
//       invariant_assert(
//           cur_size == last_size,
//           "entry %zu: expected `fmov` %zu to have same size registers "
//           "(expected %zu found %zu)",
//           data->entry->glb_id, instr->id, cur_size, last_size);
//     } else if (instr->x64->ty == X64_INSTR_TY_FCVT) {
//       invariant_assert(x64_reg_ty_is_fp(reg.ty) &&
//                            x64_reg_ty_is_fp(data->reg_ty),
//                        "entry %zu: expected `fcvt` %zu to have all registers
//                        " "floating-point", data->entry->glb_id, instr->id);
//     } else if (instr->x64->ty == X64_INSTR_TY_UCVTF ||
//                instr->x64->ty == X64_INSTR_TY_SCVTF) {
//       invariant_assert(
//           x64_reg_ty_is_fp(reg.ty) != x64_reg_ty_is_fp(data->reg_ty),
//           "entry %zu: expected `ucvtf`/`scvtf` %zu to have one fp register "
//           "and one gp register",
//           data->entry->glb_id, instr->id);
//     } else {
//       invariant_assert(
//           reg.ty == data->reg_ty,
//           "entry %zu: reg ty mismatch in %zu (expected %d found %d)",
//           data->entry->glb_id, instr->id, data->reg_ty, reg.ty);
//     }
//   }

//   data->last = instr;
// }

// static int sort_entries_by_id(const void *a, const void *b) {
//   const struct codegen_entry *l = a;
//   const struct codegen_entry *r = b;

//   if (l->glb_id > r->glb_id) {
//     return 1;
//   } else if (l->glb_id == r->glb_id) {
//     return 0;
//   } else {
//     return -1;
//   }
// }

// static void codegen_write_var_value(struct ir_unit *iru, struct vector
// *relocs,
//                                     size_t offset, struct ir_var_value
//                                     *value, char *data) {
//   if (!value || value->ty == IR_VAR_VALUE_TY_ZERO) {
//     return;
//   }

//   switch (value->var_ty.ty) {
//   case IR_VAR_TY_TY_NONE:
//   case IR_VAR_TY_TY_VARIADIC:
//     break;
//   case IR_VAR_TY_TY_PRIMITIVE: {
//     switch (value->var_ty.primitive) {
//     case IR_VAR_PRIMITIVE_TY_I8:
//       DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT, "expected int");
//       memcpy(data, &value->int_value, 1);
//       break;
//     case IR_VAR_PRIMITIVE_TY_F16:
//     case IR_VAR_PRIMITIVE_TY_I16:
//       DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
//                        value->ty == IR_VAR_VALUE_TY_FLT,
//                    "expected int/flt");
//       memcpy(data, &value->int_value, 2);
//       break;
//     case IR_VAR_PRIMITIVE_TY_I32:
//     case IR_VAR_PRIMITIVE_TY_F32:
//       DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
//                        value->ty == IR_VAR_VALUE_TY_FLT,
//                    "expected int/flt");
//       memcpy(data, &value->int_value, 4);
//       break;
//     case IR_VAR_PRIMITIVE_TY_I64:
//     case IR_VAR_PRIMITIVE_TY_F64:
//       DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_INT ||
//                        value->ty == IR_VAR_VALUE_TY_FLT,
//                    "expected int/flt");
//       memcpy(data, &value->int_value, sizeof(unsigned long));
//       break;
//     }
//     break;
//   }

//   case IR_VAR_TY_TY_FUNC:
//     BUG("func can not have data as a global var");

//     // FIXME: some bugs here around using compiler-ptr-size when we mean to
//     use
//     // target-ptr-size

//   case IR_VAR_TY_TY_POINTER:
//   case IR_VAR_TY_TY_ARRAY:
//     switch (value->ty) {
//     case IR_VAR_VALUE_TY_ZERO:
//     case IR_VAR_VALUE_TY_FLT:
//       BUG("doesn't make sense");
//     case IR_VAR_VALUE_TY_ADDR: {
//       struct relocation reloc = {.ty = RELOCATION_TY_POINTER,
//                                  .size = 3,
//                                  .address = offset,
//                                  .offset = value->addr.offset,
//                                  .symbol_index = value->addr.glb->id};

//       vector_push_back(relocs, &reloc);

//       memcpy(data, &value->addr.offset, sizeof(void *));
//       break;
//     }
//     case IR_VAR_VALUE_TY_INT:
//       memcpy(data, &value->int_value, sizeof(void *));
//       break;
//     case IR_VAR_VALUE_TY_STR:
//       // FIXME: !!!! doesn't work with string literals containing null char
//       strcpy(data, value->str_value);
//       break;
//     case IR_VAR_VALUE_TY_VALUE_LIST:
//       for (size_t i = 0; i < value->value_list.num_values; i++) {
//         size_t value_offset = value->value_list.offsets[i];
//         codegen_write_var_value(iru, relocs, offset + value_offset,
//                                 &value->value_list.values[i],
//                                 &data[value_offset]);
//       }
//       break;
//     }
//     break;

//   case IR_VAR_TY_TY_STRUCT:
//   case IR_VAR_TY_TY_UNION:
//     DEBUG_ASSERT(value->ty == IR_VAR_VALUE_TY_VALUE_LIST,
//                  "expected value list");
//     for (size_t i = 0; i < value->value_list.num_values; i++) {
//       size_t field_offset = value->value_list.offsets[i];
//       codegen_write_var_value(iru, relocs, offset + field_offset,
//                               &value->value_list.values[i],
//                               &data[field_offset]);
//     }
//   }
// }

// static struct codegen_entry codegen_var_data(struct ir_unit *ir, size_t id,
//                                              const char *name,
//                                              struct ir_var *var) {
//   switch (var->ty) {
//   case IR_VAR_TY_STRING_LITERAL: {
//     BUG("str literal should have been lowered seperately");
//   }
//   case IR_VAR_TY_CONST_DATA:
//   case IR_VAR_TY_DATA: {
//     struct ir_var_ty_info info = var_ty_info(ir, &var->var_ty);

//     // TODO: this leak
//     struct vector *relocs = vector_create(sizeof(struct relocation));

//     size_t len = info.size;

//     char *data = arena_alloc(ir->arena, len);
//     memset(data, 0, len);

//     codegen_write_var_value(ir, relocs, 0, &var->value, data);

//     // TODO: handle const data
//     return (struct codegen_entry){
//         .ty = CODEGEN_ENTRY_TY_DATA,
//         .glb_id = id,
//         .alignment = info.alignment,
//         .name = name,
//         .data = (struct codegen_data){.data = data,
//                                       .len_data = len,
//                                       .relocs = vector_head(relocs),
//                                       .num_relocs = vector_length(relocs)}};
//   }
//   }
// }

struct codegen_unit *x64_codegen(struct ir_unit *ir) {
  struct codegen_unit *unit = arena_alloc(ir->arena, sizeof(*unit));
  *unit = (struct codegen_unit){
      .ty = CODEGEN_UNIT_TY_X64,
      .instr_size = sizeof(struct x64_instr),
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
          // unit->entries[i] = codegen_var_data(ir, glb->id, name, glb->var);
          break;
        }
        break;
      }
      case IR_GLB_TY_FUNC: {
        struct ir_func *ir_func = glb->func;

        clear_metadata(ir_func);

        unit->entries[i] = (struct codegen_entry){
            .ty = CODEGEN_ENTRY_TY_FUNC,
            .alignment = X64_FUNCTION_ALIGNMENT,
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
        };
        // .ssp = {.ty = X64_REG_TY_X, .idx = 28}};

        // state.stack_args_size = 0;

        struct ir_basicblock *basicblock = ir_func->first;
        while (basicblock) {
          struct ir_stmt *stmt = basicblock->first;

          while (stmt) {
            struct ir_op *op = stmt->first;

            while (op) {
              if (op->ty == IR_OP_TY_CALL) {
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

        // func->prologue = state.prologue_info.prologue_generated;
        // func->stack_size = state.prologue_info.stack_size;

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

  // qsort(unit->entries, unit->num_entries, sizeof(struct codegen_entry),
  //       sort_entries_by_id);

  if (log_enabled()) {
    x64_debug_print_codegen(stderr, unit);
  }

  // now do sanity checks
  for (size_t i = 0; i < unit->num_entries; i++) {
    const struct codegen_entry *entry = &unit->entries[i];

    if (entry->ty == CODEGEN_ENTRY_TY_FUNC) {
      // // codegen is now done
      // // do some basic sanity checks
      // struct check_reg_type_data data = {
      //     .entry = entry, .last = NULL, .reg_ty = 0};
      // walk_regs(&entry->func, check_reg_type_callback, &data);
    }
  }

  return unit;
}

// static char reg_prefix(struct x64_reg reg) {
//   switch (reg.ty) {
//   case X64_REG_TY_NONE:
//     return '!';
//   case X64_REG_TY_W:
//     return 'w';
//   case X64_REG_TY_X:
//     return 'x';
//   case X64_REG_TY_V:
//     return 'b';
//   case X64_REG_TY_Q:
//     return 'q';
//   case X64_REG_TY_D:
//     return 'd';
//   case X64_REG_TY_S:
//     return 's';
//   case X64_REG_TY_H:
//     return 'h';
//   case X64_REG_TY_B:
//     return 'b';
//   }
// }

// void walk_regs(const struct codegen_function *func, walk_regs_callback *cb,
//                void *metadata) {
//   struct instr *instr = func->first;

//   while (instr) {
//     enum x64_instr_class class = instr_class(instr->x64->ty);
//     switch (class) {
//     case X64_INSTR_CLASS_NOP:
//       break;
//     case X64_INSTR_CLASS_LOGICAL_REG: {
//       struct x64_logical_reg logical_reg = instr->x64->logical_reg;
//       cb(instr, logical_reg.lhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, logical_reg.rhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, logical_reg.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_LOGICAL_IMM: {
//       struct x64_logical_imm logical_imm = instr->x64->logical_imm;
//       cb(instr, logical_imm.source, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, logical_imm.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_ADDSUB_REG: {
//       struct x64_addsub_reg addsub_reg = instr->x64->addsub_reg;
//       cb(instr, addsub_reg.lhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, addsub_reg.rhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, addsub_reg.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_ADDSUB_EXT: {
//       struct x64_addsub_ext addsub_ext = instr->x64->addsub_ext;
//       cb(instr, addsub_ext.lhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, addsub_ext.rhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, addsub_ext.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_ADDSUB_IMM: {
//       struct x64_addsub_imm addsub_imm = instr->x64->addsub_imm;
//       cb(instr, addsub_imm.source, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, addsub_imm.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_ADDR_IMM: {
//       struct x64_addr_imm addr_imm = instr->x64->addr_imm;
//       cb(instr, addr_imm.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_BITFIELD: {
//       struct x64_bitfield bitfield = instr->x64->bitfield;
//       cb(instr, bitfield.source, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, bitfield.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_FCMP: {
//       struct x64_fcmp fcmp = instr->x64->fcmp;
//       cb(instr, fcmp.lhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, fcmp.rhs, X64_REG_USAGE_TY_READ, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_FCMP_ZERO: {
//       struct x64_fcmp_zero fcmp_zero = instr->x64->fcmp_zero;
//       cb(instr, fcmp_zero.lhs, X64_REG_USAGE_TY_READ, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_REG_1_SOURCE: {
//       struct x64_reg_1_source reg_1_source = instr->x64->reg_1_source;
//       cb(instr, reg_1_source.source, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, reg_1_source.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_REG_2_SOURCE: {
//       struct x64_reg_2_source reg_2_source = instr->x64->reg_2_source;
//       cb(instr, reg_2_source.lhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, reg_2_source.rhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, reg_2_source.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_MOV_IMM: {
//       struct x64_mov_imm mov_imm = instr->x64->mov_imm;
//       cb(instr, mov_imm.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_FMA: {
//       struct x64_fma fma = instr->x64->fma;
//       cb(instr, fma.lhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, fma.rhs, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, fma.addsub, X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, fma.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_CONDITIONAL_SELECT: {
//       struct x64_conditional_select conditional_select =
//           instr->x64->conditional_select;
//       cb(instr, conditional_select.false_source, X64_REG_USAGE_TY_READ,
//          metadata);
//       cb(instr, conditional_select.true_source, X64_REG_USAGE_TY_READ,
//          metadata);
//       cb(instr, conditional_select.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_CONDITIONAL_BRANCH: {
//       break;
//     }
//     case X64_INSTR_CLASS_BRANCH: {
//       break;
//     }
//     case X64_INSTR_CLASS_BRANCH_REG: {
//       struct x64_branch_reg branch_reg = instr->x64->branch_reg;
//       cb(instr, branch_reg.target, X64_REG_USAGE_TY_READ, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_COMPARE_AND_BRANCH: {
//       struct x64_compare_and_branch compare_and_branch =
//           instr->x64->compare_and_branch;
//       cb(instr, compare_and_branch.cmp, X64_REG_USAGE_TY_READ, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_LOAD_IMM: {
//       struct x64_load_imm load_imm = instr->x64->load_imm;
//       cb(instr, load_imm.addr, X64_REG_USAGE_TY_DEREF, metadata);
//       cb(instr, load_imm.dest, X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_STORE_IMM: {
//       struct x64_store_imm store_imm = instr->x64->store_imm;
//       cb(instr, store_imm.addr, X64_REG_USAGE_TY_DEREF, metadata);
//       cb(instr, store_imm.source, X64_REG_USAGE_TY_READ, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_LOAD_PAIR_IMM: {
//       struct x64_load_pair_imm load_pair_imm =
//           instr->x64->load_pair_imm;
//       cb(instr, load_pair_imm.addr, X64_REG_USAGE_TY_DEREF, metadata);
//       cb(instr, load_pair_imm.dest[0], X64_REG_USAGE_TY_WRITE, metadata);
//       cb(instr, load_pair_imm.dest[1], X64_REG_USAGE_TY_WRITE, metadata);
//       break;
//     }
//     case X64_INSTR_CLASS_STORE_PAIR_IMM: {
//       struct x64_store_pair_imm store_pair_imm =
//           instr->x64->store_pair_imm;
//       cb(instr, store_pair_imm.addr, X64_REG_USAGE_TY_DEREF, metadata);
//       cb(instr, store_pair_imm.source[0], X64_REG_USAGE_TY_READ, metadata);
//       cb(instr, store_pair_imm.source[1], X64_REG_USAGE_TY_READ, metadata);
//       break;
//     }
//     }

//     instr = instr->succ;
//   }
// }

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

    if (strncmp(format, "reg", 3) == 0) {
      struct x64_reg reg = va_arg(list, struct x64_reg);
      switch (reg.ty) {
      case X64_REG_TY_R:
        if (reg.idx > 7) {
          fprintf(file, "r%zud", reg.idx);
        } else {
          const char *name = reg_names[reg.idx];
          fprintf(file, "r%s", name);
        }
        break;
      case X64_REG_TY_RD:
        DEBUG_ASSERT(reg.idx > 7, "RD index should be > 7");
        fprintf(file, "r%zud", reg.idx);
        break;
      case X64_REG_TY_E:
        invariant_assert(reg.idx < ARR_LENGTH(reg_names), "reg idx too large");

        const char *name = reg_names[reg.idx];
        fprintf(file, "e%s", name);
      }

      format += 3;
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


static void debug_print_alu_unary(FILE *file, const struct x64_alu_unary *alu_unary) {
  codegen_fprintf(file, " %reg", alu_unary->dest);
}

static void debug_print_alu_reg(FILE *file, const struct x64_alu_reg *alu_reg) {
  codegen_fprintf(file, " %reg, %reg", alu_reg->dest, alu_reg->rhs);
}

static void debug_print_mov_imm(FILE *file, const struct x64_mov_imm *mov_imm) {
  codegen_fprintf(file, " %reg, %imm", mov_imm->dest, mov_imm->imm);
}

static void debug_print_mov_reg(FILE *file, const struct x64_mov_reg *mov_reg) {
  codegen_fprintf(file, " %reg, %reg", mov_reg->dest, mov_reg->source);
}

static void debug_print_instr(FILE *file,
                              UNUSED_ARG(const struct codegen_function *func),
                              const struct instr *instr) {

  switch (instr->x64->ty) {
  case X64_INSTR_TY_MOV_IMM:
    fprintf(file, "mov");
    debug_print_mov_imm(file, &instr->x64->mov_imm);
    break;
  case X64_INSTR_TY_MOV_REG:
    fprintf(file, "mov");
    debug_print_mov_reg(file, &instr->x64->mov_reg);
    break;
  case X64_INSTR_TY_ADD:
    fprintf(file, "add");
    debug_print_alu_reg(file, &instr->x64->add);
    break;
  case X64_INSTR_TY_SUB:
    fprintf(file, "sub");
    debug_print_alu_reg(file, &instr->x64->sub);
    break;
  case X64_INSTR_TY_OR:
    fprintf(file, "or");
    debug_print_alu_reg(file, &instr->x64->or);
    break;
  case X64_INSTR_TY_AND:
    fprintf(file, "and");
    debug_print_alu_reg(file, &instr->x64->and);
    break;
  case X64_INSTR_TY_EOR:
    fprintf(file, "eor");
    debug_print_alu_reg(file, &instr->x64->eor);
    break;
  case X64_INSTR_TY_NOT:
    fprintf(file, "not");
    debug_print_alu_unary(file, &instr->x64->not);
    break;
  case X64_INSTR_TY_NEG:
    fprintf(file, "neg");
    debug_print_alu_unary(file, &instr->x64->neg);
    break;
  case X64_INSTR_TY_RET:
    fprintf(file, "ret");
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
