#ifndef RV32I_CODEGEN_H
#define RV32I_CODEGEN_H

#include "../codegen.h"

#include <stdio.h>

#if defined(STACK_PTR_REG) || defined(FRAME_PTR_REG) || defined(RET_PTR_REG)
#error                                                                         \
    "STACK_PTR_REG/FRAME_PTR_REG/RET_PTR_REG already defined. Check your includes"
#endif

// `[w|x]zr` and `sp` are encoded as the same thing and the instruction decides
// which is relevant
#define STACK_PTR_REG ((struct rv32i_reg){RV32I_REG_TY_GP, 2})
#define FRAME_PTR_REG ((struct rv32i_reg){RV32I_REG_TY_GP, 8})
#define RET_PTR_REG ((struct rv32i_reg){RV32I_REG_TY_GP, 1})

typedef unsigned long long imm_t;
typedef long long simm_t;

enum rv32i_instr_ty {
  RV32I_INSTR_TY_ADDI,
  RV32I_INSTR_TY_ADD,
  RV32I_INSTR_TY_LUI,
  RV32I_INSTR_TY_JALR,

  RV32I_INSTR_TY_SB,
  RV32I_INSTR_TY_SH,
  RV32I_INSTR_TY_SW,

  RV32I_INSTR_TY_LB,
  RV32I_INSTR_TY_LBU,
  RV32I_INSTR_TY_LH,
  RV32I_INSTR_TY_LHU,
  RV32I_INSTR_TY_LW,
};

enum rv32i_reg_ty {
  RV32I_REG_TY_NONE,

  RV32I_REG_TY_GP,
  RV32I_REG_TY_FP,
};

enum rv32i_reg_class {
  RV32I_REG_CLASS_GP,
  RV32I_REG_CLASS_FP,
};

enum rv32i_reg_attr_flags {
  RV32I_REG_ATTR_FLAG_NONE = 0,
  RV32I_REG_ATTR_FLAG_VOLATILE = 1, // v8-15 are upper half only volatile, we don't support this yet
  RV32I_REG_ATTR_FLAG_ARG_REG = 2,
  RV32I_REG_ATTR_FLAG_RET_REG = 4,
  RV32I_REG_ATTR_FLAG_RESERVED = 8,
};

bool rv32i_reg_ty_is_gp(enum rv32i_reg_ty ty);
bool rv32i_reg_ty_is_fp(enum rv32i_reg_ty ty);

struct rv32i_reg {
  enum rv32i_reg_ty ty;

  size_t idx;
};

enum rv32i_reg_attr_flags reg_attr_flags(struct rv32i_reg reg);

enum rv32i_instr_class {
  RV32I_INSTR_CLASS_NOP,
  RV32I_INSTR_CLASS_OP_IMM,
  RV32I_INSTR_CLASS_OP,
  RV32I_INSTR_CLASS_LUI,
  RV32I_INSTR_CLASS_JALR,
  RV32I_INSTR_CLASS_LOAD,
  RV32I_INSTR_CLASS_STORE,
};

struct rv32i_op_imm {
  struct rv32i_reg dest;
  struct rv32i_reg source;

  imm_t imm;
};

struct rv32i_op {
  struct rv32i_reg dest;
  struct rv32i_reg lhs;
  struct rv32i_reg rhs;
};

struct rv32i_lui {
  struct rv32i_reg dest;

  imm_t imm;
};

struct rv32i_jalr {
  struct rv32i_reg ret_addr;
  struct rv32i_reg target;

  imm_t imm;
};

struct rv32i_load {
  struct rv32i_reg dest;
  struct rv32i_reg addr;

  imm_t imm;
};

struct rv32i_store {
  struct rv32i_reg source;
  struct rv32i_reg addr;

  imm_t imm;
};

struct rv32i_instr {
  enum rv32i_instr_ty ty;

  union {
    union {
      struct rv32i_op_imm op_imm, addi;
    };

    union {
      struct rv32i_op op, add;
    };

    union {
      struct rv32i_load load, lb, lbu, lh, lhu, lw;
    };

    union {
      struct rv32i_store store, sb, sh, sw;
    };

    union {
      struct rv32i_lui lui;
    };

    union {
      struct rv32i_jalr jalr;
    };
  };
};

enum rv32i_reg_usage_ty {
  RV32I_REG_USAGE_TY_WRITE, // mov x9, ...
  RV32I_REG_USAGE_TY_READ,  // mov ..., x9
  RV32I_REG_USAGE_TY_DEREF, // ldr ..., [x9]
};

size_t reg_size(enum rv32i_reg_ty reg_ty);

struct rv32i_reg get_full_reg_for_ir_reg(struct ir_reg reg);

bool is_return_reg(struct rv32i_reg reg);
bool is_zero_reg(struct rv32i_reg reg);
bool reg_eq(struct rv32i_reg l, struct rv32i_reg r);

enum rv32i_instr_class instr_class(enum rv32i_instr_ty ty);

typedef void(walk_regs_callback)(struct instr *instr, struct rv32i_reg reg,
                                 enum rv32i_reg_usage_ty usage_ty,
                                 void *metadata);
void walk_regs(const struct codegen_function *func, walk_regs_callback *cb,
               void *metadata);

struct codegen_unit *rv32i_codegen(struct ir_unit *ir);
void rv32i_debug_print_codegen(FILE *file, struct codegen_unit *unit);

#endif
