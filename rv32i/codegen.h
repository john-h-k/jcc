#ifndef RV32I_CODEGEN_H
#define RV32I_CODEGEN_H

#include "../codegen.h"

#include <stdio.h>

#if defined(STACK_PTR_REG) || defined(FRAME_PTR_REG) || defined(RET_PTR_REG)
#error                                                                         \
    "STACK_PTR_REG/FRAME_PTR_REG/RET_PTR_REG already defined. Check your includes"
#endif

#define STACK_PTR_REG ((struct rv32i_reg){RV32I_REG_TY_W, 2})
#define FRAME_PTR_REG ((struct rv32i_reg){RV32I_REG_TY_W, 8})
#define RET_PTR_REG ((struct rv32i_reg){RV32I_REG_TY_W, 1})
#define GP_ZERO_REG ((struct rv32i_reg){RV32I_REG_TY_W, 0})

typedef long long simm_t;

enum rv32i_instr_ty {
  /* -------------------------- */
  /* rv32i base instruction set */
  /* -------------------------- */

  // U-type
  RV32I_INSTR_TY_LUI,
  RV32I_INSTR_TY_AUIPC,

  // jumps
  RV32I_INSTR_TY_JAL,
  RV32I_INSTR_TY_JALR,

  // conditional branches
  RV32I_INSTR_TY_BEQ,
  RV32I_INSTR_TY_BNE,
  RV32I_INSTR_TY_BLT,
  RV32I_INSTR_TY_BGE,
  RV32I_INSTR_TY_BLTU,
  RV32I_INSTR_TY_BGEU,

  // loads
  RV32I_INSTR_TY_LB,
  RV32I_INSTR_TY_LH,
  RV32I_INSTR_TY_LW,
  RV32I_INSTR_TY_LBU,
  RV32I_INSTR_TY_LHU,

  // stores
  RV32I_INSTR_TY_SB,
  RV32I_INSTR_TY_SH,
  RV32I_INSTR_TY_SW,

  // I-type
  RV32I_INSTR_TY_ADDI,
  RV32I_INSTR_TY_SLTI,
  RV32I_INSTR_TY_SLTIU,
  RV32I_INSTR_TY_XORI,
  RV32I_INSTR_TY_ORI,
  RV32I_INSTR_TY_ANDI,
  RV32I_INSTR_TY_SLLI,
  RV32I_INSTR_TY_SRLI,
  RV32I_INSTR_TY_SRAI,

  // R-type
  RV32I_INSTR_TY_ADD,
  RV32I_INSTR_TY_SUB,
  RV32I_INSTR_TY_SLL,
  RV32I_INSTR_TY_SLT,
  RV32I_INSTR_TY_SLTU,
  RV32I_INSTR_TY_XOR,
  RV32I_INSTR_TY_SRL,
  RV32I_INSTR_TY_SRA,
  RV32I_INSTR_TY_OR,
  RV32I_INSTR_TY_AND,

  // other
  RV32I_INSTR_TY_FENCE,
  RV32I_INSTR_TY_FENCE_TSO,
  RV32I_INSTR_TY_PAUSE,
  RV32I_INSTR_TY_ECALL,
  RV32I_INSTR_TY_EBREAK,

  /* --------------------- */
  /* rv32i M-extension set */
  /* --------------------- */

  // multiplication
  RV32I_INSTR_TY_MUL,
  RV32I_INSTR_TY_MULH,
  RV32I_INSTR_TY_MULHU,
  RV32I_INSTR_TY_MULHSU,

  // division/remainder
  RV32I_INSTR_TY_DIV,
  RV32I_INSTR_TY_DIVU,
  RV32I_INSTR_TY_REM,
  RV32I_INSTR_TY_REMU,

  /* ------------------------ */
  /* rv32i F/D-extension sets */
  /* ------------------------ */

  // load/store
  RV32I_INSTR_TY_FLW,
  RV32I_INSTR_TY_FSW,

  // computational
  RV32I_INSTR_TY_FADD,
  RV32I_INSTR_TY_FSUB,
  RV32I_INSTR_TY_FMUL,
  RV32I_INSTR_TY_FDIV,
  RV32I_INSTR_TY_FSQRT,
  RV32I_INSTR_TY_FMIN,
  RV32I_INSTR_TY_FMAX,

  // conversion and move
  RV32I_INSTR_TY_FCVT,
  RV32I_INSTR_TY_FCVTU,
  RV32I_INSTR_TY_FMV,

  // sign injection
  RV32I_INSTR_TY_FSGNJ,
  RV32I_INSTR_TY_FSGNJN,
  RV32I_INSTR_TY_FSGNJX,

  // compare
  RV32I_INSTR_TY_FEQ,
  RV32I_INSTR_TY_FLT,
  RV32I_INSTR_TY_FLE,

  // classify
  RV32I_INSTR_TY_FCLASS,
};

enum rv32i_reg_ty {
  RV32I_REG_TY_NONE,

  RV32I_REG_TY_W,
  RV32I_REG_TY_F,
};

enum rv32i_reg_class {
  RV32I_REG_CLASS_GP,
  RV32I_REG_CLASS_FP,
};

enum rv32i_reg_attr_flags {
  RV32I_REG_ATTR_FLAG_NONE = 0,
  RV32I_REG_ATTR_FLAG_VOLATILE =
      1, // v8-15 are upper half only volatile, we don't support this yet
  RV32I_REG_ATTR_FLAG_ARG_REG = 2,
  RV32I_REG_ATTR_FLAG_RET_REG = 4,
  RV32I_REG_ATTR_FLAG_RESERVED = 8,
};

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

  simm_t imm;
};

struct rv32i_op {
  struct rv32i_reg dest;
  struct rv32i_reg lhs;
  struct rv32i_reg rhs;
};

struct rv32i_op_fp {
  struct rv32i_reg dest;
  struct rv32i_reg lhs;
  struct rv32i_reg rhs;
};

struct rv32i_op_unary_fp {
  struct rv32i_reg dest;
  struct rv32i_reg source;
};

struct rv32i_op_mov {
  struct rv32i_reg dest;
  struct rv32i_reg source;
};

struct rv32i_u {
  struct rv32i_reg dest;

  simm_t imm;
};

struct rv32i_jalr {
  struct rv32i_reg ret_addr;
  struct rv32i_reg target;

  simm_t imm;
};

struct rv32i_load {
  struct rv32i_reg dest;
  struct rv32i_reg addr;

  simm_t imm;
};

struct rv32i_store {
  struct rv32i_reg source;
  struct rv32i_reg addr;

  simm_t imm;
};

struct rv32i_jal {
  struct rv32i_reg ret_addr;
  struct ir_basicblock *target;
};

struct rv32i_conditional_branch {
  struct rv32i_reg lhs;
  struct rv32i_reg rhs;
  struct ir_basicblock *target;
};

struct rv32i_conditional_set {
  struct rv32i_reg lhs;
  struct rv32i_reg rhs;
  struct rv32i_reg dest;
};

struct rv32i_instr {
  enum rv32i_instr_ty ty;

  union {
    union {
      struct rv32i_op_imm op_imm, addi, xori, andi, ori, slli, srli, srai, slti,
          sltiu;
    };

    union {
      struct rv32i_op op;

      /* base */
      struct rv32i_op add, sub, and, or, xor, sll, srl, sra, slt, sltu;

      /* M-extension */
      struct rv32i_op mul, mulh, mulhsu, mulhu, div, rem, divu, remu;
    };

    union {
      struct rv32i_op_fp op_fp, fadd, fsub, fmul, fdiv, fsgnj, fsgnjn, fsgnjx,
          fmin, fmax, feq, flt, fle;
    };

    union {
      struct rv32i_op_unary_fp op_unary_fp, fsqrt, fclass;
    };

    union {
      // NOTE: `fmv` is only for f<->w/x registers, not between f registers
      struct rv32i_op_mov op_mov, fmv, fcvt, fcvtu;
    };

    union {
      struct rv32i_load load;

      /* base */
      struct rv32i_load lb, lbu, lh, lhu, lw;

      /* F/D-extension */
      struct rv32i_load flw;
    };

    union {
      struct rv32i_store store;

      /* base */
      struct rv32i_store sb, sh, sw;

      /* F/D-extension */
      struct rv32i_store fsw;
    };

    union {
      struct rv32i_u u, lui, auipc;
    };

    union {
      struct rv32i_jalr jalr;
    };

    union {
      struct rv32i_jal jal;
    };

    union {
      struct rv32i_conditional_branch conditional_branch, beq, bne, blt, bge,
          bltu, bgeu;
    };
  };
};

enum rv32i_reg_usage_ty {
  RV32I_REG_USAGE_TY_WRITE, // add a0, ...
  RV32I_REG_USAGE_TY_READ,  // add ..., x0
  RV32I_REG_USAGE_TY_DEREF, // lw ..., 0(x9)
};

const char *rv32i_mangle(struct arena_allocator *arena, const char *name);

size_t reg_size(enum rv32i_reg_ty reg_ty);

struct rv32i_reg get_full_reg_for_ir_reg(struct ir_reg reg);

bool is_return_reg(struct rv32i_reg reg);
bool is_zero_reg(struct rv32i_reg reg);

enum rv32i_instr_class instr_class(enum rv32i_instr_ty ty);

typedef void(walk_regs_callback)(struct instr *instr, struct rv32i_reg reg,
                                 enum rv32i_reg_usage_ty usage_ty,
                                 void *metadata);
void walk_regs(const struct codegen_function *func, walk_regs_callback *cb,
               void *metadata);

struct codegen_unit *rv32i_codegen(struct ir_unit *ir);
void rv32i_debug_print_codegen(FILE *file, struct codegen_unit *unit);

#endif
