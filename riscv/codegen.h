#ifndef RISCV_CODEGEN_H
#define RISCV_CODEGEN_H

#include "../codegen.h"

typedef unsigned long long imm_t;
typedef long long simm_t;

enum riscv_instr_ty {
  RISCV_INSTR_TY_MOV,
  RISCV_INSTR_TY_MOV_IMM,

  RISCV_INSTR_TY_ADD,
  RISCV_INSTR_TY_SUB,
  RISCV_INSTR_TY_ADC,
  RISCV_INSTR_TY_SBC,
  RISCV_INSTR_TY_AND,
  RISCV_INSTR_TY_CMP,

  RISCV_INSTR_TY_ADD_IMM,
  RISCV_INSTR_TY_SUB_IMM,
  RISCV_INSTR_TY_ADC_IMM,
  RISCV_INSTR_TY_SBC_IMM,
  RISCV_INSTR_TY_AND_IMM,
  RISCV_INSTR_TY_CMP_IMM,

  RISCV_INSTR_TY_LSL,
  RISCV_INSTR_TY_LSR,
  RISCV_INSTR_TY_ASR,
  RISCV_INSTR_TY_XSR,

  RISCV_INSTR_TY_LDR_DIRECT,
  RISCV_INSTR_TY_STR_DIRECT,

  RISCV_INSTR_TY_LDR_OFFSET,
  RISCV_INSTR_TY_STR_OFFSET,

  RISCV_INSTR_TY_JMP,
  RISCV_INSTR_TY_EXT
};

enum riscv_instr_class {
  RISCV_INSTR_CLASS_MOV,
  RISCV_INSTR_CLASS_MOV_IMM,

  RISCV_INSTR_CLASS_ALU,
  RISCV_INSTR_CLASS_ALU_IMM,
  RISCV_INSTR_CLASS_ALU_SHIFT,

  RISCV_INSTR_CLASS_LDR_DIRECT,
  RISCV_INSTR_CLASS_STR_DIRECT,

  RISCV_INSTR_CLASS_LDR_OFFSET,
  RISCV_INSTR_CLASS_STR_OFFSET,

  RISCV_INSTR_CLASS_JMP,
  RISCV_INSTR_CLASS_EXT
};

struct riscv_reg {
  unsigned long idx;
};

struct riscv_mov {
  struct riscv_reg dest;
  struct riscv_reg source;
};

struct riscv_mov_imm {
  struct riscv_reg dest;
  simm_t imm;
};

struct riscv_alu {
  struct riscv_reg dest;
  struct riscv_reg lhs;
  struct riscv_reg rhs;
};

struct riscv_alu_imm {
  struct riscv_reg dest;
  simm_t imm;
};

struct riscv_alu_shift {
  struct riscv_reg dest;
  struct riscv_reg source;
  imm_t imm;
};

struct riscv_ldr_direct {
  struct riscv_reg dest;
  imm_t imm;
};

struct riscv_str_direct {
  struct riscv_reg source;
  imm_t imm;
};

struct riscv_ldr_offset {
  struct riscv_reg dest;
  struct riscv_reg addr;
  imm_t imm;
};

struct riscv_str_offset {
  struct riscv_reg source;
  struct riscv_reg addr;
  imm_t imm;
};

enum riscv_cond {
  RISCV_COND_JMP = 0b0000,
  RISCV_COND_NOOP = 0b0001,

  RISCV_COND_JEQ = 0b0010,
  RISCV_COND_JNE = 0b0011,

  RISCV_COND_JCS = 0b0100,
  RISCV_COND_JCC = 0b0101,

  RISCV_COND_JMI = 0b0110,
  RISCV_COND_JPL = 0b0111,

  RISCV_COND_JGE = 0b1000,
  RISCV_COND_JLT = 0b1001,

  RISCV_COND_JGT = 0b1010,
  RISCV_COND_JLE = 0b1011,

  RISCV_COND_JHI = 0b1100,
  RISCV_COND_JLS = 0b1101,

  RISCV_COND_JSR = 0b1110,
  RISCV_COND_RET = 0b1111,
};

struct riscv_jmp {
  enum riscv_cond cond;
  imm_t imm;
};

struct riscv_ext {
  imm_t imm;
};

struct riscv_instr {
  enum riscv_instr_ty ty;
  
  union {
    union {
      struct riscv_mov mov;
    };

    union {
      struct riscv_mov_imm mov_imm;
    };

    union {
      struct riscv_alu alu, add, sub, adc, sbc, and, cmp;
    };

    union {
      struct riscv_alu_imm alu_imm, add_imm, sub_imm, adc_imm, sbc_imm, and_imm, cmp_imm;
    };

    union {
      struct riscv_alu_shift alu_shift, lsl, lsr, asr, xsr;
    };

    union {
      struct riscv_str_direct str_direct;
    };

    union {
      struct riscv_ldr_direct ldr_direct;
    };

    union {
      struct riscv_str_offset str_offset;
    };

    union {
      struct riscv_ldr_offset ldr_offset;
    };

    union {
      struct riscv_jmp jmp;
    };

    union {
      struct riscv_ext ext;
    };
  };
};

struct codegen_unit *riscv_codegen(struct ir_unit *ir);
#endif
