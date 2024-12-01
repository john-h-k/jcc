#ifndef RISCV_CODEGEN_H
#define RISCV_CODEGEN_H

#include "../codegen.h"

typedef unsigned long long imm_t;
typedef long long simm_t;

enum rv32i_instr_ty {
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

enum rv32i_instr_class {
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

struct rv32i_reg {
  unsigned long idx;
};

struct rv32i_mov {
  struct rv32i_reg dest;
  struct rv32i_reg source;
};

struct rv32i_mov_imm {
  struct rv32i_reg dest;
  simm_t imm;
};

struct rv32i_alu {
  struct rv32i_reg dest;
  struct rv32i_reg lhs;
  struct rv32i_reg rhs;
};

struct rv32i_alu_imm {
  struct rv32i_reg dest;
  simm_t imm;
};

struct rv32i_alu_shift {
  struct rv32i_reg dest;
  struct rv32i_reg source;
  imm_t imm;
};

struct rv32i_ldr_direct {
  struct rv32i_reg dest;
  imm_t imm;
};

struct rv32i_str_direct {
  struct rv32i_reg source;
  imm_t imm;
};

struct rv32i_ldr_offset {
  struct rv32i_reg dest;
  struct rv32i_reg addr;
  imm_t imm;
};

struct rv32i_str_offset {
  struct rv32i_reg source;
  struct rv32i_reg addr;
  imm_t imm;
};

enum rv32i_cond {
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

struct rv32i_jmp {
  enum rv32i_cond cond;
  imm_t imm;
};

struct rv32i_ext {
  imm_t imm;
};

struct rv32i_instr {
  enum rv32i_instr_ty ty;

  union {
    union {
      struct rv32i_mov mov;
    };

    union {
      struct rv32i_mov_imm mov_imm;
    };

    union {
      struct rv32i_alu alu, add, sub, adc, sbc, and, cmp;
    };

    union {
      struct rv32i_alu_imm alu_imm, add_imm, sub_imm, adc_imm, sbc_imm, and_imm,
          cmp_imm;
    };

    union {
      struct rv32i_alu_shift alu_shift, lsl, lsr, asr, xsr;
    };

    union {
      struct rv32i_str_direct str_direct;
    };

    union {
      struct rv32i_ldr_direct ldr_direct;
    };

    union {
      struct rv32i_str_offset str_offset;
    };

    union {
      struct rv32i_ldr_offset ldr_offset;
    };

    union {
      struct rv32i_jmp jmp;
    };

    union {
      struct rv32i_ext ext;
    };
  };
};

struct codegen_unit *rv32i_codegen(struct ir_unit *ir);
#endif
