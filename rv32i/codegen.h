#ifndef RV32I_CODEGEN_H
#define RV32I_CODEGEN_H

#include "../codegen.h"

typedef unsigned long long imm_t;
typedef long long simm_t;

enum rv32i_instr_ty {
  RV32I_INSTR_TY_MOV,
  RV32I_INSTR_TY_MOV_IMM,

  RV32I_INSTR_TY_ADD,
  RV32I_INSTR_TY_SUB,
  RV32I_INSTR_TY_ADC,
  RV32I_INSTR_TY_SBC,
  RV32I_INSTR_TY_AND,
  RV32I_INSTR_TY_CMP,

  RV32I_INSTR_TY_ADD_IMM,
  RV32I_INSTR_TY_SUB_IMM,
  RV32I_INSTR_TY_ADC_IMM,
  RV32I_INSTR_TY_SBC_IMM,
  RV32I_INSTR_TY_AND_IMM,
  RV32I_INSTR_TY_CMP_IMM,

  RV32I_INSTR_TY_LSL,
  RV32I_INSTR_TY_LSR,
  RV32I_INSTR_TY_ASR,
  RV32I_INSTR_TY_XSR,

  RV32I_INSTR_TY_LDR_DIRECT,
  RV32I_INSTR_TY_STR_DIRECT,

  RV32I_INSTR_TY_LDR_OFFSET,
  RV32I_INSTR_TY_STR_OFFSET,

  RV32I_INSTR_TY_JMP,
  RV32I_INSTR_TY_EXT
};

enum rv32i_instr_class {
  RV32I_INSTR_CLASS_MOV,
  RV32I_INSTR_CLASS_MOV_IMM,

  RV32I_INSTR_CLASS_ALU,
  RV32I_INSTR_CLASS_ALU_IMM,
  RV32I_INSTR_CLASS_ALU_SHIFT,

  RV32I_INSTR_CLASS_LDR_DIRECT,
  RV32I_INSTR_CLASS_STR_DIRECT,

  RV32I_INSTR_CLASS_LDR_OFFSET,
  RV32I_INSTR_CLASS_STR_OFFSET,

  RV32I_INSTR_CLASS_JMP,
  RV32I_INSTR_CLASS_EXT
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
  RV32I_COND_JMP = 0b0000,
  RV32I_COND_NOOP = 0b0001,

  RV32I_COND_JEQ = 0b0010,
  RV32I_COND_JNE = 0b0011,

  RV32I_COND_JCS = 0b0100,
  RV32I_COND_JCC = 0b0101,

  RV32I_COND_JMI = 0b0110,
  RV32I_COND_JPL = 0b0111,

  RV32I_COND_JGE = 0b1000,
  RV32I_COND_JLT = 0b1001,

  RV32I_COND_JGT = 0b1010,
  RV32I_COND_JLE = 0b1011,

  RV32I_COND_JHI = 0b1100,
  RV32I_COND_JLS = 0b1101,

  RV32I_COND_JSR = 0b1110,
  RV32I_COND_RET = 0b1111,
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
