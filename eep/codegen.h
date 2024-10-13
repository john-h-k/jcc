#ifndef EEP_CODEGEN_H
#define EEP_CODEGEN_H

#include "../codegen.h"

typedef unsigned long long imm_t;
typedef long long simm_t;

enum eep_instr_ty {
  EEP_INSTR_TY_MOV,
  EEP_INSTR_TY_MOV_IMM,

  EEP_INSTR_TY_ADD,
  EEP_INSTR_TY_SUB,
  EEP_INSTR_TY_ADC,
  EEP_INSTR_TY_SBC,
  EEP_INSTR_TY_AND,
  EEP_INSTR_TY_CMP,

  EEP_INSTR_TY_ADD_IMM,
  EEP_INSTR_TY_SUB_IMM,
  EEP_INSTR_TY_ADC_IMM,
  EEP_INSTR_TY_SBC_IMM,
  EEP_INSTR_TY_AND_IMM,
  EEP_INSTR_TY_CMP_IMM,

  EEP_INSTR_TY_LSL,
  EEP_INSTR_TY_LSR,
  EEP_INSTR_TY_ASR,
  EEP_INSTR_TY_XSR,

  EEP_INSTR_TY_LDR_DIRECT,
  EEP_INSTR_TY_STR_DIRECT,

  EEP_INSTR_TY_LDR_OFFSET,
  EEP_INSTR_TY_STR_OFFSET,

  EEP_INSTR_TY_JMP,
  EEP_INSTR_TY_EXT
};

enum eep_instr_class {
  EEP_INSTR_CLASS_MOV,
  EEP_INSTR_CLASS_MOV_IMM,

  EEP_INSTR_CLASS_ALU,
  EEP_INSTR_CLASS_ALU_IMM,
  EEP_INSTR_CLASS_ALU_SHIFT,

  EEP_INSTR_CLASS_LDR_DIRECT,
  EEP_INSTR_CLASS_STR_DIRECT,

  EEP_INSTR_CLASS_LDR_OFFSET,
  EEP_INSTR_CLASS_STR_OFFSET,

  EEP_INSTR_CLASS_JMP,
  EEP_INSTR_CLASS_EXT
};

struct eep_reg {
  unsigned long idx;
};

struct eep_mov {
  struct eep_reg dest;
  struct eep_reg source;
};

struct eep_mov_imm {
  struct eep_reg dest;
  simm_t imm;
};

struct eep_alu {
  struct eep_reg dest;
  struct eep_reg lhs;
  struct eep_reg rhs;
};

struct eep_alu_imm {
  struct eep_reg dest;
  simm_t imm;
};

struct eep_alu_shift {
  struct eep_reg dest;
  struct eep_reg source;
  imm_t imm;
};

struct eep_ldr_direct {
  struct eep_reg dest;
  imm_t imm;
};

struct eep_str_direct {
  struct eep_reg source;
  imm_t imm;
};

struct eep_ldr_offset {
  struct eep_reg dest;
  struct eep_reg addr;
  imm_t imm;
};

struct eep_str_offset {
  struct eep_reg source;
  struct eep_reg addr;
  imm_t imm;
};

enum eep_cond {
  EEP_COND_JMP = 0b0000,
  EEP_COND_NOOP = 0b0001,

  EEP_COND_JEQ = 0b0010,
  EEP_COND_JNE = 0b0011,

  EEP_COND_JCS = 0b0100,
  EEP_COND_JCC = 0b0101,

  EEP_COND_JMI = 0b0110,
  EEP_COND_JPL = 0b0111,

  EEP_COND_JGE = 0b1000,
  EEP_COND_JLT = 0b1001,

  EEP_COND_JGT = 0b1010,
  EEP_COND_JLE = 0b1011,

  EEP_COND_JHI = 0b1100,
  EEP_COND_JLS = 0b1101,

  EEP_COND_JSR = 0b1110,
  EEP_COND_RET = 0b1111,
};

struct eep_jmp {
  enum eep_cond cond;
  imm_t imm;
};

struct eep_ext {
  imm_t imm;
};

struct eep_instr {
  enum eep_instr_ty ty;
  
  union {
    union {
      struct eep_mov mov;
    };

    union {
      struct eep_mov_imm mov_imm;
    };

    union {
      struct eep_alu alu, add, sub, adc, sbc, and, cmp;
    };

    union {
      struct eep_alu_imm alu_imm, add_imm, sub_imm, adc_imm, sbc_imm, and_imm, cmp_imm;
    };

    union {
      struct eep_alu_shift alu_shift, lsl, lsr, asr, xsr;
    };

    union {
      struct eep_str_direct str_direct;
    };

    union {
      struct eep_ldr_direct ldr_direct;
    };

    union {
      struct eep_str_offset str_offset;
    };

    union {
      struct eep_ldr_offset ldr_offset;
    };

    union {
      struct eep_jmp jmp;
    };

    union {
      struct eep_ext ext;
    };
  };
};

struct codegen_unit *eep_codegen(struct ir_unit *ir);
#endif
