#ifndef AARCH64_CODEGEN_H
#define AARCH64_CODEGEN_H

#include "../codegen.h"

enum aarch64_instr_ty {AARCH64_INSTR_TY_ADDS_32,
  AARCH64_INSTR_TY_ADDS_64,
  AARCH64_INSTR_TY_ADD_32,
  AARCH64_INSTR_TY_ADD_32_IMM,
  AARCH64_INSTR_TY_ADD_64,
  AARCH64_INSTR_TY_ADD_64_IMM,
  AARCH64_INSTR_TY_ADR,
  AARCH64_INSTR_TY_ADRP,
  AARCH64_INSTR_TY_ANDS_32,
  AARCH64_INSTR_TY_ANDS_32_IMM,
  AARCH64_INSTR_TY_ANDS_64,
  AARCH64_INSTR_TY_ANDS_64_IMM,
  AARCH64_INSTR_TY_AND_32,
  AARCH64_INSTR_TY_AND_32_IMM,
  AARCH64_INSTR_TY_AND_64,
  AARCH64_INSTR_TY_AND_64_IMM,
  AARCH64_INSTR_TY_ASRV_32,
  AARCH64_INSTR_TY_ASRV_64,
  AARCH64_INSTR_TY_B,
  AARCH64_INSTR_TY_BC_COND,
  AARCH64_INSTR_TY_BFM_32_IMM,
  AARCH64_INSTR_TY_BFM_64_IMM,
  AARCH64_INSTR_TY_BL,
  AARCH64_INSTR_TY_B_COND,
  AARCH64_INSTR_TY_CBNZ_32_IMM,
  AARCH64_INSTR_TY_CBZ_32_IMM,
  AARCH64_INSTR_TY_CBZ_64_IMM,
  AARCH64_INSTR_TY_CNBZ_64_IMM,
  AARCH64_INSTR_TY_CSEL_32,
  AARCH64_INSTR_TY_CSEL_64,
  AARCH64_INSTR_TY_CSINC_32,
  AARCH64_INSTR_TY_CSINC_64,
  AARCH64_INSTR_TY_CSINV_32,
  AARCH64_INSTR_TY_CSINV_64,
  AARCH64_INSTR_TY_CSNEG_32,
  AARCH64_INSTR_TY_CSNEG_64,
  AARCH64_INSTR_TY_EON_32,
  AARCH64_INSTR_TY_EON_64,
  AARCH64_INSTR_TY_EOR_32,
  AARCH64_INSTR_TY_EOR_32_IMM,
  AARCH64_INSTR_TY_EOR_64,
  AARCH64_INSTR_TY_EOR_64_IMM,
  AARCH64_INSTR_TY_LOAD_32_IMM,
  AARCH64_INSTR_TY_LOAD_64_IMM,
  AARCH64_INSTR_TY_LOAD_PAIR_32_IMM,
  AARCH64_INSTR_TY_LOAD_PAIR_64_IMM,
  AARCH64_INSTR_TY_LSLV_32,
  AARCH64_INSTR_TY_LSLV_64,
  AARCH64_INSTR_TY_LSRV_32,
  AARCH64_INSTR_TY_LSRV_64,
  AARCH64_INSTR_TY_MADD_32,
  AARCH64_INSTR_TY_MADD_64,
  AARCH64_INSTR_TY_MOVN_32_IMM,
  AARCH64_INSTR_TY_MOVN_64_IMM,
  AARCH64_INSTR_TY_MOV_32_IMM,
  AARCH64_INSTR_TY_MOV_64_IMM,
  AARCH64_INSTR_TY_MOV_32,
  AARCH64_INSTR_TY_MOV_64,
  AARCH64_INSTR_TY_MSUB_32,
  AARCH64_INSTR_TY_MSUB_64,
  AARCH64_INSTR_TY_NOP,
  AARCH64_INSTR_TY_ORN_32,
  AARCH64_INSTR_TY_ORN_64,
  AARCH64_INSTR_TY_ORR_32,
  AARCH64_INSTR_TY_ORR_32_IMM,
  AARCH64_INSTR_TY_ORR_64,
  AARCH64_INSTR_TY_ORR_64_IMM,
  AARCH64_INSTR_TY_RET,
  AARCH64_INSTR_TY_RORV_32,
  AARCH64_INSTR_TY_RORV_64,
  AARCH64_INSTR_TY_SBFM_32_IMM,
  AARCH64_INSTR_TY_SBFM_64_IMM,
  AARCH64_INSTR_TY_SDIV_32,
  AARCH64_INSTR_TY_SDIV_64,
  AARCH64_INSTR_TY_STORE_32_IMM,
  AARCH64_INSTR_TY_STORE_64_IMM,
  AARCH64_INSTR_TY_STORE_PAIR_32_IMM,
  AARCH64_INSTR_TY_STORE_PAIR_64_IMM,
  AARCH64_INSTR_TY_SUBS_32,
  AARCH64_INSTR_TY_SUBS_64,
  AARCH64_INSTR_TY_SUB_32,
  AARCH64_INSTR_TY_SUB_32_IMM,
  AARCH64_INSTR_TY_SUB_64,
  AARCH64_INSTR_TY_SUB_64_IMM,
  AARCH64_INSTR_TY_UBFM_32_IMM,
  AARCH64_INSTR_TY_UBFM_64_IMM,
  AARCH64_INSTR_TY_UDIV_32,
  AARCH64_INSTR_TY_UDIV_64};

enum aarch64_cond {
  // always true
  AARCH64_COND_AL = 0b1110,
  AARCH64_COND_AL_ALT = 0b1111,

  AARCH64_COND_EQ = 0b0000,
  AARCH64_COND_NE = 0b0001,

  /* signed conditions */
  AARCH64_COND_GE = 0b1010,
  AARCH64_COND_LT = 0b1011,
  AARCH64_COND_GT = 0b1100,
  AARCH64_COND_LE = 0b1101,

  // signed overflow & no overflow
  AARCH64_COND_VS = 0b0110,
  AARCH64_COND_VC = 0b0111,

  // carry set & clear
  AARCH64_COND_CS = 0b0010,
  AARCH64_COND_CC = 0b0011,

  /* unsigned conditions */
  AARCH64_COND_HI = 0b1000,
  AARCH64_COND_LS = 0b1001,
  AARCH64_COND_HS = AARCH64_COND_CS,
  AARCH64_COND_LO = AARCH64_COND_CC,

  // minus & positive or zero
  AARCH64_COND_MI = 0b0100,
  AARCH64_COND_PL = 0b0101,
};

enum aarch64_shift {
  AARCH64_SHIFT_LSL = 0b00,
  AARCH64_SHIFT_LSR = 0b01,
  AARCH64_SHIFT_ASR = 0b10,
  AARCH64_SHIFT_RESERVED = 0b11,
};

enum aarch64_addressing_mode {
  AARCH64_ADDRESSING_MODE_OFFSET,
  AARCH64_ADDRESSING_MODE_PREINDEX,
  AARCH64_ADDRESSING_MODE_POSTINDEX,
};

enum aarch64_reg_ty {
  AARCH64_REG_TY_W, // 32-bit int
  AARCH64_REG_TY_X, // 64-bit int

  AARCH64_REG_TY_V, // 128-bit vector

  AARCH64_REG_TY_Q, // 128-bit float
  AARCH64_REG_TY_D, // 64-bit float
  AARCH64_REG_TY_S, // 32-bit float
  AARCH64_REG_TY_H, // 16-bit float
  AARCH64_REG_TY_B, // 8-bit float
};

struct aarch64_reg {
  enum aarch64_reg_ty ty;

  size_t idx;
};

struct aarch64_logical_reg {
  struct aarch64_reg dest;
  struct aarch64_reg lhs;
  struct aarch64_reg rhs;
  enum aarch64_shift shift;
  size_t imm6;
};

struct aarch64_logical_imm {
  struct aarch64_reg dest;
  struct aarch64_reg source;

  size_t n;
  size_t immr;
  size_t imms;
};

struct aarch64_addr_imm {
  struct aarch64_reg dest;
  size_t imm;
};

struct aarch64_addsub_imm {
  struct aarch64_reg dest;
  struct aarch64_reg source;

  size_t imm;
  size_t shift;
};

struct aarch64_addsub_reg {
  struct aarch64_reg dest;
  struct aarch64_reg lhs;
  struct aarch64_reg rhs;

  size_t imm6;
  enum aarch64_shift shift;
};

struct aarch64_bitfield_imm {
  struct aarch64_reg dest;
  struct aarch64_reg source;

  size_t immr;
  size_t imms;
};

struct aarch64_reg_1_source {
  struct aarch64_reg dest;
  struct aarch64_reg source;
};

struct aarch64_mov_imm {
  struct aarch64_reg dest;
  size_t imm;
  size_t shift;
};

struct aarch64_reg_2_source {
  struct aarch64_reg dest;
  struct aarch64_reg lhs;
  struct aarch64_reg rhs;
};

struct aarch64_conditional_select {
  enum aarch64_cond cond;
  struct aarch64_reg true_source;
  struct aarch64_reg false_source;
  struct aarch64_reg dest;
};

struct aarch64_conditional_branch {
  enum aarch64_cond cond;
  struct instr *target;
};

struct aarch64_branch {
  struct instr *target;
};

struct aarch64_ret {
  struct aarch64_reg target;
};

struct aarch64_compare_and_branch {
  struct aarch64_reg cmp;
  struct instr *target;
};

struct aarch64_loadstore_imm {
  enum aarch64_addressing_mode mode;

  struct aarch64_reg dest;
  struct aarch64_reg source;
  size_t imm;
};

struct aarch64_loadstore_pair_imm {
  enum aarch64_addressing_mode mode;

  struct aarch64_reg dest[2];
  struct aarch64_reg source;
  size_t imm;
};

struct aarch64_instr {
  enum aarch64_instr_ty ty;

  union {
    struct aarch64_logical_reg logical_reg;
    struct aarch64_logical_imm logical_imm;
    struct aarch64_addr_imm addr_imm;
    struct aarch64_addsub_reg addsub_reg;
    struct aarch64_addsub_imm addsub_imm;
    struct aarch64_reg_1_source reg_1_source;
    struct aarch64_reg_2_source reg_2_source;
    struct aarch64_bitfield_imm bitfield_imm;
    struct aarch64_conditional_select conditional_select;
    struct aarch64_conditional_branch conditional_branch;
    struct aarch64_branch branch;
    struct aarch64_ret ret;
    struct aarch64_compare_and_branch compare_and_branch;
    struct aarch64_loadstore_imm loadstore_imm;
    struct aarch64_loadstore_pair_imm loadstore_pair_imm;
    struct aarch64_mov_imm mov_imm;
  };
};

void aarch64_codegen(const struct ir_builder *irb);

#endif
