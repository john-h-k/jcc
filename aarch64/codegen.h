#ifndef AARCH64_CODEGEN_H
#define AARCH64_CODEGEN_H

#include "../codegen.h"

#if defined(STACK_PTR_REG) || defined(FRAME_PTR_REG) || defined(RET_PTR_REG)
#error                                                                         \
    "STACK_PTR_REG/FRAME_PTR_REG/RET_PTR_REG already defined. Check your includes"
#endif

// `[w|x]zr` and `sp` are encoded as the same thing and the instruction decides
// which is relevant
#define STACK_PTR_REG ((struct aarch64_reg){AARCH64_REG_TY_X, 31})
#define FRAME_PTR_REG ((struct aarch64_reg){AARCH64_REG_TY_X, 29})
#define RET_PTR_REG ((struct aarch64_reg){AARCH64_REG_TY_X, 30})

typedef unsigned long long imm_t;
typedef long long simm_t;

enum aarch64_instr_ty {
  AARCH64_INSTR_TY_FADD,
  AARCH64_INSTR_TY_FMUL,
  AARCH64_INSTR_TY_FDIV,
  AARCH64_INSTR_TY_FSUB,

  AARCH64_INSTR_TY_ADDS,
  AARCH64_INSTR_TY_ADDS_IMM,
  AARCH64_INSTR_TY_ADD,
  AARCH64_INSTR_TY_ADD_IMM,
  AARCH64_INSTR_TY_ADR,
  AARCH64_INSTR_TY_ADRP,
  AARCH64_INSTR_TY_ANDS,
  AARCH64_INSTR_TY_ANDS_IMM,
  AARCH64_INSTR_TY_AND,
  AARCH64_INSTR_TY_AND_IMM,
  AARCH64_INSTR_TY_ASRV,
  AARCH64_INSTR_TY_B,
  AARCH64_INSTR_TY_BR,
  AARCH64_INSTR_TY_BC_COND,
  AARCH64_INSTR_TY_BFM,
  AARCH64_INSTR_TY_BL,
  AARCH64_INSTR_TY_BLR,
  AARCH64_INSTR_TY_B_COND,
  AARCH64_INSTR_TY_CBZ,
  AARCH64_INSTR_TY_CBNZ,
  AARCH64_INSTR_TY_CSEL,
  AARCH64_INSTR_TY_CSINC,
  AARCH64_INSTR_TY_CSINV,
  AARCH64_INSTR_TY_CSNEG,
  AARCH64_INSTR_TY_EON,
  AARCH64_INSTR_TY_EOR,
  AARCH64_INSTR_TY_EOR_IMM,
  AARCH64_INSTR_TY_LOAD_IMM,
  AARCH64_INSTR_TY_LOAD_BYTE_IMM,
  AARCH64_INSTR_TY_LOAD_HALF_IMM,
  AARCH64_INSTR_TY_LOAD_PAIR_IMM,
  AARCH64_INSTR_TY_LSLV,
  AARCH64_INSTR_TY_LSRV,
  AARCH64_INSTR_TY_MADD,
  AARCH64_INSTR_TY_MOVN,
  AARCH64_INSTR_TY_MOVZ,
  AARCH64_INSTR_TY_MOVK,
  AARCH64_INSTR_TY_FMOV,
  AARCH64_INSTR_TY_FCVT,
  AARCH64_INSTR_TY_UCVTF,
  AARCH64_INSTR_TY_SCVTF,
  AARCH64_INSTR_TY_MSUB,
  AARCH64_INSTR_TY_NOP,
  AARCH64_INSTR_TY_ORN,
  AARCH64_INSTR_TY_ORR,
  AARCH64_INSTR_TY_ORR_IMM,
  AARCH64_INSTR_TY_RET,
  AARCH64_INSTR_TY_RORV,
  AARCH64_INSTR_TY_SBFM,
  AARCH64_INSTR_TY_SDIV,
  AARCH64_INSTR_TY_STORE_IMM,
  AARCH64_INSTR_TY_STORE_BYTE_IMM,
  AARCH64_INSTR_TY_STORE_HALF_IMM,
  AARCH64_INSTR_TY_STORE_PAIR_IMM,
  AARCH64_INSTR_TY_SUBS,
  AARCH64_INSTR_TY_SUB,
  AARCH64_INSTR_TY_SUB_IMM,
  AARCH64_INSTR_TY_SUBS_IMM,
  AARCH64_INSTR_TY_UBFM,
  AARCH64_INSTR_TY_UDIV
};

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

bool aarch64_reg_ty_is_gp(enum aarch64_reg_ty ty);
bool aarch64_reg_ty_is_fp(enum aarch64_reg_ty ty);

struct aarch64_reg {
  enum aarch64_reg_ty ty;

  size_t idx;
};

enum aarch64_instr_class {
  AARCH64_INSTR_CLASS_NOP,
  AARCH64_INSTR_CLASS_LOGICAL_REG,
  AARCH64_INSTR_CLASS_LOGICAL_IMM,
  AARCH64_INSTR_CLASS_ADDSUB_REG,
  AARCH64_INSTR_CLASS_ADDSUB_IMM,
  AARCH64_INSTR_CLASS_ADDR_IMM,
  AARCH64_INSTR_CLASS_BITFIELD,
  AARCH64_INSTR_CLASS_REG_1_SOURCE,
  AARCH64_INSTR_CLASS_REG_2_SOURCE,
  AARCH64_INSTR_CLASS_MOV_IMM,
  AARCH64_INSTR_CLASS_FMA,
  AARCH64_INSTR_CLASS_CONDITIONAL_SELECT,
  AARCH64_INSTR_CLASS_CONDITIONAL_BRANCH,
  AARCH64_INSTR_CLASS_BRANCH,
  AARCH64_INSTR_CLASS_BRANCH_REG,
  AARCH64_INSTR_CLASS_COMPARE_AND_BRANCH,
  AARCH64_INSTR_CLASS_LOAD_IMM,
  AARCH64_INSTR_CLASS_STORE_IMM,
  AARCH64_INSTR_CLASS_LOAD_PAIR_IMM,
  AARCH64_INSTR_CLASS_STORE_PAIR_IMM,
};

struct aarch64_logical_reg {
  struct aarch64_reg dest;
  struct aarch64_reg lhs;
  struct aarch64_reg rhs;

  imm_t imm6;
  enum aarch64_shift shift;
};

struct aarch64_logical_imm {
  struct aarch64_reg dest;
  struct aarch64_reg source;

  imm_t n;
  imm_t immr;
  imm_t imms;
};

struct aarch64_addsub_imm {
  struct aarch64_reg dest;
  struct aarch64_reg source;

  imm_t imm;
  size_t shift;
};

struct aarch64_addsub_reg {
  struct aarch64_reg dest;
  struct aarch64_reg lhs;
  struct aarch64_reg rhs;

  imm_t imm6;
  enum aarch64_shift shift;
};

struct aarch64_addr_imm {
  struct aarch64_reg dest;
  size_t imm;
};

struct aarch64_bitfield {
  struct aarch64_reg dest;
  struct aarch64_reg source;

  imm_t immr;
  imm_t imms;
};

struct aarch64_reg_1_source {
  struct aarch64_reg dest;
  struct aarch64_reg source;
};

struct aarch64_reg_2_source {
  struct aarch64_reg dest;
  struct aarch64_reg lhs;
  struct aarch64_reg rhs;
};

struct aarch64_mov_imm {
  struct aarch64_reg dest;
  imm_t imm;
  size_t shift;
};

struct aarch64_fma {
  struct aarch64_reg dest;
  struct aarch64_reg lhs;
  struct aarch64_reg rhs;
  struct aarch64_reg addsub;
};

struct aarch64_conditional_select {
  enum aarch64_cond cond;
  struct aarch64_reg true_source;
  struct aarch64_reg false_source;
  struct aarch64_reg dest;
};

struct aarch64_conditional_branch {
  enum aarch64_cond cond;
  struct ir_basicblock *target;
};

struct aarch64_branch {
  struct ir_basicblock *target;
};

struct aarch64_branch_reg {
  struct aarch64_reg target;
};

struct aarch64_compare_and_branch {
  struct aarch64_reg cmp;
  struct ir_basicblock *target;
};

struct aarch64_load_imm {
  enum aarch64_addressing_mode mode;

  struct aarch64_reg dest;
  struct aarch64_reg addr;
  simm_t imm;
};

struct aarch64_store_imm {
  enum aarch64_addressing_mode mode;

  struct aarch64_reg source;
  struct aarch64_reg addr;
  simm_t imm;
};

struct aarch64_load_pair_imm {
  enum aarch64_addressing_mode mode;

  struct aarch64_reg dest[2];
  struct aarch64_reg addr;
  simm_t imm;
};

struct aarch64_store_pair_imm {
  enum aarch64_addressing_mode mode;

  struct aarch64_reg source[2];
  struct aarch64_reg addr;
  simm_t imm;
};

struct aarch64_instr {
  enum aarch64_instr_ty ty;

  union {
    union {
      struct aarch64_logical_reg logical_reg, and, ands, orr, orn, eor, eon;
    };

    union {
      struct aarch64_logical_imm logical_imm, and_imm, ands_imm, orr_imm,
          orn_imm, eor_imm, eon_imm;
    };

    union {
      struct aarch64_addr_imm addr_imm, adr, adrp;
    };

    union {
      struct aarch64_addsub_reg addsub_reg, add, adds, sub, subs;
    };

    union {
      struct aarch64_addsub_imm addsub_imm, add_imm, adds_imm, sub_imm,
          subs_imm;
    };

    union {
      struct aarch64_reg_1_source reg_1_source, fmov, fcvt, ucvtf, scvtf;
    };

    union {
      struct aarch64_reg_2_source reg_2_source, asrv, lslv, lsrv, rorv, sdiv,
          udiv, fmul, fdiv, fadd, fsub;
    };

    union {
      struct aarch64_fma fma, madd, msub;
    };

    union {
      struct aarch64_bitfield bitfield, sbfm, bfm, ubfm;
    };

    union {
      struct aarch64_conditional_select conditional_select, csel, csinc, csinv,
          csneg;
    };

    union {
      struct aarch64_conditional_branch conditional_branch, b_cond, bc_cond;
    };

    union {
      struct aarch64_branch branch, b, bl;
    };

    union {
      struct aarch64_branch_reg branch_reg, br, blr, ret;
    };

    union {
      struct aarch64_compare_and_branch compare_and_branch, cbz, cbnz;
    };

    union {
      struct aarch64_load_imm load_imm, ldr_imm, ldrh_imm, ldrb_imm;
    };

    union {
      struct aarch64_store_imm store_imm, str_imm, strh_imm, strb_imm;
    };

    union {
      struct aarch64_load_pair_imm load_pair_imm, ldp_imm;
    };

    union {
      struct aarch64_store_pair_imm store_pair_imm, stp_imm;
    };

    union {
      struct aarch64_mov_imm mov_imm, movz, movk, movn;
    };
  };
};

enum aarch64_reg_usage_ty {
  AARCH64_REG_USAGE_TY_WRITE, // mov x9, ...
  AARCH64_REG_USAGE_TY_READ,  // mov ..., x9
  AARCH64_REG_USAGE_TY_DEREF, // ldr ..., [x9]
};

struct aarch64_reg return_reg_for_ty(enum aarch64_reg_ty reg_ty);
struct aarch64_reg zero_reg_for_ty(enum aarch64_reg_ty reg_ty);

size_t reg_size(enum aarch64_reg_ty reg_ty);

struct aarch64_reg get_full_reg_for_ir_reg(struct ir_reg reg);

bool is_return_reg(struct aarch64_reg reg);
bool is_zero_reg(struct aarch64_reg reg);
bool reg_eq(struct aarch64_reg l, struct aarch64_reg r);

enum aarch64_instr_class instr_class(enum aarch64_instr_ty ty);

typedef void(walk_regs_callback)(struct instr *instr, struct aarch64_reg reg,
                                 enum aarch64_reg_usage_ty usage_ty,
                                 void *metadata);
void walk_regs(const struct codegen_function *func, walk_regs_callback *cb,
               void *metadata);

struct codegen_unit *aarch64_codegen(struct ir_unit *ir);
void aarch64_debug_print_codegen(FILE *file, struct codegen_unit *unit);

#endif
