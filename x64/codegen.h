#ifndef X64_CODEGEN_H
#define X64_CODEGEN_H

#include "../codegen.h"

#include <stdio.h>
#include <assert.h>

#if defined(STACK_PTR_REG) || defined(FRAME_PTR_REG) || defined(RET_PTR_REG)
#error                                                                         \
    "STACK_PTR_REG/FRAME_PTR_REG/RET_PTR_REG already defined. Check your includes"
#endif

#define REG_IDX_AX (0)
#define REG_IDX_CX (1)
#define REG_IDX_DX (2)
#define REG_IDX_BX (3)
#define REG_IDX_SP (4)
#define REG_IDX_BP (5)
#define REG_IDX_SI (6)
#define REG_IDX_DI (7)

#define STACK_PTR_REG ((struct x64_reg){X64_REG_TY_R, REG_IDX_SP})
#define FRAME_PTR_REG ((struct x64_reg){X64_REG_TY_R, REG_IDX_BP})

typedef unsigned long long imm_t;
typedef long long simm_t;

static_assert(sizeof(imm_t) == 8, "imm_t must be 8 bytes");
static_assert(sizeof(simm_t) == 8, "imm_t must be 8 bytes");

enum x64_instr_ty {
  X64_INSTR_TY_MOVZX_LOAD_BYTE_IMM,
  X64_INSTR_TY_MOVZX_LOAD_HALF_IMM,
  X64_INSTR_TY_MOV_LOAD_IMM,

  X64_INSTR_TY_MOV_LOAD_SS_IMM,
  X64_INSTR_TY_MOV_LOAD_SD_IMM,

  X64_INSTR_TY_MOV_STORE_BYTE_IMM,
  X64_INSTR_TY_MOV_STORE_HALF_IMM,
  X64_INSTR_TY_MOV_STORE_IMM,

  X64_INSTR_TY_MOV_STORE_SS_IMM,
  X64_INSTR_TY_MOV_STORE_SD_IMM,

  X64_INSTR_TY_MOVSX,
  // X64_INSTR_TY_MOVZX,

  X64_INSTR_TY_LEA,
  X64_INSTR_TY_LEA_PCREL,

  X64_INSTR_TY_PUSH,
  X64_INSTR_TY_POP,

  X64_INSTR_TY_MOV_IMM,
  X64_INSTR_TY_MOV_REG,

  X64_INSTR_TY_ADD_IMM,
  X64_INSTR_TY_SUB_IMM,

  X64_INSTR_TY_MOVAPS,
  X64_INSTR_TY_MOVAPD,

  X64_INSTR_TY_SQRTSS,
  X64_INSTR_TY_SQRTSD,

  X64_INSTR_TY_UCOMISS,
  X64_INSTR_TY_UCOMISD,

  X64_INSTR_TY_ANDPS,
  X64_INSTR_TY_ANDPD,
  X64_INSTR_TY_XORPS,
  X64_INSTR_TY_XORPD,
  X64_INSTR_TY_ORPS,
  X64_INSTR_TY_ORPD,

  X64_INSTR_TY_ADDSS,
  X64_INSTR_TY_ADDSD,
  X64_INSTR_TY_SUBSS,
  X64_INSTR_TY_SUBSD,
  X64_INSTR_TY_MULSS,
  X64_INSTR_TY_MULSD,
  X64_INSTR_TY_DIVSS,
  X64_INSTR_TY_DIVSD,

  X64_INSTR_TY_CVTSI2SS,
  X64_INSTR_TY_CVTSI2SD,

  X64_INSTR_TY_CVTTSS2SI,
  X64_INSTR_TY_CVTTSD2SI,

  X64_INSTR_TY_CVTSS2SI,
  X64_INSTR_TY_CVTSD2SI,

  X64_INSTR_TY_CVTSS2SD,
  X64_INSTR_TY_CVTSD2SS,

  X64_INSTR_TY_ADD,
  X64_INSTR_TY_SUB,

  X64_INSTR_TY_IMUL,
  X64_INSTR_TY_DIV,
  X64_INSTR_TY_IDIV,

  X64_INSTR_TY_XOR,
  X64_INSTR_TY_OR,
  X64_INSTR_TY_AND,

  X64_INSTR_TY_XOR_IMM,
  X64_INSTR_TY_OR_IMM,
  X64_INSTR_TY_AND_IMM,

  X64_INSTR_TY_NOT,
  X64_INSTR_TY_NEG,

  X64_INSTR_TY_SHL,
  X64_INSTR_TY_SHR,
  X64_INSTR_TY_SAR,

  X64_INSTR_TY_JMP,
  X64_INSTR_TY_JMP_REG,
  X64_INSTR_TY_JCC,

  X64_INSTR_TY_SETCC,

  X64_INSTR_TY_CALL,
  X64_INSTR_TY_CALL_REG,

  X64_INSTR_TY_CMP,
  X64_INSTR_TY_TEST,

  X64_INSTR_TY_CMP_IMM,
  X64_INSTR_TY_TEST_IMM,

  X64_INSTR_TY_RET,

  X64_INSTR_TY_MOVQ,
  X64_INSTR_TY_MOVD,
};

enum x64_reg_class {
  X64_REG_CLASS_GP,
  X64_REG_CLASS_FP,
};

enum x64_reg_ty {
  X64_REG_TY_NONE,
  X64_REG_TY_R, // 64 bit
  X64_REG_TY_E, // 32 bit
  X64_REG_TY_W, // 16 bit
  X64_REG_TY_L, // 8 bit

  X64_REG_TY_XMM, // 128 bit
};

enum x64_reg_attr_flags {
  X64_REG_ATTR_FLAG_NONE = 0,
  X64_REG_ATTR_FLAG_VOLATILE = 1, // v8-15 are upper half only volatile, we don't support this yet
  X64_REG_ATTR_FLAG_ARG_REG = 2,
  X64_REG_ATTR_FLAG_RET_REG = 4,
  X64_REG_ATTR_FLAG_RESERVED = 8,
};

bool x64_reg_ty_is_gp(enum x64_reg_ty ty);
bool x64_reg_ty_is_fp(enum x64_reg_ty ty);

struct x64_reg {
  enum x64_reg_ty ty;

  size_t idx;
};

enum x64_reg_attr_flags reg_attr_flags(struct x64_reg reg);

enum x64_instr_class {
  X64_INSTR_CLASS_NOP,
  X64_INSTR_CLASS_RET,
  X64_INSTR_CLASS_MOV_IMM,
};

enum x64_cond {
  X64_COND_OVERFLOW = 0x0,
  X64_COND_NOT_OVERFLOW = 0x1,

  X64_COND_BELOW = 0x2,
  X64_COND_NOT_BELOW = 0x3,

  X64_COND_ZERO = 0x4,
  X64_COND_NOT_ZERO = 0x5,

  X64_COND_BELOW_OR_EQUAL = 0x6,
  X64_COND_NOT_BELOW_OR_EQUAL = 0x7,

  X64_COND_SIGN = 0x8,
  X64_COND_NOT_SIGN = 0x9,

  X64_COND_PARITY = 0xA,
  X64_COND_NOT_PARITY = 0xB,

  X64_COND_LESS = 0xC,
  X64_COND_NOT_LESS = 0xD,

  X64_COND_LESS_OR_EQUAL = 0xE,
  X64_COND_NOT_LESS_OR_EQUAL = 0xF,
};

struct x64_mov_load_imm {
  struct x64_reg dest;
  struct x64_reg addr;
  imm_t imm;
};

struct x64_mov_store_imm {
  struct x64_reg source;
  struct x64_reg addr;
  imm_t imm;
};

struct x64_mov_imm {
  struct x64_reg dest;
  imm_t imm;
};

struct x64_mov_reg {
  struct x64_reg dest;
  struct x64_reg source;
};

struct x64_alu_reg {
  struct x64_reg dest;
  struct x64_reg rhs;
};

struct x64_alu_imm {
  struct x64_reg dest;
  imm_t imm;
};

struct x64_shift {
  struct x64_reg dest;
};

struct x64_cmp {
  struct x64_reg lhs;
  struct x64_reg rhs;
};

struct x64_cmp_imm {
  struct x64_reg lhs;
  imm_t imm;
};

struct x64_1_reg {
  struct x64_reg dest;
};

struct x64_2_reg_unary {
  struct x64_reg dest;
  struct x64_reg source;
};

struct x64_div {
  struct x64_reg rhs;
};

struct x64_mul {
  struct x64_reg rhs;
};

struct x64_push {
  struct x64_reg source;
};

struct x64_pop {
  struct x64_reg dest;
};

struct x64_lea_pcrel {
  struct x64_reg dest;
  size_t offset;
};

struct x64_lea {
  struct x64_reg dest;
  struct x64_reg base;
  struct x64_reg index;
  size_t scale;
  size_t offset;
};

struct x64_conditional_select {
  enum x64_cond cond;
  struct x64_reg dest;
};

struct x64_conditional_branch {
  enum x64_cond cond;
  struct ir_basicblock *target;
};

struct x64_branch {
  struct ir_basicblock *target;
};

struct x64_branch_reg {
  struct x64_reg target;
};

struct x64_instr {
  enum x64_instr_ty ty;

  union {
    union {
      struct x64_alu_reg alu_reg, add, sub, xor, or, and, andps, andpd, xorps, xorpd, orps, orpd, addss, addsd, subss, subsd, mulss, mulsd, divss, divsd;
    };

    union {
      struct x64_alu_imm alu_imm, add_imm, sub_imm, xor_imm, or_imm, and_imm;
    };

    union {
      struct x64_shift shift, shl, shr, sar;
    };

    union {
      struct x64_2_reg_unary two_reg_unary, sqrtss, sqrtsd, movaps, movapd, cvt, cvtsi2ss, cvtsi2sd, cvttss2si, cvttsd2si, cvtss2si, cvtsd2si, cvtsd2ss, cvtss2sd;
    };

    union {
      struct x64_1_reg one_reg, not, neg;
    };

    union {
      struct x64_push push;
    };

    union {
      struct x64_lea lea;
    };

    union {
      struct x64_lea_pcrel lea_pcrel;
    };

    union {
      struct x64_pop pop;
    };

    union {
      struct x64_cmp cmp, test, ucomiss, ucomisd;
    };

    union {
      struct x64_cmp_imm cmp_imm, test_imm;
    };

    union {
      struct x64_div div, idiv;
    };

    union {
      struct x64_mul mul, imul;
    };

    union {
      struct x64_mov_imm mov_imm;
    };

    union {
      struct x64_mov_load_imm mov_load_imm, movzx_load_half_imm, movzx_load_byte_imm, mov_load_ss_imm, mov_load_sd_imm;
    };

    union {
      struct x64_mov_store_imm mov_store_imm, mov_store_half_imm, mov_store_byte_imm, mov_store_ss_imm, mov_store_sd_imm;
    };

    union {
      struct x64_mov_reg mov_reg, movsx, movzx, movq, movd;
    };

    union {
      struct x64_branch branch, jmp, call;
    };

    union {
      struct x64_conditional_select conditional_select, setcc;
    };

    union {
      struct x64_branch_reg branch_reg, jmp_reg, call_reg;
    };

    union {
      struct x64_conditional_branch conditional_branch, jcc;
    };
  };
};

enum x64_reg_usage_ty {
  X64_REG_USAGE_TY_WRITE, // mov x9, ...
  X64_REG_USAGE_TY_READ,  // mov ..., x9
  X64_REG_USAGE_TY_DEREF, // ldr ..., [x9]
};

size_t reg_size(enum x64_reg_ty reg_ty);

struct x64_reg get_full_reg_for_ir_reg(struct ir_reg reg);

enum x64_instr_class instr_class(enum x64_instr_ty ty);

typedef void(walk_regs_callback)(struct instr *instr, struct x64_reg reg,
                                 enum x64_reg_usage_ty usage_ty,
                                 void *metadata);
void walk_regs(const struct codegen_function *func, walk_regs_callback *cb,
               void *metadata);

struct codegen_unit *x64_codegen(struct ir_unit *ir);
void x64_debug_print_codegen(FILE *file, struct codegen_unit *unit);

#endif
