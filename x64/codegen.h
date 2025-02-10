#ifndef X64_CODEGEN_H
#define X64_CODEGEN_H

#include "../codegen.h"

#include <stdio.h>

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

enum x64_instr_ty {
  // X64_INSTR_TY_LOAD_BYTE_IMM,
  // X64_INSTR_TY_LOAD_HALF_IMM,
  X64_INSTR_TY_MOV_LOAD_IMM,

  // X64_INSTR_TY_STORE_BYTE_IMM,
  // X64_INSTR_TY_STORE_HALF_IMM,
  X64_INSTR_TY_MOV_STORE_IMM,

  X64_INSTR_TY_PUSH,
  X64_INSTR_TY_POP,

  X64_INSTR_TY_MOV_IMM,
  X64_INSTR_TY_MOV_REG,

  X64_INSTR_TY_ADD_IMM,
  X64_INSTR_TY_SUB_IMM,

  X64_INSTR_TY_ADD,
  X64_INSTR_TY_SUB,

  X64_INSTR_TY_EOR,
  X64_INSTR_TY_OR,
  X64_INSTR_TY_AND,

  X64_INSTR_TY_NOT,
  X64_INSTR_TY_NEG,

  X64_INSTR_TY_SHL,
  X64_INSTR_TY_SHR,
  X64_INSTR_TY_SAR,

  X64_INSTR_TY_RET,
};

enum x64_reg_class {
  X64_REG_CLASS_GP,
  X64_REG_CLASS_FP,
};

enum x64_reg_ty {
  X64_REG_TY_R,
  X64_REG_TY_E,
  X64_REG_TY_RD,
};

enum x64_reg_attr_flags {
  X64_REG_ATTR_FLAG_NONE = 0,
  X64_REG_ATTR_FLAG_VOLATILE =
      1, // v8-15 are upper half only volatile, we don't support this yet
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

struct x64_1_reg {
  struct x64_reg dest;
};

struct x64_push {
  struct x64_reg source;
};

struct x64_pop {
  struct x64_reg dest;
};

struct x64_conditional_select {
  // enum x64_cond cond;
  struct x64_reg true_source;
  struct x64_reg false_source;
  struct x64_reg dest;
};

struct x64_conditional_branch {
  // enum x64_cond cond;
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
      struct x64_alu_reg alu_reg, add, sub, eor, or, and;
    };

    union {
      struct x64_alu_imm alu_imm, add_imm, sub_imm, eor_imm, or_imm, and_imm;
    };

    union {
      struct x64_shift shift, shl, shr, sar;
    };

    union {
      struct x64_1_reg alu_unary, not, neg;
    };

    union {
      struct x64_push push;
    };

    union {
      struct x64_pop pop;
    };

    union {
      struct x64_mov_imm mov_imm;
    };

    union {
      struct x64_mov_load_imm mov_load_imm, mov_load32_imm;
    };

    union {
      struct x64_mov_store_imm mov_store_imm, mov_store32_imm;
    };

    union {
      struct x64_mov_reg mov_reg;
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

bool is_return_reg(struct x64_reg reg);
bool is_zero_reg(struct x64_reg reg);

enum x64_instr_class instr_class(enum x64_instr_ty ty);

typedef void(walk_regs_callback)(struct instr *instr, struct x64_reg reg,
                                 enum x64_reg_usage_ty usage_ty,
                                 void *metadata);
void walk_regs(const struct codegen_function *func, walk_regs_callback *cb,
               void *metadata);

struct codegen_unit *x64_codegen(struct ir_unit *ir);
void x64_debug_print_codegen(FILE *file, struct codegen_unit *unit);

#endif
