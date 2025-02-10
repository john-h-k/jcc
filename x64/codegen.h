#ifndef X64_CODEGEN_H
#define X64_CODEGEN_H

#include "../codegen.h"

#include <stdio.h>

#if defined(STACK_PTR_REG) || defined(FRAME_PTR_REG) || defined(RET_PTR_REG)
#error                                                                         \
    "STACK_PTR_REG/FRAME_PTR_REG/RET_PTR_REG already defined. Check your includes"
#endif

// `[w|x]zr` and `sp` are encoded as the same thing and the instruction decides
// which is relevant
#define STACK_PTR_REG ((struct x64_reg){X64_REG_TY_X, 31})
#define FRAME_PTR_REG ((struct x64_reg){X64_REG_TY_X, 29})
#define RET_PTR_REG ((struct x64_reg){X64_REG_TY_X, 30})

typedef unsigned long long imm_t;
typedef long long simm_t;

enum x64_instr_ty {
  X64_INSTR_TY_MOV_IMM,
  X64_INSTR_TY_MOV_REG,

  X64_INSTR_TY_ADD,
  X64_INSTR_TY_SUB,

  X64_INSTR_TY_EOR,
  X64_INSTR_TY_OR,
  X64_INSTR_TY_AND,

  X64_INSTR_TY_NOT,
  X64_INSTR_TY_NEG,

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

struct x64_alu_unary {
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
      struct x64_alu_unary alu_unary, not, neg;
    };

    union {
      struct x64_mov_imm mov_imm;
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
