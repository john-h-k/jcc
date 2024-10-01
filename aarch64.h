#ifndef AARCH64_H
#define AARCH64_H

#include "aarch64/emit.h"
#include "aarch64/lower.h"
#include "disasm.h"
#include "macos/mach-o.h"
#include "target.h"

// FIXME: this unnecessarily ties arm64 to mach-o

#define AARCH64_FUNCTION_ALIGNMENT (16)
#define AARCH64_STACK_ALIGNMENT (16)
#define AARCH64_OP_SIZE (4)

const struct target AARCH64_TARGET;

struct aarch64_op_page {
  struct ir_op *glb_ref;
};

struct aarch64_op_page_off {
  struct ir_op *glb_ref;
};

struct aarch64_store_variadic {
  struct ir_op *value;
  size_t idx;
};

enum aarch64_op_ty {
  // Saving LR has two steps - one instr per op so we have two ops for it
  AARCH64_OP_TY_SAVE_LR,
  AARCH64_OP_TY_SAVE_FP,

  AARCH64_OP_TY_RSTR_LR,

  AARCH64_OP_TY_SUB_STACK,
  AARCH64_OP_TY_ADD_STACK,

  AARCH64_OP_TY_SAVE_REG,
  AARCH64_OP_TY_RSTR_REG,

  AARCH64_OP_TY_PAGE,
  AARCH64_OP_TY_PAGE_OFF,

  AARCH64_OP_TY_STORE_VARIADIC,
};

struct aarch64_op {
  enum aarch64_op_ty ty;

  union {
    struct aarch64_op_page page;
    struct aarch64_op_page_off page_off;
    struct aarch64_store_variadic store_variadic;
  };
};

#endif
