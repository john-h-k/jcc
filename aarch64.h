#ifndef AARCH64_H
#define AARCH64_H

#include "target.h"
#include "disasm.h"
#include "aarch64/emit.h"
#include "aarch64/lower.h"
#include "macos/mach-o.h"

// FIXME: this unnecessarily ties arm64 to mach-o

#define AARCH64_FUNCTION_ALIGNMENT (16)
#define AARCH64_STACK_ALIGNMENT (16)
#define AARCH64_OP_SIZE (4)

const struct target AARCH64_TARGET;

enum aarch64_op_ty {
  AARCH64_OP_TY_SAVE_LR,
  AARCH64_OP_TY_RSTR_LR,

  AARCH64_OP_TY_SUB_STACK,
  AARCH64_OP_TY_ADD_STACK,

  AARCH64_OP_TY_SAVE_REG,
  AARCH64_OP_TY_RSTR_REG,
};

struct aarch64_op {
  enum aarch64_op_ty ty;
};

#endif
