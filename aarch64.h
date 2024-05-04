#ifndef AARCH64_H
#define AARCH64_H

#include "target.h"
#include "disasm.h"
#include "aarch64/emit.h"
#include "aarch64/lower.h"
#include "macos/mach-o.h"

// FIXME: this unnecessarily ties arm64 to mach-o

#define AARCH64_FUNCTION_ALIGNMENT (16)

const struct target AARCH64_TARGET = {
  (struct reg_info) {.num_regs = 18},
  AARCH64_FUNCTION_ALIGNMENT,
  aarch64_lower,
  aarch64_emit_function,
  write_macho,
  objdump_debug_disasm
};

#endif
