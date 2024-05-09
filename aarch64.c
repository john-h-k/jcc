#include "aarch64.h"

const struct target AARCH64_TARGET = {
  // x0..x30 excluding x18
  (struct reg_info) {.num_volatile = 18, .num_nonvolatile = 10, .num_reserved = 2},
  AARCH64_FUNCTION_ALIGNMENT,
  AARCH64_OP_SIZE,
  aarch64_pre_reg_lower,
  aarch64_post_reg_lower,
  aarch64_emit_function,
  write_macho,
  objdump_debug_disasm
};
