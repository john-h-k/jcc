#include "eep.h"

const struct target EEP_TARGET = {
  (struct reg_info) {.num_volatile = 3, .num_nonvolatile = 3, .num_reserved = 2},
  EEP_FUNCTION_ALIGNMENT,
  EEP_OP_SIZE,
  NULL,
  eep_pre_reg_lower,
  NULL,
  eep_emit_function,
  write_eep,
  eep_debug_disasm
};
