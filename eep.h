#ifndef EEP_H
#define EEP_H

#include "target.h"
#include "eep/emit.h"
#include "eep/lower.h"
#include "eep/object.h"
#include "eep/disasm.h"

#define EEP_FUNCTION_ALIGNMENT (2)

const struct target EEP_TARGET = {
  (struct reg_info) {.num_regs = 8},
  EEP_FUNCTION_ALIGNMENT,
  eep_lower,
  eep_emit_function,
  write_eep,
  eep_debug_disasm
};

#endif

