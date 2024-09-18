#include "eep.h"

#include "eep/lower.h"

const struct target EEP_TARGET = {(struct reg_info){.num_volatile = 3,
                                                    .num_nonvolatile = 3,
                                                    .num_reserved = 2},
                                  EEP_FUNCTION_ALIGNMENT,
                                  EEP_OP_SIZE,
                                  NULL,
                                  eep_pre_reg_lower,
                                  NULL,
                                  eep_emit_function,
                                  write_eep,
                                  eep_debug_disasm,
                                  eep_debug_print_custom_ir_op};
