#include "eep.h"

#include "eep/lower.h"

const struct target EEP_TARGET =  {  (struct reg_info){
                                    .integral_registers = (struct reg_set_info){
                                        .num_volatile = 3, .num_nonvolatile = 3, .num_reserved = 2},
                                      .fp_registers = (struct reg_set_info){
                                          // FIXME: technically v8-15 are non-volatile bottom half
                                          // but we don't suppport vectors yet
                                          .num_volatile = 0, .num_nonvolatile = 0, .num_reserved = 0},
                                  },
                                  EEP_FUNCTION_ALIGNMENT,
                                  EEP_OP_SIZE,
                                  NULL,
                                  eep_pre_reg_lower,
                                  NULL,
                                  eep_emit_function,
                                  write_eep,
                                  eep_debug_disasm,
                                  eep_debug_print_custom_ir_op};
