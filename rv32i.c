#include "riscv.h"

#include "riscv/codegen.h"
#include "riscv/lower.h"

const struct target RISCV_TARGET = {
    (struct reg_info){
        .gp_registers = (struct reg_set_info){.num_volatile = 3,
                                              .num_nonvolatile = 3,
                                              .num_reserved = 2},
        .fp_registers =
            (struct reg_set_info){
                // FIXME: technically v8-15 are non-volatile bottom half
                // but we don't suppport vectors yet
                .num_volatile = 0,
                .num_nonvolatile = 0,
                .num_reserved = 0},
    },
    RISCV_FUNCTION_ALIGNMENT,
    NULL,
    riscv_lower,
    riscv_codegen,
    riscv_emit,
    NULL,
    NULL,
    NULL};
