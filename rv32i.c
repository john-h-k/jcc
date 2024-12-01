#include "rv32i.h"

#include "rv32i/codegen.h"
#include "rv32i/lower.h"
#include "rv32i/object.h"

const struct target RV32I_TARGET = {
    (struct reg_info){
        .gp_registers = (struct reg_set_info){.num_volatile = 17,
                                              .num_nonvolatile = 10,
                                              .num_reserved = 0},
        .fp_registers =
            (struct reg_set_info){
                // FIXME: technically v8-15 are non-volatile bottom half
                // but we don't suppport vectors yet
                .num_volatile = 0,
                .num_nonvolatile = 0,
                .num_reserved = 0},
    },
    RV32I_FUNCTION_ALIGNMENT,
    NULL,
    rv32i_lower,
    rv32i_codegen,
    rv32i_emit,
    rv32i_write_object,
    NULL,
    rv32i_debug_print_codegen};
