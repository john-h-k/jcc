#include "rv32i.h"

#include "rv32i/codegen.h"
#include "rv32i/lower.h"
#include "rv32i/object.h"
#include "target.h"

const struct target RV32I_TARGET = {
    TARGET_ID_RV32I_UNKNOWN,
    TARGET_LP_SZ_LP32,
    {
        .gp_registers = {.num_volatile = 17,
                                              .num_nonvolatile = 10,
                                              .num_reserved = 0},
        .fp_registers = {.num_volatile = 19,
                                              .num_nonvolatile = 12,
                                              .num_reserved = 0},
    },
    RV32I_FUNCTION_ALIGNMENT,
    NULL,
    rv32i_lower,
    rv32i_codegen,
    rv32i_emit,
    rv32i_write_object,
    NULL,
    NULL,
    rv32i_debug_print_codegen};
