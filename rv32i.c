#include "rv32i.h"

#include "linux/link.h"
#include "linux/elf.h"
#include "rv32i/codegen.h"
#include "rv32i/emit.h"
#include "rv32i/lower.h"
#include "disasm.h"
#include "target.h"


const struct target RV32I_LINUX_TARGET = {
    TARGET_ID_RV32I_LINUX,
    TARGET_LP_SZ_LP32,
    {
        .ssp = 9,
        .gp_registers = {.num_volatile = 17,
                                              .num_nonvolatile = 10,
                                              .num_reserved = 4},
        .fp_registers = {.num_volatile = 20,
                                              .num_nonvolatile = 12,
                                              .num_reserved = 0},
    },
    RV32I_FUNCTION_ALIGNMENT,
    rv32i_mangle,
    rv32i_lower,
    rv32i_lower_func_ty,
    rv32i_codegen,
    rv32i_emit,
    // rv32i_write_object,
    write_elf,
    linux_link_objects,
    objdump_debug_disasm,
    rv32i_debug_print_codegen};
