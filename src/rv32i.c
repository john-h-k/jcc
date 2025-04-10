#include "rv32i.h"

#include "disasm.h"
#include "linux/elf.h"
#include "linux/link.h"
#include "rv32i/codegen.h"
#include "rv32i/emit.h"
#include "rv32i/lower.h"
#include "target.h"
#include "typechk.h"

#if !defined(JCC_ALL) && !defined(JCC_RV32I)

const struct target RV32I_LINUX_TARGET = {.target_id = TARGET_ID_NOT_SUPPORTED};

#else

const struct target RV32I_LINUX_TARGET = {
    TARGET_ID_RV32I_LINUX,
    TARGET_LP_SZ_LP32,
    {
        // TODO:
        .va_list_var_ty = &TD_VAR_TY_UNKNOWN
    },
    {
        .ssp = 9,
        .gp_registers = {.max_reg_size = 4,
                         .num_volatile = 15,
                         .num_nonvolatile = 12,
                         .num_reserved = 4},
        .fp_registers = {.max_reg_size = 8,
                         .num_volatile = 20,
                         .num_nonvolatile = 12,
                         .num_reserved = 0},
    },
    RV32I_FUNCTION_ALIGNMENT,
    rv32i_mangle,
    rv32i_lower,
    rv32i_lower_func_ty,
    {
        .ty = CODEGEN_UNIT_TY_RV32I,
        .instr_sz = sizeof(struct rv32i_instr),
        .function_align = RV32I_FUNCTION_ALIGNMENT,
        .codegen_start = rv32i_codegen_start,
        .codegen_basicblock = rv32i_codegen_basicblock,
        .codegen_end = rv32i_codegen_end,
    },
    rv32i_emit,
    // rv32i_write_object,
    write_elf,
    linux_link_objects,
    objdump_debug_disasm,
    rv32i_debug_print_codegen,
    rv32i_emit_asm};

#endif
