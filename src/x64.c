#include "x64.h"

#include "macos/link.h"
#include "macos/mach-o.h"
#include "linux/link.h"
#include "linux/elf.h"

#include "x64/codegen.h"
#include "x64/lower.h"
#include "target.h"

const struct target X64_MACOS_TARGET = {
    TARGET_ID_X64_MACOS,
    TARGET_LP_SZ_LP64,
    {
        .ssp = 7,
        .gp_registers = {.num_volatile = 9,
                                              .num_nonvolatile = 5,
                                              .num_reserved = 2},
        .fp_registers =
            {
                .num_volatile = 16,
                .num_nonvolatile = 0,
                .num_reserved = 0},
    },
    X64_FUNCTION_ALIGNMENT, x64_macos_mangle, x64_lower,
    x64_lower_func_ty,
    {.ty = CODEGEN_UNIT_TY_X64,
     .instr_sz = sizeof(struct x64_instr),
     .function_align = X64_FUNCTION_ALIGNMENT,
     .codegen_start = x64_codegen_start,
     .codegen_basicblock = x64_codegen_basicblock,
     .codegen_end = x64_codegen_end,
   },
    x64_emit, write_macho, macos_link_objects, objdump_debug_disasm,
    x64_debug_print_codegen,
NULL};

const struct target X64_LINUX_TARGET = {
    TARGET_ID_X64_LINUX,
    TARGET_LP_SZ_LP64,
    {
        .ssp = 7,
        .gp_registers = {.num_volatile = 9,
                                              .num_nonvolatile = 5,
                                              .num_reserved = 2},
        .fp_registers =
            {
                .num_volatile = 16,
                .num_nonvolatile = 0,
                .num_reserved = 0},
    },
    X64_FUNCTION_ALIGNMENT, x64_linux_mangle, x64_lower,
    x64_lower_func_ty,
    {.ty = CODEGEN_UNIT_TY_X64,
     .instr_sz = sizeof(struct x64_instr),
     .function_align = X64_FUNCTION_ALIGNMENT,
     .codegen_start = x64_codegen_start,
     .codegen_basicblock = x64_codegen_basicblock,
     .codegen_end = x64_codegen_end,
   },
    x64_emit, write_elf, linux_link_objects, objdump_debug_disasm,
    x64_debug_print_codegen, NULL};
