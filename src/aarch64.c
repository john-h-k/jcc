#include "aarch64.h"

#include "aarch64/codegen.h"
#include "aarch64/emit.h"
#include "aarch64/lower.h"
#include "disasm.h"
#include "linux/elf.h"
#include "linux/link.h"
#include "macos/link.h"
#include "macos/mach-o.h"
#include "target.h"
#include "typechk.h"

#if !defined(JCC_ALL) && !defined(JCC_AARCH64)

const struct target AARCH64_MACOS_TARGET = {.target_id =
                                                TARGET_ID_NOT_SUPPORTED};
const struct target AARCH64_LINUX_TARGET = {.target_id =
                                                TARGET_ID_NOT_SUPPORTED};

#else

const struct target AARCH64_MACOS_TARGET = {
    TARGET_ID_AARCH64_MACOS,
    TARGET_LP_SZ_LP64,
    // x0..x30 excluding x18
    {
        .va_list_var_ty = &TD_VAR_TY_VOID_POINTER,
        .flags = TARGET_VARIADIC_INFO_FLAG_NONE
    },
    {
        .ssp = 9,
        .gp_registers = {.max_reg_size = 8,
                         .num_volatile = 18,
                         .num_nonvolatile = 10,
                         .num_reserved = 2},
        .fp_registers =
            {// FIXME: technically v8-15 are volatile top half
             // but we don't suppport vectors yet
             .max_reg_size = 8, // TODO: vectors
             .num_volatile = 24,
             .num_nonvolatile = 8,
             .num_reserved = 0},
    },
    AARCH64_FUNCTION_ALIGNMENT,
    aarch64_macos_mangle,
    aarch64_lower,
    aarch64_lower_func_ty,
    {
        .ty = CODEGEN_UNIT_TY_AARCH64,
        .instr_sz = sizeof(struct aarch64_instr),
        .function_align = AARCH64_FUNCTION_ALIGNMENT,
        .codegen_start = aarch64_codegen_start,
        .codegen_basicblock = aarch64_codegen_basicblock,
        .codegen_end = aarch64_codegen_end,
    },
    aarch64_emit,
    write_macho,
    macos_link_objects,
    objdump_debug_disasm,
    aarch64_debug_print_codegen,
    NULL};

const struct target AARCH64_LINUX_TARGET = {
    TARGET_ID_AARCH64_LINUX,
    TARGET_LP_SZ_LP64,
    // x0..x30 excluding x18
    {
        // TODO:
        .va_list_var_ty = &TD_VAR_TY_UNKNOWN,
        .flags = TARGET_VARIADIC_INFO_FLAG_VA_LIST_BYREF
    },
    {
        .ssp = 9,
        .gp_registers = {.max_reg_size = 8,
                         .num_volatile = 18,
                         .num_nonvolatile = 10,
                         .num_reserved = 2},
        .fp_registers =
            {// FIXME: technically v8-15 are volatile top half
             // but we don't suppport vectors yet
             .max_reg_size = 8, // TODO: vectors
             .num_volatile = 24,
             .num_nonvolatile = 8,
             .num_reserved = 0},
    },
    AARCH64_FUNCTION_ALIGNMENT,
    aarch64_linux_mangle,
    aarch64_lower,
    aarch64_lower_func_ty,
    {
        .ty = CODEGEN_UNIT_TY_AARCH64,
        .instr_sz = sizeof(struct aarch64_instr),
        .function_align = AARCH64_FUNCTION_ALIGNMENT,
        .codegen_start = aarch64_codegen_start,
        .codegen_basicblock = aarch64_codegen_basicblock,
        .codegen_end = aarch64_codegen_end,
    },
    aarch64_emit,
    write_elf,
    linux_link_objects,
    objdump_debug_disasm,
    aarch64_debug_print_codegen,
    NULL};

#endif
