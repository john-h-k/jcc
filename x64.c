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
        .gp_registers = {.num_volatile = 9,
                                              .num_nonvolatile = 7,
                                              .num_reserved = 0},
        .fp_registers =
            {
                .num_volatile = 16,
                .num_nonvolatile = 0,
                .num_reserved = 0},
    },
    X64_FUNCTION_ALIGNMENT, x64_macos_mangle, x64_lower, x64_codegen,
    x64_emit, write_macho, macos_link_objects, objdump_debug_disasm,
    x64_debug_print_codegen};

const struct target X64_LINUX_TARGET = {
    TARGET_ID_X64_LINUX,
    TARGET_LP_SZ_LP64,
    {
        .gp_registers = {.num_volatile = 9,
                                              .num_nonvolatile = 7,
                                              .num_reserved = 0},
        .fp_registers =
            {
                .num_volatile = 16,
                .num_nonvolatile = 0,
                .num_reserved = 0},
    },
    X64_FUNCTION_ALIGNMENT, x64_linux_mangle, x64_lower, x64_codegen,
    x64_emit, write_elf, linux_link_objects, objdump_debug_disasm,
    x64_debug_print_codegen};
