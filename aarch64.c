#include "aarch64.h"

#include "aarch64/emit.h"
#include "disasm.h"
#include "macos/link.h"
#include "macos/mach-o.h"
#include "linux/link.h"
#include "linux/elf.h"

#include "aarch64/codegen.h"
#include "aarch64/lower.h"
#include "target.h"

const struct target AARCH64_MACOS_TARGET = {
    TARGET_ID_AARCH64_MACOS,
    TARGET_LP_SZ_LP64,
    // x0..x30 excluding x18
    {
        .gp_registers = {.num_volatile = 18,
                                              .num_nonvolatile = 10,
                                              .num_reserved = 2},
        .fp_registers =
            {
                // FIXME: technically v8-15 are volatile top half
                // but we don't suppport vectors yet
                .num_volatile = 24,
                .num_nonvolatile = 8,
                .num_reserved = 0},
    },
    AARCH64_FUNCTION_ALIGNMENT, aarch64_macos_mangle, aarch64_lower, aarch64_codegen,
    aarch64_emit, write_macho, macos_link_objects, objdump_debug_disasm,
    aarch64_debug_print_codegen};

const struct target AARCH64_LINUX_TARGET = {
    TARGET_ID_AARCH64_LINUX,
    TARGET_LP_SZ_LP64,
    // x0..x30 excluding x18
    {
        .gp_registers = {.num_volatile = 18,
                                              .num_nonvolatile = 10,
                                              .num_reserved = 2},
        .fp_registers =
            {
                // FIXME: technically v8-15 are volatile top half
                // but we don't suppport vectors yet
                .num_volatile = 24,
                .num_nonvolatile = 8,
                .num_reserved = 0},
    },
    AARCH64_FUNCTION_ALIGNMENT, aarch64_linux_mangle, aarch64_lower, aarch64_codegen,
    aarch64_emit, write_elf, linux_link_objects, objdump_debug_disasm,
    aarch64_debug_print_codegen};
