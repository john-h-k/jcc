#include "aarch64.h"

#include "aarch64/codegen.h"
#include "aarch64/lower.h"
#include "target.h"

const struct target AARCH64_TARGET = {
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
    AARCH64_FUNCTION_ALIGNMENT, aarch64_mangle, aarch64_lower, aarch64_codegen,
    aarch64_emit, write_macho, objdump_debug_disasm,
    aarch64_debug_print_codegen};
