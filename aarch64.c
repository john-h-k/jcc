#include "aarch64.h"

#include "aarch64/lower.h"
#include "aarch64/codegen.h"

const struct target AARCH64_TARGET = {
    // x0..x30 excluding x18
    (struct reg_info){
        .integral_registers = (struct reg_set_info){
            .num_volatile = 18, .num_nonvolatile = 10, .num_reserved = 2},
        .fp_registers = (struct reg_set_info){
            // FIXME: technically v8-15 are volatile top half
            // but we don't suppport vectors yet
            .num_volatile = 24, .num_nonvolatile = 8, .num_reserved = 0},
    },
    AARCH64_FUNCTION_ALIGNMENT,
    AARCH64_OP_SIZE,
    aarch64_mangle,
    aarch64_lower,
    aarch64_codegen,
    aarch64_emit_function,
    write_macho,
    objdump_debug_disasm,
    aarch64_debug_print_custom_ir_op};
