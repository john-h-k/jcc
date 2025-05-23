#include "x64.h"

#include "disasm.h"
#include "linux/elf.h"
#include "linux/link.h"
#include "macos/link.h"
#include "macos/mach-o.h"
#include "target.h"
#include "typechk.h"
#include "x64/codegen.h"
#include "x64/emit.h"
#include "x64/lower.h"

#if !defined(JCC_ALL) && !defined(JCC_X64)

const struct target X64_MACOS_TARGET = {.target_id = TARGET_ID_NOT_SUPPORTED};
const struct target X64_LINUX_TARGET = {.target_id = TARGET_ID_NOT_SUPPORTED};

#else

// struct __va_list_tag {
//    unsigned *gp_offset;
//    unsigned *fp_offset;
//    void *overflow_arg_area;
//    void *reg_save_area;
// };
//
// typedef __builtin_va_list __va_list_tag[1]

const static struct td_var_ty X64_VA_LIST_TAG_TY = {
    .ty = TD_VAR_TY_TY_AGGREGATE,
    .aggregate = {
        .ty = TD_TY_AGGREGATE_TY_STRUCT,
        .name = MK_NULL_USTR(),
        .num_fields = 4,
        .fields =
            (struct td_struct_field[4]){
                [0] =
                    {
                        .identifier = MK_USTR_LITERAL("gp_offset"),
                        .var_ty = {.ty = TD_VAR_TY_TY_WELL_KNOWN,
                                   .well_known = WELL_KNOWN_TY_UNSIGNED_INT},
                    },
                [1] =
                    {
                        .identifier = MK_USTR_LITERAL("fp_offset"),
                        .var_ty = {.ty = TD_VAR_TY_TY_WELL_KNOWN,
                                   .well_known = WELL_KNOWN_TY_UNSIGNED_INT},
                    },
                [2] =
                    {
                        .identifier = MK_USTR_LITERAL("overflow_arg_area"),
                        .var_ty = {.ty = TD_VAR_TY_TY_POINTER,
                                   .pointer = {.underlying = &TD_VAR_TY_VOID}},
                    },
                [3] =
                    {
                        .identifier = MK_USTR_LITERAL("reg_save_area"),
                        .var_ty = {.ty = TD_VAR_TY_TY_POINTER,
                                   .pointer = {.underlying = &TD_VAR_TY_VOID}},
                    },

            }}};

const static struct td_var_ty X64_VA_LIST_TY = {
    .ty = TD_VAR_TY_TY_ARRAY,
    .array = {.size = 1,
              // FIXME:
              .underlying =
                  CONST_CAST((struct td_var_ty *)&X64_VA_LIST_TAG_TY)}};

const struct target X64_MACOS_TARGET = {
    TARGET_ID_X64_MACOS,
    TARGET_LP_SZ_LP64,
    {.va_list_var_ty = &X64_VA_LIST_TY,
     .flags = TARGET_VARIADIC_INFO_FLAG_VA_LIST_BYREF},
    {
        .ssp = 7,
        .gp_registers = {.max_reg_size = 8,
                         .num_volatile = 9,
                         .num_nonvolatile = 5,
                         .num_reserved = 2},
        .fp_registers = {.max_reg_size = 8, // TODO: vectors
                         .num_volatile = 16,
                         .num_nonvolatile = 0,
                         .num_reserved = 0},
    },
    X64_FUNCTION_ALIGNMENT,
    x64_macos_mangle,
    {
        x64_lower_variadic,
        x64_lower,
        x64_lower_func_ty,
    },
    {.ty = CODEGEN_UNIT_TY_X64,
     .instr_sz = sizeof(struct x64_instr),
     .function_align = X64_FUNCTION_ALIGNMENT,
     .codegen_start = x64_codegen_start,
     .codegen_basicblock = x64_codegen_basicblock,
     .codegen_end = x64_codegen_end,
     .debug_codegen_entry = x64_debug_print_codegen_entry},
    x64_emit,
    write_macho,
    macos_link_objects,
    objdump_debug_disasm,
    NULL};

const struct target X64_LINUX_TARGET = {
    TARGET_ID_X64_LINUX,
    TARGET_LP_SZ_LP64,
    {.va_list_var_ty = &X64_VA_LIST_TY,
     .flags = TARGET_VARIADIC_INFO_FLAG_VA_LIST_BYREF},
    {
        .ssp = 7,
        .gp_registers = {.max_reg_size = 8,
                         .num_volatile = 9,
                         .num_nonvolatile = 5,
                         .num_reserved = 2},
        .fp_registers = {.max_reg_size = 8, // TODO: vectors
                         .num_volatile = 16,
                         .num_nonvolatile = 0,
                         .num_reserved = 0},
    },
    X64_FUNCTION_ALIGNMENT,
    x64_linux_mangle,
    {
        x64_lower_variadic,
        x64_lower,
        x64_lower_func_ty,
    },
    {.ty = CODEGEN_UNIT_TY_X64,
     .instr_sz = sizeof(struct x64_instr),
     .function_align = X64_FUNCTION_ALIGNMENT,
     .codegen_start = x64_codegen_start,
     .codegen_basicblock = x64_codegen_basicblock,
     .codegen_end = x64_codegen_end,
     .debug_codegen_entry = x64_debug_print_codegen_entry},
    x64_emit,
    write_elf,
    linux_link_objects,
    objdump_debug_disasm,
    NULL};

#endif
