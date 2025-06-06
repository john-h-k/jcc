#ifndef TARGET_H
#define TARGET_H

#include "alloc.h"
#include "compiler.h"

#include <stdio.h>

enum relocation_ty {
  // raw pointer, e.g a field, or an x64 movq
  RELOCATION_TY_POINTER,

  // a call
  RELOCATION_TY_CALL,

  // a relocation to a defined symbol which applies to one instruction
  RELOCATION_TY_LOCAL_SINGLE,

  // a relocation to a defined symbol which applies to two instructions
  RELOCATION_TY_LOCAL_PAIR,

  // a relocation to an undefined symbol which applies to one instruction
  RELOCATION_TY_UNDEF_SINGLE,

  // a relocation to an undefined symbol which applies to two instructions
  RELOCATION_TY_UNDEF_PAIR,
};

struct relocation {
  enum relocation_ty ty;

  size_t symbol_index;

  size_t address;
  size_t offset;
  size_t size;
};

enum symbol_ty {
  SYMBOL_TY_DECL,
  SYMBOL_TY_FUNC,
  SYMBOL_TY_STRING,
  SYMBOL_TY_CONST_DATA,
  SYMBOL_TY_DATA,
};

enum symbol_visibility {
  SYMBOL_VISIBILITY_PRIVATE,
  SYMBOL_VISIBILITY_GLOBAL,
  SYMBOL_VISIBILITY_UNDEF,
};

enum FLAG_ENUM symbol_flags {
  SYMBOL_FLAG_NONE = 0,
  SYMBOL_FLAG_WEAK = 1 << 0,
};

struct symbol {
  enum symbol_ty ty;

  enum symbol_visibility visibility;
  const char *name;

  enum symbol_flags flags;
};

struct external_symbol {
  const char *name;
};

struct emitted_unit {
  size_t num_entries;
  struct object_entry *entries;
};

enum object_entry_ty {
  OBJECT_ENTRY_TY_FUNC,
  OBJECT_ENTRY_TY_C_STRING,
  OBJECT_ENTRY_TY_CONST_DATA,
  OBJECT_ENTRY_TY_MUT_DATA,
  OBJECT_ENTRY_TY_DECL // undefined symbols
};

struct object_entry {
  enum object_entry_ty ty;

  struct symbol symbol;

  const void *data;
  size_t len_data;

  size_t alignment;

  const struct relocation *relocations;
  size_t num_relocations;
};

struct build_object_args {
  FILE *output;

  enum compile_target target;
  const struct object_entry *entries;
  size_t num_entries;
};

enum target_lp_sz {
  TARGET_LP_SZ_LP32,
  TARGET_LP_SZ_LP64,
};

struct ir_unit;
struct ir_func;
struct ir_op;
struct ir_var_func_ty;

// This follows a very specific ordering. Indices are used to represent
// registers For register N, if `N < num_volatile`, it represents a volatile reg
// For register N, if `N >= num_volatile && N < (num_volatile +
// num_nonvolatile)`, it is an involatile reg Else, it is a reserved reg
struct reg_set_info {
  // the maximum size of a register in this set (e.g 64 for a x64 GP reg but 256
  // for an FP reg if AVX is enabled)
  size_t max_reg_size;

  size_t num_volatile;
  size_t num_nonvolatile;
  size_t num_reserved; // e.g x29 and x30 - can't be touched by regalloc
};

struct reg_info {
  size_t ssp; // a GP register that can be safely used as a scratch register
              // around calls (so not an arg register)
  struct reg_set_info gp_registers;
  struct reg_set_info fp_registers;
};

struct link_args {
  struct fs *fs;
  const struct compile_args *args;

  char **linker_args;
  size_t num_linker_args;

  const char *const *objects;
  size_t num_objects;

  const char *output;
};

enum link_result { LINK_RESULT_SUCCESS, LINK_RESULT_FAILURE };

typedef const char *(*mangle)(struct arena_allocator *arena, const char *name);

struct cg_unit;

typedef struct emitted_unit (*emit_function)(const struct cg_unit *unit);
typedef void (*build_object)(const struct build_object_args *args);
typedef enum link_result (*link_objects)(const struct link_args *args);
typedef void (*debug_disasm)(enum compile_target target, const char *filename,
                             const char *output);

enum target_id {
  TARGET_ID_NOT_SUPPORTED,
  TARGET_ID_X64_MACOS,
  TARGET_ID_X64_LINUX,
  TARGET_ID_AARCH64_MACOS,
  TARGET_ID_AARCH64_LINUX,
  TARGET_ID_RV32I_LINUX,
};

enum cg_unit_ty {
  CODEGEN_UNIT_TY_AARCH64,
  CODEGEN_UNIT_TY_X64,
  CODEGEN_UNIT_TY_EEP,
  CODEGEN_UNIT_TY_RV32I,
};

struct cg_state {
  struct arena_allocator *arena;

  enum codegen_flags flags;

  const struct target *target;
  struct cg_entry *entry;
  struct cg_func *func;
  struct ir_func *ir;

  union {
    struct aarch64_prologue_info *aarch64_prologue_info;
    struct x64_prologue_info *x64_prologue_info;
    struct rv32i_prologue_info *rv32i_prologue_info;
  };
};

struct ir_glb;
struct ir_basicblock;

typedef void (*target_codegen_basicblock)(struct cg_state *state,
                                          struct ir_basicblock *basicblock);
typedef void (*target_codegen)(struct cg_state *state);

typedef void (*emit_asm)(FILE *file, struct cg_unit *unit,
                         enum codegen_flags flags);

typedef struct cg_entry (*target_codegen_var)(struct cg_unit *unit,
                                              struct ir_glb *glb);

typedef void (*debug_print_codegen_entry)(FILE *file,
                                          const struct cg_entry *entry);

struct codegen_info {
  enum cg_unit_ty ty;
  size_t instr_sz;
  size_t function_align;
  target_codegen codegen_start;
  target_codegen_basicblock codegen_basicblock;
  target_codegen codegen_end;
  debug_print_codegen_entry debug_codegen_entry;
};

typedef void (*target_lower_variadic)(struct ir_func *func);
typedef void (*target_lower)(struct ir_unit *unit);
typedef struct ir_func_info (*target_lower_func_ty)(
    struct ir_func *func, struct ir_var_func_ty func_ty, struct ir_op **args,
    size_t num_args);

struct lower_info {
  // variadics must be lowered early
  target_lower_variadic lower_variadic;
  target_lower lower;
  target_lower_func_ty lower_func_ty;
};

enum FLAG_ENUM target_variadic_info_flags {
  TARGET_VARIADIC_INFO_FLAG_NONE = 0,

  // e.g if `va_list` is an array, then this is true
  // this indicates that `va_copy` must load its rhs
  TARGET_VARIADIC_INFO_FLAG_VA_LIST_BYREF = 1 << 0,
};

struct target_variadic_info {
  const struct td_var_ty *va_list_var_ty;
  enum target_variadic_info_flags flags;
};

struct target {
  enum target_id target_id;
  enum target_lp_sz lp_sz;
  struct target_variadic_info variadic_info;
  struct reg_info reg_info;
  size_t function_alignment;
  mangle mangle;
  struct lower_info lower;
  struct codegen_info codegen;
  emit_function emit_function;
  build_object build_object;
  link_objects link_objects;
  debug_disasm debug_disasm;
  emit_asm emit_asm;
};

#endif
