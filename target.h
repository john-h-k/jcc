#ifndef TARGET_H
#define TARGET_H

#include "alloc.h"

#include <stdio.h>

enum relocation_ty {
  RELOCATION_TY_POINTER,
  RELOCATION_TY_CALL,
  RELOCATION_TY_LOCAL_SINGLE,
  RELOCATION_TY_LOCAL_PAIR,
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

struct symbol {
  enum symbol_ty ty;

  enum symbol_visibility visibility;
  const char *name;
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
  const struct compile_args *compile_args;

  const char *output;

  const struct object_entry *entries;
  size_t num_entries;
};

enum target_lp_sz {
  TARGET_LP_SZ_LP32,
  TARGET_LP_SZ_LP64,
};

struct ir_unit;

// This follows a very specific ordering. Indices are used to represent
// registers For register N, if `N < num_volatile`, it represents a volatile reg
// For register N, if `N >= num_volatile && N < (num_volatile +
// num_nonvolatile)`, it is an involatile reg Else, it is a reserved reg
struct reg_set_info {
  size_t num_volatile;
  size_t num_nonvolatile;
  size_t num_reserved; // e.g x29 and x30 - can't be touched by regalloc
};

struct reg_info {
  struct reg_set_info gp_registers;
  struct reg_set_info fp_registers;
};

struct link_args {
  const char *const *objects;
  size_t num_objects;

  const char *output;
};

enum link_result { LINK_RESULT_SUCCESS, LINK_RESULT_FAILURE };

typedef const char *(*mangle)(struct arena_allocator *arena, const char *name);
typedef void (*target_lower)(struct ir_unit *unit);
typedef struct codegen_unit *(*codegen)(struct ir_unit *unit);
typedef struct emitted_unit (*emit_function)(const struct codegen_unit *unit);
typedef void (*build_object)(const struct build_object_args *args);
typedef enum link_result (*link_objects)(const struct link_args *args);
typedef void (*debug_disasm)(const char *filename, const char *output);
typedef void (*debug_print_codegen)(FILE *file, struct codegen_unit *unit);

enum target_id {
  TARGET_ID_X64_MACOS,
  TARGET_ID_X64_LINUX,
  TARGET_ID_AARCH64_MACOS,
  TARGET_ID_AARCH64_LINUX,
  TARGET_ID_RV32I_UNKNOWN,
};

struct target {
  enum target_id target_id;
  enum target_lp_sz lp_sz;
  struct reg_info reg_info;
  size_t function_alignment;
  mangle mangle;
  target_lower lower;
  codegen codegen;
  emit_function emit_function;
  build_object build_object;
  link_objects link_objects;
  debug_disasm debug_disasm;
  debug_print_codegen debug_print_codegen;
};

#endif
