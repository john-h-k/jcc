#ifndef TARGET_H
#define TARGET_H

#include "alloc.h"
#include "ir/ir.h"
#include "codegen.h"
#include "lsra.h"

enum relocation_ty {
  RELOCATION_TY_SINGLE,
  RELOCATION_TY_PAIR,
};

struct relocation {
  enum relocation_ty ty;

  size_t symbol_index;

  size_t address;
  size_t size;
};

enum symbol_ty {
  SYMBOL_TY_FUNC,
  SYMBOL_TY_STRING,
  SYMBOL_TY_CONST_DATA,
  SYMBOL_TY_DATA
};

enum symbol_visibility {
  SYMBOL_VISIBILITY_PRIVATE,
  SYMBOL_VISIBILITY_GLOBAL,
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
  OBJECT_ENTRY_TY_MUT_DATA
};

struct object_entry {
  enum object_entry_ty ty;

  struct symbol symbol;

  void *data;
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

  // symbols not defined in this object
  const struct external_symbol *extern_symbols;
  size_t num_extern_symbols;
};

typedef const char *(*mangle)(struct arena_allocator *arena, const char *name);
typedef void (*lower)(struct ir_unit *unit);
typedef struct codegen_unit *(*codegen)(struct ir_unit *unit);
typedef struct emitted_unit (*emit_function)(const struct codegen_unit *unit);
typedef void (*build_object)(const struct build_object_args *args);
typedef void (*debug_disasm)(const char *filename);
typedef void (*debug_print_codegen)(FILE *file, struct codegen_unit *unit);


struct target {
  struct reg_info reg_info;
  size_t function_alignment;
  size_t op_size;
  mangle mangle;
  lower lower;
  codegen codegen;
  emit_function emit_function;
  build_object build_object;
  debug_disasm debug_disasm;
  debug_print_codegen debug_print_codegen;
};

#endif
