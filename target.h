#ifndef TARGET_H
#define TARGET_H

#include "alloc.h"
#include "lsra.h"
#include "ir/build.h"

struct relocation {
  const char *symbol_name;

  size_t address;
  size_t size;
};

struct symbol {
  const char *name;
  size_t section;
  size_t value;
};

struct external_symbol {
  const char *name;
};

struct compiled_function {
  const char *name;
  void *code;
  size_t len_code;

  const char **strings;
  size_t num_strings;

  struct relocation *relocations;
  size_t num_relocations;
};

struct build_object_args {
  const struct compile_args *compile_args;

  const char *output;

  const char *data;
  size_t len_data;

  const char **strings;
  size_t num_strings;

  struct symbol *symbols;
  size_t num_symbols;

  // symbols not defined in this object
  struct external_symbol *extern_symbols;
  size_t num_extern_symbols;

  struct relocation *relocations;
  size_t num_relocations;
};

typedef const char *(*mangle)(struct arena_allocator *arena, const char *name);
typedef void (*lower)(struct ir_builder *func);
typedef struct compiled_function (*emit_function)(struct ir_builder *func);
typedef void (*build_object)(const struct build_object_args *args);
typedef void (*debug_disasm)(const char *filename);

struct target {
  struct reg_info reg_info;
  size_t function_alignment;
  size_t op_size;
  mangle mangle;
  lower pre_reg_lower;
  lower post_reg_lower;
  emit_function emit_function;
  build_object build_object;
  debug_disasm debug_disasm;
};

#endif
