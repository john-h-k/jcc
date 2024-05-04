#ifndef TARGET_H
#define TARGET_H

#include "emit.h"
#include "lsra.h"
#include "ir/build.h"

struct symbol {
  const char *name;
  size_t section;
  size_t value;
};

struct build_object_args {
  const struct compile_args *compile_args;

  const char *output;

  const char *data;
  size_t len_data;

  struct symbol *symbols;
  size_t num_symbols;
};

typedef void (*lower)(struct ir_builder *func);
typedef struct compiled_function (*emit_function)(struct ir_builder *func);
typedef void (*build_object)(const struct build_object_args *args);
typedef void (*debug_disasm)(const char *filename);

struct target {
  struct reg_info reg_info;
  size_t function_alignment;
  lower lower;
  emit_function emit_function;
  build_object build_object;
  debug_disasm debug_disasm;
};

#endif
