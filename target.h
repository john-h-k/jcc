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

struct sym_relocation {
  const char *symbol_name;
};

struct str_relocation {
  // index into the strings array of `build_object_args`
  // we should really also do this for symbols instead of the slow lookup...
  size_t str_index;
};

struct relocation {
  enum relocation_ty ty;

  union {
    struct sym_relocation sym;
    struct str_relocation str;
  };

  size_t address;
  size_t size;
};

enum symbol_ty {
  SYMBOL_TY_STRING,
  SYMBOL_TY_FUNC,
};

enum symbol_visibility {
  SYMBOL_VISIBILITY_PRIVATE,
  SYMBOL_VISIBILITY_GLOBAL,
};

struct symbol {
  enum symbol_ty ty;

  enum symbol_visibility visibility;

  const char *name;
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

  // one relocation may be multiple instructions
  size_t num_relocation_instrs;
};

struct object_string {
  const char *id;
  const char *string;
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
typedef struct codegen_function *(*codegen)(struct ir_builder *func);
typedef struct compiled_function (*emit_function)(const struct codegen_function *func);
typedef void (*build_object)(const struct build_object_args *args);
typedef void (*debug_disasm)(const char *filename);
typedef void (*debug_print_codegen)(FILE *file, struct codegen_function *func);


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
