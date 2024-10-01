#ifndef CODEGEN_H
#define CODEGEN_H

#include "ir/ir.h"
#include <stddef.h>

enum codegen_function_ty {
  CODEGEN_FUNCTION_TY_AARCH64,
  CODEGEN_FUNCTION_TY_EEP,
};

struct codegen_function {
  enum codegen_function_ty ty;

  const char *name;

  struct arena_allocator *arena;

  size_t instr_count;

  // the size of the element within the union, so it can be allocated
  size_t instr_size;

  size_t num_strings;
  const char **strings;

  size_t num_datas;
  const char **datas;

  struct instr* first;
  struct instr* last;

  size_t max_variadic_args;
};

struct aarch64_instr;
struct eep_instr;

struct instr {
  size_t id;

  struct instr *pred;
  struct instr *succ;

  struct relocation *reloc;

  union {
    void *p;
    struct aarch64_instr *aarch64;
    struct eep_instr *eep;
  };
};

struct instr *alloc_instr(struct codegen_function *func);

#endif
