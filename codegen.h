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

  struct arena_allocator *arena;

  size_t instr_count;

  // the size of the element within the union, so it can be allocated
  size_t instr_size;

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

  union {
    void *p;
    struct aarch64_instr *aarch64;
    struct eep_instr *eep;
  };
};

void debug_print_func(FILE *file, const struct codegen_function *func);
struct instr *alloc_instr(struct codegen_function *func);

#endif
