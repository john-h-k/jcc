#ifndef CODEGEN_H
#define CODEGEN_H

#include "ir/ir.h"
#include <stddef.h>

enum codegen_unit_ty {
  CODEGEN_UNIT_TY_AARCH64,
  CODEGEN_UNIT_TY_EEP,
};

struct codegen_unit {
  enum codegen_unit_ty ty;

  struct arena_allocator *arena;

  struct codegen_var **vars;
  size_t num_vars;

  struct codegen_function **funcs;
  size_t num_funcs;

  // the size of the element within the union, so it can be allocated
  size_t instr_size;
};

struct codegen_var {
  const char *name;
};

struct codegen_function {
  struct codegen_unit *unit;

  const char *name;

  size_t instr_count;

  struct instr* first;
  struct instr* last;
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
