#ifndef CODEGEN_H
#define CODEGEN_H

#include "ir/ir.h"

#include <stddef.h>

enum codegen_unit_ty {
  CODEGEN_UNIT_TY_AARCH64,
  CODEGEN_UNIT_TY_EEP,
};

struct codegen_var {
  int tmp;
};

struct codegen_function {
  struct codegen_unit *unit;

  bool prologue;
  size_t stack_size;

  size_t instr_count;

  struct instr *first;
  struct instr *last;
};

enum codegen_entry_ty {
  CODEGEN_ENTRY_TY_FUNC,
  CODEGEN_ENTRY_TY_STRING,
  CODEGEN_ENTRY_TY_CONST_DATA,
  CODEGEN_ENTRY_TY_DATA,
  CODEGEN_ENTRY_TY_DECL,
};

struct codegen_data {
  void *data;
  size_t len_data;
};

struct codegen_entry {
  enum codegen_entry_ty ty;

  size_t glb_id;
  const char *name;

  union {
    struct codegen_function func;
    struct codegen_data data;
    const char *str;
  };
};

struct codegen_unit {
  enum codegen_unit_ty ty;

  struct arena_allocator *arena;

  struct codegen_entry *entries;
  size_t num_entries;

  // the size of the element within the union, so it can be allocated
  size_t instr_size;
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

struct move {
  size_t from;
  size_t to;
};

struct move_set {
  struct move *moves;
  size_t num_moves;
};


struct move_set gen_move_order(struct arena_allocator *arena,
                                      size_t *from, size_t *to, size_t num,
                                      size_t tmp_index);

#endif
