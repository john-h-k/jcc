#ifndef CODEGEN_H
#define CODEGEN_H

#include "ir/ir.h"
#include "target.h"


struct aarch64_instr;
struct eep_instr;
struct rv32i_instr;
struct x64_instr;

struct instr {
  size_t id;

  struct ir_op *op;

  struct instr *pred;
  struct instr *succ;

  struct cg_basicblock *basicblock;

  struct relocation *reloc;

  union {
    void *p;
    struct aarch64_instr *aarch64;
    struct eep_instr *eep;
    struct rv32i_instr *rv32i;
    struct x64_instr *x64;
  };
};

struct cg_basicblock {
  size_t id;

  struct cg_func *func;

  struct ir_basicblock *ir_basicblock;

  struct instr *first;
  struct instr *last;

  struct cg_basicblock *pred;
  struct cg_basicblock *succ;
};

struct cg_func {
  struct cg_unit *unit;

  bool prologue;
  size_t stack_size;

  size_t basicblock_count;
  size_t instr_count;

  struct cg_basicblock *first;
  struct cg_basicblock *last;
};

enum cg_entry_ty {
  CG_ENTRY_TY_FUNC,
  CG_ENTRY_TY_STRING,
  CG_ENTRY_TY_CONST_DATA,
  CG_ENTRY_TY_DATA,
  CG_ENTRY_TY_DECL,
};

struct cg_data {
  void *data;
  size_t len_data;

  struct relocation *relocs;
  size_t num_relocs;
};

struct cg_entry {
  enum cg_entry_ty ty;

  size_t glb_id;
  const char *name;

  size_t alignment;

  struct symbol symbol;

  union {
    struct cg_func func;
    struct cg_data data;
  };
};

struct cg_unit {
  enum cg_unit_ty ty;

  struct arena_allocator *arena;
  const struct target *target;

  struct cg_entry *entries;
  size_t num_entries;

  // the size of the element within the union, so it can be allocated
  size_t instr_size;
};

// returns the next instr that can be jumped to (ie skips empty blocks)
struct instr *cg_get_next_instr(struct cg_basicblock *target);

struct cg_basicblock *cg_alloc_basicblock(struct cg_func *func, struct ir_basicblock *ir_basicblock);
struct instr *cg_alloc_instr(struct cg_func *func, struct cg_basicblock *basicblock);

void cg_detach_basicblock(struct cg_func *func, struct cg_basicblock *basicblock);

const char *cg_mangle_str_cnst_name(struct arena_allocator *arena,
                                 const char *func_name, size_t id);

int cg_sort_entries_by_id(const void *a, const void *b);

void cg_rebuild_ids(struct cg_func *func);

struct cg_unit *codegen(struct ir_unit *unit, enum codegen_flags flags);

void codegen_free(struct cg_unit **unit);

#endif
