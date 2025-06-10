#ifndef IR_VAR_REFS_H
#define IR_VAR_REFS_H

#include "../util.h"
#include "ir.h"

// uniquely tracks vars by (name, scope)
struct var_refs;

// TODO: use hashing/interning instead for efficiency
struct var_key {
  ustr_t name;
  int scope;
  struct ir_basicblock *basicblock;
};

enum var_ref_ty { VAR_REF_TY_SSA, VAR_REF_TY_LCL, VAR_REF_TY_GLB };

struct var_ref {
  enum var_ref_ty ty;

  struct var_key key;

  // spilled ops may have lcl but last op is needed for phi gen _before_ the
  // spill
  struct ir_op *op;

  union {
    struct ir_lcl *lcl;
    struct ir_glb *glb;
    unsigned long long enum_cnst;
  };
};

struct var_refs *var_refs_create(struct arena_allocator *arena);

void var_refs_push_scope(struct var_refs *var_refs);
void var_refs_pop_scope(struct var_refs *var_refs);

struct var_ref *var_refs_get(const struct var_refs *var_refs,
                             const struct var_key *key);
struct var_ref *var_refs_add(struct var_refs *var_refs,
                             const struct var_key *key, enum var_ref_ty ty);

void var_refs_free(struct var_refs **var_refs);

#endif
