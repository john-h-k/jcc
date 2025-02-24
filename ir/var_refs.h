#ifndef IR_VAR_REFS_H
#define IR_VAR_REFS_H

#include "ir.h"

// uniquely tracks vars by (name, scope)
struct var_refs;

// TODO: use hashing/interning instead for efficiency
struct var_key {
  const char *name;
  int scope;
  struct ir_basicblock *basicblock;
};

enum var_ref_ty { VAR_REF_TY_SSA, VAR_REF_TY_LCL, VAR_REF_TY_GLB };

struct var_ref {
  enum var_ref_ty ty;

  struct var_key key;

  union {
    struct ir_op *op;
    struct ir_lcl *lcl;
    struct ir_glb *glb;
    unsigned long long enum_cnst;
  };
};

struct var_refs *var_refs_create(void);

struct var_ref *var_refs_get(const struct var_refs *var_refs,
                             const struct var_key *key);
struct var_ref *var_refs_add(struct var_refs *var_refs,
                             const struct var_key *key, enum var_ref_ty ty);

void var_refs_free(struct var_refs **var_refs);

#endif
