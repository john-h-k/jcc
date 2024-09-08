#ifndef IR_VAR_REFS_H
#define IR_VAR_REFS_H

#include "ir.h"

// uniquely tracks vars by (name, scope)
struct var_refs;

// TODO: use hashing/interning instead for efficiency
struct var_key {
  const char *name;
  int scope;
};

enum var_ref_ty {
  VAR_REF_TY_LCL,
  VAR_REF_TY_GLB
};

struct var_ref {
  enum var_ref_ty ty;

  struct var_key key;

  // potentially null - the func associated with this var ref
  struct ir_builder *func;

  union {
    struct ir_op *lcl;
    struct ir_string *glb;
  };
};

struct var_refs *var_refs_create(struct var_refs *parent);

struct var_ref *var_refs_get(struct var_refs *var_refs, const struct var_key *key);
struct var_ref *var_refs_add(struct var_refs *var_refs, const struct var_key *key);

#endif
