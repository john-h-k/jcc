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

struct var_ref {
  struct var_key key;
  struct ir_op *op;
};

struct var_refs *var_refs_create();

struct var_ref *var_refs_get(struct var_refs *var_refs, const struct var_key *key);
struct var_ref *var_refs_add(struct var_refs *var_refs, const struct var_key *key);

#endif
