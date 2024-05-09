#include "var_refs.h"

#include "../vector.h"

struct var_refs {
  struct var_refs *parent;
  struct vector *refs;
};

struct var_refs *var_refs_create(struct var_refs *parent) {  
  struct var_refs refs = {
    .parent = parent,
    .refs = vector_create(sizeof(struct var_ref))
  };

  struct var_refs *var_refs = nonnull_malloc(sizeof(*var_refs));
  *var_refs = refs;
  return var_refs;
}

struct var_ref *var_refs_add(struct var_refs *var_refs, const struct var_key *key) {
  struct var_ref ref = { .key = *key, .op = NULL, .func = NULL };
  return (struct var_ref *)vector_push_back(var_refs->refs, &ref);
}

struct var_ref *var_refs_get(struct var_refs *var_refs, const struct var_key *key) {
  size_t num_refs = vector_length(var_refs->refs);
  for (size_t i = 0; i < num_refs; i++) {
    struct var_ref *ref = vector_get(var_refs->refs, i);

    if (ref->key.scope == key->scope && strcmp(ref->key.name, key->name) == 0) {
      return ref;
    }
  }

  return var_refs->parent ? var_refs_get(var_refs->parent, key) : NULL;
}

