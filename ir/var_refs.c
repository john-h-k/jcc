#include "var_refs.h"

#include "../vector.h"

struct var_refs {
  struct vector *refs;
};

struct var_refs *var_refs_create() {
  struct var_refs refs = {.refs = vector_create(sizeof(struct var_ref))};

  struct var_refs *var_refs = nonnull_malloc(sizeof(*var_refs));
  *var_refs = refs;
  return var_refs;
}

struct var_ref *var_refs_add(struct var_refs *var_refs,
                             const struct var_key *key, enum var_ref_ty ty) {
  debug_assert(ty == VAR_REF_TY_GLB || key->basicblock,
               "must provide basicblock for non globals!");

  struct var_ref ref = {.key = *key, .ty = ty, .op = NULL};
  return (struct var_ref *)vector_push_back(var_refs->refs, &ref);
}

struct var_ref *var_refs_get(const struct var_refs *var_refs,
                             const struct var_key *key) {
  size_t num_refs = vector_length(var_refs->refs);
  for (size_t i = 0; i < num_refs; i++) {
    struct var_ref *ref = vector_get(var_refs->refs, i);

    if (ref->key.scope != key->scope || strcmp(ref->key.name, key->name) != 0) {
      continue;
    }

    // SSA variables can only be accessed within their basicblock
    // and will generate a phi if not found
    // however, if spilled to a local (or the var is global) they can be
    // accessed anywhere
    if (ref->ty == VAR_REF_TY_SSA && ref->key.basicblock == key->basicblock) {
      return ref;
    } else if (ref->ty != VAR_REF_TY_SSA || key->basicblock == NULL) {
      return ref;
    }
  }

  return NULL;
}
