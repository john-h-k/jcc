#include "var_refs.h"

#include "../vector.h"

struct var_refs {
  struct vector *refs;
};

// static void hash_var_key(struct hasher *hasher, const struct var_key *key) {
//   hasher_hash_str(hasher, key->name);
//   hasher_hash_pointer(hasher, key->basicblock);
//   hasher_hash_integer(hasher, key->scope, sizeof(key->scope));
// }

struct var_refs *var_refs_create(void) {
  struct var_refs refs = {.refs = vector_create(sizeof(struct var_ref))};

  struct var_refs *var_refs = nonnull_malloc(sizeof(*var_refs));
  *var_refs = refs;
  return var_refs;
}

struct var_ref *var_refs_add(struct var_refs *var_refs,
                             const struct var_key *key, enum var_ref_ty ty) {
  DEBUG_ASSERT(ty == VAR_REF_TY_GLB || key->basicblock,
               "must provide basicblock for non globals!");

  // might be same name and scope as a now-out-of-scope var
  struct var_ref *prev = var_refs_get(var_refs, key);
  if (prev) {
    *prev = (struct var_ref){.key = *key, .ty = ty, .op = NULL};
    return prev;
  }

  struct var_ref ref = {.key = *key, .ty = ty, .op = NULL};
  return (struct var_ref *)vector_push_back(var_refs->refs, &ref);
}

// TODO: needs general refactor, old code
static struct var_ref *var_refs_get_impl(const struct var_refs *var_refs,
                             const struct var_key *key, bool bb_specific) {
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
    } else if ((!bb_specific && ref->ty != VAR_REF_TY_SSA) || key->basicblock == NULL) {
      return ref;
    }
  }

  return NULL;
}

struct var_ref *var_refs_get_for_basicblock(const struct var_refs *var_refs,
                             const struct var_key *key) {
  return var_refs_get_impl(var_refs, key, true);
}

struct var_ref *var_refs_get(const struct var_refs *var_refs,
                             const struct var_key *key) {
  return var_refs_get_impl(var_refs, key, false);
}

void var_refs_free(struct var_refs **var_refs) {
  vector_free(&(*var_refs)->refs);

  free(*var_refs);
  *var_refs = NULL;
}
