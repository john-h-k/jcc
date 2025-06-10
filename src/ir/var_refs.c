#include "var_refs.h"

#include "../hashtbl.h"
#include "../vector.h"

struct var_ref_scope {
  struct hashtbl *vars;
};

struct var_refs {
  struct arena_allocator *arena;
  struct vector *scopes;
};

static void hash_var_key(struct hasher *hasher, const void *obj) {
  const struct var_key *key = obj;

  hashtbl_hash_ustr(hasher, &key->name);
  hasher_hash_integer(hasher, key->scope, sizeof(key->scope));
}

static bool eq_var_key(const void *l, const void *r) {
  const struct var_key *sl = l;
  const struct var_key *sr = r;

  if (sl->scope != sr->scope) {
    return false;
  }

  return ustr_eq(sl->name, sr->name);
}

struct var_refs *var_refs_create(struct arena_allocator *arena) {
  struct var_refs refs = {
      .arena = arena,
      .scopes = vector_create_in_arena(sizeof(struct var_ref_scope), arena)};

  var_refs_push_scope(&refs);

  struct var_refs *var_refs = aralloc(arena, sizeof(*var_refs));
  *var_refs = refs;
  return var_refs;
}

void var_refs_push_scope(struct var_refs *var_refs) {
  struct var_ref_scope var_ref_scope = {
      .vars = hashtbl_create_in_arena(var_refs->arena, sizeof(struct var_key),
                                      sizeof(struct var_ref), hash_var_key,
                                      eq_var_key)};

  vector_push_back(var_refs->scopes, &var_ref_scope);
}

void var_refs_pop_scope(struct var_refs *var_refs) {
  vector_pop(var_refs->scopes);
}

struct var_ref *var_refs_add(struct var_refs *var_refs,
                             const struct var_key *key, enum var_ref_ty ty) {
  DEBUG_ASSERT(ty == VAR_REF_TY_GLB || key->basicblock,
               "must provide basicblock for non globals!");

  struct var_ref ref = {.key = *key, .ty = ty, .op = NULL};
  struct var_ref_scope *scope = vector_tail(var_refs->scopes);

  // FIXME: `hashtbl_insert` should return data pointer
  hashtbl_insert(scope->vars, key, &ref);
  return hashtbl_lookup(scope->vars, key);
}

// TODO: needs general refactor, old code
struct var_ref *var_refs_get(const struct var_refs *var_refs,
                             const struct var_key *key) {
  size_t num_scopes = vector_length(var_refs->scopes);
  for (size_t i = num_scopes; i; i--) {
    struct var_ref_scope *scope = vector_get(var_refs->scopes, i - 1);

    struct var_ref *ref = hashtbl_lookup(scope->vars, key);

    if (!ref) {
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

void var_refs_free(struct var_refs **var_refs) { *var_refs = NULL; }
