#include "var_table.h"

#include "alloc.h"
#include "hashtbl.h"
#include "log.h"
#include "util.h"
#include "vector.h"

struct var_key {
  ustr_t identifier;
  enum var_table_ns ns;
};

static void hash_var_key(struct hasher *hasher, const void *value) {
  const struct var_key *s = value;

  // TODO: having parser/typechk used sized str would improve efficiency
  hashtbl_hash_ustr(hasher, &s->identifier);
  hasher_hash_integer(hasher, s->ns, sizeof(s->ns));
}

static bool eq_var_key(const void *l, const void *r) {
  const struct var_key *sl = l;
  const struct var_key *sr = r;

  if (sl->ns != sr->ns) {
    return false;
  }

  return ustr_eq(sl->identifier, sr->identifier);
}

static struct var_table_scope *var_table_scope_create(struct var_table *table,
                                                      int scope) {
  struct var_table_scope var_table_scope = {
      .entries = hashtbl_create_in_arena(table->arena, sizeof(struct var_key),
                                         sizeof(struct var_table_entry),
                                         hash_var_key, eq_var_key),
      .scope = scope};

  return vector_push_back(table->scopes, &var_table_scope);
}

struct var_table vt_create(struct arena_allocator *arena) {
  struct var_table var_table = {
      .arena = arena,
      .scopes = vector_create_in_arena(sizeof(struct var_table_scope), arena)};

  var_table_scope_create(&var_table, SCOPE_GLOBAL);

  return var_table;
}

static struct var_table_entry *add_entry(struct var_table_scope *scope,
                                         enum var_table_ns ns,
                                         ustr_t name) {
  struct var_key key = {.identifier = name, .ns = ns};

  struct var_table_entry *entry =
      hashtbl_lookup_or_insert(scope->entries, &key, NULL);
  *entry =
      (struct var_table_entry){.name = name, .ns = ns, .scope = scope->scope};

  return entry;
}

struct var_table_entry *vt_create_entry_at_scope(struct var_table *var_table,
                                                 enum var_table_ns ns,
                                                 ustr_t name, size_t scope) {
  struct var_table_scope *tbl = vector_get(var_table->scopes, scope);

  return add_entry(tbl, ns, name);
}

struct var_table_entry *vt_create_entry(struct var_table *var_table,
                                        enum var_table_ns ns,
                                        ustr_t name) {
  struct var_table_scope *last = vector_tail(var_table->scopes);

  return add_entry(last, ns, name);
}

void vt_free(UNUSED struct var_table *var_table) {
  // nop all arena alloc'd
}

int vt_cur_scope(struct var_table *var_table) {
  struct var_table_scope *last = vector_tail(var_table->scopes);
  DEBUG_ASSERT(last->scope + 1 == vector_length(var_table->scopes),
               "scope + 1 (%zu) should eq len (%zu)", last->scope + 1,
               vector_length(var_table->scopes));
  return last->scope;
}

void vt_push_scope(struct var_table *var_table) {
  var_table_scope_create(var_table, vt_cur_scope(var_table) + 1);
}

void vt_pop_scope(struct var_table *var_table) {

  DEBUG_ASSERT(vt_cur_scope(var_table) != SCOPE_GLOBAL,
               "popping global scope is never correct");

  vector_pop(var_table->scopes);
}

struct var_table_entry *vt_get_or_create_entry(struct var_table *var_table,
                                               enum var_table_ns ns,
                                               ustr_t name) {
  DEBUG_ASSERT(name.str, "name must be non-null");

  struct var_table_entry *entry = vt_get_entry(var_table, ns, name);

  if (entry) {
    return entry;
  }

  trace("couldn't find variable, creating new entry '%s' with scope '%d'",
        name.str, vt_cur_scope(var_table));

  return vt_create_entry(var_table, ns, name);
}

struct var_table_entry *vt_get_entry(struct var_table *var_table,
                                     enum var_table_ns ns,
                                     ustr_t name) {
  // super inefficient, TODO: make efficient
  // does linear scan for entry at current scope, if that fails, tries at
  // higher scope, until scope is global then creates new entry

  for (ssize_t scope_idx = vt_cur_scope(var_table); scope_idx >= SCOPE_GLOBAL;
       scope_idx--) {
    struct var_table_scope *scope = vector_get(var_table->scopes, scope_idx);

    struct var_key key = {.ns = ns, .identifier = name};
    struct var_table_entry *entry = hashtbl_lookup(scope->entries, &key);

    if (entry) {
      DEBUG_ASSERT(ustr_eq(entry->name, name), "names should have been equal");
      return entry;
    }
  }

  trace("did not find entry for %s", name.str);
  return NULL;
}
