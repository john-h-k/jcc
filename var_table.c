#include "var_table.h"

#include "util.h"
#include "vector.h"

static struct var_table_scope
var_table_scope_create(struct var_table_scope *prev) {
  struct var_table_scope var_table_scope = {
      .next = NULL,
      .prev = prev,
      .entries = vector_create(sizeof(struct var_table_entry)),
      .scope = prev ? prev->scope + 1 : SCOPE_GLOBAL};

  return var_table_scope;
}

struct var_table var_table_create(struct arena_allocator *arena) {
  // known memory leak here, no `free` function atm
  struct var_table var_table = {
      .arena = arena,
      .first = arena_alloc(arena, sizeof(*var_table.first)),
      .last = NULL};

  *var_table.first = var_table_scope_create(NULL);
  var_table.last = var_table.first;

  return var_table;
}

struct var_table_entry *var_table_create_entry(struct var_table *var_table,
                                               const char *name) {
  struct var_table_scope *last = var_table->last;

  struct var_table_entry entry = {
      .name = name, .scope = last->scope, .var = NULL, .var_ty = NULL};

  struct var_table_entry *p = vector_push_back(last->entries, &entry);

  return p;
}

// to show all entries
// size_t num_entries = vector_length(var_table->entries);
// for (size_t i = 0; i < num_entries; i++) {
//   struct var_table_entry *entry = vector_get(var_table->entries, i);
//   err("%s @ %d", entry->name, entry->scope);
// }

int cur_scope(struct var_table *var_table) { return var_table->last->scope; }

void push_scope(struct var_table *var_table) {
  struct var_table_scope *last = var_table->last;

  last->next = arena_alloc(var_table->arena, sizeof(*last->next));
  *last->next = var_table_scope_create(last);

  var_table->last = last->next;
}

void pop_scope(struct var_table *var_table) {
  struct var_table_scope *last = var_table->last;

  DEBUG_ASSERT(last->scope != SCOPE_GLOBAL,
               "popping global scope is never correct");

  vector_free(&last->entries);

  DEBUG_ASSERT(!last->next, "popping var_table_scope but it has a `next` "
                            "entry? should be impossible");

  var_table->last = last->prev;

  last->prev->next = NULL;
  last->prev = NULL;
}

struct var_table_entry *
var_table_get_or_create_entry(struct var_table *var_table, const char *name) {
  DEBUG_ASSERT(name, "name must be non-null");

  struct var_table_entry *entry = var_table_get_entry(var_table, name);

  if (entry) {
    return entry;
  }

  trace("couldn't find variable, creating new entry '%s' with scope '%d'", name,
        var_table->last->scope);

  return var_table_create_entry(var_table, name);
}

struct var_table_entry *var_table_get_entry(struct var_table *var_table,
                                            const char *name) {
  // super inefficient, TODO: make efficient
  // does linear scan for entry at current scope, if that fails, tries at
  // higher scope, until scope is global then creates new entry

  struct var_table_scope *scope = var_table->last;
  while (scope) {
    size_t num_vars = vector_length(scope->entries);

    for (size_t i = 0; i < num_vars; i++) {
      struct var_table_entry *entry = vector_get(scope->entries, i);

      if (strcmp(entry->name, name) == 0) {
        trace("found '%s' at scope %d", entry->name, scope->scope);
        return entry;
      }
    }

    scope = scope->prev;
  }

  trace("did not find entry for %s", name);
  return NULL;
}

void debug_print_entries(FILE *file, struct var_table *var_table,
                         debug_print_entries_callback cb, void *metadata) {
  fprintf(file, "var_table entries:\n");

  struct var_table_scope *scope = var_table->last;
  while (scope) {
    size_t num_vars = vector_length(scope->entries);

    for (size_t i = 0; i < num_vars; i++) {
      struct var_table_entry *entry = vector_get(scope->entries, i);

      if (cb) {
        cb(file, entry, metadata);
      } else {
        fprintf(file, "entry %s\n @ %d\n", entry->name, entry->scope);
      }
    }

    scope = scope->prev;
  }
}
