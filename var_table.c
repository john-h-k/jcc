#include "var_table.h"
#include "vector.h"

struct var_table var_table_create(struct parser *parser) {
  // known memory leak here, no `free` function atm
  struct var_table var_table = {
      .entries = vector_create(sizeof(struct var_table_entry)),
      .parser = parser};

  return var_table;
}

struct var_table_entry *create_entry(struct var_table *var_table,
                                     const struct ast_var *var) {
  const char *name = identifier_str(var_table->parser, &var->identifier);
  struct var_table_entry entry = {.name = name, .scope = var->scope};

  return vector_push_back(var_table->entries, &entry);
}

struct var_table_entry *get_or_create_entry(struct var_table *var_table,
                                            const struct ast_var *var) {
  // super inefficient, TODO: make efficient
  // does linear scan for entry at current scope, if that fails, tries at
  // higher scope, until scope is global then creates new entry

  const char *name = identifier_str(var_table->parser, &var->identifier);
  size_t num_vars = vector_length(var_table->entries);

  for (int scope = var->scope; scope >= SCOPE_GLOBAL; scope--) {
    trace("trying to find var '%s' at scope '%d' (var has scope '%d')", name,
          scope, var->scope);

    for (size_t i = 0; i < num_vars; i++) {
      struct var_table_entry *entry = vector_get(var_table->entries, i);

      if (entry->scope == scope && strcmp(entry->name, name) == 0) {
        trace("found var at scope '%d'", scope);
        return entry;
      }
    }

    if (scope != SCOPE_GLOBAL) {
      trace("failed! trying at next scope up");
    }
  }

  trace("couldn't find variable, creating new entry '%s' with scope '%d'", name,
        var->scope);

  return create_entry(var_table, var);
}
