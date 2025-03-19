#ifndef VAR_TABLE_H
#define VAR_TABLE_H

#include "alloc.h"
#include "hashtbl.h"

#define SCOPE_GLOBAL (0)
#define SCOPE_PARAMS (1)

// namespacing (e.g `struct foo` and `union foo` are allowed as distinct types)
enum var_table_ns {
  VAR_TABLE_NS_NONE, // used for vars

  VAR_TABLE_NS_STRUCT,
  VAR_TABLE_NS_UNION,
  VAR_TABLE_NS_ENUM,
  VAR_TABLE_NS_TYPEDEF,
};

struct var_table_entry {
  enum var_table_ns ns;
  struct sized_str name;
  int scope;

  struct td_var_ty *var_ty;

  union {
    struct td_var *var;
  };
};

struct var_table {
  struct vector *scopes;

  struct arena_allocator *arena;
};

struct var_table_scope {
  struct hashtbl *entries;

  size_t scope;
};

struct var_table var_table_create(struct arena_allocator *arena);
void var_table_free(struct var_table *var_table);

struct var_table_entry *
var_table_create_top_level_entry(struct var_table *var_table, enum var_table_ns ns, struct sized_str name);
struct var_table_entry *var_table_create_entry(struct var_table *var_table,
                                               enum var_table_ns ns, struct sized_str name);

int cur_scope(struct var_table *var_table);

void push_scope(struct var_table *var_table);
void pop_scope(struct var_table *var_table);

struct var_table_entry *var_table_get_entry(struct var_table *var_table,
                                            enum var_table_ns ns, struct sized_str name);

struct var_table_entry *
var_table_get_or_create_entry(struct var_table *var_table, enum var_table_ns ns, struct sized_str name);

typedef void (*debug_print_entries_callback)(FILE *file,
                                             struct var_table_entry *entry,
                                             void *metadata);
void debug_print_entries(FILE *file, struct var_table *var_table,
                         debug_print_entries_callback cb, void *metadata);

#endif
