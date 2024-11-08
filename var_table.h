#ifndef VAR_TABLE_H
#define VAR_TABLE_H

#include "alloc.h"
#include "parse.h"

#define SCOPE_GLOBAL (-1)
#define SCOPE_PARAMS (0)

struct var_table_entry {
  const char *name;
  int scope;

  struct td_var_ty *var_ty;

  union {
    struct td_var *var;
  };
};

struct var_table {
  struct var_table_scope *first;
  struct var_table_scope *last;

  struct arena_allocator *arena;
};

struct var_table_scope {
  struct var_table_scope *prev;
  struct var_table_scope *next;

  // vector of `var_table_entry`
  // change to hash eventually?
  struct vector *entries;

  int scope;
};

struct var_table var_table_create(struct arena_allocator *arena);
struct var_table_entry *var_table_create_entry(struct var_table *var_table,
                                     const char *name);

int cur_scope(struct var_table *var_table);

void push_scope(struct var_table *var_table);
void pop_scope(struct var_table *var_table);

struct var_table_entry *var_table_get_entry(struct var_table *var_table,
                                  const char *name);

struct var_table_entry *var_table_get_or_create_entry(struct var_table *var_table,
                                            const char *name);

typedef void (*debug_print_entries_callback)(FILE *file,
                                             struct var_table_entry *entry,
                                             void *metadata);
void debug_print_entries(FILE *file, struct var_table *var_table,
                         debug_print_entries_callback cb, void *metadata);

#endif
