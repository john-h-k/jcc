#ifndef VAR_TABLE_H
#define VAR_TABLE_H

#include "alloc.h"
#include "parse.h"

#define SCOPE_GLOBAL (-1)

struct var_table_entry {
  const char *name;

  void *value;
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
struct var_table_entry *create_entry(struct var_table *var_table, const char *name);

int cur_scope(struct var_table *var_table);

void push_scope(struct var_table *var_table);
void pop_scope(struct var_table *var_table);

struct var_table_entry *get_entry(struct var_table *var_table, const char *name);

struct var_table_entry *get_or_create_entry(struct var_table *var_table, const char *name);

#endif
