#ifndef VAR_TABLE_H
#define VAR_TABLE_H

#include "parse.h"

struct var_table_entry {
  const char *name;
  const int scope;

  void *value;
};

struct var_table {
  // vector of `var_table_entry`
  // change to hash eventually?
  struct vector *entries;

  // needed for accessing AST text
  struct parser *parser;
};

struct var_table var_table_create(struct parser *parser);
struct var_table_entry *create_entry(struct var_table *var_table,
                                     const struct ast_var *var);

struct var_table_entry *get_entry(struct var_table *var_table,
                                  const struct ast_var *var);

struct var_table_entry *get_entry_up_scopes(struct var_table *var_table,
                                  const struct ast_var *var);

struct var_table_entry *get_or_create_entry(struct var_table *var_table,
                                            const struct ast_var *var);

struct var_table_entry *get_or_create_entry_up_scopes(struct var_table *var_table,
                                            const struct ast_var *var);

#endif
