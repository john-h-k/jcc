#ifndef HASHTBL_H
#define HASHTBL_H

#include <stddef.h>
#include <stdbool.h>

struct hashtbl;

typedef size_t hash_t;

typedef hash_t (*hash_fn)(const void *obj);
typedef bool (*eq_fn)(const void *l, const void *r);

struct hashtbl *hashtbl_create(size_t key_size, size_t element_size,
                               hash_fn hash_fn, eq_fn eq_gn);

hash_t hashtbl_hash_str(const void *obj);
bool hashtbl_eq_str(const void *l, const void *r);

struct hashtbl *hashtbl_create_str_keyed(size_t element_size);

void hashtbl_insert(struct hashtbl *hashtbl, const void *key, const void *data);
void hashtbl_remove(struct hashtbl *hashtbl, const void *key);
void *hashtbl_lookup(struct hashtbl *hashtbl, const void *key);

#endif
