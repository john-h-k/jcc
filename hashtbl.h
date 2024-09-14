#ifndef HASHTBL_H
#define HASHTBL_H

#include <stddef.h>

struct hashtbl;

typedef size_t hash_t;

typedef hash_t (*hash_fn)(void *obj);

struct hashtbl *hashtbl_create(size_t key_size, size_t element_size,
                               hash_fn hash_fn);

void hashtbl_insert(struct hashtbl *hashtbl, void *key, void *data);
void hashtbl_remove(struct hashtbl *hashtbl, void *key);
void *hashtbl_lookup(struct hashtbl *hashtbl, void *key);

#endif
