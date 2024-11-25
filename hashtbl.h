#ifndef HASHTBL_H
#define HASHTBL_H

#include "hash.h"

#include <stddef.h>
#include <stdbool.h>

struct hashtbl;
struct hashtbl_iter;

struct hashtbl_entry {
  const void *key;
  void *data;
};

typedef void (*hash_fn)(struct hasher *hasher, const void *obj);
typedef bool (*eq_fn)(const void *l, const void *r);

struct hashtbl *hashtbl_create(size_t key_size, size_t element_size,
                               hash_fn hash_fn, eq_fn eq_gn);

void hashtbl_hash_str(struct hasher *hasher, const void *obj);
bool hashtbl_eq_str(const void *l, const void *r);

struct hashtbl *hashtbl_create_str_keyed(size_t element_size);

size_t hashtbl_size(struct hashtbl *hashtbl);

struct hashtbl_iter *hashtbl_iter(struct hashtbl *hashtbl);
bool hashtbl_iter_next(struct hashtbl_iter *hashtbl_iter, struct hashtbl_entry *entry);

void hashtbl_insert(struct hashtbl *hashtbl, const void *key, const void *data);
void hashtbl_remove(struct hashtbl *hashtbl, const void *key);
void *hashtbl_lookup(struct hashtbl *hashtbl, const void *key);

#endif
