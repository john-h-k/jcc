#ifndef HASHTBL_H
#define HASHTBL_H

#include "alloc.h"
#include "hash.h"

#include <stdbool.h>
#include <stddef.h>

struct hashtbl;
struct hashtbl_iter;

typedef size_t hashtbl_ver_t;

struct hashtbl_entry {
  const void *key;
  void *data;

  hashtbl_ver_t ver;
};

typedef void (*hash_fn)(struct hasher *hasher, const void *obj);
typedef bool (*eq_fn)(const void *l, const void *r);

struct hashtbl *hashtbl_create(size_t key_size, size_t element_size,
                               hash_fn hash_fn, eq_fn eq_gn);

struct hashtbl *hashtbl_create_str_keyed(size_t element_size);

struct hashtbl *hashtbl_create_ustr_keyed(size_t element_size);

struct hashtbl *hashtbl_create_in_arena(struct arena_allocator *arena,
                                        size_t key_size, size_t element_size,
                                        hash_fn hash_fn, eq_fn eq_fn);

struct hashtbl *hashtbl_create_str_keyed_in_arena(struct arena_allocator *arena,
                                                  size_t element_size);

struct hashtbl *
hashtbl_create_ustr_keyed_in_arena(struct arena_allocator *arena,
                                        size_t element_size);

void hashtbl_free(struct hashtbl **hashtbl);

size_t hashtbl_size(struct hashtbl *hashtbl);
hashtbl_ver_t hashtbl_ver(struct hashtbl *hashtbl);

struct hashtbl_iter *hashtbl_iter(struct hashtbl *hashtbl);
bool hashtbl_iter_next(struct hashtbl_iter *hashtbl_iter,
                       struct hashtbl_entry *entry);

void hashtbl_insert(struct hashtbl *hashtbl, const void *key, const void *data);
void hashtbl_remove(struct hashtbl *hashtbl, const void *key);

void *hashtbl_lookup(struct hashtbl *hashtbl, const void *key);

struct hashtbl_entry hashtbl_lookup_entry(struct hashtbl *hashtbl, const void *key);
struct hashtbl_entry hashtbl_lookup_with_entry(struct hashtbl *hashtbl, const void *key, const struct hashtbl_entry *entry);

void *hashtbl_lookup_or_insert(struct hashtbl *hashtbl, const void *key,
                               void *data);

struct hashtbl_entry hashtbl_lookup_entry_or_insert(struct hashtbl *hashtbl, const void *key,
                               void *data);


void hashtbl_invalidate_entries(struct hashtbl *hashtbl);

// given a pointer to data and should write to it
typedef void hashtbl_data_fn(void *data, void *metadata);
void *hashtbl_lookup_or_insert_with(struct hashtbl *hashtbl, const void *key,
                                    hashtbl_data_fn fn, void *metadata);

struct hashtbl_entry
hashtbl_lookup_entry_or_insert_with(struct hashtbl *hashtbl, const void *key,
                                    hashtbl_data_fn fn, void *metadata);

// Often hash tables are keyed by either a standard C string, or a C string with
// an explicit size so we provide convenience methods for them

void hashtbl_hash_str(struct hasher *hasher, const void *obj);
bool hashtbl_eq_str(const void *l, const void *r);

void hashtbl_hash_ustr(struct hasher *hasher, const void *value);
bool hashtbl_eq_ustr(const void *l, const void *r);

#endif
