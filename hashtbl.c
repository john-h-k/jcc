#include "hashtbl.h"

#include "util.h"
#include "vector.h"

// two indirections, maybe change so it contains a single instance of element?
struct bucket {
  hash_t hash;

  // contains pair (key, elem)
  struct vector *elems;
};

struct hashtbl {
  hash_fn hash_fn;
  size_t key_size;
  size_t element_size;

  struct vector *buckets;

  size_t len;
};

struct hashtbl *hashtbl_create(size_t key_size, size_t element_size,
                               hash_fn hash_fn) {
  struct hashtbl *tbl = nonnull_malloc(sizeof(*tbl));
  tbl->hash_fn = hash_fn;
  tbl->key_size = key_size;
  tbl->element_size = element_size;
  tbl->buckets = vector_create(sizeof(struct bucket));

  return tbl;
}

// when len >= buckets * max_fill, double bucket size and rebuild
const float MAX_FILL = 0.8;

void hashtbl_rebuild(struct hashtbl *hashtbl) {
  UNUSED_ARG(hashtbl);
  todo(__func__);
}

void hashtbl_insert(struct hashtbl *hashtbl, void *key, void *data) {
  hash_t hash = hashtbl->hash_fn(key);

  size_t num_buckets = vector_length(hashtbl->buckets);

  if (hashtbl->len + 1 > num_buckets * MAX_FILL) {
    hashtbl_rebuild(hashtbl);
  }

  hash_t bucket_idx = hash % num_buckets;

  struct bucket *bucket = vector_get(hashtbl->buckets, bucket_idx);

  void *key_and_elem = vector_push_back(bucket->elems, NULL);
  memcpy(key_and_elem, key, hashtbl->key_size);
  memcpy((char *)key_and_elem + hashtbl->key_size, data, hashtbl->element_size);

  hashtbl->len++;
}

void hashtbl_remove(struct hashtbl *hashtbl, void *key);
void *hashtbl_lookup(struct hashtbl *hashtbl, void *key) {
  hash_t hash = hashtbl->hash_fn(key);

  size_t num_buckets = vector_length(hashtbl->buckets);

  if (hashtbl->len + 1 > num_buckets * MAX_FILL) {
    hashtbl_rebuild(hashtbl);
  }

  hash_t bucket_idx = hash % num_buckets;

  struct bucket *bucket = vector_get(hashtbl->buckets, bucket_idx);

  size_t bucket_len = vector_length(bucket->elems);
  for (size_t i = 0; i < bucket_len; i++) {
    void *el = vector_get(bucket->elems, i);

    // TODO: use eq fn
    if (memcmp(el, key, hashtbl->key_size) == 0) {
      return el;
    }
  }

  return NULL;
}
