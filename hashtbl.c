#include "hashtbl.h"

#include "util.h"
#include "vector.h"

// two indirections, maybe change so it contains a single instance of element?
struct bucket {
  // contains triple (hash, key, elem)
  struct vector *elems;
};

struct hashtbl {
  hash_fn hash_fn;
  eq_fn eq_fn;
  size_t key_size;
  size_t element_size;

  struct vector *buckets;

  size_t len;
};

struct hashtbl *hashtbl_create(size_t key_size, size_t element_size,
                               hash_fn hash_fn, eq_fn eq_fn) {
  struct hashtbl *tbl = nonnull_malloc(sizeof(*tbl));
  tbl->hash_fn = hash_fn;
  tbl->key_size = key_size;
  tbl->element_size = element_size;
  tbl->eq_fn = eq_fn;
  tbl->buckets = vector_create(sizeof(struct bucket));

  return tbl;
}

hash_t hashtbl_hash_str(const void *obj) {
  const char *str = obj;

  hash_t hash = 5381;
  int c;

  while ((c = *str++)) {
    hash = ((hash << 5) + hash) + c;
  }

  return hash;
}

bool hashtbl_eq_str(const void *l, const void *r) { return strcmp(l, r) == 0; }

struct hashtbl *hashtbl_create_str_keyed(size_t element_size) {
  return hashtbl_create(sizeof(char *), element_size, hashtbl_hash_str,
                        hashtbl_eq_str);
}

void hashtbl_insert_with_hash(struct hashtbl *hashtbl, const void *key,
                              const void *data, hash_t hash);

// when len >= buckets * max_fill, double bucket size and rebuild
static const float MAX_FILL = 0.8f;

static void hashtbl_rebuild(struct hashtbl *hashtbl) {
  struct vector *buckets = hashtbl->buckets;
  size_t num_buckets = vector_length(buckets);

  size_t new_num_buckets = num_buckets < 8 ? 8 : num_buckets * 2;

  hashtbl->len = 0;
  hashtbl->buckets = vector_create(sizeof(struct bucket));
  vector_ensure_capacity(hashtbl->buckets, new_num_buckets);

  size_t triple_size = sizeof(hash_t) + hashtbl->key_size + hashtbl->element_size;
  triple_size = ROUND_UP(triple_size, 8);

  for (size_t i = 0; i < new_num_buckets; i++) {
    struct bucket bucket = {
      .elems = vector_create(triple_size)
    };

    vector_push_back(hashtbl->buckets, &bucket);
  }

  for (size_t i = 0; i < num_buckets; i++) {
    struct bucket *bucket = vector_get(buckets, i);

    struct vector *elems = bucket->elems;

    size_t num_elems = vector_length(elems);
    for (size_t j = 0; j < num_elems; j++) {
      void *triple = vector_get(elems, j);
      hash_t hash = *(hash_t *)triple;
      void *key = (char *)triple + sizeof(hash);
      void *data = (char *)key + hashtbl->key_size;

      hashtbl_insert_with_hash(hashtbl, key, data, hash);
    }

    vector_free(&elems);
  }

  vector_free(&buckets);
}

void hashtbl_insert_with_hash(struct hashtbl *hashtbl, const void *key,
                              const void *data, hash_t hash) {
  size_t num_buckets = vector_length(hashtbl->buckets);

  if (hashtbl->len + 1 > num_buckets * MAX_FILL) {
    hashtbl_rebuild(hashtbl);
  }

  num_buckets = vector_length(hashtbl->buckets);

  hash_t bucket_idx = hash % num_buckets;

  struct bucket *bucket = vector_get(hashtbl->buckets, bucket_idx);

  void *triple = vector_push_back(bucket->elems, NULL);
  *(hash_t *)triple = hash;

  void *key_entry = (char *)triple + sizeof(hash);
  void *data_entry = (char *)key_entry + hashtbl->key_size;
  memcpy(key_entry, key, hashtbl->key_size);
  memcpy(data_entry, data, hashtbl->element_size);

  hashtbl->len++;
}

void hashtbl_insert(struct hashtbl *hashtbl, const void *key,
                    const void *data) {
  hash_t hash = hashtbl->hash_fn(key);

  hashtbl_insert_with_hash(hashtbl, key, data, hash);
}

void hashtbl_remove(struct hashtbl *hashtbl, const void *key);
void *hashtbl_lookup(struct hashtbl *hashtbl, const void *key) {
  hash_t hash = hashtbl->hash_fn(key);

  size_t num_buckets = vector_length(hashtbl->buckets);

  if (!num_buckets) {
    return NULL;
  }

  hash_t bucket_idx = hash % num_buckets;

  struct bucket *bucket = vector_get(hashtbl->buckets, bucket_idx);

  size_t bucket_len = vector_length(bucket->elems);
  for (size_t i = 0; i < bucket_len; i++) {
    void *triple = vector_get(bucket->elems, i);
    void *key_entry = (char *)triple + sizeof(hash_t);

    bool eq;
    if (hashtbl->eq_fn) {
      eq = hashtbl->eq_fn(key, key_entry);
    } else {
      eq = memcmp(key, key_entry, hashtbl->key_size) == 0;
    }

    if (eq) {
      void *entry_data = (char *)key_entry + hashtbl->key_size;
      return entry_data;
    }
  }

  return NULL;
}
