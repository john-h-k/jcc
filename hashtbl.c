#include "hashtbl.h"

#include "hash.h"
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

void hashtbl_hash_str(struct hasher *hasher, const void *obj) {
  hasher_hash_str(hasher, obj);
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
  struct hasher hasher = hasher_create();
  hashtbl->hash_fn(&hasher, key);
  hash_t hash = hasher_finish(&hasher);

  hashtbl_insert_with_hash(hashtbl, key, data, hash);
}

struct lookup_internal {
  void *triple;
  struct bucket *bucket;
  size_t idx;
};

static struct lookup_internal hashtbl_lookup_triple(struct hashtbl *hashtbl, const void *key) {
  struct hasher hasher = hasher_create();
  hashtbl->hash_fn(&hasher, key);
  hash_t hash = hasher_finish(&hasher);

  size_t num_buckets = vector_length(hashtbl->buckets);

  if (!num_buckets) {
    return (struct lookup_internal){ .triple = NULL, .bucket = NULL };
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
      return (struct lookup_internal){ .triple = triple, .bucket = bucket, .idx = i };
    }
  }

  return (struct lookup_internal){ .triple = NULL, .bucket = NULL };
}


void hashtbl_remove(struct hashtbl *hashtbl, const void *key) {
  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  vector_remove_at(lookup.bucket->elems, lookup.idx);
}

void *hashtbl_lookup(struct hashtbl *hashtbl, const void *key) {
  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  if (lookup.triple) {
    return (char *)lookup.triple + sizeof(hash_t) + hashtbl->key_size;
  } else {
    return NULL;
  }
}
