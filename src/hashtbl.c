#include "hashtbl.h"

#include "alloc.h"
#include "hash.h"
#include "util.h"
#include "vector.h"

// two indirections, maybe change so it contains a single instance of element?
struct bucket {
  // contains triple (hash, key, elem)
  struct vector *elems;
};

struct hashtbl {
  struct arena_allocator *arena;

  hash_fn hash_fn;
  eq_fn eq_fn;
  size_t key_size;
  size_t element_size;

  // increments whenever hashtbl pointers are invalidated
  size_t ver;

  struct vector *buckets;

  size_t len;
};

struct hashtbl_iter {
  struct hashtbl *hashtbl;
  size_t bucket_idx;
  size_t elem_idx;
};

void hashtbl_invalidate_entries(struct hashtbl *hashtbl) { hashtbl->ver++; }

struct hashtbl *hashtbl_create(size_t key_size, size_t element_size,
                               hash_fn hash_fn, eq_fn eq_fn) {
  return hashtbl_create_in_arena(NULL, key_size, element_size, hash_fn, eq_fn);
}

struct hashtbl *hashtbl_create_str_keyed(size_t element_size) {
  return hashtbl_create_str_keyed_in_arena(NULL, element_size);
}

struct hashtbl *hashtbl_create_ustr_keyed(size_t element_size) {
  return hashtbl_create_ustr_keyed_in_arena(NULL, element_size);
}

struct hashtbl *hashtbl_create_in_arena(struct arena_allocator *arena,
                                        size_t key_size, size_t element_size,
                                        hash_fn hash_fn, eq_fn eq_fn) {

  struct hashtbl *tbl;
  if (arena) {
    tbl = aralloc(arena, sizeof(*tbl));
  } else {
    tbl = nonnull_malloc(sizeof(*tbl));
  }

  tbl->arena = arena;
  tbl->hash_fn = hash_fn;
  tbl->key_size = key_size;
  tbl->element_size = element_size;
  tbl->eq_fn = eq_fn;
  tbl->buckets = vector_create_in_arena(sizeof(struct bucket), arena);
  tbl->len = 0;
  tbl->ver = 0;

  // add one bucket because it makes lookup/insertion easier
  // is this a price we are happy to pay? can be reworked to not do this
  size_t triple_size = sizeof(hash_t) + tbl->key_size + tbl->element_size;
  struct bucket bucket = {.elems =
                              vector_create_in_arena(triple_size, tbl->arena)};

  vector_push_back(tbl->buckets, &bucket);

  return tbl;
}

struct hashtbl *hashtbl_create_str_keyed_in_arena(struct arena_allocator *arena,
                                                  size_t element_size) {
  return hashtbl_create_in_arena(arena, sizeof(char *), element_size,
                                 hashtbl_hash_str, hashtbl_eq_str);
}

struct hashtbl *
hashtbl_create_ustr_keyed_in_arena(struct arena_allocator *arena,
                                   size_t element_size) {
  return hashtbl_create_in_arena(arena, sizeof(ustr_t), element_size,
                                 hashtbl_hash_ustr, hashtbl_eq_ustr);
}

void hashtbl_free(struct hashtbl **hashtbl) {
  if ((*hashtbl)->arena) {
    *hashtbl = NULL;
    return;
  }

  size_t num_buckets = vector_length((*hashtbl)->buckets);
  for (size_t i = 0; i < num_buckets; i++) {
    struct bucket *bucket = vector_get((*hashtbl)->buckets, i);

    vector_free(&bucket->elems);
  }

  vector_free(&(*hashtbl)->buckets);

  free(*hashtbl);
  *hashtbl = NULL;
}

static struct hashtbl_entry hashtbl_mk_entry(struct hashtbl *hashtbl,
                                             void *triple) {
  if (triple) {
    return (struct hashtbl_entry){.key = (char *)triple + sizeof(hash_t),
                                  .data = (char *)triple + sizeof(hash_t) +
                                          hashtbl->key_size,
                                  .ver = hashtbl->ver};
  } else {
    return (struct hashtbl_entry){0};
  }
}

struct hashtbl_iter *hashtbl_iter(struct hashtbl *hashtbl) {
  struct hashtbl_iter *iter;
  if (hashtbl->arena) {
    iter = aralloc(hashtbl->arena, sizeof(*iter));
  } else {
    iter = nonnull_malloc(sizeof(*iter));
  }
  *iter =
      (struct hashtbl_iter){.hashtbl = hashtbl, .bucket_idx = 0, .elem_idx = 0};

  return iter;
}

bool hashtbl_iter_next(struct hashtbl_iter *hashtbl_iter,
                       struct hashtbl_entry *entry) {
  struct hashtbl *hashtbl = hashtbl_iter->hashtbl;
  size_t num_buckets = vector_length(hashtbl->buckets);

  if (hashtbl_iter->bucket_idx >= num_buckets) {
    goto finished;
  }

  struct bucket *bucket =
      vector_get(hashtbl->buckets, hashtbl_iter->bucket_idx);

  while (vector_empty(bucket->elems) &&
         ++hashtbl_iter->bucket_idx < num_buckets) {
    hashtbl_iter->elem_idx = 0;
    bucket = vector_get(hashtbl->buckets, hashtbl_iter->bucket_idx);
  }

  if (vector_empty(bucket->elems)) {
    goto finished;
  }

  size_t num_elems = vector_length(bucket->elems);

  if (hashtbl_iter->elem_idx >= num_elems) {
    goto finished;
  }

  // BUG: this does not work! i assume lower is moving around ops and messing up
  // order void *triple = vector_get(bucket->elems, hashtbl_iter->elem_idx++);
  void *triple = vector_get(bucket->elems, hashtbl_iter->elem_idx);
  hashtbl_iter->elem_idx++;

  if (hashtbl_iter->elem_idx == num_elems) {
    hashtbl_iter->elem_idx = 0;
    hashtbl_iter->bucket_idx++;
  }

  *entry = hashtbl_mk_entry(hashtbl, triple);

  return true;

finished:
  if (!hashtbl->arena) {
    free(hashtbl_iter);
  }

  return false;
}

size_t hashtbl_size(struct hashtbl *hashtbl) { return hashtbl->len; }

hashtbl_ver_t hashtbl_ver(struct hashtbl *hashtbl) { return hashtbl->ver; }

void hashtbl_hash_str(struct hasher *hasher, const void *obj) {
  hasher_hash_str(hasher, *(const char *const *)obj);
}

bool hashtbl_eq_str(const void *l, const void *r) {
  return strcmp(*(const char *const *)l, *(const char *const *)r) == 0;
}

void hashtbl_insert_with_hash(struct hashtbl *hashtbl, const void *key,
                              const void *data, hash_t hash);

// when len >= buckets * max_fill, double bucket size and rebuild
static const float MAX_FILL = 0.7f;

#define MIN_BUCKET_COUNT (8)

static struct bucket *hashtbl_create_bucket(struct hashtbl *hashtbl) {
  size_t triple_size =
      sizeof(hash_t) + hashtbl->key_size + hashtbl->element_size;
  triple_size = ROUND_UP(triple_size, 8);

  struct bucket bucket = {
      .elems = vector_create_in_arena(triple_size, hashtbl->arena)};

  return vector_push_back(hashtbl->buckets, &bucket);
}

static void hashtbl_rebuild(struct hashtbl *hashtbl) {
  hashtbl_ver_t ver = hashtbl->ver;

  struct vector *buckets = hashtbl->buckets;
  size_t num_buckets = vector_length(buckets);

  size_t new_num_buckets =
      num_buckets < MIN_BUCKET_COUNT ? MIN_BUCKET_COUNT : num_buckets * 2;

  hashtbl->len = 0;
  hashtbl->buckets =
      vector_create_in_arena(sizeof(struct bucket), hashtbl->arena);
  vector_ensure_capacity(hashtbl->buckets, new_num_buckets);

  for (size_t i = 0; i < new_num_buckets; i++) {
    hashtbl_create_bucket(hashtbl);
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

  hashtbl->ver = ver + 1;
}

struct lookup_internal {
  // no good way to represent this in C, but it is `hash_t ; key ; data`
  void *triple;

  // we store a dup of the hash just so we don't deal with touching triple so
  // much maybe remove?
  hash_t hash;
  struct bucket *bucket;
  size_t idx;
};

static struct lookup_internal hashtbl_lookup_triple(struct hashtbl *hashtbl,
                                                    const void *key);
static struct lookup_internal
hashtbl_lookup_triple_with_hash(struct hashtbl *hashtbl, const void *key,
                                hash_t hash);

// given a lookup, insert. used to efficiently implement
// `hashtbl_lookup_or_insert...` fns
static struct lookup_internal
hashtbl_insert_with_lookup(struct hashtbl *hashtbl, const void *key,
                           const void *data, struct lookup_internal lookup) {

  if (!lookup.triple) {
    if (!lookup.bucket) {
      lookup.bucket = hashtbl_create_bucket(hashtbl);
    }

    struct bucket *bucket = lookup.bucket;
    lookup.triple = vector_push_back(bucket->elems, NULL);

    hashtbl->len++;
    hashtbl->ver++;
  }

  memcpy(lookup.triple, &lookup.hash, sizeof(lookup.hash));

  void *key_entry = (char *)lookup.triple + sizeof(lookup.hash);
  void *data_entry = (char *)key_entry + hashtbl->key_size;
  memcpy(key_entry, key, hashtbl->key_size);

  if (data) {
    memcpy(data_entry, data, hashtbl->element_size);
  }

  return lookup;
}

void hashtbl_insert_with_hash(struct hashtbl *hashtbl, const void *key,
                              const void *data, hash_t hash) {
  struct lookup_internal lookup =
      hashtbl_lookup_triple_with_hash(hashtbl, key, hash);
  hashtbl_insert_with_lookup(hashtbl, key, data, lookup);

  // do rebuild AFTER actual lookup
  // this prevents false rebuild when `hashtbl_insert` overwrites
  size_t num_buckets = vector_length(hashtbl->buckets);

  if ((float)(hashtbl->len + 1) > (float)num_buckets * MAX_FILL) {
    hashtbl_rebuild(hashtbl);
  }
}

static hash_t hashtbl_hash(const struct hashtbl *restrict hashtbl,
                           const void *key) {
  if (hashtbl->key_size <= sizeof(hash_t) && !hashtbl->hash_fn) {
    hash_t hash;
    memcpy(&hash, key, hashtbl->key_size);
    return hash;
  }

  struct hasher hasher = hasher_create();

  if (hashtbl->hash_fn) {
    hashtbl->hash_fn(&hasher, key);
  } else {
    hasher_hash_bytes(&hasher, key, hashtbl->key_size);
  }
  return hasher_finish(&hasher);
}

static bool hashtbl_eq(struct hashtbl *hashtbl, const void *lkey,
                       const void *rkey) {
  if (hashtbl->eq_fn) {
    return hashtbl->eq_fn(lkey, rkey);
  } else {
    return memcmp(lkey, rkey, hashtbl->key_size) == 0;
  }
}

void hashtbl_insert(struct hashtbl *hashtbl, const void *key,
                    const void *data) {
  hash_t hash = hashtbl_hash(hashtbl, key);

  hashtbl_insert_with_hash(hashtbl, key, data, hash);
}

static struct lookup_internal
hashtbl_lookup_triple_with_hash(struct hashtbl *hashtbl, const void *key,
                                hash_t hash) {

  size_t num_buckets = vector_length(hashtbl->buckets);

  if (!num_buckets) {
    return (struct lookup_internal){
        .triple = NULL, .hash = hash, .bucket = NULL};
  }

  hash_t bucket_idx = hash % num_buckets;

  struct bucket *bucket = vector_get(hashtbl->buckets, bucket_idx);

  size_t bucket_len = vector_length(bucket->elems);
  for (size_t i = 0; i < bucket_len; i++) {
    void *triple = vector_get(bucket->elems, i);
    void *key_entry = (char *)triple + sizeof(hash_t);

    if (hashtbl_eq(hashtbl, key, key_entry)) {
      return (struct lookup_internal){
          .triple = triple, .hash = hash, .bucket = bucket, .idx = i};
    }
  }

  return (struct lookup_internal){
      .triple = NULL, .hash = hash, .bucket = bucket};
}

static struct lookup_internal hashtbl_lookup_triple(struct hashtbl *hashtbl,
                                                    const void *key) {
  hash_t hash = hashtbl_hash(hashtbl, key);
  return hashtbl_lookup_triple_with_hash(hashtbl, key, hash);
}

void hashtbl_remove(struct hashtbl *hashtbl, const void *key) {
  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  if (lookup.triple) {
    hashtbl->ver++;
    vector_remove_at(lookup.bucket->elems, lookup.idx);
  }
}

void *hashtbl_lookup(struct hashtbl *hashtbl, const void *key) {
  if (!hashtbl->len) {
    return NULL;
  }

  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  if (lookup.triple) {
    return (char *)lookup.triple + sizeof(hash_t) + hashtbl->key_size;
  } else {
    return NULL;
  }
}

struct hashtbl_entry hashtbl_lookup_entry(struct hashtbl *hashtbl,
                                          const void *key) {
  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  return hashtbl_mk_entry(hashtbl, lookup.triple);
}

struct hashtbl_entry
hashtbl_lookup_with_entry(struct hashtbl *hashtbl, const void *key,
                          const struct hashtbl_entry *entry) {
  DEBUG_ASSERT(hashtbl->ver >= entry->ver,
               "hashtbl version %zu less than entry version %zu", hashtbl->ver,
               entry->ver);

  if (entry->ver == hashtbl->ver) {
    return *entry;
  }

  return hashtbl_lookup_entry(hashtbl, key);
}

void *hashtbl_lookup_or_insert(struct hashtbl *hashtbl, const void *key,
                               void *data) {
  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  if (!lookup.triple) {
    lookup = hashtbl_insert_with_lookup(hashtbl, key, data, lookup);
  }

  return (char *)lookup.triple + sizeof(hash_t) + hashtbl->key_size;
}

struct hashtbl_entry hashtbl_lookup_entry_or_insert(struct hashtbl *hashtbl,
                                                    const void *key,
                                                    void *data) {
  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  if (!lookup.triple) {
    lookup = hashtbl_insert_with_lookup(hashtbl, key, data, lookup);
  }

  return hashtbl_mk_entry(hashtbl, lookup.triple);
}

void *hashtbl_lookup_or_insert_with(struct hashtbl *hashtbl, const void *key,
                                    hashtbl_data_fn fn, void *metadata) {
  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  if (!lookup.triple) {
    lookup = hashtbl_insert_with_lookup(hashtbl, key, NULL, lookup);

    void *data = (char *)lookup.triple + sizeof(hash_t) + hashtbl->key_size;
    fn(data, metadata);
  }

  return (char *)lookup.triple + sizeof(hash_t) + hashtbl->key_size;
}

struct hashtbl_entry
hashtbl_lookup_entry_or_insert_with(struct hashtbl *hashtbl, const void *key,
                                    hashtbl_data_fn fn, void *metadata) {
  struct lookup_internal lookup = hashtbl_lookup_triple(hashtbl, key);

  if (!lookup.triple) {
    lookup = hashtbl_insert_with_lookup(hashtbl, key, NULL, lookup);

    void *data = (char *)lookup.triple + sizeof(hash_t) + hashtbl->key_size;
    fn(data, metadata);
  }

  return hashtbl_mk_entry(hashtbl, lookup.triple);
}

void hashtbl_hash_ustr(struct hasher *hasher, const void *value) {
  const ustr_t *s = value;

  hasher_hash_bytes(hasher, s->str, s->len);
  hasher_hash_integer(hasher, s->len, sizeof(s->len));
}

bool hashtbl_eq_ustr(const void *l, const void *r) {
  const ustr_t *sl = l;
  const ustr_t *sr = r;

  return ustr_eq(*sl, *sr);
}
