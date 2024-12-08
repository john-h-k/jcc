#ifndef HASH_H
#define HASH_H

#include <stddef.h>
#include <stdint.h>

typedef size_t hash_t;

// type is public so we don't need to allocate for it
struct hasher {
  uint64_t accumulator;
  uint64_t sponge0, sponge1;
  uint8_t sponge_len;
  uint64_t fold_seed, expand_seed0, expand_seed1, expand_seed2;
};

struct hasher hasher_create(void);

void hasher_hash_pointer(struct hasher *hasher, const void *value);
void hasher_hash_integer(struct hasher *hasher, unsigned long long value,
                         size_t byte_size);
void hasher_hash_str(struct hasher *hasher, const char *value);
void hasher_hash_bytes(struct hasher *hasher, const void *value, size_t length);

hash_t hasher_finish(struct hasher *hasher);

#endif

