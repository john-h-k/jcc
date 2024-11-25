#include "hash.h"

#include "util.h"

#include <stddef.h>

// roughly an implementation of foldhash, https://github.com/orlp/foldhash

static const uint64_t SEEDS[5] = {
    0x243f6a8885a308d3, 0x13198a2e03707344, 0xa4093822299f31d0,
    0x082efa98ec4e6c89, 0x452821e638d01377,
};

static uint64_t folded_multiply(uint64_t l, uint64_t r) {
#ifdef HAS_INT128
  uint128_t full = (uint128_t)l * (uint128_t)r;
  uint64_t lo = (uint64_t)full;
  uint64_t hi = (uint64_t)(full >> 64);

  return lo ^ hi;
#else
  uint32_t ll = (uint32_t)l;
  uint32_t lr = (uint32_t)r;
  uint32_t hr = (uint32_t)(l >> 32);
  uint32_t hr = (uint32_t)(r >> 32);

  uint64_t afull = (uint64_t)ll * (uint64_t)hr;
  uint64_t bfull = (uint64_t)hl * (uint64_t)lr;

  return afull ^ rotateright64(bfull, 32);
#endif
}

struct hasher hasher_create(void) {
  return (struct hasher){.accumulator = SEEDS[0],
                            .sponge0 = 0,
                            .sponge1 = 0,
                            .sponge_len = 0,
                            .fold_seed = SEEDS[1],
                            .expand_seed0 = SEEDS[2],
                            .expand_seed1 = SEEDS[3],
                            .expand_seed2 = SEEDS[4]};
}

void hasher_hash_pointer(struct hasher *hasher, void *value) {
  hasher_hash_integer(hasher, (unsigned long long)value, sizeof(value));
}

void hasher_hash_integer(struct hasher *hasher, unsigned long long value,
                         size_t byte_size) {
  uint8_t bits = (uint8_t)(8 * byte_size);

  if (hasher->sponge_len + bits > 128) {
    uint64_t lo = hasher->sponge0;
    uint64_t hi = hasher->sponge1;

    hasher->accumulator =
        folded_multiply(lo ^ hasher->accumulator, hi ^ hasher->fold_seed);

    // because our hash integer only takes uint64, behaviour differs
    hasher->sponge0 = value;
    hasher->sponge1 = 0;
    hasher->sponge_len = bits;
  } else {
    size_t shift = 128 - hasher->sponge_len;

    uint64_t lo = hasher->sponge_len >= 64 ? 0 : value << hasher->sponge_len;
    uint64_t hi = shift >= 64 ? 0 : value >> shift ;

    hasher->sponge0 |= lo;
    hasher->sponge1 |= hi;
    hasher->sponge_len += bits;
  }
}

void hasher_hash_str(struct hasher *hasher, const char *value) {
  // TODO: implement a method that doesn't first precompute length
  size_t len = strlen(value);

  hasher_hash_bytes(hasher, value, len);
}

static uint64_t read_uint64_unaligned(const void *p) {
  switch ((uintptr_t)p % sizeof(uint64_t)) {
  case 0:
    return *(const uint64_t *)p;
  case 2:
  case 6: {
    const uint16_t *c = p;
    return (uint64_t)c[0] | ((uint64_t)c[1] << 16) | ((uint64_t)c[2] << 32) |
           ((uint64_t)c[3] << 48);
  }
  case 4: {
    const uint32_t *c = p;
    return (uint64_t)c[0] | ((uint64_t)c[1] << 32);
  }
  default: {
    const uint8_t *c = p;
    return (uint64_t)c[0] | ((uint64_t)c[1] << 8) | ((uint64_t)c[2] << 16) |
           ((uint64_t)c[3] << 24) | ((uint64_t)c[4] << 32) |
           ((uint64_t)c[5] << 40) | ((uint64_t)c[6] << 48) |
           ((uint64_t)c[7] << 56);
  }
  }
}

static uint32_t read_uint32_unaligned(const void *p) {
  switch ((uintptr_t)p % sizeof(uint32_t)) {
  case 0:
    return *(const uint32_t *)p;
  case 2: {
    const uint16_t *c = p;
    return (uint16_t)c[0] | ((uint16_t)c[1] << 16);
  }
  default: {
    const uint8_t *c = p;
    return (uint32_t)c[0] | ((uint32_t)c[1] << 8) | ((uint32_t)c[2] << 16) |
           ((uint32_t)c[3] << 24);
  }
  }
}

static hash_t hasher_hash_med(const char *data, size_t length, uint64_t s0,
                              uint64_t s1, uint64_t fold_seed) {
  size_t num_chunks = length / 16;

  const char *head = data;
  const char *tail = (data + length) - 16;

  for (size_t i = 0; i < num_chunks; i++, head += 16, tail -= 16) {
    if (head >= (tail + 16)) {
      break;
    }

    uint64_t a = read_uint64_unaligned(head);
    uint64_t b = read_uint64_unaligned(head + 8);
    uint64_t c = read_uint64_unaligned(tail);
    uint64_t d = read_uint64_unaligned(tail + 8);
    s0 = folded_multiply(a ^ s0, c ^ fold_seed);
    s1 = folded_multiply(b ^ s1, d ^ fold_seed);
  }

  return s0 ^ s1;
}

static hash_t hasher_hash_long(const char *data, size_t length, uint64_t s0,
                               uint64_t s1, uint64_t s2, uint64_t s3,
                               uint64_t fold_seed) {
  size_t num_chunks = length / 64;
  size_t remainder = length % 64;

  for (size_t i = 0; i < num_chunks; i++) {
    const char *chunk = &data[i * 64];

    uint64_t a = read_uint64_unaligned(chunk);
    uint64_t b = read_uint64_unaligned(chunk + 8);
    uint64_t c = read_uint64_unaligned(chunk + 16);
    uint64_t d = read_uint64_unaligned(chunk + 24);
    uint64_t e = read_uint64_unaligned(chunk + 32);
    uint64_t f = read_uint64_unaligned(chunk + 40);
    uint64_t g = read_uint64_unaligned(chunk + 48);
    uint64_t h = read_uint64_unaligned(chunk + 56);

    s0 = folded_multiply(a ^ s0, e ^ fold_seed);
    s1 = folded_multiply(b ^ s1, f ^ fold_seed);
    s2 = folded_multiply(c ^ s2, g ^ fold_seed);
    s3 = folded_multiply(d ^ s3, h ^ fold_seed);
  }

  s0 ^= s2;
  s1 ^= s3;

  if (remainder) {
    remainder = MAX(remainder, 16);
    return hasher_hash_med((data + length) - remainder, length, s0, s1,
                           fold_seed);
  } else {
    return s0 ^ s1;
  }
}

void hasher_hash_bytes(struct hasher *hasher, const void *value,
                       size_t length) {
  uint64_t s0 = hasher->accumulator;
  uint64_t s1 = hasher->expand_seed0;

  const char *data = value;

  if (length <= 16) {
    if (length >= 8) {
      s0 ^= read_uint64_unaligned(data);
      s1 ^= read_uint64_unaligned(data + (length - 8));
    } else if (length >= 4) {
      s0 ^= read_uint32_unaligned(data);
      s1 ^= read_uint32_unaligned(data + (length - 4));
    } else if (length > 0) {
      const uint8_t lo = data[0];
      uint8_t mid = data[length / 2];
      uint8_t hi = data[length - 1];

      s0 ^= (uint64_t)lo;
      s1 ^= ((uint64_t)hi << 8) | (uint64_t)mid;
    }

    hasher->accumulator = folded_multiply(s0, s1);
  } else {
    hasher->accumulator =
        hasher_hash_long(value, length, s0, s1, hasher->expand_seed1,
                         hasher->expand_seed2, hasher->fold_seed);
  }
}

hash_t hasher_finish(struct hasher *hasher) {
  if (hasher->sponge_len) {
    uint64_t lo = hasher->sponge0;
    uint64_t hi = hasher->sponge1;

    return folded_multiply(lo ^ hasher->accumulator, hi ^ hasher->fold_seed);
  } else {
    return hasher->accumulator;
  }
}
