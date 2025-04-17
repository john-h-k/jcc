#include "bitset.h"

#include "util.h"

#include <limits.h>

typedef unsigned long long chunk_t;

struct bitset {
  size_t num_set;

  size_t num_elements;
  size_t num_chunks;

  chunk_t chunks[];
};

struct bitset *bitset_create(size_t num_elements, bool init_value) {
  size_t bits_per_chunk = sizeof(chunk_t) * 8;

  size_t num_chunks = (num_elements + (bits_per_chunk - 1)) / bits_per_chunk;

  struct bitset *bitset =
      nonnull_malloc(sizeof(*bitset) + (sizeof(chunk_t) * num_chunks));
  bitset->num_elements = num_elements;
  bitset->num_chunks = num_chunks;
  bitset->num_set = 0;

  bitset_clear(bitset, init_value);

  return bitset;
}

size_t bitset_length(struct bitset *bitset) { return bitset->num_elements; }

unsigned long long bitset_as_ull(struct bitset *bitset) {
  DEBUG_ASSERT(bitset->num_chunks <= 1, "too many chunks to get bitset as ull");

  if (bitset->num_chunks) {
    return bitset->chunks[0];
  }

  return 0;
}

unsigned long long bitset_popcnt(struct bitset *bitset) {
  return bitset->num_set;
}

unsigned long long bitset_lzcnt(struct bitset *bitset) {
  size_t bits_per_chunk = sizeof(chunk_t) * 8;
  size_t rem_bits = bitset->num_elements % bits_per_chunk;

  for (size_t i = 0; i < bitset->num_chunks; i++) {
    chunk_t chunk = bitset->chunks[bitset->num_chunks - i - 1];
    if (i == 0) {
      chunk <<= (bits_per_chunk - rem_bits);
    }

    size_t lz = (size_t)lzcnt(chunk);

    if (lz < bits_per_chunk) {
      return MIN((i * bits_per_chunk) + lz, bitset->num_elements);
    }
  }

  return bitset->num_elements;
}

unsigned long long bitset_tzcnt(struct bitset *bitset) {
  size_t bits_per_chunk = sizeof(chunk_t) * 8;

  for (size_t i = 0; i < bitset->num_chunks; i++) {
    chunk_t chunk = bitset->chunks[i];

    size_t tz = chunk ? (size_t)tzcnt(chunk) : bits_per_chunk;

    if (tz < bits_per_chunk) {
      return MIN((i * bits_per_chunk) + tz, bitset->num_elements);
    }
  }

  return bitset->num_elements;
}

bool bitset_get(struct bitset *bitset, size_t idx) {
  DEBUG_ASSERT(idx < bitset->num_elements, "bitset idx out of range");

  size_t chunk_idx = idx / (sizeof(chunk_t) * 8);
  size_t mod_idx = idx % (sizeof(chunk_t) * 8);
  chunk_t chunk = bitset->chunks[chunk_idx];

  return (chunk & ((chunk_t)1 << mod_idx)) > 0;
}

unsigned long long bitset_all(struct bitset *bitset, bool value) {
  return value ? bitset->num_set == bitset->num_elements : !bitset->num_set;
}

unsigned long long bitset_any(struct bitset *bitset, bool value) {
  return value ? bitset->num_set : bitset->num_set != bitset->num_elements;
}

void bitset_set(struct bitset *bitset, size_t idx, bool value) {
  DEBUG_ASSERT(idx < bitset->num_elements, "bitset idx out of range");

  size_t chunk_idx = idx / (sizeof(chunk_t) * 8);
  size_t mod_idx = idx % (sizeof(chunk_t) * 8);
  chunk_t *chunk = &bitset->chunks[chunk_idx];

  bool prev = (*chunk & ((chunk_t)1 << mod_idx)) > 0;

  if (value) {
    if (!prev) {
      bitset->num_set++;
    }
    *chunk |= ((chunk_t)1 << mod_idx);
  } else {
    if (prev) {
      bitset->num_set--;
    }
    *chunk &= ~((chunk_t)1 << mod_idx);
  }
}

void bitset_clear(struct bitset *bitset, bool value) {
  unsigned char mask = value ? UCHAR_MAX : 0;
  memset(bitset->chunks, mask, sizeof(chunk_t) * bitset->num_chunks);

  bitset->num_set = value ? bitset->num_elements : 0;
}

void bitset_free(struct bitset **bitset) {
  free(*bitset);

  *bitset = NULL;
}

struct bitset_iter bitset_iter(struct bitset *bitset, size_t start,
                               bool value) {
  return (struct bitset_iter){.bitset = bitset, .value = value, .idx = start};
}

bool bitset_iter_next(struct bitset_iter *iter, size_t *idx) {
  // TODO: this can be optimised
  while (iter->idx < iter->bitset->num_elements) {
    if (bitset_get(iter->bitset, iter->idx) == iter->value) {
      *idx = iter->idx++;
      return true;
    }

    iter->idx++;
  }

  return false;
}
