#include "bitset.h"
#include "util.h"
#include <limits.h>

typedef unsigned long long chunk_t;

struct bitset {
  size_t num_elements;
  size_t num_chunks;

  chunk_t chunks[];
};

struct bitset *bitset_create(size_t num_elements) {
  size_t bits_per_chunk = sizeof(chunk_t) * 8;

  size_t num_chunks = (num_elements + (bits_per_chunk - 1)) / bits_per_chunk;

  struct bitset *bitset = nonnull_malloc(sizeof(*bitset) + sizeof(chunk_t) * num_chunks);
  bitset->num_elements = num_elements;
  bitset->num_chunks = num_chunks;

  bitset_clear(bitset, false);

  return bitset;
}

unsigned long long bitset_as_ull(struct bitset *bitset) {
  debug_assert(bitset->num_chunks <= 1, "too many chunks to get bitset as ull");

  if (bitset->num_chunks) {
    return bitset->chunks[0];
  }

  return 0;
}

size_t bitset_tzcnt(struct bitset *bitset) {
  size_t bits_per_chunk = sizeof(chunk_t) * 8;

  for (size_t i = 0; i < bitset->num_chunks; i++) {
    chunk_t chunk = bitset->chunks[i];

    size_t tz = tzcnt(chunk);

    if (tz < bits_per_chunk) {
      return MIN(i * bits_per_chunk + tz, bitset->num_elements);
    }
  }

  return bitset->num_elements;
}



bool bitset_get(struct bitset *bitset, size_t idx) {
  debug_assert(idx < bitset->num_elements, "bitset idx out of range");

  size_t chunk_idx = idx / (sizeof(chunk_t) * 8);
  size_t mod_idx = idx % (sizeof(chunk_t) * 8);
  chunk_t chunk = bitset->chunks[chunk_idx];

  return chunk & ((chunk_t)1 < mod_idx);
}

void bitset_set(struct bitset *bitset, size_t idx, bool value) {
  debug_assert(idx < bitset->num_elements, "bitset idx out of range");

  size_t chunk_idx = idx / (sizeof(chunk_t) * 8);
  size_t mod_idx = idx % (sizeof(chunk_t) * 8);
  chunk_t *chunk = &bitset->chunks[chunk_idx];

  if (value) {
    *chunk |= ((chunk_t)1 << mod_idx);
  } else {
    *chunk &= ~((chunk_t)1 << mod_idx);
  }
}


void bitset_clear(struct bitset *bitset, bool value) {
  unsigned char mask = value ? UCHAR_MAX : 0;
  memset(bitset->chunks, mask, sizeof(chunk_t) * bitset->num_chunks);
}

void bitset_free(struct bitset **bitset) {
  free(*bitset);

  *bitset = NULL;
}

