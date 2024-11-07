#ifndef BITSET_H
#define BITSET_H

#include <stdbool.h>
#include <stddef.h>

struct bitset;

struct bitset *bitset_create(size_t num_elements, bool init_value);
bool bitset_get(struct bitset *bitset, size_t idx);
void bitset_set(struct bitset *bitset, size_t idx, bool value);

size_t bitset_length(struct bitset *bitset);

struct bitset_iter {
  struct bitset *bitset;
  bool value;
  size_t idx;
};

struct bitset_iter bitset_iter(struct bitset *bitset, size_t start, bool value);

// returns `false` when finished (idx is undefined as soon as this occurs)
bool bitset_iter_next(struct bitset_iter *iter, size_t *idx);

unsigned long long bitset_all(struct bitset *bitset, bool value);
unsigned long long bitset_any(struct bitset *bitset, bool value);

unsigned long long bitset_popcnt(struct bitset *bitset);
unsigned long long bitset_lzcnt(struct bitset *bitset);
unsigned long long bitset_tzcnt(struct bitset *bitset);

unsigned long long bitset_as_ull(struct bitset *bitset);

void bitset_clear(struct bitset *bitset, bool value);

void bitset_free(struct bitset **bitset);

#endif
