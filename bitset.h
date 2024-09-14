#ifndef BITSET_H
#define BITSET_H

#include <stddef.h>

struct bitset;

struct bitset *bitset_create(size_t num_elements);
bool bitset_get(struct bitset *bitset, size_t idx);
void bitset_set(struct bitset *bitset, size_t idx, bool value);

size_t bitset_tzcnt(struct bitset *bitset);

unsigned long long bitset_as_ull(struct bitset *bitset);

void bitset_clear(struct bitset *bitset, bool value);

void bitset_free(struct bitset **bitset);

#endif
