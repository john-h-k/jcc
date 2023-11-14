#ifndef ALLOC_H
#define ALLOC_H

#include <stdlib.h>

// only guarantees word alignment
struct arena_allocator;

void create_arena_allocator(struct arena_allocator **allocator);
void free_arena_allocator(struct arena_allocator **allocator);

void *alloc(struct arena_allocator *allocator, size_t size);
void *alloc_strcpy(struct arena_allocator *allocator, const char *str);

#endif
