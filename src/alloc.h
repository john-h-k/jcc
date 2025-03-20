#ifndef ALLOC_H
#define ALLOC_H

#include "util.h"

/* Allocator which progressively grows and is freed in one go */
struct arena_allocator;

void arena_allocator_create(struct arena_allocator **allocator);
void arena_allocator_free(struct arena_allocator **allocator);

/* Alloc word-aligned block in arena */
void *arena_alloc(struct arena_allocator *allocator, size_t size);

void *arena_alloc_init(struct arena_allocator *allocator, size_t size,
                       const void *data);

/* Alloc space necessary for `str` (including null-terminator), then copy it
   into that space. Returns the start of the new allocation */
void *arena_alloc_strndup(struct arena_allocator *allocator, const char *str,
                          size_t len);
void *arena_alloc_strdup(struct arena_allocator *allocator, const char *str);

PRINTF_ARGS(1)
char *arena_alloc_snprintf(struct arena_allocator *allocator,
                           const char *format, ...);

/* Try and expand the allocation at `ptr` to `size`.
     - If `ptr` is `NULL`, acts the same as calling `arena_alloc(allocator,
   size)`
     - If `size` is less than the allocation size, this method does nothing */
void *arena_realloc(struct arena_allocator *allocator, void *ptr, size_t size);

#endif
