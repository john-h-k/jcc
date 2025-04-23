#ifndef ALLOC_H
#define ALLOC_H

#include "util.h"

/* Allocator which progressively grows and is freed in one go */
struct arena_allocator;

void arena_allocator_create(const char *name,
                            struct arena_allocator **allocator);
void arena_allocator_free(struct arena_allocator **allocator);

/* Alloc word-aligned block in arena */
void *aralloc(struct arena_allocator *allocator, size_t size);

void *aralloc_init(struct arena_allocator *allocator, size_t size,
                       const void *data);

/* Alloc space necessary for `str` (including null-terminator), then copy it
   into that space. Returns the start of the new allocation */
void *aralloc_strndup(struct arena_allocator *allocator, const char *str,
                          size_t len);
void *aralloc_strdup(struct arena_allocator *allocator, const char *str);

ustr_t aralloc_ustrdup(struct arena_allocator *allocator, ustr_t str);

char *aralloc_ustrconv(struct arena_allocator *allocator, ustr_t str);

PRINTF_ARGS(1)
char *aralloc_snprintf(struct arena_allocator *allocator,
                           const char *format, ...);

/* Try and expand the allocation at `ptr` to `size`.
     - If `ptr` is `NULL`, acts the same as calling `arena_alloc(allocator,
   size)`
     - If `size` is less than the allocation size, this method does nothing */
void *arrealloc(struct arena_allocator *allocator, void *ptr, size_t size);

#endif
