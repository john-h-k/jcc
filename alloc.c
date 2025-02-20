#include "alloc.h"

#include "codegen.h"
#include "log.h"
#include "util.h"
#include "vector.h"
#include <stdlib.h>

// TODO: allow larger allocs
#define BLOCK_SIZE (4096 * 32)

struct arena;

struct arena_allocator {
  struct arena *first;
  struct arena *last;

  struct vector *large_allocs;
};

// TODO: use this metadata to walk on free

struct alloc_metadata {
  struct arena *arena;
  // the size NOT including this metadata
  size_t size;
};

struct arena {
  char *block;
  size_t size;
  size_t pos;

  struct arena *next;
  struct arena_allocator *allocator;
};

void arena_allocator_create(struct arena_allocator **allocator) {
  struct arena_allocator value = {.first = NULL, .last = NULL, .large_allocs = vector_create(sizeof(void *)) };

  struct arena_allocator *p = nonnull_malloc(sizeof(value));
  *p = value;
  *allocator = p;
}

void arena_allocator_free(struct arena_allocator **allocator) {
  while (vector_length((*allocator)->large_allocs)) {
    void *alloc = *(void **)vector_pop((*allocator)->large_allocs);
    DEBUG_ASSERT(alloc, "expected alloc");
    free(alloc);
  }

  struct arena *arena = (*allocator)->first;

  while (arena) {
    free(arena->block);

    arena = arena->next;
  }

  free(*allocator);
  *allocator = NULL;
}

bool try_alloc_in_arena(struct arena *arena, size_t size, void **allocation);
struct arena new_arena(struct arena_allocator *allocator, size_t size);

void *arena_alloc_strcpy(struct arena_allocator *allocator, const char *str) {
  size_t len = strlen(str);

  char *cp = arena_alloc(allocator, len + 1);
  memcpy(cp, str, len * sizeof(*str));
  cp[len] = '\0';

  return cp;
}

PRINTF_ARGS(1) char *arena_alloc_snprintf(struct arena_allocator *allocator,
                           const char *format, ...) {
  va_list args, args_copy;

  va_start(args, format);
  va_copy(args_copy, args);

  int len = vsnprintf(NULL, 0, format, args_copy);

  va_end(args_copy);

  DEBUG_ASSERT(len > 0, "vnsprintf call failed");

  char *buf = arena_alloc(allocator, len + 1);

  if (!buf) {
    va_end(args);
    return NULL;
  }

  vsnprintf(buf, len + 1, format, args);
  va_end(args);

  return buf;
}

void *arena_realloc(struct arena_allocator *allocator, void *ptr, size_t size) {
  if (!ptr) {
    return arena_alloc(allocator, size);
  }

  // TODO: make this actually try to not realloc
  struct alloc_metadata *metadata = ((struct alloc_metadata *)ptr) - 1;
  void *new = arena_alloc(allocator, size);

  memcpy(new, ptr, metadata->size);
  return new;
}

void *arena_alloc_init(struct arena_allocator *allocator, size_t size,
                       void *data) {
  void *p = arena_alloc(allocator, size);

  memcpy(p, data, size);

  return p;
}

void *arena_alloc(struct arena_allocator *allocator, size_t size) {
  if (!size) {
    return 0;
  }

  size_t aligned = ROUND_UP(size, sizeof(void *));

  struct arena *arena;
  size_t next_arena_size;

  if (aligned > BLOCK_SIZE) {
    void *alloc = nonnull_malloc(aligned);
    vector_push_back(allocator->large_allocs, &alloc);
    return alloc;
  } else {
    arena = allocator->first;
    next_arena_size = BLOCK_SIZE;

    // FIXME: not _that_ efficient as walks all the allocations
    while (arena) {
      void *allocation;
      if (try_alloc_in_arena(arena, aligned, &allocation)) {
        return allocation;
      }

      arena = arena->next;
    }
  }

  // need to create a new arena
  struct arena **next;
  if (allocator->last) {
    next = &allocator->last->next;
  } else {
    next = &allocator->last;
  }

  *next = nonnull_malloc(sizeof(*allocator->last->next));
  **next = new_arena(allocator, next_arena_size);
  allocator->last = *next;

  void *allocation;
  invariant_assert(
      try_alloc_in_arena(allocator->last, aligned, &allocation),
      "allocating into new arena should be infallible (%zu bytes requested)",
      aligned);
  return allocation;
}

bool try_alloc_in_arena(struct arena *arena, size_t size, void **allocation) {
  size_t adj_size = size + sizeof(struct alloc_metadata);

  if (arena->size - arena->pos < adj_size) {
    return false;
  }

  *allocation = &arena->block[arena->pos];

  struct alloc_metadata *metadata = (struct alloc_metadata *)*allocation;
  metadata->arena = arena;
  metadata->size = size;

  *allocation = metadata + 1;

  arena->pos += adj_size;
  return true;
}

struct arena new_arena(struct arena_allocator *allocator, size_t size) {
  struct arena arena = {.allocator = allocator,
                        .next = NULL,
                        .block = nonnull_malloc(size),
                        .pos = 0,
                        .size = size};

  return arena;
}
