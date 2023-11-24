#include "alloc.h"

#include "log.h"
#include "util.h"

#define BLOCK_SIZE 4096 * 16

struct arena;

struct arena_allocator {
  struct arena *first;
  struct arena *last;
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

void create_arena_allocator(struct arena_allocator **allocator) {
  struct arena_allocator value = {.first = NULL, .last = NULL};

  struct arena_allocator *p = nonnull_malloc(sizeof(value));
  *p = value;
  *allocator = p;
}

void free_arena_allocator(struct arena_allocator **allocator) {
  struct arena *arena = (*allocator)->first;

  while (arena) {
    free(arena->block);

    arena = arena->next;
  }

  free(*allocator);
  *allocator = NULL;
}

bool try_alloc_in_arena(struct arena *arena, size_t size, void **allocation);
struct arena new_arena(struct arena_allocator *allocator);

void *arena_alloc_strcpy(struct arena_allocator *allocator, const char *str) {
  size_t len = strlen(str);

  char *cp = arena_alloc(allocator, len);
  memcpy(cp, str, len * sizeof(*str));

  return cp;
}

void *arena_realloc(struct arena_allocator *allocator, void *ptr, size_t size) {
  if (!ptr) {
    return arena_alloc(allocator, size);
  }

  // TODO: make this actually try to not realloc
  struct alloc_metadata *metadata = ((struct alloc_metadata *)ptr) - 1;
  void *new = arena_alloc(allocator, size);

  debug("realloc copying %zu", metadata->size);
  memcpy(new, ptr, metadata->size);
  return new;
}

void *arena_alloc(struct arena_allocator *allocator, size_t size) {
  size_t aligned = ROUND_UP(size, sizeof(size_t));

  if (aligned > BLOCK_SIZE) {
    todo("handle blocks > BLOCK_SIZE");
  }

  // the allocator is created with no arenas
  if (!allocator->first) {
    allocator->first = nonnull_malloc(sizeof(*allocator->first));
    *allocator->first = new_arena(allocator);
    allocator->last = allocator->first;
  }

  struct arena *arena = allocator->first;

  while (arena) {
    void *allocation;
    if (try_alloc_in_arena(arena, aligned, &allocation)) {
      return allocation;
    }

    arena = arena->next;
  }

  // need to create a new arena
  allocator->last->next = nonnull_malloc(sizeof(*allocator->last->next));
  *allocator->last->next = new_arena(allocator);
  allocator->last = allocator->last->next;

  void *allocation;
  invariant_assert(try_alloc_in_arena(allocator->last, aligned, &allocation),
                   "allocating into new arena should be infallible");
  return allocation;
}

bool try_alloc_in_arena(struct arena *arena, size_t size, void **allocation) {
  size_t adj_size = size + sizeof(struct alloc_metadata);

  if (arena->size - arena->pos >= adj_size) {
    *allocation = &arena->block[arena->pos];

    struct alloc_metadata *metadata = (struct alloc_metadata *)*allocation;
    metadata->arena = arena;
    metadata->size = size;

    *allocation = metadata + 1;

    arena->pos += adj_size;
    return true;
  }

  return false;
}

struct arena new_arena(struct arena_allocator *allocator) {
  struct arena arena = {.allocator = allocator,
                        .next = NULL,
                        .block = nonnull_malloc(BLOCK_SIZE),
                        .pos = 0,
                        .size = BLOCK_SIZE};

  return arena;
}
