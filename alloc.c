#include "alloc.h"
#include "util.h"

#define BLOCK_SIZE 4096 * 16

struct arena;
struct arena_allocator {
  struct arena *first;
  struct arena *last;
};

struct arena {
  char *block;
  size_t size;
  size_t pos;

  struct arena *next;
};

void create_arena_allocator(struct arena_allocator **allocator) {
  struct arena_allocator value = {
    .first = NULL,
    .last = NULL
  };

  struct arena_allocator *p = nonnull_malloc(sizeof(value));
  *p = value;
  *allocator = p;
}

void free_arena_allocator(struct arena_allocator *allocator) {
  struct arena *arena = allocator->first;

  while (arena) {
    free(arena->block);
    
    arena = arena->next;
  }
  
  free(allocator);
}

bool try_alloc_in_arena(struct arena *arena, size_t size, void **allocation);
struct arena new_arena();

void* alloc_strcpy(struct arena_allocator *allocator, const char *str) {
  size_t len = strlen(str);

  char *cp = alloc(allocator, len);
  memcpy(cp, str, len * sizeof(*str));

  return cp;
}

void* alloc(struct arena_allocator *allocator, size_t size) {  
  size_t aligned = ROUND_UP(size, sizeof(size_t));

  if (aligned > BLOCK_SIZE) {
    todo("handle blocks > BLOCK_SIZE");
  }

  // the allocator is created with no arenas
  if (!allocator->first) {
    allocator->first = nonnull_malloc(sizeof(*allocator->first));
    *allocator->first = new_arena();
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
  *allocator->last->next = new_arena();
  allocator->last = allocator->last->next;

  void *allocation;
  invariant_assert(try_alloc_in_arena(allocator->last, aligned, &allocation), "allocating into new arena should be infallible");
  return allocation;
}

bool try_alloc_in_arena(struct arena *arena, size_t size, void **allocation) {
  if (arena->size - arena->pos >= size) {
    *allocation = &arena->block[arena->pos];
    arena->pos += size;
    return true;
  }

  return false;
}

struct arena new_arena() {
  struct arena arena = {
    .block = nonnull_malloc(BLOCK_SIZE),
    .pos = 0,
    .size = BLOCK_SIZE
  };

  return arena;
}
