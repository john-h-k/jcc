#include "alloc.h"

#include "thrd.h"
#include "util.h"
#include "vector.h"

#include <stdlib.h>

#ifdef ALWAYS_MALLOC
#define BLOCK_SIZE (0)
#else
#define BLOCK_SIZE (4096 * 1024)
#endif

struct arena;

struct arena_allocator {
  const char *name;
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

  struct arena *succ;
  struct arena *pred;
  struct arena_allocator *allocator;
};

#define ALIGNMENT (16)

#ifdef ASAN
struct allocator_info {
  const char *name;
  void *base;
  void *end;
};

static mtx_t LOCK;
static once_flag METADATA_ONCE = ONCE_FLAG_INIT;
static struct vector *ALLOCATORS = NULL;

static void arena_asan_error(const char *report);

static void asan_metadata_init(void) {
  mtx_init(&LOCK, mtx_plain);
  ALLOCATORS = vector_create(sizeof(struct allocator_info));
  __asan_set_error_report_callback(arena_asan_error);
}

static void arena_asan_error(const char *report) {
  if (!report) {
    return;
  }

  do {
    if (!(mtx_lock((&LOCK)) == thrd_success)) {
      invariant_assert_fail("lock failed");
    }
  } while (0);
  {{const char *addr_str = strstr(report, "on address ");
  unsigned long long addr_val = 0;
  if (addr_str) {
    addr_str += strlen("on address ");
  }
  const char *addr_end = strchr(addr_str, ' ');
  if (!addr_end ||
      !try_parse_integer(addr_str, addr_end - addr_str, &addr_val)) {
    return;
  }
  void *addr = (void *)addr_val;
  size_t num_allocators = vector_length(ALLOCATORS);
  if (!ALLOCATORS || !addr) {
    return;
  }
  for (size_t i = 0; i < num_allocators; i++) {
    struct allocator_info *alloc = vector_get(ALLOCATORS, i);
    void *base = alloc->base;
    void *end = alloc->end;
    if (addr >= base && addr <= end) {
      fprintf(__stderrp,
              "\x1B[31m"
              "\033[1m"
              "\nError occurred in arena '%s'\n\n"
              "\x1B[0m",
              alloc->name);
    }
  }
}
}
;
do {
  if (!(mtx_unlock((&LOCK)) == thrd_success)) {
    invariant_assert_fail("unlock failed");
  }
} while (0);
;
}
#endif

struct arena new_arena(struct arena_allocator *allocator, size_t size);

void arena_allocator_create(const char *name,
                            struct arena_allocator **allocator) {
  struct arena_allocator value = {.name = name,
                                  .first = NULL,
                                  .last = NULL,
                                  .large_allocs =
                                      vector_create(sizeof(void *))};

  struct arena_allocator *p = nonnull_malloc(sizeof(value));
  *p = value;
  *allocator = p;

  p->last = nonnull_malloc(sizeof(*p->last));
  *p->last = new_arena(p, BLOCK_SIZE);

  p->first = p->last;

#ifdef ASAN
  call_once(&METADATA_ONCE, asan_metadata_init);
#endif
}

void arena_allocator_free(struct arena_allocator **allocator) {
  while (vector_length((*allocator)->large_allocs)) {
    void *alloc = *(void **)vector_pop((*allocator)->large_allocs);
    DEBUG_ASSERT(alloc, "expected alloc");
    free(alloc);
  }

  vector_free(&(*allocator)->large_allocs);

  struct arena *arena = (*allocator)->first;

  while (arena) {
#ifdef ASAN
    struct allocator_info info = {
        .name = (*allocator)->name,
        .base = arena->block,
        .end = arena->block + arena->size,
    };

    // TODO: not very efficient
    MTX_LOCK(&LOCK, { vector_push_back(ALLOCATORS, &info); });
#endif

    void *p = arena;

    free(arena->block);

    arena = arena->succ;

    free(p);
  }

  free(*allocator);
  *allocator = NULL;
}

bool try_alloc_in_arena(struct arena *arena, size_t size, void **allocation);

void *aralloc_strndup(struct arena_allocator *allocator, const char *str,
                      size_t len) {
  len = MIN(len, strlen(str));

  char *cp = aralloc(allocator, len + 1);
  memcpy(cp, str, len * sizeof(*str));
  cp[len] = '\0';

  return cp;
}

void *aralloc_strdup(struct arena_allocator *allocator, const char *str) {
  size_t len = strlen(str);

  char *cp = aralloc(allocator, len + 1);
  memcpy(cp, str, len * sizeof(*str));
  cp[len] = '\0';

  return cp;
}

ustr_t aralloc_ustrdup(struct arena_allocator *allocator, ustr_t str) {
  if (!str.str) {
    return str;
  }

  char *cp = aralloc(allocator, str.len);
  memcpy(cp, str.str, str.len);
  return (ustr_t){.str = cp, .len = str.len};
}

char *aralloc_ustrconv(struct arena_allocator *allocator, ustr_t str) {
  DEBUG_ASSERT(ustr_nullsafe(str), "ustr was not nullsafe!");

  if (!str.str) {
    str.str = "";
  }

  char *cp = aralloc(allocator, str.len + 1);
  memcpy(cp, str.str, str.len);
  cp[str.len] = '\0';
  return cp;
}

PRINTF_ARGS(1)
char *aralloc_snprintf(struct arena_allocator *allocator, const char *format,
                       ...) {
  if (!format || !format[0]) {
    return aralloc_strdup(allocator, "");
  }

  va_list args;
  va_list args_copy;

  va_start(args, format);
  va_copy(args_copy, args);

  int len = vsnprintf(NULL, 0, format, args_copy);

  va_end(args_copy);

  DEBUG_ASSERT(len >= 0, "vnsprintf call failed");

  char *buf = aralloc(allocator, len + 1);

  if (!buf) {
    va_end(args);
    return NULL;
  }

  vsnprintf(buf, len + 1, format, args);
  va_end(args);

  return buf;
}

void *arrealloc(struct arena_allocator *allocator, void *ptr, size_t size) {
  if (!ptr || !size) {
    return aralloc(allocator, size);
  }

  // TODO: make this actually try to not realloc
  struct alloc_metadata *metadata = ((struct alloc_metadata *)ptr) - 1;

  void *new = aralloc(allocator, size);

  // need to unpoison due to alignment bytes
#ifdef ASAN
  __asan_unpoison_memory_region(metadata, sizeof(*metadata));
  __asan_unpoison_memory_region(ptr, MIN(size, metadata->size));
#endif

  memcpy(new, ptr, MIN(size, metadata->size));

#ifdef ASAN
  __asan_poison_memory_region(metadata, sizeof(*metadata));
#endif

  return new;
}

void *aralloc_init(struct arena_allocator *allocator, size_t size,
                   const void *data) {
  void *p = aralloc(allocator, size);

  if (p && data) {
    memcpy(p, data, size);
  }

  return p;
}

COLD static void *arena_alloc_large(struct arena_allocator *allocator,
                                    size_t size) {
  // FIXME: code is old in this file and should be neatened up
  size_t adj_size = ROUND_UP(size + sizeof(struct alloc_metadata), ALIGNMENT);

  void *alloc = nonnull_malloc(adj_size);
  struct alloc_metadata *metadata = (struct alloc_metadata *)alloc;
  metadata->arena = NULL;
  metadata->size = size;

  vector_push_back(allocator->large_allocs, (const void *)&alloc);
  return metadata + 1;
}

static void *arena_alloc_in_new(struct arena_allocator *allocator,
                                size_t aligned) {
  struct arena *next;
  if (allocator->last) {
    next = allocator->last->succ;
  } else {
    next = allocator->first;
  }

  next = nonnull_malloc(sizeof(*next));
  *next = new_arena(allocator, BLOCK_SIZE);

  if (allocator->last) {
    allocator->last->succ = next;
  }

  allocator->last = next;

  void *allocation;
  invariant_assert(
      try_alloc_in_arena(allocator->last, aligned, &allocation),
      "allocating into new arena should be infallible (%zu bytes requested)",
      aligned);

  return allocation;
}

void *aralloc(struct arena_allocator *allocator, size_t size) {
  if (!size) {
    return 0;
  }

  size_t aligned = ROUND_UP(size, ALIGNMENT);

  struct arena *arena;

  if (UNLIKELY(aligned > BLOCK_SIZE)) {
    return arena_alloc_large(allocator, size);
  } else {
    arena = allocator->last;

    void *allocation;
    if (LIKELY(try_alloc_in_arena(arena, aligned, &allocation))) {
      return allocation;
    }
  }

  // need to create a new arena
  return arena_alloc_in_new(allocator, aligned);
}

bool try_alloc_in_arena(struct arena *arena, size_t size, void **allocation) {
  size_t adj_size = ROUND_UP(size + sizeof(struct alloc_metadata), ALIGNMENT);

  if (arena->size - arena->pos < adj_size) {
    return false;
  }

  *allocation = &arena->block[arena->pos];

#ifdef ASAN
  __asan_unpoison_memory_region(*allocation, sizeof(struct alloc_metadata));
#endif
  struct alloc_metadata *metadata = (struct alloc_metadata *)*allocation;
  metadata->arena = arena;
  metadata->size = size;
#ifdef ASAN
  __asan_poison_memory_region(*allocation, sizeof(struct alloc_metadata));
#endif

  *allocation = metadata + 1;

  arena->pos += adj_size;

#ifdef ASAN
  __asan_unpoison_memory_region(*allocation, size);
#endif

  return true;
}

struct arena new_arena(struct arena_allocator *allocator, size_t size) {
  struct arena arena = {.allocator = allocator,
                        .pred = allocator->last,
                        .succ = NULL,
                        .block = nonnull_malloc(size),
                        .pos = 0,
                        .size = size};

#ifdef ASAN
  __asan_poison_memory_region(arena.block, arena.size);
#endif

  return arena;
}
