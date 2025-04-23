#include "vector.h"

#include "alloc.h"
#include "util.h"

struct vector {
  struct arena_allocator *arena;

  char *data;
  size_t element_size;
  size_t len;
  size_t capacity;
};

struct vector *vector_create(size_t element_size) {
  return vector_create_in_arena(element_size, NULL);
}

struct vector *vector_create_in_arena(size_t element_size,
                                      struct arena_allocator *arena) {
  invariant_assert(element_size > 0, "`element_size` of 0 impossible for vec");

  struct vector *v;
  if (arena) {
    v = arena_alloc(arena, sizeof(*v));
  } else {
    v = nonnull_malloc(sizeof(*v));
  }

  v->arena = arena;
  v->data = NULL;
  v->element_size = element_size;
  v->len = 0;
  v->capacity = 0;

  return v;
}

#define VEC_DEFAULT_SIZE 16

void vector_ensure_capacity(struct vector *v, size_t capacity) {
  if (capacity <= v->capacity) {
    return;
  }

  size_t new_capacity =
      MAX(capacity, v->capacity ? v->capacity * 2 : VEC_DEFAULT_SIZE);

  char *new_data;
  if (v->arena) {
    new_data = arena_realloc(v->arena, v->data, new_capacity * v->element_size);
  } else {
    new_data = realloc(v->data, new_capacity * v->element_size);
  }

  v->data = new_data;
  v->capacity = new_capacity;
}

bool vector_empty(struct vector *v) { return v->len == 0; }

void *vector_push_front(struct vector *v, const void *data) {
  size_t length = vector_length(v);
  vector_extend(v, NULL, 1);

  for (size_t i = length; i > 0; i--) {
    void *curr = vector_get(v, i);
    void *prev = vector_get(v, i - 1);
    memmove(curr, prev, v->element_size);
  }

  void *head = vector_head(v);
  memcpy(head, data, v->element_size);
  return head;
}

void vector_remove_at(struct vector *v, size_t index) {
  vector_remove_range(v, index, 1);
}

void vector_remove_range(struct vector *v, size_t index, size_t len) {
  DEBUG_ASSERT(index + len <= v->len, "index out of range");

  size_t tail = v->len - (index + len);

  char *start = &v->data[index * v->element_size];
  char *end = &v->data[(index + len) * v->element_size];

  memmove(start, end, tail * v->element_size);

  v->len -= len;
}

void vector_clear(struct vector *v) { v->len = 0; }

void vector_truncate(struct vector *v, size_t new_len) {
  DEBUG_ASSERT(new_len <= v->len, "truncating out of range");
  v->len = new_len;
}

void *vector_pop(struct vector *v) {
  void *data = vector_get(v, v->len - 1);
  v->len--;

  return data;
}

void *vector_push_back(struct vector *v, const void *data) {
  vector_extend(v, data, 1);

  return &v->data[(v->len - 1) * v->element_size];
}

void vector_extend(struct vector *v, const void *data, size_t num_elems) {
  if (!num_elems) {
    return;
  }

  if (v->len + num_elems > v->capacity) {
    vector_ensure_capacity(v, v->len + num_elems);
  }

  DEBUG_ASSERT(v->capacity >= v->len + num_elems,
               "vector did not expand properly");

  if (data) {
    void *end = &v->data[v->len * v->element_size];
    memcpy(end, data, v->element_size * num_elems);
  }

  v->len += num_elems;
}

void vector_resize(struct vector *v, size_t size) {
  DEBUG_ASSERT(size <= v->len, "resizing too big");
  v->len = size;
}

size_t vector_length(struct vector *v) { return v->len; }

size_t vector_element_size(struct vector *v) { return v->element_size; }

size_t vector_byte_size(struct vector *v) { return v->len * v->element_size; }

void *vector_head(struct vector *v) { return v->data; }
void *vector_tail(struct vector *v) { return vector_get(v, v->len - 1); }

void *vector_get(struct vector *v, size_t index) {
  DEBUG_ASSERT(index < v->len, "index out of bounds! (idx=%zu, len=%zu)", index,
               v->len);
  return &v->data[index * v->element_size];
}

void *vector_revget(struct vector *v, size_t rindex) {
  return vector_get(v, v->len - (1 + rindex));
}

void vector_copy_to(struct vector *v, void *dest) {
  if (v->data) {
    memcpy(dest, v->data, v->len * v->element_size);
  }
}

void vector_sort(struct vector *v, int (*comp)(const void *, const void *)) {
  qsort(vector_head(v), vector_length(v), vector_element_size(v), comp);
}

void vector_free(struct vector **v) {
  if ((*v)->arena) {
    *v = NULL;
    return;
  }

  free((*v)->data);

  free(*v);
  *v = NULL;
}
