#ifndef VECTOR_H
#define VECTOR_H

#include "alloc.h"

#define CLONE_AND_FREE_VECTOR(arena, vec, num, p)                              \
  do {                                                                         \
    void *buf = arena_alloc((arena), vector_byte_size((vec)));                 \
    vector_copy_to((vec), buf);                                                \
    (num) = vector_length((vec));                                              \
    (p) = buf;                                                                 \
    vector_free(&(vec));                                                       \
  } while (0)

struct vector;

struct vector *vector_create(size_t element_size);

// TODO: make arena first param for consistency
struct vector *vector_create_in_arena(size_t element_size,
                                      struct arena_allocator *arena);

void vector_extend(struct vector *v, const void *data, size_t num_elems);

void vector_ensure_capacity(struct vector *v, size_t capacity);

// TODO: consider macro like this, to enforce address taking and
// prevent passing a `T*` to a vec of `T*` (because you want to pass a `T**`)
// (C23)
//   #define VECTOR_PUSH_BACK(v, data) \
//     do {\
//       typeof(data) d = data; \
//       vector_push_back(v, &d); \
//     } while (0)
//
// (C11)
//   #define VECTOR_PUSH_BACK(v, data) \
//       vector_push_back(v, &data);

void *vector_push_back(struct vector *v, const void *data);

void *vector_push_front(struct vector *v, const void *data);
bool vector_empty(struct vector *v);
size_t vector_length(struct vector *v);
size_t vector_byte_size(struct vector *v);
size_t vector_element_size(struct vector *v);

void vector_clear(struct vector *v);
void vector_truncate(struct vector *v, size_t new_len);

void vector_remove_at(struct vector *v, size_t index);
void vector_remove_range(struct vector *v, size_t index, size_t len);

void *vector_pop(struct vector *v);

void *vector_head(struct vector *v);
void *vector_tail(struct vector *v);
void *vector_get(struct vector *v, size_t index);
void *vector_revget(struct vector *v, size_t rindex);

void vector_resize(struct vector *v, size_t size);

void vector_copy_to(struct vector *v, void *dest);

void vector_to_array(struct vector *v, void **data, size_t *len);

void vector_free(struct vector **v);

#endif
