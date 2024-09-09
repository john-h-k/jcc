#include "vector.h"

#include "util.h"

#include <math.h>

struct vector {
  char *data;
  size_t element_size;
  size_t len;
  size_t capacity;
};

struct vector *vector_create(size_t element_size) {
  invariant_assert(element_size > 0, "`element_size` of 0 impossible for vec");

  struct vector *v = nonnull_malloc(sizeof(*v));

  v->data = NULL;
  v->element_size = element_size;
  v->len = 0;
  v->capacity = 0;

  return v;
}

void vector_expand(struct vector *v, size_t min_size) {
  size_t new_capacity = MAX(min_size, v->capacity ? v->capacity * 2 : 1);
  char *new_data = nonnull_malloc(new_capacity * v->element_size);

  memcpy(new_data, v->data, v->len * v->element_size);

  if (v->data) {
    free(v->data);
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
    memcpy(curr, prev, v->element_size);
  }

  void *head = vector_head(v);
  memcpy(head, data, v->element_size);
  return head;
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
    vector_expand(v, v->len + num_elems);
  }

  debug_assert(v->capacity >= v->len + num_elems, "vector did not expand properly");

  if (data) {
    void *end = &v->data[v->len * v->element_size];
    memcpy(end, data, v->element_size * num_elems);
  }

  v->len += num_elems;
}  

void vector_resize(struct vector *v, size_t size) {
  debug_assert(size <= v->len, "resizing too big");
  v->len = size;
}

size_t vector_length(struct vector *v) { return v->len; }

size_t vector_byte_size(struct vector *v) { return v->len * v->element_size; }

void *vector_head(struct vector *v) {
  return v->data;
}

void *vector_get(struct vector *v, size_t index) {
  debug_assert(index < v->len, "index out of bounds!");
  return &v->data[index * v->element_size];
}

void vector_copy_to(struct vector *v, void *dest) {
  memcpy(dest, v->data, v->len * v->element_size);
}

void vector_free(struct vector **v) {
  free((*v)->data);

  free(*v);
  *v = NULL;
}
