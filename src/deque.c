#include "util.h"

#include "deque.h"

#include <math.h>

struct deque {
  // standard circular buffer impl

  char *data;
  size_t head;
  size_t len;

  size_t element_size;
  size_t capacity;
};

#define GET_IDX(idx) (((d)->head + (idx)) % (d)->capacity)

struct deque *deque_create(size_t element_size) {
  invariant_assert(element_size > 0, "`element_size` of 0 impossible for vec");

  struct deque *d = nonnull_malloc(sizeof(*d));

  d->data = NULL;
  d->element_size = element_size;
  d->head = 0;
  d->len = 0;
  d->capacity = 0;

  return d;
}

void deque_ensure_capacity(struct deque *d, size_t capacity) {
  if (capacity <= d->capacity) {
    return;
  }

  size_t new_capacity = MAX(capacity, d->capacity ? d->capacity * 2 : 1);
  char *new_data = nonnull_malloc(new_capacity * d->element_size);

  size_t head_cpy = MIN(d->len, d->capacity - d->head);
  memcpy(new_data, (char *)d->data + (d->element_size * d->head),
         head_cpy * d->element_size);

  if (head_cpy != d->len) {
    size_t tail_cpy = d->len - head_cpy;
    memcpy((char *)new_data + (d->element_size * head_cpy), d->data,
           tail_cpy * d->element_size);
  }

  if (d->data) {
    free(d->data);
  }

  d->data = new_data;
  d->head = 0;
  d->capacity = new_capacity;
}

bool deque_empty(struct deque *d) { return d->len == 0; }

void *deque_push_front(struct deque *d, const void *data) {
  deque_ensure_capacity(d, d->len + 1);

  if (d->head) {
    d->head--;
  } else {
    d->head = d->capacity - 1;
  }

  d->len++;
  return memcpy((char *)d->data + (d->head * d->element_size), data,
                d->element_size);
}

void *deque_pop_back(struct deque *d) {
  void *data = deque_get(d, d->len - 1);
  d->len--;

  return data;
}

void *deque_pop_front(struct deque *d) {
  void *data = deque_get(d, 0);
  d->len--;

  if (d->head + 1 == d->capacity) {
    d->head = 0;
  } else {
    d->head++;
  }

  return data;
}

TODO_FUNC(void deque_remove_at(struct deque *d, size_t index))

void deque_clear(struct deque *d) {
  d->head = 0;
  d->len = 0;
}

void deque_truncate(struct deque *d, size_t new_len) {
  DEBUG_ASSERT(new_len <= d->len, "truncating out of range");
  d->len = new_len;
}

void *deque_push_back(struct deque *d, const void *data) {
  deque_extend(d, data, 1);

  size_t idx = GET_IDX(d->len - 1);
  return &d->data[idx * d->element_size];
}

void deque_extend(struct deque *d, const void *data, size_t num_elems) {
  if (!num_elems) {
    return;
  }

  if (d->len + num_elems > d->capacity) {
    deque_ensure_capacity(d, d->len + num_elems);
  }

  DEBUG_ASSERT(d->capacity >= d->len + num_elems,
               "deque did not expand properly");

  if (data) {
    size_t end = GET_IDX(d->len - 1);

    // there is at most one discontinuity in the copy, so we split it into two

    size_t head_cpy = MIN(num_elems, d->capacity - end);
    memcpy((char *)d->data + (end * d->element_size), data,
           d->element_size * head_cpy);

    if (head_cpy != num_elems) {
      size_t tail_cpy = num_elems - head_cpy;
      memcpy(d->data, (const char *)data + (head_cpy * d->element_size),
             d->element_size * tail_cpy);
    }
  }

  d->len += num_elems;
}

void deque_resize(struct deque *d, size_t size) {
  DEBUG_ASSERT(size <= deque_length(d), "resizing too big");

  d->len = size;
}

size_t deque_length(struct deque *d) { return d->len; }

size_t deque_element_size(struct deque *d) { return d->element_size; }

size_t deque_byte_size(struct deque *d) { return d->len * d->element_size; }

void *deque_head(struct deque *d) {
  return (char *)d->data + (d->head * d->element_size);
}
void *deque_tail(struct deque *d) {
  return (char *)d->data + (GET_IDX(d->len - 1) * d->element_size);
}

void *deque_get(struct deque *d, size_t index) {
  DEBUG_ASSERT(index < d->len, "index out of bounds!");

  size_t idx = GET_IDX(index);
  return &d->data[idx * d->element_size];
}

void deque_copy_to(struct deque *d, void *dest) {
  size_t head_cpy = MIN(d->len, d->capacity - d->head);
  memcpy(dest, (char *)d->data + (d->element_size * d->head),
         head_cpy * d->element_size);

  if (head_cpy != d->len) {
    size_t tail_cpy = d->len - head_cpy;
    memcpy((char *)dest + (d->element_size * head_cpy), d->data,
           tail_cpy * d->element_size);
  }
}

void deque_free(struct deque **d) {
  free((*d)->data);

  free(*d);
  *d = NULL;
}
