#ifndef DEQUE_H
#define DEQUE_H

#include "util.h"

#include <stdlib.h>

struct deque;

struct deque *deque_create(size_t element_size);

void deque_extend(struct deque *d, const void *data, size_t num_elems);

void deque_ensure_capacity(struct deque *d, size_t capacity);

void *deque_push_front(struct deque *d, const void *data);
void *deque_push_back(struct deque *d, const void *data);

void *deque_pop_front(struct deque *d);
void *deque_pop_back(struct deque *d);

bool deque_empty(struct deque *d);

size_t deque_length(struct deque *d);
size_t deque_byte_size(struct deque *d);
size_t deque_element_size(struct deque *d);

void deque_clear(struct deque *d);
void deque_truncate(struct deque *d, size_t new_len);

void deque_remove_at(struct deque *d, size_t index);

void *deque_head(struct deque *d);
void *deque_tail(struct deque *d);

void *deque_get(struct deque *d, size_t index);

void deque_resize(struct deque *d, size_t size);

void deque_copy_to(struct deque *d, void *dest);

void deque_to_array(struct deque *d, void **data, size_t *len);

void deque_free(struct deque **d);

#endif
