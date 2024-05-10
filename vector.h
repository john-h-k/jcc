#ifndef VECTOR_H
#define VECTOR_H

#include <stdlib.h>

struct vector;

struct vector *vector_create(size_t element_size);

void vector_extend(struct vector *v, const void *data, size_t num_elems);

void *vector_push_back(struct vector *v, const void *data);
bool vector_empty(struct vector *v);
size_t vector_length(struct vector *v);
size_t vector_byte_size(struct vector *v);

void *vector_head(struct vector *v);
void *vector_get(struct vector *v, size_t index);

void vector_resize(struct vector *v, size_t size);

void vector_copy_to(struct vector *v, void *dest);

void vector_to_array(struct vector *v, void **data, size_t *len);

void vector_free(struct vector **v);

#endif
