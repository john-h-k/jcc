#ifndef PROGRAM_H
#define PROGRAM_H

#include "hash.h"

#include <limits.h>
#include <stdbool.h>
#include <stddef.h>

struct program {
  const char *text;
};

struct text_pos {
  size_t idx;
  size_t line;
  size_t col;
};

struct text_span {
  struct text_pos start;
  struct text_pos end;
};

// invalid is a misnomer here. it just means it isn't actually in the file
#define TEXT_POS_INVALID_COL (SIZE_MAX)
#define TEXT_POS_INVALID_LINE (SIZE_MAX)

#define MK_INVALID_TEXT_POS(idx)                                               \
  (struct text_pos) { (idx), TEXT_POS_INVALID_LINE, TEXT_POS_INVALID_COL }
#define MK_INVALID_TEXT_SPAN(start, end)                                       \
  (struct text_span) { MK_INVALID_TEXT_POS((start)), MK_INVALID_TEXT_POS((end)) }

#define TEXT_POS_INVALID(pos) ((pos).col == TEXT_POS_INVALID_COL)

bool text_span_eq(const void *l, const void *r);
bool text_pos_eq(const void *l, const void *r);

void hash_text_pos(struct hasher *hasher, const void *value);
void hash_text_span(struct hasher *hasher, const void *value);

size_t text_pos_len(struct text_pos start, struct text_pos end);
size_t text_span_len(const struct text_span *span);

void next_col(struct text_pos *pos);

void next_line(struct text_pos *pos);

#endif
