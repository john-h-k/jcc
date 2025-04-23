#ifndef PROGRAM_H
#define PROGRAM_H

#include "hash.h"
#include "util.h"

#include <limits.h>
#include <stdbool.h>
#include <stddef.h>

// TODO: kill this type
struct program {
  const char *text;
};

struct text_pos {
  const char *file;

  size_t idx;
  size_t line;
  size_t col;
};

// FIXME: You cannot always rely on zero-length spans to have valid positions
// (because they don't have a token to grab position from)
// we should fix this
struct text_span {
  struct text_pos start;
  struct text_pos end;
};

// invalid is a misnomer here. it just means it isn't actually in the file
#define TEXT_POS_INVALID_COL (SIZE_MAX)
#define TEXT_POS_INVALID_LINE (SIZE_MAX)

#define MK_TEXT_SPAN(start_pos, end_pos)                                       \
  (struct text_span) { .start = (start_pos), .end = (end_pos) }

#define MK_INVALID_TEXT_POS(idx)                                               \
  (struct text_pos) { NULL, (idx), TEXT_POS_INVALID_LINE, TEXT_POS_INVALID_COL }
#define MK_INVALID_TEXT_SPAN(start_pos, end_pos)                               \
  (struct text_span) {                                                         \
    .start = MK_INVALID_TEXT_POS((start_pos)),                                 \
    .end = MK_INVALID_TEXT_POS((end_pos))                                      \
  }

#define MK_INVALID_TEXT_SPAN2()                                                \
  MK_TEXT_SPAN(MK_INVALID_TEXT_POS(0), MK_INVALID_TEXT_POS(0))

#define TEXT_POS_INVALID(pos) ((pos).col == TEXT_POS_INVALID_COL)
#define TEXT_SPAN_INVALID(span) (TEXT_POS_INVALID((span.start)))

bool text_span_eq(const void *l, const void *r);
bool text_pos_eq(const void *l, const void *r);

void hash_text_pos(struct hasher *hasher, const void *value);
void hash_text_span(struct hasher *hasher, const void *value);

static inline size_t text_pos_len(struct text_pos start, struct text_pos end) {
  return end.idx - start.idx;
}

static inline size_t text_span_len(const struct text_span *span) {
  return text_pos_len(span->start, span->end);
}

#define POS_CHECK(p)                                                           \
  DEBUG_ASSERT((p).col != TEXT_POS_INVALID_COL, "invalid text pos")

static inline void next_col(struct text_pos *pos) {
  POS_CHECK(*pos);

  pos->idx++;
  pos->col++;
}

static inline void next_line(struct text_pos *pos) {
  POS_CHECK(*pos);

  pos->idx++;
  pos->line++;
  pos->col = 0;
}

#endif
