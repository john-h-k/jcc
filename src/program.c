#include "program.h"

#include "util.h"

bool text_pos_eq(const void *l, const void *r) {
  const struct text_pos *l_pos = l;
  const struct text_pos *r_pos = r;

  POS_CHECK(*l_pos);
  POS_CHECK(*r_pos);

  return l_pos->idx == r_pos->idx;
}

bool text_span_eq(const void *l, const void *r) {
  const struct text_span *l_span = l;
  const struct text_span *r_span = r;

  return text_pos_eq(&l_span->start, &r_span->start) &&
         text_pos_eq(&l_span->end, &r_span->end);
}

void hash_text_pos(struct hasher *hasher, const void *value) {
  const struct text_pos *pos = value;

  POS_CHECK(*pos);

  hasher_hash_integer(hasher, pos->idx, sizeof(pos->idx));
  hasher_hash_integer(hasher, pos->line, sizeof(pos->line));
  hasher_hash_integer(hasher, pos->col, sizeof(pos->col));
}

void hash_text_span(struct hasher *hasher, const void *value) {
  const struct text_span *token = value;

  hash_text_pos(hasher, &token->start);
  hash_text_pos(hasher, &token->end);
}
