#include "program.h"

size_t text_pos_len(struct text_pos start, struct text_pos end) {
  return end.idx - start.idx;
}

size_t text_span_len(const struct text_span *span) {
  return text_pos_len(span->start, span->end);
}

void next_col(struct text_pos *pos) {
  pos->idx++;
  pos->col++;
}

void next_line(struct text_pos *pos) {
  pos->idx++;
  pos->line++;
  pos->col = 0;
}
