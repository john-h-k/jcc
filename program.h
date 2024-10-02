#ifndef PROGRAM_H
#define PROGRAM_H

#include <stddef.h>

struct program {
  const char *text;
};

struct preprocessed_program {
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

int text_pos_len(struct text_pos start, struct text_pos end);
int text_span_len(const struct text_span *span);

void next_col(struct text_pos *pos);

void next_line(struct text_pos *pos);

#endif
