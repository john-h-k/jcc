#ifndef ERROR_H
#define ERROR_H

#include "lex.h"

struct compiler_error {
  struct text_pos start;
  struct text_pos end;
};

#endif
