#ifndef __ERROR_H__
#define __ERROR_H__

#include "lex.h"

struct compiler_error {
  struct text_pos start;
  struct text_pos end;
};

#endif
