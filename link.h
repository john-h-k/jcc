#ifndef LINKER_H
#define LINKER_H

#include "alloc.h"

#include <stdlib.h>

struct link_args {
  const char *const *objects;
  size_t num_objects;

  const char *output;
};

enum link_result { LINK_RESULT_SUCCESS, LINK_RESULT_FAILURE };

enum link_result link_objects(const struct link_args *args);

#endif
