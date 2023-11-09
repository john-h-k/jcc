#ifndef __LINKER_H__
#define __LINKER_H__

#include <stdlib.h>
#include "alloc.h"

struct link_args {
  const char **objects;
  size_t num_objects;  

  const char *output;
};

enum link_result {
  LINK_RESULT_SUCCESS,
  LINK_RESULT_FAILURE
};

enum link_result link_objects(const struct link_args *args);

#endif
