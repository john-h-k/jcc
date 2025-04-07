#ifndef PROFILE_H
#define PROFILE_H

#include "hashtbl.h"
#include "util.h"

#include <stdio.h>

// this is all done by globals so we don't need to pass around stuff
// (A) watch out for thread safety
// (B) is this bad?

#define PROFILE_BEGIN(name)                                                    \
  struct profiler_region profiler_##name##_region =                            \
      profiler_begin_region(#name);
#define PROFILE_END(name) profiler_end_region(profiler_##name##_region);

#define PROFILE_CREATE_MULTI(nm) static struct profiler_multi_region profiler_multi_region_##nm = {.name = #nm, .entry = {0}};
#define PROFILE_BEGIN_MULTI(name)                                              \
  struct profiler_multi_region_inst profiler_multi_##name##_inst =                            \
      profiler_begin_multi_region(&profiler_multi_region_##name);
#define PROFILE_END_MULTI(name)                                                \
  profiler_end_multi_region(profiler_multi_##name##_inst);

#define PROFILE(name, code)                                                    \
  PROFILE_BEGIN(name);                                                         \
  code;                                                                        \
  PROFILE_END(name);

struct profiler_multi_region {
  const char *name;
  struct hashtbl_entry entry;
};

struct profiler_multi_region_inst {
  const char *name;
  struct hashtbl_entry entry;
  size_t idx;
};


NOINLINE struct profiler_multi_region_inst profiler_begin_multi_region(struct profiler_multi_region *multi_region);
NOINLINE void profiler_end_multi_region(struct profiler_multi_region_inst inst);

struct profiler_region {
  size_t idx;
};

NOINLINE struct profiler_region profiler_begin_region(const char *name);
NOINLINE void profiler_end_region(struct profiler_region region);

void profiler_print(FILE *file);

#endif
