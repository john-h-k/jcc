#ifndef PROFILE_H
#define PROFILE_H

#include "hashtbl.h"
#include "util.h"

#include <stdio.h>

// ********** PROFILER **********
// * Regions are non re-entrant parts of code used for profiling "sections" e.g linking
//   - `PROFILE_BEGIN(ident)` and `PROFILE_END(ident)`
//   - `ident` must be an actual identifier token, not a string (as it uses it to construct variables)
// * Multi-regions are used for profiling sections that are executed many times (e.g individual functions) 
//   - `PROFILE_CREATE_MULTI(ident)` creates the metadata needed
//   - `PROFILE_BEGIN_MULTI(ident)` and `PROFILE_END_MULTI(ident)` are used to enter and exit a multi-region
// * The macro `PROFILE_SELF` can be defined to get the profiler to show overhead information about the profiling itself
//   - Profiling times will _not_ include this overhead, but profiling sections containing it will
// ******************************

// this is all done by globals so we don't need to pass around stuff
// (A) watch out for thread safety
// (B) is this bad?


#define NO_PROFILE

#ifdef NO_PROFILE
#define PROFILE_BEGIN(name)
#define PROFILE_END(name) 

#define PROFILE_CREATE_MULTI(nm)
#define PROFILE_BEGIN_MULTI(name)
#define PROFILE_END_MULTI(name)

#define PROFILE(name, code) code

#else
#define PROFILE_BEGIN(name)                                                    \
  struct profiler_region profiler_##name##_region =                            \
      profiler_begin_region(#name)
#define PROFILE_END(name) profiler_end_region(profiler_##name##_region)

#define PROFILE_CREATE_MULTI(nm) static struct profiler_multi_region profiler_multi_region_##nm = {.name = #nm, .entry = {0}}
#define PROFILE_BEGIN_MULTI(name)                                              \
  struct profiler_multi_region_inst profiler_multi_##name##_inst =                            \
      profiler_begin_multi_region(&profiler_multi_region_##name)
#define PROFILE_END_MULTI(name)                                                \
  profiler_end_multi_region(profiler_multi_##name##_inst)

#define PROFILE(name, code)                                                    \
  PROFILE_BEGIN(name);                                                         \
  code;                                                                        \
  PROFILE_END(name);
#endif

struct profiler_multi_region {
  const char *name;
  struct hashtbl_entry entry;
  size_t ver;
};

struct profiler_multi_region_inst {
  const char *name;
  struct hashtbl_entry entry;
  size_t idx;
};

// not required to be called, but ensures more accurate measurement if it is
void profiler_init(void);
void profiler_reset(void);

NOINLINE struct profiler_multi_region_inst profiler_begin_multi_region(struct profiler_multi_region *multi_region);
NOINLINE void profiler_end_multi_region(struct profiler_multi_region_inst inst);

struct profiler_region {
  size_t idx;
};

NOINLINE struct profiler_region profiler_begin_region(const char *name);
NOINLINE void profiler_end_region(struct profiler_region region);

void profiler_print_text(FILE *file);
void profiler_print_json(FILE *file);

#endif
