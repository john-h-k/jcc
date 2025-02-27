#ifndef PROFILE_H
#define PROFILE_H

#include <stdio.h>

// this is all done by globals so we don't need to pass around stuff
// (A) watch out for thread safety
// (B) is this bad?

#define PROFILE_BEGIN(name) \
    struct profiler_region profiler_ ## name ## _region = profiler_begin_region(# name);
#define PROFILE_END(name) \
    profiler_end_region(profiler_ ## name ## _region);

struct profiler_region {
  size_t idx;
};

struct profiler_region profiler_begin_region(const char *name);
void profiler_end_region(struct profiler_region region);

void profiler_print(FILE *file);

#endif
