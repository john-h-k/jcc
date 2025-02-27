#include "profile.h"

#include "log.h"
#include "util.h"
#include "vector.h"

#include <time.h>

static struct vector *regions = NULL;

struct profiler_region_data {
  const char *name;

  bool ended;

  struct timespec start;
  struct timespec end;
};

struct profiler_region profiler_begin_region(const char *name) {
  if (!regions) {
    regions = vector_create(sizeof(struct profiler_region_data));
  }

  struct timespec start;
  invariant_assert(TIME_UTC == timespec_get(&start, TIME_UTC),
                   "timespec_get failed");

  struct profiler_region_data region = {
      .name = name,
      .ended = false,
      .start = start,
  };

  size_t idx = vector_length(regions);
  vector_push_back(regions, &region);
  return (struct profiler_region){idx};
}

void profiler_end_region(struct profiler_region region) {
  struct profiler_region_data *data = vector_get(regions, region.idx);

  DEBUG_ASSERT(!data->ended, "region'%s' already ended!", data->name);

  struct timespec end;
  invariant_assert(TIME_UTC == timespec_get(&end, TIME_UTC),
                   "timespec_get failed");

  data->end = end;
  data->ended = true;
}

static double get_time(struct timespec start, struct timespec end) {
  long secs = end.tv_sec - start.tv_sec;
  long nanos = end.tv_nsec - start.tv_nsec;

  if (nanos < 0) {
    nanos += 1000000000;
    secs--;
  }

  return secs * 1e9 + nanos;
}

static void profiler_print_region(FILE *file,
                                  size_t depth,
                                  struct profiler_region_data *region) {
  double nanos = get_time(region->start, region->end);

  if (nanos < 1) {
    return;
  }
  
  fprintf(file, "%*s%s: ", (int)((depth + 1) * 4), "", region->name);

  if (nanos >= 1e9) {
    fprintf(stderr, "%.3fs\n", nanos / 1e9);
  } else if (nanos >= 1e6) {
    fprintf(stderr, "%.3fms\n", nanos / 1e6);
  } else if (nanos >= 1e3) {
    fprintf(stderr, "%.3fµs\n", nanos / 1e3);
  } else {
    fprintf(stderr, "%.0fns\n", nanos);
  }
}

static void profiler_print_subregions(FILE *file,
                                      struct profiler_region_data *parent,
                                      size_t depth, size_t *start,
                                      size_t num_regions) {
  profiler_print_region(file, depth, parent);
  (*start)++;

  bool subregion = false;
  while (*start < num_regions) {
    struct profiler_region_data *region = vector_get(regions, *start);

    if (!region->ended) {
      (*start)++;
      continue;
    }

    if (region->start.tv_sec > parent->end.tv_sec ||
        (region->start.tv_sec == parent->end.tv_sec &&
         region->start.tv_nsec >= parent->end.tv_nsec)) {
      return;
    }

    subregion = true;
    profiler_print_subregions(file, region, depth + 1, start, num_regions);
  }

  if (subregion) {
    fprintf(file, "\n");
  }

  fprintf(file, "\n");
}

void profiler_print(FILE *file) {
  fprintf(file, "\nPROFILER: ");
  fprintf(file, "\n");

  size_t num_regions = vector_length(regions);
  if (!num_regions) {
    return;
  }

  for (size_t i = 0; i < num_regions;) {
    profiler_print_subregions(file, vector_get(regions, i), 1, &i, num_regions);
    fprintf(file, "\n");
  }

  for (size_t i = 0; i < num_regions; i++) {
    struct profiler_region_data *data = vector_get(regions, i);

    if (!data->ended) {
      warn("Unended profiler region '%s'", data->name);
    }
  }
}
