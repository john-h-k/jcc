#include "profile.h"

#include "log.h"
#include "util.h"
#include "vector.h"

#include <time.h>

#if defined(__APPLE__) && __has_include(<mach/mach_time.h>)
#include <mach/mach_time.h>
static mach_timebase_info_data_t timebase_info_data;

static unsigned long long get_time(void) {
  if (!timebase_info_data.numer) {
    mach_timebase_info(&timebase_info_data);
  }

  return mach_absolute_time() * timebase_info_data.numer /
         timebase_info_data.denom;
}
#else
static unsigned long long get_time(void) {
  struct timespec ts;
  invariant_assert(TIME_UTC == timespec_get(&ts, TIME_UTC),
                   "timespec_get failed");

  DEBUG_ASSERT(ts.tv_nsec > 0, "expected pos ns");

  return ts.tv_sec * 1000000000 + ts.tv_nsec;
}
#endif

static struct vector *regions = NULL;

struct profiler_region_data {
  const char *name;

  bool ended;

  unsigned long long start;
  unsigned long long end;
};

struct profiler_region profiler_begin_region(const char *name) {
  if (!regions) {
    regions = vector_create(sizeof(struct profiler_region_data));

    // we have a dummy region that covers everything. makes printing subregions
    // easier
    struct profiler_region_data region = {
        .name = "top",
        .ended = false,
        .start = get_time(),
    };
    vector_push_back(regions, &region);
  }

  size_t idx = vector_length(regions);
  struct profiler_region_data *region = vector_push_back(regions, NULL);

  unsigned long long start = get_time();

  *region = (struct profiler_region_data){
      .name = name,
      .ended = false,
      .start = start,
  };

  return (struct profiler_region){idx};
}

void profiler_end_region(struct profiler_region region) {
  unsigned long long end = get_time();

  struct profiler_region_data *data = vector_get(regions, region.idx);

  DEBUG_ASSERT(!data->ended, "region '%s' already ended!", data->name);

  data->end = end;
  data->ended = true;
}

static void print_time(FILE *file, double nanos) {
  if (nanos >= 1e9) {
    fprintf(file, "%.3fs", nanos / 1e9);
  } else if (nanos >= 1e6) {
    fprintf(file, "%.3fms", nanos / 1e6);
  } else if (nanos >= 1e3) {
    fprintf(file, "%.3fµs", nanos / 1e3);
  } else {
    fprintf(file, "%.1fns", nanos);
  }
}

static void profiler_print_region(FILE *file, size_t depth,
                                  struct profiler_region_data *region,
                                  double *region_sum) {
  double nanos = (double)region->end - (double)region->start;

  fprintf(file, "%*s%s: ", (int)((depth + 1) * 4), "", region->name);

  if (nanos < 0.5) {
    nanos = MAX(0, nanos);
    fprintf(file, "~0ns");
  } else {
    print_time(file, nanos);
  }

  fprintf(file, "\n");
  *region_sum += nanos;
}

static void profiler_print_subregions(FILE *file,
                                      struct profiler_region_data *parent,
                                      size_t depth, double *region_sum,
                                      size_t *start, size_t num_regions) {
  profiler_print_region(file, depth, parent, region_sum);
  (*start)++;

  double sub_region_sum = 0;
  double sub_region_gap_sum = 0;

  unsigned long long parent_len = parent->end - parent->start;
  unsigned long long last_end = parent->start;

  bool subregion = false;
  while (*start < num_regions) {
    struct profiler_region_data *region = vector_get(regions, *start);

    if (!region->ended) {
      (*start)++;
      continue;
    }

    if (region->start >= parent->end) {
      break;
    }

    unsigned long long gap = region->start - last_end;

    // only show gaps <10us
    double gap_proportion = (double)gap / parent_len;
    if (gap_proportion > 0.15) {
      fprintf(file, "%*s", (int)((depth + 2) * 4), "");
      fprintf(file, "(profiling gap ");
      print_time(file, gap);
      fprintf(file, ")\n");
    }

    sub_region_gap_sum += gap;

    last_end = region->end;

    subregion = true;
    profiler_print_subregions(file, region, depth + 1, &sub_region_sum, start,
                              num_regions);
  }

  if (subregion) {
    unsigned long long gap = parent->end - last_end;
    sub_region_gap_sum += gap;

    double gap_proportion = (double)gap / parent_len;
    if (gap_proportion > 0.15) {
      // only mention gaps if they are more than 15%

      fprintf(file, "%*s", (int)((depth + 2) * 4), "");
      fprintf(file, "(profiling gap ");
      print_time(file, gap);
      fprintf(file, ")\n");

      fprintf(file, "%*s", (int)((depth + 1) * 4), "");
      fprintf(file, "(subregions of %s took ", parent->name);
      print_time(file, sub_region_sum);
      fprintf(file, ", total time was ");
      print_time(file, parent->end - parent->start);
      fprintf(file, ", with gaps of length ");
      print_time(file, sub_region_gap_sum);
      fprintf(file, ", ~%.2f%%)\n\n", gap_proportion * 100);
    }
  }
}

void profiler_print(FILE *file) {
  if (!regions || !vector_length(regions)) {
    fprintf(file, "No profiling available\n");
    return;
  }

  struct profiler_region_data *top = vector_head(regions);
  invariant_assert(!strcmp(top->name, "top"), "first entry has been corrupted");
  top->end = get_time();
  top->ended = true;

  fprintf(file, "\nPROFILER: ");
  fprintf(file, "\n");

  size_t num_regions = vector_length(regions);
  if (!num_regions) {
    return;
  }

  for (size_t i = 0; i < num_regions;) {
    double sub_region_sum = 0;
    profiler_print_subregions(file, vector_get(regions, i), 1, &sub_region_sum,
                              &i, num_regions);
    fprintf(file, "\n");
  }

  for (size_t i = 0; i < num_regions; i++) {
    struct profiler_region_data *data = vector_get(regions, i);

    if (!data->ended) {
      warn("Unended profiler region '%s'", data->name);
    }
  }
}
