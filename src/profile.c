#include "profile.h"

#include "hashtbl.h"
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
#ifdef TIME_UTC
  struct timespec ts;
  invariant_assert(TIME_UTC == timespec_get(&ts, TIME_UTC),
                   "timespec_get failed");

  DEBUG_ASSERT(ts.tv_nsec > 0, "expected pos ns");

  return ts.tv_sec * 1000000000 + ts.tv_nsec;
#else
  return 0;
#endif
}
#endif

static struct vector *REGIONS = NULL;
static struct hashtbl *MULTI_REGIONS = NULL;

struct profiler_span {
  bool ended;
  unsigned long long start;
  unsigned long long end;

#ifdef PROFILE_SELF
  unsigned long long prof_start;
  unsigned long long prof_end;
#endif
};

struct profiler_multi_region_data {
  const char *name;

  struct vector *spans;
};

struct profiler_region_data {
  const char *name;

  struct profiler_span span;
};

static void profiler_init(void) {
  if (!REGIONS) {
    REGIONS = vector_create(sizeof(struct profiler_region_data));
    MULTI_REGIONS =
        hashtbl_create_str_keyed(sizeof(struct profiler_multi_region_data));

    // we have a dummy region that covers everything. makes printing subregions
    // easier
    struct profiler_region_data region = {.name = "top",
                                          .span = {
                                              .ended = false,
                                              .start = get_time(),
                                          }};
    vector_push_back(REGIONS, &region);
  }
}

struct profiler_multi_region_inst profiler_begin_multi_region(struct profiler_multi_region *multi_region) {
#ifdef PROFILE_SELF
  unsigned long long prof_start = get_time();
#endif

  profiler_init();

  struct hashtbl_entry entry;
  struct profiler_multi_region_data *region;
  if (UNLIKELY(!multi_region->entry.key)) {
    // TODO: validate no other regions exist with this name (this will overwrite)
    entry = hashtbl_lookup_entry_or_insert(MULTI_REGIONS, &multi_region->name, NULL);
    multi_region->entry = entry;

    region = entry.data;

    region->name = multi_region->name;
    region->spans = vector_create(sizeof(struct profiler_span));
  } else {
    entry = hashtbl_lookup_with_entry(MULTI_REGIONS, &multi_region->name, &multi_region->entry);
    region = entry.data;

    multi_region->entry = entry;
  }

  size_t idx = vector_length(region->spans);
  struct profiler_span *span = vector_push_back(region->spans, NULL);

  // leak in profiler is fine because it runs until end of app
  LSAN_IGNORE(span);

  unsigned long long start = get_time();

#ifdef PROFILE_SELF
  span->prof_start = prof_start;
#endif
  span->start = start;
  span->ended = false;

  return (struct profiler_multi_region_inst){.name = multi_region->name, .entry = entry, .idx = idx};
}

void profiler_end_multi_region(struct profiler_multi_region_inst region) {
  unsigned long long end = get_time();

  struct hashtbl_entry entry =
      hashtbl_lookup_with_entry(MULTI_REGIONS, &region.name, &region.entry);
  struct profiler_multi_region_data *data = entry.data;

  DEBUG_ASSERT(data, "multi-region '%s' did not exist!", (const char *)entry.key);

  struct profiler_span *span = vector_get(data->spans, region.idx);

  DEBUG_ASSERT(!span->ended, "span %zu in multi-region '%s' already ended!",
               region.idx, (const char *)entry.key);

#ifdef PROFILE_SELF
  unsigned long long prof_end = get_time();
  span->prof_end = prof_end;
#endif

  span->end = end;
  span->ended = true;
}

struct profiler_region profiler_begin_region(const char *name) {
  profiler_init();

  size_t idx = vector_length(REGIONS);
  struct profiler_region_data *region = vector_push_back(REGIONS, NULL);

  unsigned long long start = get_time();

  *region = (struct profiler_region_data){.name = name,
                                          .span = {
                                              .ended = false,
                                              .start = start,
                                          }};

  return (struct profiler_region){.idx = idx};
}

void profiler_end_region(struct profiler_region region) {
  unsigned long long end = get_time();

  struct profiler_region_data *data = vector_get(REGIONS, region.idx);

  DEBUG_ASSERT(!data->span.ended, "region '%s' already ended!", data->name);

  data->span.end = end;
  data->span.ended = true;
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

static double profiler_elapsed_nanos(struct profiler_span span) {
  return (double)span.end - (double)span.start;
}

#ifdef PROFILE_SELF
static void profiler_elapsed_self_nanos(struct profiler_span span, double *start, double *end, double *total) {
  *start = (double)span.start - (double)span.prof_start;
  *end = (double)span.prof_end - (double)span.end;
  *total = (double)span.prof_end - (double)span.prof_start;
}
#endif

static void profiler_print_region(FILE *file, size_t depth,
                                  struct profiler_region_data *region,
                                  double *region_sum) {
  double nanos = profiler_elapsed_nanos(region->span);

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

  unsigned long long parent_len = parent->span.end - parent->span.start;
  unsigned long long last_end = parent->span.start;

  bool subregion = false;
  while (*start < num_regions) {
    struct profiler_region_data *region = vector_get(REGIONS, *start);

    if (!region->span.ended) {
      (*start)++;
      continue;
    }

    if (region->span.start >= parent->span.end) {
      break;
    }

    unsigned long long gap = region->span.start - last_end;

    // only show gaps <10us
    double gap_proportion = (double)gap / parent_len;
    if (gap_proportion > 0.15) {
      fprintf(file, "%*s", (int)((depth + 2) * 4), "");
      fprintf(file, "(profiling gap ");
      print_time(file, gap);
      fprintf(file, ")\n");
    }

    sub_region_gap_sum += gap;

    last_end = region->span.end;

    subregion = true;
    profiler_print_subregions(file, region, depth + 1, &sub_region_sum, start,
                              num_regions);
  }

  if (subregion) {
    unsigned long long gap = parent->span.end - last_end;
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
      print_time(file, profiler_elapsed_nanos(parent->span));
      fprintf(file, ", with gaps of length ");
      print_time(file, sub_region_gap_sum);
      fprintf(file, ", ~%.2f%%)\n\n", gap_proportion * 100);
    }
  }
}

void profiler_print(FILE *file) {
#ifndef TIME_UTC
  warn("profiling not supported on this system (TIME_UTC was not defined)");
  return;
#endif

  if (!REGIONS || !vector_length(REGIONS)) {
    fprintf(file, "No profiling available\n");
    return;
  }

  struct profiler_region_data *top = vector_head(REGIONS);
  invariant_assert(!strcmp(top->name, "top"), "first entry has been corrupted");
  top->span.end = get_time();
  top->span.ended = true;

  fprintf(file, "\nPROFILER: ");
  fprintf(file, "\n");

  size_t num_regions = vector_length(REGIONS);
  if (!num_regions) {
    return;
  }

  for (size_t i = 0; i < num_regions;) {
    double sub_region_sum = 0;
    profiler_print_subregions(file, vector_get(REGIONS, i), 1, &sub_region_sum,
                              &i, num_regions);
    fprintf(file, "\n");
  }

  for (size_t i = 0; i < num_regions; i++) {
    struct profiler_region_data *data = vector_get(REGIONS, i);

    if (!data->span.ended) {
      warn("Unended profiler region '%s'", data->name);
    }
  }

  if (!hashtbl_size(MULTI_REGIONS)) {
    return;
  }

  fprintf(file, "\nMULTI-REGIONS:");
  fprintf(file, "\n");

#ifdef PROFILE_SELF
  double profiler_start_elapsed = 0;
  double profiler_end_elapsed = 0;
  double profiler_span_elapsed = 0;
  double profiler_total_elapsed = 0;
  size_t num_profiles = 0;
#endif

  struct hashtbl_iter *iter = hashtbl_iter(MULTI_REGIONS);
  struct hashtbl_entry entry;
  while (hashtbl_iter_next(iter, &entry)) {
    struct profiler_multi_region_data *data = entry.data;

    size_t num_spans = vector_length(data->spans);
    double elapsed = 0;
    bool warned = false;
    for (size_t i = 0; i < num_spans; i++) {
      struct profiler_span *span = vector_get(data->spans, i);

      if (!span->ended && !warned) {
        warned = true;
        warn("Unended profiler span in multi-region '%s'", data->name);
        continue;
      }

      elapsed += profiler_elapsed_nanos(*span);

      #ifdef PROFILE_SELF
        double start, end, total;
        profiler_elapsed_self_nanos(*span, &start, &end, &total);
        profiler_start_elapsed += start;
        profiler_end_elapsed += end;
        profiler_span_elapsed += profiler_elapsed_nanos(*span);
        profiler_total_elapsed += total;
        num_profiles++;
      #endif
    }

    fprintf(file, "        %s: ", data->name);
    print_time(file, elapsed);
    fprintf(file, "\n");
  }

#ifdef PROFILE_SELF
  fprintf(file, "\nPROFILER OVERHEAD:");

  double start_avg = profiler_start_elapsed / num_profiles;
  double span_avg = profiler_span_elapsed / num_profiles;
  double end_avg = profiler_end_elapsed / num_profiles;

  fprintf(file, "\n");
  fprintf(file, "        start_avg: ");
  print_time(file, start_avg);

  fprintf(file, "\n");
  fprintf(file, "        span_avg: ");
  print_time(file, span_avg);
  fprintf(file, "\n");

  fprintf(file, "        end_avg: ");
  print_time(file, end_avg);
  fprintf(file, "\n");

  double overhead_proportion = (profiler_start_elapsed + profiler_end_elapsed) / profiler_total_elapsed;

  fprintf(file, "        ");
  fprintf(file, "(total profiling time ");
  print_time(file, profiler_total_elapsed);
  fprintf(file, ", overhead ~%.2f%%)\n\n", overhead_proportion * 100);
  fprintf(file, "\n");
#endif
}
