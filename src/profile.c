#include "profile.h"

#include "hashtbl.h"
#include "json.h"
#include "log.h"
#include "thrd.h"
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

#ifdef MSAN
  // msan does not properly recognise this function as initing it
  __msan_unpoison(&ts, sizeof(ts));
#endif

  invariant_assert(TIME_UTC == timespec_get(&ts, TIME_UTC),
                   "timespec_get failed");

  DEBUG_ASSERT(ts.tv_nsec > 0, "expected pos ns");

  return ts.tv_sec * 1000000000 + ts.tv_nsec;
#else
  return 0;
#endif
}
#endif

struct timestmp get_timestamp(void) {
  return (struct timestmp){ .val = get_time() };
}

double timestamp_elapsed(struct timestmp start, struct timestmp end) {
  return (double)end.val - (double)start.val;
}

static once_flag lock_once = ONCE_FLAG_INIT;
static mtx_t lock;

static struct vector *REGIONS = NULL;
static struct hashtbl *MULTI_REGIONS = NULL;
static size_t VER = 0;

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

static void lock_init(void) { mtx_init(&lock, mtx_plain); }

void profiler_init(void) {
  // is this safe?
  if (REGIONS) {
    return;
  }

  call_once(&lock_once, lock_init);

  MTX_LOCK(&lock, {
    if (!REGIONS) {
      REGIONS = vector_create(sizeof(struct profiler_region_data));
      MULTI_REGIONS =
          hashtbl_create_str_keyed(sizeof(struct profiler_multi_region_data));

      // we have a dummy region that covers everything. makes printing
      // subregions easier
      struct profiler_region_data region = {.name = "top",
                                            .span = {
                                                .ended = false,
                                                .start = get_time(),
                                            }};
      vector_push_back(REGIONS, &region);
    }
  });
}

void profiler_reset(void) {
  call_once(&lock_once, lock_init);

  MTX_LOCK(&lock, {
    VER++;

    if (REGIONS) {
      vector_free(&REGIONS);
      hashtbl_free(&MULTI_REGIONS);
    }
  });

  profiler_init();
}

struct profiler_multi_region_inst
profiler_begin_multi_region(struct profiler_multi_region *multi_region) {
#ifdef PROFILE_SELF

  unsigned long long prof_start = get_time();
#endif

  profiler_init();

  if (UNLIKELY(multi_region->ver < VER)) {
    multi_region->entry = (struct hashtbl_entry){0};
    multi_region->ver = VER;
  }

  struct hashtbl_entry entry;
  struct profiler_multi_region_data *region;
  if (UNLIKELY(!multi_region->entry.key)) {
    // TODO: validate no other regions exist with this name (this will
    // overwrite)
    entry = hashtbl_lookup_entry_or_insert(MULTI_REGIONS, &multi_region->name,
                                           NULL);
    multi_region->entry = entry;

    region = entry.data;

    region->name = multi_region->name;
    region->spans = vector_create(sizeof(struct profiler_span));
  } else {
    entry = hashtbl_lookup_with_entry(MULTI_REGIONS, &multi_region->name,
                                      &multi_region->entry);
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

  return (struct profiler_multi_region_inst){
      .name = multi_region->name, .entry = entry, .idx = idx};
}

void profiler_end_multi_region(struct profiler_multi_region_inst region) {
  unsigned long long end = get_time();

  struct hashtbl_entry entry =
      hashtbl_lookup_with_entry(MULTI_REGIONS, &region.name, &region.entry);
  struct profiler_multi_region_data *data = entry.data;

  DEBUG_ASSERT(data, "multi-region '%s' did not exist!",
               (const char *)entry.key);

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

void print_time(FILE *file, double nanos) {
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
static void profiler_elapsed_self_nanos(struct profiler_span span,
                                        double *start, double *end,
                                        double *total) {
  *start = (double)span.start - (double)span.prof_start;
  *end = (double)span.prof_end - (double)span.end;
  *total = (double)span.prof_end - (double)span.prof_start;
}
#endif

typedef void (*profiler_cb)(void *metadata);
typedef void (*profiler_cb_begin)(void *metadata, size_t len);

typedef void (*profiler_cb_region)(void *metadata,
                                   const struct profiler_region_data *region);
typedef void (*profiler_cb_gap)(void *metadata, double gap,
                                double gap_proportion);

struct multi_region_info {
  const char *name;
  double elapsed;
};

typedef void (*profiler_cb_multi_region)(void *metadata,
                                         struct multi_region_info region);

struct parent_gap_info {
  const char *name;
  double parent_len;
  double gap;
  double gap_proportion;
  double sub_region_sum;
  double sub_region_gap_sum;
};

typedef void (*profiler_cb_parent_gap)(void *metadata,
                                       struct parent_gap_info info);

struct overhead_info {
  double start_avg;
  double span_avg;
  double end_avg;

  double total_elapsed;
  double overhead_proportion;
};
typedef void (*profiler_cb_overhead)(void *metadata, struct overhead_info info);

struct profiler_print_cb {
  profiler_cb begin;
  profiler_cb end;

  profiler_cb_begin begin_tree;
  profiler_cb end_tree;

  profiler_cb_begin begin_multi;
  profiler_cb end_multi;

  profiler_cb begin_subregions;
  profiler_cb end_subregions;

  profiler_cb begin_region;
  profiler_cb end_region;

  profiler_cb_region region;
  profiler_cb_multi_region multi_region;
  profiler_cb_gap gap;
  profiler_cb_parent_gap parent_gap;

  profiler_cb_overhead overhead;
};

#define CBZ(name)                                                              \
  do {                                                                         \
    if (cb.name) {                                                             \
      cb.name(metadata);                                                       \
    }                                                                          \
  } while (0)

#define CB(name, ...)                                                          \
  do {                                                                         \
    if (cb.name) {                                                             \
      cb.name(metadata, __VA_ARGS__);                                          \
    }                                                                          \
  } while (0)

static void profiler_print_subregions(struct profiler_print_cb cb,
                                      void *metadata, double gap,
                                      double gap_proportion,
                                      const struct profiler_region_data *parent,
                                      double *region_sum, size_t *start,
                                      size_t num_regions) {
  CBZ(begin_region);
  if (gap != 0.0) {
    CB(gap, gap, gap_proportion);
  }
  CB(region, parent);
  (*start)++;

  double parent_elapsed = profiler_elapsed_nanos(parent->span);
  *region_sum += parent_elapsed;

  double sub_region_sum = 0;
  double sub_region_gap_sum = 0;

  unsigned long long parent_len = parent->span.end - parent->span.start;
  unsigned long long last_end = parent->span.start;

  CBZ(begin_subregions);
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

    unsigned long long sub_gap = region->span.start - last_end;

    // only show gaps <10us
    double sub_gap_proportion = gap / parent_len;

    sub_region_gap_sum += gap;

    last_end = region->span.end;

    subregion = true;
    profiler_print_subregions(cb, metadata, sub_gap, sub_gap_proportion, region,
                              &sub_region_sum, start, num_regions);
  }
  CBZ(end_subregions);

  if (subregion) {
    unsigned long long total_gap = parent->span.end - last_end;
    sub_region_gap_sum += gap;

    double total_gap_proportion = gap / parent_len;

    struct parent_gap_info info = {
        .name = parent->name,
        .parent_len = parent_elapsed,
        .sub_region_sum = sub_region_sum,
        .sub_region_gap_sum = sub_region_gap_sum,
        .gap = total_gap,
        .gap_proportion = total_gap_proportion,
    };

    CB(parent_gap, info);
  }

  CBZ(end_region);
}

static void profiler_print_generic(struct profiler_print_cb cb,
                                   void *metadata) {
#ifndef TIME_UTC
  warn("profiling not supported on this system (TIME_UTC was not defined)");
  return;
#endif

  CBZ(begin);

  {
    struct profiler_region_data *top = vector_head(REGIONS);
    invariant_assert(!strcmp(top->name, "top"),
                     "first entry has been corrupted");

    if (!top->span.ended) {
      top->span.end = get_time();
      top->span.ended = true;
    }

    size_t num_regions = vector_length(REGIONS);

    CB(begin_tree, num_regions);

    for (size_t i = 0; i < num_regions;) {
      double sub_region_sum = 0;
      profiler_print_subregions(cb, metadata, 0, 0, vector_get(REGIONS, i),
                                &sub_region_sum, &i, num_regions);
    }

    for (size_t i = 0; i < num_regions; i++) {
      const struct profiler_region_data *data = vector_get(REGIONS, i);

      if (!data->span.ended) {
        warn("Unended profiler region '%s'", data->name);
      }
    }

    CBZ(end_tree);
  }

#ifdef PROFILE_SELF
  double profiler_start_elapsed = 0;
  double profiler_end_elapsed = 0;
  double profiler_span_elapsed = 0;
  double profiler_total_elapsed = 0;
  size_t num_profiles = 0;
#endif

  {
    CB(begin_multi, hashtbl_size(MULTI_REGIONS));

    struct hashtbl_iter *iter = hashtbl_iter(MULTI_REGIONS);
    struct hashtbl_entry entry;
    while (hashtbl_iter_next(iter, &entry)) {
      const struct profiler_multi_region_data *data = entry.data;

      size_t num_spans = vector_length(data->spans);
      double elapsed = 0;
      bool warned = false;
      for (size_t i = 0; i < num_spans; i++) {
        const struct profiler_span *span = vector_get(data->spans, i);

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

      struct multi_region_info info = {.name = data->name, .elapsed = elapsed};
      CB(multi_region, info);
    }

    CBZ(end_multi);
  }

#ifdef PROFILE_SELF

  double start_avg = profiler_start_elapsed / num_profiles;
  double span_avg = profiler_span_elapsed / num_profiles;
  double end_avg = profiler_end_elapsed / num_profiles;

  double overhead_proportion =
      (profiler_start_elapsed + profiler_end_elapsed) / profiler_total_elapsed;

  struct overhead_info info = {.start_avg = start_avg,
                               .span_avg = span_avg,
                               .end_avg = end_avg,

                               .total_elapsed = profiler_total_elapsed,
                               .overhead_proportion = overhead_proportion};

  CB(overhead, info);
#endif

  CBZ(end);
}

struct profiler_print_data {
  FILE *file;
  int depth;
  bool any;
};

static void profiler_print_text_begin_tree(void *metadata, size_t len) {
  struct profiler_print_data *data = metadata;

  if (!len) {
    return;
  }

  data->any = true;
  fprintf(data->file, "\n    REGIONS:");
  fprintf(data->file, "\n");
}

static void profiler_print_text_begin_multi(void *metadata, size_t len) {
  struct profiler_print_data *data = metadata;

  if (!len) {
    return;
  }

  data->any = true;
  fprintf(data->file, "\n    MULTI-REGIONS:");
  fprintf(data->file, "\n");
}

static void profiler_print_text_begin_subregions(void *metadata) {
  struct profiler_print_data *data = metadata;
  data->depth++;
}

static void profiler_print_text_end_subregions(void *metadata) {
  struct profiler_print_data *data = metadata;
  data->depth--;
}

static void
profiler_print_text_region(void *metadata,
                           const struct profiler_region_data *region) {
  struct profiler_print_data *data = metadata;
  FILE *file = data->file;
  fprintf(file, "%*s%s: ", ((data->depth + 1) * 4), "", region->name);

  double nanos = profiler_elapsed_nanos(region->span);
  if (nanos < 0.5) {
    nanos = MAX(0, nanos);
    fprintf(file, "~0ns");
  } else {
    print_time(file, nanos);
  }

  fprintf(file, "\n");
}

static void profiler_print_text_gap(void *metadata, double gap,
                                    double gap_proportion) {
  struct profiler_print_data *data = metadata;
  FILE *file = data->file;

  if (gap_proportion > 0.15) {
    fprintf(file, "%*s", ((data->depth + 2) * 4), "");
    fprintf(file, "(profiling gap ");
    print_time(file, gap);
    fprintf(file, ")\n");
  }
}

static void profiler_print_text_parent_gap(void *metadata,
                                           struct parent_gap_info info) {
  struct profiler_print_data *data = metadata;
  FILE *file = data->file;

  if (info.gap_proportion > 0.15) {
    // only mention gaps if they are more than 15%

    fprintf(file, "%*s", ((data->depth + 2) * 4), "");
    fprintf(file, "(profiling gap ");
    print_time(file, info.gap);
    fprintf(file, ")\n");

    fprintf(file, "%*s", ((data->depth + 1) * 4), "");
    fprintf(file, "(subregions of %s took ", info.name);
    print_time(file, info.sub_region_sum);
    fprintf(file, ", total time was ");
    print_time(file, info.parent_len);
    fprintf(file, ", with gaps of length ");
    print_time(file, info.sub_region_gap_sum);
    fprintf(file, ", ~%.2f%%)\n\n", info.gap_proportion * 100);
  }
}

static void profiler_print_text_multi_region(void *metadata,
                                             struct multi_region_info info) {
  struct profiler_print_data *data = metadata;
  FILE *file = data->file;

  fprintf(file, "        %s: ", info.name);
  print_time(file, info.elapsed);
  fprintf(file, "\n");
}

static void profiler_print_text_overhead(void *metadata,
                                         struct overhead_info info) {
  struct profiler_print_data *data = metadata;
  FILE *file = data->file;

  fprintf(file, "\nPROFILER OVERHEAD:");

  fprintf(file, "\n");
  fprintf(file, "        start_avg: ");
  print_time(file, info.start_avg);

  fprintf(file, "\n");
  fprintf(file, "        span_avg: ");
  print_time(file, info.span_avg);
  fprintf(file, "\n");

  fprintf(file, "        end_avg: ");
  print_time(file, info.end_avg);
  fprintf(file, "\n");

  fprintf(file, "        ");
  fprintf(file, "(total profiling time ");
  print_time(file, info.total_elapsed);
  fprintf(file, ", overhead ~%.2f%%)\n\n", info.overhead_proportion * 100);
  fprintf(file, "\n");
}

void profiler_print_text(FILE *file) {
  struct profiler_print_cb cb = {
      .begin_tree = profiler_print_text_begin_tree,

      .begin_multi = profiler_print_text_begin_multi,

      .begin_subregions = profiler_print_text_begin_subregions,
      .end_subregions = profiler_print_text_end_subregions,

      .gap = profiler_print_text_gap,
      .parent_gap = profiler_print_text_parent_gap,
      .region = profiler_print_text_region,
      .multi_region = profiler_print_text_multi_region,
      .overhead = profiler_print_text_overhead};

  struct profiler_print_data data = {.file = file, .depth = 0, .any = false};

  fprintf(file, "\nPROFILER: ");

  profiler_print_generic(cb, &data);

  if (!data.any) {
    fprintf(file, "No profiling available\n");
  }
}

struct profiler_json_data {
  struct json_writer *writer;
};

static void profiler_print_json_begin_tree(void *metadata, UNUSED size_t len) {
  struct profiler_json_data *data = metadata;

  JSON_WRITE_FIELD_NAME(data->writer, "regions");
  json_writer_write_begin_arr(data->writer);
}

static void profiler_print_json_end_tree(void *metadata) {
  struct profiler_json_data *data = metadata;
  json_writer_write_end_arr(data->writer);
}

static void profiler_print_json_begin_multi(void *metadata, UNUSED size_t len) {
  struct profiler_json_data *data = metadata;

  JSON_WRITE_FIELD_NAME(data->writer, "multi-regions");
  json_writer_write_begin_arr(data->writer);
}

static void profiler_print_json_end_multi(void *metadata) {
  struct profiler_json_data *data = metadata;
  json_writer_write_end_arr(data->writer);
}

static void profiler_print_json_begin_subregions(void *metadata) {
  struct profiler_json_data *data = metadata;

  JSON_WRITE_FIELD_NAME(data->writer, "subregions");
  json_writer_write_begin_arr(data->writer);
}

static void profiler_print_json_end_subregions(void *metadata) {
  struct profiler_json_data *data = metadata;
  json_writer_write_end_arr(data->writer);
}

static void profiler_print_json_begin_region(void *metadata) {
  struct profiler_json_data *data = metadata;

  json_writer_write_begin_obj(data->writer);
}

static void profiler_print_json_end_region(void *metadata) {
  struct profiler_json_data *data = metadata;
  json_writer_write_end_obj(data->writer);
}

static void
profiler_print_json_region(void *metadata,
                           const struct profiler_region_data *region) {
  struct profiler_json_data *data = metadata;

  double nanos = profiler_elapsed_nanos(region->span);
  if (nanos < 0.5) {
    nanos = MAX(0, nanos);
  }

  JSON_WRITE_FIELD(data->writer, "name", MK_USTR(region->name));
  JSON_WRITE_FIELD(data->writer, "elapsed", nanos);
}

static void profiler_print_json_gap(void *metadata, double gap,
                                    UNUSED double gap_proportion) {
  struct profiler_json_data *data = metadata;

  JSON_WRITE_FIELD(data->writer, "start_gap", gap);
}

static void profiler_print_json_parent_gap(void *metadata,
                                           struct parent_gap_info info) {
  struct profiler_json_data *data = metadata;

  // TODO: other fields
  JSON_WRITE_FIELD(data->writer, "gap", info.gap);
}

static void profiler_print_json_multi_region(void *metadata,
                                             struct multi_region_info info) {
  struct profiler_json_data *data = metadata;

  JSON_OBJECT(data->writer, NULL, {
    JSON_WRITE_FIELD(data->writer, "name", MK_USTR(info.name));
    JSON_WRITE_FIELD(data->writer, "elapsed", info.elapsed);
  });
}

static void profiler_print_json_overhead(void *metadata,
                                         struct overhead_info info) {
  struct profiler_json_data *data = metadata;

  JSON_OBJECT(data->writer, "overhead", {
    JSON_WRITE_FIELD(data->writer, "start_avg", info.start_avg);
    JSON_WRITE_FIELD(data->writer, "span_avg", info.span_avg);
    JSON_WRITE_FIELD(data->writer, "end_avg", info.end_avg);
    JSON_WRITE_FIELD(data->writer, "total", info.total_elapsed);
    JSON_WRITE_FIELD(data->writer, "proportion", info.overhead_proportion);
  });
}

void profiler_print_json(FILE *file) {
  struct profiler_print_cb cb = {
      .begin_tree = profiler_print_json_begin_tree,
      .end_tree = profiler_print_json_end_tree,

      .begin_multi = profiler_print_json_begin_multi,
      .end_multi = profiler_print_json_end_multi,

      .begin_subregions = profiler_print_json_begin_subregions,
      .end_subregions = profiler_print_json_end_subregions,

      .begin_region = profiler_print_json_begin_region,
      .end_region = profiler_print_json_end_region,

      .gap = profiler_print_json_gap,
      .parent_gap = profiler_print_json_parent_gap,
      .region = profiler_print_json_region,
      .multi_region = profiler_print_json_multi_region,
      .overhead = profiler_print_json_overhead};

  struct json_writer *writer = json_writer_create();
  struct profiler_json_data data = {.writer = writer};

  JSON_OBJECT(writer, NULL, { profiler_print_generic(cb, &data); });

  ustr_t json = json_writer_get_buf(writer);
  fprintf(file, "%.*s", (int)json.len, json.str);
  fflush(file);

  json_writer_free(&writer);
}
