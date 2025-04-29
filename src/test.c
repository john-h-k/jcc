#include "alloc.h"
#include "driver.h"
#include "fs.h"
#include "profile.h"
#include "syscmd.h"
#include "thrd.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <regex.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define DEFAULT_JCC "../build/jcc"
#define DEFAULT_TEST_DIR "../tests"
#define TM_STR "Dec 10 2025 10:04:33 2024"

struct jobq;

struct jobq *jobq_create(struct arena_allocator *arena, size_t job_size);
void jobq_free(struct jobq **jobq);
void jobq_push(struct jobq *jobq, void *job);
bool jobq_try_pop(struct jobq *jobq, size_t *id, void *job);
size_t jobq_count(struct jobq *jobq);

struct jobq {
  // NOTE: not thread safe
  struct arena_allocator *arena;
  // TODO: use deque, this is actually job stack currently
  struct vector *jobs;

  size_t jid;

  // TODO: we really want a rw_lock type
  mtx_t lock;
};

struct jobq *jobq_create(struct arena_allocator *arena, size_t job_size) {
  struct jobq *jobq = aralloc(arena, sizeof(*jobq));

  mtx_t lock;
  invariant_assert(mtx_init(&lock, mtx_plain) == thrd_success,
                   "jobq mtx_init failed");

  *jobq = (struct jobq){.arena = arena,
                        .jid = 0,
                        .jobs = vector_create_in_arena(job_size, arena),
                        .lock = lock};

  return jobq;
}

void jobq_free(struct jobq **jobq) {
  mtx_destroy(&(*jobq)->lock);
  *jobq = NULL;
}

size_t jobq_count(struct jobq *jobq) {
  size_t count;

  MTX_LOCK(&jobq->lock, { count = vector_length(jobq->jobs); });

  return count;
}

void jobq_push(struct jobq *jobq, void *job) {
  MTX_LOCK(&jobq->lock, { vector_push_back(jobq->jobs, job); });
}

bool jobq_try_pop(struct jobq *jobq, size_t *id, void *job) {
  bool found = false;

  MTX_LOCK(&jobq->lock, {
    if (vector_length(jobq->jobs)) {
      found = true;
      void *p = vector_pop(jobq->jobs);
      memcpy(job, p, vector_element_size(jobq->jobs));

      *id = jobq->jid++;
    }
  });

  return found;
}

struct jcc_test {
  size_t id;
  const char *file;
  const char *driver;
};

enum test_status { TEST_STATUS_PASS, TEST_STATUS_FAIL, TEST_STATUS_SKIP };

struct jcc_test_opts {
  size_t jobs;
  size_t num_tests;
  char *jcc;

  ustr_t arch;
  ustr_t os;

  char **paths;
  int num_paths;

  char **args;
  size_t num_args;

  char **arg_groups;
  size_t num_arg_groups;

  bool quiet;
  bool use_process;
};

PRINTF_ARGS(1) static void echo(const char *color, const char *fmt, ...) {
  va_list args;
  printf("%s", color);
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf(PR_RESET "\n");
}

static void discover_tests_file(struct arena_allocator *arena,
                                struct vector *tests, const char *path);

static void discover_tests_dir(struct arena_allocator *arena,
                               struct vector *tests, const char *dirpath) {
  DIR *dp = opendir(dirpath);
  if (!dp) {
    return;
  }

  struct dirent *entry;
  char path[1024];

  while ((entry = readdir(dp)) != NULL) {
    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
      continue;
    }

    snprintf(path, sizeof(path), "%s/%s", dirpath, entry->d_name);
    struct stat st;

    if (stat(path, &st) < 0) {
      continue;
    }

    if (S_ISDIR(st.st_mode)) {
      discover_tests_dir(arena, tests, path);
    } else {
      discover_tests_file(arena, tests, path);
    }
  }

  closedir(dp);
}

static void discover_tests_file(struct arena_allocator *arena,
                                struct vector *tests, const char *path) {
  struct stat st;

  if (stat(path, &st) < 0) {
    return;
  }

  if (S_ISREG(st.st_mode)) {
    char *dot = strrchr(path, '.');

    if (!dot || strcmp(dot, ".c") != 0) {
      return;
    }

    if (strstr(path, "programs/") || strstr(path, "_driver.c")) {
      return;
    }

    // if file path contains "langproc/", add driver file
    struct jcc_test test;
    if (strstr(path, "langproc/")) {
      int len = strlen(path);
      if (len > 2) {
        char *drv = aralloc(arena, len + 16);
        strncpy(drv, path, len - 2);
        drv[len - 2] = '\0';
        strcat(drv, "_driver.c");

        test = (struct jcc_test){.file = path, .driver = drv};
      } else {
        test = (struct jcc_test){.file = path, .driver = NULL};
      }
    } else {
      test = (struct jcc_test){.file = path, .driver = NULL};
    }

    // copy for lifetimes
    test.file = aralloc_strdup(arena, test.file);
    if (test.driver) {
      test.driver = aralloc_strdup(arena, test.driver);
    }

    vector_push_back(tests, &test);
  }
}

static void discover_tests(struct arena_allocator *arena, struct vector *tests,
                           char **paths, int count) {
  for (int i = 0; i < count; i++) {
    struct stat st;
    if (stat(paths[i], &st) < 0) {
      continue;
    }

    if (S_ISDIR(st.st_mode)) {
      discover_tests_dir(arena, tests, paths[i]);
    } else if (S_ISREG(st.st_mode)) {
      discover_tests_file(arena, tests, paths[i]);
    }
  }
}

struct jcc_test_result {
  enum test_status status;
  const char *file;
  const char *msg;
};

struct jcc_tests_status {
  mtx_t lock;
  cnd_t cond;

  struct vector *results;
  size_t num_done;
  size_t num_pass;
  size_t num_fail;
  size_t num_skip;
};

struct jcc_worker_args {
  atomic_bool *die;
  atomic_size_t *num_completed;
  struct jobq *jobq;
  struct arena_allocator *arena;
  struct jcc_tests_status *results;
  struct jcc_test_opts opts;
};

struct jcc_comp_info {
  int exc;
  char *stderr_buf;
};

static struct jcc_comp_info run_compilation(const struct jcc_worker_args *args,
                                            UNUSED const struct jcc_test *test,
                                            struct vector *comp_args) {
  if (args->opts.use_process) {
    struct syscmd *cmd = syscmd_create(args->arena, args->opts.jcc);

    size_t argc = vector_length(comp_args);
    char **argv = vector_head(comp_args);

    for (size_t i = 1; i < argc; i++) {
      syscmd_add_arg(cmd, argv[i]);
    }

    char *stderr_buf;
    syscmd_set_stderr(cmd, SYSCMD_BUF_FLAG_NONE, &stderr_buf);

    struct syscmd_exec exec =
        syscmd_timed_exec(&cmd, (struct syscmd_timeout){.secs = 30});

    switch (exec.result) {
    case SYSCMD_EXEC_RESULT_EXEC:
      return (struct jcc_comp_info){.exc = exec.exc, .stderr_buf = stderr_buf};
    case SYSCMD_EXEC_RESULT_FAILED:
      // use non-success codes here
      return (struct jcc_comp_info){.exc = 1, .stderr_buf = stderr_buf};
    case SYSCMD_EXEC_RESULT_TIMEOUT:
      return (struct jcc_comp_info){.exc = 1,
                                    .stderr_buf = "compilation timed out"};
    }

  } else {
    const char *sink_flag = "-fdiagnostics-sink";
    vector_push_back(comp_args, &sink_flag);

    const char *sink;
    FILE *f = fs_tmpfile(&sink);
    fclose(f);

    vector_push_back(comp_args, &sink);

    size_t argc = vector_length(comp_args);
    char **argv = vector_head(comp_args);

    int exc = jcc_main(argc, argv);

    char *result = NULL;
    size_t n = 0;

    FILE *file = fopen(sink, "r");
    invariant_assert(file, "open file failed");

    char buf[1024];
    while (fgets(buf, sizeof(buf), file)) {
      size_t len = strlen(buf);
      result = arrealloc(args->arena, result, n + len + 1);
      memcpy(result + n, buf, len);
      n += len;
      result[n] = '\0';
    }

    fclose(file);

    return (struct jcc_comp_info){.exc = exc, .stderr_buf = result};
  }
}

struct jcc_test_info {
  bool skip;
  ustr_t skip_msg;
  bool no_compile;

  ustr_t std;
  struct vector *flags;

  ustr_t stdin_val;

  int expected_exc;
  ustr_t expected_stdout;
};

static struct jcc_test_info get_test_info(const struct jcc_worker_args *args,
                                          const struct jcc_test *test) {
  FILE *file = fopen(test->file, "r");
  invariant_assert(file, "failed to open test '%s'", test->file);

  struct jcc_test_info info = {
      .skip = false,
      .no_compile = false,
      .stdin_val = MK_NULL_USTR(),

      .expected_exc = 0,
      .expected_stdout = MK_NULL_USTR(),
      .std = MK_USTR("c23"),
      .flags = vector_create_in_arena(sizeof(ustr_t), args->arena)};

  // if file in "/c-testsuite/", try to read .expected file
  struct vector *precompiled_stdout =
      vector_create_in_arena(sizeof(char), args->arena);
  if (strstr(test->file, "/c-testsuite/")) {
    char exp_file[1024];
    snprintf(exp_file, sizeof(exp_file), "%s.expected", test->file);
    FILE *exp = fopen(exp_file, "r");

    char val[1024];
    if (exp) {
      while (fgets(val, sizeof(val), exp)) {
        vector_extend(precompiled_stdout, val, strlen(val));
      }

      if (vector_length(precompiled_stdout) &&
          *(char *)vector_tail(precompiled_stdout) == '\n') {
        vector_truncate(precompiled_stdout,
                        vector_length(precompiled_stdout) - 1);
      }

      info.expected_stdout = (ustr_t){.str = vector_head(precompiled_stdout),
                                      .len = vector_length(precompiled_stdout)};

      fclose(exp);
    }
  }

  char line_buf[1024];

  char os_flag_buf[128];
  snprintf(os_flag_buf, sizeof(os_flag_buf), "flags-%s", OS_NAME);
  ustr_t os_flag_spec = MK_USTR(os_flag_buf);

  while (fgets(line_buf, sizeof(line_buf), file)) {
    ustr_t line = MK_USTR(line_buf);

    if (ustr_prefix(line, MK_USTR("//"))) {
      line = ustr_strip_prefix(line, MK_USTR("//"));
    } else {
      break;
    }

    line = ustr_strip_suffix(line, MK_USTR("\n"));

    ustr_t spec;
    ustr_t value;
    if (!ustr_split(line, ':', &spec, &value)) {
      spec = line;
      value = MK_NULL_USTR();
    }

    spec = ustr_trim(spec);
    value = ustr_trim(value);

    value = aralloc_ustrdup(args->arena, value);

    if (ustr_eq(spec, MK_USTR("skip"))) {
      info.skip = true;
      info.skip_msg = value;
    } else if (ustr_eq(spec, MK_USTR("no-compile"))) {
      info.no_compile = true;
    } else if (ustr_eq(spec, MK_USTR("expected value"))) {
      unsigned long long exc;
      invariant_assert(try_parse_integer(value.str, value.len, &exc),
                       "%s: invalid 'expected value' spec '%.*s'", test->file,
                       USTR_SPEC(value));
      info.expected_exc = exc;
    } else if (ustr_eq(spec, MK_USTR("stdin"))) {
      info.stdin_val = value;
    } else if (ustr_eq(spec, MK_USTR("stdout"))) {
      info.expected_stdout = value;
    } else if (ustr_eq(spec, MK_USTR("std"))) {
      info.std = value;
    } else if (ustr_eq(spec, MK_USTR("arch"))) {
      if (!ustr_eq(value, args->opts.arch)) {
        info.skip = true;
        info.skip_msg = MK_USTR(aralloc_snprintf(
            args->arena,
            "skipped due to arch (test arch: %.*s, runner arch: %.*s)",
            USTR_SPEC(value), USTR_SPEC(args->opts.arch)));
      }
    } else if (ustr_eq(spec, MK_USTR("arch-skip"))) {
      if (ustr_eq(value, args->opts.arch)) {
        info.skip = true;
        info.skip_msg = MK_USTR(aralloc_snprintf(
            args->arena, "skipped due to arch-skip (arch: %.*s)",
            USTR_SPEC(value)));
      }
    } else if (ustr_eq(spec, MK_USTR("os"))) {
      if (!ustr_eq(value, args->opts.os)) {
        info.skip = true;
        info.skip_msg = MK_USTR(aralloc_snprintf(
            args->arena, "skipped due to os (test os: %.*s, runner os: %.*s)",
            USTR_SPEC(value), USTR_SPEC(args->opts.os)));
      }
    } else if (ustr_eq(spec, MK_USTR("flags"))) {
      vector_push_back(info.flags, &value);
    } else if (ustr_prefix(spec, MK_USTR("flags-"))) {
      if (ustr_eq(spec, os_flag_spec)) {
        vector_push_back(info.flags, &value);
      }
    } else {
      BUG("%s: invalid test spec '%.*s'", test->file, USTR_SPEC(spec));
    }
  }

  fclose(file);

  return info;
}

static void add_test_result(struct jcc_worker_args *args,
                            const struct jcc_test_result *result) {
  MTX_LOCK(&args->results->lock, {
    args->results->num_done++;

    switch (result->status) {
    case TEST_STATUS_PASS:
      args->results->num_pass++;
      break;
    case TEST_STATUS_FAIL:
      args->results->num_fail++;
      break;
    case TEST_STATUS_SKIP:
      args->results->num_skip++;
      break;
    }

    vector_push_back(args->results->results, result);
    cnd_signal(&args->results->cond);
  });
}

static void run_test(struct jcc_worker_args *args, const struct jcc_test *test,
                     UNUSED const char *arggroup) {
  struct jcc_test_info info = get_test_info(args, test);

  if (info.skip) {
    add_test_result(args,
                    &(struct jcc_test_result){
                        .status = TEST_STATUS_SKIP,
                        .file = test->file,
                        .msg = aralloc_ustrconv(args->arena, info.skip_msg)});
    return;
  }

  setenv("MallocNanoZone", "0", 1);

  char output_file[128];
  snprintf(output_file, sizeof(output_file), "./test-output-%zu", test->id);

  // this is ugly

  struct vector *comp_args =
      vector_create_in_arena(sizeof(char *), args->arena);

  vector_push_back(comp_args, &args->opts.jcc);

  size_t num_flags = vector_length(info.flags);

  for (size_t i = 0; i < num_flags; i++) {
    ustr_t *flag = vector_get(info.flags, i);
    char *buf = aralloc_ustrconv(args->arena, *flag);
    vector_push_back(comp_args, &buf);
  }

  for (size_t i = 0; i < args->opts.num_args; i++) {
    vector_push_back(comp_args, &args->opts.args[i]);
  }

  const char *out = "-o";
  vector_push_back(comp_args, &out);

  const char *outp_file = output_file;
  vector_push_back(comp_args, &outp_file);

  const char *std = "-std";
  vector_push_back(comp_args, &std);

  const char *std_val = aralloc_ustrconv(args->arena, info.std);
  vector_push_back(comp_args, &std_val);

  const char *tm = "-tm";
  vector_push_back(comp_args, &tm);

  const char *tm_val = TM_STR;
  vector_push_back(comp_args, &tm_val);

  vector_push_back(comp_args, &test->file);

  if (test->driver) {
    vector_push_back(comp_args, &test->driver);
  }

  profiler_reset();
  struct jcc_comp_info comp_info = run_compilation(args, test, comp_args);

  if (info.no_compile) {
    if (comp_info.exc == 0) {
      add_test_result(args,
                      &(struct jcc_test_result){
                          .status = TEST_STATUS_FAIL,
                          .file = test->file,
                          .msg = "test marked 'no-compile' but it compiled!",
                      });
    } else {
      add_test_result(args, &(struct jcc_test_result){
                                .status = TEST_STATUS_PASS,
                                .file = test->file,
                            });
    }

    return;
  }

  if (comp_info.exc != 0) {
    add_test_result(
        args, &(struct jcc_test_result){
                  .status = TEST_STATUS_FAIL,
                  .file = test->file,
                  .msg = comp_info.stderr_buf
                             ? aralloc_snprintf(
                                   args->arena,
                                   "compilation error! build output: \n%s\n",
                                   comp_info.stderr_buf)
                             : "compilation error!"});
    return;
  }

  /* Run produced executable */
  char run_cmd[512];
  snprintf(run_cmd, sizeof(run_cmd), "%s", output_file);
  struct syscmd *cmd = syscmd_create(args->arena, run_cmd);

  if (info.stdin_val.str) {
    syscmd_set_stdin(cmd, info.stdin_val);
  }

  char *run_output;
  syscmd_set_stdout(cmd, SYSCMD_BUF_FLAG_STRIP_TRAILING_NEWLINE, &run_output);

  syscmd_set_stderr_path(cmd, SYSCMD_BUF_FLAG_NONE, "/dev/null");

  // FIXME: handle malformed executable (syscmd will throw assert i think)
  struct syscmd_exec exec =
      syscmd_timed_exec(&cmd, (struct syscmd_timeout){.secs = 30});

  switch (exec.result) {
  case SYSCMD_EXEC_RESULT_FAILED:
    add_test_result(args, &(struct jcc_test_result){
                              .status = TEST_STATUS_FAIL,
                              .file = test->file,
                              .msg = "Running process failed",
                          });
    return;
  case SYSCMD_EXEC_RESULT_TIMEOUT:
    add_test_result(args, &(struct jcc_test_result){
                              .status = TEST_STATUS_FAIL,
                              .file = test->file,
                              .msg = "Process timed out",
                          });
    return;
  case SYSCMD_EXEC_RESULT_EXEC:
    break;
  }

  int run_ret = exec.exc;
  if (run_ret != info.expected_exc) {
    add_test_result(args, &(struct jcc_test_result){
                              .status = TEST_STATUS_FAIL,
                              .file = test->file,
                              .msg = aralloc_snprintf(
                                  args->arena, "Exit code %d vs expected %d",
                                  run_ret, info.expected_exc)});
    return;
  }

  if (info.expected_stdout.str &&
      !ustr_eq(info.expected_stdout, MK_USTR(run_output))) {
    add_test_result(
        args,
        &(struct jcc_test_result){
            .status = TEST_STATUS_FAIL,
            .file = test->file,
            .msg = aralloc_snprintf(
                args->arena, "Stdout mismatch: expected \"%.*s\" got \"%s\"",
                USTR_SPEC(info.expected_stdout), run_output)});
    return;
  }

  add_test_result(args, &(struct jcc_test_result){.status = TEST_STATUS_PASS,
                                                  .file = test->file});
}

_Thread_local static bool is_worker = false;

enum worker_result { WORKER_RESULT_SUCCESS = 0, WORKER_RESULT_KILLED = 1 };

static int test_worker(void *arg) {
  is_worker = true;

  struct jcc_worker_args *args = arg;

  struct jcc_test job;
  size_t id;
  while (!atomic_load(args->die) && jobq_try_pop(args->jobq, &id, &job)) {
    job.id = id;

    for (size_t i = 0; i < args->opts.num_arg_groups; i++) {
      run_test(args, &job, args->opts.arg_groups[i]);
    }
  }

  atomic_fetch_add(args->num_completed, 1);

  return args->die ? WORKER_RESULT_KILLED : WORKER_RESULT_SUCCESS;
}

static bool parse_args(struct arena_allocator *arena, int argc, char *argv[],
                       struct jcc_test_opts *opts) {
  *opts = (struct jcc_test_opts){.jobs = /* TODO: default to nproc */ 10,
                                 .num_tests = 0,
                                 .jcc = DEFAULT_JCC,
                                 .arch = MK_USTR(ARCH_NAME),
                                 .os = MK_USTR(OS_NAME),
                                 .num_paths = 0,
                                 .paths = NULL,
                                 .num_args = 0,
                                 .args = NULL,
                                 .num_arg_groups = 0,
                                 .arg_groups = NULL};

  struct vector *paths = vector_create_in_arena(sizeof(char *), arena);
  struct vector *args = vector_create_in_arena(sizeof(char *), arena);
  struct vector *arg_groups = vector_create_in_arena(sizeof(char *), arena);

  int i = 1;
  for (; i < argc; i++) {
    if (!strcmp(argv[i], "--")) {
      vector_extend(args, &argv[i + 1], argc - i - 1);
      break;
    }

    if (strcmp(argv[i], "-j") == 0 && i + 1 < argc) {
      opts->jobs = atoi(argv[++i]);
    } else if (strcmp(argv[i], "-p") == 0) {
      opts->use_process = true;
    } else if (strcmp(argv[i], "--quiet") == 0) {
      opts->quiet = true;
    } else if (strcmp(argv[i], "-arch") == 0 && i + 1 < argc) {
      i++;
      vector_push_back(arg_groups, &argv[i]);

      opts->arch = MK_USTR(argv[i]);
    } else if (strcmp(argv[i], "-target") == 0 && i + 1 < argc) {
      i++;
      vector_push_back(arg_groups, &argv[i]);

      ustr_t target = MK_USTR(argv[i]);
      ustr_t arch;
      ustr_t rest;
      if (!ustr_split(target, '-', &arch, &rest)) {
        arch = target;
      }
      opts->arch = arch;
    } else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc) {
      opts->num_tests = atoi(argv[++i]);
    } else if (strcmp(argv[i], "--jcc") == 0 && i + 1 < argc) {
      opts->jcc = argv[++i];
    } else if (strcmp(argv[i], "--arg-group") == 0 && i + 1 < argc) {
      vector_push_back(arg_groups, &argv[++i]);
    } else if (argv[i] && argv[i][0] == '-') {
      echo(PR_BOLD PR_RED, "Unrecognised argument '%s'", argv[i]);
      return false;
    } else {
      vector_push_back(paths, &argv[i]);
    }
  }

  opts->num_args = vector_length(args);
  opts->args = vector_head(args);

  if (vector_empty(paths)) {
    opts->num_paths = 1;
    opts->paths = aralloc(arena, sizeof(char *));
    opts->paths[0] = DEFAULT_TEST_DIR;
  } else {
    opts->num_paths = vector_length(paths);
    opts->paths = vector_head(paths);
  }

  if (vector_empty(arg_groups)) {
    opts->num_arg_groups = 1;
    opts->arg_groups = aralloc(arena, sizeof(char *));
    opts->arg_groups[0] = NULL;
  } else {
    opts->num_arg_groups = vector_length(arg_groups);
    opts->arg_groups = vector_head(arg_groups);
  }

  return true;
}

static int void_strcmp(const void *l, const void *r) { return strcmp(l, r); }

// set on sigtrap
// indicates test threads should exit
static atomic_bool die = false;

static void sigint_handle(UNUSED int sig) {
  bool old = atomic_exchange(&die, true);

  if (old) {
    // multiple interrupts received, hard kill
    raise(SIGKILL);
  }
}

// FIXME: known issue with seemingly wrong TSAN "data race" warnings when using
// process mode (-p) but none of the attributes to disable them seem to work?
int main(int argc, char **argv) {
  struct arena_allocator *arena;
  arena_allocator_create("test", &arena);

  jcc_init();

  struct jcc_test_opts opts;
  if (!parse_args(arena, argc, argv, &opts)) {
    arena_allocator_free(&arena);
    return 1;
  }

  struct vector *tests = vector_create_in_arena(sizeof(struct jcc_test), arena);
  discover_tests(arena, tests, opts.paths, opts.num_paths);

  if (!opts.num_tests) {
    opts.num_tests = vector_length(tests);
  } else {
    opts.num_tests = MIN(opts.num_tests, vector_length(tests));
  }

  if (!opts.num_tests) {
    echo(PR_BOLD PR_RED, "Could not find any tests!");
    return 1;
  }

  vector_sort(tests, void_strcmp);

  struct jobq *jobq = jobq_create(arena, sizeof(struct jcc_test));

  for (size_t i = 0; i < opts.num_tests; i++) {
    jobq_push(jobq, vector_get(tests, i));
  }

  echo(PR_BOLD, "Using %zu processes...", opts.jobs);
  echo(PR_BOLD, "Found %zu tests, running %zu", vector_length(tests),
       opts.num_tests);

  struct timestmp start = get_timestamp();

  thrd_t *threads = aralloc(arena, opts.jobs * sizeof(*threads));
  struct arena_allocator **arenas = aralloc(arena, opts.jobs * sizeof(*arenas));

  struct jcc_tests_status status = {
      .results = vector_create_in_arena(sizeof(struct jcc_test_result), arena),
      .num_done = 0,
      .num_pass = 0,
      .num_fail = 0,
      .num_skip = 0};

  invariant_assert(mtx_init(&status.lock, mtx_plain) == thrd_success,
                   "results mtx_init failed");

  invariant_assert(cnd_init(&status.cond) == thrd_success,
                   "results cnd_init failed");

  // block workers from receiving SIGINT
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGINT);
  pthread_sigmask(SIG_BLOCK, &set, NULL);

  atomic_size_t num_completed = 0;

  for (size_t i = 0; i < opts.jobs; i++) {
    struct arena_allocator *worker_arena;
    arena_allocator_create("thread-worker", &worker_arena);

    arenas[i] = worker_arena;

    struct jcc_worker_args worker = {.die = &die,
                                     .num_completed = &num_completed,
                                     .jobq = jobq,
                                     .results = &status,
                                     .opts = opts,
                                     .arena = worker_arena};

    void *data = aralloc_init(worker_arena, sizeof(worker), &worker);

    invariant_assert(thrd_create(&threads[i], test_worker, data) ==
                         thrd_success,
                     "thrd_create failed");
  }

  // re-register this thread to receive them
  sigemptyset(&set);
  sigaddset(&set, SIGINT);
  pthread_sigmask(SIG_UNBLOCK, &set, NULL);

  struct sigaction sa = {.sa_handler = sigint_handle};
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  sigaction(SIGINT, &sa, NULL);

  size_t total_tests = opts.num_tests * opts.num_arg_groups;
  int pad = (int)num_digits(total_tests);

  if (!opts.quiet) {
    printf(PR_BOLD "Completed %*zu/%zu (%zu tests, %zu arg groups) "
                   "   " PR_BOLD PR_GREEN "Pass: %*zu  " PR_BOLD PR_RED
                   "Fail: %*zu  " PR_BOLD PR_YELLOW "Skip: %*zu" PR_RESET "\r",
           pad, (size_t)0, total_tests, (size_t)0, opts.num_arg_groups, pad,
           (size_t)0, pad, (size_t)0, pad, (size_t)0);
    fflush(stdout);

    while (true) {
      invariant_assert(mtx_lock(&status.lock) == thrd_success,
                       "mtx_lock failed");

      struct timespec ts = {.tv_nsec = 50000000};

      int st = cnd_timedwait(&status.cond, &status.lock, &ts);
      invariant_assert(st == thrd_success || st == thrd_timedout,
                       "cnd_wait failed");

      size_t num_done = status.num_done;
      size_t num_pass = status.num_pass;
      size_t num_fail = status.num_fail;
      size_t num_skip = status.num_skip;

      invariant_assert(mtx_unlock(&status.lock) == thrd_success,
                       "mtx_unlock failed");

      printf("\r\033[?25l");
      fflush(stdout);
      printf(PR_BOLD "Completed %*zu/%zu (%zu tests, %zu arg groups) "
                     "   " PR_BOLDGREEN "Pass: %*zu  " PR_BOLDRED
                     "Fail: %*zu  " PR_BOLDYELLOW "Skip: %*zu" PR_RESET,
             pad, num_done, total_tests, opts.num_tests, opts.num_arg_groups,
             pad, num_pass, pad, num_fail, pad, num_skip);
      printf("\033[?25h");
      fflush(stdout);

      if (atomic_load(&die) || atomic_load(&num_completed) == opts.jobs) {
        break;
      }
    }

    printf("\n");
  }

  for (size_t i = 0; i < opts.jobs; i++) {
    thrd_join(threads[i], NULL);
  }

  struct timestmp end = get_timestamp();

  int passed = 0;
  int failed = 0;
  int skipped = 0;

  struct vector *results = status.results;

  size_t num_results = vector_length(results);
  for (size_t i = 0; i < num_results; i++) {
    struct jcc_test_result *result = vector_get(results, i);

    switch (result->status) {
    case TEST_STATUS_PASS:
      passed++;
      break;
    case TEST_STATUS_FAIL:
      failed++;
      break;
    case TEST_STATUS_SKIP:
      skipped++;
      break;
    }
  }

  if (skipped > 0) {
    printf("\n");

    echo(PR_BOLD PR_YELLOW, "Skipped tests:");
    for (size_t i = 0; i < num_results; i++) {
      struct jcc_test_result *result = vector_get(results, i);

      if (result->status == TEST_STATUS_SKIP) {
        echo(PR_BOLD PR_YELLOW, "- '%s' skipped: '%s'", result->file,
             result->msg);
      }
    }
  }

  if (failed > 0) {
    printf("\n");

    echo(PR_BOLD PR_RED, "Failed tests:");
    for (size_t i = 0; i < num_results; i++) {
      struct jcc_test_result *result = vector_get(results, i);

      if (result->status == TEST_STATUS_FAIL) {
        echo(PR_BOLD PR_RED, "- '%s' failed: %s", result->file, result->msg);
      }
    }
  }

  printf("\n");
  echo(PR_BOLD PR_GREEN, "Pass: %d", passed);
  echo(PR_BOLD PR_RED, "Fail: %d", failed);
  echo(PR_BOLD PR_YELLOW, "Skip: %d", skipped);

  printf(PR_BOLD "Tests took ");
  print_time(stdout, timestamp_elapsed(start, end));
  printf(PR_RESET "\n");

  mtx_destroy(&status.lock);
  cnd_destroy(&status.cond);
  jobq_free(&jobq);

  for (size_t i = 0; i < opts.jobs; i++) {
    arena_allocator_free(&arenas[i]);
  }

  arena_allocator_free(&arena);

  return failed > 0;
}
