#include "../util.h"

#include <ctype.h>
#include <libgen.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef __STDC_NO_ATOMICS__
#include <stdatomic.h>
#endif

// eek! not stdlib
#include <dirent.h>
#include <pthread.h>
#include <unistd.h> // for `nproc`

#define BUFSZ 4096
#define MAX_TESTS 1024

#define RESET "\033[0m"
#define BOLD "\033[1m"
#define BOLDRED "\033[1;31m"
#define BOLDGREEN "\033[1;32m"
#define BOLDYELLOW "\033[1;33m"

static void trim(char *s) {
  size_t len = strlen(s);

  if (len && s[len - 1] == '\n') {
    s[len - 1] = '\0';
  }
}

static int case_ins_prefix(const char *line, const char *prefix) {
  while (*prefix) {
    if (tolower((unsigned char)*line) != tolower((unsigned char)*prefix)) {
      return 0;
    }

    line++;
    prefix++;
  }
  return 1;
}

struct test {
  char path[BUFSZ];
  char target_arch[128];
  int expected_value;
  char stdin_in[BUFSZ];
  char stdout_expected[BUFSZ];
  int no_compile;
  bool failed;

  char *fail_reason;
};

static int run_command(const char *cmd, char *output, size_t outsz) {
  FILE *p = popen(cmd, "r");
  if (!p) {
    return -1;
  }

  size_t n = fread(output, 1, outsz - 1, p);
  output[n] = '\0';

  int status = pclose(p);

  if (WIFEXITED(status)) {
    return WEXITSTATUS(status);
  }

  return -1;
}

static char global_arch[128];
static char global_jcc_args[BUFSZ];
static const char *global_tm;

struct test_results {
  size_t total;
  size_t completed;
  size_t passed;
  size_t failed;
  size_t skipped;
  pthread_mutex_t lock;
};

static struct test_results results;

enum test_outcome { TEST_OUTCOME_PASS, TEST_OUTCOME_SKIP, TEST_OUTCOME_FAIL };

static void update_result(enum test_outcome outcome) {
  pthread_mutex_lock(&results.lock);

  results.completed++;

  switch (outcome) {
  case TEST_OUTCOME_PASS:
    results.passed++;
    break;
  case TEST_OUTCOME_SKIP:
    results.skipped++;
    break;
  case TEST_OUTCOME_FAIL:
    results.failed++;
    break;
  }

  int pad = num_digits(results.total);

  fprintf(stderr,
          BOLD "\rCompleted %zu/%zu "
               "    " BOLDGREEN "Pass: %*zu"
               "  " BOLDRED "Fail: %*zu"
               "  " BOLDYELLOW "Skip: %*zu" RESET,
          results.completed, results.total, pad, results.passed, pad,
          results.failed, pad, results.skipped);

  pthread_mutex_unlock(&results.lock);
}

PRINTF_ARGS(0) static char *alloc_sprintf(const char *format, ...) {
  va_list args, args_copy;

  va_start(args, format);
  va_copy(args_copy, args);

  int len = vsnprintf(NULL, 0, format, args_copy);

  va_end(args_copy);

  char *buf = malloc(len + 1);

  if (!buf) {
    va_end(args);
    return NULL;
  }

  vsnprintf(buf, len + 1, format, args);
  va_end(args);

  return buf;
}

static enum test_outcome run_single_test(int id, struct test *test) {
  if (strlen(test->target_arch) &&
      strcmp(test->target_arch, global_arch) != 0) {
    return TEST_OUTCOME_SKIP;
  }

  char cmd[BUFSZ], output[BUFSZ];
  int ret;

  char bin_name[BUFSZ];
  snprintf(bin_name, sizeof(bin_name), "%d.out", id);

  if (test->no_compile) {
    snprintf(cmd, sizeof(cmd),
             "./jcc %s -std=c23 -tm \"%s\" \"%s\" -o \"%s\" 2>&1",
             global_jcc_args, global_tm, test->path, bin_name);
    ret = run_command(cmd, output, sizeof(output));

    if (!ret) {
      test->fail_reason = alloc_sprintf("Expected to not compile but it did");
      return TEST_OUTCOME_FAIL;
    } else {
      return TEST_OUTCOME_PASS;
    }
  } else {
    snprintf(cmd, sizeof(cmd),
             "./jcc %s -std=c23 -tm \"%s\" \"%s\" -o \"%s\" 2>&1",
             global_jcc_args, global_tm, test->path, bin_name);

    ret = run_command(cmd, output, sizeof(output));

    if (ret != 0) {
      test->fail_reason = alloc_sprintf("compilation failed!");
      return TEST_OUTCOME_FAIL;
    }

    if (strlen(test->stdin_in)) {
      snprintf(cmd, sizeof(cmd), "echo \"%s\" | ./%s 2>&1", test->stdin_in,
               bin_name);
    } else {
      snprintf(cmd, sizeof(cmd), "./%s 2>&1", bin_name);
    }

    ret = run_command(cmd, output, sizeof(output));

    trim(output);

    if (ret != test->expected_value) {
      test->fail_reason = alloc_sprintf("expected value %d, found %d",
                                        test->expected_value, ret);

      return TEST_OUTCOME_FAIL;
    } else if (strcmp(output, test->stdout_expected) != 0) {
      test->fail_reason = alloc_sprintf("expected stdout %s, found %s",
                                        test->stdout_expected, output);
      return TEST_OUTCOME_FAIL;
    }

    return TEST_OUTCOME_PASS;
  }
}

static struct test tests[MAX_TESTS];

#ifdef __STDC_NO_ATOMICS__
static size_t num_tests = 0;
static size_t next_test = 0;

static pthread_mutex_t num_tests_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t next_test_mutex = PTHREAD_MUTEX_INITIALIZER;

static size_t get_next_test(void) {
  pthread_mutex_lock(&next_test_mutex);

  size_t val = next_test;
  next_test++;

  pthread_mutex_unlock(&next_test_mutex);
  return val;
}

static size_t inc_num_tests(void) {
  pthread_mutex_lock(&num_tests_mutex);

  size_t val = num_tests;
  num_tests++;

  pthread_mutex_unlock(&num_tests_mutex);
  return val;
}

static size_t get_num_tests(void) {
  pthread_mutex_lock(&num_tests_mutex);

  size_t val = num_tests;

  pthread_mutex_unlock(&num_tests_mutex);
  return val;
}

#else

static atomic_size_t num_tests = 0;
static atomic_size_t next_test = 0;

static size_t get_next_test(void) {
  return atomic_fetch_add_explicit(&next_test, 1, memory_order_acq_rel);
}

static size_t inc_num_tests(void) { return atomic_fetch_add(&num_tests, 1); }

static size_t get_num_tests(void) { return atomic_load(&num_tests); }

#endif

static void *worker(void *arg) {
  size_t id = *(size_t *)arg;
  free(arg);

  while (true) {
    size_t test_idx = get_next_test();

    if (test_idx >= get_num_tests()) {
      return NULL;
    }

    struct test *test = &tests[test_idx];
    enum test_outcome res = run_single_test(id, test);

    if (res == TEST_OUTCOME_FAIL) {
      test->failed = true;
    }

    update_result(res);
  }
}

int main(int argc, char **argv) {
  memset(global_jcc_args, 0, sizeof(global_jcc_args));
  for (int i = 1; i < argc; i++) {
    strcat(global_jcc_args, argv[i]);
    strcat(global_jcc_args, " ");
  }

  memset(global_arch, 0, sizeof(global_arch));
  for (int i = 1; i < argc - 1; i++) {
    if (strcmp(argv[i], "-arch") == 0) {
      strncpy(global_arch, argv[i + 1], sizeof(global_arch) - 1);
      break;
    }
  }

  if (strlen(global_arch) == 0) {
    FILE *archfp = popen("arch", "r");
    if (archfp) {
      fgets(global_arch, sizeof(global_arch), archfp);
      trim(global_arch);
      pclose(archfp);
    }
  }

  if (strstr(global_arch, "arm64")) {
    strcpy(global_arch, "aarch64");
  }

  global_tm = "Tue Dec 10 10:04:33 2024";

  char find_cmd[BUFSZ];

  snprintf(find_cmd, sizeof(find_cmd), "find \"%s\" -name '*.c' | sort",
           "../tests/tests");

  FILE *fp = popen(find_cmd, "r");
  if (!fp) {
    fprintf(stderr, BOLDRED "Failed to run find command.\n" RESET);
    exit(-1);
  }

  char file[BUFSZ];
  while (fgets(file, sizeof(file), fp)) {
    trim(file);

    if (strstr(file, "/programs/")) {
      continue;
    }

    FILE *f = fopen(file, "r");

    if (!f) {
      fprintf(stderr, BOLDRED "Could not open %s\n" RESET, file);
      exit(-1);
    }

    struct test test = {.expected_value = -1};
    strncpy(test.path, file, sizeof(test.path) - 1);

    char line[BUFSZ];
    int first_line = 1;
    while (fgets(line, sizeof(line), f)) {
      trim(line);
      if (first_line) {
        if (strcmp(line, "// no-compile") == 0) {
          test.no_compile = 1;
        }

        first_line = 0;
      }

      if (!*test.target_arch && case_ins_prefix(line, "// arch: ")) {
        sscanf(line + 9, "%127s", test.target_arch);
      } else if (test.expected_value < 0 &&
                 case_ins_prefix(line, "// expected value:")) {
        sscanf(line, "%*[^0123456789]%d", &test.expected_value);
      } else if (!*test.stdin_in && case_ins_prefix(line, "// stdin: ")) {
        strncpy(test.stdin_in, line + 10, sizeof(test.stdin_in) - 1);
      } else if (!*test.stdout_expected &&
                 case_ins_prefix(line, "// stdout: ")) {
        strncpy(test.stdout_expected, line + 11,
                sizeof(test.stdout_expected) - 1);
      }
    }

    fclose(f);

    if (test.expected_value < 0) {
      test.expected_value = 0;
    }

    tests[inc_num_tests()] = test;

    if (num_tests >= MAX_TESTS) {
      break;
    }
  }
  pclose(fp);

  results.total = get_num_tests();
  results.completed = results.passed = results.failed = results.skipped = 0;
  pthread_mutex_init(&results.lock, NULL);

  long nproc = sysconf(_SC_NPROCESSORS_ONLN);
  size_t num_workers = nproc > 0 ? (int)nproc : 1;

  pthread_t *threads = malloc(sizeof(pthread_t) * num_workers);

  for (size_t i = 0; i < num_workers; i++) {
    size_t *id = malloc(sizeof(*id));
    *id = i;
    pthread_create(&threads[i], NULL, worker, id);
  }

  for (size_t i = 0; i < num_workers; i++) {
    pthread_join(threads[i], NULL);
  }

  printf("\n" BOLD "====================\n" RESET);
  printf(BOLD "Total tests: %zu\n" RESET, results.completed);
  printf(BOLDGREEN "Passed: %zu\n" RESET, results.passed);
  printf(BOLDRED "Failed: %zu\n" RESET, results.failed);
  printf(BOLDYELLOW "Skipped: %zu\n" RESET, results.skipped);

  printf("\n");

  for (size_t i = 0; i < results.total; i++) {
    if (!tests[i].failed) {
      continue;
    }

    printf("\n" BOLDRED "Failed %s: %s" RESET "\n", tests[i].path,
           tests[i].fail_reason);

    free(tests[i].fail_reason);
  }

  printf("\n");

  free(threads);

  return results.failed != 0;
}
