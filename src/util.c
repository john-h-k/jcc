#include "util.h"

#include <stdint.h>
#include <ctype.h>
#include <limits.h>
#include <wctype.h>

NORETURN void unreachable(void) {
  fprintf(stderr, "`unreachable` hit, program exiting");
  EXIT_FAIL(-2);
}

PRINTF_ARGS(0) NORETURN void unsupported(const char *msg, ...) {
  FMTPRINT(stderr, "unsupported: ", msg);
  EXIT_FAIL(-2);
}

void util_debug_assert(bool b, const char *cond, const char *func,
                              const char *file, int line, const char *msg,
                              ...) {
  if (!b) {
    fprintf(stderr, "DEBUG_ASSERT failed %s:%d in %s: \nexpected `%s`    ", file,
            line, func, cond);

    va_list v;
    va_start(v, msg);
    vfprintf(stderr, msg, v);
    fprintf(stderr, "\n");
    va_end(v);
    EXIT_FAIL(-1);
  }
}

#ifdef UTIL_STACK_TRACE_IMPL

#include <errno.h>
#include <execinfo.h>
#include <unistd.h>

#define TRACE_SZ 32

void debug_print_stack_trace(void) {
  void *base = NULL;

  FILE *fp = fopen("/proc/self/maps", "r");
  if (!fp) {
    fprintf(stderr, "failed to open /proc/self/maps: %s\n", strerror(errno));
  } else {

    char line[256];

    if (fgets(line, sizeof(line), fp)) {
      char *endptr;
      base = (void *)strtoul(line, &endptr, 16);
    }

    fclose(fp);
  }

  if (!base) {
    fprintf(stderr, "failed to determine executable base address\n");
    return;
  }

  void *buffer[TRACE_SZ];
  size_t size = backtrace(buffer, TRACE_SZ);

  fprintf(stderr, "\nSTACK TRACE:\n");
  for (size_t i = 0; i < size; i++) {
    void *relative_addr = (void *)((char *)buffer[i] - (char *)base);

    fprintf(stderr, "  [%zu] %p", i, relative_addr);

    char command[256];
    snprintf(command, sizeof(command),
             // "addr2line -C -i -f -p -s -a -e /root/repos/jcc/build/jcc +%p",
             "addr2line -C -i -f -p -s -a -e /proc/%d/exe +%p", getpid(),
             relative_addr);

    if (system(command) == -1) {
      fprintf(stderr, "  (failed to run addr2line: %s)\n", strerror(errno));
    }
  }
}
#endif

#define PRINT_STR(get_ch, fn_prefix, literal_prefix) \
  DEBUG_ASSERT(file, "null arg"); \
   \
  if (!input) { \
    fprintf(file, "(null)"); \
    return; \
  } \
   \
  fprintf(file, # literal_prefix "\""); \
   \
  for (size_t i = 0; i < len;) { \
    get_ch; \
    switch (ch) { \
    case literal_prefix ## '\0': \
      fprintf(file, "\\0"); \
      break; \
    case literal_prefix ## '\\': \
      fprintf(file, "\\\\"); \
      break; \
    case literal_prefix ## '\"': \
      fprintf(file, "\\\""); \
      break; \
    case literal_prefix ## '\n': \
      fprintf(file, "\\n"); \
      break; \
    case literal_prefix ## '\t': \
      fprintf(file, "\\t"); \
      break; \
    case literal_prefix ## '\r': \
      fprintf(file, "\\r"); \
      break; \
    case literal_prefix ## '\b': \
      fprintf(file, "\\b"); \
      break; \
    case literal_prefix ## '\f': \
      fprintf(file, "\\f"); \
      break; \
    case literal_prefix ## '\v': \
      fprintf(file, "\\v"); \
      break; \
    default: \
      if (is ## fn_prefix ## print(ch)) { \
        fputc(((int32_t)ch < 128) ? (char)ch : '?', file); \
      } else if ((int32_t)ch <= 0xFFFF) { \
        fprintf(file, "\\u%04x", (unsigned)ch); \
      } else { \
        fprintf(file, "\\U%08x", (unsigned)ch); \
      } \
      break; \
    } \
  } \
   \
  fprintf(file, "\"");



// explicit len because may contain null chars
void fprint_str(FILE *file, const char *input, size_t len) {
  PRINT_STR(char ch = input[i++],,);
}

// this takes BYTE length of string because thats the info we have easiest access to throughout most of frontend
void fprint_wstr(FILE *file, const char *input, size_t len) {
  DEBUG_ASSERT(len % 4 == 0, "expected len to be mod 4 for wstr");

  PRINT_STR(int32_t ch; memcpy(&ch, input + i, sizeof(ch)); i += sizeof(ch),w,L);
}

bool try_parse_integer(const char *str, size_t len, unsigned long long *value) {
  if (!*str) {
    return false;
  }

  size_t i = 0;

  bool neg = false;
  switch (*str) {
    case '+':
      i++;
      break;
    case '-':
      neg = true;
      i++;
      break;
    default:
      break;
  }

  if (i == len) {
    return false;
  }

  size_t rem = len - i - 1;
  int base = 10;
  if (rem >= 2 && str[i] == '0' && str[i + 1] == 'x') {
    i += 2;
    base = 16;
  } else if (rem >= 1 && str[i] == '0') {
    i++;
    base = 8;
  }

  unsigned long long cur = 0;
  for (; i < len; i++) {
    char digit;

    char ch = str[i];
    // FIXME: should only allow as last chars
    if (tolower(ch) == 'l' || tolower(ch) == 'u') {
      break;
    }

    if (ch >= '0' && ch <= '7') {
      digit = ch - '0';
    } else if (base > 8 && ch >= '8' && ch <= '9') {
      digit = ch - '0';
    } else if (base > 10) {
      switch (tolower(ch)) {
        case 'a':
          digit = 10;
          break;
        case 'b':
          digit = 11;
          break;
        case 'c':
          digit = 12;
          break;
        case 'd':
          digit = 13;
          break;
        case 'e':
          digit = 14;
          break;
        case 'f':
          digit = 15;
          break;
        default:
          return false;
      }
    } else {
      return false;
    }

    cur *= base;
    cur += digit;
  }

  *value = neg && cur ? ULLONG_MAX - (cur  - 1) : cur;
  return true;
}

#undef PRINT_STR
