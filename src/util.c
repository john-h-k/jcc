#include "util.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <wctype.h>

// FIXME: posix non-portable
void fprintf_errno(FILE *file, int val) {
  char buf[256];
  strerror_r(val, buf, sizeof(buf));
  fprintf(file, "errno=%s\n", buf);
}

NORETURN void unreachable(void) {
  fprintf(stderr, "`unreachable` hit, program exiting");
  EXIT_FAIL(-2);
}

PRINTF_ARGS(0) NORETURN void unsupported(const char *msg, ...) {
  FMTPRINT(stderr, "unsupported: ", msg);
  EXIT_FAIL(-2);
}

#ifndef NDEBUG
void util_debug_assert(bool b, const char *cond, const char *func,
                       const char *file, int line, const char *msg, ...) {
  if (b) {
    return;
  }

  fprintf(stderr, "DEBUG_ASSERT failed %s:%d in %s: \nexpected `%s`    ", file,
          line, func, cond);
  va_list v;
  va_start(v, msg);
  vfprintf(stderr, msg, v);
  fprintf(stderr, "\n");
  va_end(v);
  EXIT_FAIL(-1);
}
#endif

#ifdef UTIL_STACK_TRACE_IMPL

#include <errno.h>
#include <execinfo.h>
#include <unistd.h>

#define TRACE_SZ 32

#if OS_APPLE

#include <dlfcn.h>
#include <execinfo.h>
#include <mach-o/dyld.h>

void debug_print_stack_trace(void) {
  void *trace[TRACE_SZ];
  int n = backtrace(trace, TRACE_SZ);
  char **sym = backtrace_symbols(trace, n);

  fprintf(stderr, "\nSTACK TRACE (%d frames):\n", n);

  for (int i = 0; i < n; i++) {
    Dl_info info = {0};
    if (dladdr(trace[i], &info) && info.dli_fname) {
      uintptr_t slide = (uintptr_t)info.dli_fbase;

      char cmd[512];
      snprintf(cmd, sizeof(cmd), "atos -o \"%s\" -l %p %p",

               info.dli_fname, (void *)slide, trace[i]);
#undef system
      if (system(cmd) == -1) {
        fprintf(stderr, "    (atos failed: %s)\n", strerror(errno));
      }
    } else {
      fprintf(stderr, " [%2d] %p  %s\n", i, trace[i], sym[i]);
    }
  }
}

#else

void debug_print_stack_trace(void) {
  // TODO: on macOS use `atos` as `addr2line` does not exist

  void *base = NULL;

  FILE *fp = fopen("/proc/self/maps", "r");
  if (!fp) {
    fprintf(stderr, "failed to open /proc/self/maps: %s\n", strerror(errno));
    return;
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
  int size = backtrace(buffer, TRACE_SZ);

  fprintf(stderr, "\nSTACK TRACE:\n");
  for (int i = 0; i < size; i++) {
    void *relative_addr = (void *)((char *)buffer[i] - (char *)base);

    fprintf(stderr, "  [%zu] %p", (size_t)i, relative_addr);

    char command[256];
    snprintf(command, sizeof(command),
             // "addr2line -C -i -f -p -s -a -e /root/repos/jcc/build/jcc +%p",
             "addr2line -C -i -f -p -s -a -e /proc/%d/exe +%p", getpid(),
             relative_addr);

#undef system
    if (system(command) == -1) {
      fprintf(stderr, "  (failed to run addr2line: %s)\n", strerror(errno));
    }
  }
}
#endif

#endif

#define PRINT_STR(get_ch, write_str, ret, fn_prefix, literal_prefix)           \
  if (!input) {                                                                \
    write_str("(null)");                                                       \
    ret;                                                                       \
  }                                                                            \
                                                                               \
  write_str("\"");                                                             \
                                                                               \
  for (size_t i = 0; i < len;) {                                               \
    get_ch;                                                                    \
    switch (ch) {                                                              \
    case literal_prefix##'\0':                                                 \
      write_str("\\0");                                                        \
      break;                                                                   \
    case literal_prefix##'\\':                                                 \
      write_str("\\\\");                                                       \
      break;                                                                   \
    case literal_prefix##'\"':                                                 \
      write_str("\\\"");                                                       \
      break;                                                                   \
    case literal_prefix##'\n':                                                 \
      write_str("\\n");                                                        \
      break;                                                                   \
    case literal_prefix##'\t':                                                 \
      write_str("\\t");                                                        \
      break;                                                                   \
    case literal_prefix##'\r':                                                 \
      write_str("\\r");                                                        \
      break;                                                                   \
    case literal_prefix##'\b':                                                 \
      write_str("\\b");                                                        \
      break;                                                                   \
    case literal_prefix##'\f':                                                 \
      write_str("\\f");                                                        \
      break;                                                                   \
    case literal_prefix##'\v':                                                 \
      write_str("\\v");                                                        \
      break;                                                                   \
    default:                                                                   \
      if (is##fn_prefix##print(ch)) {                                          \
        write_str("%c", ((int32_t)ch < 128) ? (char)ch : '?');                 \
      } else if ((int32_t)ch <= 0xFFFF) {                                      \
        write_str("\\u%04x", (unsigned)ch);                                    \
      } else {                                                                 \
        write_str("\\U%08x", (unsigned)ch);                                    \
      }                                                                        \
      break;                                                                   \
    }                                                                          \
  }                                                                            \
                                                                               \
  write_str("\"");

// explicit len because may contain null chars
void fprint_str(FILE *file, const char *input, size_t len) {
  DEBUG_ASSERT(file, "null arg");

#define WRITE_STR(...) fprintf(file, __VA_ARGS__)
  PRINT_STR(char ch = input[i++], WRITE_STR, return, , );
#undef WRITE_STR
}

// explicit len because may contain null chars
size_t sprint_str(char *buf, size_t buf_sz, const char *input, size_t len) {
  size_t res_len = 0;
  int write;

#define WRITE_STR(...)                                                         \
  write = snprintf(buf, buf_sz, __VA_ARGS__);                                  \
  DEBUG_ASSERT(write >= 0, "snprintf call failed");                            \
  res_len += (size_t)write;                                                    \
  if (buf) {                                                                   \
    buf += write;                                                              \
    buf_sz -= (size_t)write;                                                   \
  }
  PRINT_STR(char ch = input[i++], WRITE_STR, return res_len, , );
#undef WRITE_STR

  return res_len;
}

// this takes BYTE length of string because thats the info we have easiest
// access to throughout most of frontend
void fprint_wstr(FILE *file, const uint32_t *input, size_t len) {
  DEBUG_ASSERT(file, "null arg");

#define WRITE_STR(...) fprintf(file, __VA_ARGS__)
  PRINT_STR(int32_t ch; memcpy(&ch, input + i, sizeof(ch));
            i++, WRITE_STR, return, w, L);
#undef WRITE_STR
}

bool try_parse_integer(const char *str, size_t len, unsigned long long *value) {
  if (!str || !*str) {
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
  } else if (rem >= 2 && str[i] == '0' && str[i + 1] == 'b') {
    i += 2;
    base = 2;
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

    if (ch >= '0' && ch <= '1') {
      digit = ch - '0';
    } else if (base > 2 && ch >= '0' && ch <= '7') {
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

    cur *= (unsigned long long)base;
    cur += (unsigned long long)digit;
  }

  *value = neg && cur ? ULLONG_MAX - (cur - 1) : cur;

  return true;
}

#undef PRINT_STR
