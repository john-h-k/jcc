#ifndef LOG_H
#define LOG_H

#include "util.h"

#include <stdarg.h>
#include <stdio.h>

#define DECL_LOG_FN(NAME)                                                      \
  bool NAME##_enabled(void);                                                   \
  void NAME##_nl(void);                                                        \
  PRINTF_ARGS(0) void NAME(const char *format, ...);                           \
  PRINTF_ARGS(0) void NAME##sl(const char *format, ...);                       \
  PRINTF_ARGS(1) void f##NAME(FILE *file, const char *format, ...);            \
  PRINTF_ARGS(1) void f##NAME##sl(FILE *file, const char *format, ...);

void enable_log(void);
void disable_log(void);

bool log_enabled(void);

#ifndef __JCC__
DECL_LOG_FN(err)
DECL_LOG_FN(warn)
DECL_LOG_FN(info)
DECL_LOG_FN(debug)
DECL_LOG_FN(trace)

DECL_LOG_FN(slog)
#else

#define err_nl(...)                                                            \
  do {                                                                         \
    fprintf(stderr, "\n");                                                     \
  } while (0)

#define err(...)                                                               \
  do {                                                                         \
    fprintf(stderr, PR_RED PR_BOLD "ERROR: " PR_RESET PR_BOLD);                \
    fprintf(stderr, __VA_ARGS__);                                              \
    fprintf(stderr, PR_RESET "\n");                                            \
  } while (0)

#define errsl(...)                                                             \
  do {                                                                         \
    fprintf(stderr, PR_RED PR_BOLD "ERROR: " PR_RESET PR_BOLD);                \
    fprintf(stderr, __VA_ARGS__);                                              \
    fprintf(stderr, PR_RESET);                                                 \
  } while (0)

#define warn_nl(...)                                                           \
  do {                                                                         \
    fprintf(stderr, "\n");                                                     \
  } while (0)

#define warn(...)                                                              \
  do {                                                                         \
    fprintf(stderr, PR_YELLOW PR_BOLD "WARNING: " PR_RESET PR_BOLD);           \
    fprintf(stderr, __VA_ARGS__);                                              \
    fprintf(stderr, PR_RESET "\n");                                            \
  } while (0)

#define warnsl(...)                                                            \
  do {                                                                         \
    fprintf(stderr, PR_YELLOW PR_BOLD "WARNING: " PR_RESET PR_BOLD);           \
    fprintf(stderr, __VA_ARGS__);                                              \
    fprintf(stderr, PR_RESET);                                                 \
  } while (0)

#define info(...)                                                              \
  do {                                                                         \
    if (!log_enabled()) {                                                      \
      break;                                                                   \
    }                                                                          \
    fprintf(stderr, PR_GREEN PR_BOLD "INFO: " PR_RESET PR_BOLD);               \
    fprintf(stderr, __VA_ARGS__);                                              \
    fprintf(stderr, PR_RESET "\n");                                            \
  } while (0)

#define debug_enabled() log_enabled()
#define debug_nl()                                                             \
  do {                                                                         \
    if (!log_enabled()) {                                                      \
      break;                                                                   \
    }                                                                          \
    fprintf(stderr, "\n");                                                     \
  } while (0)

#define debug(...)                                                             \
  do {                                                                         \
    if (!log_enabled()) {                                                      \
      break;                                                                   \
    }                                                                          \
    fprintf(stderr, PR_WHITE PR_BOLD "DEBUG: " PR_RESET PR_BOLD);              \
    fprintf(stderr, __VA_ARGS__);                                              \
    fprintf(stderr, PR_RESET "\n");                                            \
  } while (0)

#define tracenl()                                                              \
  do {                                                                         \
    if (!log_enabled()) {                                                      \
      break;                                                                   \
    }                                                                          \
    fprintf(stderr, "\n");                                                     \
  } while (0)

#define trace(...)                                                             \
  do {                                                                         \
    if (!log_enabled()) {                                                      \
      break;                                                                   \
    }                                                                          \
    fprintf(stderr, PR_WHITE PR_BOLD "TRACE: " PR_RESET __VA_ARGS__);          \
    fprintf(stderr, PR_RESET "\n");                                            \
  } while (0)

#define slog(...)                                                              \
  do {                                                                         \
    fprintf(stderr, __VA_ARGS__);                                              \
    fprintf(stderr, "\n");                                                     \
  } while (0)

#define slogsl(...)                                                            \
  do {                                                                         \
    fprintf(stderr, __VA_ARGS__);                                              \
  } while (0)
#endif

#undef DECL_LOG_FN

#define BEGIN_STAGE(name)                                                      \
  slog("\n\n****************************** " name                              \
       " ******************************\n")

#define _DBG_FORMAT_STR(val, specifier) #val ": " specifier "\n"
#define _GENERIC_DBG_FORMAT_SPECIFIER(val)                                     \
  _Generic((val),                                                              \
      char *: _DBG_FORMAT_STR(val, "%s"),                                      \
      int: _DBG_FORMAT_STR(val, "%d"),                                         \
      size_t: _DBG_FORMAT_STR(val, "%zu"),                                     \
      float: _DBG_FORMAT_STR(val, "%f"),                                       \
      double: _DBG_FORMAT_STR(val, "%lf"),                                     \
      default: _DBG_FORMAT_STR(val, "unknown type for `DEBUG` macro"))

#define DEBUG(val) fprintf(stderr, _GENERIC_DBG_FORMAT_SPECIFIER(val), (val))

#endif
