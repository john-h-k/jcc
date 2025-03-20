#include "log.h"

static bool LOG_ENABLED = false;

void enable_log(void) { LOG_ENABLED = true; }

void disable_log(void) { LOG_ENABLED = false; }

bool log_enabled(void) { return LOG_ENABLED; }

#ifdef NLOG
#define DEF_LOG_FN(NAME, _PREFIX, force)                                       \
  bool NAME##_enabled(void) { return false; }                                  \
  void NAME##_nl(void) { (void)format; }                                       \
  void NAME(const char *format, ...) { (void)format; }                         \
  void NAME##sl(const char *format, ...) { (void)format; }                     \
  void f##NAME(const char *format, ...) { (void)format; }                      \
  void f##NAME##sl(const char *format, ...) { (void)format; }
#else

#ifndef __JCC__
// define them as macros so we don't need to use va_list
#define DEF_LOG_FN(NAME, PREFIX, force)                                        \
  bool NAME##_enabled(void) { return LOG_ENABLED; }                            \
  void NAME##_nl(void) {                                                       \
    if (!force && !LOG_ENABLED) {                                              \
      return;                                                                  \
    }                                                                          \
    fprintf(stderr, "\n");                                                     \
  }                                                                            \
  void NAME(const char *format, ...) {                                         \
    if (!force && !LOG_ENABLED) {                                              \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(stderr, PREFIX PR_BOLD PR_RESET);                                  \
    vfprintf(stderr, format, v);                                               \
    fprintf(stderr, "\n");                                                     \
    va_end(v);                                                                 \
  }                                                                            \
  void NAME##sl(const char *format, ...) {                                     \
    if (!force && !LOG_ENABLED) {                                              \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(stderr, PREFIX PR_BOLD PR_RESET);                                  \
    vfprintf(stderr, format, v);                                               \
    va_end(v);                                                                 \
  }                                                                            \
  void f##NAME(FILE *file, const char *format, ...) {                          \
    if (!force && !LOG_ENABLED) {                                              \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(file, PREFIX PR_BOLD PR_RESET);                                    \
    vfprintf(file, format, v);                                                 \
    fprintf(file, "\n");                                                       \
    va_end(v);                                                                 \
  }                                                                            \
  void f##NAME##sl(FILE *file, const char *format, ...) {                      \
    if (!force && !LOG_ENABLED) {                                              \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(file, PREFIX PR_BOLD PR_RESET);                                    \
    vfprintf(file, format, v);                                                 \
    va_end(v);                                                                 \
  }

DEF_LOG_FN(err, PR_RED PR_BOLD "ERROR: ", true)
DEF_LOG_FN(warn, PR_YELLOW PR_BOLD "WARN: ", true)
DEF_LOG_FN(info, PR_GREEN PR_BOLD "INFO: ", false)
DEF_LOG_FN(debug, PR_WHITE PR_BOLD "DEBUG: ", false)
DEF_LOG_FN(trace, PR_WHITE PR_BOLD "TRACE: ", false)

DEF_LOG_FN(slog, "", true)

#endif
#endif
