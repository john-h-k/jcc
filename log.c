#include "log.h"

static bool LOG_ENABLED = false;

void enable_log(void) { LOG_ENABLED = true; }

void disable_log(void) { LOG_ENABLED = false; }

bool log_enabled(void) { return LOG_ENABLED; }

#ifdef NLOG
#define DEF_LOG_FN(NAME, _PREFIX)                                              \
  void NAME##_nl(void) { (void)format; }                         \
  void NAME(const char *format, ...) { (void)format; }                         \
  void NAME##sl(const char *format, ...) { (void)format; }                     \
  void f##NAME(const char *format, ...) { (void)format; }                      \
  void f##NAME##sl(const char *format, ...) { (void)format; }
#else
#define DEF_LOG_FN(NAME, PREFIX)                                               \
  void NAME##_nl(void) {                                         \
    if (!LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    fprintf(stderr, "\n");                                                     \
  } \
  void NAME(const char *format, ...) {                                         \
    if (!LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(stderr, "%s" PR_RESET, PREFIX);                                    \
    vfprintf(stderr, format, v);                                               \
    fprintf(stderr, "\n");                                                     \
    va_end(v);                                                                 \
  }                                                                            \
  void NAME##sl(const char *format, ...) {                                     \
    if (!LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(stderr, "%s" PR_RESET, PREFIX);                                    \
    vfprintf(stderr, format, v);                                               \
    va_end(v);                                                                 \
  }                                                                            \
  void f##NAME(FILE *file, const char *format, ...) {                          \
    if (!LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(file, "%s" PR_RESET, PREFIX);                                      \
    vfprintf(file, format, v);                                                 \
    fprintf(file, "\n");                                                       \
    va_end(v);                                                                 \
  }                                                                            \
  void f##NAME##sl(FILE *file, const char *format, ...) {                      \
    if (!LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(file, "%s" PR_RESET, PREFIX);                                      \
    vfprintf(file, format, v);                                                 \
    va_end(v);                                                                 \
  }
#endif

#define PR_RESET "\x1B[0m"
#define PR_RED "\x1B[31m"
#define PR_GREEN "\x1B[32m"
#define PR_YELLOW "\x1B[33m"
// #define PR_BLUE "\x1B[34m"
// #define PR_MAGENTA "\x1B[35m"
// #define PR_CYAN "\x1B[36m"
#define PR_WHITE "\x1B[37m"

DEF_LOG_FN(err, PR_RED "ERROR: ")
DEF_LOG_FN(warn, PR_YELLOW "WARN: ")
DEF_LOG_FN(info, PR_GREEN "INFO: ")
DEF_LOG_FN(debug, PR_WHITE "DEBUG: ")
DEF_LOG_FN(trace, PR_WHITE "TRACE: ")

DEF_LOG_FN(slog, "")
