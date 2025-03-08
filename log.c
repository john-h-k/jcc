#include "log.h"

static bool LOG_ENABLED = false;

void enable_log(void) { LOG_ENABLED = true; }

void disable_log(void) { LOG_ENABLED = false; }

bool log_enabled(void) { return LOG_ENABLED; }

#ifdef NLOG
#define DEF_LOG_FN(NAME, _PREFIX, force)                                              \
  bool NAME##_enabled(void)  { return false; }                         \
  void NAME##_nl(void) { (void)format; }                         \
  void NAME(const char *format, ...) { (void)format; }                         \
  void NAME##sl(const char *format, ...) { (void)format; }                     \
  void f##NAME(const char *format, ...) { (void)format; }                      \
  void f##NAME##sl(const char *format, ...) { (void)format; }
#else
#define DEF_LOG_FN(NAME, PREFIX, force)                                               \
  bool NAME##_enabled(void)  { return LOG_ENABLED; }                         \
  void NAME##_nl(void) {                                         \
    if (!force && !LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    fprintf(stderr, "\n");                                                     \
  } \
  void NAME(const char *format, ...) {                                         \
    if (!force && !LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(stderr, PREFIX PR_BOLD PR_RESET);                                    \
    vfprintf(stderr, format, v);                                               \
    fprintf(stderr, "\n");                                                     \
    va_end(v);                                                                 \
  }                                                                            \
  void NAME##sl(const char *format, ...) {                                     \
    if (!force && !LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(stderr, PREFIX PR_BOLD PR_RESET);                                    \
    vfprintf(stderr, format, v);                                               \
    va_end(v);                                                                 \
  }                                                                            \
  void f##NAME(FILE *file, const char *format, ...) {                          \
    if (!force && !LOG_ENABLED) {                                                        \
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
    if (!force && !LOG_ENABLED) {                                                        \
      return;                                                                  \
    }                                                                          \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(file, PREFIX PR_BOLD PR_RESET);                                    \
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
#define PR_BOLD "\033[1m"

DEF_LOG_FN(err, PR_RED PR_BOLD "ERROR: ", true)
DEF_LOG_FN(warn, PR_YELLOW PR_BOLD "WARN: ", true)
DEF_LOG_FN(info, PR_GREEN PR_BOLD "INFO: ", false)
DEF_LOG_FN(debug, PR_WHITE PR_BOLD "DEBUG: ", false)
DEF_LOG_FN(trace, PR_WHITE PR_BOLD "TRACE: ", false)

DEF_LOG_FN(slog, "", true)
