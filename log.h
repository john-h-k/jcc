#ifndef LOG_H
#define LOG_H

#include <stdarg.h>
#include <stdio.h>

#ifdef NLOG
#define DEF_LOG_FN(NAME, _PREFIX)                                              \
  static inline void NAME(const char *format, ...) { (void)format; }
#else
#define DEF_LOG_FN(NAME, PREFIX)                                               \
  static inline void NAME(const char *format, ...) {                           \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(stderr, "%s: " PR_RESET, PREFIX);                                  \
    vfprintf(stderr, format, v);                                               \
    fprintf(stderr, "\n");                                                     \
    va_end(v);                                                                 \
  }
#endif

#define PR_RESET "\x1B[0m"
#define PR_RED "\x1B[31m"
#define PR_GREEN "\x1B[32m"
#define PR_YELLOW "\x1B[33m"
#define PR_BLUE "\x1B[34m"
#define PR_MAGENTA "\x1B[35m"
#define PR_CYAN "\x1B[36m"
#define PR_WHITE "\x1B[37m"

DEF_LOG_FN(err, PR_RED "ERROR")
DEF_LOG_FN(warn, PR_YELLOW "WARN")
DEF_LOG_FN(info, PR_GREEN "INFO")
DEF_LOG_FN(debug, PR_WHITE "DEBUG")
DEF_LOG_FN(trace, PR_WHITE "TRACE")

#define BEGIN_STAGE(name)                                                      \
  fprintf(stderr, "\n\n**********  " name "  **********\n\n")

#define BEGIN_SUB_STAGE(name)                                                      \
  fprintf(stderr, "\n\n>> " name " \n\n")

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
