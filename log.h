#ifndef __LOG_H__
#define __LOG_H__

#include <stdarg.h>
#include <stdio.h>

#define DEF_LOG_FN(NAME, PREFIX) static inline void NAME(const char *format, ...) { \
  va_list v; \
  va_start(v, format); \
  fprintf(stderr, "%s: ", PREFIX); \
  vfprintf(stderr, format, v); \
  fprintf(stderr, "\n"); \
  va_end(v); \
}

DEF_LOG_FN(err, "ERROR")
DEF_LOG_FN(warn, "WARN")
DEF_LOG_FN(info, "INFO")
DEF_LOG_FN(debug, "DEBUG")
DEF_LOG_FN(trace, "TRACE")

#endif
