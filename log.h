#ifndef LOG_H
#define LOG_H

#include <stdarg.h>
#include <stdio.h>

#define DECL_LOG_FN(NAME)                                                      \
  void NAME(const char *format, ...);                                          \
  void NAME##sl(const char *format, ...);                                      \
  void f##NAME(FILE *file, const char *format, ...);                           \
  void f##NAME##sl(FILE *file, const char *format, ...);

void enable_log();
void disable_log();

DECL_LOG_FN(err)
DECL_LOG_FN(warn)
DECL_LOG_FN(info)
DECL_LOG_FN(debug)
DECL_LOG_FN(trace)

DECL_LOG_FN(slog)

#define BEGIN_STAGE(name) slog("\n\n**********  " name "  **********\n")

#define BEGIN_SUB_STAGE(name) slog("\n\n>> " name " \n")

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
