#ifndef ARGS_H
#define ARGS_H

#include "compiler.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum arg_ty {
  // e.g `-O3`
  ARG_TY_BOOL,

  // e.g `-arch x86_64`
  ARG_TY_OPTION,

  // e.g `-Lall`
  ARG_TY_FLAGS,

  // e.g `-o foo`
  ARG_TY_STRING,

  // e.g `-I foo -I ../bar`
  ARG_TY_STRING_LIST,
};

struct arg_bool {
  bool enabled;
};

struct arg_option {
  int value;
};

struct arg_flags {
  int value;
};

struct arg_string {
  const char *value;
};

struct arg_string_list {
  size_t num_values;
  const char **values;
};

typedef bool (*try_parse_arg)(const char *, int *);
typedef const char *(*string_arg)(int);

struct arg {
  enum arg_ty ty;

  const char *name;

  const char *short_name;
  const char *long_name;

  try_parse_arg try_parse;
  string_arg string;

  union {
    struct arg_bool *arg_bool;
    struct arg_option *arg_option;
    struct arg_flags *arg_flags;
    struct arg_string *arg_string;
    struct arg_string_list *arg_string_list;
  };
};

#define LOG_ENUM_LIST \
  ENUM_FN(COMPILE_LOG_FLAGS_NONE, "none") \
  ENUM_FN(COMPILE_LOG_FLAGS_PREPROC, "preproc") \
  ENUM_FN(COMPILE_LOG_FLAGS_PARSE, "parse") \
  ENUM_FN(COMPILE_LOG_FLAGS_TYPECHK, "typechk") \
  ENUM_FN(COMPILE_LOG_FLAGS_IR, "ir") \
  ENUM_FN(COMPILE_LOG_FLAGS_OPTS, "opts") \
  ENUM_FN(COMPILE_LOG_FLAGS_LOWER, "lower") \
  ENUM_FN(COMPILE_LOG_FLAGS_REGALLOC, "regalloc") \
  ENUM_FN(COMPILE_LOG_FLAGS_EMIT, "emit") \
  ENUM_FN(COMPILE_LOG_FLAGS_ASM, "asm") \
  ENUM_FN(COMPILE_LOG_FLAGS_ALL, "all") \

#define ARCH_ENUM_LIST                                                         \
  ENUM_FN(COMPILE_ARCH_X86_64, "x86_64")                                       \
  ENUM_FN(COMPILE_ARCH_ARM64, "arm64")                                         \
  ENUM_FN(COMPILE_ARCH_RV32I, "rv32i")                                         \
  ENUM_FN(COMPILE_ARCH_EEP, "eep")

#define TARGET_ENUM_LIST                                                       \
  ENUM_FN(COMPILE_TARGET_MACOS_ARM64, "aarch64-apple-darwin")                  \
  ENUM_FN(COMPILE_TARGET_MACOS_X86_64, "x86_64-apple-darwin")                  \
  ENUM_FN(COMPILE_TARGET_LINUX_ARM64, "aarch64-unknown-linux-gnu")             \
  ENUM_FN(COMPILE_TARGET_LINUX_X86_64, "x86_64-unknown-linux-gnu")             \
  ENUM_FN(COMPILE_TARGET_EEP, "eep-unknown-unknown")                           \
  ENUM_FN(COMPILE_TARGET_LINUX_RV32I, "rv32i-unknown-elf")

#define C_STANDARD_ENUM_LIST                                                   \
  ENUM_FN(COMPILE_C_STANDARD_C11, "c11")                                       \
  ENUM_FN(COMPILE_C_STANDARD_C17, "c17")                                       \
  ENUM_FN(COMPILE_C_STANDARD_C23, "c23")

#define ENUM_FN(enum_value, str_value)                                         \
  if (strcmp(str, str_value) == 0) {                                           \
    *value = enum_value;                                                       \
    return true;                                                               \
  }

inline static bool parse_log_level(const char *str, int *value) {
  LOG_ENUM_LIST;
  return false;
}

inline static bool parse_arch(const char *str, int *value) {
  ARCH_ENUM_LIST;
  return false;
}

inline static bool parse_target(const char *str, int *value) {
  TARGET_ENUM_LIST;
  return false;
}

inline static bool parse_c_standard(const char *str, int *value) {
  C_STANDARD_ENUM_LIST;
  return false;
}

#undef ENUM_FN

#define ENUM_FN(enum_value, str_value)                                         \
  if (value == enum_value) {                                                   \
    return str_value;                                                          \
  }

inline static const char *string_log_level(int value) {
  LOG_ENUM_LIST;
  return false;
}

inline static const char *string_arch(int value) {
  ARCH_ENUM_LIST;
  return "(invalid)";
}

inline static const char *string_target(int value) {
  TARGET_ENUM_LIST;
  return "(invalid)";
}

inline static const char *string_c_standard(int value) {
  C_STANDARD_ENUM_LIST;
  return "(invalid)";
}

#undef ENUM_FN

#define ARG_OPT_LIST                                                           \
  ARG_OPT(BOOL, bool, preprocess, "-E", "--preprocess", NULL, NULL)            \
  ARG_OPT(BOOL, bool, assembly, "-S", "--assemble", NULL, NULL)                \
  ARG_OPT(BOOL, bool, object, "-c", "--compile", NULL, NULL)                   \
                                                                               \
  ARG_OPT(OPTION, option, arch, "", "-arch", parse_arch, string_arch)          \
  ARG_OPT(OPTION, option, target, "", "-target", parse_target, string_target)  \
  ARG_OPT(STRING, string, output, "-o", "", NULL, NULL)                        \
\
  ARG_OPT(FLAGS, flags, log_level, "-L", "--log", parse_log_level, string_log_level)                        \
                                                                               \
  ARG_OPT(OPTION, option, c_standard, "", "-std", parse_c_standard,            \
          string_c_standard)                                                   \
                                                                               \
  ARG_OPT(STRING, string, timestamp, "", "-tm", NULL, NULL)                    \
                                                                               \
  ARG_OPT(STRING_LIST, string_list, include_paths, "-I", "", NULL, NULL)

struct parsed_args {
#define ARG_OPT(_0, struct_ty, name, ...) struct arg_##struct_ty name;

  ARG_OPT_LIST

#undef ARG_OPT

  size_t num_values;
  const char **values;
};

void debug_print_parsed_args(FILE *file, const struct parsed_args *args);

struct parsed_args parse_args(int argc, char **argv);
void free_args(struct parsed_args *args);

#endif
