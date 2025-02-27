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

struct arg_string_list {
  size_t num_values;
  const char **values;
};

typedef bool (*try_parse_arg)(const char *, int *);
typedef const char *(*string_arg)(int);

struct arg {
  enum arg_ty ty;

  const char *name;
  const char *description;

  const char *short_name;
  const char *long_name;

  try_parse_arg try_parse;
  string_arg string;

  union {
    bool *arg_bool;
    int *arg_option;
    int *arg_flags;
    const char **arg_string;
    struct arg_string_list *arg_string_list;
  };
};

#define OPTS_ENUM_LIST                                                         \
  ENUM_FN(COMPILE_OPTS_LEVEL_0, "0")                                           \
  ENUM_FN(COMPILE_OPTS_LEVEL_1, "1")                                           \
  ENUM_FN(COMPILE_OPTS_LEVEL_2, "2")                                           \
  ENUM_FN(COMPILE_OPTS_LEVEL_3, "3")

#define LOG_ENUM_LIST                                                          \
  ENUM_FN(COMPILE_LOG_FLAGS_NONE, "none")                                      \
  ENUM_FN(COMPILE_LOG_FLAGS_ARGS, "args")                                      \
  ENUM_FN(COMPILE_LOG_FLAGS_PREPROC, "preproc")                                \
  ENUM_FN(COMPILE_LOG_FLAGS_PARSE, "parse")                                    \
  ENUM_FN(COMPILE_LOG_FLAGS_TYPECHK, "typechk")                                \
  ENUM_FN(COMPILE_LOG_FLAGS_IR, "ir")                                          \
  ENUM_FN(COMPILE_LOG_FLAGS_OPTS, "opts")                                      \
  ENUM_FN(COMPILE_LOG_FLAGS_LOWER, "lower")                                    \
  ENUM_FN(COMPILE_LOG_FLAGS_REGALLOC, "regalloc")                              \
  ENUM_FN(COMPILE_LOG_FLAGS_EMIT, "emit")                                      \
  ENUM_FN(COMPILE_LOG_FLAGS_ASM, "asm")                                        \
  ENUM_FN(COMPILE_LOG_FLAGS_ALL, "all")

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

inline static bool parse_opts_level(const char *str, int *value) {
  OPTS_ENUM_LIST;
  return false;
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

inline static const char *string_opts_level(int value) {
  OPTS_ENUM_LIST;
  return "(invalid)";
}

inline static const char *string_log_level(int value) {
  LOG_ENUM_LIST;
  return "all";
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

#define ARG_BOOL(name, sh, lo, desc) ARG_OPT(BOOL, bool, name, sh, lo, desc, NULL, NULL)

#define ARG_OPTION(ty, name, sh, lo, parse_fn, string_fn, desc)                      \
  ARG_OPT(OPTION, ty, name, sh, lo, desc, parse_fn, string_fn)

#define ARG_FLAGS(ty, name, sh, lo, parse_fn, string_fn, desc)                       \
  ARG_OPT(FLAGS, ty, name, sh, lo, desc, parse_fn, string_fn)

#define ARG_STRING(name, sh, lo, desc)                                               \
  ARG_OPT(STRING, const char *, name, sh, lo, desc, NULL, NULL)

#define ARG_STRING_LIST(name, sh, lo, desc)                                          \
  ARG_OPT(STRING_LIST, struct arg_string_list, name, sh, lo, desc, NULL, NULL)

#define ARG_OPT_LIST                                                           \
  ARG_BOOL(preprocess, "-E", "--preprocess", "Only run the preprocessor")                                   \
  ARG_BOOL(assembly, "-S", "--assemble", "Only run preprocessor and compiler; output assembly, linking")                                       \
  ARG_BOOL(object, "-c", "--compile", "Only run preprocessor and compiler; output object file without linking")                                          \
                                                                               \
  ARG_OPTION(enum compile_opts_level, opts, "-O", "--opts", parse_opts_level,  \
             string_opts_level, "Optimisation level 0..3")                                                \
                                                                               \
  ARG_OPTION(enum compile_arch, arch, "", "-arch", parse_arch, string_arch, "Architecture to build for")    \
  ARG_OPTION(enum compile_target, target, "", "-target", parse_target,         \
             string_target, "Target triple (arch-vendor-os)")                                                    \
  ARG_STRING(output, "-o", "", "Output file")                                                 \
                                                                               \
  ARG_FLAGS(enum compile_log_flags, log_level, "-L", "--log", parse_log_level, \
            string_log_level, "[DEBUG] Log level flags")                                                  \
                                                                               \
  ARG_OPTION(enum compile_c_standard, c_standard, "", "-std",                  \
             parse_c_standard, string_c_standard, "C standard to use")                              \
                                                                               \
  ARG_STRING(timestamp, "", "-tm", "[DEBUG] Fixed timestamp to use for __DATE__ and __TIME__")                                             \
                                                                               \
  ARG_STRING_LIST(include_paths, "-I", "", "Directories to search for `#include` directives")

struct parsed_args {
#define ARG_OPT(_0, field_ty, name, ...) field_ty name;

  ARG_OPT_LIST

#undef ARG_OPT

  size_t num_values;
  const char **values;
};

void debug_print_parsed_args(FILE *file, const struct parsed_args *args);

enum parse_args_result {
  PARSE_ARGS_RESULT_SUCCESS,
  PARSE_ARGS_RESULT_HELP,
  PARSE_ARGS_RESULT_FAIL,
};

enum parse_args_result parse_args(int argc, char **argv, struct parsed_args *args);
void free_args(struct parsed_args *args);

#endif
