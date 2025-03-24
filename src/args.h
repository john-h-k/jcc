#ifndef ARGS_H
#define ARGS_H

#include "compiler.h"
#include "util.h"

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
  char **values;
};

typedef bool (*try_parse_arg)(const char *, int *);
typedef const char *(*string_arg)(int);
typedef void (*values_arg)(const char ***, size_t *);

struct arg {
  enum arg_ty ty;

  const char *name;
  const char *description;

  const char *short_name;
  const char *long_name;

  try_parse_arg try_parse;
  string_arg string;
  values_arg values;

  union {
    bool *arg_bool;
    int *arg_option;
    int *arg_flags;
    const char **arg_string;
    struct arg_string_list *arg_string_list;
  };
};

#define CODEGEN_FLAG_ENUM_LIST                                                 \
  ENUM_FN(CODEGEN_FLAG_NONE, "none")                                           \
  ENUM_FN(CODEGEN_FLAG_MNEMONICS, "mnemonics")

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
  ENUM_FN(COMPILE_LOG_FLAGS_INLINE, "inline")                                  \
  ENUM_FN(COMPILE_LOG_FLAGS_LOWER_ABI, "lower_abi")                            \
  ENUM_FN(COMPILE_LOG_FLAGS_OPTS, "opts")                                      \
  ENUM_FN(COMPILE_LOG_FLAGS_LOWER, "lower")                                    \
  ENUM_FN(COMPILE_LOG_FLAGS_REGALLOC, "regalloc")                              \
  ENUM_FN(COMPILE_LOG_FLAGS_ELIM_PHI, "elim_phi")                              \
  ENUM_FN(COMPILE_LOG_FLAGS_CODEGEN_PREPARE, "codegen_prepare")                \
  ENUM_FN(COMPILE_LOG_FLAGS_CODEGEN, "codegen")                                \
  ENUM_FN(COMPILE_LOG_FLAGS_EMIT, "emit")                                      \
  ENUM_FN(COMPILE_LOG_FLAGS_BUILD_OBJECT, "build_object")                      \
  ENUM_FN(COMPILE_LOG_FLAGS_ALL, "all")

#define ARCH_ENUM_LIST                                                         \
  ENUM_FN(COMPILE_ARCH_X86_64, "x86_64")                                       \
  ENUM_FN(COMPILE_ARCH_ARM64, "arm64")                                         \
  ENUM_FN(COMPILE_ARCH_RV32I, "rv32i")                                         \
  ENUM_FN(COMPILE_ARCH_EEP, "eep")

#define TARGET_ENUM_LIST                                                       \
  ENUM_FN(COMPILE_TARGET_MACOS_ARM64, "aarch64-apple-darwin")                  \
  ENUM_FN(COMPILE_TARGET_MACOS_X86_64, "x86_64-apple-darwin")                  \
  ENUM_FN(COMPILE_TARGET_LINUX_ARM64, "aarch64-linux-gnu")                     \
  ENUM_FN(COMPILE_TARGET_LINUX_X86_64, "x86_64-linux-gnu")                     \
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

#define PARSE_FN(name, enum_name)                                              \
  inline static bool parse_##name(const char *str, int *value) {               \
    enum_name##_ENUM_LIST;                                                     \
    return false;                                                              \
  }

#define STRING_FN(name, enum_name, default)                                    \
  inline static const char *string_##name(int value) {                         \
    enum_name##_ENUM_LIST;                                                     \
    return default;                                                            \
  }

#define VALUES_FN(name, enum_name)                                             \
  inline static void values_##name(const char ***values, size_t *num_values) { \
    static const char *enum_values[] = {enum_name##_ENUM_LIST};                \
                                                                               \
    *values = enum_values;                                                     \
    *num_values = ARR_LENGTH(enum_values);                                     \
  }

inline static bool parse_log_level(const char *str, int *value) {
  LOG_ENUM_LIST;

  if (strcmp(str, "ir.all") == 0) {
    *value =
        COMPILE_LOG_FLAGS_IR | COMPILE_LOG_FLAGS_INLINE |
        COMPILE_LOG_FLAGS_LOWER_ABI | COMPILE_LOG_FLAGS_OPTS |
        COMPILE_LOG_FLAGS_LOWER /* temp disable regalloc bc it doesn't respect
                                   --log-sym | COMPILE_LOG_FLAGS_REGALLOC */
        | COMPILE_LOG_FLAGS_ELIM_PHI | COMPILE_LOG_FLAGS_CODEGEN_PREPARE;
    return true;
  }

  return false;
}

PARSE_FN(codegen_flags, CODEGEN_FLAG)
PARSE_FN(opts_level, OPTS)
PARSE_FN(arch, ARCH)
PARSE_FN(target, TARGET)
PARSE_FN(c_standard, C_STANDARD)

#undef ENUM_FN

#define ENUM_FN(enum_value, str_value)                                         \
  if (value == enum_value) {                                                   \
    return str_value;                                                          \
  }

STRING_FN(codegen_flags, CODEGEN_FLAG, "all")
STRING_FN(opts_level, OPTS, "(invalid)")
STRING_FN(arch, ARCH, "(invalid)")
STRING_FN(target, TARGET, "(invalid)")
STRING_FN(c_standard, C_STANDARD, "(invalid)")
STRING_FN(log_level, LOG, "all")

#undef ENUM_FN

#define ENUM_FN(enum_value, str_value) str_value,

inline static void values_log_level(const char ***values, size_t *num_values) {
  static const char *enum_values[] = {LOG_ENUM_LIST "ir.all"};

  *values = enum_values;
  *num_values = ARR_LENGTH(enum_values);
}

VALUES_FN(codegen_flags, CODEGEN_FLAG)
VALUES_FN(opts_level, OPTS)
VALUES_FN(arch, ARCH)
VALUES_FN(target, TARGET)
VALUES_FN(c_standard, C_STANDARD)

#undef ENUM_FN

#define ARG_BOOL(name, sh, lo, desc)                                           \
  ARG_OPT(BOOL, bool, name, sh, lo, desc, NULL, NULL, NULL)

#define ARG_OPTION(ty, name, sh, lo, fn, desc)                                 \
  ARG_OPT(OPTION, ty, name, sh, lo, desc, parse_##fn, string_##fn, values_##fn)

#define ARG_FLAGS(ty, name, sh, lo, fn, desc)                                  \
  ARG_OPT(FLAGS, ty, name, sh, lo, desc, parse_##fn, string_##fn, values_##fn)

#define ARG_STRING(name, sh, lo, desc)                                         \
  ARG_OPT(STRING, const char *, name, sh, lo, desc, NULL, NULL, NULL)

#define ARG_STRING_LIST(name, sh, lo, desc)                                    \
  ARG_OPT(STRING_LIST, struct arg_string_list, name, sh, lo, desc, NULL, NULL, \
          NULL)

/* ------------------------- Preprocessor options ------------------------- */
#define PREPROC_OPT_LIST                                                       \
  ARG_BOOL(preprocess, "-E", "--preprocess",                                   \
           "Only run the preprocessor. If '-o' is not provided, this will "    \
           "output to stdout")                                                 \
                                                                               \
  ARG_BOOL(no_line_commands, "-P", "--no-line-commands",                       \
           "Disable line markers in -E/--preprocess")                          \
                                                                               \
  ARG_STRING_LIST(define_macros, "-D", "--define-macro",                       \
                  "Define `arg` as 1, or `arg=value` as `value`")              \
                                                                               \
  ARG_STRING(isys_root, "", "-isysroot",                                       \
             "Root directory for `#include <header>` directives")              \
                                                                               \
  ARG_STRING_LIST(sys_include_paths, "", "-isystem",                           \
                  "Directories to search for `#include <header>` directives")  \
                                                                               \
  ARG_STRING_LIST(                                                             \
      include_paths, "-I", "",                                                 \
      "Directories to search for `#include \" header\"` directives")

/* ------------------------- Debug-only options ------------------------- */
#define DEBUG_OPT_LIST                                                         \
  ARG_FLAGS(enum compile_log_flags, log_level, "-L", "--log", log_level,       \
            "[DEBUG] Log level flags")                                         \
                                                                               \
  ARG_STRING_LIST(log_symbols, "", "--log-sym",                                \
                  "[DEBUG] Symbols to log (default: all)")                     \
                                                                               \
  ARG_STRING(timestamp, "", "-tm",                                             \
             "[DEBUG] Fixed timestamp to use for __DATE__ and __TIME__")

/* ------------------------- Output options ------------------------- */
#define OUTPUT_OPT_LIST                                                        \
  ARG_BOOL(                                                                    \
      assembly, "-S", "--assemble",                                            \
      "Only run preprocessor and compiler; output assembly without linking")   \
                                                                               \
  ARG_BOOL(object, "-c", "--compile",                                          \
           "Only run preprocessor and compiler; output object file without "   \
           "linking")                                                          \
                                                                               \
  ARG_STRING(output, "-o", "", "Output file")

/* ------------------------- Target options ------------------------- */
#define TARGET_OPT_LIST                                                        \
  ARG_OPTION(enum compile_arch, arch, "", "-arch", arch,                       \
             "Architecture to build for")                                      \
                                                                               \
  ARG_OPTION(enum compile_target, target, "", "-target", target,               \
             "Target triple (arch-vendor-os)")                                 \
                                                                               \
  ARG_OPTION(enum compile_c_standard, c_standard, "", "-std", c_standard,      \
             "C standard to use")

/* ------------------------- Codegen options ------------------------- */
#define CODEGEN_OPT_LIST                                                       \
  ARG_OPTION(enum compile_opts_level, opts, "-O", "--opts", opts_level,        \
             "Optimisation level 0..3")                                        \
                                                                               \
  ARG_FLAGS(enum codegen_flags, codegen_flags, "-C", "--codegen",              \
            codegen_flags, "Codegen flags")                                    \
                                                                               \
  ARG_BOOL(use_graphcol_regalloc, "", "--use-graphcol",                        \
           "[EXPERIMENTAL] Use "                                               \
           "graph-colouring "                                                  \
           "based regalloc")                                                   \
                                                                               \
  /* FIXME: ignored */                                                         \
  ARG_BOOL(debug, "-g", "", "Debug info (currently does nothing)")

/* ------------------------- Feature options ------------------------- */
#define FEATURE_OPT_LIST                                                       \
  ARG_BOOL(syntax_only, "", "-fsyntax-only",                                   \
           "Only run preprocessor, syntax, and typechecking stages")

/* ------------------------- Warning options ------------------------- */
#define WARNING_OPT_LIST                                                       \
  /* FIXME: Not used */                                                        \
  ARG_STRING_LIST(warnings, "-W", "", "Warning settings")

/* ------------------------- Link options ------------------------- */
#define LINK_OPT_LIST                                                          \
  /* FIXME: these are currently ignored */                                     \
  ARG_STRING_LIST(linker_args, "", "-Wl,", "Arguments to pass to the linker")  \
                                                                               \
  ARG_STRING_LIST(link_libraries, "-l", "", "Libraries to link against")

#define ARG_OPT_LIST                                                           \
  PREPROC_OPT_LIST                                                             \
                                                                               \
  WARNING_OPT_LIST                                                             \
                                                                               \
  FEATURE_OPT_LIST                                                             \
                                                                               \
  TARGET_OPT_LIST                                                              \
                                                                               \
  CODEGEN_OPT_LIST                                                             \
                                                                               \
  LINK_OPT_LIST                                                                \
                                                                               \
  OUTPUT_OPT_LIST                                                              \
                                                                               \
  ARG_BOOL(verbose, "-v", "--verbose", "Show all commands executed")           \
                                                                               \
  ARG_BOOL(version, "-V", "--version", "Print version")                        \
                                                                               \
  ARG_BOOL(profile, "", "--profile", "Run profiler")                           \
                                                                               \
  DEBUG_OPT_LIST

struct parsed_args {
  int argc;
  char **argv;

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

enum parse_args_result parse_args(int argc, char **argv,
                                  struct parsed_args *args);
void free_args(struct parsed_args *args);

#endif
