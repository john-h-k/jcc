#include "aarch64.h"
#include "alloc.h"
#include "compiler.h"
#include "io.h"
#include "log.h"
#include "program.h"
#include "rv32i.h"
#include "target.h"
#include "util.h"
#include "vector.h"
#include "x64.h"

#include <stdio.h>
#include <stdlib.h>

enum parse_args_result {
  PARSE_ARGS_RESULT_SUCCESS = 0,
  PARSE_ARGS_RESULT_NO_SOURCES = 1,
  PARSE_ARGS_RESULT_ERROR = 2,
};

static enum parse_args_result parse_args(int argc, char **argv,
                                         struct compile_args *args,
                                         const char ***sources,
                                         size_t *num_sources);

static bool target_needs_linking(const struct compile_args *args,
                                 const struct target *target) {
  if (args->preproc_only || args->build_asm_file || args->build_object_file) {
    return false;
  }

  return target->link_objects != NULL;
}

static const struct target *get_target(const struct compile_args *args) {
  switch (args->target) {
  case COMPILE_TARGET_NATIVE:
    BUG("hit COMPILE_TARGET_ARCH_NATIVE in compiler! should have been chosen "
        "earlier");
  case COMPILE_TARGET_MACOS_X86_64:
    return &X64_MACOS_TARGET;
  // FIXME: linux is actually subtly different in register usage and calling
  // conv
  case COMPILE_TARGET_LINUX_X86_64:
    return &X64_LINUX_TARGET;
  case COMPILE_TARGET_LINUX_ARM64:
    return &AARCH64_LINUX_TARGET;
  case COMPILE_TARGET_MACOS_ARM64:
    return &AARCH64_MACOS_TARGET;
  case COMPILE_TARGET_LINUX_RV32I:
    return &RV32I_LINUX_TARGET;
  case COMPILE_TARGET_EEP:
    BUG("redo eep");
    // return &EEP_TARGET;
  }

  BUG("unexpected target in `get_target`");
}

int main(int argc, char **argv) {
  enable_log();

  info("parsing command line args");

  struct compile_args args;
  const char **sources;
  size_t num_sources;
  if (parse_args(argc, argv, &args, &sources, &num_sources) !=
      PARSE_ARGS_RESULT_SUCCESS) {
    err("failed to parse arguments");
    return -1;
  }

  if (args.arch != COMPILE_ARCH_NATIVE) {
    if (args.target != COMPILE_TARGET_NATIVE) {
      err("cannot provide both `-arch` and `-T/--target` flags");
      return -1;
    }

    switch (args.arch) {
    case COMPILE_ARCH_NATIVE:
      unreachable();
    case COMPILE_ARCH_X86_64:
#if defined(__APPLE__)
      args.target = COMPILE_TARGET_MACOS_X86_64;
      break;
#elif defined(__linux__)
      args.target = COMPILE_TARGET_LINUX_X86_64;
      break;
#else
      err("Could not determine native platform");
      return -1;
#endif
    case COMPILE_ARCH_ARM64:
#if defined(__APPLE__)
      args.target = COMPILE_TARGET_MACOS_ARM64;
      break;
#elif defined(__linux__)
      args.target = COMPILE_TARGET_LINUX_ARM64;
      break;
#else
      err("Could not determine native platform");
      return -1;
#endif
    case COMPILE_ARCH_RV32I:
#if defined(__linux__)
      args.target = COMPILE_TARGET_LINUX_RV32I;
      break;
#else
      err("Could not determine native platform");
      return -1;
#endif
    case COMPILE_ARCH_EEP:
      args.target = COMPILE_TARGET_EEP;
      break;
    }
  } else if (args.target == COMPILE_TARGET_NATIVE) {
#if defined(__APPLE__) && defined(__aarch64__)
    info("Compiling for native platform - assuming macOS ARM64...\n");
    args.target = COMPILE_TARGET_MACOS_ARM64;
#elif defined(__APPLE__) && defined(__x86_64__)
    info("Compiling for native platform - assuming macOS x64...\n");
    args.target = COMPILE_TARGET_MACOS_X86_64;
#elif defined(__linux__) && defined(__aarch64__)
    info("Compiling for native platform - assuming Linux ARM64...\n");
    args.target = COMPILE_TARGET_LINUX_ARM64;
#elif defined(__linux__) && defined(__x86_64__)
    info("Compiling for native platform - assuming Linux x64...\n");
    args.target = COMPILE_TARGET_LINUX_X86_64;
#else
    err("Could not determine native platform");
    return -1;
#endif
  }

  struct arena_allocator *arena;
  arena_allocator_create(&arena);

  const struct target *target = get_target(&args);
  char **objects = nonnull_malloc(sizeof(*objects) * num_sources);

  info("beginning compilation stage...");
  for (size_t i = 0; i < num_sources; i++) {
    info("compiling source file \"%s\"", sources[i]);

    const char *source = read_file(arena, sources[i]);

    if (!source) {
      err("source file \"%s\" could not be read!", sources[i]);
      return COMPILE_RESULT_BAD_FILE;
    }

    char *object_file;

    if (args.preproc_only && !args.output) {
      // FIXME: hacky
      object_file = nonnull_malloc(strlen("stdout") + 1);
      strcpy(object_file, "stdout");
      object_file[strlen("stdout")] = 0;
    } else if (args.build_asm_file && !args.output) {
      object_file = path_replace_ext(arena, sources[i], ".s");
    } else if (target_needs_linking(&args, target) || !args.output) {
      object_file = path_replace_ext(arena, sources[i], "o");
    } else {
      object_file = args.output;
    }

    info("compiling source file '%s' into object file '%s'", sources[i],
         object_file);

    objects[i] = object_file;

    struct program program = {.text = source};

    disable_log();
    struct compiler *compiler;

    if (create_compiler(&program, target, object_file, sources[i], &args,
                        &compiler) != COMPILER_CREATE_RESULT_SUCCESS) {
      err("failed to create compiler");
      return -1;
    }

    if (compile(compiler) != COMPILE_RESULT_SUCCESS) {
      err("compilation failed!");
      return -1;
    }
    enable_log();
  }

  if (target_needs_linking(&args, target)) {
    const char *output = args.output ? args.output : "a.out";

    struct link_args link_args = {
      .args = &args,
      .objects = (const char *const *)objects,
                                  .num_objects = num_sources,
                                  .output = output};

    if (target->link_objects(&link_args) != LINK_RESULT_SUCCESS) {
      err("link failed");
      exit(-1);
    }
  } else {
    if (num_sources > 1) {
      TODO("multiple objects, but target does not support linking");
    }
  }

  info("Compilation succeeded!");

  arena_allocator_free(&arena);
  free(objects);
}

static const char *try_get_arg(const char *arg, const char *prefix) {
  if (strncmp(arg, prefix, strlen(prefix)) == 0) {
    return &arg[strlen(prefix)];
  }

  return NULL;
}

PRINTF_ARGS(0) static void parse_arg_error(const char *fmt, ...) {
  va_list v;
  va_start(v, fmt);
  vfprintf(stderr, fmt, v);
  fprintf(stderr, "\n");
  va_end(v);
}

static bool parse_log_flag(const char *flag, enum compile_log_flags *flags) {
#define LOG_FLAG(name, str)                                                    \
  if (strcmp(flag, str) == 0) {                                                \
    *flags |= name;                                                            \
    return true;                                                               \
  }

  LOG_FLAG(COMPILE_LOG_FLAGS_PREPROC, "preproc");
  LOG_FLAG(COMPILE_LOG_FLAGS_PARSE, "parse");
  LOG_FLAG(COMPILE_LOG_FLAGS_TYPECHK, "typechk");
  LOG_FLAG(COMPILE_LOG_FLAGS_IR, "ir");
  LOG_FLAG(COMPILE_LOG_FLAGS_LOWER, "lower");
  LOG_FLAG(COMPILE_LOG_FLAGS_OPTS, "opts");
  LOG_FLAG(COMPILE_LOG_FLAGS_REGALLOC, "regalloc");
  LOG_FLAG(COMPILE_LOG_FLAGS_EMIT, "emit");
  LOG_FLAG(COMPILE_LOG_FLAGS_ASM, "asm");
  LOG_FLAG(COMPILE_LOG_FLAGS_ALL, "all");

  parse_arg_error("Unrecognised log flag '-L%s'", flag);
  return false;
}

static bool parse_target_flag(const char *flag, enum compile_target *arch) {
  if (strcmp(flag, "aarch64-apple-darwin") == 0) {
    *arch = COMPILE_TARGET_MACOS_ARM64;
    return true;
  } else if (strcmp(flag, "x86_64-apple-darwin") == 0) {
    *arch = COMPILE_TARGET_MACOS_X86_64;
    return true;
  } else if (strcmp(flag, "aarch64-unknown-linux-gnu") == 0) {
    *arch = COMPILE_TARGET_LINUX_ARM64;
    return true;
  } else if (strcmp(flag, "x86_64-unknown-linux-gnu") == 0) {
    *arch = COMPILE_TARGET_LINUX_X86_64;
    return true;
  } else if (strcmp(flag, "eep-unknown-unknown") == 0) {
    *arch = COMPILE_TARGET_EEP;
    return true;
  } else if (strcmp(flag, "rv32i-unknown-elf") == 0) {
    *arch = COMPILE_TARGET_LINUX_RV32I;
    return true;
  }

  return false;
}

static bool parse_arch_flag(const char *flag, enum compile_arch *arch) {
  if (strcmp(flag, "x86_64") == 0) {
    *arch = COMPILE_ARCH_X86_64;
    return true;
  } else if (strcmp(flag, "arm64") == 0) {
    *arch = COMPILE_ARCH_ARM64;
    return true;
  } else if (strcmp(flag, "rv32i") == 0) {
    *arch = COMPILE_ARCH_RV32I;
    return true;
  } else if (strcmp(flag, "eep") == 0) {
    *arch = COMPILE_ARCH_EEP;
    return true;
  }

  return false;
}

static bool parse_c_standard(const char *flag,
                             enum compile_c_standard *c_standard) {
  if (strcmp(flag, "c11") == 0) {
    *c_standard = COMPILE_C_STANDARD_C11;
    return true;
  } else if (strcmp(flag, "c17") == 0) {
    *c_standard = COMPILE_C_STANDARD_C17;
    return true;
  } else if (strcmp(flag, "c23") == 0) {
    *c_standard = COMPILE_C_STANDARD_C23;
    return true;
  }

  warn("JCC only supports c11/c17/c23");
  return false;
}

static bool parse_output(const char *str, char **output) {
  size_t output_len = strlen(str);

  if (output_len) {
    char *name = nonnull_malloc(output_len + 1);
    memcpy(name, str, output_len);
    name[output_len] = '\0';

    *output = name;
    return true;
  }

  return false;
}

static bool parse_fixed_timestamp(const char *str,
                                  const char **fixed_timestamp) {
  size_t len = strlen(str);

  if (len >= 19) {
    *fixed_timestamp = str;
    return true;
  }

  err("`fixed_timestamp` must be exactly at least 19 chars (for symmetry with "
      "`asctime`)");
  return false;
}
static enum parse_args_result parse_args(int argc, char **argv,
                                         struct compile_args *args,
                                         const char ***sources,
                                         size_t *num_sources) {
  memset(args, 0, sizeof(*args));

  // default to native arch
  args->target = COMPILE_TARGET_NATIVE;

  // should always be true as argv[0] is program namr
  invariant_assert(argc > 0, "argc must be >0");

  struct vector *include_path_vec = vector_create(sizeof(*argv));
  struct vector *sources_vec = vector_create(sizeof(*argv));
  for (int i = 1; i < argc; i++) {
    const char *arg = argv[i];

// allows both '-Tfoo' and '-T foo' by using the next argument if the current
// one is empty (after prefix) if it uses the next argument it skips it
#define GET_ARGUMENT(v)                                                        \
  v = (!(v) || (v)[0] || i + 1 == argc ? ((v) && (v)[0] == '=' ? (v) + 1 : v)  \
                                       : argv[++i])

    const char *log = try_get_arg(arg, "-L");
    GET_ARGUMENT(log);
    if (log) {
      if (!parse_log_flag(log, &args->log_flags)) {
        return PARSE_ARGS_RESULT_ERROR;
      }

      continue;
    }

    const char *include_path = try_get_arg(arg, "-I");
    GET_ARGUMENT(include_path);
    if (include_path) {
      vector_push_back(include_path_vec, &include_path);

      continue;
    }

    const char *target = try_get_arg(arg, "-T");
    GET_ARGUMENT(target);
    if (target) {
      if (!parse_target_flag(target, &args->target)) {
        return PARSE_ARGS_RESULT_ERROR;
      }

      continue;
    }

    const char *arch = try_get_arg(arg, "-arch");
    GET_ARGUMENT(arch);
    if (arch) {
      if (!parse_arch_flag(arch, &args->arch)) {
        return PARSE_ARGS_RESULT_ERROR;
      }

      continue;
    }

    const char *output = try_get_arg(arg, "-o");
    GET_ARGUMENT(output);
    if (output) {
      if (!parse_output(output, &args->output)) {
        return PARSE_ARGS_RESULT_ERROR;
      }

      continue;
    }

    const char *fixed_timestamp = try_get_arg(arg, "-tm");
    GET_ARGUMENT(fixed_timestamp);
    if (fixed_timestamp) {
      if (!parse_fixed_timestamp(fixed_timestamp, &args->fixed_timestamp)) {
        return PARSE_ARGS_RESULT_ERROR;
      }

      continue;
    }

    const char *preproc_only = try_get_arg(arg, "-E");
    if (preproc_only) {
      args->preproc_only = true;

      continue;
    }

    const char *build_asm_file = try_get_arg(arg, "-S");
    if (build_asm_file) {
      args->build_asm_file = true;

      continue;
    }

    const char *build_object_file = try_get_arg(arg, "-c");
    if (build_object_file) {
      args->build_object_file = true;

      continue;
    }

    const char *c_standard = try_get_arg(arg, "-std");
    GET_ARGUMENT(c_standard);
    if (c_standard) {
      if (!parse_c_standard(c_standard, &args->c_standard)) {
        return PARSE_ARGS_RESULT_ERROR;
      }

      continue;
    }

#undef GET_ARGUMENT

    // wasn't recognised as a flag, assume source arg
    vector_push_back(sources_vec, &arg);
  }

  args->num_include_paths = vector_length(include_path_vec);
  args->include_paths = nonnull_malloc(vector_byte_size(include_path_vec));
  vector_copy_to(include_path_vec, args->include_paths);

  *num_sources = vector_length(sources_vec);

  if (*num_sources == 0) {
    parse_arg_error("No sources provided!");
    return PARSE_ARGS_RESULT_NO_SOURCES;
  }

  *sources = nonnull_malloc(vector_byte_size(sources_vec));
  vector_copy_to(sources_vec, *sources);

  return PARSE_ARGS_RESULT_SUCCESS;
}
