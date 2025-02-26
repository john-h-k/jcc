#include "aarch64.h"
#include "alloc.h"
#include "args.h"
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

TODO_FUNC(static enum parse_args_result parse_args_old(int argc, char **argv,
                                             struct compile_args *args,
                                             const char ***sources,
                                             size_t *num_sources))

void free_args_old(struct compile_args *args, const char **sources);

static bool target_needs_linking(const struct compile_args *args,
                                 const struct target *target) {
  if (args->preproc_only || args->build_asm_file || args->build_object_file) {
    return false;
  }

  return target->link_objects != NULL;
}

static const struct target *get_target(const struct compile_args *args) {
  switch (args->target) {
  case COMPILE_TARGET_MACOS_X86_64:
    return &X64_MACOS_TARGET;
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

UNUSED static bool get_target_for_args(enum compile_arch arch,
                                enum compile_target *target) {
  switch (arch) {
  case COMPILE_ARCH_NATIVE:
#if defined(__APPLE__) && defined(__aarch64__)
    info("Compiling for native platform - assuming macOS ARM64...\n");
    *target = COMPILE_TARGET_MACOS_ARM64;
    return true;
#elif defined(__APPLE__) && defined(__x86_64__)
    info("Compiling for native platform - assuming macOS x64...\n");
    *target = COMPILE_TARGET_MACOS_X86_64;
    return true;
#elif defined(__linux__) && defined(__aarch64__)
    info("Compiling for native platform - assuming Linux ARM64...\n");
    *target = COMPILE_TARGET_LINUX_ARM64;
    return true;
#elif defined(__linux__) && defined(__x86_64__)
    info("Compiling for native platform - assuming Linux x64...\n");
    *target = COMPILE_TARGET_LINUX_X86_64;
    return true;
#else
    err("Could not determine native platform");
    return false;
#endif

  case COMPILE_ARCH_X86_64:
#if defined(__APPLE__)
    *target = COMPILE_TARGET_MACOS_X86_64;
    return true;
#elif defined(__linux__)
    *target = COMPILE_TARGET_LINUX_X86_64;
    return true;
#else
    err("Could not determine native platform for x86_64");
    return false;
#endif
  case COMPILE_ARCH_ARM64:
#if defined(__APPLE__)
    *target = COMPILE_TARGET_MACOS_ARM64;
    return true;
#elif defined(__linux__)
    *target = COMPILE_TARGET_LINUX_ARM64;
    return true;
    break;
#else
    err("Could not determine native platform for arm64");
    exc = -1;
    goto exit;
#endif
  case COMPILE_ARCH_RV32I:
#if defined(__linux__)
    *target = COMPILE_TARGET_LINUX_RV32I;
    return true;
#else
    err("Could not determine native platform for rv32i");
    return false;
#endif
  case COMPILE_ARCH_EEP:
    *target = COMPILE_TARGET_EEP;
    return true;
  }
}

int main(int argc, char **argv) {
  size_t exc = -1;

  enable_log();

  info("parsing command line args");

  struct parsed_args parsed = parse_args(argc, argv);
  debug_print_parsed_args(stderr, &parsed);

  BUG("");

  struct arena_allocator *arena = NULL;
  char **objects = NULL;

  struct compile_args args;
  const char **sources;
  size_t num_sources;
  if (parse_args_old(argc, argv, &args, &sources, &num_sources) !=
      PARSE_ARGS_RESULT_SUCCESS) {
    err("failed to parse arguments");
    exc = -1;
    goto exit;
  }

  arena_allocator_create(&arena);

  const struct target *target = get_target(&args);
  objects = nonnull_malloc(sizeof(*objects) * num_sources);

  info("beginning compilation stage...");
  for (size_t i = 0; i < num_sources; i++) {
    info("compiling source file \"%s\"", sources[i]);

    const char *source = read_file(arena, sources[i]);

    if (!source) {
      err("source file \"%s\" could not be read!", sources[i]);
      exc = COMPILE_RESULT_BAD_FILE;
      goto exit;
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
      exc = -1;
      goto exit;
    }

    if (compile(compiler) != COMPILE_RESULT_SUCCESS) {
      err("compilation failed!");
      exc = -1;
      goto exit;
    }
    enable_log();

    free_compiler(&compiler);
  }

  if (target_needs_linking(&args, target)) {
    const char *output = args.output ? args.output : "a.out";

    struct link_args link_args = {.args = &args,
                                  .objects = (const char *const *)objects,
                                  .num_objects = num_sources,
                                  .output = output};

    if (target->link_objects(&link_args) != LINK_RESULT_SUCCESS) {
      err("link failed");
      exc = -1;
      goto exit;
    }
  } else {
    if (num_sources > 1) {
      TODO("multiple objects, but target does not support linking");
    }
  }

  info("Compilation succeeded!");

  exc = 0;

exit:
  if (arena) {
    arena_allocator_free(&arena);
  }

  if (objects) {
    free(objects);
  }

  free_args_old(&args, sources);

  return exc;
}

UNUSED static const char *try_get_arg(const char *arg, const char *prefix) {
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

UNUSED static bool parse_log_flag(const char *flag, enum compile_log_flags *flags) {
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

UNUSED static bool parse_fixed_timestamp(const char *str,
                                  const char **fixed_timestamp) {
  size_t len = strlen(str);

  if (len >= 19) {
    *fixed_timestamp = str;
    return true;
  }

  err("`fixed_timestamp` must be exactly at least 19 chars (for symmetry "
      "with "
      "`asctime`)");
  return false;
}
