#include "compiler.h"
#include "io.h"
#include "lex.h"
#include "link.h"
#include "log.h"
#include "macos/mach-o.h"
#include "parse.h"
#include "program.h"
#include "util.h"
#include "vector.h"

#include <ctype.h>
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

static bool target_needs_linking(const struct compile_args *args) {
  if (args->preproc_only || args->build_object_file) {
    return false;
  }

  switch (args->target_arch) {
  case COMPILE_TARGET_ARCH_NATIVE:
    bug("native arch should not be here");
  case COMPILE_TARGET_ARCH_MACOS_ARM64:
  case COMPILE_TARGET_ARCH_MACOS_X86_64:
    return true;
  case COMPILE_TARGET_ARCH_EEP:
  case COMPILE_TARGET_ARCH_RV32I:
    return false;
  }
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

  if (args.target_arch == COMPILE_TARGET_ARCH_NATIVE) {
    info("Compiling for native platform - assuming macOS ARM64...\n");
    args.target_arch = COMPILE_TARGET_ARCH_MACOS_ARM64;
  }

  char **objects = nonnull_malloc(sizeof(*objects) * num_sources);

  info("beginning compilation stage...");
  for (size_t i = 0; i < num_sources; i++) {
    info("compiling source file \"%s\"", sources[i]);

    const char *source = read_file(sources[i]);

    if (!source) {
      err("source file \"%s\" could not be read!", sources[i]);
      return COMPILE_RESULT_BAD_FILE;
    }

    const char *working_dir = path_dir(sources[i]);
    char *object_file;

    if (args.preproc_only && !args.output) {
      // FIXME: hacky
      object_file = nonnull_malloc(strlen("stdout") + 1);
      strcpy(object_file, "stdout");
      object_file[strlen("stdout")] = 0;
    } else if (target_needs_linking(&args) || !args.output) {
      object_file = path_replace_ext(sources[i], "o");
    } else {
      object_file = args.output;
    }

    info("compiling source file '%s' into object file '%s'", sources[i],
         object_file);

    objects[i] = object_file;

    struct program program = {.text = source};

    disable_log();
    struct compiler *compiler;

    if (create_compiler(&program, object_file, working_dir, &args, &compiler) !=
        COMPILER_CREATE_RESULT_SUCCESS) {
      err("failed to create compiler");
      return -1;
    }

    if (compile(compiler) != COMPILE_RESULT_SUCCESS) {
      err("compilation failed!");
      return -1;
    }
    enable_log();
  }

  if (target_needs_linking(&args)) {
    const char *output = args.output ? args.output : "a.out";

    struct link_args link_args = {.objects = (const char *const *)objects,
                                  .num_objects = num_sources,
                                  .output = output};

    if (link_objects(&link_args) != LINK_RESULT_SUCCESS) {
      err("link failed");
      exit(-1);
    }
  } else {
    if (num_sources > 1) {
      todo("multiple objects, but target does not support linking");
    }
  }

  info("Compilation succeeded!");

  for (size_t i = 0; i < num_sources; i++) {
    free(objects[i]);
  }

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
  LOG_FLAG(COMPILE_LOG_FLAGS_REGALLOC, "regalloc");
  LOG_FLAG(COMPILE_LOG_FLAGS_EMIT, "emit");
  LOG_FLAG(COMPILE_LOG_FLAGS_ASM, "asm");
  LOG_FLAG(COMPILE_LOG_FLAGS_ALL, "all");

  parse_arg_error("Unrecognised log flag '-L%s'", flag);
  return false;
}

static bool parse_target_flag(const char *flag,
                              enum compile_target_arch *arch) {
  if (strcmp(flag, "x64") == 0) {
    *arch = COMPILE_TARGET_ARCH_MACOS_X86_64;
    return true;
  } else if (strcmp(flag, "aarch64") == 0) {
    *arch = COMPILE_TARGET_ARCH_MACOS_ARM64;
    return true;
  } else if (strcmp(flag, "eep") == 0) {
    *arch = COMPILE_TARGET_ARCH_EEP;
    return true;
  } else if (strcmp(flag, "rv32i") == 0) {
    *arch = COMPILE_TARGET_ARCH_RV32I;
    return true;
  }

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

static enum parse_args_result parse_args(int argc, char **argv,
                                         struct compile_args *args,
                                         const char ***sources,
                                         size_t *num_sources) {
  memset(args, 0, sizeof(*args));

  // default to native arch
  args->target_arch = COMPILE_TARGET_ARCH_NATIVE;

  // should always be true as argv[0] is program namr
  invariant_assert(argc > 0, "argc must be >0");

  struct vector *include_path_vec = vector_create(sizeof(*argv));
  struct vector *sources_vec = vector_create(sizeof(*argv));
  for (int i = 1; i < argc; i++) {
    const char *arg = argv[i];

// allows both '-Tfoo' and '-T foo' by using the next argument if the current
// one is empty (after prefix) if it uses the next argument it skips it
#define GET_ARGUMENT(v) v = (!(v) || (v)[0] || i + 1 == argc ? v : argv[++i])

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
      vector_push_back(include_path_vec, include_path);

      continue;
    }

    const char *target = try_get_arg(arg, "-T");
    GET_ARGUMENT(target);
    if (target) {
      if (!parse_target_flag(target, &args->target_arch)) {
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

    const char *preproc_only = try_get_arg(arg, "-E");
    if (preproc_only) {
      args->preproc_only = true;

      continue;
    }

    const char *build_object_file = try_get_arg(arg, "-c");
    if (build_object_file) {
      args->build_object_file = true;

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
