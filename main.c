#include "compiler.h"
#include "lex.h"
#include "link.h"
#include "log.h"
#include "macos/mach-o.h"
#include "parse.h"
#include "util.h"
#include "vector.h"

#include <stdio.h>
#include <stdlib.h>

enum parse_args_result {
  PARSE_ARGS_RESULT_SUCCESS = 0,
  PARSE_ARGS_RESULT_NO_SOURCES = 1,
  PARSE_ARGS_RESULT_ERROR = 2,
};

enum parse_args_result parse_args(int argc, char **argv,
                                  struct compile_args *args,
                                  const char ***sources, size_t *num_sources);

char *readfile(const char *path) {
  FILE *f = fopen(path, "r");

  if (!f) {
    return NULL;
  }

  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  rewind(f);

  char *content = nonnull_malloc(fsize + 1);
  fread(content, fsize, 1, f);
  fclose(f);

  content[fsize] = '\0';
  return content;
}

bool target_needs_linking(const struct compile_args *args) {
  return args->target_arch != COMPILE_TARGET_ARCH_EEP;
}

int main(int argc, char **argv) {
  enable_log();

  info("parsing command line args");

  struct compile_args args;
  const char **sources;
  size_t num_sources;
  if (parse_args(argc, argv, &args, &sources, &num_sources) !=
      PARSE_ARGS_RESULT_SUCCESS) {
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

    const char *source = readfile(sources[i]);

    if (!source) {
      err("source file \"%s\" could not be read!", sources[i]);
      return COMPILE_RESULT_BAD_FILE;
    }

    char *output = nonnull_malloc(strlen(sources[i]) + sizeof(".obj"));
    strcpy(output, sources[i]);
    strcat(output, ".obj");

    info("compiling source file '%s' into object file '%s'", sources[i], output);

    objects[i] = output;

    disable_log();
    struct compiler *compiler;
    if (create_compiler(source, output, &args, &compiler) !=
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

  struct link_args link_args = {.objects = (const char **)objects,
                                .num_objects = num_sources,
                                .output = "a.out"};


  if (target_needs_linking(&args)) {
    if (link_objects(&link_args) != LINK_RESULT_SUCCESS) {
      err("link failed");
      exit(-1);
    }
  }

  for (size_t i = 0; i < num_sources; i++) {
    free(objects[i]);
  }
}

const char *try_get_arg(const char *arg, const char *prefix) {
  if (strncmp(arg, prefix, strlen(prefix)) == 0) {
    return &arg[strlen(prefix)];
  }

  return NULL;
}

void parse_arg_error(const char *fmt, ...) {
  va_list v;
  va_start(v, fmt);
  vfprintf(stderr, fmt, v);
  fprintf(stderr, "\n");
  va_end(v);
}

bool parse_log_flag(const char *flag, enum compile_log_flags *flags) {
#define LOG_FLAG(name, str)                                                    \
  if (strcmp(flag, str) == 0) {                                                \
    *flags |= name;                                                            \
    return true;                                                               \
  }

  LOG_FLAG(COMPILE_LOG_FLAGS_PARSE, "parse");
  LOG_FLAG(COMPILE_LOG_FLAGS_IR, "ir");
  LOG_FLAG(COMPILE_LOG_FLAGS_REGALLOC, "regalloc");
  LOG_FLAG(COMPILE_LOG_FLAGS_EMIT, "emit");
  LOG_FLAG(COMPILE_LOG_FLAGS_ASM, "asm");
  LOG_FLAG(COMPILE_LOG_FLAGS_ALL, "all");

  parse_arg_error("Unrecognised log flag '-L%s'", flag);
  return false;
}

bool parse_target_flag(const char *flag, enum compile_target_arch *arch) {
  if (strcmp(flag, "x64") == 0) {
    *arch = COMPILE_TARGET_ARCH_MACOS_X86_64;
    return true;
  } else if (strcmp(flag, "aarch64") == 0) {
    *arch = COMPILE_TARGET_ARCH_MACOS_ARM64;
    return true;
  } else if (strcmp(flag, "eep") == 0) {
    *arch = COMPILE_TARGET_ARCH_EEP;
    return true;
  }

  return false;
}

enum parse_args_result parse_args(int argc, char **argv,
                                  struct compile_args *args,
                                  const char ***sources, size_t *num_sources) {

  memset(args, 0, sizeof(*args));

  // default to native arch
  args->target_arch = COMPILE_TARGET_ARCH_NATIVE;

  // should always be true as argv[0] is program namr
  invariant_assert(argc > 0, "argc must be >0");

  struct vector *sources_vec = vector_create(sizeof(*argv));
  for (size_t i = 1; i < (size_t)argc; i++) {
    const char *arg = argv[i];

    const char *log = try_get_arg(arg, "-L");
    if (log) {
      if (!parse_log_flag(log, &args->log_flags)) {
        return PARSE_ARGS_RESULT_ERROR;
      }

      continue;
    }

    const char *target = try_get_arg(arg, "-T");
    if (target) {
      if (!parse_target_flag(target, &args->target_arch)) {
        return PARSE_ARGS_RESULT_ERROR;
      }

      continue;
    }

    // wasn't recognised as a flag, assume source arg
    vector_push_back(sources_vec, &arg);
  }

  *num_sources = vector_length(sources_vec);

  if (*num_sources == 0) {
    parse_arg_error("No sources provided!");
    return PARSE_ARGS_RESULT_NO_SOURCES;
  }

  *sources = nonnull_malloc(vector_byte_size(sources_vec));

  vector_copy_to(sources_vec, *sources);

  return PARSE_ARGS_RESULT_SUCCESS;
}
