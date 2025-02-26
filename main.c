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

static bool target_needs_linking(const struct compile_args *args,
                                 const struct target *target) {
  if (args->preproc_only || args->build_asm_file || args->build_object_file) {
    return false;
  }

  return target->link_objects != NULL;
}

static const struct target *get_target(enum compile_target target) {
  switch (target) {
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


static bool validate_fixed_timestamp(const char *str) {
  size_t len = strlen(str);

  if (len >= 19) {
    return true;
  }

  err("'-tm fixed_timestamp' must be at least 19 chars (for symmetry "
      "with "
      "`asctime`)");
  return false;
}

static bool get_target_for_args(enum compile_arch arch,
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
    info("Compiling for '%s'...\n", string_target(*target));
    return true;
#elif defined(__linux__)
    *target = COMPILE_TARGET_LINUX_X86_64;
    info("Compiling for '%s'...\n", string_target(*target));
    return true;
#else
    err("Could not determine native platform for x86_64");
    return false;
#endif
  case COMPILE_ARCH_ARM64:
#if defined(__APPLE__)
    *target = COMPILE_TARGET_MACOS_ARM64;
    info("Compiling for '%s'...\n", string_target(*target));
    return true;
#elif defined(__linux__)
    *target = COMPILE_TARGET_LINUX_ARM64;
    info("Compiling for '%s'...\n", string_target(*target));
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
    info("Compiling for '%s'...\n", string_target(*target));
    return true;
#else
    err("Could not determine native platform for rv32i");
    return false;
#endif
  case COMPILE_ARCH_EEP:
    *target = COMPILE_TARGET_EEP;
    info("Compiling for '%s'...\n", string_target(*target));
    return true;
  }
}


static bool try_get_compile_args(int argc, char **argv, struct parsed_args *args, struct compile_args *compile_args, size_t *num_sources, const char ***sources) {
  *args = parse_args(argc, argv);

  *compile_args = (struct compile_args){
      .preproc_only = args->preprocess,
      .build_asm_file = args->assembly,
      .build_object_file = args->object,

      .c_standard = args->c_standard,
      .log_flags = args->log_level,

      .fixed_timestamp = args->timestamp,
      .include_paths = args->include_paths.values,
      .num_include_paths = args->include_paths.num_values,

      .output = args->output,
  };

  *num_sources = args->num_values;
  *sources = args->values;

  // debug_print_parsed_args(stderr, &args);
  
  if (!args->target) {
    if (!get_target_for_args(args->arch, &compile_args->target)) {
      return false;
    }
  } else {
    if (args->arch) {
      err("Cannot provide '-arch' and '-target'");
      return false;
    }

    compile_args->target = args->target;
  }

  if (compile_args->fixed_timestamp && !validate_fixed_timestamp(compile_args->fixed_timestamp)) {
    return false;
  }

  return true;
}

int main(int argc, char **argv) {
  size_t exc = -1;

  enable_log();

  info("parsing command line args");


  struct arena_allocator *arena = NULL;
  char **objects = NULL;

  arena_allocator_create(&arena);

  struct parsed_args args;
  struct compile_args compile_args;
  size_t num_sources;
  const char **sources;
  if (!try_get_compile_args(argc, argv, &args, &compile_args, &num_sources, &sources)) {
    exc = -1;
    goto exit;
  }

  const struct target *target = get_target(compile_args.target);
  objects = nonnull_malloc(sizeof(*objects) * num_sources);

  info("beginning compilation stage...");
  for (size_t i = 0; i < num_sources; i++) {
    const char *source_path = sources[i];

    info("compiling source file \"%s\"", source_path);

    const char *source = read_file(arena, source_path);

    if (!source) {
      err("source file \"%s\" could not be read!", source_path);
      exc = COMPILE_RESULT_BAD_FILE;
      goto exit;
    }

    char *object_file;

    if (compile_args.preproc_only && !compile_args.output) {
      // FIXME: hacky
      object_file = nonnull_malloc(strlen("stdout") + 1);
      strcpy(object_file, "stdout");
      object_file[strlen("stdout")] = 0;
    } else if (compile_args.build_asm_file && !compile_args.output) {
      object_file = path_replace_ext(arena, source_path, ".s");
    } else if (target_needs_linking(&compile_args, target) || !compile_args.output) {
      object_file = path_replace_ext(arena, source_path, "o");
    } else {
      object_file = strdup(compile_args.output);
    }

    info("compiling source file '%s' into object file '%s'", source_path,
         object_file);

    objects[i] = object_file;

    struct program program = {.text = source};

    disable_log();
    struct compiler *compiler;

    if (create_compiler(&program, target, object_file, source_path,
                        &compile_args,
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

  if (target_needs_linking(&compile_args, target)) {
    const char *output = compile_args.output ? compile_args.output : "a.out";

    struct link_args link_args = {.args = &compile_args,
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

  free_args(&args);

  return exc;
}
